unit MicroCoin.BlockChain.Block;

{
  This unit contains code from PascalCoin:

  Copyright (c) Albert Molina 2016 - 2018 original code from PascalCoin https://pascalcoin.org/

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

}
{$ifdef FPC}
  {$mode delphi}
{$endif}

interface

uses SysUtils, Classes, UTime, MicroCoin.Account.Transaction, MicroCoin.Transaction.HashTree,
  MicroCoin.BlockChain.BlockHeader, UConst, UCrypto, MicroCoin.Account.AccountKey, Ulog,
  MicroCoin.Transaction.ITransaction,
  MicroCoin.BlockChain.Protocol, MicroCoin.BlockChain.Base, MicroCoin.Transaction.Base, UThread;

type
  TBlock = class(TComponent)
  private
    FBlockManager: TBlockManagerBase;
    FAccountTransaction: TAccountTransaction;
    FBlockHeader: TBlockHeader;
    FTransactionHashTree: TTransactionHashTree;
    FDigest_Part1: TRawBytes;
    FDigest_Part2_Payload: TRawBytes;
    FDigest_Part3: TRawBytes;
    FIsOnlyBlock: Boolean;
    FStreamPoW: TMemoryStream;
    FDisableds: Integer;
    FBlockLock: TPCCriticalSection;
    function GetTransaction(index: Integer): ITransaction;
    procedure SetBank(const value: TBlockManagerBase);
    procedure SetnOnce(const value: Cardinal);
    procedure Settimestamp(const value: Cardinal);
    function GetnOnce: Cardinal;
    function Gettimestamp: Cardinal;
    procedure SetAccountKey(const value: TAccountKey);
    function GetAccountKey: TAccountKey;
    procedure Calc_Digest_Parts;
    procedure Calc_Digest_Part3;
    procedure CalcProofOfWork(fullcalculation: Boolean; var PoW: TRawBytes);
    function GetBlockPayload: TRawBytes;
    procedure SetBlockPayload(const value: TRawBytes);
    procedure OnHashTreeChanged(Sender: TObject);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function SaveBlockToStreamExt(ASaveOnlyBlock: Boolean; AStream: TStream; ASaveToStorage: Boolean): Boolean;
    function LoadBlockFromStreamExt(AStream: TStream; ALoadingFromStorage: Boolean; var RErrors: AnsiString): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CopyFromExceptAddressKey(ATransactions: TBlock);
    procedure CopyFrom(ATransactions: TBlock);
    function AddTransaction(AExecute: Boolean; ATransaction: ITransaction; var RErrors: AnsiString): Boolean;
    function AddTransactions(ATransactions: TTransactionHashTree; var errors: AnsiString): Integer;
    property Transaction[index: Integer]: ITransaction read GetTransaction;
    property BlockManager: TBlockManagerBase read FBlockManager write SetBank;
    procedure Clear(DeleteTransactions: Boolean);
    function Count: Integer;
    property BlockHeader: TBlockHeader read FBlockHeader;
    class function BlockToString(ABlockHeader: TBlockHeader): AnsiString;
    class function SaveBlockToStream(const ABlockHeader: TBlockHeader; Stream: TStream): Boolean; overload;
    property AccountKey: TAccountKey read GetAccountKey write SetAccountKey;
    property nonce: Cardinal read GetnOnce write SetnOnce;
    property timestamp: Cardinal read Gettimestamp write Settimestamp;
    property BlockPayload: TRawBytes read GetBlockPayload write SetBlockPayload;
    procedure UpdateTimestamp;
    function SaveBlockToStorage(Stream: TStream): Boolean;
    function SaveBlockToStream(ASaveOnlyBlock: Boolean; Stream: TStream): Boolean; overload;
    function LoadBlockFromStorage(Stream: TStream; var errors: AnsiString): Boolean;
    function LoadBlockFromStream(Stream: TStream; var errors: AnsiString): Boolean;
    //
    function ValidateBlock(var errors: AnsiString): Boolean;
    property IsOnlyBlock: Boolean read FIsOnlyBlock;
    procedure Lock;
    procedure Unlock;
    //
    procedure Sanitize;

    class function GetFirstBlock: TBlockHeader;
    class function Equals(const ABlock1, ABlock2: TBlockHeader): Boolean;
    //
    property AccountTransaction: TAccountTransaction read FAccountTransaction;
    property TransactionHashTree: TTransactionHashTree read FTransactionHashTree;
    property PoW_Digest_Part1: TRawBytes read FDigest_Part1;
    property PoW_Digest_Part2_Payload: TRawBytes read FDigest_Part2_Payload;
    property PoW_Digest_Part3: TRawBytes read FDigest_Part3;
  end;

implementation

function TBlock.AddTransaction(AExecute: Boolean; ATransaction: ITransaction; var RErrors: AnsiString): Boolean;
begin
  Lock;
  try
    RErrors := '';
    Result := false;
    if AExecute then
    begin
      if (FBlockManager = nil) then
      begin
        RErrors := 'No Bank';
        exit;
      end;
      if (FBlockManager.BlocksCount <> BlockHeader.Block) then
      begin
        RErrors := 'Bank blockcount<>OperationBlock.Block';
        exit;
      end;
      // Only process when in current address, prevent do it when reading operations from file
      Result := ATransaction.ApplyTransaction(AccountTransaction, RErrors);
    end
    else
      Result := true;
    if Result then
    begin
      FTransactionHashTree.AddTransactionToHashTree(ATransaction);
      FBlockHeader.Fee := FBlockHeader.Fee + ATransaction.Fee;
      FBlockHeader.transactionHash := FTransactionHashTree.HashTree;
      if FDisableds <= 0 then
        Calc_Digest_Parts;
    end;
  finally
    Unlock;
  end;
end;

function TBlock.AddTransactions(ATransactions: TTransactionHashTree; var errors: AnsiString): Integer;
var
  i: Integer;
  E: AnsiString;
begin
  Lock;
  try
    Result := 0;
    errors := '';
    if ATransactions = FTransactionHashTree then
      exit;
    inc(FDisableds);
    try
      for i := 0 to ATransactions.TransactionCount - 1 do
      begin
        if not AddTransaction(true, ATransactions.GetTransaction(i), E) then
        begin
          if (errors <> '') then
            errors := errors + ' ';
          errors := errors + 'Op' + Inttostr(i + 1) + '/' + Inttostr(ATransactions.TransactionCount) + ':' + E;
        end
        else
          inc(Result);
      end;
    finally
      Dec(FDisableds);
      Calc_Digest_Parts;
    end;
  finally
    Unlock;
  end;
end;

procedure TBlock.CalcProofOfWork(fullcalculation: Boolean; var PoW: TRawBytes);
begin
  if fullcalculation then
  begin
    Calc_Digest_Parts;
  end;
  FStreamPoW.Position := 0;
  FStreamPoW.WriteBuffer(FDigest_Part1[1], length(FDigest_Part1));
  FStreamPoW.WriteBuffer(FDigest_Part2_Payload[1], length(FDigest_Part2_Payload));
  FStreamPoW.WriteBuffer(FDigest_Part3[1], length(FDigest_Part3));
  FStreamPoW.Write(FBlockHeader.timestamp, 4);
  FStreamPoW.Write(FBlockHeader.nonce, 4);
  TCrypto.DoDoubleSha256(FStreamPoW.Memory, length(FDigest_Part1) + length(FDigest_Part2_Payload) +
    length(FDigest_Part3) + 8, PoW);
end;

procedure TBlock.Calc_Digest_Parts;
begin
  TMicroCoinProtocol.CalcProofOfWork_Part1(FBlockHeader, FDigest_Part1);
  FDigest_Part2_Payload := FBlockHeader.block_payload;
  Calc_Digest_Part3;
end;

procedure TBlock.Calc_Digest_Part3;
begin
  FBlockHeader.transactionHash := FTransactionHashTree.HashTree;
  TMicroCoinProtocol.CalcProofOfWork_Part3(FBlockHeader, FDigest_Part3);
end;

procedure TBlock.Clear(DeleteTransactions: Boolean);
begin
  Lock;
  try
    if DeleteTransactions then
    begin
      FTransactionHashTree.ClearHastThree;
      if Assigned(FAccountTransaction) then
        FAccountTransaction.CleanTransaction;
    end;

    // Note:
    // This function does not initializes "account_key" nor "block_payload" fields

    FBlockHeader.timestamp := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
    if Assigned(FBlockManager) then
    begin
      FBlockHeader.protocol_version := BlockManager.AccountStorage.CurrentProtocol;
      if (FBlockHeader.protocol_version = cPROTOCOL_1) and (FBlockManager.AccountStorage.CanUpgradeToProtocol2) then
      begin
        FBlockHeader.protocol_version := cPROTOCOL_2;
        // If minting... upgrade to Protocol 2
      end;
      FBlockHeader.Block := BlockManager.BlocksCount;
      FBlockHeader.reward := TMicroCoinProtocol.GetRewardForNewLine(BlockManager.BlocksCount);
      FBlockHeader.compact_target := BlockManager.AccountStorage.GetActualCompactTargetHash
        (FBlockHeader.protocol_version = cPROTOCOL_2);
      FBlockHeader.initial_safe_box_hash := BlockManager.AccountStorage.AccountStorageHash;
      if BlockManager.LastBlock.timestamp > FBlockHeader.timestamp then
        FBlockHeader.timestamp := BlockManager.LastBlock.timestamp;
    end
    else
    begin
      FBlockHeader.Block := 0;
      FBlockHeader.reward := TMicroCoinProtocol.GetRewardForNewLine(0);
      FBlockHeader.compact_target := cMinimumDifficulty;
      FBlockHeader.initial_safe_box_hash := TCrypto.DoSha256(cGenesisBlockMagic);
      // Nothing for first line
      FBlockHeader.protocol_version := cPROTOCOL_1;
    end;
    FBlockHeader.transactionHash := FTransactionHashTree.HashTree;
    FBlockHeader.Fee := 0;
    FBlockHeader.nonce := 0;
    FBlockHeader.proof_of_work := '';
    FBlockHeader.protocol_available := cBlockChain_Protocol_Available;
    FIsOnlyBlock := false;
  finally
    try
      CalcProofOfWork(true, FBlockHeader.proof_of_work);
    finally
      Unlock;
    end;
  end;
end;

procedure TBlock.CopyFrom(ATransactions: TBlock);
begin
  if Self = ATransactions then
    exit;
  Lock;
  ATransactions.Lock;
  try
    FBlockHeader := ATransactions.FBlockHeader;
    FIsOnlyBlock := ATransactions.FIsOnlyBlock;
    FTransactionHashTree.CopyFromHashTree(ATransactions.FTransactionHashTree);
    if Assigned(FAccountTransaction) and Assigned(ATransactions.FAccountTransaction) then
    begin
      FAccountTransaction.CopyFrom(ATransactions.FAccountTransaction);
    end;
    FDigest_Part1 := ATransactions.FDigest_Part1;
    FDigest_Part2_Payload := ATransactions.FDigest_Part2_Payload;
    FDigest_Part3 := ATransactions.FDigest_Part3;
  finally
    ATransactions.Unlock;
    Unlock;
  end;
end;

procedure TBlock.CopyFromExceptAddressKey(ATransactions: TBlock);
var
  lastopb: TBlockHeader;
begin
  Lock;
  try
    if Self = ATransactions then
      exit;
    lastopb := FBlockHeader;
    FBlockHeader := ATransactions.FBlockHeader;
    FBlockHeader.account_key := lastopb.account_key; // Except AddressKey
    FBlockHeader.compact_target := BlockManager.AccountStorage.GetActualCompactTargetHash
      (FBlockHeader.protocol_version = cPROTOCOL_2);
    FIsOnlyBlock := ATransactions.FIsOnlyBlock;
    FTransactionHashTree.CopyFromHashTree(ATransactions.FTransactionHashTree);
    FBlockHeader.transactionHash := FTransactionHashTree.HashTree;
    if Assigned(FAccountTransaction) and Assigned(ATransactions.FAccountTransaction) then
    begin
      FAccountTransaction.CopyFrom(ATransactions.FAccountTransaction);
    end;
    // Recalc all
    CalcProofOfWork(true, FBlockHeader.proof_of_work);
  finally
    Unlock;
  end;
end;

function TBlock.Count: Integer;
begin
  Result := FTransactionHashTree.TransactionCount;
end;

constructor TBlock.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBlockLock := TPCCriticalSection.Create('TPCOperationsComp_OPERATIONSLOCK');
  FDisableds := 0;
  FStreamPoW := TMemoryStream.Create;
  FStreamPoW.Position := 0;
  FTransactionHashTree := TTransactionHashTree.Create;
  FTransactionHashTree.OnChanged := OnHashTreeChanged;
  FBlockManager := nil;
  FBlockHeader := GetFirstBlock;
  FAccountTransaction := nil;
  if Assigned(AOwner) and (AOwner is TBlockManagerBase) then
  begin
    BlockManager := TBlockManagerBase(AOwner);
  end
  else
    Clear(true);
end;

destructor TBlock.Destroy;
begin
  FBlockLock.Acquire;
  try
    Clear(true);
    FreeAndNil(FTransactionHashTree);
    if Assigned(FAccountTransaction) then
    begin
      FreeAndNil(FAccountTransaction);
    end;
    FreeAndNil(FStreamPoW);
  finally
    FreeAndNil(FBlockLock);
  end;
  inherited;
end;

class function TBlock.Equals(const ABlock1, ABlock2: TBlockHeader): Boolean;
begin

  Result := (ABlock1.Block = ABlock2.Block) and
    (TAccountKey.EqualAccountKeys(ABlock1.account_key, ABlock2.account_key)) and
    (ABlock1.reward = ABlock2.reward) and (ABlock1.Fee = ABlock2.Fee) and
    (ABlock1.protocol_version = ABlock2.protocol_version) and
    (ABlock1.protocol_available = ABlock2.protocol_available) and
    (ABlock1.timestamp = ABlock2.timestamp) and
    (ABlock1.compact_target = ABlock2.compact_target) and
    (ABlock1.nonce = ABlock2.nonce) and (ABlock1.block_payload = ABlock2.block_payload)
    and (ABlock1.initial_safe_box_hash = ABlock2.initial_safe_box_hash) and
    (ABlock1.transactionHash = ABlock2.transactionHash) and
    (ABlock1.proof_of_work = ABlock2.proof_of_work);
end;

function TBlock.GetAccountKey: TAccountKey;
begin
  Result := FBlockHeader.account_key;
end;

function TBlock.GetBlockPayload: TRawBytes;
begin
  Result := FBlockHeader.block_payload;
end;

class function TBlock.GetFirstBlock: TBlockHeader;
begin
  Result := TBlockHeader.Empty;
end;

function TBlock.GetnOnce: Cardinal;
begin
  Result := FBlockHeader.nonce;
end;

function TBlock.GetTransaction(index: Integer): ITransaction;
begin
  Result := FTransactionHashTree.GetTransaction(index);
end;

function TBlock.Gettimestamp: Cardinal;
begin
  Result := FBlockHeader.timestamp;
end;

function TBlock.LoadBlockFromStorage(Stream: TStream; var errors: AnsiString): Boolean;
begin
  Result := LoadBlockFromStreamExt(Stream, true, errors);
end;

function TBlock.LoadBlockFromStream(Stream: TStream; var errors: AnsiString): Boolean;
begin
  Result := LoadBlockFromStreamExt(Stream, false, errors);
end;

function TBlock.LoadBlockFromStreamExt(AStream: TStream; ALoadingFromStorage: Boolean; var RErrors: AnsiString): Boolean;
var
  i: Cardinal;
  lastfee: UInt64;
  soob: Byte;
  m: AnsiString;
  load_protocol_v2: Boolean;
begin
  Lock;
  try
    Clear(true);
    Result := false;
    //
    RErrors := 'Invalid protocol structure. Check application version!';
    if (AStream.Size - AStream.Position < 5) then
      exit;
    AStream.Read(soob, 1);
    // About soob var:
    // In build prior to 1.0.4 soob only can have 2 values: 0 or 1
    // In build 1.0.4 soob can has 2 more values: 2 or 3
    // In build 2.0 soob can has 1 more value: 4
    // In future, old values 0 and 1 will no longer be used!
    // - Value 0 and 2 means that contains also o
    // - Value 1 and 3 means that only contains operationblock info
    // - Value 2 and 3 means that contains protocol info prior to block number
    // - Value 4 means that is loading from storage using protocol v2 (so, includes always operations)
    load_protocol_v2 := false;
    if (soob in [0, 2]) then
      FIsOnlyBlock := false
    else if (soob in [1, 3]) then
      FIsOnlyBlock := true
    else if (soob in [4]) then
    begin
      FIsOnlyBlock := false;
      load_protocol_v2 := true;
    end
    else
    begin
      RErrors := 'Invalid value in protocol header! Found:' + Inttostr(soob) +
        ' - Check if your application version is Ok';
      exit;
    end;

    if (soob in [2, 3, 4]) then
    begin
      AStream.Read(FBlockHeader.protocol_version, Sizeof(FBlockHeader.protocol_version));
      AStream.Read(FBlockHeader.protocol_available, Sizeof(FBlockHeader.protocol_available));
    end
    else
    begin
      // We assume that protocol_version is 1 and protocol_available is 0
      FBlockHeader.protocol_version := 1;
      FBlockHeader.protocol_available := 0;
    end;

    if AStream.Read(FBlockHeader.Block, Sizeof(FBlockHeader.Block)) < 0 then
      exit;

    if TStreamOp.ReadAnsiString(AStream, m) < 0 then
      exit;
    FBlockHeader.account_key := TAccountKey.FromRawString(m);
    if AStream.Read(FBlockHeader.reward, Sizeof(FBlockHeader.reward)) < 0 then
      exit;
    if AStream.Read(FBlockHeader.Fee, Sizeof(FBlockHeader.Fee)) < 0 then
      exit;
    if AStream.Read(FBlockHeader.timestamp, Sizeof(FBlockHeader.timestamp)) < 0 then
      exit;
    if AStream.Read(FBlockHeader.compact_target, Sizeof(FBlockHeader.compact_target)) < 0 then
      exit;
    if AStream.Read(FBlockHeader.nonce, Sizeof(FBlockHeader.nonce)) < 0 then
      exit;
    if TStreamOp.ReadAnsiString(AStream, FBlockHeader.block_payload) < 0 then
      exit;
    if TStreamOp.ReadAnsiString(AStream, FBlockHeader.initial_safe_box_hash) < 0 then
      exit;
    if TStreamOp.ReadAnsiString(AStream, FBlockHeader.transactionHash) < 0 then
      exit;
    if TStreamOp.ReadAnsiString(AStream, FBlockHeader.proof_of_work) < 0 then
      exit;
    if FIsOnlyBlock then
    begin
      Result := true;
      exit;
    end;
    // Fee will be calculated for each operation. Set it to 0 and check later for integrity
    lastfee := BlockHeader.Fee;
    FBlockHeader.Fee := 0;
    Result := FTransactionHashTree.LoadFromStream(AStream, ALoadingFromStorage, load_protocol_v2, RErrors);
    if not Result then
    begin
      exit;
    end;
    FBlockHeader.Fee := FTransactionHashTree.TotalFee;
    FBlockHeader.transactionHash := FTransactionHashTree.HashTree;
    Calc_Digest_Parts;
    // Validation control:
    if (lastfee <> BlockHeader.Fee) then
    begin
      RErrors := 'Corrupted operations fee old:' + Inttostr(lastfee) + ' new:' + Inttostr(BlockHeader.Fee);
      for i := 0 to FTransactionHashTree.TransactionCount - 1 do
      begin
        RErrors := RErrors + ' Op' + Inttostr(i + 1) + ':' + FTransactionHashTree.GetTransaction(i).ToString;
      end;
      Result := false;
      exit;
    end;
    Result := true;
  finally
    Unlock;
  end;
end;

procedure TBlock.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if AComponent = FBlockManager then
    begin
      FBlockManager := nil;
      FreeAndNil(FAccountTransaction);
    end;
  end;
end;

class function TBlock.BlockToString(ABlockHeader: TBlockHeader): AnsiString;
begin
  Result := Format('Block:%d Timestamp:%d Reward:%d Fee:%d Target:%d PoW:%s',
    [ABlockHeader.Block, ABlockHeader.timestamp, ABlockHeader.reward, ABlockHeader.Fee,
    ABlockHeader.compact_target, TCrypto.ToHexaString(ABlockHeader.proof_of_work)]);
end;

procedure TBlock.Sanitize;
{ This function check operationblock with bank and updates itself if necessary
  Then checks if operations are ok, and deletes old ones.
  Finally calculates new operation pow
  It's used when a new account has beed found by other chanels (miners o nodes...)
}
var
  i, n, lastn: Integer;
  ATransaction: ITransaction;
  xErrors: AnsiString;
  aux, aux2: TTransactionHashTree;
begin
  Lock;
  try
    FBlockHeader.timestamp := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
    if Assigned(FBlockManager) then
    begin
      FBlockHeader.protocol_version := BlockManager.AccountStorage.CurrentProtocol;
      if (FBlockHeader.protocol_version = cPROTOCOL_1) and (FBlockManager.AccountStorage.CanUpgradeToProtocol2) then
      begin
        TLog.NewLog(ltinfo, Classname, 'New miner protocol version to 2 at sanitize');
        FBlockHeader.protocol_version := cPROTOCOL_2;
      end;
      FBlockHeader.Block := BlockManager.BlocksCount;
      FBlockHeader.reward := TMicroCoinProtocol.GetRewardForNewLine(BlockManager.BlocksCount);
      FBlockHeader.compact_target := BlockManager.AccountStorage.GetActualCompactTargetHash
        (FBlockHeader.protocol_version = cPROTOCOL_2);
      FBlockHeader.initial_safe_box_hash := BlockManager.AccountStorage.AccountStorageHash;
      if BlockManager.LastBlock.timestamp > FBlockHeader.timestamp then
        FBlockHeader.timestamp := BlockManager.LastBlock.timestamp;
    end
    else
    begin
      FBlockHeader.Block := 0;
      FBlockHeader.reward := TMicroCoinProtocol.GetRewardForNewLine(0);
      FBlockHeader.compact_target := cMinimumDifficulty;
      FBlockHeader.initial_safe_box_hash := TCrypto.DoSha256(cGenesisBlockMagic);
      FBlockHeader.protocol_version := cPROTOCOL_1;
    end;
    FBlockHeader.proof_of_work := '';
    FBlockHeader.protocol_available := cBlockChain_Protocol_Available;
    n := 0;
    FBlockHeader.Fee := 0;
    //
    AccountTransaction.CleanTransaction;
    //
    aux := TTransactionHashTree.Create;
    try
      lastn := FTransactionHashTree.TransactionCount;
      for i := 0 to lastn - 1 do
      begin
        ATransaction := FTransactionHashTree.GetTransaction(i);
        if (ATransaction.ApplyTransaction(AccountTransaction, xErrors)) then
        begin
          inc(n);
          aux.AddTransactionToHashTree(ATransaction);
          inc(FBlockHeader.Fee, ATransaction.Fee);
          TLog.NewLog(ltdebug, Classname, 'Sanitizing (pos:' + Inttostr(i + 1) + '/' + Inttostr(lastn) + '): ' +
            ATransaction.ToString);
        end;
      end;
    finally
      aux2 := FTransactionHashTree;
      FTransactionHashTree := aux;
      aux2.Free;
      FBlockHeader.transactionHash := FTransactionHashTree.HashTree;
    end;
  finally
    CalcProofOfWork(true, FBlockHeader.proof_of_work);
    Unlock;
  end;
  if (n > 0) then
  begin
    TLog.NewLog(ltdebug, Classname, Format('Sanitize operations (before %d - after %d)', [lastn, n]));
  end;
end;

function TBlock.SaveBlockToStorage(Stream: TStream): Boolean;
begin
  Result := SaveBlockToStreamExt(false, Stream, true);
end;

function TBlock.SaveBlockToStream(ASaveOnlyBlock: Boolean; Stream: TStream): Boolean;
begin
  Result := SaveBlockToStreamExt(ASaveOnlyBlock, Stream, false);
end;

function TBlock.SaveBlockToStreamExt(ASaveOnlyBlock: Boolean; AStream: TStream;
  ASaveToStorage: Boolean): Boolean;
var
  soob: Byte;
begin
  Lock;
  try
    if ASaveOnlyBlock then
    begin
      { Old versions:
        if (FOperationBlock.protocol_version=1) And (FOperationBlock.protocol_available=0) then soob := 1
        else soob := 3; }
      soob := 3;
    end
    else
    begin
      { Old versions:
        if (FOperationBlock.protocol_version=1) And (FOperationBlock.protocol_available=0) then soob := 0
        else soob := 2; }
      soob := 2;
      if (ASaveToStorage) then
      begin
        // Introduced on protocol v2: soob = 4 when saving to storage
        soob := 4;
      end;
    end;
    AStream.Write(soob, 1);
    if (soob >= 2) then
    begin
      AStream.Write(FBlockHeader.protocol_version, Sizeof(FBlockHeader.protocol_version));
      AStream.Write(FBlockHeader.protocol_available, Sizeof(FBlockHeader.protocol_available));
    end;
    //
    AStream.Write(FBlockHeader.Block, Sizeof(FBlockHeader.Block));
    //
    TStreamOp.WriteAnsiString(AStream, FBlockHeader.account_key.ToRawString);
    AStream.Write(FBlockHeader.reward, Sizeof(FBlockHeader.reward));
    AStream.Write(FBlockHeader.Fee, Sizeof(FBlockHeader.Fee));
    AStream.Write(FBlockHeader.timestamp, Sizeof(FBlockHeader.timestamp));
    AStream.Write(FBlockHeader.compact_target, Sizeof(FBlockHeader.compact_target));
    AStream.Write(FBlockHeader.nonce, Sizeof(FBlockHeader.nonce));
    TStreamOp.WriteAnsiString(AStream, FBlockHeader.block_payload);
    TStreamOp.WriteAnsiString(AStream, FBlockHeader.initial_safe_box_hash);
    TStreamOp.WriteAnsiString(AStream, FBlockHeader.transactionHash);
    TStreamOp.WriteAnsiString(AStream, FBlockHeader.proof_of_work);
    { Basic size calculation:
      protocols : 2 words = 4 bytes
      block : 4 bytes
      Account_key (VARIABLE LENGTH) at least 2 + 34 + 34 for secp256k1 key = 70 bytes
      reward, fee, timestamp, compact_target, nonce = 8+8+4+4+4 = 28 bytes
      payload (VARIABLE LENGTH) minimum 2 bytes... but usually 40 by average = 40 bytes
      sbh, operations_hash, pow ( 32 + 32 + 32 ) =  96 bytes
      Total, by average: 242 bytes
    }
    if (not ASaveOnlyBlock) then
    begin
      Result := FTransactionHashTree.SaveToStream(AStream, ASaveToStorage);
    end
    else
      Result := true;
  finally
    Unlock;
  end;
end;

class function TBlock.SaveBlockToStream(const ABlockHeader: TBlockHeader; Stream: TStream): Boolean;
var
  soob: Byte;
begin
  soob := 3;
  Stream.Write(soob, 1);
  Stream.Write(ABlockHeader.protocol_version, Sizeof(ABlockHeader.protocol_version));
  Stream.Write(ABlockHeader.protocol_available, Sizeof(ABlockHeader.protocol_available));
  //
  Stream.Write(ABlockHeader.Block, Sizeof(ABlockHeader.Block));
  //
  TStreamOp.WriteAnsiString(Stream, ABlockHeader.account_key.ToRawString);
  Stream.Write(ABlockHeader.reward, Sizeof(ABlockHeader.reward));
  Stream.Write(ABlockHeader.Fee, Sizeof(ABlockHeader.Fee));
  Stream.Write(ABlockHeader.timestamp, Sizeof(ABlockHeader.timestamp));
  Stream.Write(ABlockHeader.compact_target, Sizeof(ABlockHeader.compact_target));
  Stream.Write(ABlockHeader.nonce, Sizeof(ABlockHeader.nonce));
  TStreamOp.WriteAnsiString(Stream, ABlockHeader.block_payload);
  TStreamOp.WriteAnsiString(Stream, ABlockHeader.initial_safe_box_hash);
  TStreamOp.WriteAnsiString(Stream, ABlockHeader.transactionHash);
  TStreamOp.WriteAnsiString(Stream, ABlockHeader.proof_of_work);
  Result := true;
end;

procedure TBlock.SetAccountKey(const value: TAccountKey);
begin
  Lock;
  try
    if value.ToRawString = FBlockHeader.account_key.ToRawString then
      exit;
    FBlockHeader.account_key := value;
    Calc_Digest_Parts;
  finally
    Unlock;
  end;
end;

procedure TBlock.SetBank(const value: TBlockManagerBase);
begin
  if FBlockManager = value then
    exit;
  if Assigned(FBlockManager) then
  begin
    FreeAndNil(FAccountTransaction);
  end;
  FBlockManager := value;
  if Assigned(value) then
  begin
    value.FreeNotification(Self);
    FAccountTransaction := TAccountTransaction.Create(FBlockManager.AccountStorage);
  end;
  Clear(true);
end;

procedure TBlock.SetBlockPayload(const value: TRawBytes);
var
  i: Integer;
begin
  Lock;
  try
    if value = FBlockHeader.block_payload then
      exit;
    if length(value) > cMaxPayloadSize then
      exit;
    // Checking Miner Payload valid chars
    for i := 1 to length(value) do
    begin
      if not(value[i] in [#32 .. #254]) then
      begin
        exit;
      end;
    end;
    FBlockHeader.block_payload := value;
    CalcProofOfWork(true, FBlockHeader.proof_of_work);
  finally
    Unlock;
  end;
end;

procedure TBlock.OnHashTreeChanged(Sender: TObject);
begin
  FBlockHeader.transactionHash := FTransactionHashTree.HashTree;
  Calc_Digest_Part3;
end;

procedure TBlock.SetnOnce(const value: Cardinal);
begin
  Lock;
  try
    FBlockHeader.nonce := value;
    CalcProofOfWork(false, FBlockHeader.proof_of_work);
  finally
    Unlock;
  end;
end;

procedure TBlock.Settimestamp(const value: Cardinal);
begin
  Lock;
  try
    if FBlockHeader.timestamp = value then
      exit; // No change, nothing to do
    FBlockHeader.timestamp := value;
    CalcProofOfWork(false, FBlockHeader.proof_of_work);
  finally
    Unlock;
  end;
end;

procedure TBlock.UpdateTimestamp;
var
  ts: Cardinal;
begin
  Lock;
  try
    ts := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
    if Assigned(BlockManager) then
    begin
      if BlockManager.LastBlock.timestamp > ts then
        ts := BlockManager.LastBlock.timestamp;
    end;
    timestamp := ts;
  finally
    Unlock;
  end;
end;

function TBlock.ValidateBlock(var errors: AnsiString): Boolean;
var
  lastpow: AnsiString;
  i: Integer;
begin
  errors := '';
  Result := false;
  Lock;
  try
    if not Assigned(AccountTransaction) then
    begin
      errors := 'ERROR DEV 20170523-1';
      exit;
    end;
    if not Assigned(AccountTransaction.FreezedAccountStorage) then
    begin
      errors := 'ERROR DEV 20170523-2';
      exit;
    end;
    // Check OperationBlock info:
    if not AccountTransaction.FreezedAccountStorage.IsValidNewBlockHeader(BlockHeader, true, errors) then
      exit;
    // Execute SafeBoxTransaction operations:
    AccountTransaction.Rollback;
    for i := 0 to Count - 1 do
    begin
      if not Transaction[i].ApplyTransaction(AccountTransaction, errors) then
      begin
        errors := 'Error executing operation ' + Inttostr(i + 1) + '/' + Inttostr(Count) + ': ' + errors;
        exit;
      end;
    end;
    // Check OperationsHash value is valid
    if FTransactionHashTree.HashTree <> BlockHeader.transactionHash then
    begin
      errors := 'Invalid Operations Hash ' + TCrypto.ToHexaString(BlockHeader.transactionHash) + '<>' +
        TCrypto.ToHexaString(FTransactionHashTree.HashTree);
      exit;
    end;
    // Check OperationBlock with SafeBox info:
    if (AccountTransaction.FreezedAccountStorage.TotalBalance <> (AccountTransaction.TotalBalance +
      AccountTransaction.TotalFee)) then
    begin
      errors := Format('Invalid integrity balance at SafeBox. Actual Balance:%d  New Balance:(%d + fee %d = %d)',
        [AccountTransaction.FreezedAccountStorage.TotalBalance, AccountTransaction.TotalBalance,
        AccountTransaction.TotalFee, AccountTransaction.TotalBalance + AccountTransaction.TotalFee]);
      exit;
    end;
    // Check fee value
    if (AccountTransaction.TotalFee <> BlockHeader.Fee) then
    begin
      errors := Format
        ('Invalid fee integrity at SafeBoxTransaction. New Balance:(%d + fee %d = %d)  OperationBlock.fee:%d',
        [AccountTransaction.TotalBalance, AccountTransaction.TotalFee, AccountTransaction.TotalBalance +
        AccountTransaction.TotalFee, BlockHeader.Fee]);
      exit;
    end;

    Result := true;
  finally
    Unlock;
  end;
end;

procedure TBlock.Lock;
begin
  FBlockLock.Acquire;
end;

procedure TBlock.Unlock;
begin
  FBlockLock.Release;
end;

end.
