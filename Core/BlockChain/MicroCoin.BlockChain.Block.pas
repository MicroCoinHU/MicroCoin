unit MicroCoin.BlockChain.Block;

{
  This unit contains code from PascalCoin:

  Copyright (c) Albert Molina 2016 - 2018 original code from PascalCoin https://pascalcoin.org/

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

}

interface

uses SysUtils, Classes, UTime, MicroCoin.Account.Transaction, MicroCoin.Transaction.HashTree,
  MicroCoin.BlockChain.BlockHeader, UConst, UCrypto, MicroCoin.Account.AccountKey, Ulog,
  MicroCoin.BlockChain.Protocol, MicroCoin.BlockChain.Base, MicroCoin.Transaction.Base, UThread;

type
  TBlock = class(TComponent)
  private
    FBank: TBlockManagerBase;
    FSafeBoxTransaction: TAccountTransaction;
    FOperationBlock: TBlockHeader;
    FOperationsHashTree: TTransactionHashTree;
    FDigest_Part1: TRawBytes;
    FDigest_Part2_Payload: TRawBytes;
    FDigest_Part3: TRawBytes;
    FIsOnlyOperationBlock: Boolean;
    FStreamPoW: TMemoryStream;
    FDisableds: Integer;
    FOperationsLock: TPCCriticalSection;
    function GetOperation(index: Integer): ITransaction;
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
    procedure OnOperationsHashTreeChanged(Sender: TObject);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function SaveBlockToStreamExt(save_only_OperationBlock: Boolean; Stream: TStream; SaveToStorage: Boolean): Boolean;
    function LoadBlockFromStreamExt(Stream: TStream; LoadingFromStorage: Boolean; var errors: AnsiString): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CopyFromExceptAddressKey(Operations: TBlock);
    procedure CopyFrom(Operations: TBlock);
    function AddOperation(Execute: Boolean; op: ITransaction; var errors: AnsiString): Boolean;
    function AddOperations(Operations: TTransactionHashTree; var errors: AnsiString): Integer;
    property Operation[index: Integer]: ITransaction read GetOperation;
    property bank: TBlockManagerBase read FBank write SetBank;
    procedure Clear(DeleteOperations: Boolean);
    function Count: Integer;
    property OperationBlock: TBlockHeader read FOperationBlock;
    class function OperationBlockToText(OperationBlock: TBlockHeader): AnsiString;
    class function SaveOperationBlockToStream(const OperationBlock: TBlockHeader; Stream: TStream): Boolean;
    property AccountKey: TAccountKey read GetAccountKey write SetAccountKey;
    property nonce: Cardinal read GetnOnce write SetnOnce;
    property timestamp: Cardinal read Gettimestamp write Settimestamp;
    property BlockPayload: TRawBytes read GetBlockPayload write SetBlockPayload;
    procedure UpdateTimestamp;
    function SaveBlockToStorage(Stream: TStream): Boolean;
    function SaveBlockToStream(save_only_OperationBlock: Boolean; Stream: TStream): Boolean;
    function LoadBlockFromStorage(Stream: TStream; var errors: AnsiString): Boolean;
    function LoadBlockFromStream(Stream: TStream; var errors: AnsiString): Boolean;
    //
    function ValidateOperationBlock(var errors: AnsiString): Boolean;
    property IsOnlyOperationBlock: Boolean read FIsOnlyOperationBlock;
    procedure Lock;
    procedure Unlock;
    //
    procedure SanitizeOperations;

    class function GetFirstBlock: TBlockHeader;
    class function EqualsOperationBlock(const OperationBlock1, OperationBlock2: TBlockHeader): Boolean;
    //
    property SafeBoxTransaction: TAccountTransaction read FSafeBoxTransaction;
    property OperationsHashTree: TTransactionHashTree read FOperationsHashTree;
    property PoW_Digest_Part1: TRawBytes read FDigest_Part1;
    property PoW_Digest_Part2_Payload: TRawBytes read FDigest_Part2_Payload;
    property PoW_Digest_Part3: TRawBytes read FDigest_Part3;
  end;

implementation

function TBlock.AddOperation(Execute: Boolean; op: ITransaction; var errors: AnsiString): Boolean;
begin
  Lock;
  try
    errors := '';
    Result := false;
    if Execute then
    begin
      if (FBank = nil) then
      begin
        errors := 'No Bank';
        exit;
      end;
      if (FBank.BlocksCount <> OperationBlock.Block) then
      begin
        errors := 'Bank blockcount<>OperationBlock.Block';
        exit;
      end;
      // Only process when in current address, prevent do it when reading operations from file
      Result := op.ApplyTransaction(SafeBoxTransaction, errors);
    end
    else
      Result := true;
    if Result then
    begin
      FOperationsHashTree.AddTransactionToHashTree(op);
      FOperationBlock.Fee := FOperationBlock.Fee + op.Fee;
      FOperationBlock.operations_hash := FOperationsHashTree.HashTree;
      if FDisableds <= 0 then
        Calc_Digest_Parts;
    end;
  finally
    Unlock;
  end;
end;

function TBlock.AddOperations(Operations: TTransactionHashTree; var errors: AnsiString): Integer;
var
  i: Integer;
  E: AnsiString;
begin
  Lock;
  try
    Result := 0;
    errors := '';
    if Operations = FOperationsHashTree then
      exit;
    inc(FDisableds);
    try
      for i := 0 to Operations.OperationsCount - 1 do
      begin
        if not AddOperation(true, Operations.GetOperation(i), E) then
        begin
          if (errors <> '') then
            errors := errors + ' ';
          errors := errors + 'Op' + Inttostr(i + 1) + '/' + Inttostr(Operations.OperationsCount) + ':' + E;
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
  FStreamPoW.Write(FOperationBlock.timestamp, 4);
  FStreamPoW.Write(FOperationBlock.nonce, 4);
  TCrypto.DoDoubleSha256(FStreamPoW.Memory, length(FDigest_Part1) + length(FDigest_Part2_Payload) +
    length(FDigest_Part3) + 8, PoW);
end;

procedure TBlock.Calc_Digest_Parts;
begin
  TMicroCoinProtocol.CalcProofOfWork_Part1(FOperationBlock, FDigest_Part1);
  FDigest_Part2_Payload := FOperationBlock.block_payload;
  Calc_Digest_Part3;
end;

procedure TBlock.Calc_Digest_Part3;
begin
  FOperationBlock.operations_hash := FOperationsHashTree.HashTree;
  TMicroCoinProtocol.CalcProofOfWork_Part3(FOperationBlock, FDigest_Part3);
end;

procedure TBlock.Clear(DeleteOperations: Boolean);
begin
  Lock;
  try
    if DeleteOperations then
    begin
      FOperationsHashTree.ClearHastThree;
      if Assigned(FSafeBoxTransaction) then
        FSafeBoxTransaction.CleanTransaction;
    end;

    // Note:
    // This function does not initializes "account_key" nor "block_payload" fields

    FOperationBlock.timestamp := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
    if Assigned(FBank) then
    begin
      FOperationBlock.protocol_version := bank.AccountStorage.CurrentProtocol;
      if (FOperationBlock.protocol_version = CT_PROTOCOL_1) and (FBank.AccountStorage.CanUpgradeToProtocol2) then
      begin
        FOperationBlock.protocol_version := CT_PROTOCOL_2;
        // If minting... upgrade to Protocol 2
      end;
      FOperationBlock.Block := bank.BlocksCount;
      FOperationBlock.reward := TMicroCoinProtocol.GetRewardForNewLine(bank.BlocksCount);
      FOperationBlock.compact_target := bank.AccountStorage.GetActualCompactTargetHash
        (FOperationBlock.protocol_version = CT_PROTOCOL_2);
      FOperationBlock.initial_safe_box_hash := bank.AccountStorage.AccountStorageHash;
      if bank.LastOperationBlock.timestamp > FOperationBlock.timestamp then
        FOperationBlock.timestamp := bank.LastOperationBlock.timestamp;
    end
    else
    begin
      FOperationBlock.Block := 0;
      FOperationBlock.reward := TMicroCoinProtocol.GetRewardForNewLine(0);
      FOperationBlock.compact_target := CT_MinCompactTarget;
      FOperationBlock.initial_safe_box_hash := TCrypto.DoSha256(CT_Genesis_Magic_String_For_Old_Block_Hash);
      // Nothing for first line
      FOperationBlock.protocol_version := CT_PROTOCOL_1;
    end;
    FOperationBlock.operations_hash := FOperationsHashTree.HashTree;
    FOperationBlock.Fee := 0;
    FOperationBlock.nonce := 0;
    FOperationBlock.proof_of_work := '';
    FOperationBlock.protocol_available := CT_BlockChain_Protocol_Available;
    FIsOnlyOperationBlock := false;
  finally
    try
      CalcProofOfWork(true, FOperationBlock.proof_of_work);
    finally
      Unlock;
    end;
  end;
end;

procedure TBlock.CopyFrom(Operations: TBlock);
begin
  if Self = Operations then
    exit;
  Lock;
  Operations.Lock;
  try
    FOperationBlock := Operations.FOperationBlock;
    FIsOnlyOperationBlock := Operations.FIsOnlyOperationBlock;
    FOperationsHashTree.CopyFromHashTree(Operations.FOperationsHashTree);
    if Assigned(FSafeBoxTransaction) and Assigned(Operations.FSafeBoxTransaction) then
    begin
      FSafeBoxTransaction.CopyFrom(Operations.FSafeBoxTransaction);
    end;
    FDigest_Part1 := Operations.FDigest_Part1;
    FDigest_Part2_Payload := Operations.FDigest_Part2_Payload;
    FDigest_Part3 := Operations.FDigest_Part3;
  finally
    Operations.Unlock;
    Unlock;
  end;
end;

procedure TBlock.CopyFromExceptAddressKey(Operations: TBlock);
var
  lastopb: TBlockHeader;
begin
  Lock;
  try
    if Self = Operations then
      exit;
    lastopb := FOperationBlock;
    FOperationBlock := Operations.FOperationBlock;
    FOperationBlock.account_key := lastopb.account_key; // Except AddressKey
    FOperationBlock.compact_target := bank.AccountStorage.GetActualCompactTargetHash
      (FOperationBlock.protocol_version = CT_PROTOCOL_2);
    FIsOnlyOperationBlock := Operations.FIsOnlyOperationBlock;
    FOperationsHashTree.CopyFromHashTree(Operations.FOperationsHashTree);
    FOperationBlock.operations_hash := FOperationsHashTree.HashTree;
    if Assigned(FSafeBoxTransaction) and Assigned(Operations.FSafeBoxTransaction) then
    begin
      FSafeBoxTransaction.CopyFrom(Operations.FSafeBoxTransaction);
    end;
    // Recalc all
    CalcProofOfWork(true, FOperationBlock.proof_of_work);
  finally
    Unlock;
  end;
end;

function TBlock.Count: Integer;
begin
  Result := FOperationsHashTree.OperationsCount;
end;

constructor TBlock.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOperationsLock := TPCCriticalSection.Create('TPCOperationsComp_OPERATIONSLOCK');
  FDisableds := 0;
  FStreamPoW := TMemoryStream.Create;
  FStreamPoW.Position := 0;
  FOperationsHashTree := TTransactionHashTree.Create;
  FOperationsHashTree.OnChanged := OnOperationsHashTreeChanged;
  FBank := nil;
  FOperationBlock := GetFirstBlock;
  FSafeBoxTransaction := nil;
  if Assigned(AOwner) and (AOwner is TBlockManagerBase) then
  begin
    bank := TBlockManagerBase(AOwner);
  end
  else
    Clear(true);
end;

destructor TBlock.Destroy;
begin
  FOperationsLock.Acquire;
  try
    Clear(true);
    FreeAndNil(FOperationsHashTree);
    if Assigned(FSafeBoxTransaction) then
    begin
      FreeAndNil(FSafeBoxTransaction);
    end;
    FreeAndNil(FStreamPoW);
  finally
    FreeAndNil(FOperationsLock);
  end;
  inherited;
end;

class function TBlock.EqualsOperationBlock(const OperationBlock1, OperationBlock2: TBlockHeader): Boolean;
begin

  Result := (OperationBlock1.Block = OperationBlock2.Block) and
    (TAccountKey.EqualAccountKeys(OperationBlock1.account_key, OperationBlock2.account_key)) and
    (OperationBlock1.reward = OperationBlock2.reward) and (OperationBlock1.Fee = OperationBlock2.Fee) and
    (OperationBlock1.protocol_version = OperationBlock2.protocol_version) and
    (OperationBlock1.protocol_available = OperationBlock2.protocol_available) and
    (OperationBlock1.timestamp = OperationBlock2.timestamp) and
    (OperationBlock1.compact_target = OperationBlock2.compact_target) and
    (OperationBlock1.nonce = OperationBlock2.nonce) and (OperationBlock1.block_payload = OperationBlock2.block_payload)
    and (OperationBlock1.initial_safe_box_hash = OperationBlock2.initial_safe_box_hash) and
    (OperationBlock1.operations_hash = OperationBlock2.operations_hash) and
    (OperationBlock1.proof_of_work = OperationBlock2.proof_of_work);
end;

function TBlock.GetAccountKey: TAccountKey;
begin
  Result := FOperationBlock.account_key;
end;

function TBlock.GetBlockPayload: TRawBytes;
begin
  Result := FOperationBlock.block_payload;
end;

class function TBlock.GetFirstBlock: TBlockHeader;
begin
  Result := CT_OperationBlock_NUL;
end;

function TBlock.GetnOnce: Cardinal;
begin
  Result := FOperationBlock.nonce;
end;

function TBlock.GetOperation(index: Integer): ITransaction;
begin
  Result := FOperationsHashTree.GetOperation(index);
end;

function TBlock.Gettimestamp: Cardinal;
begin
  Result := FOperationBlock.timestamp;
end;

function TBlock.LoadBlockFromStorage(Stream: TStream; var errors: AnsiString): Boolean;
begin
  Result := LoadBlockFromStreamExt(Stream, true, errors);
end;

function TBlock.LoadBlockFromStream(Stream: TStream; var errors: AnsiString): Boolean;
begin
  Result := LoadBlockFromStreamExt(Stream, false, errors);
end;

function TBlock.LoadBlockFromStreamExt(Stream: TStream; LoadingFromStorage: Boolean; var errors: AnsiString): Boolean;
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
    errors := 'Invalid protocol structure. Check application version!';
    if (Stream.Size - Stream.Position < 5) then
      exit;
    Stream.Read(soob, 1);
    // About soob var:
    // In build prior to 1.0.4 soob only can have 2 values: 0 or 1
    // In build 1.0.4 soob can has 2 more values: 2 or 3
    // In build 2.0 soob can has 1 more value: 4
    // In future, old values 0 and 1 will no longer be used!
    // - Value 0 and 2 means that contains also operations
    // - Value 1 and 3 means that only contains operationblock info
    // - Value 2 and 3 means that contains protocol info prior to block number
    // - Value 4 means that is loading from storage using protocol v2 (so, includes always operations)
    load_protocol_v2 := false;
    if (soob in [0, 2]) then
      FIsOnlyOperationBlock := false
    else if (soob in [1, 3]) then
      FIsOnlyOperationBlock := true
    else if (soob in [4]) then
    begin
      FIsOnlyOperationBlock := false;
      load_protocol_v2 := true;
    end
    else
    begin
      errors := 'Invalid value in protocol header! Found:' + Inttostr(soob) +
        ' - Check if your application version is Ok';
      exit;
    end;

    if (soob in [2, 3, 4]) then
    begin
      Stream.Read(FOperationBlock.protocol_version, Sizeof(FOperationBlock.protocol_version));
      Stream.Read(FOperationBlock.protocol_available, Sizeof(FOperationBlock.protocol_available));
    end
    else
    begin
      // We assume that protocol_version is 1 and protocol_available is 0
      FOperationBlock.protocol_version := 1;
      FOperationBlock.protocol_available := 0;
    end;

    if Stream.Read(FOperationBlock.Block, Sizeof(FOperationBlock.Block)) < 0 then
      exit;

    if TStreamOp.ReadAnsiString(Stream, m) < 0 then
      exit;
    FOperationBlock.account_key := TAccountKey.FromRawString(m);
    if Stream.Read(FOperationBlock.reward, Sizeof(FOperationBlock.reward)) < 0 then
      exit;
    if Stream.Read(FOperationBlock.Fee, Sizeof(FOperationBlock.Fee)) < 0 then
      exit;
    if Stream.Read(FOperationBlock.timestamp, Sizeof(FOperationBlock.timestamp)) < 0 then
      exit;
    if Stream.Read(FOperationBlock.compact_target, Sizeof(FOperationBlock.compact_target)) < 0 then
      exit;
    if Stream.Read(FOperationBlock.nonce, Sizeof(FOperationBlock.nonce)) < 0 then
      exit;
    if TStreamOp.ReadAnsiString(Stream, FOperationBlock.block_payload) < 0 then
      exit;
    if TStreamOp.ReadAnsiString(Stream, FOperationBlock.initial_safe_box_hash) < 0 then
      exit;
    if TStreamOp.ReadAnsiString(Stream, FOperationBlock.operations_hash) < 0 then
      exit;
    if TStreamOp.ReadAnsiString(Stream, FOperationBlock.proof_of_work) < 0 then
      exit;
    if FIsOnlyOperationBlock then
    begin
      Result := true;
      exit;
    end;
    // Fee will be calculated for each operation. Set it to 0 and check later for integrity
    lastfee := OperationBlock.Fee;
    FOperationBlock.Fee := 0;
    Result := FOperationsHashTree.LoadFromStream(Stream, LoadingFromStorage, load_protocol_v2, errors);
    if not Result then
    begin
      exit;
    end;
    FOperationBlock.Fee := FOperationsHashTree.TotalFee;
    FOperationBlock.operations_hash := FOperationsHashTree.HashTree;
    Calc_Digest_Parts;
    // Validation control:
    if (lastfee <> OperationBlock.Fee) then
    begin
      errors := 'Corrupted operations fee old:' + Inttostr(lastfee) + ' new:' + Inttostr(OperationBlock.Fee);
      for i := 0 to FOperationsHashTree.OperationsCount - 1 do
      begin
        errors := errors + ' Op' + Inttostr(i + 1) + ':' + FOperationsHashTree.GetOperation(i).ToString;
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
    if AComponent = FBank then
    begin
      FBank := nil;
      FreeAndNil(FSafeBoxTransaction);
    end;
  end;
end;

class function TBlock.OperationBlockToText(OperationBlock: TBlockHeader): AnsiString;
begin
  Result := Format('Block:%d Timestamp:%d Reward:%d Fee:%d Target:%d PoW:%s',
    [OperationBlock.Block, OperationBlock.timestamp, OperationBlock.reward, OperationBlock.Fee,
    OperationBlock.compact_target, TCrypto.ToHexaString(OperationBlock.proof_of_work)]);
end;

procedure TBlock.SanitizeOperations;
{ This function check operationblock with bank and updates itself if necessary
  Then checks if operations are ok, and deletes old ones.
  Finally calculates new operation pow
  It's used when a new account has beed found by other chanels (miners o nodes...)
}
var
  i, n, lastn: Integer;
  op: ITransaction;
  errors: AnsiString;
  aux, aux2: TTransactionHashTree;
begin
  Lock;
  try
    FOperationBlock.timestamp := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
    if Assigned(FBank) then
    begin
      FOperationBlock.protocol_version := bank.AccountStorage.CurrentProtocol;
      if (FOperationBlock.protocol_version = CT_PROTOCOL_1) and (FBank.AccountStorage.CanUpgradeToProtocol2) then
      begin
        TLog.NewLog(ltinfo, Classname, 'New miner protocol version to 2 at sanitize');
        FOperationBlock.protocol_version := CT_PROTOCOL_2;
      end;
      FOperationBlock.Block := bank.BlocksCount;
      FOperationBlock.reward := TMicroCoinProtocol.GetRewardForNewLine(bank.BlocksCount);
      FOperationBlock.compact_target := bank.AccountStorage.GetActualCompactTargetHash
        (FOperationBlock.protocol_version = CT_PROTOCOL_2);
      FOperationBlock.initial_safe_box_hash := bank.AccountStorage.AccountStorageHash;
      if bank.LastOperationBlock.timestamp > FOperationBlock.timestamp then
        FOperationBlock.timestamp := bank.LastOperationBlock.timestamp;
    end
    else
    begin
      FOperationBlock.Block := 0;
      FOperationBlock.reward := TMicroCoinProtocol.GetRewardForNewLine(0);
      FOperationBlock.compact_target := CT_MinCompactTarget;
      FOperationBlock.initial_safe_box_hash := TCrypto.DoSha256(CT_Genesis_Magic_String_For_Old_Block_Hash);
      FOperationBlock.protocol_version := CT_PROTOCOL_1;
    end;
    FOperationBlock.proof_of_work := '';
    FOperationBlock.protocol_available := CT_BlockChain_Protocol_Available;
    n := 0;
    FOperationBlock.Fee := 0;
    //
    SafeBoxTransaction.CleanTransaction;
    //
    aux := TTransactionHashTree.Create;
    try
      lastn := FOperationsHashTree.OperationsCount;
      for i := 0 to lastn - 1 do
      begin
        op := FOperationsHashTree.GetOperation(i);
        if (op.ApplyTransaction(SafeBoxTransaction, errors)) then
        begin
          inc(n);
          aux.AddTransactionToHashTree(op);
          inc(FOperationBlock.Fee, op.Fee);
          TLog.NewLog(ltdebug, Classname, 'Sanitizing (pos:' + Inttostr(i + 1) + '/' + Inttostr(lastn) + '): ' +
            op.ToString);
        end;
      end;
    finally
      aux2 := FOperationsHashTree;
      FOperationsHashTree := aux;
      aux2.Free;
      FOperationBlock.operations_hash := FOperationsHashTree.HashTree;
    end;
  finally
    CalcProofOfWork(true, FOperationBlock.proof_of_work);
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

function TBlock.SaveBlockToStream(save_only_OperationBlock: Boolean; Stream: TStream): Boolean;
begin
  Result := SaveBlockToStreamExt(save_only_OperationBlock, Stream, false);
end;

function TBlock.SaveBlockToStreamExt(save_only_OperationBlock: Boolean; Stream: TStream;
  SaveToStorage: Boolean): Boolean;
var
  soob: Byte;
begin
  Lock;
  try
    if save_only_OperationBlock then
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
      if (SaveToStorage) then
      begin
        // Introduced on protocol v2: soob = 4 when saving to storage
        soob := 4;
      end;
    end;
    Stream.Write(soob, 1);
    if (soob >= 2) then
    begin
      Stream.Write(FOperationBlock.protocol_version, Sizeof(FOperationBlock.protocol_version));
      Stream.Write(FOperationBlock.protocol_available, Sizeof(FOperationBlock.protocol_available));
    end;
    //
    Stream.Write(FOperationBlock.Block, Sizeof(FOperationBlock.Block));
    //
    TStreamOp.WriteAnsiString(Stream, FOperationBlock.account_key.ToRawString);
    Stream.Write(FOperationBlock.reward, Sizeof(FOperationBlock.reward));
    Stream.Write(FOperationBlock.Fee, Sizeof(FOperationBlock.Fee));
    Stream.Write(FOperationBlock.timestamp, Sizeof(FOperationBlock.timestamp));
    Stream.Write(FOperationBlock.compact_target, Sizeof(FOperationBlock.compact_target));
    Stream.Write(FOperationBlock.nonce, Sizeof(FOperationBlock.nonce));
    TStreamOp.WriteAnsiString(Stream, FOperationBlock.block_payload);
    TStreamOp.WriteAnsiString(Stream, FOperationBlock.initial_safe_box_hash);
    TStreamOp.WriteAnsiString(Stream, FOperationBlock.operations_hash);
    TStreamOp.WriteAnsiString(Stream, FOperationBlock.proof_of_work);
    { Basic size calculation:
      protocols : 2 words = 4 bytes
      block : 4 bytes
      Account_key (VARIABLE LENGTH) at least 2 + 34 + 34 for secp256k1 key = 70 bytes
      reward, fee, timestamp, compact_target, nonce = 8+8+4+4+4 = 28 bytes
      payload (VARIABLE LENGTH) minimum 2 bytes... but usually 40 by average = 40 bytes
      sbh, operations_hash, pow ( 32 + 32 + 32 ) =  96 bytes
      Total, by average: 242 bytes
    }
    if (not save_only_OperationBlock) then
    begin
      Result := FOperationsHashTree.SaveToStream(Stream, SaveToStorage);
    end
    else
      Result := true;
  finally
    Unlock;
  end;
end;

class function TBlock.SaveOperationBlockToStream(const OperationBlock: TBlockHeader; Stream: TStream): Boolean;
var
  soob: Byte;
begin
  soob := 3;
  Stream.Write(soob, 1);
  Stream.Write(OperationBlock.protocol_version, Sizeof(OperationBlock.protocol_version));
  Stream.Write(OperationBlock.protocol_available, Sizeof(OperationBlock.protocol_available));
  //
  Stream.Write(OperationBlock.Block, Sizeof(OperationBlock.Block));
  //
  TStreamOp.WriteAnsiString(Stream, OperationBlock.account_key.ToRawString);
  Stream.Write(OperationBlock.reward, Sizeof(OperationBlock.reward));
  Stream.Write(OperationBlock.Fee, Sizeof(OperationBlock.Fee));
  Stream.Write(OperationBlock.timestamp, Sizeof(OperationBlock.timestamp));
  Stream.Write(OperationBlock.compact_target, Sizeof(OperationBlock.compact_target));
  Stream.Write(OperationBlock.nonce, Sizeof(OperationBlock.nonce));
  TStreamOp.WriteAnsiString(Stream, OperationBlock.block_payload);
  TStreamOp.WriteAnsiString(Stream, OperationBlock.initial_safe_box_hash);
  TStreamOp.WriteAnsiString(Stream, OperationBlock.operations_hash);
  TStreamOp.WriteAnsiString(Stream, OperationBlock.proof_of_work);
  Result := true;
end;

procedure TBlock.SetAccountKey(const value: TAccountKey);
begin
  Lock;
  try
    if value.ToRawString = FOperationBlock.account_key.ToRawString then
      exit;
    FOperationBlock.account_key := value;
    Calc_Digest_Parts;
  finally
    Unlock;
  end;
end;

procedure TBlock.SetBank(const value: TBlockManagerBase);
begin
  if FBank = value then
    exit;
  if Assigned(FBank) then
  begin
    FreeAndNil(FSafeBoxTransaction);
  end;
  FBank := value;
  if Assigned(value) then
  begin
    value.FreeNotification(Self);
    FSafeBoxTransaction := TAccountTransaction.Create(FBank.AccountStorage);
  end;
  Clear(true);
end;

procedure TBlock.SetBlockPayload(const value: TRawBytes);
var
  i: Integer;
begin
  Lock;
  try
    if value = FOperationBlock.block_payload then
      exit;
    if length(value) > CT_MaxPayloadSize then
      exit;
    // Checking Miner Payload valid chars
    for i := 1 to length(value) do
    begin
      if not(value[i] in [#32 .. #254]) then
      begin
        exit;
      end;
    end;
    FOperationBlock.block_payload := value;
    CalcProofOfWork(true, FOperationBlock.proof_of_work);
  finally
    Unlock;
  end;
end;

procedure TBlock.OnOperationsHashTreeChanged(Sender: TObject);
begin
  FOperationBlock.operations_hash := FOperationsHashTree.HashTree;
  Calc_Digest_Part3;
end;

procedure TBlock.SetnOnce(const value: Cardinal);
begin
  Lock;
  try
    FOperationBlock.nonce := value;
    CalcProofOfWork(false, FOperationBlock.proof_of_work);
  finally
    Unlock;
  end;
end;

procedure TBlock.Settimestamp(const value: Cardinal);
begin
  Lock;
  try
    if FOperationBlock.timestamp = value then
      exit; // No change, nothing to do
    FOperationBlock.timestamp := value;
    CalcProofOfWork(false, FOperationBlock.proof_of_work);
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
    if Assigned(bank) then
    begin
      if bank.LastOperationBlock.timestamp > ts then
        ts := bank.LastOperationBlock.timestamp;
    end;
    timestamp := ts;
  finally
    Unlock;
  end;
end;

function TBlock.ValidateOperationBlock(var errors: AnsiString): Boolean;
var
  lastpow: AnsiString;
  i: Integer;
begin
  errors := '';
  Result := false;
  Lock;
  try
    if not Assigned(SafeBoxTransaction) then
    begin
      errors := 'ERROR DEV 20170523-1';
      exit;
    end;
    if not Assigned(SafeBoxTransaction.FreezedAccountStorage) then
    begin
      errors := 'ERROR DEV 20170523-2';
      exit;
    end;
    // Check OperationBlock info:
    if not SafeBoxTransaction.FreezedAccountStorage.IsValidNewBlockHeader(OperationBlock, true, errors) then
      exit;
    // Execute SafeBoxTransaction operations:
    SafeBoxTransaction.Rollback;
    for i := 0 to Count - 1 do
    begin
      if not Operation[i].ApplyTransaction(SafeBoxTransaction, errors) then
      begin
        errors := 'Error executing operation ' + Inttostr(i + 1) + '/' + Inttostr(Count) + ': ' + errors;
        exit;
      end;
    end;
    // Check OperationsHash value is valid
    if FOperationsHashTree.HashTree <> OperationBlock.operations_hash then
    begin
      errors := 'Invalid Operations Hash ' + TCrypto.ToHexaString(OperationBlock.operations_hash) + '<>' +
        TCrypto.ToHexaString(FOperationsHashTree.HashTree);
      exit;
    end;
    // Check OperationBlock with SafeBox info:
    if (SafeBoxTransaction.FreezedAccountStorage.TotalBalance <> (SafeBoxTransaction.TotalBalance +
      SafeBoxTransaction.TotalFee)) then
    begin
      errors := Format('Invalid integrity balance at SafeBox. Actual Balance:%d  New Balance:(%d + fee %d = %d)',
        [SafeBoxTransaction.FreezedAccountStorage.TotalBalance, SafeBoxTransaction.TotalBalance,
        SafeBoxTransaction.TotalFee, SafeBoxTransaction.TotalBalance + SafeBoxTransaction.TotalFee]);
      exit;
    end;
    // Check fee value
    if (SafeBoxTransaction.TotalFee <> OperationBlock.Fee) then
    begin
      errors := Format
        ('Invalid fee integrity at SafeBoxTransaction. New Balance:(%d + fee %d = %d)  OperationBlock.fee:%d',
        [SafeBoxTransaction.TotalBalance, SafeBoxTransaction.TotalFee, SafeBoxTransaction.TotalBalance +
        SafeBoxTransaction.TotalFee, OperationBlock.Fee]);
      exit;
    end;

    Result := true;
  finally
    Unlock;
  end;
end;

procedure TBlock.Lock;
begin
  FOperationsLock.Acquire;
end;

procedure TBlock.Unlock;
begin
  FOperationsLock.Release;
end;

end.
