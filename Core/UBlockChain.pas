unit UBlockChain;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}
{
  Copyright (c) Albert Molina 2016 - 2018 original code from PascalCoin https://pascalcoin.org/

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

}

interface

uses
  Classes, UCrypto, ULog, UThread, SyncObjs,
  MicroCoin.Transaction.Base, MicroCoin.BlockChain.Base,
  MicroCoin.Transaction.Manager, MicroCoin.Transaction.HashTree,
  MicroCoin.Account.AccountKey, MicroCoin.BlockChain.BlockHeader,
  MicroCoin.Account.Storage, MicroCoin.Account.Transaction, MicroCoin.BlockChain.Protocol,
  MicroCoin.BlockChain.Storage, MicroCoin.BlockChain.Block;
{$I config.inc}
{

  Bank BlockChain:

  Safe Box content: (See Unit "UAccounts.pas" to see pascal code)
  +--------------+--------------------------------------------------+------------+------------+
  + BlockAccount + Each BlockAccount has N "Account"                +  Timestamp + Block Hash +
  +              +--------------------------------------------------+            +            +
  +              + Addr B0 + Public key +  Balance + updated + n_op +            +            +
  +              + Addr B1 + Public key +  Balance + updated + n_op +            +            +
  +              + ......                                           +            +            +
  +              + Addr B4 + Public key +  Balance + updated + n_op +            +            +
  +--------------+---------+----------------------------------------+------------+------------+
  +            0 +       0 + pk_aaaaaaa + 100.0000 +       0 +    0 + 1461701856 +   Sha256() +
  +              +       1 + pk_aaaaaaa +   0.0000 +       0 +    0 +            + = h1111111 +
  +              +       2 + pk_aaaaaaa +   0.0000 +       0 +    0 +            +            +
  +              +       3 + pk_aaaaaaa +   0.0000 +       0 +    0 +            +            +
  +              +       4 + pk_aaaaaaa +   0.0000 +       0 +    0 +            +            +
  +--------------+---------+----------------------------------------+------------+------------+
  +            1 +       5 + pk_bbbbbbb + 100.0000 +       0 +    0 + 1461702960 +   Sha256() +
  +              +       6 + pk_bbbbbbb +   0.0000 +       0 +    0 +            + = h2222222 +
  +              +       7 + pk_bbbbbbb +   0.0000 +       0 +    0 +            +            +
  +              +       8 + pk_bbbbbbb +   0.0000 +       0 +    0 +            +            +
  +              +       9 + pk_bbbbbbb +   0.0000 +       0 +    0 +            +            +
  +--------------+---------+----------------------------------------+------------+------------+
  +     ................                                                                      +
  +--------------+---------+----------------------------------------+------------+------------+
  +            5 +      25 + pk_bbbbbbb + 100.0000 +       0 +    0 + 1461713484 +   Sha256() +
  +              +      26 + pk_bbbbbbb +   0.0000 +       0 +    0 +            + = h3333333 +
  +              +      27 + pk_bbbbbbb +   0.0000 +       0 +    0 +            +            +
  +              +      28 + pk_bbbbbbb +   0.0000 +       0 +    0 +            +            +
  +              +      29 + pk_bbbbbbb +   0.0000 +       0 +    0 +            +            +
  +--------------+---------+----------------------------------------+------------+------------+
  +  Safe Box Hash  : Sha256(h1111111 + h2222222 + ... + h3333333) = sbh_A1                   +
  +-------------------------------------------------------------------------------------------+

  BlockChain:

  To generate a BlockChain (block X) we need the previous "Safe Box Hash"
  (the Safe Box Hash number X-1, generated when BlockChain X-1 was generated)
  Each BlockChain block generates a new "Safe Box" with a new "Safe Box Hash"

  With this method, Safe Box is unique after a BlockChain, so we can assume
  that a hard coded Safe Box X is the same that to load all previous BlockChain
  from 0 to X. Conclusion: It's not necessary historical operations (block chains)
  to work with Micro Coin

  Some BlockChain fields:
  +-------+-----------------+----------+------+-----+-----+------------+--------+-------+---------------+---------------+-----------------+---------------+-----------------------+
  + Block + Account key     +  reward  + fee  + protocols + timestamp  + target + nonce + Miner Payload + safe box hash + operations hash + Proof of Work + Operations stream     +
  +-------+-----------------+----------+------+-----+-----+------------+--------+-------+---------------+---------------+-----------------+---------------+-----------------------+
  +     0 + (hard coded)    + 100.0000 +    0 +   1 +   0 + 1461701856 + trgt_1 +  ...  + (Hard coded)  +  (Hard coded) + Sha256(Operat.) + 000000C3F5... + Operations of block 0 +
  +-------+-----------------+----------+------+-----+-----+------------+--------+-------+---------------+---------------+-----------------+---------------+-----------------------+
  +     1 + hhhhhhhhhhhhhhh + 100.0000 +    0 +   1 +   0 + 1461701987 + trgt_1 +  ...  +      ...      + SFH block 0   + Sha256(Operat.) + 000000A987... + Operations of block 1 +
  +-------+-----------------+----------+------+-----+-----+------------+--------+-------+---------------+---------------+-----------------+---------------+-----------------------+
  +     2 + iiiiiiiiiiiiiii + 100.0000 + 0.43 +   1 +   0 + 1461702460 + trgt_1 +  ...  +      ...      + SFH block 1   + Sha256(Operat.) + 0000003A1C... + Operations of block 2 +
  +-------+-----------------+----------+------+-----+-----+------------+--------+-------+---------------+---------------+-----------------+---------------+-----------------------+
  +       .....                                                                                                                                                   +
  +-------+-----------------+----------+------+-----+-----+------------+--------+-------+---------------+---------------+-----------------+---------------+-----------------------+

  Considerations:
  - Account Key: Is a public key that will have all new generated Accounts of the Safe Box
  - Protocols are 2 values: First indicate protocol of this block, second future candidate protocol that is allowed by miner who made this. (For protocol upgrades)
  - Safe Box Has: Each Block of the Bloch Chain is made in base of a previous Safe Box. This value hard codes consistency
  - Operations Stream includes all the operations that will be made to the Safe Box after this block is generated. A hash value of Operations stream is "Operations Hash"

  Operations:

  Each Block of the Block Chain has its owns operations that will be used to change Safe Box after block is completed and included in BlockChain

  Operations of actual Protocol (version 1) can be one of this:
  - Transaction from 1 account to 1 account
  - Change AccountKey of an account
  - Recover balance from an unused account (lost keys)

  Each Operation has a Hash value that is used to generate "Operations Hash". Operations Hash is a Sha256 of all the Operations included
  inside it hashed like a Merkle Tree.

  In unit "UOpTransaction.pas" you can see how each Operation Works.

}

type
  TBlockManager = class;
  TBlockManagerNotify = class;

  { TPCOperationsComp }


  TBlockManagerLog = procedure(Sender: TBlockManager; Operations: TBlock; Logtype: TLogType; Logtxt: AnsiString) of object;

  TBlockManagerNotify = class(TComponent)
  private
    FOnNewBlock: TNotifyEvent;
    FBank: TBlockManager;
    procedure SetBank(const value: TBlockManager);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure NotifyNewBlock;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property bank: TBlockManager read FBank write SetBank;
    property OnNewBlock: TNotifyEvent read FOnNewBlock write FOnNewBlock;
  end;


  { TPCBank }

  TBlockManager = class(TBank)
  private
    FStorage: TStorage;
    FSafeBox: TAccountStorage;
    FLastBlockCache: TBlock;
    FLastOperationBlock: TBlockHeader;
    FIsRestoringFromFile: Boolean;
    FUpgradingToV2: Boolean;
    FOnLog: TBlockManagerLog;
    FBankLock: TPCCriticalSection;
    FNotifyList: TList;
    FStorageClass: TStorageClass;
    function GetStorage: TStorage;
    procedure SetStorageClass(const value: TStorageClass);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BlocksCount: Cardinal;
    function AccountsCount: Cardinal;
    procedure AssignTo(Dest: TPersistent); override;
    function GetActualTargetSecondsAverage(BackBlocks: Cardinal): Real;
    function GetTargetSecondsAverage(FromBlock, BackBlocks: Cardinal): Real;
    function LoadBankFromStream(Stream: TStream; useSecureLoad: Boolean; var errors: AnsiString): Boolean; override;
    procedure Clear;
    function LoadOperations(Operations: TBlock; Block: Cardinal): Boolean;
    property SafeBox: TAccountStorage read FSafeBox;
    function AddNewBlockChainBlock(Operations: TBlock; MaxAllowedTimestamp: Cardinal; var newBlock: TAccountStorageEntry; var errors: AnsiString): Boolean;
    procedure DiskRestoreFromOperations(max_block: Int64);
    procedure NewLog(Operations: TBlock; Logtype: TLogType; Logtxt: AnsiString);
    property OnLog: TBlockManagerLog read FOnLog write FOnLog;
    property LastOperationBlock: TBlockHeader read FLastOperationBlock;
    // TODO: Use
    property Storage: TStorage read GetStorage;
    property StorageClass: TStorageClass read FStorageClass write SetStorageClass;
    function IsReady(var CurrentProcess: AnsiString): Boolean;
    property LastBlockFound: TBlock read FLastBlockCache;
    property UpgradingToV2: Boolean read FUpgradingToV2;
  end;

implementation

uses
  {Messages,}
  SysUtils, Variants, {Graphics,}
  {Controls, Forms,}
  {StdCtrls,}
  UTime, UConst;

{ TPCBank }

function TBlockManager.AccountsCount: Cardinal;
begin
  Result := FSafeBox.AccountsCount;
end;

function TBlockManager.AddNewBlockChainBlock(Operations: TBlock; MaxAllowedTimestamp: Cardinal; var newBlock: TAccountStorageEntry; var errors: AnsiString): Boolean;
var
  buffer, PoW: AnsiString;
  i: Integer;
begin
  TPCThread.ProtectEnterCriticalSection(Self, FBankLock);
  try
    Result := false;
    errors := '';
    try
      if not Operations.ValidateOperationBlock(errors) then
      begin
        exit;
      end;
      if (Operations.OperationBlock.Block > 0) then
      begin
        if ((MaxAllowedTimestamp > 0) and (Operations.OperationBlock.timestamp > MaxAllowedTimestamp)) then
        begin
          errors := 'Invalid timestamp (Future time: New timestamp ' + Inttostr(Operations.OperationBlock.timestamp) + ' > max allowed ' + Inttostr(MaxAllowedTimestamp) + ')';
          exit;
        end;
      end;
      // Ok, include!
      // WINNER !!!
      // Congrats!

      if not Operations.SafeBoxTransaction.Commit(Operations.OperationBlock, errors) then
      begin
        exit;
      end;

      newBlock := SafeBox.Block(SafeBox.BlocksCount - 1);

      // Initialize values
      FLastOperationBlock := Operations.OperationBlock;
      // log it!
      NewLog(Operations, ltupdate,
        Format('New block height:%d nOnce:%d timestamp:%d Operations:%d Fee:%d SafeBoxBalance:%d=%d PoW:%s Operations previous Safe Box hash:%s Future old Safe Box hash for next block:%s',
        [Operations.OperationBlock.Block, Operations.OperationBlock.nonce, Operations.OperationBlock.timestamp, Operations.Count, Operations.OperationBlock.Fee, SafeBox.TotalBalance,
        Operations.SafeBoxTransaction.TotalBalance, TCrypto.ToHexaString(Operations.OperationBlock.proof_of_work), TCrypto.ToHexaString(Operations.OperationBlock.initial_safe_box_hash),
        TCrypto.ToHexaString(SafeBox.AccountStorageHash)]));
      // Save Operations to disk
      if not FIsRestoringFromFile then
      begin
        Storage.SaveBlockChainBlock(Operations);
      end;
      FLastBlockCache.CopyFrom(Operations);
      Operations.Clear(true);
      Result := true;
    finally
      if not Result then
        NewLog(Operations, lterror, 'Invalid new block ' + Inttostr(Operations.OperationBlock.Block) + ': ' + errors);
    end;
  finally
    FBankLock.Release;
  end;
  if Result then
  begin
    for i := 0 to FNotifyList.Count - 1 do
    begin
      TBlockManagerNotify(FNotifyList.Items[i]).NotifyNewBlock;
    end;
  end;
end;

procedure TBlockManager.AssignTo(Dest: TPersistent);
var
  d: TBlockManager;
begin
  if (not(Dest is TBlockManager)) then
  begin
    inherited;
    exit;
  end;
  if (Self = Dest) then
    exit;

  d := TBlockManager(Dest);
  d.SafeBox.CopyFrom(SafeBox);
  d.FLastOperationBlock := FLastOperationBlock;
  d.FIsRestoringFromFile := FIsRestoringFromFile;
  d.FLastBlockCache.CopyFrom(FLastBlockCache);
end;

function TBlockManager.BlocksCount: Cardinal;
begin
  Result := SafeBox.BlocksCount;
end;

procedure TBlockManager.Clear;
begin
  SafeBox.Clear;
  FLastOperationBlock := TBlock.GetFirstBlock;
  FLastOperationBlock.initial_safe_box_hash := TCrypto.DoSha256(CT_Genesis_Magic_String_For_Old_Block_Hash);
  // Genesis hash
  FLastBlockCache.Clear(true);
  NewLog(nil, ltupdate, 'Clear Bank');
end;

constructor TBlockManager.Create(AOwner: TComponent);
begin
  inherited;
  FStorage := nil;
  FStorageClass := nil;
  FBankLock := TPCCriticalSection.Create('TPCBank_BANKLOCK');
  FIsRestoringFromFile := false;
  FOnLog := nil;
  FSafeBox := TAccountStorage.Create;
  FNotifyList := TList.Create;
  FLastBlockCache := TBlock.Create(nil);
  FIsRestoringFromFile := false;
  FUpgradingToV2 := false;
  Clear;
end;

destructor TBlockManager.Destroy;
var
  step: string;
begin
  try
    step := 'Deleting critical section';
    FreeAndNil(FBankLock);
    step := 'Clear';
    Clear;
    step := 'Destroying LastBlockCache';
    FreeAndNil(FLastBlockCache);
    step := 'Destroying SafeBox';
    FreeAndNil(FSafeBox);
    step := 'Destroying NotifyList';
    FreeAndNil(FNotifyList);
    step := 'Destroying Storage';
    FreeAndNil(FStorage);
    step := 'inherited';
    inherited;
  except
    on E: Exception do
    begin
      TLog.NewLog(lterror, Classname, 'Error destroying Bank step: ' + step + ' Errors (' + E.Classname + '): ' + E.Message);
      raise;
    end;
  end;
end;

procedure TBlockManager.DiskRestoreFromOperations(max_block: Int64);
var
  errors: AnsiString;
  newBlock: TAccountStorageEntry;
  Operations: TBlock;
  n: Int64;
begin
  if FIsRestoringFromFile then
  begin
    TLog.NewLog(lterror, Classname, 'Is Restoring!!!');
    raise Exception.Create('Is restoring!');
  end;
  TPCThread.ProtectEnterCriticalSection(Self, FBankLock);
  try
    FUpgradingToV2 := not Storage.HasUpgradedToVersion2;
    FIsRestoringFromFile := true;
    try
      Clear;
      Storage.Initialize;
      if (max_block < Storage.LastBlock) then
        n := max_block
      else
        n := Storage.LastBlock;
      Storage.RestoreBank(n);
      // Restore last blockchain
      if (BlocksCount > 0) and (SafeBox.CurrentProtocol = CT_PROTOCOL_1) then
      begin
        if not Storage.LoadBlockChainBlock(FLastBlockCache, BlocksCount - 1) then
        begin
          NewLog(nil, lterror, 'Cannot find blockchain ' + Inttostr(BlocksCount - 1) + ' so cannot accept bank current block ' + Inttostr(BlocksCount));
          Clear;
        end;
      end;
      NewLog(nil, ltinfo, 'Start restoring from disk operations (Max ' + Inttostr(max_block) + ') BlockCount: ' + Inttostr(BlocksCount) + ' Orphan: ' + Storage.Orphan);
      Operations := TBlock.Create(Self);
      try
        while ((BlocksCount <= max_block)) do
        begin
          if Storage.BlockExists(BlocksCount) then
          begin
            if Storage.LoadBlockChainBlock(Operations, BlocksCount) then
            begin
              SetLength(errors, 0);
              if not AddNewBlockChainBlock(Operations, 0, newBlock, errors) then
              begin
                NewLog(Operations, lterror, 'Error restoring block: ' + Inttostr(BlocksCount) + ' Errors: ' + errors);
                Storage.DeleteBlockChainBlocks(BlocksCount);
                break;
              end
              else
              begin
                // To prevent continuous saving...
{$IFDEF TESTNET}
                Storage.SaveBank;
{$ELSE}
                if (BlocksCount mod (CT_BankToDiskEveryNBlocks * 10)) = 0 then
                begin
                  Storage.SaveBank;
                end;
{$ENDIF}
              end;
            end
            else
              break;
          end
          else
            break;
        end;
        if FUpgradingToV2 then
          Storage.CleanupVersion1Data;
      finally
        Operations.Free;
      end;
      NewLog(nil, ltinfo, 'End restoring from disk operations (Max ' + Inttostr(max_block) + ') Orphan: ' + Storage.Orphan + ' Restored ' + Inttostr(BlocksCount) + ' blocks');
    finally
      FIsRestoringFromFile := false;
      FUpgradingToV2 := false;
    end;
  finally
    FBankLock.Release;
  end;
end;

function TBlockManager.GetActualTargetSecondsAverage(BackBlocks: Cardinal): Real;
var
  ts1, ts2: Int64;
begin
  if BlocksCount > BackBlocks then
  begin
    ts1 := SafeBox.Block(BlocksCount - 1).BlockHeader.timestamp;
    ts2 := SafeBox.Block(BlocksCount - BackBlocks - 1).BlockHeader.timestamp;
  end
  else if (BlocksCount > 1) then
  begin
    ts1 := SafeBox.Block(BlocksCount - 1).BlockHeader.timestamp;
    ts2 := SafeBox.Block(0).BlockHeader.timestamp;
    BackBlocks := BlocksCount - 1;
  end
  else
  begin
    Result := 0;
    exit;
  end;
  Result := (ts1 - ts2) / BackBlocks;
end;

function TBlockManager.GetTargetSecondsAverage(FromBlock, BackBlocks: Cardinal): Real;
var
  ts1, ts2: Int64;
begin
  if FromBlock >= BlocksCount then
  begin
    Result := 0;
    exit;
  end;
  if FromBlock > BackBlocks then
  begin
    ts1 := SafeBox.Block(FromBlock - 1).BlockHeader.timestamp;
    ts2 := SafeBox.Block(FromBlock - BackBlocks - 1).BlockHeader.timestamp;
  end
  else if (FromBlock > 1) then
  begin
    ts1 := SafeBox.Block(FromBlock - 1).BlockHeader.timestamp;
    ts2 := SafeBox.Block(0).BlockHeader.timestamp;
    BackBlocks := FromBlock - 1;
  end
  else
  begin
    Result := 0;
    exit;
  end;
  Result := (ts1 - ts2) / BackBlocks;
end;

function TBlockManager.GetStorage: TStorage;
begin
  if not Assigned(FStorage) then
  begin
    if not Assigned(FStorageClass) then
      raise Exception.Create('StorageClass not defined');
    FStorage := FStorageClass.Create(Self);
    FStorage.bank := Self;
  end;
  Result := FStorage;
end;

function TBlockManager.IsReady(var CurrentProcess: AnsiString): Boolean;
begin
  Result := false;
  CurrentProcess := '';
  if FIsRestoringFromFile then
  begin
    if FUpgradingToV2 then
      CurrentProcess := 'Migrating to version 2 format'
    else
      CurrentProcess := 'Restoring from file'
  end
  else
    Result := true;
end;

function TBlockManager.LoadBankFromStream(Stream: TStream; useSecureLoad: Boolean; var errors: AnsiString): Boolean;
var
  LastReadBlock: TAccountStorageEntry;
  i: Integer;
  auxSB: TAccountStorage;
begin
  auxSB := nil;
  try
    if useSecureLoad then
    begin
      // When on secure load will load Stream in a separate SafeBox, changing only real SafeBox if successfully
      auxSB := TAccountStorage.Create;
      Result := auxSB.LoadFromStream(Stream, true, LastReadBlock, errors);
      if not Result then
        exit;
    end;
    TPCThread.ProtectEnterCriticalSection(Self, FBankLock);
    try
      if Assigned(auxSB) then
      begin
        SafeBox.CopyFrom(auxSB);
      end
      else
      begin
        Result := SafeBox.LoadFromStream(Stream, false, LastReadBlock, errors);
      end;
      if not Result then
        exit;
      if SafeBox.BlocksCount > 0 then
        FLastOperationBlock := SafeBox.Block(SafeBox.BlocksCount - 1).BlockHeader
      else
      begin
        FLastOperationBlock := TBlock.GetFirstBlock;
        FLastOperationBlock.initial_safe_box_hash := TCrypto.DoSha256(CT_Genesis_Magic_String_For_Old_Block_Hash);
        // Genesis hash
      end;
    finally
      FBankLock.Release;
    end;
    for i := 0 to FNotifyList.Count - 1 do
    begin
      TBlockManagerNotify(FNotifyList.Items[i]).NotifyNewBlock;
    end;
  finally
    if Assigned(auxSB) then
      auxSB.Free;
  end;
end;

function TBlockManager.LoadOperations(Operations: TBlock; Block: Cardinal): Boolean;
begin
  TPCThread.ProtectEnterCriticalSection(Self, FBankLock);
  try
    if (Block > 0) and (Block = FLastBlockCache.OperationBlock.Block) then
    begin
      // Same as cache, sending cache
      Operations.CopyFrom(FLastBlockCache);
      Result := true;
    end
    else
    begin
      Result := Storage.LoadBlockChainBlock(Operations, Block);
    end;
  finally
    FBankLock.Release;
  end;
end;

procedure TBlockManager.NewLog(Operations: TBlock; Logtype: TLogType; Logtxt: AnsiString);
var
  s: AnsiString;
begin
  if Assigned(Operations) then
    s := Operations.Classname
  else
    s := Classname;
  TLog.NewLog(Logtype, s, Logtxt);
  if Assigned(FOnLog) then
    FOnLog(Self, Operations, Logtype, Logtxt);
end;

procedure TBlockManager.SetStorageClass(const value: TStorageClass);
begin
  if FStorageClass = value then
    exit;
  FStorageClass := value;
  if Assigned(FStorage) then
    FreeAndNil(FStorage);
end;


{ TPCBankNotify }

constructor TBlockManagerNotify.Create(AOwner: TComponent);
begin
  inherited;
  FBank := nil;
end;

destructor TBlockManagerNotify.Destroy;
begin
  bank := nil;
  inherited;
end;

procedure TBlockManagerNotify.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
    if AComponent = FBank then
      FBank := nil;
end;

procedure TBlockManagerNotify.NotifyNewBlock;
begin
  if Assigned(FOnNewBlock) then
    FOnNewBlock(bank);
end;

procedure TBlockManagerNotify.SetBank(const value: TBlockManager);
begin
  if Assigned(FBank) then
  begin
    FBank.FNotifyList.Remove(Self);
    FBank.RemoveFreeNotification(Self);
  end;
  FBank := value;
  if Assigned(FBank) then
  begin
    FBank.FreeNotification(Self);
    FBank.FNotifyList.Add(Self);
  end;
end;

end.
