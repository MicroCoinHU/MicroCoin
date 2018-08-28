unit MicroCoin.BlockChain.BlockManager;

{
  This unit contains code from PascalCoin:

  Copyright (c) Albert Molina 2016 - 2018 original code from PascalCoin https://pascalcoin.org/

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

}

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses Classes, UCrypto, ULog, UThread, SyncObjs,
  MicroCoin.Transaction.Base, MicroCoin.BlockChain.Base,
  MicroCoin.Transaction.Manager, MicroCoin.Transaction.HashTree,
  MicroCoin.Account.AccountKey, MicroCoin.BlockChain.BlockHeader,
  MicroCoin.Account.Storage, MicroCoin.Account.Transaction, MicroCoin.BlockChain.Protocol,
  MicroCoin.BlockChain.Storage, MicroCoin.BlockChain.Block;

{$I ../config.inc}

type
  TBlockManager = class;
  TBlockManagerNotify = class;

  TBlockManagerLog = procedure(Sender: TBlockManager; Operations: TBlock; Logtype: TLogType; Logtxt: AnsiString)
    of object;

  TBlockManagerNotify = class(TComponent)
  private
    FOnNewBlock: TNotifyEvent;
    FBlockManager: TBlockManager;
    procedure SetBlockManager(const value: TBlockManager);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure NotifyNewBlock;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property BlockManager: TBlockManager read FBlockManager write SetBlockManager;
    property OnNewBlock: TNotifyEvent read FOnNewBlock write FOnNewBlock;
  end;

  TBlockManager = class(TBlockManagerBase)
  private
    FStorage: TStorage;
    FAccountStorage: TAccountStorage;
    FLastBlockCache: TBlock;
    FLastOperationBlock: TBlockHeader;
    FIsRestoringFromFile: Boolean;
    FUpgradingToV2: Boolean;
    FOnLog: TBlockManagerLog;
    FAccountStorageLock: TPCCriticalSection;
    FNotifyList: TList;
    FStorageClass: TStorageClass;
    function GetStorage: TStorage;
    procedure SetStorageClass(const value: TStorageClass);
    function GetAccountStorage: TAccountStorage; override;
    function GetBlocksCount: Cardinal; override;
    function GetLastOperationBlock: TBlockHeader; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AccountsCount: Cardinal;
    procedure AssignTo(Dest: TPersistent); override;
    function GetActualTargetSecondsAverage(BackBlocks: Cardinal): Real;
    function GetTargetSecondsAverage(FromBlock, BackBlocks: Cardinal): Real;
    function LoadAccountsFromStream(Stream: TStream; useSecureLoad: Boolean; var errors: AnsiString): Boolean; override;
    procedure Clear;
    function LoadOperations(Operations: TBlock; Block: Cardinal): Boolean;
    function AddNewBlockChainBlock(Operations: TBlock; MaxAllowedTimestamp: Cardinal;
      var newBlock: TAccountStorageEntry; var errors: AnsiString): Boolean;
    procedure DiskRestoreFromOperations(max_block: Int64);
    procedure NewLog(Operations: TBlock; Logtype: TLogType; Logtxt: AnsiString);
    property OnLog: TBlockManagerLog read FOnLog write FOnLog;
    property LastOperationBlock: TBlockHeader read GetLastOperationBlock;
    // TODO: Use
    property Storage: TStorage read GetStorage;
    property StorageClass: TStorageClass read FStorageClass write SetStorageClass;
    function IsReady(var CurrentProcess: AnsiString): Boolean;
    property LastBlockFound: TBlock read FLastBlockCache;
    property UpgradingToV2: Boolean read FUpgradingToV2;
    property AccountStorage: TAccountStorage read GetAccountStorage;
    property BlocksCount: Cardinal read GetBlocksCount;
  end;

implementation

uses
  {Messages,}
    SysUtils, Variants, {Graphics,}
  {Controls, Forms,}
  {StdCtrls,}
  UTime, UConst;

function TBlockManager.AccountsCount: Cardinal;
begin
  Result := FAccountStorage.AccountsCount;
end;

function TBlockManager.AddNewBlockChainBlock(Operations: TBlock; MaxAllowedTimestamp: Cardinal;
  var newBlock: TAccountStorageEntry; var errors: AnsiString): Boolean;
var
  buffer, PoW: AnsiString;
  i: Integer;
begin
  TPCThread.ProtectEnterCriticalSection(Self, FAccountStorageLock);
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
          errors := 'Invalid timestamp (Future time: New timestamp ' + Inttostr(Operations.OperationBlock.timestamp) +
            ' > max allowed ' + Inttostr(MaxAllowedTimestamp) + ')';
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

      newBlock := AccountStorage.Block(AccountStorage.BlocksCount - 1);

      // Initialize values
      FLastOperationBlock := Operations.OperationBlock;
      // log it!
      NewLog(Operations, ltupdate,
        Format('New block height:%d nOnce:%d timestamp:%d Operations:%d Fee:%d SafeBoxBalance:%d=%d PoW:%s Operations previous Safe Box hash:%s Future old Safe Box hash for next block:%s',
        [Operations.OperationBlock.Block, Operations.OperationBlock.nonce, Operations.OperationBlock.timestamp,
        Operations.Count, Operations.OperationBlock.Fee, AccountStorage.TotalBalance,
        Operations.SafeBoxTransaction.TotalBalance, TCrypto.ToHexaString(Operations.OperationBlock.proof_of_work),
        TCrypto.ToHexaString(Operations.OperationBlock.initial_safe_box_hash),
        TCrypto.ToHexaString(AccountStorage.AccountStorageHash)]));
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
    FAccountStorageLock.Release;
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
  d.AccountStorage.CopyFrom(AccountStorage);
  d.FLastOperationBlock := FLastOperationBlock;
  d.FIsRestoringFromFile := FIsRestoringFromFile;
  d.FLastBlockCache.CopyFrom(FLastBlockCache);
end;

function TBlockManager.GetBlocksCount: Cardinal;
begin
  Result := AccountStorage.BlocksCount;
end;

procedure TBlockManager.Clear;
begin
  AccountStorage.Clear;
  FLastOperationBlock := TBlock.GetFirstBlock;
  FLastOperationBlock.initial_safe_box_hash := TCrypto.DoSha256(CT_Genesis_Magic_String_For_Old_Block_Hash);
  // Genesis hash
  FLastBlockCache.Clear(true);
  NewLog(nil, ltupdate, 'Clear cache and account storage');
end;

constructor TBlockManager.Create(AOwner: TComponent);
begin
  inherited;
  FStorage := nil;
  FStorageClass := nil;
  FAccountStorageLock := TPCCriticalSection.Create('TBlockManager_LOCKSTORAGE');
  FIsRestoringFromFile := false;
  FOnLog := nil;
  FAccountStorage := TAccountStorage.Create;
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
    FreeAndNil(FAccountStorageLock);
    step := 'Clear';
    Clear;
    step := 'Destroying LastBlockCache';
    FreeAndNil(FLastBlockCache);
    step := 'Destroying SafeBox';
    FreeAndNil(FAccountStorage);
    step := 'Destroying NotifyList';
    FreeAndNil(FNotifyList);
    step := 'Destroying Storage';
    FreeAndNil(FStorage);
    step := 'inherited';
    inherited;
  except
    on E: Exception do
    begin
      TLog.NewLog(lterror, Classname, 'Error destroying Blockmanager step: ' + step + ' Errors (' + E.Classname + '): '
        + E.Message);
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
  TPCThread.ProtectEnterCriticalSection(Self, FAccountStorageLock);
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
      Storage.RestoreAccountStorage(n);
      // Restore last blockchain
      if (BlocksCount > 0) and (AccountStorage.CurrentProtocol = CT_PROTOCOL_1) then
      begin
        if not Storage.LoadBlockChainBlock(FLastBlockCache, BlocksCount - 1) then
        begin
          NewLog(nil, lterror, 'Cannot find blockchain ' + Inttostr(BlocksCount - 1) +
            ' so cannot accept bank current block ' + Inttostr(BlocksCount));
          Clear;
        end;
      end;
      NewLog(nil, ltinfo, 'Start restoring from disk operations (Max ' + Inttostr(max_block) + ') BlockCount: ' +
        Inttostr(BlocksCount) + ' Orphan: ' + Storage.Orphan);
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
                  Storage.SaveAccountStorage;
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
      NewLog(nil, ltinfo, 'End restoring from disk operations (Max ' + Inttostr(max_block) + ') Orphan: ' +
        Storage.Orphan + ' Restored ' + Inttostr(BlocksCount) + ' blocks');
    finally
      FIsRestoringFromFile := false;
      FUpgradingToV2 := false;
    end;
  finally
    FAccountStorageLock.Release;
  end;
end;

function TBlockManager.GetAccountStorage: TAccountStorage;
begin
  Result := FAccountStorage;
end;

function TBlockManager.GetActualTargetSecondsAverage(BackBlocks: Cardinal): Real;
var
  ts1, ts2: Int64;
begin
  if BlocksCount > BackBlocks then
  begin
    ts1 := AccountStorage.Block(BlocksCount - 1).BlockHeader.timestamp;
    ts2 := AccountStorage.Block(BlocksCount - BackBlocks - 1).BlockHeader.timestamp;
  end
  else if (BlocksCount > 1) then
  begin
    ts1 := AccountStorage.Block(BlocksCount - 1).BlockHeader.timestamp;
    ts2 := AccountStorage.Block(0).BlockHeader.timestamp;
    BackBlocks := BlocksCount - 1;
  end
  else
  begin
    Result := 0;
    exit;
  end;
  Result := (ts1 - ts2) / BackBlocks;
end;

function TBlockManager.GetLastOperationBlock: TBlockHeader;
begin
  Result := FLastOperationBlock;
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
    ts1 := AccountStorage.Block(FromBlock - 1).BlockHeader.timestamp;
    ts2 := AccountStorage.Block(FromBlock - BackBlocks - 1).BlockHeader.timestamp;
  end
  else if (FromBlock > 1) then
  begin
    ts1 := AccountStorage.Block(FromBlock - 1).BlockHeader.timestamp;
    ts2 := AccountStorage.Block(0).BlockHeader.timestamp;
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
    FStorage.BlockManager := Self;
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

function TBlockManager.LoadAccountsFromStream(Stream: TStream; useSecureLoad: Boolean; var errors: AnsiString): Boolean;
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
    TPCThread.ProtectEnterCriticalSection(Self, FAccountStorageLock);
    try
      if Assigned(auxSB) then
      begin
        AccountStorage.CopyFrom(auxSB);
      end
      else
      begin
        Result := AccountStorage.LoadFromStream(Stream, false, LastReadBlock, errors);
      end;
      if not Result then
        exit;
      if AccountStorage.BlocksCount > 0 then
        FLastOperationBlock := AccountStorage.Block(AccountStorage.BlocksCount - 1).BlockHeader
      else
      begin
        FLastOperationBlock := TBlock.GetFirstBlock;
        FLastOperationBlock.initial_safe_box_hash := TCrypto.DoSha256(CT_Genesis_Magic_String_For_Old_Block_Hash);
        // Genesis hash
      end;
    finally
      FAccountStorageLock.Release;
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
  TPCThread.ProtectEnterCriticalSection(Self, FAccountStorageLock);
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
    FAccountStorageLock.Release;
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

constructor TBlockManagerNotify.Create(AOwner: TComponent);
begin
  inherited;
  FBlockManager := nil;
end;

destructor TBlockManagerNotify.Destroy;
begin
  BlockManager := nil;
  inherited;
end;

procedure TBlockManagerNotify.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
    if AComponent = FBlockManager then
      FBlockManager := nil;
end;

procedure TBlockManagerNotify.NotifyNewBlock;
begin
  if Assigned(FOnNewBlock) then
    FOnNewBlock(BlockManager);
end;

procedure TBlockManagerNotify.SetBlockManager(const value: TBlockManager);
begin
  if Assigned(FBlockManager) then
  begin
    FBlockManager.FNotifyList.Remove(Self);
    FBlockManager.RemoveFreeNotification(Self);
  end;
  FBlockManager := value;
  if Assigned(FBlockManager) then
  begin
    FBlockManager.FreeNotification(Self);
    FBlockManager.FNotifyList.Add(Self);
  end;
end;

end.
