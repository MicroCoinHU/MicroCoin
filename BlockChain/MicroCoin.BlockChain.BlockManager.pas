{==============================================================================|
| MicroCoin                                                                    |
| Copyright (c) 2017-2018 MicroCoin Developers                                 |
|==============================================================================|
| Permission is hereby granted, free of charge, to any person obtaining a copy |
| of this software and associated documentation files (the "Software"), to     |
| deal in the Software without restriction, including without limitation the   |
| rights to use, copy, modify, merge, publish, distribute, sublicense, and/or  |
| sell opies of the Software, and to permit persons to whom the Software is    |
| furnished to do so, subject to the following conditions:                     |
|                                                                              |
| The above copyright notice and this permission notice shall be included in   |
| all copies or substantial portions of the Software.                          |
|------------------------------------------------------------------------------|
| THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR   |
| IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,     |
| FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE  |
| AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER       |
| LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING      |
| FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER          |
| DEALINGS IN THE SOFTWARE.                                                    |
|==============================================================================|
| This unit contains portions from PascalCoin                                  |
| Copyright (c) Albert Molina 2016 - 2018                                      |
|                                                                              |
| Distributed under the MIT software license, see the accompanying file        |
| LICENSE or visit http://www.opensource.org/licenses/mit-license.php.         |
|==============================================================================|
| File:       MicroCoin.BlockChain.BlockManager.pas                            |
| Created at: 2018-09-16                                                       |
| Purpose:                                                                     |
|==============================================================================}
unit MicroCoin.BlockChain.BlockManager;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses Classes, UCrypto, ULog, UThread, SyncObjs, Math,
  MicroCoin.Transaction.Base, MicroCoin.BlockChain.Base,
  MicroCoin.Transaction.Manager, MicroCoin.Transaction.HashTree,
  MicroCoin.Account.AccountKey, MicroCoin.BlockChain.BlockHeader,
  MicroCoin.Account.Storage, MicroCoin.Account.Transaction, MicroCoin.BlockChain.Protocol,
  MicroCoin.BlockChain.Storage, MicroCoin.BlockChain.Block, Types;

{$I config.inc}

type
  TBlockManager = class;
  TBlockManagerNotify = class;

  TBlockManagerLog = procedure(Sender: TBlockManager; ATransactions: TBlock; Logtype: TLogType; Logtxt: AnsiString)
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
    class var FStorage: TStorage;
    class var FStorageClass: TStorageClass;
  private
    FAccountStorage: TAccountStorage;
    FLastBlockCache: TBlock;
    FLastBlockHeader: TBlockHeader;
    FIsRestoringFromFile: Boolean;
    FIsLoadingBlocks: Boolean;
    FUpgradingToV2: Boolean;
    FOnLog: TBlockManagerLog;
    FAccountStorageLock: TPCCriticalSection;
    FNotifyList: TList;
    FStopped : boolean;
    function GetStorage: TStorage;
    class procedure SetStorageClass(const value: TStorageClass); static;
  protected
    function GetAccountStorage: TAccountStorage; override;
    function GetBlocksCount: Cardinal; override;
    function GetLastBlockHeader: TBlockHeader; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AccountsCount: Cardinal;
    procedure AssignTo(Dest: TPersistent); override;
    function GetActualTargetSecondsAverage(BackBlocks: Cardinal): Real;
    function GetTargetSecondsAverage(FromBlock, BackBlocks: Cardinal): Real;
    function LoadAccountsFromStream(Stream: TStream; useSecureLoad: Boolean; var errors: AnsiString): Boolean; override;
    procedure Clear;
    function LoadTransactions(ABlock: TBlock; ABlockNumber: Cardinal): Boolean;
    function AddNewBlockToBlockChain(ABlock: TBlock; MaxAllowedTimestamp: Cardinal;
      var newBlock: TAccountStorageEntry; var errors: AnsiString): Boolean;
    procedure DiskRestoreFromTransactions(max_block: Int64);
    procedure NewLog(ABlock: TBlock; Logtype: TLogType; Logtxt: AnsiString);
    function IsReady(var CurrentProcess: AnsiString): Boolean;

    property OnLog: TBlockManagerLog read FOnLog write FOnLog;

    property LastBlock: TBlockHeader read GetLastBlockHeader;
    property Storage: TStorage read GetStorage;
    class property StorageClass: TStorageClass read FStorageClass write SetStorageClass;
    property LastBlockFound: TBlock read FLastBlockCache;
    property UpgradingToV2: Boolean read FUpgradingToV2;
    property AccountStorage: TAccountStorage read GetAccountStorage;
    property BlocksCount: Cardinal read GetBlocksCount;
    property Stopped : Boolean read FStopped write FStopped;
    property IsLoadingBlocks: Boolean read FIsLoadingBlocks;
  end;

implementation

uses
  {Messages,}
    SysUtils, Variants, {Graphics,}
  {Controls, Forms,}
  {StdCtrls,}
  UTime, UConst;

resourcestring
  StrMigratingToVersion = 'Migrating to version 2 format';
  StrLoadingCheckpointBlocks = 'Loading checkpoint blocks %d/%d';
  StrLoadingBlocks = 'Loading blocks';

function TBlockManager.AccountsCount: Cardinal;
begin
  Result := FAccountStorage.AccountsCount;
end;

function TBlockManager.AddNewBlockToBlockChain(ABlock: TBlock; MaxAllowedTimestamp: Cardinal;
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
      if not ABlock.ValidateBlock(errors) then
      begin
        exit;
      end;
      if (ABlock.BlockHeader.Block > 0) then
      begin
        if ((MaxAllowedTimestamp > 0) and (ABlock.BlockHeader.timestamp > MaxAllowedTimestamp)) then
        begin
          errors := 'Invalid timestamp (Future time: New timestamp ' + Inttostr(ABlock.BlockHeader.timestamp) +
            ' > max allowed ' + Inttostr(MaxAllowedTimestamp) + ')';
          exit;
        end;
      end;
      // Ok, include!
      // WINNER !!!
      // Congrats!

      if not ABlock.AccountTransaction.Commit(ABlock.BlockHeader, errors) then
      begin
        exit;
      end;

      newBlock := AccountStorage.Block(AccountStorage.BlocksCount - 1);

      // Initialize values
      FLastBlockHeader := ABlock.BlockHeader;
      // log it!
      NewLog(ABlock, ltupdate,
        Format('New block height:%d nOnce:%d timestamp:%d Operations:%d Fee:%d SafeBoxBalance:%d=%d',
        [ABlock.BlockHeader.Block, ABlock.BlockHeader.nonce, ABlock.BlockHeader.timestamp,
        ABlock.Count, ABlock.BlockHeader.Fee, AccountStorage.TotalBalance,
        ABlock.AccountTransaction.TotalBalance]));
      // Save Operations to disk
      if not FIsRestoringFromFile then
      begin
        Storage.SaveBlockChainBlock(ABlock);
      end;
      FLastBlockCache.CopyFrom(ABlock);
      ABlock.Clear(true);
      Result := true;
    finally
      if not Result then
        NewLog(ABlock, lterror, 'Invalid new block ' + Inttostr(ABlock.BlockHeader.Block) + ': ' + errors);
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
  d.FLastBlockHeader := FLastBlockHeader;
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
  FLastBlockHeader := TBlock.GetFirstBlock;
  FLastBlockHeader.initial_safe_box_hash := TCrypto.DoSha256(cGenesisBlockMagic);
  // Genesis hash
  FLastBlockCache.Clear(true);
  NewLog(nil, ltupdate, 'Clear cache and account storage');
end;

constructor TBlockManager.Create(AOwner: TComponent);
begin
  inherited;
  FStorage := nil;
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

procedure TBlockManager.DiskRestoreFromTransactions(max_block: Int64);
var
  errors: AnsiString;
  newBlock: TAccountStorageEntry;
  Operations: TBlock;
  n: Int64;
begin
  if FIsRestoringFromFile then
  begin
    TLog.NewLog(lterror, ClassName, 'Is Restoring!!!');
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
      if (BlocksCount > 0) and (AccountStorage.CurrentProtocol = cPROTOCOL_1) then
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
        FIsLoadingBlocks := true;
        while ((BlocksCount <= max_block)) do
        begin
          if FStopped then exit;
          if Storage.BlockExists(BlocksCount) then
          begin
            if Storage.LoadBlockChainBlock(Operations, BlocksCount) then
            begin
              SetLength(errors, 0);
              if not AddNewBlockToBlockChain(Operations, 0, newBlock, errors) then
              begin
                NewLog(Operations, lterror, 'Error restoring block: ' + Inttostr(BlocksCount) + ' Errors: ' + errors);
                Storage.DeleteBlockChainBlocks(BlocksCount);
                break;
              end
              else
              begin
                // To prevent continuous saving...
{$IFDEF TESTNET}
                Storage.SaveAccountStorage;
{$ELSE}
  {$IFDEF DEVNET}
                Storage.SaveAccountStorage;
  {$ELSE}
                if (BlocksCount mod (cSaveAccountStorageOnBlocks*10)) = 0 then
                begin
                  Storage.SaveAccountStorage;
                end;
  {$ENDIF}
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
        FIsLoadingBlocks := false;
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

function TBlockManager.GetLastBlockHeader: TBlockHeader;
begin
  Result := FLastBlockHeader;
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
    FStorage := FStorageClass.Create;
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
      CurrentProcess := StrMigratingToVersion
    else if not FIsLoadingBlocks
         then CurrentProcess := Format(StrLoadingCheckpointBlocks,[BlocksCount, Storage.LastBlock])
         else CurrentProcess := StrLoadingBlocks; // Format('Loading blockchain %d%%',
//         [Round(100*(BlocksCount mod 100) / Max(1, (Storage.LastBlock) mod 100)) ])
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
        FLastBlockHeader := AccountStorage.Block(AccountStorage.BlocksCount - 1).BlockHeader
      else
      begin
        FLastBlockHeader := TBlock.GetFirstBlock;
        FLastBlockHeader.initial_safe_box_hash := TCrypto.DoSha256(cGenesisBlockMagic);
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

function TBlockManager.LoadTransactions(ABlock: TBlock; ABlockNumber: Cardinal): Boolean;
begin
  TPCThread.ProtectEnterCriticalSection(Self, FAccountStorageLock);
  try
    if (ABlockNumber > 0) and (ABlockNumber = FLastBlockCache.BlockHeader.Block) then
    begin
      // Same as cache, sending cache
      ABlock.CopyFrom(FLastBlockCache);
      Result := true;
    end
    else
    begin
      Result := Storage.LoadBlockChainBlock(ABlock, ABlockNumber);
    end;
  finally
    FAccountStorageLock.Release;
  end;
end;

procedure TBlockManager.NewLog(ABlock: TBlock; Logtype: TLogType; Logtxt: AnsiString);
var
  s: AnsiString;
begin
  if Assigned(ABlock) then
    s := ABlock.Classname
  else
    s := Classname;
  TLog.NewLog(Logtype, s, Logtxt);
  if Assigned(FOnLog) then
    FOnLog(Self, ABlock, Logtype, Logtxt);
end;

class procedure TBlockManager.SetStorageClass(const value: TStorageClass);
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
