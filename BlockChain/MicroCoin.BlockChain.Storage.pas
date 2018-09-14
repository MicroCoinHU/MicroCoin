unit MicroCoin.BlockChain.Storage;

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

uses SysUtils, Classes, ULog, MicroCoin.Account.Storage, MicroCoin.BlockChain.Base, MicroCoin.BlockChain.Block;

type

  TOrphan = AnsiString;

  TStorageClass = class of TStorage;

  TStorage = class(TComponent)
  private
    FOrphan: TOrphan;
    FBlockManager: TBlockManagerBase;
    FReadOnly: Boolean;
    procedure SetBlockManager(const value: TBlockManagerBase);
  protected
    procedure SetOrphan(const value: TOrphan); virtual;
    procedure SetReadOnly(const value: Boolean); virtual;
    function DoLoadBlockChain(Operations: TBlock; Block: Cardinal): Boolean; virtual; abstract;
    function DoSaveBlockChain(Operations: TBlock): Boolean; virtual; abstract;
    function DoMoveBlockChain(StartBlock: Cardinal; const DestOrphan: TOrphan; DestStorage: TStorage): Boolean;
      virtual; abstract;
    function DoSaveAccountStorage: Boolean; virtual; abstract;
    function DoRestoreAccountStorage(max_block: Int64): Boolean; virtual; abstract;
    procedure DoDeleteBlockChainBlocks(StartingDeleteBlock: Cardinal); virtual; abstract;
    function GetFirstBlockNumber: Int64; virtual; abstract;
    function GetLastBlockNumber: Int64; virtual; abstract;
    function DoInitialize: Boolean; virtual; abstract;
    function DoCreateSafeBoxStream(blockCount: Cardinal): TStream; virtual; abstract;
    procedure DoEraseStorage; virtual; abstract;
  public
    function BlockExists(Block: Cardinal): Boolean; virtual; abstract;
    function LoadBlockChainBlock(Operations: TBlock; Block: Cardinal): Boolean;
    function SaveBlockChainBlock(Operations: TBlock): Boolean;
    function MoveBlockChainBlocks(StartBlock: Cardinal; const DestOrphan: TOrphan; DestStorage: TStorage): Boolean;
    procedure DeleteBlockChainBlocks(StartingDeleteBlock: Cardinal);
    function SaveAccountStorage: Boolean;
    function RestoreAccountStorage(max_block: Int64): Boolean;
    constructor Create(AOwner: TComponent); override;
    property Orphan: TOrphan read FOrphan write SetOrphan;
    property readonly: Boolean read FReadOnly write SetReadOnly;
    property BlockManager: TBlockManagerBase read FBlockManager write SetBlockManager;
    procedure CopyConfiguration(const CopyFrom: TStorage); virtual;
    property FirstBlock: Int64 read GetFirstBlockNumber;
    property LastBlock: Int64 read GetLastBlockNumber;
    function Initialize: Boolean;
    function CreateSafeBoxStream(blockCount: Cardinal): TStream;
    function HasUpgradedToVersion2: Boolean; virtual; abstract;
    procedure CleanupVersion1Data; virtual; abstract;
    procedure EraseStorage;
  end;

implementation

procedure TStorage.CopyConfiguration(const CopyFrom: TStorage);
begin
  Orphan := CopyFrom.Orphan;
end;

constructor TStorage.Create(AOwner: TComponent);
begin
  inherited;
  FOrphan := '';
  FReadOnly := false;
end;

procedure TStorage.DeleteBlockChainBlocks(StartingDeleteBlock: Cardinal);
begin
  if readonly then
    raise Exception.Create('Cannot delete blocks because is ReadOnly');
  DoDeleteBlockChainBlocks(StartingDeleteBlock);
end;

function TStorage.Initialize: Boolean;
begin
  Result := DoInitialize;
end;

function TStorage.CreateSafeBoxStream(blockCount: Cardinal): TStream;
begin
  Result := DoCreateSafeBoxStream(blockCount);
end;

procedure TStorage.EraseStorage;
begin
  TLog.NewLog(ltinfo, Classname, 'Executing EraseStorage');
  DoEraseStorage;
end;

function TStorage.LoadBlockChainBlock(Operations: TBlock; Block: Cardinal): Boolean;
begin
  if (Block < FirstBlock) or (Block > LastBlock) then
    Result := false
  else
    Result := DoLoadBlockChain(Operations, Block);
end;

function TStorage.MoveBlockChainBlocks(StartBlock: Cardinal; const DestOrphan: TOrphan; DestStorage: TStorage): Boolean;
begin
  if Assigned(DestStorage) then
  begin
    if DestStorage.ReadOnly then
      raise Exception.Create('Cannot move blocks because is ReadOnly');
  end
  else if readonly then
    raise Exception.Create('Cannot move blocks from myself because is ReadOnly');
  Result := DoMoveBlockChain(StartBlock, DestOrphan, DestStorage);
end;

function TStorage.RestoreAccountStorage(max_block: Int64): Boolean;
begin
  Result := DoRestoreAccountStorage(max_block);
end;

function TStorage.SaveAccountStorage: Boolean;
begin
  Result := true;
  if not TAccountStorage.MustSaved(BlockManager.BlocksCount) then
    exit; // No save
  try
    Result := DoSaveAccountStorage;
    BlockManager.AccountStorage.CheckMemory;
  except
    on E: Exception do
    begin
      TLog.NewLog(lterror, Classname, 'Error saving AccountStorage: ' + E.Message);
      raise;
    end;
  end;
end;

function TStorage.SaveBlockChainBlock(Operations: TBlock): Boolean;
begin
  try
    if readonly then
      raise Exception.Create('Cannot save because is ReadOnly');
    Result := DoSaveBlockChain(Operations);
  except
    on E: Exception do
    begin
      TLog.NewLog(lterror, Classname, 'Error saving block chain: ' + E.Message);
      raise;
    end;
  end;
end;

procedure TStorage.SetBlockManager(const value: TBlockManagerBase);
begin
  FBlockManager := value;
end;

procedure TStorage.SetOrphan(const value: TOrphan);
begin
  FOrphan := value;
end;

procedure TStorage.SetReadOnly(const value: Boolean);
begin
  FReadOnly := value;
end;

end.
