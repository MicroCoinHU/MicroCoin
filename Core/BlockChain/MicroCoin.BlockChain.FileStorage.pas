unit MicroCoin.BlockChain.FileStorage;

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

uses Classes, MicroCoin.BlockChain.BlockManager, SyncObjs, UThread, UCrypto, MicroCoin.Account.Storage,
  MicroCoin.BlockChain.Storage, MicroCoin.BlockChain.Block;

{$I ../config.inc}

type
  TBlockHeader = record
    BlockNumber: Cardinal;
    StreamBlockRelStartPos: Int64;
    BlockSize: Cardinal;
  end; // 16 bytes

  TArrayOfInt64 = array of Int64;

  { TFileStorage }

  TFileStorage = class(TStorage)
  private
    FStorageLock: TPCCriticalSection;
    FBlockChainStream: TFileStream;
    FStreamFirstBlockNumber: Int64;
    FStreamLastBlockNumber: Int64;
    FBlockHeadersFirstBytePosition: TArrayOfInt64;
    FDatabaseFolder: AnsiString;
    FBlockChainFileName: AnsiString;
    function StreamReadBlockHeader(Stream: TStream; iBlockHeaders: Integer; BlockHeaderFirstBlock, Block: Cardinal;
      CanSearchBackward: Boolean; var BlockHeader: TBlockHeader): Boolean;
    function StreamBlockRead(Stream: TStream; iBlockHeaders: Integer; BlockHeaderFirstBlock, Block: Cardinal;
      Operations: TBlock): Boolean;
    function StreamBlockSave(Stream: TStream; iBlockHeaders: Integer; BlockHeaderFirstBlock: Cardinal;
      Operations: TBlock): Boolean;
    function GetFolder(const AOrphan: TOrphan): AnsiString;
    function GetBlockHeaderFirstBytePosition(Stream: TStream; Block: Cardinal; CanInitialize: Boolean;
      var iBlockHeaders: Integer; var BlockHeaderFirstBlock: Cardinal): Boolean;
    function GetBlockHeaderFixedSize: Int64;
    procedure SetDatabaseFolder(const Value: AnsiString);
    procedure ClearStream;
    procedure GrowStreamUntilPos(Stream: TStream; newPos: Int64; DeleteDataStartingAtCurrentPos: Boolean);
  protected
    procedure SetReadOnly(const Value: Boolean); override;
    procedure SetOrphan(const Value: TOrphan); override;
    function DoLoadBlockChain(Operations: TBlock; Block: Cardinal): Boolean; override;
    function DoSaveBlockChain(Operations: TBlock): Boolean; override;
    function DoMoveBlockChain(Start_Block: Cardinal; const DestOrphan: TOrphan; DestStorage: TStorage)
      : Boolean; override;
    function DoSaveAccountStorage: Boolean; override;
    function DoRestoreAccountStorage(max_block: Int64): Boolean; override;
    procedure DoDeleteBlockChainBlocks(StartingDeleteBlock: Cardinal); override;
    function BlockExists(Block: Cardinal): Boolean; override;
    function LockBlockChainStream: TFileStream;
    procedure UnlockBlockChainStream;
    function LoadBankFileInfo(const Filename: AnsiString; var safeBoxHeader: TAccountStorageHeader): Boolean;
    function GetFirstBlockNumber: Int64; override;
    function GetLastBlockNumber: Int64; override;
    function DoInitialize: Boolean; override;
    function DoCreateSafeBoxStream(blockCount: Cardinal): TStream; override;
    procedure DoEraseStorage; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetSafeboxCheckpointingFileName(const BaseDataFolder: AnsiString; Block: Cardinal): AnsiString;
    property DatabaseFolder: AnsiString read FDatabaseFolder write SetDatabaseFolder;
    procedure CopyConfiguration(const CopyFrom: TStorage); override;
    procedure SetBlockChainFile(BlockChainFileName: AnsiString);
    function HasUpgradedToVersion2: Boolean; override;
    procedure CleanupVersion1Data; override;
  end;

implementation

uses ULog, SysUtils, UConst;

{ TFileStorage }

const
  CT_TBlockHeader_NUL: TBlockHeader = (BlockNumber: 0; StreamBlockRelStartPos: 0; BlockSize: 0);

  CT_GroupBlockSize = 1000;
  CT_SizeOfBlockHeader = 16;
  {
    BlockChain file storage:

    BlockHeader 0 -> From Block 0 to (CT_GroupBlockSize-1)
    Foreach Block:
    BlockNumber : 4 bytes
    StreamBlockRelStartPos : 8 bytes  -> Start pos relative to End of BlockHeader
    BlockSizeH : 4 bytes
    -- Total size of BlockHeader: (4+8+4) * (CT_GroupBlockSize) = 16 * CT_GroupBlockSize
    -- Note: If BlockHeader starts at pos X, it ends at pos X + (16*CT_GroupBlockSize)
    Block 0
    BlockSizeC: 4 bytes
    Data: BlockSizeC bytes
    Block 1
    ...
    Block CT_GroupBlockSize-1

    BlockHeader 1 -> From Block CT_GroupBlockSize to ((CT_GroupBlockSize*2)-1)
    (Same as BlockHeader 1)
    Block CT_GroupBlockSize
    ...
    Block ((CT_GroupBlockSize*2)-1)

    ...
    BlockHeader X -> From (CT_GroupBlockSize*X) to ((CT_GroupBlockSize*(X+1))-1)
    ...

  }

function TFileStorage.BlockExists(Block: Cardinal): Boolean;
var
  iBlockHeaders: Integer;
  BlockHeaderFirstBlock: Cardinal;
  Stream: TStream;
  BlockHeader: TBlockHeader;
begin
  Result := false;
  Stream := LockBlockChainStream;
  try
    if not GetBlockHeaderFirstBytePosition(Stream, Block, false, iBlockHeaders, BlockHeaderFirstBlock) then
      exit;
    if not StreamReadBlockHeader(Stream, iBlockHeaders, BlockHeaderFirstBlock, Block, false, BlockHeader) then
      exit;
    Result := (BlockHeader.BlockNumber = Block) and (((BlockHeader.BlockNumber mod CT_GroupBlockSize) = 0) or
      (BlockHeader.StreamBlockRelStartPos > 0)) and (BlockHeader.BlockSize > 0);
  finally
    UnlockBlockChainStream;
  end;
end;

procedure TFileStorage.ClearStream;
begin
  FreeAndNil(FBlockChainStream);
  FStreamFirstBlockNumber := 0;
  FStreamLastBlockNumber := -1;
  SetLength(FBlockHeadersFirstBytePosition, 0);
end;

procedure TFileStorage.GrowStreamUntilPos(Stream: TStream; newPos: Int64; DeleteDataStartingAtCurrentPos: Boolean);
var
  null_buff: array [1 .. CT_GroupBlockSize] of Byte;
  i, antPos, antSize: Int64;
begin
  antPos := Stream.Position;
  antSize := Stream.Size;
  if not DeleteDataStartingAtCurrentPos then
  begin
    Stream.Position := Stream.Size;
  end;
  if (Stream.Position < newPos) then
  begin
    FillChar(null_buff, length(null_buff), 0);
    while (Stream.Position < newPos) do
    begin
      i := newPos - Stream.Position;
      if i > length(null_buff) then
        i := length(null_buff);
      Stream.WriteBuffer(null_buff, i);
    end;
  end;
  Stream.Position := newPos;
end;

procedure TFileStorage.CopyConfiguration(const CopyFrom: TStorage);
begin
  inherited;
  if CopyFrom is TFileStorage then
  begin
    DatabaseFolder := TFileStorage(CopyFrom).DatabaseFolder;
  end;
end;

constructor TFileStorage.Create(AOwner: TComponent);
begin
  inherited;
  FDatabaseFolder := '';
  FBlockChainFileName := '';
  FBlockChainStream := nil;
  SetLength(FBlockHeadersFirstBytePosition, 0);
  FStreamFirstBlockNumber := 0;
  FStreamLastBlockNumber := -1;
  FStorageLock := TPCCriticalSection.Create('TFileStorage_StorageLock');
end;

destructor TFileStorage.Destroy;
begin
  inherited;
  ClearStream;
  FreeAndNil(FStorageLock);
end;

procedure TFileStorage.DoDeleteBlockChainBlocks(StartingDeleteBlock: Cardinal);
var
  Stream: TStream;
  iBlockHeaders: Integer;
  BlockHeaderFirstBlock: Cardinal;
  _Header: TBlockHeader;
  _intBlockIndex: Cardinal;
  p: Int64;
begin
  Stream := LockBlockChainStream;
  try
    if not GetBlockHeaderFirstBytePosition(Stream, StartingDeleteBlock, false, iBlockHeaders, BlockHeaderFirstBlock)
    then
      exit;
    if not StreamReadBlockHeader(Stream, iBlockHeaders, BlockHeaderFirstBlock, StartingDeleteBlock, True, _Header) then
      exit;
    _intBlockIndex := (_Header.BlockNumber - BlockHeaderFirstBlock);
    p := Int64(_intBlockIndex) * Int64(CT_SizeOfBlockHeader);
    Stream.Position := p;
    // Write null data until end of header
    GrowStreamUntilPos(Stream, FBlockHeadersFirstBytePosition[iBlockHeaders] + GetBlockHeaderFixedSize, True);
    // End Stream at _Header
    Stream.Size := Stream.Position + _Header.StreamBlockRelStartPos;
  finally
    UnlockBlockChainStream;
  end;
end;

function TFileStorage.DoInitialize: Boolean;
var
  Stream: TStream;
begin
  Stream := LockBlockChainStream;
  try
    Result := True;
  finally
    UnlockBlockChainStream;
  end;
end;

function TFileStorage.DoCreateSafeBoxStream(blockCount: Cardinal): TStream;
var
  fn: TFilename;
  err: AnsiString;
begin
  Result := nil;
  fn := GetSafeboxCheckpointingFileName(GetFolder(Orphan), blockCount);
  if (fn <> '') and (FileExists(fn)) then
  begin
    Result := TFileStream.Create(fn, fmOpenRead);
  end;
  if not Assigned(Result) then
  begin
    err := 'Cannot load SafeBoxStream (block:' + IntToStr(blockCount) + ') file:' + fn;
    TLog.NewLog(ltError, ClassName, err);
  end;
end;

procedure TFileStorage.DoEraseStorage;
var
  Stream: TStream;
begin
  Stream := LockBlockChainStream;
  try
    Stream.Size := 0; // Erase
    ClearStream;
  finally
    UnlockBlockChainStream;
  end;
end;

function TFileStorage.DoLoadBlockChain(Operations: TBlock; Block: Cardinal): Boolean;
var
  Stream: TStream;
  iBlockHeaders: Integer;
  BlockHeaderFirstBlock: Cardinal;
begin
  Result := false;
  Stream := LockBlockChainStream;
  try
    if not GetBlockHeaderFirstBytePosition(Stream, Block, false, iBlockHeaders, BlockHeaderFirstBlock) then
      exit;
    Result := StreamBlockRead(Stream, iBlockHeaders, BlockHeaderFirstBlock, Block, Operations);
  finally
    UnlockBlockChainStream;
  end;
end;

function TFileStorage.DoMoveBlockChain(Start_Block: Cardinal; const DestOrphan: TOrphan; DestStorage: TStorage)
  : Boolean;
  procedure DoCopyFile(sourcefn, destfn: AnsiString);
  var
    sourceFS, destFS: TFileStream;
  begin
    if not FileExists(sourcefn) then
      raise Exception.Create('Source file not found: ' + sourcefn);
    sourceFS := TFileStream.Create(sourcefn, fmOpenRead + fmShareDenyNone);
    try
      sourceFS.Position := 0;
      destFS := TFileStream.Create(destfn, fmCreate + fmShareDenyWrite);
      try
        destFS.Size := 0;
        destFS.CopyFrom(sourceFS, sourceFS.Size);
      finally
        destFS.Free;
      end;
    finally
      sourceFS.Free;
    end;
  end;

  procedure DoCopySafebox;
  var
    sr: TSearchRec;
    FileAttrs: Integer;
    folder: AnsiString;
    sourcefn, destfn: AnsiString;
  begin
    FileAttrs := faArchive;
    folder := GetFolder(Orphan);
    if SysUtils.FindFirst(GetFolder(Orphan) + PathDelim + '*.safebox', FileAttrs, sr) = 0 then
    begin
      repeat
        if (sr.Attr and FileAttrs) = FileAttrs then
        begin
          sourcefn := GetFolder(Orphan) + PathDelim + sr.Name;
          destfn := GetFolder('') + PathDelim + sr.Name;
          TLog.NewLog(ltInfo, ClassName, 'Copying safebox file ' + sourcefn + ' to ' + destfn);
          try
            DoCopyFile(sourcefn, destfn);
          except
            on E: Exception do
            begin
              TLog.NewLog(ltError, ClassName, 'Error copying file: (' + E.ClassName + ') ' + E.Message);
            end;
          end;
        end;
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;
  end;

var
  db: TFileStorage;
  i: Integer;
  ops: TBlock;
  b: Cardinal;
begin
  try
    if (Assigned(DestStorage)) and (DestStorage is TFileStorage) then
      db := TFileStorage(DestStorage)
    else
      db := nil;
    try
      if not Assigned(db) then
      begin
        db := TFileStorage.Create(nil);
        db.DatabaseFolder := Self.DatabaseFolder;
        db.BlockManager := Self.BlockManager;
        db.Orphan := DestOrphan;
        db.FStreamFirstBlockNumber := Start_Block;
      end;
      if db is TFileStorage then
        TFileStorage(db).LockBlockChainStream;
      try
        ops := TBlock.Create(nil);
        try
          b := Start_Block;
          while LoadBlockChainBlock(ops, b) do
          begin
            inc(b);
            TLog.NewLog(ltDebug, ClassName, 'Moving block from "' + Orphan + '" to "' + DestOrphan + '" ' +
              TBlock.OperationBlockToText(ops.OperationBlock));
            db.SaveBlockChainBlock(ops);
          end;
          TLog.NewLog(ltDebug, ClassName, 'Moved blockchain from "' + Orphan + '" to "' + DestOrphan + '" from block ' +
            IntToStr(Start_Block) + ' to ' + IntToStr(b - 1));
        finally
          ops.Free;
        end;
        // If DestOrphan is empty, then copy possible updated safebox (because, perhaps current saved safebox is from invalid blockchain)
        if (DestOrphan = '') and (Orphan <> '') then
        begin
          DoCopySafebox;
        end;
      finally
        if db is TFileStorage then
          TFileStorage(db).UnlockBlockChainStream;
      end;
    finally
      if not Assigned(DestStorage) then
        db.Free;
    end;
  except
    on E: Exception do
    begin
      TLog.NewLog(ltError, ClassName, 'Error at DoMoveBlockChain: (' + E.ClassName + ') ' + E.Message);
      raise;
    end;
  end;
end;

function TFileStorage.DoRestoreAccountStorage(max_block: Int64): Boolean;
var
  sr: TSearchRec;
  FileAttrs: Integer;
  folder: AnsiString;
  Filename, auxfn: AnsiString;
  fs: TFileStream;
  ms: TMemoryStream;
  errors: AnsiString;
  blockscount: Cardinal;
  sbHeader: TAccountStorageHeader;
begin
  LockBlockChainStream;
  try
    FileAttrs := faArchive;
    folder := GetFolder(Orphan);
    Filename := '';
    blockscount := 0;
    if SysUtils.FindFirst(folder + PathDelim + '*.safebox', FileAttrs, sr) = 0 then
    begin
      repeat
        if (sr.Attr and FileAttrs) = FileAttrs then
        begin
          auxfn := folder + PathDelim + sr.Name;
          if LoadBankFileInfo(auxfn, sbHeader) then
          begin
            if (((max_block < 0) or (sbHeader.blockscount <= max_block)) and (sbHeader.blockscount > blockscount)) and
              (sbHeader.startBlock = 0) and (sbHeader.endBlock = sbHeader.startBlock + sbHeader.blockscount - 1) then
            begin
              Filename := auxfn;
              blockscount := sbHeader.blockscount;
            end;
          end;
        end;
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;
    if (Filename <> '') then
    begin
      TLog.NewLog(ltInfo, Self.ClassName, 'Loading SafeBox with ' + IntToStr(blockscount) + ' blocks from file ' +
        Filename);
      fs := TFileStream.Create(Filename, fmOpenRead);
      try
        ms := TMemoryStream.Create;
        try
          ms.CopyFrom(fs, 0);
          fs.Position := 0;
          ms.Position := 0;
          if not BlockManager.LoadAccountsFromStream(ms, false, errors) then
          begin
            TLog.NewLog(ltError, ClassName, 'Error reading bank from file: ' + Filename + ' Error: ' + errors);
          end;
        finally
          ms.Free;
        end;
      finally
        fs.Free;
      end;
    end;
  finally
    UnlockBlockChainStream;
  end;
end;

function TFileStorage.DoSaveAccountStorage: Boolean;
var
  fs: TFileStream;
  bankfilename: AnsiString;
  ms: TMemoryStream;
begin
  Result := True;
  bankfilename := GetSafeboxCheckpointingFileName(GetFolder(Orphan), BlockManager.blockscount);
  if (bankfilename <> '') then
  begin
    TLog.NewLog(ltInfo, ClassName, 'Saving Safebox blocks:' + IntToStr(BlockManager.blockscount) + ' file:' +
      bankfilename);
    fs := TFileStream.Create(bankfilename, fmCreate);
    try
      fs.Size := 0;
      ms := TMemoryStream.Create;
      try
        BlockManager.AccountStorage.SaveToStream(ms, 0, BlockManager.AccountStorage.blockscount - 1);
        ms.Position := 0;
        fs.Position := 0;
        fs.CopyFrom(ms, 0);
      finally
        ms.Free;
      end;
    finally
      fs.Free;
    end;
  end;
end;

function TFileStorage.DoSaveBlockChain(Operations: TBlock): Boolean;
var
  Stream: TStream;
  iBlockHeaders: Integer;
  BlockHeaderFirstBlock: Cardinal;
begin
  Result := false;
  Stream := LockBlockChainStream;
  try
    if (length(FBlockHeadersFirstBytePosition) = 0) then
    begin
      // Is saving first block on the stream?
      if (Stream.Size = 0) then
      begin
        // Yes! Positioning
        FStreamFirstBlockNumber := Operations.OperationBlock.Block;
      end;
      TLog.NewLog(ltDebug, ClassName, Format('Saving Block %d on a newer stream, stream first position=%d',
        [Operations.OperationBlock.Block, FStreamFirstBlockNumber]));
    end;
    if not GetBlockHeaderFirstBytePosition(Stream, Operations.OperationBlock.Block, True, iBlockHeaders,
      BlockHeaderFirstBlock) then
      exit;
    Result := StreamBlockSave(Stream, iBlockHeaders, BlockHeaderFirstBlock, Operations);
  finally
    UnlockBlockChainStream;
  end;
  if Assigned(BlockManager) then
    SaveAccountStorage;
end;

const
  CT_SafeboxsToStore = 10;

class function TFileStorage.GetSafeboxCheckpointingFileName(const BaseDataFolder: AnsiString; Block: Cardinal)
  : AnsiString;
begin
  Result := '';
  if not ForceDirectories(BaseDataFolder) then
    exit;
  // We will store checkpointing
  Result := BaseDataFolder + PathDelim + 'checkpoint' +
    IntToStr((Block div CT_BankToDiskEveryNBlocks) mod CT_SafeboxsToStore) + '.safebox';
end;

function TFileStorage.GetBlockHeaderFirstBytePosition(Stream: TStream; Block: Cardinal; CanInitialize: Boolean;
  var iBlockHeaders: Integer; var BlockHeaderFirstBlock: Cardinal): Boolean;
var
  iPos, start, nCurrBlock: Cardinal;
  bh: TBlockHeader;
  null_buff: array [1 .. (CT_GroupBlockSize * CT_SizeOfBlockHeader)] of Byte;
begin
  Result := false;
  if Block < FStreamFirstBlockNumber then
  begin
    TLog.NewLog(ltError, ClassName, Format('Block %d is lower than Stream First block %d',
      [Block, FStreamFirstBlockNumber]));
    exit;
  end;
  iPos := (Block - FStreamFirstBlockNumber) div CT_GroupBlockSize;
  if iPos > high(FBlockHeadersFirstBytePosition) then
  begin
    if length(FBlockHeadersFirstBytePosition) > 0 then
    begin
      start := high(FBlockHeadersFirstBytePosition);
    end
    else
    begin
      if CanInitialize then
      begin
        // Initialize and start at 0
        SetLength(FBlockHeadersFirstBytePosition, 1);
        FBlockHeadersFirstBytePosition[0] := 0;
        start := 0;
      end
      else
        exit;
    end;
    while (start < iPos) do
    begin
      // Read last start position
      if (Stream.Size < (FBlockHeadersFirstBytePosition[start] + GetBlockHeaderFixedSize)) then
      begin
        // This position not exists...
        if (CanInitialize) then
        begin
          GrowStreamUntilPos(Stream, FBlockHeadersFirstBytePosition[start], false);
          // Save BlockHeader values (initialized to 0)
          FillChar(null_buff, length(null_buff), 0);
          Stream.WriteBuffer(null_buff, length(null_buff));
        end
        else
        begin
          // This is a Fatal error due must find previos block!
          TLog.NewLog(ltError, ClassName,
            Format('Stream size %d is lower than BlockHeader[%d] position %d + BlockHeaderSize %d',
            [Stream.Size, start, FBlockHeadersFirstBytePosition[start], GetBlockHeaderFixedSize]));
          exit;
        end;
      end;
      Stream.Position := FBlockHeadersFirstBytePosition[start] + GetBlockHeaderFixedSize - CT_SizeOfBlockHeader;
      // Read last saved Header
      nCurrBlock := FStreamFirstBlockNumber + ((start + 1) * CT_GroupBlockSize) - 1;
      repeat
        Stream.Read(bh.BlockNumber, SizeOf(bh.BlockNumber));
        Stream.Read(bh.StreamBlockRelStartPos, SizeOf(bh.StreamBlockRelStartPos));
        Stream.Read(bh.BlockSize, SizeOf(bh.BlockSize));
        if (bh.BlockNumber <> nCurrBlock) then
        begin
          if (bh.BlockNumber <> 0) or (bh.StreamBlockRelStartPos <> 0) or (bh.BlockSize <> 0) then
          begin
            TLog.NewLog(ltError, ClassName,
              Format('Fatal error. Found a Tblockheader with no 0 values searching for block:%d at nCurrBlock:%d - Number:%d RelStartPos:%d Size:%d',
              [Block, nCurrBlock, bh.BlockNumber, bh.StreamBlockRelStartPos, bh.BlockSize]));
            exit;
          end;
          if ((start = 0) and (nCurrBlock > FStreamFirstBlockNumber)) or
            ((start > 0) and (nCurrBlock > (FStreamFirstBlockNumber + ((start) * CT_GroupBlockSize)))) then
          begin
            dec(nCurrBlock);
            // Positioning for new read:
            Stream.Seek(Int64(CT_SizeOfBlockHeader) * (-2), soFromCurrent);
          end
          else
          begin
            break; // End of blockheader!
          end;
        end;
      until (bh.BlockNumber > 0);
      // Positioning!
      Stream.Position := FBlockHeadersFirstBytePosition[start] + GetBlockHeaderFixedSize;
      //
      SetLength(FBlockHeadersFirstBytePosition, length(FBlockHeadersFirstBytePosition) + 1);
      if bh.BlockNumber > 0 then
      begin
        FBlockHeadersFirstBytePosition[high(FBlockHeadersFirstBytePosition)] := Stream.Position +
          bh.StreamBlockRelStartPos + bh.BlockSize;
      end
      else
      begin
        // Not found a block, starting at last pos
        FBlockHeadersFirstBytePosition[high(FBlockHeadersFirstBytePosition)] := Stream.Position;
      end;
      inc(start);

      // Check if blockheader size is ok:
      if (CanInitialize) and (Stream.Size < (FBlockHeadersFirstBytePosition[start] + GetBlockHeaderFixedSize)) then
      begin
        Stream.Position := FBlockHeadersFirstBytePosition[start];
        TLog.NewLog(ltInfo, ClassName,
          Format('Increasing size for blockheader %d at pos:%d (current stream pos %d size %d) to position:%d',
          [start, FBlockHeadersFirstBytePosition[start], Stream.Position, Stream.Size,
          FBlockHeadersFirstBytePosition[start] + GetBlockHeaderFixedSize]));
        GrowStreamUntilPos(Stream, FBlockHeadersFirstBytePosition[start] + GetBlockHeaderFixedSize, True);
      end;

    end;
  end;
  iBlockHeaders := iPos;
  BlockHeaderFirstBlock := FStreamFirstBlockNumber + (iPos * CT_GroupBlockSize);
  Result := True;
end;

function TFileStorage.GetBlockHeaderFixedSize: Int64;
begin
  Result := (CT_GroupBlockSize * CT_SizeOfBlockHeader);
end;

function TFileStorage.GetFirstBlockNumber: Int64;
begin
  Result := FStreamFirstBlockNumber;
end;

function TFileStorage.GetFolder(const AOrphan: TOrphan): AnsiString;
begin
  if FDatabaseFolder = '' then
    raise Exception.Create('No Database Folder');
  if AOrphan <> '' then
    Result := FDatabaseFolder + PathDelim + AOrphan
  else
    Result := FDatabaseFolder;
  if not ForceDirectories(Result) then
    raise Exception.Create('Cannot create database folder: ' + Result);
end;

function TFileStorage.GetLastBlockNumber: Int64;
begin
  Result := FStreamLastBlockNumber;
end;

function TFileStorage.LoadBankFileInfo(const Filename: AnsiString; var safeBoxHeader: TAccountStorageHeader): Boolean;
var
  fs: TFileStream;
begin
  Result := false;
  safeBoxHeader := CT_AccountStorageHeader_NUL;
  if not FileExists(Filename) then
    exit;
  fs := TFileStream.Create(Filename, fmOpenRead);
  try
    fs.Position := 0;
    Result := BlockManager.AccountStorage.LoadHeaderFromStream(fs, safeBoxHeader);
  finally
    fs.Free;
  end;
end;

function TFileStorage.LockBlockChainStream: TFileStream;
  function InitStreamInfo(Stream: TStream; var errors: string): Boolean;
  var
    mem: TStream;
    iPos: Int64;
    i, j, k: Integer;
    bh, lastbh: TBlockHeader;
  begin
    errors := '';
    FStreamFirstBlockNumber := 0;
    FStreamLastBlockNumber := -1;
    SetLength(FBlockHeadersFirstBytePosition, 0);
    //
    if Stream.Size < GetBlockHeaderFixedSize then
    begin
      if (Stream.Size = 0) then
      begin
        Result := True;
        exit;
      end
      else
      begin
        // Invalid stream!
        Result := false;
        errors := Format('Invalid stream size %d. Lower than minimum %d', [Stream.Size, GetBlockHeaderFixedSize]);
        exit;
      end;
    end;
    // Initialize it
    if Stream.Size > GetBlockHeaderFixedSize then
    begin
      SetLength(FBlockHeadersFirstBytePosition, 1);
      FBlockHeadersFirstBytePosition[0] := 0;
    end;
    mem := TMemoryStream.Create;
    try
      iPos := 0;
      while (iPos + GetBlockHeaderFixedSize < Stream.Size) do
      begin
        Stream.Position := iPos;
        mem.Size := 0;
        mem.CopyFrom(Stream, GetBlockHeaderFixedSize);
        // Analize it:
        mem.Position := 0;
        for i := 0 to CT_GroupBlockSize - 1 do
        begin
          mem.Read(bh.BlockNumber, SizeOf(bh.BlockNumber));
          mem.Read(bh.StreamBlockRelStartPos, SizeOf(bh.StreamBlockRelStartPos));
          mem.Read(bh.BlockSize, SizeOf(bh.BlockSize));
          if (i = 0) and (iPos = 0) then
          begin
            FStreamFirstBlockNumber := bh.BlockNumber;
            FStreamLastBlockNumber := bh.BlockNumber;
            if (0 <> bh.StreamBlockRelStartPos) then
            begin
              errors := Format('Invalid first block start rel pos %d', [bh.StreamBlockRelStartPos]);
              Result := false;
              exit;
            end;
            lastbh := bh;
          end
          else
          begin
            // Protocol 2: We can find blocks not saved, with all values to 0
            if (bh.BlockNumber = 0) then
            begin
              // This is an "empty" block. Check that ok
              if (bh.BlockNumber <> 0) or (bh.StreamBlockRelStartPos <> 0) or (bh.BlockSize <> 0) then
              begin
                errors := Format
                  ('Invalid empty block on block header. iPos=%d i=%d BlockNumber=%d relstart=%d size=%d - Last block:%d BlockNumber=%d relstart=%d size=%d',
                  [iPos, i, bh.BlockNumber, bh.StreamBlockRelStartPos, bh.BlockSize, FStreamLastBlockNumber,
                  lastbh.BlockNumber, lastbh.StreamBlockRelStartPos, lastbh.BlockSize]);
                Result := false;
                exit;
              end;
              // Ok, inc blocknumber
              inc(lastbh.BlockNumber);
            end
            else
            begin
              if (lastbh.BlockNumber + 1 <> bh.BlockNumber) or
                ((lastbh.StreamBlockRelStartPos + lastbh.BlockSize <> bh.StreamBlockRelStartPos) and (i > 0)) or
                ((0 <> bh.StreamBlockRelStartPos) and (i = 0)) then
              begin
                errors := Format
                  ('Invalid check on block header. iPos=%d i=%d BlockNumber=%d relstart=%d size=%d - Last block:%d BlockNumber=%d relstart=%d size=%d',
                  [iPos, i, bh.BlockNumber, bh.StreamBlockRelStartPos, bh.BlockSize, FStreamLastBlockNumber,
                  lastbh.BlockNumber, lastbh.StreamBlockRelStartPos, lastbh.BlockSize]);
                Result := false;
                exit;
              end
              else
              begin
                FStreamLastBlockNumber := bh.BlockNumber;
                lastbh := bh;
              end;
            end;
          end;
        end;
        iPos := iPos + GetBlockHeaderFixedSize + lastbh.StreamBlockRelStartPos + lastbh.BlockSize;
        lastbh.StreamBlockRelStartPos := 0;
        lastbh.BlockSize := 0;
      end;
      Result := True;
    finally
      mem.Free;
    end;
  end;

var
  fn: TFilename;
  fm: Word;
  exists: Boolean;
  bh: TBlockHeader;
  errors: string;
begin
  TPCThread.ProtectEnterCriticalSection(Self, FStorageLock);
  try
    if not Assigned(FBlockChainStream) then
    begin
      if FBlockChainFileName <> '' then
      begin
        fn := FBlockChainFileName
      end
      else
      begin
        fn := GetFolder(Orphan) + PathDelim + 'BlockChainStream.blocks';
      end;
      exists := FileExists(fn);
      if readonly then
      begin
        if exists then
          fm := fmOpenRead + fmShareDenyNone
        else
          raise Exception.Create('FileStorage not exists for open ReadOnly: ' + fn);
      end
      else
      begin
        if exists then
          fm := fmOpenReadWrite + fmShareDenyWrite
        else
          fm := fmCreate + fmShareDenyWrite
      end;
      FBlockChainStream := TFileStream.Create(fn, fm);
      // Init stream
      if not InitStreamInfo(FBlockChainStream, errors) then
      begin
        TLog.NewLog(ltError, ClassName, errors);
        raise Exception.Create('Error reading File: ' + fn + #10 + 'Errors:' + #10 + errors);
      end;
    end;
  except
    FStorageLock.Release;
    raise;
  end;
  Result := FBlockChainStream;
end;

procedure TFileStorage.SetBlockChainFile(BlockChainFileName: AnsiString);
begin
  ClearStream;
  FBlockChainFileName := BlockChainFileName;
end;

procedure TFileStorage.SetDatabaseFolder(const Value: AnsiString);
begin
  if FDatabaseFolder = Value then
    exit;
  FDatabaseFolder := Value;
  ClearStream;
end;

procedure TFileStorage.SetOrphan(const Value: TOrphan);
begin
  inherited;
  ClearStream;
end;

procedure TFileStorage.SetReadOnly(const Value: Boolean);
begin
  inherited;
  ClearStream;
end;

function TFileStorage.StreamBlockRead(Stream: TStream; iBlockHeaders: Integer; BlockHeaderFirstBlock, Block: Cardinal;
  Operations: TBlock): Boolean;
var
  p: Int64;
  errors: AnsiString;
  streamFirstBlock, _BlockSizeC, _intBlockIndex: Cardinal;
  _Header: TBlockHeader;
  _ops: TStream;
  _StreamBlockHeaderStartPos: Int64;
begin
  Result := false;
  if not StreamReadBlockHeader(Stream, iBlockHeaders, BlockHeaderFirstBlock, Block, false, _Header) then
    exit;

  // Calculating block position
  _StreamBlockHeaderStartPos := FBlockHeadersFirstBytePosition[iBlockHeaders];
  p := (_StreamBlockHeaderStartPos + GetBlockHeaderFixedSize) + (_Header.StreamBlockRelStartPos);
  if Stream.Size < (p + _Header.BlockSize) then
  begin
    TLog.NewLog(ltError, ClassName,
      Format('Invalid stream size. Block %d need to be at relative %d after %d = %d BlockSize:%d (Size %d)',
      [Block, _Header.StreamBlockRelStartPos, (_StreamBlockHeaderStartPos + GetBlockHeaderFixedSize), p,
      _Header.BlockSize, Stream.Size]));
    exit;
  end;
  Stream.Position := p;
  // Read the block
  // Reading size
  Stream.Read(_BlockSizeC, SizeOf(_BlockSizeC));
  if ((_BlockSizeC + SizeOf(_BlockSizeC)) > (_Header.BlockSize)) then
  begin
    TLog.NewLog(ltError, ClassName, Format('Corruption at stream Block size. Block %d SizeH:%d SizeC:%d',
      [Block, _Header.BlockSize, _BlockSizeC]));
    exit;
  end;
  // Reading Block
  _ops := TMemoryStream.Create;
  try
    _ops.CopyFrom(Stream, _BlockSizeC);
    _ops.Position := 0;
    if not Operations.LoadBlockFromStorage(_ops, errors) then
    begin
      TLog.NewLog(ltError, ClassName, 'Error reading OperationBlock ' + IntToStr(Block) + ' from stream. Errors: '
        + errors);
      exit;
    end;
    Result := True;
  finally
    _ops.Free;
  end;
end;

function TFileStorage.StreamBlockSave(Stream: TStream; iBlockHeaders: Integer; BlockHeaderFirstBlock: Cardinal;
  Operations: TBlock): Boolean;
var
  p: Int64;
  c: Cardinal;
  _Header, _HeaderPrevious: TBlockHeader;
  _intBlockIndex: Cardinal;
  _ops: TStream;
  _StreamBlockHeaderStartPos: Int64;
{$IFDEF HIGHLOG}s: string; {$ENDIF}
begin
  Result := false;
  _Header := CT_TBlockHeader_NUL;
  _Header.BlockNumber := Operations.OperationBlock.Block;
  if BlockHeaderFirstBlock > _Header.BlockNumber then
    raise Exception.Create('Dev error 20160917-3')
  else if BlockHeaderFirstBlock < _Header.BlockNumber then
  begin
    Result := StreamReadBlockHeader(Stream, iBlockHeaders, BlockHeaderFirstBlock, _Header.BlockNumber - 1, True,
      _HeaderPrevious);
    // If true then Stream is positioned on blockheader for current block
    if not Result then
    begin
      raise Exception.Create('Cannot found header of previous block ' + IntToStr(Operations.OperationBlock.Block));
    end;
    if ((_Header.BlockNumber - BlockHeaderFirstBlock) mod CT_GroupBlockSize) = 0 then
    begin
      _Header.StreamBlockRelStartPos := 0;
    end
    else
    begin
      _Header.StreamBlockRelStartPos := _HeaderPrevious.StreamBlockRelStartPos + _HeaderPrevious.BlockSize;
    end;
  end
  else
  begin
    // First block of the stream
    Result := True;
    _Header.StreamBlockRelStartPos := 0;
  end;
  _ops := TMemoryStream.Create;
  try
    Operations.SaveBlockToStorage(_ops);
    _Header.BlockSize := _ops.Size;
    // Positioning until Header Position to save Header data
    _intBlockIndex := (_Header.BlockNumber - BlockHeaderFirstBlock);
    p := Int64(_intBlockIndex) * Int64(CT_SizeOfBlockHeader);
    _StreamBlockHeaderStartPos := FBlockHeadersFirstBytePosition[iBlockHeaders];
{$IFDEF HIGHLOG}s := Format('Saving block header (block %d) at position %d', [_Header.BlockNumber, Stream.Position]);
    {$ENDIF}
    GrowStreamUntilPos(Stream, _StreamBlockHeaderStartPos + p, false);
    // Save Header
    Stream.Write(_Header.BlockNumber, SizeOf(_Header.BlockNumber));
    Stream.Write(_Header.StreamBlockRelStartPos, SizeOf(_Header.StreamBlockRelStartPos));
    c := _Header.BlockSize + SizeOf(c);
    Stream.Write(c, SizeOf(_Header.BlockSize));
    // Positioning until Header end
    GrowStreamUntilPos(Stream, _StreamBlockHeaderStartPos + GetBlockHeaderFixedSize, True);
    // And now positioning until Data:
    GrowStreamUntilPos(Stream, _StreamBlockHeaderStartPos + GetBlockHeaderFixedSize +
      _Header.StreamBlockRelStartPos, false);
{$IFDEF HIGHLOG}
    s := s + Format(' saving content at position %d (size %d)', [Stream.Position, _Header.BlockSize]);
    TLog.NewLog(ltInfo, ClassName, s);
{$ENDIF}
    // Save stream size
    Stream.Write(_Header.BlockSize, SizeOf(_Header.BlockSize));
    // Save Data
    _ops.Position := 0;
    Stream.CopyFrom(_ops, _ops.Size);
    // End Stream here
    Stream.Size := Stream.Position;
    //
    FStreamLastBlockNumber := Operations.OperationBlock.Block;
  finally
    _ops.Free;
  end;
end;

function TFileStorage.StreamReadBlockHeader(Stream: TStream; iBlockHeaders: Integer;
  BlockHeaderFirstBlock, Block: Cardinal; CanSearchBackward: Boolean; var BlockHeader: TBlockHeader): Boolean;
var
  iBlock: Cardinal;
  _iBlockHeaders: Integer;
  _StreamBlockHeaderStartPos: Int64;
  _BlockHeaderFirstBlock: Cardinal;
begin
  Result := false;
  BlockHeader := CT_TBlockHeader_NUL;
  if (BlockHeaderFirstBlock > Block) then
    raise Exception.Create('Dev error 20160917-1');
  if (BlockHeaderFirstBlock + CT_GroupBlockSize) < Block then
    raise Exception.Create('Dev error 20160917-2');
  _StreamBlockHeaderStartPos := FBlockHeadersFirstBytePosition[iBlockHeaders];
  if Stream.Size < (_StreamBlockHeaderStartPos + (GetBlockHeaderFixedSize)) then
  begin
    // Not log... it's normal when finding block
    TLog.NewLog(ltError, ClassName, Format('Invalid stream size %d < (%d + %d) Reading block %d',
      [Stream.Size, _StreamBlockHeaderStartPos, GetBlockHeaderFixedSize, Block]));
    exit;
  end;
  iBlock := Block;
  _iBlockHeaders := iBlockHeaders;
  _BlockHeaderFirstBlock := BlockHeaderFirstBlock;
  // Reading block header
  repeat
    _StreamBlockHeaderStartPos := FBlockHeadersFirstBytePosition[_iBlockHeaders];
    Stream.Position := _StreamBlockHeaderStartPos + (CT_SizeOfBlockHeader * (iBlock - _BlockHeaderFirstBlock));
    if Stream.Read(BlockHeader.BlockNumber, SizeOf(BlockHeader.BlockNumber)) < SizeOf(BlockHeader.BlockNumber) then
      exit;
    if Stream.Read(BlockHeader.StreamBlockRelStartPos, SizeOf(BlockHeader.StreamBlockRelStartPos)) <
      SizeOf(BlockHeader.StreamBlockRelStartPos) then
      exit;
    if Stream.Read(BlockHeader.BlockSize, SizeOf(BlockHeader.BlockSize)) < SizeOf(BlockHeader.BlockSize) then
      exit;
    Result := (BlockHeader.BlockNumber = iBlock);
    if (not Result) and (CanSearchBackward) then
    begin
      if (iBlock > _BlockHeaderFirstBlock) then
        dec(iBlock)
      else
      begin
        // Search on previous header...
        if (iBlockHeaders > 0) then
        begin
          dec(_iBlockHeaders);
          dec(_BlockHeaderFirstBlock, CT_GroupBlockSize);
        end
        else
        begin
          break;
        end;
      end;
    end;
  until Result or (not CanSearchBackward);
  // Positioning
  Stream.Position := FBlockHeadersFirstBytePosition[iBlockHeaders] +
    (CT_SizeOfBlockHeader * (Block + 1 - BlockHeaderFirstBlock));
  if Result then
  begin // For Backward searching...
    BlockHeader.BlockNumber := Block;
    if (_iBlockHeaders <> iBlockHeaders) then
    begin
      BlockHeader.BlockSize := 0;
      BlockHeader.StreamBlockRelStartPos := 0;
    end;
  end;
end;

procedure TFileStorage.UnlockBlockChainStream;
begin
  FStorageLock.Release;
end;

function TFileStorage.HasUpgradedToVersion2: Boolean;
var
  searchRec: TSearchRec;
begin
  HasUpgradedToVersion2 := SysUtils.FindFirst(GetFolder(Orphan) + PathDelim + '*.safebox', faArchive, searchRec) = 0;
  FindClose(searchRec);
end;

procedure TFileStorage.CleanupVersion1Data;
var
  folder: AnsiString;
  searchRec: TSearchRec;
begin
  folder := GetFolder(Orphan);
  if SysUtils.FindFirst(folder + PathDelim + '*.bank', faArchive, searchRec) = 0 then
    repeat
      SysUtils.DeleteFile(folder + PathDelim + searchRec.Name);
    until FindNext(searchRec) <> 0;
  FindClose(searchRec);
end;

end.
