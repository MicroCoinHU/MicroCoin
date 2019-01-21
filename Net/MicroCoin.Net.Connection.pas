{==============================================================================|
| MicroCoin                                                                    |
| Copyright (c) 2018 MicroCoin Developers                                      |
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
| File:       MicroCoin.Net.Connection.pas                                     |
| Created at: 2018-08-28                                                       |
| Purpose:    Net protocol implementation                                      |
| Todo:                                                                        |
|   - Clean up code                                                            |
|==============================================================================}

unit MicroCoin.Net.Connection;

{$ifdef FPC}
  {$mode delphi}
{$endif}


interface

uses SysUtils, Classes, UTCPIP, MicroCoin.BlockChain.BlockHeader, UThread,
  MicroCoin.Account.AccountKey, MicroCoin.Common.Lists, MicroCoin.Net.Protocol,
  MicroCoin.Net.NodeServer, UBaseTypes,
  MicroCoin.Transaction.ITransaction,
  MicroCoin.Transaction.HashTree, MicroCoin.BlockChain.Block, ULog,
  MicroCoin.Net.ConnectionBase;

type
  TNetConnection = class(TNetConnectionBase)
  strict private
    FBufferLock: TPCCriticalSection;
    FBufferReceivedOperationsHash: TOrderedRawList;
    FBufferToSendOperations: TTransactionHashTree;
    FIsDownloadingBlocks: Boolean;
    FRemoteOperationBlock: TBlockHeader;
    FRemoteAccumulatedWork: UInt64;
    FClientPublicKey: TAccountKey;
    FTimestampDiff: Integer;
    FClientAppVersion: AnsiString;
  strict protected
    procedure DoProcess_GetBlocks_Request(HeaderData: TNetHeaderData; DataBuffer: TStream); override;
    procedure DoProcess_GetBlocks_Response(HeaderData: TNetHeaderData; DataBuffer: TStream); override;
    procedure DoProcess_GetOperationsBlock_Request(HeaderData: TNetHeaderData; DataBuffer: TStream); override;
    procedure DoProcess_AddOperations(HeaderData: TNetHeaderData; DataBuffer: TStream); override;
    procedure DoProcess_GetAccountStorage_Request(HeaderData: TNetHeaderData; DataBuffer: TStream); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    function AddOperationsToBufferForSend(Operations: TTransactionHashTree): Integer;

    function Send_Hello(NetTranferType: TNetTransferType; request_id: Integer): Boolean; override;
    function Send_NewBlockFound(const NewBlock: TBlock): Boolean; override;
    function Send_GetBlocks(StartAddress, quantity: Cardinal; var request_id: Cardinal): Boolean; override;
    function Send_AddOperations(Operations: TTransactionHashTree): Boolean; override;
    function Send_Message(const TheMessage: AnsiString): Boolean; override;

    property IsDownloadingBlocks: Boolean read FIsDownloadingBlocks write FIsDownloadingBlocks;
    property RemoteOperationBlock: TBlockHeader read FRemoteOperationBlock write FRemoteOperationBlock;
    property RemoteAccumulatedWork: UInt64 read FRemoteAccumulatedWork write FRemoteAccumulatedWork;
    property ClientPublicKey: TAccountKey read FClientPublicKey write FClientPublicKey;
    property TimestampDiff: Integer read FTimestampDiff write FTimestampDiff;
    property ClientAppVersion: AnsiString read FClientAppVersion write FClientAppVersion;
  end;

  PNetRequestRegistered = ^TNetRequestRegistered;

  TNetRequestRegistered = record
    NetClient: TNetConnectionBase;
    operation: Word;
    RequestId: Cardinal;
    SendTime: TDateTime;
  end;

implementation

uses UTime, MicroCoin.Net.ConnectionManager, MicroCoin.Common.Config, UCrypto,
  UECIES, MicroCoin.Common.Stream,
  UChunk, MicroCoin.Net.Client,{$IFDEF MSWINDOWS} Windows,{$ENDIF} MicroCoin.Transaction.Base,
  MicroCoin.Net.Utils,
  MicroCoin.Transaction.Manager, MicroCoin.Node.Node, MicroCoin.Account.Storage;

constructor TNetConnection.Create(AOwner: TComponent);
begin
  FClientAppVersion := '';
  FBufferLock := TPCCriticalSection.Create('TNetConnectionBase_BufferLock');
  FBufferReceivedOperationsHash := TOrderedRawList.Create;
  FBufferToSendOperations := TTransactionHashTree.Create;
  FRemoteOperationBlock := TBlockHeader.Empty;
  FRemoteAccumulatedWork := 0;
  FClientPublicKey := TAccountKey.Empty;
  FTimestampDiff := 0;
  inherited;
end;

destructor TNetConnection.Destroy;
begin
    FreeAndNil(FBufferLock);
    FreeAndNil(FBufferReceivedOperationsHash);
    FreeAndNil(FBufferToSendOperations);
  inherited;
end;

procedure TNetConnection.DoProcess_AddOperations(HeaderData: TNetHeaderData; DataBuffer: TStream);
var
  c, i: Integer;
  xTransactionType: Byte;
  xTransactionClass: TTransactionClass;
  xTransaction: ITransaction;
  xTransactions: TTransactionHashTree;
  errors: AnsiString;
  DoDisconnect: Boolean;
begin
  DoDisconnect := true;
  xTransactions := TTransactionHashTree.Create;
  try
    if HeaderData.HeaderType <> ntp_autosend then
    begin
      errors := 'Not autosend';
      exit;
    end;
    if DataBuffer.Size < 4 then
    begin
      errors := 'Invalid databuffer size';
      exit;
    end;
    DataBuffer.Read(c, 4);
    for i := 1 to c do
    begin
      errors := 'Invalid operation ' + Inttostr(i) + '/' + Inttostr(c);
      if not DataBuffer.Read(xTransactionType, 1) = 1 then
        exit;
      xTransactionClass := TTransactionManager.GetTransactionPlugin(xTransactionType);
      if not Assigned(xTransactionClass) then
        exit;
      xTransaction := xTransactionClass.Create;
      try
        xTransaction.LoadFromNettransfer(DataBuffer);
        xTransactions.AddTransactionToHashTree(xTransaction);
      finally
        // op.Free;
      end;
    end;
    DoDisconnect := false;
  finally
    try
      if DoDisconnect then
      begin
        DisconnectInvalidClient(false, errors + ' > ' + HeaderData.ToString + ' BuffSize: '
          + Inttostr(DataBuffer.Size));
      end
      else
      begin
        // Add to received buffer
        FBufferLock.Acquire;
        try
          for i := 0 to xTransactions.TransactionCount - 1 do
          begin
            xTransaction := xTransactions.GetTransaction(i);
            FBufferReceivedOperationsHash.Add(xTransaction.Sha256);
            c := FBufferToSendOperations.IndexOf(xTransaction);
            if (c >= 0) then
              FBufferToSendOperations.Delete(c);
          end;
        finally
          FBufferLock.Release;
        end;
        TNode.Node.AddOperations(Self, xTransactions, nil, errors);
      end;
    finally
      xTransactions.Free;
    end;
  end;
end;

procedure TNetConnection.DoProcess_GetBlocks_Request(HeaderData: TNetHeaderData; DataBuffer: TStream);
var
  b, b_start, b_end: Cardinal;
  xBlock: TBlock;
  xMemoryStream: TMemoryStream;
  c: Cardinal;
  xErrors: AnsiString;
  xDoDisconnect: Boolean;
  xPosQuantity: Int64;
begin
  xDoDisconnect := true;
  try
    if HeaderData.HeaderType <> ntp_request then
    begin
      xErrors := 'Not request';
      exit;
    end;
    // DataBuffer contains: from and to
    xErrors := 'Invalid structure';
    if (DataBuffer.Size - DataBuffer.Position < 8) then
    begin
      exit;
    end;
    DataBuffer.Read(b_start, 4);
    DataBuffer.Read(b_end, 4);
    if (b_start < 0) or (b_start > b_end) then
    begin
      xErrors := 'Invalid structure start or end: ' + Inttostr(b_start) + ' ' + Inttostr(b_end);
      exit;
    end;
    if (b_end >= TConnectionManager.Instance.Bank.BlocksCount) then
      b_end := TConnectionManager.Instance.Bank.BlocksCount - 1;

    xDoDisconnect := false;

    xMemoryStream := TMemoryStream.Create;
    try
      xBlock := TBlock.Create(TConnectionManager.Instance.Bank);
      try
        c := b_end - b_start + 1;
        xPosQuantity := xMemoryStream.Position;
        xMemoryStream.Write(c, 4);
        c := 0;
        b := b_start;
        for b := b_start to b_end do
        begin
          Inc(c);
          if TConnectionManager.Instance.Bank.LoadTransactions(xBlock, b) then
          begin
            xBlock.SaveBlockToStream(false, xMemoryStream);
            // db.SaveToFile('stream0');
          end
          else
          begin
            Send_Error(ntp_response, HeaderData.Operation, HeaderData.RequestId, cNetError_InternalServerError,
              'Operations of block:' + Inttostr(b) + ' not found');
            exit;
          end;
          // Build 1.0.5 To prevent high data over net in response (Max 2 Mb of data)
          if (xMemoryStream.Size > (1024 * 1024 * 2)) then
          begin
            // Stop
            xMemoryStream.Position := xPosQuantity;
            xMemoryStream.Write(c, 4);
            // BUG of Build 1.0.5 !!! Need to break bucle OH MY GOD!
            xMemoryStream.Position := xMemoryStream.Size;
            break;
          end;
        end;
        // db.SaveToFile('stream1');
        Send(ntp_response, HeaderData.Operation, 0, HeaderData.RequestId, xMemoryStream);
      finally
        xBlock.Free;
      end;
    finally
      xMemoryStream.Free;
    end;
    TLog.NewLog(ltdebug, Classname, 'Sending operations from block ' + Inttostr(b_start) + ' to ' + Inttostr(b_end));
  finally
    if xDoDisconnect then
    begin
      DisconnectInvalidClient(false, xErrors + ' > ' + (HeaderData.ToString) + ' BuffSize: ' +
        Inttostr(DataBuffer.Size));
    end;
  end;
end;

procedure TNetConnection.DoProcess_GetBlocks_Response(HeaderData: TNetHeaderData; DataBuffer: TStream);
var
  xBlock: TBlock;
  xCount, i: Cardinal;
  xNewBlockAccount: TAccountStorageEntry;
  xErrors: AnsiString;
  xDoDisconnect: Boolean;
begin
  xDoDisconnect := true;
  try
    if HeaderData.HeaderType <> ntp_response then
    begin
      xErrors := 'Not response';
      exit;
    end;
    if HeaderData.IsError then
    begin
      xDoDisconnect := false;
      exit; //
    end;
    // DataBuffer contains: from and to
    xErrors := 'Invalid structure';
    xBlock := TBlock.Create(nil);
    try
      xBlock.BlockManager := TNode.Node.BlockManager;
      if DataBuffer.Size - DataBuffer.Position < 4 then
      begin
        DisconnectInvalidClient(false, 'DoProcess_GetBlocks_Response invalid format: ' + xErrors);
        exit;
      end;
      DataBuffer.Read(xCount, 4);
      xDoDisconnect := false;
      for i := 1 to xCount do
      begin
        if not xBlock.LoadBlockFromStream(DataBuffer, xErrors) then
        begin
          xErrors := 'Error decoding block ' + Inttostr(i) + '/' + Inttostr(xCount) + ' Errors:' + xErrors;
          xDoDisconnect := true;
          exit;
        end;
        if (xBlock.BlockHeader.Block = TNode.Node.BlockManager.BlocksCount) then
        begin
          if (TNode.Node.BlockManager.AddNewBlockToBlockChain(xBlock,
            TConnectionManager.Instance.NetworkAdjustedTime.GetMaxAllowedTimestampForNewBlock, xNewBlockAccount, xErrors))
          then
          begin
            // Ok, one more!
          end
          else
          begin
            // Is not a valid entry????
            // Perhaps an orphan blockchain: Me or Client!
            TLog.NewLog(ltInfo, Classname, 'Distinct operation block found! My:' +
              TNode.Node.BlockManager.AccountStorage.Blocks[TNode.Node.BlockManager.BlocksCount - 1]
              .BlockHeader.ToString() + ' remote:' + xBlock.BlockHeader.ToString() + ' Errors: ' + xErrors);
          end;
        end
        else
        begin
          // Receiving an unexpected operationblock
          TLog.NewLog(ltError, Classname, 'Received a distinct block, finalizing: ' +
            xBlock.BlockHeader.ToString() + ' (My block: ' +
            TNode.Node.BlockManager.LastBlock.ToString() + ')');
          FIsDownloadingBlocks := false;
          exit;
        end;
      end;
      FIsDownloadingBlocks := false;
      if ((xCount > 0) and (FRemoteOperationBlock.Block >= TNode.Node.BlockManager.BlocksCount)) then
      begin
        Send_GetBlocks(TNode.Node.BlockManager.BlocksCount, 100, i);
      end;
      TNode.Node.NotifyBlocksChanged;
    finally
      xBlock.Free;
    end;
  finally
    if xDoDisconnect then
    begin
      DisconnectInvalidClient(false, xErrors + ' > ' + (HeaderData.ToString) + ' BuffSize: ' +
        Inttostr(DataBuffer.Size));
    end;
  end;
end;

procedure TNetConnection.DoProcess_GetOperationsBlock_Request(HeaderData: TNetHeaderData; DataBuffer: TStream);
const
  CT_Max_Positions = 10;
var
  inc_b, b, b_start, b_end, total_b: Cardinal;
  db, msops: TMemoryStream;
  errors, blocksstr: AnsiString;
  DoDisconnect: Boolean;
  ob: TBlockHeader;
begin
  blocksstr := '';
  DoDisconnect := true;
  try
    if HeaderData.HeaderType <> ntp_request then
    begin
      errors := 'Not request';
      exit;
    end;
    errors := 'Invalid structure';
    if (DataBuffer.Size - DataBuffer.Position < 8) then
    begin
      exit;
    end;
    DataBuffer.Read(b_start, 4);
    DataBuffer.Read(b_end, 4);
    if (b_start < 0) or (b_start > b_end) or (b_start >= TNode.Node.BlockManager.BlocksCount) then
    begin
      errors := 'Invalid start (' + Inttostr(b_start) + ') or end (' + Inttostr(b_end) + ') of count (' +
        Inttostr(TNode.Node.BlockManager.BlocksCount) + ')';
      exit;
    end;

    DoDisconnect := false;

    // Build 1.4
    if b_start < TNode.Node.BlockManager.Storage.FirstBlock then
    begin
      b_start := TNode.Node.BlockManager.Storage.FirstBlock;
      if b_end < b_start then
      begin
        errors := 'Block:' + Inttostr(b_end) + ' not found';
        Send_Error(ntp_response, HeaderData.Operation, HeaderData.RequestId, cNetError_InternalServerError, errors);
        exit;
      end;
    end;

    if (b_end >= TNode.Node.BlockManager.BlocksCount) then
      b_end := TNode.Node.BlockManager.BlocksCount - 1;
    inc_b := ((b_end - b_start) div CT_Max_Positions) + 1;
    msops := TMemoryStream.Create;
    try
      b := b_start;
      total_b := 0;
      repeat
        ob := TNode.Node.BlockManager.AccountStorage.Blocks[b].BlockHeader;
        if TBlock.SaveBlockToStream(ob, msops) then
        begin
          blocksstr := blocksstr + Inttostr(b) + ',';
          b := b + inc_b;
          Inc(total_b);
        end
        else
        begin
          errors := 'ERROR DEV 20170522-1 block:' + Inttostr(b);
          Send_Error(ntp_response, HeaderData.Operation, HeaderData.RequestId, cNetError_InternalServerError, errors);
          exit;
        end;
      until (b > b_end);
      db := TMemoryStream.Create;
      try
        db.Write(total_b, 4);
        db.WriteBuffer(msops.Memory^, msops.Size);
        Send(ntp_response, HeaderData.Operation, 0, HeaderData.RequestId, db);
      finally
        db.Free;
      end;
    finally
      msops.Free;
    end;
    TLog.NewLog(ltdebug, Classname, 'Sending ' + Inttostr(total_b) + ' operations block from block ' + Inttostr(b_start)
      + ' to ' + Inttostr(b_end) + ' ' + blocksstr);
  finally
    if DoDisconnect then
    begin
      DisconnectInvalidClient(false, errors + ' > ' + (HeaderData.ToString) + ' BuffSize: ' +
        Inttostr(DataBuffer.Size));
    end;
  end;
end;

procedure TNetConnection.DoProcess_GetAccountStorage_Request(HeaderData: TNetHeaderData; DataBuffer: TStream);
var
  xBlockCount: Cardinal;
  xAccountStorageHash: TRawBytes;
  xFrom, xTo: Cardinal;
  xAccountStorageStream: TStream;
  xResponseStream: TStream;
  antPos: Int64;
  xAccountStorageHeader: TAccountStorageHeader;
  xErrors: AnsiString;
begin
  {
    This call is used to obtain a chunk of the safebox
    Request:
    BlockCount (4 bytes) - The safebox checkpoint
    SafeboxHash (AnsiString) - The safeboxhash of that checkpoint
    StartPos (4 bytes) - The start index (0..BlockCount-1)
    EndPos   (4 bytes) - The final index (0..BlockCount-1)
    If valid info:
    - If available will return a LZIP chunk of safebox
    - If not available (requesting for an old safebox) will retun not available
    If not valid will disconnect
  }
  DataBuffer.Read(xBlockCount, SizeOf(xBlockCount));
  DataBuffer.ReadAnsiString(xAccountStorageHash);
  DataBuffer.Read(xFrom, SizeOf(xFrom));
  DataBuffer.Read(xTo, SizeOf(xTo));
  //
  xAccountStorageStream := TNode.Node.BlockManager.Storage.CreateAccountStorageStream(xBlockCount);
  try
    xResponseStream := TMemoryStream.Create;
    try
      if not Assigned(xAccountStorageStream) then
      begin
        Send_Error(ntp_response, HeaderData.Operation, cNetError_AccountStorageNotFound, HeaderData.RequestId,
          Format('Safebox for block %d not found', [xBlockCount]));
        exit;
      end;
      antPos := xAccountStorageStream.Position;
      // TODO: exception
      xAccountStorageHeader := TAccountStorageHeader.LoadFromStream(xAccountStorageStream);
      if xAccountStorageHeader.AccountStorageHash <> xAccountStorageHash then
      begin
        DisconnectInvalidClient(false, Format('Invalid safeboxhash on GetSafeBox request (Real:%s > Requested:%s)',
          [TBaseType.ToHexaString(xAccountStorageHeader.AccountStorageHash),
          TBaseType.ToHexaString(xAccountStorageHash)]));
        exit;
      end;
      // Response:
      xAccountStorageStream.Position := antPos;
      if not TPCChunk.SaveChunkFromAccountStorage(xAccountStorageStream, xResponseStream, xFrom, xTo, xErrors) then
      begin
        TLog.NewLog(ltError, Classname, 'Error saving chunk: ' + xErrors);
        exit;
      end;
      // Sending
      Send(ntp_response, HeaderData.Operation, 0, HeaderData.RequestId, xResponseStream);
    finally
      xResponseStream.Free;
    end;
  finally
    FreeAndNil(xAccountStorageStream);
  end;
end;

function TNetConnection.Send_AddOperations(Operations: TTransactionHashTree): Boolean;
var
  data: TMemoryStream;
  c1, xRequestId: Cardinal;
  i, xNumberOfTransactionsToSend: Integer;
  xTransactionType: Byte;
begin
  Result := false;
  if not Connected then
    exit;
  NetLock.Acquire;
  try
    xNumberOfTransactionsToSend := 0;
    FBufferLock.Acquire;
    try
      if Assigned(Operations) then
      begin
        for i := 0 to Operations.TransactionCount - 1 do
        begin
          if FBufferReceivedOperationsHash.IndexOf(Operations.GetTransaction(i).Sha256) < 0 then
          begin
            FBufferReceivedOperationsHash.Add(Operations.GetTransaction(i).Sha256);
            if FBufferToSendOperations.IndexOf(Operations.GetTransaction(i)) < 0 then
            begin
              FBufferToSendOperations.AddTransactionToHashTree(Operations.GetTransaction(i));
            end;
          end;
        end;
        xNumberOfTransactionsToSend := Operations.TransactionCount;
      end;
      if FBufferToSendOperations.TransactionCount > 0 then
      begin
        TLog.NewLog(ltdebug, Classname, Format('Sending %d Operations to %s (inProc:%d, Received:%d)',
          [FBufferToSendOperations.TransactionCount, ClientRemoteAddr, xNumberOfTransactionsToSend,
          FBufferReceivedOperationsHash.Count]));
        data := TMemoryStream.Create;
        try
          xRequestId := TConnectionManager.Instance.NewRequestId;
          c1 := FBufferToSendOperations.TransactionCount;
          data.Write(c1, 4);
          for i := 0 to FBufferToSendOperations.TransactionCount - 1 do
          begin
            xTransactionType := FBufferToSendOperations.GetTransaction(i).TransactionType;
            data.Write(xTransactionType, 1);
            FBufferToSendOperations.GetTransaction(i).SaveToNettransfer(data);
          end;
          Send(ntp_autosend, cNetOp_AddOperations, 0, xRequestId, data);
          FBufferToSendOperations.ClearHastThree;
        finally
          data.Free;
        end;
      end
      else
        TLog.NewLog(ltdebug, Classname, Format('Not sending any operations to %s (inProc:%d, Received:%d, Sent:%d)',
          [ClientRemoteAddr, xNumberOfTransactionsToSend, FBufferReceivedOperationsHash.Count,
          FBufferToSendOperations.TransactionCount]));
    finally
      FBufferLock.Release;
    end;
  finally
    NetLock.Release;
  end;
  Result := Connected;
end;

function TNetConnection.Send_GetBlocks(StartAddress, quantity: Cardinal; var request_id: Cardinal): Boolean;
var
  data: TMemoryStream;
  c1, c2: Cardinal;
begin
  Result := false;
  request_id := 0;
  if (FRemoteOperationBlock.Block < TConnectionManager.Instance.Bank.BlocksCount) or (FRemoteOperationBlock.Block = 0)
  then
    exit;
  if not Connected then
    exit;
  // First receive operations from
  data := TMemoryStream.Create;
  try
    if TConnectionManager.Instance.Bank.BlocksCount = 0 then
      c1 := 0
    else
      c1 := StartAddress;
    if (quantity = 0) then
    begin
      if FRemoteOperationBlock.Block > 0 then
        c2 := FRemoteOperationBlock.Block
      else
        c2 := c1 + 100;
    end
    else
      c2 := c1 + quantity - 1;
    // Build 1.0.5 BUG - Always query for ONLY 1 if Build is lower or equal to 1.0.5
    if ((FClientAppVersion = '') or ((length(FClientAppVersion) = 5) and (FClientAppVersion <= '1.0.5'))) then
    begin
      c2 := c1;
    end;
    data.Write(c1, 4);
    data.Write(c2, 4);
    request_id := TConnectionManager.Instance.NewRequestId;
    TConnectionManager.Instance.RegisterRequest(Self, cNetOp_GetBlocks, request_id);
    TLog.NewLog(ltdebug, Classname, Format('Send GET BLOCKS start:%d quantity:%d (from:%d to %d)',
      [StartAddress, quantity, StartAddress, quantity + StartAddress]));
    FIsDownloadingBlocks := quantity > 1;
    Send(ntp_request, cNetOp_GetBlocks, 0, request_id, data);
    Result := Connected;
  finally
    data.Free;
  end;
end;

function TNetConnection.Send_Hello(NetTranferType: TNetTransferType; request_id: Integer): Boolean;
{ HELLO command:
  - Operation stream
  - My Active server port (0 if no active). (2 bytes)
  - A Random Longint (4 bytes) to check if its myself connection to my server socket
  - My Unix Timestamp (4 bytes)
  - Registered node servers count
  (For each)
  - ip (string)
  - port (2 bytes)
  - last_connection UTS (4 bytes)
  - My Server port (2 bytes)
  - If this is a response:
  - If remote operation block is lower than me:
  - Send My Operation Stream in the same block thant requester
}
var
  data: TMemoryStream;
  i: Integer;
  nsa: TNodeServer;
  nsarr: TNodeServerAddressArray;
  w: Word;
  currunixtimestamp: Cardinal;
begin
  Result := false;
  if not Connected then
    exit;
  // Send Hello command:
  data := TMemoryStream.Create;
  try
    if NetTranferType = ntp_request then
    begin
      TConnectionManager.Instance.RegisterRequest(Self, cNetOp_Hello, request_id);
    end;
    if TNode.Node.NetServer.Active then
      w := TNode.Node.NetServer.port
    else
      w := 0;
    // Save active server port (2 bytes). 0 = No active server port
    data.Write(w, 2);
    // Save My connection public key
    data.WriteAnsiString(TConnectionManager.Instance.NodePrivateKey.PublicKey.ToRawString);
    // Save my Unix timestamp (4 bytes)
    currunixtimestamp := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
    data.Write(currunixtimestamp, 4);
    // Save last operations block
    TBlock.SaveBlockToStream(TNode.Node.BlockManager.LastBlock, data);
    nsarr := TConnectionManager.Instance.GetValidNodeServers(true, cMAX_NODESERVERS_ON_HELLO);
    i := length(nsarr);
    data.Write(i, 4);
    for i := 0 to high(nsarr) do
    begin
      nsa := nsarr[i];
      data.WriteAnsiString(nsa.ip);
      data.Write(nsa.port, 2);
      data.Write(nsa.last_connection, 4);
    end;
    // Send client version
    data.WriteAnsiString(ClientAppVersion{$IFDEF LINUX} + ' Linux'{$ELSE} + ' Windows'{$ENDIF}{$IFDEF FPC}{$IFDEF LCL} + ' '{$ELSE} + ' '{$ENDIF}{$ELSE}+' '{$ENDIF}{$IFDEF DEBUG}+' Debug'{$ELSE}+''{$ENDIF}{$IFDEF CONSOLE}+' daemon'{$ENDIF});
    // Build 1.5 send accumulated work
    data.Write(TNode.Node.BlockManager.AccountStorage.WorkSum, SizeOf(TNode.Node.BlockManager.AccountStorage.WorkSum));
    //
    // data.SaveToFile('./hello.bin');
    Send(NetTranferType, cNetOp_Hello, 0, request_id, data);
    Result := Client.Connected;
  finally
    data.Free;
  end;
end;

function TNetConnection.Send_Message(const TheMessage: AnsiString): Boolean;
var
  data: TStream;
  cyp: TRawBytes;
begin
  Result := false;
  if not Connected then
    exit;
  data := TMemoryStream.Create;
  try
    // Cypher message:
    cyp := ECIESEncrypt(FClientPublicKey, TheMessage);
    data.WriteAnsiString(cyp);
    Send(ntp_autosend, cNetOp_Message, 0, 0, data);
    Result := true;
  finally
    data.Free;
  end;
end;

function TNetConnection.Send_NewBlockFound(const NewBlock: TBlock): Boolean;
var
  data: TStream;
  request_id: Integer;
begin
  Result := false;
  if not Connected then
    exit;
  NetLock.Acquire;
  try
    // Clear buffers
    FBufferLock.Acquire;
    try
      FBufferReceivedOperationsHash.Clear;
      FBufferToSendOperations.ClearHastThree;
    finally
      FBufferLock.Release;
    end;
    // Checking if operationblock is the same to prevent double messaging...
    if (TBlock.Equals(FRemoteOperationBlock, NewBlock.BlockHeader)) then
    begin
      TLog.NewLog(ltdebug, Classname, 'This connection has the same block, does not need to send');
      exit;
    end;
    if (TNode.Node.BlockManager.BlocksCount <> NewBlock.BlockHeader.Block + 1) then
    begin
      TLog.NewLog(ltdebug, Classname, 'The block number ' + Inttostr(NewBlock.BlockHeader.Block) +
        ' is not equal to current blocks stored in bank (' + Inttostr(TNode.Node.BlockManager.BlocksCount) + '), finalizing');
      exit;
    end;
    data := TMemoryStream.Create;
    try
      request_id := TConnectionManager.Instance.NewRequestId;
      NewBlock.SaveBlockToStream(false, data);
      data.Write(TNode.Node.BlockManager.AccountStorage.WorkSum, SizeOf(TNode.Node.BlockManager.AccountStorage.WorkSum));
      Send(ntp_autosend, cNetOp_NewBlock, 0, request_id, data);
    finally
      data.Free;
    end;
  finally
    NetLock.Release;
  end;
  Result := Connected;
end;

function TNetConnection.AddOperationsToBufferForSend(Operations: TTransactionHashTree): Integer;
var
  i: Integer;
begin
  Result := 0;
  try
    FBufferLock.Acquire;
    try
      for i := 0 to Operations.TransactionCount - 1 do
      begin
        if FBufferReceivedOperationsHash.IndexOf(Operations.GetTransaction(i).Sha256) < 0 then
        begin
          FBufferReceivedOperationsHash.Add(Operations.GetTransaction(i).Sha256);
          if FBufferToSendOperations.IndexOf(Operations.GetTransaction(i)) < 0 then
          begin
            FBufferToSendOperations.AddTransactionToHashTree(Operations.GetTransaction(i));
            Inc(Result);
          end;
        end;
      end;
    finally
      FBufferLock.Release;
    end;
  except
    on E: Exception do
    begin
      TLog.NewLog(ltError, Classname, 'Error at AddOperationsToBufferForSend (' + E.Classname + '): ' + E.Message);
      Result := 0;
    end;
  end;
end;


end.
