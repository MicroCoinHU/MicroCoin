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
  MicroCoin.Net.NodeServer,
  MicroCoin.Transaction.Itransaction,
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
    procedure DoProcess_Hello(HeaderData: TNetHeaderData; DataBuffer: TStream); override;
    procedure DoProcess_Message(HeaderData: TNetHeaderData; DataBuffer: TStream); override;
    procedure DoProcess_GetBlocks_Request(HeaderData: TNetHeaderData; DataBuffer: TStream); override;
    procedure DoProcess_GetBlocks_Response(HeaderData: TNetHeaderData; DataBuffer: TStream); override;
    procedure DoProcess_GetOperationsBlock_Request(HeaderData: TNetHeaderData; DataBuffer: TStream); override;
    procedure DoProcess_NewBlock(HeaderData: TNetHeaderData; DataBuffer: TStream); override;
    procedure DoProcess_AddOperations(HeaderData: TNetHeaderData; DataBuffer: TStream); override;
    procedure DoProcess_GetSafeBox_Request(HeaderData: TNetHeaderData; DataBuffer: TStream); override;
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
    property RemoteOperationBlock: TBlockHeader read FRemoteOperationBlock;
    property RemoteAccumulatedWork: UInt64 read FRemoteAccumulatedWork write FRemoteAccumulatedWork;
    property ClientPublicKey: TAccountKey read FClientPublicKey write FClientPublicKey;
    property TimestampDiff: Integer read FTimestampDiff;
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

uses UTime, MicroCoin.Net.ConnectionManager, UConst, UCrypto,
  UECIES,
  UChunk, MicroCoin.Net.Client,{$IFDEF MSWINDOWS} Windows,{$ENDIF} MicroCoin.Transaction.Base,
  MicroCoin.Net.Utils,
  MicroCoin.Transaction.Manager, MicroCoin.Node.Node, MicroCoin.Account.Storage;

constructor TNetConnection.Create(AOwner: TComponent);
begin
  FClientAppVersion := '';
  FBufferLock := TPCCriticalSection.Create('TNetConnectionBase_BufferLock');
  FBufferReceivedOperationsHash := TOrderedRawList.Create;
  FBufferToSendOperations := TTransactionHashTree.Create;
  FRemoteOperationBlock := CT_OperationBlock_NUL;
  FRemoteAccumulatedWork := 0;
  FClientPublicKey := CT_TECDSA_Public_Nul;
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
  optype: Byte;
  opclass: TTransactionClass;
  op: ITransaction;
  Operations: TTransactionHashTree;
  errors: AnsiString;
  DoDisconnect: Boolean;
begin
  DoDisconnect := true;
  Operations := TTransactionHashTree.Create;
  try
    if HeaderData.header_type <> ntp_autosend then
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
      if not DataBuffer.Read(optype, 1) = 1 then
        exit;
      opclass := TTransactionManager.GetTransactionPlugin(optype);
      if not Assigned(opclass) then
        exit;
      op := opclass.Create;
      try
        op.LoadFromNettransfer(DataBuffer);
        Operations.AddTransactionToHashTree(op);
      finally
        // op.Free;
      end;
    end;
    DoDisconnect := false;
  finally
    try
      if DoDisconnect then
      begin
        DisconnectInvalidClient(false, errors + ' > ' + TConnectionManager.HeaderDataToText(HeaderData) + ' BuffSize: '
          + Inttostr(DataBuffer.Size));
      end
      else
      begin
        // Add to received buffer
        FBufferLock.Acquire;
        try
          for i := 0 to Operations.OperationsCount - 1 do
          begin
            op := Operations.GetOperation(i);
            FBufferReceivedOperationsHash.Add(op.Sha256);
            c := FBufferToSendOperations.IndexOf(op);
            if (c >= 0) then
              FBufferToSendOperations.Delete(c);
          end;
        finally
          FBufferLock.Release;
        end;
        TNode.Node.AddOperations(Self, Operations, nil, errors);
      end;
    finally
      Operations.Free;
    end;
  end;
end;

procedure TNetConnection.DoProcess_GetBlocks_Request(HeaderData: TNetHeaderData; DataBuffer: TStream);
var
  b, b_start, b_end: Cardinal;
  op: TBlock;
  db: TMemoryStream;
  c: Cardinal;
  errors: AnsiString;
  DoDisconnect: Boolean;
  posquantity: Int64;
begin
  DoDisconnect := true;
  try
    if HeaderData.header_type <> ntp_request then
    begin
      errors := 'Not request';
      exit;
    end;
    // DataBuffer contains: from and to
    errors := 'Invalid structure';
    if (DataBuffer.Size - DataBuffer.Position < 8) then
    begin
      exit;
    end;
    DataBuffer.Read(b_start, 4);
    DataBuffer.Read(b_end, 4);
    if (b_start < 0) or (b_start > b_end) then
    begin
      errors := 'Invalid structure start or end: ' + Inttostr(b_start) + ' ' + Inttostr(b_end);
      exit;
    end;
    if (b_end >= TConnectionManager.Instance.Bank.BlocksCount) then
      b_end := TConnectionManager.Instance.Bank.BlocksCount - 1;

    DoDisconnect := false;

    db := TMemoryStream.Create;
    try
      op := TBlock.Create(TConnectionManager.Instance.Bank);
      try
        c := b_end - b_start + 1;
        posquantity := db.Position;
        db.Write(c, 4);
        c := 0;
        b := b_start;
        for b := b_start to b_end do
        begin
          Inc(c);
          if TConnectionManager.Instance.Bank.LoadTransactions(op, b) then
          begin
            op.SaveBlockToStream(false, db);
            // db.SaveToFile('stream0');
          end
          else
          begin
            SendError(ntp_response, HeaderData.operation, HeaderData.request_id, CT_NetError_InternalServerError,
              'Operations of block:' + Inttostr(b) + ' not found');
            exit;
          end;
          // Build 1.0.5 To prevent high data over net in response (Max 2 Mb of data)
          if (db.Size > (1024 * 1024 * 2)) then
          begin
            // Stop
            db.Position := posquantity;
            db.Write(c, 4);
            // BUG of Build 1.0.5 !!! Need to break bucle OH MY GOD!
            db.Position := db.Size;
            break;
          end;
        end;
        // db.SaveToFile('stream1');
        Send(ntp_response, HeaderData.operation, 0, HeaderData.request_id, db);
      finally
        op.Free;
      end;
    finally
      db.Free;
    end;
    TLog.NewLog(ltdebug, Classname, 'Sending operations from block ' + Inttostr(b_start) + ' to ' + Inttostr(b_end));
  finally
    if DoDisconnect then
    begin
      DisconnectInvalidClient(false, errors + ' > ' + TConnectionManager.HeaderDataToText(HeaderData) + ' BuffSize: ' +
        Inttostr(DataBuffer.Size));
    end;
  end;
end;

procedure TNetConnection.DoProcess_GetBlocks_Response(HeaderData: TNetHeaderData; DataBuffer: TStream);
var
  op: TBlock;
  opcount, i: Cardinal;
  newBlockAccount: TAccountStorageEntry;
  errors: AnsiString;
  DoDisconnect: Boolean;
begin
  DoDisconnect := true;
  try
    if HeaderData.header_type <> ntp_response then
    begin
      errors := 'Not response';
      exit;
    end;
    if HeaderData.is_error then
    begin
      DoDisconnect := false;
      exit; //
    end;
    // DataBuffer contains: from and to
    errors := 'Invalid structure';
    op := TBlock.Create(nil);
    try
      op.BlockManager := TNode.Node.BlockManager;
      if DataBuffer.Size - DataBuffer.Position < 4 then
      begin
        DisconnectInvalidClient(false, 'DoProcess_GetBlocks_Response invalid format: ' + errors);
        exit;
      end;
      DataBuffer.Read(opcount, 4);
      DoDisconnect := false;
      for i := 1 to opcount do
      begin
        if not op.LoadBlockFromStream(DataBuffer, errors) then
        begin
          errors := 'Error decoding block ' + Inttostr(i) + '/' + Inttostr(opcount) + ' Errors:' + errors;
          DoDisconnect := true;
          exit;
        end;
        if (op.BlockHeader.Block = TNode.Node.BlockManager.BlocksCount) then
        begin
          if (TNode.Node.BlockManager.AddNewBlockToBlockChain(op,
            TConnectionManager.Instance.NetworkAdjustedTime.GetMaxAllowedTimestampForNewBlock, newBlockAccount, errors))
          then
          begin
            // Ok, one more!
          end
          else
          begin
            // Is not a valid entry????
            // Perhaps an orphan blockchain: Me or Client!
            TLog.NewLog(ltInfo, Classname, 'Distinct operation block found! My:' +
              TBlock.BlockToString(TNode.Node.BlockManager.AccountStorage.Block(TNode.Node.BlockManager.BlocksCount - 1)
              .BlockHeader) + ' remote:' + TBlock.BlockToString(op.BlockHeader) + ' Errors: ' + errors);
          end;
        end
        else
        begin
          // Receiving an unexpected operationblock
          TLog.NewLog(ltError, Classname, 'Received a distinct block, finalizing: ' +
            TBlock.BlockToString(op.BlockHeader) + ' (My block: ' +
            TBlock.BlockToString(TNode.Node.BlockManager.LastBlock) + ')');
          FIsDownloadingBlocks := false;
          exit;
        end;
      end;
      FIsDownloadingBlocks := false;
      if ((opcount > 0) and (FRemoteOperationBlock.Block >= TNode.Node.BlockManager.BlocksCount)) then
      begin
        Send_GetBlocks(TNode.Node.BlockManager.BlocksCount, 100, i);
      end;
      TNode.Node.NotifyBlocksChanged;
    finally
      op.Free;
    end;
  finally
    if DoDisconnect then
    begin
      DisconnectInvalidClient(false, errors + ' > ' + TConnectionManager.HeaderDataToText(HeaderData) + ' BuffSize: ' +
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
    if HeaderData.header_type <> ntp_request then
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
        SendError(ntp_response, HeaderData.operation, HeaderData.request_id, CT_NetError_InternalServerError, errors);
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
        ob := TNode.Node.BlockManager.AccountStorage.Block(b).BlockHeader;
        if TBlock.SaveOperationBlockToStream(ob, msops) then
        begin
          blocksstr := blocksstr + Inttostr(b) + ',';
          b := b + inc_b;
          Inc(total_b);
        end
        else
        begin
          errors := 'ERROR DEV 20170522-1 block:' + Inttostr(b);
          SendError(ntp_response, HeaderData.operation, HeaderData.request_id, CT_NetError_InternalServerError, errors);
          exit;
        end;
      until (b > b_end);
      db := TMemoryStream.Create;
      try
        db.Write(total_b, 4);
        db.WriteBuffer(msops.Memory^, msops.Size);
        Send(ntp_response, HeaderData.operation, 0, HeaderData.request_id, db);
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
      DisconnectInvalidClient(false, errors + ' > ' + TConnectionManager.HeaderDataToText(HeaderData) + ' BuffSize: ' +
        Inttostr(DataBuffer.Size));
    end;
  end;
end;

procedure TNetConnection.DoProcess_GetSafeBox_Request(HeaderData: TNetHeaderData; DataBuffer: TStream);
var
  _blockcount: Cardinal;
  _safeboxHash: TRawBytes;
  _from, _to: Cardinal;
  sbStream: TStream;
  responseStream: TStream;
  antPos: Int64;
  sbHeader: TAccountStorageHeader;
  errors: AnsiString;
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
  DataBuffer.Read(_blockcount, SizeOf(_blockcount));
  TStreamOp.ReadAnsiString(DataBuffer, _safeboxHash);
  DataBuffer.Read(_from, SizeOf(_from));
  DataBuffer.Read(_to, SizeOf(_to));
  //
  sbStream := TNode.Node.BlockManager.Storage.CreateSafeBoxStream(_blockcount);
  try
    responseStream := TMemoryStream.Create;
    try
      if not Assigned(sbStream) then
      begin
        SendError(ntp_response, HeaderData.operation, CT_NetError_SafeboxNotFound, HeaderData.request_id,
          Format('Safebox for block %d not found', [_blockcount]));
        exit;
      end;
      antPos := sbStream.Position;
      TAccountStorage.LoadHeaderFromStream(sbStream, sbHeader);
      if sbHeader.AccountStorageHash <> _safeboxHash then
      begin
        DisconnectInvalidClient(false, Format('Invalid safeboxhash on GetSafeBox request (Real:%s > Requested:%s)',
          [TCrypto.ToHexaString(sbHeader.AccountStorageHash), TCrypto.ToHexaString(_safeboxHash)]));
        exit;
      end;
      // Response:
      sbStream.Position := antPos;
      if not TPCChunk.SaveSafeBoxChunkFromSafeBox(sbStream, responseStream, _from, _to, errors) then
      begin
        TLog.NewLog(ltError, Classname, 'Error saving chunk: ' + errors);
        exit;
      end;
      // Sending
      Send(ntp_response, HeaderData.operation, 0, HeaderData.request_id, responseStream);
    finally
      responseStream.Free;
    end;
  finally
    FreeAndNil(sbStream);
  end;
end;

procedure TNetConnection.DoProcess_Hello(HeaderData: TNetHeaderData; DataBuffer: TStream);
var
  op, myLastOp: TBlock;
  errors: AnsiString;
  connection_has_a_server: Word;
  i, c: Integer;
  nsa: TNodeServer;
  rid: Cardinal;
  connection_ts: Cardinal;
  Duplicate: TNetConnectionBase;
  RawAccountKey: TRawBytes;
  other_version: AnsiString;
begin
  FRemoteAccumulatedWork := 0;
  op := TBlock.Create(nil);
  try
    DataBuffer.Position := 0;
    if DataBuffer.Read(connection_has_a_server, 2) < 2 then
    begin
      DisconnectInvalidClient(false, 'Invalid data on buffer: ' + TConnectionManager.HeaderDataToText(HeaderData));
      exit;
    end;
    if TStreamOp.ReadAnsiString(DataBuffer, RawAccountKey) < 0 then
    begin
      DisconnectInvalidClient(false, 'Invalid data on buffer. No Public key: ' + TConnectionManager.HeaderDataToText
        (HeaderData));
      exit;
    end;
    FClientPublicKey := TAccountKey.FromRawString(RawAccountKey);
    if not FClientPublicKey.IsValidAccountKey(errors) then
    begin
      DisconnectInvalidClient(false, 'Invalid Public key: ' + TConnectionManager.HeaderDataToText(HeaderData) +
        ' errors: ' + errors);
      exit;
    end;
    if DataBuffer.Read(connection_ts, 4) < 4 then
    begin
      DisconnectInvalidClient(false, 'Invalid data on buffer. No TS: ' + TConnectionManager.HeaderDataToText
        (HeaderData));
      exit;
    end;
    FTimestampDiff := Integer(Int64(connection_ts) -
      Int64(TConnectionManager.Instance.NetworkAdjustedTime.GetAdjustedTime));
    if ClientTimestampIp = '' then
    begin
      ClientTimestampIp := RemoteHost;
      TConnectionManager.Instance.NetworkAdjustedTime.AddNewIp(ClientTimestampIp, connection_ts);
      if (Abs(TConnectionManager.Instance.NetworkAdjustedTime.TimeOffset) > CT_MaxFutureBlockTimestampOffset) then
      begin
        TNode.Node.NotifyNetClientMessage(nil, 'The detected network time is different from this system time in ' +
          Inttostr(TConnectionManager.Instance.NetworkAdjustedTime.TimeOffset) +
          ' seconds! Please check your local time/timezone');
      end;
      //
      if (Abs(FTimestampDiff) > CT_MaxFutureBlockTimestampOffset) then
      begin
        TLog.NewLog(ltError, Classname, 'Detected a node (' + ClientRemoteAddr + ') with incorrect timestamp: ' +
          Inttostr(connection_ts) + ' offset ' + Inttostr(FTimestampDiff));
      end;
    end;
    if (connection_has_a_server > 0) and (not SameText(Client.RemoteHost, 'localhost')) and
      (not SameText(Client.RemoteHost, '127.0.0.1')) and (not SameText('192.168.', Copy(Client.RemoteHost, 1, 8))) and
      (not SameText('10.', Copy(Client.RemoteHost, 1, 3))) and
      (not TAccountKey.EqualAccountKeys(FClientPublicKey, TConnectionManager.Instance.NodePrivateKey.PublicKey)) then
    begin
      nsa := CT_TNodeServerAddress_NUL;
      nsa.ip := Client.RemoteHost;
      nsa.port := connection_has_a_server;
      nsa.last_connection := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
      TConnectionManager.Instance.AddServer(nsa);
    end;

    if op.LoadBlockFromStream(DataBuffer, errors) then
    begin
      FRemoteOperationBlock := op.BlockHeader;
      if (DataBuffer.Size - DataBuffer.Position >= 4) then
      begin
        DataBuffer.Read(c, 4);
        for i := 1 to c do
        begin
          nsa := CT_TNodeServerAddress_NUL;
          TStreamOp.ReadAnsiString(DataBuffer, nsa.ip);
          DataBuffer.Read(nsa.port, 2);
          DataBuffer.Read(nsa.last_connection_by_server, 4);
          if (nsa.last_connection_by_server > 0) and (i <= CT_MAX_NODESERVERS_ON_HELLO) then // Protect massive data
            TConnectionManager.Instance.AddServer(nsa);
        end;
        if TStreamOp.ReadAnsiString(DataBuffer, other_version) >= 0 then
        begin
          // Captures version
          ClientAppVersion := other_version;
          if (DataBuffer.Size - DataBuffer.Position >= SizeOf(FRemoteAccumulatedWork)) then
          begin
            DataBuffer.Read(FRemoteAccumulatedWork, SizeOf(FRemoteAccumulatedWork));
            TLog.NewLog(ltdebug, Classname, 'Received HELLO with height: ' + Inttostr(op.BlockHeader.Block) +
              ' Accumulated work ' + Inttostr(FRemoteAccumulatedWork));
          end;
        end;
        //
        if (FRemoteAccumulatedWork > TNode.Node.BlockManager.AccountStorage.WorkSum) or
          ((FRemoteAccumulatedWork = 0) and (TConnectionManager.Instance.MaxRemoteOperationBlock.Block <
          FRemoteOperationBlock.Block)) then
        begin
          TConnectionManager.Instance.MaxRemoteOperationBlock := FRemoteOperationBlock;
          if TPCThread.ThreadClassFound(TThreadGetNewBlockChainFromClient, nil) < 0 then
          begin
            TThreadGetNewBlockChainFromClient.Create(false).FreeOnTerminate := true;
          end;
        end;
      end;

      TLog.NewLog(ltdebug, Classname, 'Hello received: ' + TBlock.BlockToString(FRemoteOperationBlock));
      if (HeaderData.header_type in [ntp_request, ntp_response]) then
      begin
        // Response:
        if (HeaderData.header_type = ntp_request) then
        begin
          Send_Hello(ntp_response, HeaderData.request_id);
        end;
        if (TAccountKey.EqualAccountKeys(FClientPublicKey, TConnectionManager.Instance.NodePrivateKey.PublicKey)) then
        begin
          DisconnectInvalidClient(true, 'MySelf disconnecting...');
          exit;
        end;
        Duplicate := TConnectionManager.Instance.FindConnectionByClientRandomValue(Self);
        if (Duplicate <> nil) and (Duplicate.Connected) then
        begin
          DisconnectInvalidClient(true, 'Duplicate connection with ' + Duplicate.ClientRemoteAddr);
          exit;
        end;
        TConnectionManager.Instance.NotifyReceivedHelloMessage;
      end
      else
      begin
        DisconnectInvalidClient(false, 'Invalid header type > ' + TConnectionManager.HeaderDataToText(HeaderData));
      end;
    end
    else
    begin
      TLog.NewLog(ltError, Classname, 'Error decoding operations of HELLO: ' + errors);
      DisconnectInvalidClient(false, 'Error decoding operations of HELLO: ' + errors);
    end;
  finally
    op.Free;
  end;
end;

procedure TNetConnection.DoProcess_Message(HeaderData: TNetHeaderData; DataBuffer: TStream);
var
  errors: AnsiString;
  decrypted, messagecrypted: AnsiString;
  DoDisconnect: Boolean;
begin
  errors := '';
  DoDisconnect := true;
  try
    if HeaderData.header_type <> ntp_autosend then
    begin
      errors := 'Not autosend';
      exit;
    end;
    if TStreamOp.ReadAnsiString(DataBuffer, messagecrypted) < 0 then
    begin
      errors := 'Invalid message data';
      exit;
    end;
    if not ECIESDecrypt(TConnectionManager.Instance.NodePrivateKey.EC_OpenSSL_NID,
      TConnectionManager.Instance.NodePrivateKey.PrivateKey, false, messagecrypted, decrypted) then
    begin
      errors := 'Error on decrypting message';
      exit;
    end;

    DoDisconnect := false;
    if TCrypto.IsHumanReadable(decrypted) then
      TLog.NewLog(ltInfo, Classname, 'Received new message from ' + ClientRemoteAddr + ' Message (' +
        Inttostr(length(decrypted)) + ' bytes): ' + decrypted)
    else
      TLog.NewLog(ltInfo, Classname, 'Received new message from ' + ClientRemoteAddr + ' Message (' +
        Inttostr(length(decrypted)) + ' bytes) in hexadecimal: ' + TCrypto.ToHexaString(decrypted));
    try
      TNode.Node.NotifyNetClientMessage(Self, decrypted);
    except
      on E: Exception do
      begin
        TLog.NewLog(ltError, Classname, 'Error processing received message. ' + E.Classname + ' ' + E.Message);
      end;
    end;
  finally
    if DoDisconnect then
    begin
      DisconnectInvalidClient(false, errors + ' > ' + TConnectionManager.HeaderDataToText(HeaderData) + ' BuffSize: ' +
        Inttostr(DataBuffer.Size));
    end;
  end;
end;

procedure TNetConnection.DoProcess_NewBlock(HeaderData: TNetHeaderData; DataBuffer: TStream);
var
  bacc: TAccountStorageEntry;
  op: TBlock;
  errors: AnsiString;
  DoDisconnect: Boolean;
begin
  errors := '';
  DoDisconnect := true;
  try
    if HeaderData.header_type <> ntp_autosend then
    begin
      errors := 'Not autosend';
      exit;
    end;
    op := TBlock.Create(nil);
    try
      op.BlockManager := TNode.Node.BlockManager;
      if not op.LoadBlockFromStream(DataBuffer, errors) then
      begin
        errors := 'Error decoding new account: ' + errors;
        exit;
      end
      else
      begin
        DoDisconnect := false;
        if DataBuffer.Size - DataBuffer.Position >= SizeOf(FRemoteAccumulatedWork) then
        begin
          DataBuffer.Read(FRemoteAccumulatedWork, SizeOf(FRemoteAccumulatedWork));
          TLog.NewLog(ltdebug, Classname, 'Received NEW BLOCK with height: ' + Inttostr(op.BlockHeader.Block) +
            ' Accumulated work ' + Inttostr(FRemoteAccumulatedWork));
        end
        else
          FRemoteAccumulatedWork := 0;
        FRemoteOperationBlock := op.BlockHeader;
        //
        if FRemoteAccumulatedWork = 0 then
        begin
          // Old version. No data
          if (op.BlockHeader.Block > TNode.Node.BlockManager.BlocksCount) then
          begin
            TConnectionManager.Instance.GetNewBlockChainFromClient(Self, Format('BlocksCount:%d > my BlocksCount:%d',
              [op.BlockHeader.Block + 1, TNode.Node.BlockManager.BlocksCount]));
          end
          else if (op.BlockHeader.Block = TNode.Node.BlockManager.BlocksCount) then
          begin
            // New block candidate:
            if not TNode.Node.AddNewBlockChain(Self, op, bacc, errors) then
            begin
              // Received a new invalid block... perhaps I'm an orphan blockchain
              TConnectionManager.Instance.GetNewBlockChainFromClient(Self, 'Has a distinct block. ' + errors);
            end;
          end;
        end
        else
        begin
          if (FRemoteAccumulatedWork > TNode.Node.BlockManager.AccountStorage.WorkSum) then
          begin
            if (op.BlockHeader.Block = TNode.Node.BlockManager.BlocksCount) then
            begin
              // New block candidate:
              if not TNode.Node.AddNewBlockChain(Self, op, bacc, errors) then
              begin
                // Really is a new block? (Check it)
                if (op.BlockHeader.Block = TNode.Node.BlockManager.BlocksCount) then
                begin
                  // Received a new invalid block... perhaps I'm an orphan blockchain
                  TConnectionManager.Instance.GetNewBlockChainFromClient(Self,
                    'Higher Work with same block height. I''m a orphan blockchain candidate');
                end;
              end;
            end
            else
            begin
              // Received a new higher work
              TConnectionManager.Instance.GetNewBlockChainFromClient(Self,
                Format('Higher Work and distinct blocks count. Need to download BlocksCount:%d  my BlocksCount:%d',
                [op.BlockHeader.Block + 1, TNode.Node.BlockManager.BlocksCount]));
            end;
          end;
        end;
      end;
    finally
      op.Free;
    end;
  finally
    if DoDisconnect then
    begin
      DisconnectInvalidClient(false, errors + ' > ' + TConnectionManager.HeaderDataToText(HeaderData) + ' BuffSize: ' +
        Inttostr(DataBuffer.Size));
    end;
  end;
end;

function TNetConnection.Send_AddOperations(Operations: TTransactionHashTree): Boolean;
var
  data: TMemoryStream;
  c1, request_id: Cardinal;
  i, nOpsToSend: Integer;
  optype: Byte;
begin
  Result := false;
  if not Connected then
    exit;
  NetLock.Acquire;
  try
    nOpsToSend := 0;
    FBufferLock.Acquire;
    try
      if Assigned(Operations) then
      begin
        for i := 0 to Operations.OperationsCount - 1 do
        begin
          if FBufferReceivedOperationsHash.IndexOf(Operations.GetOperation(i).Sha256) < 0 then
          begin
            FBufferReceivedOperationsHash.Add(Operations.GetOperation(i).Sha256);
            if FBufferToSendOperations.IndexOf(Operations.GetOperation(i)) < 0 then
            begin
              FBufferToSendOperations.AddTransactionToHashTree(Operations.GetOperation(i));
            end;
          end;
        end;
        nOpsToSend := Operations.OperationsCount;
      end;
      if FBufferToSendOperations.OperationsCount > 0 then
      begin
        TLog.NewLog(ltdebug, Classname, Format('Sending %d Operations to %s (inProc:%d, Received:%d)',
          [FBufferToSendOperations.OperationsCount, ClientRemoteAddr, nOpsToSend,
          FBufferReceivedOperationsHash.Count]));
        data := TMemoryStream.Create;
        try
          request_id := TConnectionManager.Instance.NewRequestId;
          c1 := FBufferToSendOperations.OperationsCount;
          data.Write(c1, 4);
          for i := 0 to FBufferToSendOperations.OperationsCount - 1 do
          begin
            optype := FBufferToSendOperations.GetOperation(i).optype;
            data.Write(optype, 1);
            FBufferToSendOperations.GetOperation(i).SaveToNettransfer(data);
          end;
          Send(ntp_autosend, CT_NetOp_AddOperations, 0, request_id, data);
          FBufferToSendOperations.ClearHastThree;
        finally
          data.Free;
        end;
      end
      else
        TLog.NewLog(ltdebug, Classname, Format('Not sending any operations to %s (inProc:%d, Received:%d, Sent:%d)',
          [ClientRemoteAddr, nOpsToSend, FBufferReceivedOperationsHash.Count,
          FBufferToSendOperations.OperationsCount]));
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
    TConnectionManager.Instance.RegisterRequest(Self, CT_NetOp_GetBlocks, request_id);
    TLog.NewLog(ltdebug, Classname, Format('Send GET BLOCKS start:%d quantity:%d (from:%d to %d)',
      [StartAddress, quantity, StartAddress, quantity + StartAddress]));
    FIsDownloadingBlocks := quantity > 1;
    Send(ntp_request, CT_NetOp_GetBlocks, 0, request_id, data);
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
      TConnectionManager.Instance.RegisterRequest(Self, CT_NetOp_Hello, request_id);
    end;
    if TNode.Node.NetServer.Active then
      w := TNode.Node.NetServer.port
    else
      w := 0;
    // Save active server port (2 bytes). 0 = No active server port
    data.Write(w, 2);
    // Save My connection public key
    TStreamOp.WriteAnsiString(data, TConnectionManager.Instance.NodePrivateKey.PublicKey.ToRawString);
    // Save my Unix timestamp (4 bytes)
    currunixtimestamp := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
    data.Write(currunixtimestamp, 4);
    // Save last operations block
    TBlock.SaveOperationBlockToStream(TNode.Node.BlockManager.LastBlock, data);
    nsarr := TConnectionManager.Instance.GetValidNodeServers(true, CT_MAX_NODESERVERS_ON_HELLO);
    i := length(nsarr);
    data.Write(i, 4);
    for i := 0 to high(nsarr) do
    begin
      nsa := nsarr[i];
      TStreamOp.WriteAnsiString(data, nsa.ip);
      data.Write(nsa.port, 2);
      data.Write(nsa.last_connection, 4);
    end;
    // Send client version
    TStreamOp.WriteAnsiString(data, CT_ClientAppVersion{$IFDEF LINUX} + 'l'{$ELSE} + 'w'{$ENDIF}{$IFDEF FPC}{$IFDEF LCL} + 'L'{$ELSE} + 'F'{$ENDIF}{$ENDIF});
    // Build 1.5 send accumulated work
    data.Write(TNode.Node.BlockManager.AccountStorage.WorkSum, SizeOf(TNode.Node.BlockManager.AccountStorage.WorkSum));
    //
    // data.SaveToFile('./hello.bin');
    Send(NetTranferType, CT_NetOp_Hello, 0, request_id, data);
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
    TStreamOp.WriteAnsiString(data, cyp);
    Send(ntp_autosend, CT_NetOp_Message, 0, 0, data);
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
    if (TBlock.EqualsOperationBlock(FRemoteOperationBlock, NewBlock.BlockHeader)) then
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
      Send(ntp_autosend, CT_NetOp_NewBlock, 0, request_id, data);
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
      for i := 0 to Operations.OperationsCount - 1 do
      begin
        if FBufferReceivedOperationsHash.IndexOf(Operations.GetOperation(i).Sha256) < 0 then
        begin
          FBufferReceivedOperationsHash.Add(Operations.GetOperation(i).Sha256);
          if FBufferToSendOperations.IndexOf(Operations.GetOperation(i)) < 0 then
          begin
            FBufferToSendOperations.AddTransactionToHashTree(Operations.GetOperation(i));
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