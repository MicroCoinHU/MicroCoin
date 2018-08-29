unit MicroCoin.Net.Connection;

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

uses SysUtils, Classes, UTCPIP, MicroCoin.BlockChain.BlockHeader, UThread,
  MicroCoin.Account.AccountKey, MicroCoin.Common.Lists, MicroCoin.Net.Protocol,
  MicroCoin.Net.NodeServer,
  MicroCoin.Transaction.HashTree, MicroCoin.BlockChain.Block, ULog;

type
  TNetConnection = class(TComponent)
  private
    FIsConnecting: Boolean;
    FTcpIpClient: TNetTcpIpClient;
    FRemoteOperationBlock: TBlockHeader;
    FRemoteAccumulatedWork: UInt64;
    FLastDataReceivedTS: Cardinal;
    FLastDataSendedTS: Cardinal;
    FClientBufferRead: TStream;
    FNetLock: TPCCriticalSection;
    FIsWaitingForResponse: Boolean;
    FTimestampDiff: Integer;
    FIsMyselfServer: Boolean;
    FClientPublicKey: TAccountKey;
    FCreatedTime: TDateTime;
    FClientAppVersion: AnsiString;
    FDoFinalizeConnection: Boolean;
    FNetProtocolVersion: TNetProtocolVersion;
    FAlertedForNewProtocolAvailable: Boolean;
    FHasReceivedData: Boolean;
    FIsDownloadingBlocks: Boolean;
    FRandomWaitSecondsSendHello: Cardinal;
    FBufferLock: TPCCriticalSection;
    FBufferReceivedOperationsHash: TOrderedRawList;
    FBufferToSendOperations: TTransactionHashTree;
    FClientTimestampIp: AnsiString;
    function GetConnected: Boolean;
    procedure SetConnected(const Value: Boolean);
    procedure TcpClient_OnConnect(Sender: TObject);
    procedure TcpClient_OnDisconnect(Sender: TObject);
    procedure DoProcess_Hello(HeaderData: TNetHeaderData; DataBuffer: TStream);
    procedure DoProcess_Message(HeaderData: TNetHeaderData; DataBuffer: TStream);
    procedure DoProcess_GetBlocks_Request(HeaderData: TNetHeaderData; DataBuffer: TStream);
    procedure DoProcess_GetBlocks_Response(HeaderData: TNetHeaderData; DataBuffer: TStream);
    procedure DoProcess_GetOperationsBlock_Request(HeaderData: TNetHeaderData; DataBuffer: TStream);
    procedure DoProcess_NewBlock(HeaderData: TNetHeaderData; DataBuffer: TStream);
    procedure DoProcess_AddOperations(HeaderData: TNetHeaderData; DataBuffer: TStream);
    procedure DoProcess_GetSafeBox_Request(HeaderData: TNetHeaderData; DataBuffer: TStream);
    function ReadTcpClientBuffer(MaxWaitMiliseconds: Cardinal; var HeaderData: TNetHeaderData;
      BufferData: TStream): Boolean;
    function GetClient: TNetTcpIpClient;
  protected
    procedure Notification(AComponent: TComponent; operation: TOperation); override;
    procedure Send(NetTranferType: TNetTransferType; operation, errorcode: Word; request_id: Integer;
      DataBuffer: TStream);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function DoSendAndWaitForResponse(operation: Word; RequestId: Integer; SendDataBuffer, ReceiveDataBuffer: TStream;
      MaxWaitTime: Cardinal; var HeaderData: TNetHeaderData): Boolean;
    procedure DisconnectInvalidClient(ItsMyself: Boolean; const why: AnsiString);
    procedure DoProcessBuffer;
    procedure SendError(NetTranferType: TNetTransferType; operation, request_id: Integer; error_code: Integer;
      error_text: AnsiString);
    procedure SetClient(const Value: TNetTcpIpClient);
    function ConnectTo(ServerIP: string; ServerPort: Word): Boolean;
    property Connected: Boolean read GetConnected write SetConnected;
    property IsConnecting: Boolean read FIsConnecting;
    function Send_Hello(NetTranferType: TNetTransferType; request_id: Integer): Boolean;
    function Send_NewBlockFound(const NewBlock: TBlock): Boolean;
    function Send_GetBlocks(StartAddress, quantity: Cardinal; var request_id: Cardinal): Boolean;
    function Send_AddOperations(Operations: TTransactionHashTree): Boolean;
    function Send_Message(const TheMessage: AnsiString): Boolean;
    function AddOperationsToBufferForSend(Operations: TTransactionHashTree): Integer;
    property Client: TNetTcpIpClient read GetClient;
    function ClientRemoteAddr: AnsiString;
    property TimestampDiff: Integer read FTimestampDiff;
    property RemoteOperationBlock: TBlockHeader read FRemoteOperationBlock;
    //
    property NetProtocolVersion: TNetProtocolVersion read FNetProtocolVersion;
    //
    property IsMyselfServer: Boolean read FIsMyselfServer;
    property CreatedTime: TDateTime read FCreatedTime;
    property ClientAppVersion: AnsiString read FClientAppVersion write FClientAppVersion;
    procedure FinalizeConnection;
    property DoFinalizeConnection: Boolean read FDoFinalizeConnection write FDoFinalizeConnection;
    property NetLock: TPCCriticalSection read FNetLock write FNetLock;
    property RemoteAccumulatedWork: UInt64 read FRemoteAccumulatedWork write FRemoteAccumulatedWork;
    property IsDownloadingBlocks: Boolean read FIsDownloadingBlocks write FIsDownloadingBlocks;
    property ClientPublicKey: TAccountKey read FClientPublicKey write FClientPublicKey;
    property TcpIpClient: TNetTcpIpClient read FTcpIpClient;
    property HasReceivedData: Boolean read FHasReceivedData;
  end;

  PNetRequestRegistered = ^TNetRequestRegistered;

  TNetRequestRegistered = record
    NetClient: TNetConnection;
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

function TNetConnection.ClientRemoteAddr: AnsiString;
begin
  if Assigned(FTcpIpClient) then
  begin
    Result := FTcpIpClient.ClientRemoteAddr
  end
  else
    Result := 'NIL';
end;

function TNetConnection.ConnectTo(ServerIP: string; ServerPort: Word): Boolean;
var
  Pnsa: PNodeServerAddress;
  lns: TList;
  i: Integer;
begin
  if FIsConnecting then
    exit;
  try
    FIsConnecting := true;
    if Client.Connected then
      Client.Disconnect;
    lns := TConnectionManager.NetData.NodeServersAddresses.LockList;
    try
      i := TConnectionManager.NetData.IndexOfNetClient(lns, ServerIP, ServerPort);
      if (i >= 0) then
        Pnsa := lns[i]
      else
        Pnsa := nil;
      if Assigned(Pnsa) then
        Pnsa^.netConnection := Self;
    finally
      TConnectionManager.NetData.NodeServersAddresses.UnlockList;
    end;

    TPCThread.ProtectEnterCriticalSection(Self, FNetLock);
    try
      Client.RemoteHost := ServerIP;
      if ServerPort <= 0 then
        ServerPort := CT_NetServer_Port;
      Client.RemotePort := ServerPort;
      TLog.NewLog(ltdebug, Classname, 'Trying to connect to a server at: ' + ClientRemoteAddr);
      TConnectionManager.NetData.NotifyNetConnectionUpdated;
      Result := Client.Connect;
    finally
      FNetLock.Release;
    end;
    if Result then
    begin
      TLog.NewLog(ltdebug, Classname, 'Connected to a possible server at: ' + ClientRemoteAddr);
      Result := Send_Hello(ntp_request, TConnectionManager.NetData.NewRequestId);
    end
    else
    begin
      TLog.NewLog(ltdebug, Classname, 'Cannot connect to a server at: ' + ClientRemoteAddr);
    end;
  finally
    FIsConnecting := false;
  end;
end;

constructor TNetConnection.Create(AOwner: TComponent);
begin
  inherited;
  FIsConnecting := false;
  FIsDownloadingBlocks := false;
  FHasReceivedData := false;
  FNetProtocolVersion.protocol_version := 0; // 0 = unknown
  FNetProtocolVersion.protocol_available := 0;
  FAlertedForNewProtocolAvailable := false;
  FDoFinalizeConnection := false;
  FClientAppVersion := '';
  FClientPublicKey := CT_TECDSA_Public_Nul;
  FCreatedTime := now;
  FIsMyselfServer := false;
  FTimestampDiff := 0;
  FIsWaitingForResponse := false;
  FClientBufferRead := TMemoryStream.Create;
  FNetLock := TPCCriticalSection.Create('TNetConnection_NetLock');
  FLastDataReceivedTS := 0;
  FLastDataSendedTS := 0;
  FRandomWaitSecondsSendHello := 90 + Random(60);
  FTcpIpClient := nil;
  FRemoteOperationBlock := CT_OperationBlock_NUL;
  FRemoteAccumulatedWork := 0;
  SetClient(TBufferedNetTcpIpClient.Create(Self));
  TConnectionManager.NetData.NetConnections.Add(Self);
  TConnectionManager.NetData.NotifyNetConnectionUpdated;
  FBufferLock := TPCCriticalSection.Create('TNetConnection_BufferLock');
  FBufferReceivedOperationsHash := TOrderedRawList.Create;
  FBufferToSendOperations := TTransactionHashTree.Create;
  FClientTimestampIp := '';
end;

destructor TNetConnection.Destroy;
var
  Pnsa: PNodeServerAddress;
  lns: TList;
  i: Integer;
begin
  try
    TLog.NewLog(ltdebug, Classname, 'Destroying ' + Classname + ' ' + IntToHex(PtrInt(Self), 8));

    Connected := false;

    lns := TConnectionManager.NetData.NodeServersAddresses.LockList;
    try
      for i := lns.Count - 1 downto 0 do
      begin
        Pnsa := lns[i];
        if Pnsa^.netConnection = Self then
        begin
          Pnsa^.netConnection := nil;
        end;
      end;
    finally
      TConnectionManager.NetData.NodeServersAddresses.UnlockList;
    end;
  finally
    TConnectionManager.NetData.NetConnections.Remove(Self);
  end;
  TConnectionManager.NetData.UnRegisterRequest(Self, 0, 0);
  try
    TConnectionManager.NetData.NotifyNetConnectionUpdated;
  finally
    FreeAndNil(FNetLock);
    FreeAndNil(FClientBufferRead);
    FreeAndNil(FTcpIpClient);
    FreeAndNil(FBufferLock);
    FreeAndNil(FBufferReceivedOperationsHash);
    FreeAndNil(FBufferToSendOperations);
    inherited;
  end;
end;

procedure TNetConnection.DisconnectInvalidClient(ItsMyself: Boolean; const why: AnsiString);
var
  P: PNodeServerAddress;
  l: TList;
  i: Integer;
  include_in_list: Boolean;
begin
  FIsDownloadingBlocks := false;
  if ItsMyself then
  begin
    TLog.NewLog(ltInfo, Classname, 'Disconecting myself ' + ClientRemoteAddr + ' > ' + why)
  end
  else
  begin
    TLog.NewLog(ltError, Classname, 'Disconecting ' + ClientRemoteAddr + ' > ' + why);
  end;
  FIsMyselfServer := ItsMyself;
  include_in_list := (not SameText(Client.RemoteHost, 'localhost')) and (not SameText(Client.RemoteHost, '127.0.0.1'))
    and (not SameText('192.168.', Copy(Client.RemoteHost, 1, 8))) and
    (not SameText('10.', Copy(Client.RemoteHost, 1, 3)));
  if include_in_list then
  begin
    l := TConnectionManager.NetData.NodeServersAddresses.LockList;
    try
      i := TConnectionManager.NetData.IndexOfNetClient(l, Client.RemoteHost, Client.RemotePort);
      if i < 0 then
      begin
        New(P);
        P^ := CT_TNodeServerAddress_NUL;
        l.Add(P);
      end
      else
        P := l[i];
      P^.ip := Client.RemoteHost;
      P^.port := Client.RemotePort;
      P^.last_connection := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
      P^.its_myself := ItsMyself;
      P^.BlackListText := why;
      P^.is_blacklisted := true;
    finally
      TConnectionManager.NetData.NodeServersAddresses.UnlockList;
    end;
  end
  else if ItsMyself then
  begin
    l := TConnectionManager.NetData.NodeServersAddresses.LockList;
    try
      i := TConnectionManager.NetData.IndexOfNetClient(l, Client.RemoteHost, Client.RemotePort);
      if i >= 0 then
      begin
        P := l[i];
        P^.its_myself := ItsMyself;
      end;
    finally
      TConnectionManager.NetData.NodeServersAddresses.UnlockList;
    end;
  end;
  Connected := false;
  TConnectionManager.NetData.NotifyBlackListUpdated;
  TConnectionManager.NetData.NotifyNodeServersUpdated;
end;

procedure TNetConnection.DoProcessBuffer;
var
  HeaderData: TNetHeaderData;
  ms: TMemoryStream;
  ops: AnsiString;
begin
  if FDoFinalizeConnection then
  begin
    TLog.NewLog(ltdebug, Classname, 'Executing DoFinalizeConnection at client ' + ClientRemoteAddr);
    Connected := false;
  end;
  if not Connected then
    exit;
  ms := TMemoryStream.Create;
  try
    if not FIsWaitingForResponse then
    begin
      DoSendAndWaitForResponse(0, 0, nil, ms, 0, HeaderData);
    end;
  finally
    ms.Free;
  end;
  if ((FLastDataReceivedTS > 0) or (not(Self is TNetServerClient))) and
    ((FLastDataReceivedTS + (1000 * FRandomWaitSecondsSendHello) < GetTickCount) and
    (FLastDataSendedTS + (1000 * FRandomWaitSecondsSendHello) < GetTickCount)) then
  begin
    // Build 1.4 -> Changing wait time from 120 secs to a random seconds value
    if TConnectionManager.NetData.PendingRequest(Self, ops) >= 2 then
    begin
      TLog.NewLog(ltdebug, Classname, 'Pending requests without response... closing connection to ' + ClientRemoteAddr +
        ' > ' + ops);
      Connected := false;
    end
    else
    begin
      TLog.NewLog(ltdebug, Classname, 'Sending Hello to check connection to ' + ClientRemoteAddr + ' > ' + ops);
      Send_Hello(ntp_request, TConnectionManager.NetData.NewRequestId);
    end;
  end
  else if (Self is TNetServerClient) and (FLastDataReceivedTS = 0) and (FCreatedTime + EncodeTime(0, 1, 0, 0) < now)
  then
  begin
    // Disconnecting client without data...
    TLog.NewLog(ltdebug, Classname, 'Disconnecting client without data ' + ClientRemoteAddr);
    Connected := false;
  end;
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
    if (b_end >= TConnectionManager.NetData.Bank.BlocksCount) then
      b_end := TConnectionManager.NetData.Bank.BlocksCount - 1;

    DoDisconnect := false;

    db := TMemoryStream.Create;
    try
      op := TBlock.Create(TConnectionManager.NetData.Bank);
      try
        c := b_end - b_start + 1;
        posquantity := db.Position;
        db.Write(c, 4);
        c := 0;
        b := b_start;
        for b := b_start to b_end do
        begin
          Inc(c);
          if TConnectionManager.NetData.Bank.LoadOperations(op, b) then
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
      op.Bank := TNode.Node.Bank;
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
        if (op.OperationBlock.Block = TNode.Node.Bank.BlocksCount) then
        begin
          if (TNode.Node.Bank.AddNewBlockChainBlock(op,
            TConnectionManager.NetData.NetworkAdjustedTime.GetMaxAllowedTimestampForNewBlock, newBlockAccount, errors))
          then
          begin
            // Ok, one more!
          end
          else
          begin
            // Is not a valid entry????
            // Perhaps an orphan blockchain: Me or Client!
            TLog.NewLog(ltInfo, Classname, 'Distinct operation block found! My:' +
              TBlock.OperationBlockToText(TNode.Node.Bank.AccountStorage.Block(TNode.Node.Bank.BlocksCount - 1)
              .BlockHeader) + ' remote:' + TBlock.OperationBlockToText(op.OperationBlock) + ' Errors: ' + errors);
          end;
        end
        else
        begin
          // Receiving an unexpected operationblock
          TLog.NewLog(ltError, Classname, 'Received a distinct block, finalizing: ' +
            TBlock.OperationBlockToText(op.OperationBlock) + ' (My block: ' +
            TBlock.OperationBlockToText(TNode.Node.Bank.LastOperationBlock) + ')');
          FIsDownloadingBlocks := false;
          exit;
        end;
      end;
      FIsDownloadingBlocks := false;
      if ((opcount > 0) and (FRemoteOperationBlock.Block >= TNode.Node.Bank.BlocksCount)) then
      begin
        Send_GetBlocks(TNode.Node.Bank.BlocksCount, 100, i);
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
    if (b_start < 0) or (b_start > b_end) or (b_start >= TNode.Node.Bank.BlocksCount) then
    begin
      errors := 'Invalid start (' + Inttostr(b_start) + ') or end (' + Inttostr(b_end) + ') of count (' +
        Inttostr(TNode.Node.Bank.BlocksCount) + ')';
      exit;
    end;

    DoDisconnect := false;

    // Build 1.4
    if b_start < TNode.Node.Bank.Storage.FirstBlock then
    begin
      b_start := TNode.Node.Bank.Storage.FirstBlock;
      if b_end < b_start then
      begin
        errors := 'Block:' + Inttostr(b_end) + ' not found';
        SendError(ntp_response, HeaderData.operation, HeaderData.request_id, CT_NetError_InternalServerError, errors);
        exit;
      end;
    end;

    if (b_end >= TNode.Node.Bank.BlocksCount) then
      b_end := TNode.Node.Bank.BlocksCount - 1;
    inc_b := ((b_end - b_start) div CT_Max_Positions) + 1;
    msops := TMemoryStream.Create;
    try
      b := b_start;
      total_b := 0;
      repeat
        ob := TNode.Node.Bank.AccountStorage.Block(b).BlockHeader;
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
  sbStream := TNode.Node.Bank.Storage.CreateSafeBoxStream(_blockcount);
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
  Duplicate: TNetConnection;
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
      Int64(TConnectionManager.NetData.NetworkAdjustedTime.GetAdjustedTime));
    if FClientTimestampIp = '' then
    begin
      FClientTimestampIp := FTcpIpClient.RemoteHost;
      TConnectionManager.NetData.NetworkAdjustedTime.AddNewIp(FClientTimestampIp, connection_ts);
      if (Abs(TConnectionManager.NetData.NetworkAdjustedTime.TimeOffset) > CT_MaxFutureBlockTimestampOffset) then
      begin
        TNode.Node.NotifyNetClientMessage(nil, 'The detected network time is different from this system time in ' +
          Inttostr(TConnectionManager.NetData.NetworkAdjustedTime.TimeOffset) +
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
      (not TAccountKey.EqualAccountKeys(FClientPublicKey, TConnectionManager.NetData.NodePrivateKey.PublicKey)) then
    begin
      nsa := CT_TNodeServerAddress_NUL;
      nsa.ip := Client.RemoteHost;
      nsa.port := connection_has_a_server;
      nsa.last_connection := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
      TConnectionManager.NetData.AddServer(nsa);
    end;

    if op.LoadBlockFromStream(DataBuffer, errors) then
    begin
      FRemoteOperationBlock := op.OperationBlock;
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
            TConnectionManager.NetData.AddServer(nsa);
        end;
        if TStreamOp.ReadAnsiString(DataBuffer, other_version) >= 0 then
        begin
          // Captures version
          ClientAppVersion := other_version;
          if (DataBuffer.Size - DataBuffer.Position >= SizeOf(FRemoteAccumulatedWork)) then
          begin
            DataBuffer.Read(FRemoteAccumulatedWork, SizeOf(FRemoteAccumulatedWork));
            TLog.NewLog(ltdebug, Classname, 'Received HELLO with height: ' + Inttostr(op.OperationBlock.Block) +
              ' Accumulated work ' + Inttostr(FRemoteAccumulatedWork));
          end;
        end;
        //
        if (FRemoteAccumulatedWork > TNode.Node.Bank.AccountStorage.WorkSum) or
          ((FRemoteAccumulatedWork = 0) and (TConnectionManager.NetData.MaxRemoteOperationBlock.Block <
          FRemoteOperationBlock.Block)) then
        begin
          TConnectionManager.NetData.MaxRemoteOperationBlock := FRemoteOperationBlock;
          if TPCThread.ThreadClassFound(TThreadGetNewBlockChainFromClient, nil) < 0 then
          begin
            TThreadGetNewBlockChainFromClient.Create(false).FreeOnTerminate := true;
          end;
        end;
      end;

      TLog.NewLog(ltdebug, Classname, 'Hello received: ' + TBlock.OperationBlockToText(FRemoteOperationBlock));
      if (HeaderData.header_type in [ntp_request, ntp_response]) then
      begin
        // Response:
        if (HeaderData.header_type = ntp_request) then
        begin
          Send_Hello(ntp_response, HeaderData.request_id);
        end;
        if (TAccountKey.EqualAccountKeys(FClientPublicKey, TConnectionManager.NetData.NodePrivateKey.PublicKey)) then
        begin
          DisconnectInvalidClient(true, 'MySelf disconnecting...');
          exit;
        end;
        Duplicate := TConnectionManager.NetData.FindConnectionByClientRandomValue(Self);
        if (Duplicate <> nil) and (Duplicate.Connected) then
        begin
          DisconnectInvalidClient(true, 'Duplicate connection with ' + Duplicate.ClientRemoteAddr);
          exit;
        end;
        TConnectionManager.NetData.NotifyReceivedHelloMessage;
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
    if not ECIESDecrypt(TConnectionManager.NetData.NodePrivateKey.EC_OpenSSL_NID,
      TConnectionManager.NetData.NodePrivateKey.PrivateKey, false, messagecrypted, decrypted) then
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
      op.Bank := TNode.Node.Bank;
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
          TLog.NewLog(ltdebug, Classname, 'Received NEW BLOCK with height: ' + Inttostr(op.OperationBlock.Block) +
            ' Accumulated work ' + Inttostr(FRemoteAccumulatedWork));
        end
        else
          FRemoteAccumulatedWork := 0;
        FRemoteOperationBlock := op.OperationBlock;
        //
        if FRemoteAccumulatedWork = 0 then
        begin
          // Old version. No data
          if (op.OperationBlock.Block > TNode.Node.Bank.BlocksCount) then
          begin
            TConnectionManager.NetData.GetNewBlockChainFromClient(Self, Format('BlocksCount:%d > my BlocksCount:%d',
              [op.OperationBlock.Block + 1, TNode.Node.Bank.BlocksCount]));
          end
          else if (op.OperationBlock.Block = TNode.Node.Bank.BlocksCount) then
          begin
            // New block candidate:
            if not TNode.Node.AddNewBlockChain(Self, op, bacc, errors) then
            begin
              // Received a new invalid block... perhaps I'm an orphan blockchain
              TConnectionManager.NetData.GetNewBlockChainFromClient(Self, 'Has a distinct block. ' + errors);
            end;
          end;
        end
        else
        begin
          if (FRemoteAccumulatedWork > TNode.Node.Bank.AccountStorage.WorkSum) then
          begin
            if (op.OperationBlock.Block = TNode.Node.Bank.BlocksCount) then
            begin
              // New block candidate:
              if not TNode.Node.AddNewBlockChain(Self, op, bacc, errors) then
              begin
                // Really is a new block? (Check it)
                if (op.OperationBlock.Block = TNode.Node.Bank.BlocksCount) then
                begin
                  // Received a new invalid block... perhaps I'm an orphan blockchain
                  TConnectionManager.NetData.GetNewBlockChainFromClient(Self,
                    'Higher Work with same block height. I''m a orphan blockchain candidate');
                end;
              end;
            end
            else
            begin
              // Received a new higher work
              TConnectionManager.NetData.GetNewBlockChainFromClient(Self,
                Format('Higher Work and distinct blocks count. Need to download BlocksCount:%d  my BlocksCount:%d',
                [op.OperationBlock.Block + 1, TNode.Node.Bank.BlocksCount]));
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

function TNetConnection.DoSendAndWaitForResponse(operation: Word; RequestId: Integer;
  SendDataBuffer, ReceiveDataBuffer: TStream; MaxWaitTime: Cardinal; var HeaderData: TNetHeaderData): Boolean;
var
  tc: Cardinal;
  was_waiting_for_response: Boolean;
  l: TList;
  i: Integer;
  iDebugStep: Integer;
begin
  iDebugStep := 0;
  try
    Result := false;
    HeaderData := CT_NetHeaderData;
    if FIsWaitingForResponse then
    begin
      TLog.NewLog(ltdebug, Classname, 'Is waiting for response ...');
      exit;
    end;
    iDebugStep := 100;
    if not Assigned(FTcpIpClient) then
      exit;
    if not Client.Connected then
      exit;
    iDebugStep := 110;
    tc := GetTickCount;
    if TPCThread.TryProtectEnterCriticalSection(Self, MaxWaitTime, FNetLock) then
    begin
      try
        iDebugStep := 120;
        was_waiting_for_response := RequestId > 0;
        try
          if was_waiting_for_response then
          begin
            iDebugStep := 200;
            FIsWaitingForResponse := true;
            Send(ntp_request, operation, 0, RequestId, SendDataBuffer);
          end;
          iDebugStep := 300;
          repeat
            iDebugStep := 400;
            if (MaxWaitTime > GetTickCount - tc) then
              MaxWaitTime := MaxWaitTime - (GetTickCount - tc)
            else
              MaxWaitTime := 1;
            tc := GetTickCount;
            if (ReadTcpClientBuffer(MaxWaitTime, HeaderData, ReceiveDataBuffer)) then
            begin
              iDebugStep := 500;
              l := TConnectionManager.NetData.NodeServersAddresses.LockList;
              try
                iDebugStep := 600;
                for i := 0 to l.Count - 1 do
                begin
                  if PNodeServerAddress(l[i])^.netConnection = Self then
                  begin
                    PNodeServerAddress(l[i])^.last_connection := (UnivDateTimeToUnix(DateTime2UnivDateTime(now)));
                    PNodeServerAddress(l[i])^.total_failed_attemps_to_connect := 0;
                  end;
                end;
              finally
                iDebugStep := 700;
                TConnectionManager.NetData.NodeServersAddresses.UnlockList;
              end;
              iDebugStep := 800;
              TLog.NewLog(ltdebug, Classname, 'Received ' + CT_NetTransferType[HeaderData.header_type] + ' operation:' +
                TConnectionManager.OperationToText(HeaderData.operation) + ' id:' + Inttostr(HeaderData.request_id) +
                ' Buffer size:' + Inttostr(HeaderData.buffer_data_length));
              if (RequestId = HeaderData.request_id) and (HeaderData.header_type = ntp_response) then
              begin
                Result := true;
              end
              else
              begin
                iDebugStep := 1000;
                case HeaderData.operation of
                  CT_NetOp_Hello:
                    begin
                      DoProcess_Hello(HeaderData, ReceiveDataBuffer);
                    end;
                  CT_NetOp_Message:
                    begin
                      DoProcess_Message(HeaderData, ReceiveDataBuffer);
                    end;
                  CT_NetOp_GetBlocks:
                    begin
                      if HeaderData.header_type = ntp_request then
                        DoProcess_GetBlocks_Request(HeaderData, ReceiveDataBuffer)
                      else if HeaderData.header_type = ntp_response then
                        DoProcess_GetBlocks_Response(HeaderData, ReceiveDataBuffer)
                      else
                        DisconnectInvalidClient(false, 'Not resquest or response: ' +
                          TConnectionManager.HeaderDataToText(HeaderData));
                    end;
                  CT_NetOp_GetOperationsBlock:
                    begin
                      if HeaderData.header_type = ntp_request then
                        DoProcess_GetOperationsBlock_Request(HeaderData, ReceiveDataBuffer)
                      else
                        TLog.NewLog(ltdebug, Classname, 'Received old response of: ' +
                          TConnectionManager.HeaderDataToText(HeaderData));
                    end;
                  CT_NetOp_NewBlock:
                    begin
                      DoProcess_NewBlock(HeaderData, ReceiveDataBuffer);
                    end;
                  CT_NetOp_AddOperations:
                    begin
                      DoProcess_AddOperations(HeaderData, ReceiveDataBuffer);
                    end;
                  CT_NetOp_GetSafeBox:
                    begin
                      if HeaderData.header_type = ntp_request then
                        DoProcess_GetSafeBox_Request(HeaderData, ReceiveDataBuffer)
                      else
                        DisconnectInvalidClient(false, 'Received ' + TConnectionManager.HeaderDataToText(HeaderData));
                    end
                else
                  DisconnectInvalidClient(false, 'Invalid operation: ' + TConnectionManager.HeaderDataToText
                    (HeaderData));
                end;
              end;
            end
            else
              sleep(1);
            iDebugStep := 900;
          until (Result) or (GetTickCount > (MaxWaitTime + tc));
        finally
          if was_waiting_for_response then
            FIsWaitingForResponse := false;
        end;
        iDebugStep := 990;
      finally
        FNetLock.Release;
      end;
    end;
  except
    on E: Exception do
    begin
      E.Message := E.Message + ' DoSendAndWaitForResponse step ' + Inttostr(iDebugStep);
      raise;
    end;
  end;
end;

procedure TNetConnection.FinalizeConnection;
begin
  if FDoFinalizeConnection then
    exit;
  TLog.NewLog(ltdebug, Classname, 'Executing FinalizeConnection to ' + ClientRemoteAddr);
  FDoFinalizeConnection := true;
end;

function TNetConnection.GetClient: TNetTcpIpClient;
begin
  if not Assigned(FTcpIpClient) then
  begin
    TLog.NewLog(ltError, Classname, 'TcpIpClient=NIL');
    raise Exception.Create('TcpIpClient=NIL');
  end;
  Result := FTcpIpClient;
end;

function TNetConnection.GetConnected: Boolean;
begin
  Result := Assigned(FTcpIpClient) and (FTcpIpClient.Connected);
end;

procedure TNetConnection.Notification(AComponent: TComponent; operation: TOperation);
begin
  inherited;
  if (operation = OpRemove) and (AComponent = FTcpIpClient) then
  begin
    FTcpIpClient := nil;
  end;
end;

function TNetConnection.ReadTcpClientBuffer(MaxWaitMiliseconds: Cardinal; var HeaderData: TNetHeaderData;
  BufferData: TStream): Boolean;
var
  auxstream: TMemoryStream;
  tc: Cardinal;
  last_bytes_read, t_bytes_read: Int64;
  //
  operation: Word;
  request_id: Integer;
  IsValidHeaderButNeedMoreData: Boolean;
  deletedBytes: Int64;

begin
  t_bytes_read := 0;
  Result := false;
  HeaderData := CT_NetHeaderData;
  BufferData.Size := 0;
  TPCThread.ProtectEnterCriticalSection(Self, FNetLock);
  try
    tc := GetTickCount;
    repeat
      if not Connected then
        exit;
      if not Client.Connected then
        exit;
      last_bytes_read := 0;
      FClientBufferRead.Position := 0;
      Result := TConnectionManager.ExtractHeaderInfo(FClientBufferRead, HeaderData, BufferData,
        IsValidHeaderButNeedMoreData);
      if Result then
      begin
        FNetProtocolVersion := HeaderData.Protocol;
        // Build 1.0.4 accepts net protocol 1 and 2
        if HeaderData.Protocol.protocol_version > CT_NetProtocol_Available then
        begin
          TNode.Node.NotifyNetClientMessage(nil, 'Detected a higher Net protocol version at ' + ClientRemoteAddr +
            ' (v ' + Inttostr(HeaderData.Protocol.protocol_version) + ' ' +
            Inttostr(HeaderData.Protocol.protocol_available) + ') ' +
            '... check that your version is Ok! Visit official download website for possible updates: https://sourceforge.net/projects/microcoin/');
          DisconnectInvalidClient(false, Format('Invalid Net protocol version found: %d available: %d',
            [HeaderData.Protocol.protocol_version, HeaderData.Protocol.protocol_available]));
          Result := false;
          exit;
        end
        else
        begin
          if (FNetProtocolVersion.protocol_available > CT_NetProtocol_Available) and
            (not FAlertedForNewProtocolAvailable) then
          begin
            FAlertedForNewProtocolAvailable := true;
            TNode.Node.NotifyNetClientMessage(nil, 'Detected a new Net protocol version at ' + ClientRemoteAddr + ' (v '
              + Inttostr(HeaderData.Protocol.protocol_version) + ' ' + Inttostr(HeaderData.Protocol.protocol_available)
              + ') ' + '... Visit official download website for possible updates: https://sourceforge.net/projects/microcoin/');
          end;
          // Remove data from buffer and save only data not processed (higher than stream.position)
          auxstream := TMemoryStream.Create;
          try
            if FClientBufferRead.Position < FClientBufferRead.Size then
            begin
              auxstream.CopyFrom(FClientBufferRead, FClientBufferRead.Size - FClientBufferRead.Position);
            end;
            FClientBufferRead.Size := 0;
            FClientBufferRead.CopyFrom(auxstream, 0);
          finally
            auxstream.Free;
          end;
        end;
      end
      else
      begin
        sleep(1);
        if not Client.WaitForData(100) then
        begin
          exit;
        end;

        auxstream := (Client as TBufferedNetTcpIpClient).ReadBufferLock;
        try
          last_bytes_read := auxstream.Size;
          if last_bytes_read > 0 then
          begin
            FLastDataReceivedTS := GetTickCount;
            FRandomWaitSecondsSendHello := 90 + Random(60);

            FClientBufferRead.Position := FClientBufferRead.Size; // Go to the end
            auxstream.Position := 0;
            FClientBufferRead.CopyFrom(auxstream, last_bytes_read);
            FClientBufferRead.Position := 0;
            auxstream.Size := 0;
            Inc(t_bytes_read, last_bytes_read);
          end;
        finally
          (Client as TBufferedNetTcpIpClient).ReadBufferUnlock;
        end;
      end;
    until (Result) or ((GetTickCount > (tc + MaxWaitMiliseconds)) and (last_bytes_read = 0));
  finally
    try
      if (Connected) then
      begin
        if (not Result) and (FClientBufferRead.Size > 0) and (not IsValidHeaderButNeedMoreData) then
        begin
          deletedBytes := FClientBufferRead.Size;
          TLog.NewLog(ltError, Classname,
            Format('Deleting %d bytes from TcpClient buffer of %s after max %d miliseconds. Elapsed: %d',
            [deletedBytes, Client.ClientRemoteAddr, MaxWaitMiliseconds, GetTickCount - tc]));
          FClientBufferRead.Size := 0;
          DisconnectInvalidClient(false, 'Invalid data received in buffer (' + Inttostr(deletedBytes) + ' bytes)');
        end
        else if (IsValidHeaderButNeedMoreData) then
        begin
          TLog.NewLog(ltdebug, Classname,
            Format('Not enough data received - Received %d bytes from TcpClient buffer of %s after max %d miliseconds. Elapsed: %d - HeaderData: %s',
            [FClientBufferRead.Size, Client.ClientRemoteAddr, MaxWaitMiliseconds, GetTickCount - tc,
            TConnectionManager.HeaderDataToText(HeaderData)]));
        end;
      end;
    finally
      FNetLock.Release;
    end;
  end;
  if t_bytes_read > 0 then
  begin
    if not FHasReceivedData then
    begin
      FHasReceivedData := true;
      if (Self is TNetClient) then
        TConnectionManager.NetData.IncStatistics(0, 0, 0, 1, t_bytes_read, 0)
      else
        TConnectionManager.NetData.IncStatistics(0, 0, 0, 0, t_bytes_read, 0);
    end
    else
    begin
      TConnectionManager.NetData.IncStatistics(0, 0, 0, 0, t_bytes_read, 0);
    end;
  end;
  if (Result) and (HeaderData.header_type = ntp_response) then
  begin
    TConnectionManager.NetData.UnRegisterRequest(Self, HeaderData.operation, HeaderData.request_id);
  end;
end;

procedure TNetConnection.Send(NetTranferType: TNetTransferType; operation, errorcode: Word; request_id: Integer;
  DataBuffer: TStream);
var
  l: Cardinal;
  w: Word;
  buffer: TMemoryStream;
  s: AnsiString;
begin
  buffer := TMemoryStream.Create;
  try
    l := CT_MagicNetIdentification;
    buffer.Write(l, 4);
    case NetTranferType of
      ntp_request:
        begin
          w := CT_MagicRequest;
          buffer.Write(w, 2);
          buffer.Write(operation, 2);
          w := 0;
          buffer.Write(w, 2);
          buffer.Write(request_id, 4);
        end;
      ntp_response:
        begin
          w := CT_MagicResponse;
          buffer.Write(w, 2);
          buffer.Write(operation, 2);
          buffer.Write(errorcode, 2);
          buffer.Write(request_id, 4);
        end;
      ntp_autosend:
        begin
          w := CT_MagicAutoSend;
          buffer.Write(w, 2);
          buffer.Write(operation, 2);
          w := errorcode;
          buffer.Write(w, 2);
          l := 0;
          buffer.Write(l, 4);
        end
    else
      raise Exception.Create('Invalid encoding');
    end;
    l := CT_NetProtocol_Version;
    buffer.Write(l, 2);
    l := CT_NetProtocol_Available;
    buffer.Write(l, 2);
    if Assigned(DataBuffer) then
    begin
      l := DataBuffer.Size;
      buffer.Write(l, 4);
      DataBuffer.Position := 0;
      buffer.CopyFrom(DataBuffer, DataBuffer.Size);
      s := '(Data:' + Inttostr(DataBuffer.Size) + 'b) ';
    end
    else
    begin
      l := 0;
      buffer.Write(l, 4);
      s := '';
    end;
    buffer.Position := 0;
    TPCThread.ProtectEnterCriticalSection(Self, FNetLock);
    try
      TLog.NewLog(ltdebug, Classname, 'Sending: ' + CT_NetTransferType[NetTranferType] + ' operation:' +
        TConnectionManager.OperationToText(operation) + ' id:' + Inttostr(request_id) + ' errorcode:' +
        Inttostr(errorcode) + ' Size:' + Inttostr(buffer.Size) + 'b ' + s + 'to ' + ClientRemoteAddr);
      (Client as TBufferedNetTcpIpClient).WriteBufferToSend(buffer);
      FLastDataSendedTS := GetTickCount;
      // if(operation=CT_NetOp_Hello) then (Buffer as TMemoryStream).SaveToFile('./fullhello0.bin');
      // if(operation=CT_NetOp_Hello) then (DataBuffer as TMemoryStream).SaveToFile('./body.bin');
      FRandomWaitSecondsSendHello := 90 + Random(60);
    finally
      FNetLock.Release;
    end;
    TConnectionManager.NetData.IncStatistics(0, 0, 0, 0, 0, buffer.Size);
  finally
    buffer.Free;
  end;
end;

procedure TNetConnection.SendError(NetTranferType: TNetTransferType; operation, request_id: Integer;
  error_code: Integer; error_text: AnsiString);
var
  buffer: TStream;
begin
  buffer := TMemoryStream.Create;
  try
    TStreamOp.WriteAnsiString(buffer, error_text);
    Send(NetTranferType, operation, error_code, request_id, buffer);
  finally
    buffer.Free;
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
  FNetLock.Acquire;
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
          request_id := TConnectionManager.NetData.NewRequestId;
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
    FNetLock.Release;
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
  if (FRemoteOperationBlock.Block < TConnectionManager.NetData.Bank.BlocksCount) or (FRemoteOperationBlock.Block = 0)
  then
    exit;
  if not Connected then
    exit;
  // First receive operations from
  data := TMemoryStream.Create;
  try
    if TConnectionManager.NetData.Bank.BlocksCount = 0 then
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
    request_id := TConnectionManager.NetData.NewRequestId;
    TConnectionManager.NetData.RegisterRequest(Self, CT_NetOp_GetBlocks, request_id);
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
      TConnectionManager.NetData.RegisterRequest(Self, CT_NetOp_Hello, request_id);
    end;
    if TNode.Node.NetServer.Active then
      w := TNode.Node.NetServer.port
    else
      w := 0;
    // Save active server port (2 bytes). 0 = No active server port
    data.Write(w, 2);
    // Save My connection public key
    TStreamOp.WriteAnsiString(data, TConnectionManager.NetData.NodePrivateKey.PublicKey.ToRawString);
    // Save my Unix timestamp (4 bytes)
    currunixtimestamp := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
    data.Write(currunixtimestamp, 4);
    // Save last operations block
    TBlock.SaveOperationBlockToStream(TNode.Node.Bank.LastOperationBlock, data);
    nsarr := TConnectionManager.NetData.GetValidNodeServers(true, CT_MAX_NODESERVERS_ON_HELLO);
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
    data.Write(TNode.Node.Bank.AccountStorage.WorkSum, SizeOf(TNode.Node.Bank.AccountStorage.WorkSum));
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
  FNetLock.Acquire;
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
    if (TBlock.EqualsOperationBlock(FRemoteOperationBlock, NewBlock.OperationBlock)) then
    begin
      TLog.NewLog(ltdebug, Classname, 'This connection has the same block, does not need to send');
      exit;
    end;
    if (TNode.Node.Bank.BlocksCount <> NewBlock.OperationBlock.Block + 1) then
    begin
      TLog.NewLog(ltdebug, Classname, 'The block number ' + Inttostr(NewBlock.OperationBlock.Block) +
        ' is not equal to current blocks stored in bank (' + Inttostr(TNode.Node.Bank.BlocksCount) + '), finalizing');
      exit;
    end;
    data := TMemoryStream.Create;
    try
      request_id := TConnectionManager.NetData.NewRequestId;
      NewBlock.SaveBlockToStream(false, data);
      data.Write(TNode.Node.Bank.AccountStorage.WorkSum, SizeOf(TNode.Node.Bank.AccountStorage.WorkSum));
      Send(ntp_autosend, CT_NetOp_NewBlock, 0, request_id, data);
    finally
      data.Free;
    end;
  finally
    FNetLock.Release;
  end;
  Result := Connected;
end;

procedure TNetConnection.SetClient(const Value: TNetTcpIpClient);
var
  old: TNetTcpIpClient;
begin
  if FTcpIpClient <> Value then
  begin
    if Assigned(FTcpIpClient) then
    begin
      FTcpIpClient.OnConnect := nil;
      FTcpIpClient.OnDisconnect := nil;
      FTcpIpClient.RemoveFreeNotification(Self);
    end;
    TConnectionManager.NetData.UnRegisterRequest(Self, 0, 0);
    old := FTcpIpClient;
    FTcpIpClient := Value;
    if Assigned(old) then
    begin
      if old.Owner = Self then
      begin
        old.Free;
      end;
    end;
  end;
  if Assigned(FTcpIpClient) then
  begin
    FTcpIpClient.FreeNotification(Self);
    FTcpIpClient.OnConnect := TcpClient_OnConnect;
    FTcpIpClient.OnDisconnect := TcpClient_OnDisconnect;
  end;
  TConnectionManager.NetData.NotifyNetConnectionUpdated;
end;

procedure TNetConnection.SetConnected(const Value: Boolean);
begin
  if (Value = GetConnected) then
    exit;
  if Value then
    ConnectTo(Client.RemoteHost, Client.RemotePort)
  else
  begin
    FinalizeConnection;
    Client.Disconnect;
  end;
end;

procedure TNetConnection.TcpClient_OnConnect(Sender: TObject);
begin
  TConnectionManager.NetData.IncStatistics(1, 0, 1, 0, 0, 0);
  TLog.NewLog(ltInfo, Classname, 'Connected to a server ' + ClientRemoteAddr);
  TConnectionManager.NetData.NotifyNetConnectionUpdated;
end;

procedure TNetConnection.TcpClient_OnDisconnect(Sender: TObject);
begin
  if Self is TNetServerClient then
    TConnectionManager.NetData.IncStatistics(-1, -1, 0, 0, 0, 0)
  else
  begin
    if FHasReceivedData then
      TConnectionManager.NetData.IncStatistics(-1, 0, -1, -1, 0, 0)
    else
      TConnectionManager.NetData.IncStatistics(-1, 0, -1, 0, 0, 0);
  end;
  TLog.NewLog(ltInfo, Classname, 'Disconnected from ' + ClientRemoteAddr);
  TConnectionManager.NetData.NotifyNetConnectionUpdated;
  if (FClientTimestampIp <> '') then
  begin
    TConnectionManager.NetData.NetworkAdjustedTime.RemoveIp(FClientTimestampIp);
  end;
end;

end.
