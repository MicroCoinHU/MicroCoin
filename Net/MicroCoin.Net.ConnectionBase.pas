{ ==============================================================================|
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
  | File:       MicroCoin.Net.ConnectionBase.pas                                 |
  | Created at: 2018-08-31                                                       |
  | Purpose:    Base class for net communication                                 |
  | Todo:                                                                        |
  |   - Convert abstract methods to Handlers                                     |
  |   - Clean up uses                                                            |
  |============================================================================== }

{$IFDEF fpc}
{$MODE delphi}
{$ENDIF}
unit MicroCoin.Net.ConnectionBase;

interface

uses SysUtils, Classes, UTCPIP, MicroCoin.BlockChain.BlockHeader, UThread,
  MicroCoin.Account.AccountKey, MicroCoin.Common.Lists, MicroCoin.Net.Protocol,
  MicroCoin.Net.NodeServer, Generics.Collections, MicroCoin.Net.CommandHandler,
  MicroCoin.Transaction.HashTree, MicroCoin.BlockChain.Block, ULog, SyncObjs;

type

  {$IFDEF FPC}
    TCriticalSection = SyncObjs.TCriticalSection;
  {$ENDIF}

  TNetConnectionBase = class(TComponent)
  strict private
    FIsConnecting: Boolean;
    FTcpIpClient: TNetTcpIpClient;
    FLastDataReceivedTS: Cardinal;
    FLastDataSendedTS: Cardinal;
    FClientBufferRead: TStream;
    FNetLock: TCriticalSection;
    FIsWaitingForResponse: Boolean;
    FIsMyselfServer: Boolean;
    FCreatedTime: TDateTime;
    FDoFinalizeConnection: Boolean;
    FNetProtocolVersion: TNetProtocolVersion;
    FAlertedForNewProtocolAvailable: Boolean;
    FHasReceivedData: Boolean;
    FRandomWaitSecondsSendHello: Cardinal;
    FClientTimestampIp: AnsiString;
    class var FHandlers: TDictionary<Word, TClass>;
    function GetConnected: Boolean;
    procedure SetConnected(const Value: Boolean);
    procedure TcpClient_OnConnect(Sender: TObject);
    procedure TcpClient_OnDisconnect(Sender: TObject);
    function ReadTcpClientBuffer(MaxWaitMiliseconds: Cardinal; var HeaderData: TNetHeaderData;
      BufferData: TStream): Boolean;
    function GetClient: TNetTcpIpClient;
  strict private
    function GetRemoteHost: AnsiString;
  protected
    procedure Notification(AComponent: TComponent; operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class constructor Create;
    class destructor DestroyClass;

    procedure Send(NetTranferType: TNetTransferType; operation, errorcode: Word; request_id: Integer;
      DataBuffer: TStream);
    function DoSendAndWaitForResponse(operation: Word; RequestId: Integer; SendDataBuffer, ReceiveDataBuffer: TStream;
      MaxWaitTime: Cardinal; var HeaderData: TNetHeaderData): Boolean;
    procedure DisconnectInvalidClient(ItsMyself: Boolean; const why: AnsiString);
    procedure DoProcessBuffer;
    procedure SetClient(const Value: TNetTcpIpClient);
    function ConnectTo(ServerIP: string; ServerPort: Word): Boolean;

    procedure Send_Error(NetTranferType: TNetTransferType; operation, request_id: Integer; error_code: Integer;
      error_text: AnsiString);
    function Send_Hello(NetTranferType: TNetTransferType; request_id: Integer): Boolean; virtual; abstract;
    function Send_NewBlockFound(const NewBlock: TBlock): Boolean; virtual; abstract;
    function Send_GetBlocks(StartAddress, quantity: Cardinal; var request_id: Cardinal): Boolean; virtual; abstract;
    function Send_AddOperations(Operations: TTransactionHashTree): Boolean; virtual; abstract;
    function Send_Message(const TheMessage: AnsiString): Boolean; virtual; abstract;

    class procedure AddHandler(ACommand: Word; AHandler: TClass);

    procedure FinalizeConnection;

    function ClientRemoteAddr: AnsiString;

    property Client: TNetTcpIpClient read GetClient;
    property Connected: Boolean read GetConnected write SetConnected;
    property IsConnecting: Boolean read FIsConnecting;
    //
    property NetProtocolVersion: TNetProtocolVersion read FNetProtocolVersion;
    //
    property IsMyselfServer: Boolean read FIsMyselfServer;
    property CreatedTime: TDateTime read FCreatedTime;
    property DoFinalizeConnection: Boolean read FDoFinalizeConnection write FDoFinalizeConnection;
    property NetLock: TCriticalSection read FNetLock write FNetLock;
    property TcpIpClient: TNetTcpIpClient read FTcpIpClient;
    property HasReceivedData: Boolean read FHasReceivedData;
    property RemoteHost: AnsiString read GetRemoteHost;
    property ClientTimestampIp: AnsiString read FClientTimestampIp write FClientTimestampIp;
  end;

implementation

uses UTime, MicroCoin.Net.ConnectionManager, MicroCoin.Common.Config, UCrypto,
  UECIES,
  UChunk, MicroCoin.Net.Client, {$IFDEF MSWINDOWS} Windows, {$ENDIF} MicroCoin.Transaction.Base,
  MicroCoin.Net.Utils, MicroCoin.Common.Stream,
  MicroCoin.Transaction.Manager, MicroCoin.Node.Node, MicroCoin.Account.Storage;

class procedure TNetConnectionBase.AddHandler(ACommand: Word; AHandler: TClass);
begin
  if not Supports(AHandler, ICommandHandler)
  then raise Exception.Create('Invalid command handler');
  if not assigned(FHandlers)
  then FHandlers := TDictionary<Word, TClass>.Create;
  if FHandlers.ContainsKey(ACommand)
  then raise Exception.Create('Duplicated command handler');
  FHandlers.Add(ACommand, AHandler);
end;

function TNetConnectionBase.ClientRemoteAddr: AnsiString;
begin
  if assigned(FTcpIpClient) then
    Result := FTcpIpClient.ClientRemoteAddr
  else
    Result := 'NIL';
end;

function TNetConnectionBase.ConnectTo(ServerIP: string; ServerPort: Word): Boolean;
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
    lns := TConnectionManager.Instance.NodeServersAddresses.LockList;
    try
      i := TConnectionManager.Instance.IndexOfNetClient(lns, ServerIP, ServerPort);
      if (i >= 0) then
        Pnsa := lns[i]
      else
        Pnsa := nil;
      if assigned(Pnsa) then
        Pnsa^.netConnection := Self;
    finally
      TConnectionManager.Instance.NodeServersAddresses.UnlockList;
    end;

    TPCThread.ProtectEnterCriticalSection(Self, FNetLock);
    try
      Client.RemoteHost := ServerIP;
      if ServerPort <= 0 then
        ServerPort := cNetServerPort;
      Client.RemotePort := ServerPort;
      LogDebug(Classname, 'Trying to connect to a server at: ' + ClientRemoteAddr);
      TConnectionManager.Instance.NotifyNetConnectionUpdated;
      Result := Client.Connect;
    finally
      FNetLock.Release;
    end;
    if Result then
    begin
      LogDebug(Classname, 'Connected to a possible server at: ' + ClientRemoteAddr);
      Result := Send_Hello(ntp_request, TConnectionManager.Instance.NewRequestId);
    end
    else
    begin
      LogDebug( Classname, 'Cannot connect to a server at: ' + ClientRemoteAddr);
    end;
  finally
    FIsConnecting := false;
  end;
end;

class constructor TNetConnectionBase.Create;
begin
  FHandlers := TDictionary<Word, TClass>.Create;
end;

constructor TNetConnectionBase.Create(AOwner: TComponent);
begin
  inherited;
  FIsConnecting := false;
  FHasReceivedData := false;
  FClientTimestampIp := '';
  FNetProtocolVersion.protocol_version := 0; // 0 = unknown
  FNetProtocolVersion.protocol_available := 0;
  FAlertedForNewProtocolAvailable := false;
  FDoFinalizeConnection := false;
  FCreatedTime := now;
  FIsMyselfServer := false;
  FIsWaitingForResponse := false;
  FClientBufferRead := TMemoryStream.Create;
  FNetLock := TCriticalSection.Create;
  FLastDataReceivedTS := 0;
  FLastDataSendedTS := 0;
  FRandomWaitSecondsSendHello := 90 + Random(60);
  FTcpIpClient := nil;
  SetClient(TBufferedNetTcpIpClient.Create(Self));
  TConnectionManager.Instance.NetConnections.Add(Self);
  TConnectionManager.Instance.NotifyNetConnectionUpdated;
end;

destructor TNetConnectionBase.Destroy;
var
  Pnsa: PNodeServerAddress;
  lns: TList;
  i: Integer;
begin
  try
    LogDebug(Classname, 'Destroying ' + Classname + ' ' + IntToHex(PtrInt(Self), 8));

    Connected := false;

    lns := TConnectionManager.NodeServersAddresses.LockList;
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
      TConnectionManager.NodeServersAddresses.UnlockList;
    end;
  finally
    TConnectionManager.NetConnections.Remove(Self);
  end;
  if not TConnectionManager.Destroying then
    TConnectionManager.Instance.UnRegisterRequest(Self, 0, 0);
  try
    if not TConnectionManager.Destroying then
      TConnectionManager.Instance.NotifyNetConnectionUpdated;
  finally
    FreeAndNil(FNetLock);
    FreeAndNil(FClientBufferRead);
    FreeAndNil(FTcpIpClient);
    inherited;
  end;
end;

class destructor TNetConnectionBase.DestroyClass;
begin
  FreeAndNil(FHandlers);
end;

procedure TNetConnectionBase.DisconnectInvalidClient(ItsMyself: Boolean; const why: AnsiString);
var
  P: PNodeServerAddress;
  l: TList;
  i: Integer;
  include_in_list: Boolean;
begin
  // FIsDownloadingBlocks := false;
  if ItsMyself then
  begin
    TLog.NewLog(ltInfo, Classname, 'Disconnecting myself ' + ClientRemoteAddr + ' > ' + why)
  end
  else
  begin
    TLog.NewLog(ltError, Classname, 'Disconnecting ' + ClientRemoteAddr + ' > ' + why);
  end;
  FIsMyselfServer := ItsMyself;
  include_in_list := (not SameText(Client.RemoteHost, 'localhost')) and (not SameText(Client.RemoteHost, '127.0.0.1'))
    and (not SameText('192.168.', Copy(Client.RemoteHost, 1, 8))) and
    (not SameText('10.', Copy(Client.RemoteHost, 1, 3)));
  if include_in_list then
  begin
    l := TConnectionManager.Instance.NodeServersAddresses.LockList;
    try
      i := TConnectionManager.Instance.IndexOfNetClient(l, Client.RemoteHost, Client.RemotePort);
      if i < 0 then
      begin
        New(P);
        P^ := TNodeServer.Empty;
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
      TConnectionManager.Instance.NodeServersAddresses.UnlockList;
    end;
  end
  else if ItsMyself then
  begin
    l := TConnectionManager.Instance.NodeServersAddresses.LockList;
    try
      i := TConnectionManager.Instance.IndexOfNetClient(l, Client.RemoteHost, Client.RemotePort);
      if i >= 0 then
      begin
        P := l[i];
        P^.its_myself := ItsMyself;
      end;
    finally
      TConnectionManager.Instance.NodeServersAddresses.UnlockList;
    end;
  end;
  Connected := false;
  TConnectionManager.Instance.NotifyBlackListUpdated;
  TConnectionManager.Instance.NotifyNodeServersUpdated;
end;

procedure TNetConnectionBase.DoProcessBuffer;
var
  HeaderData: TNetHeaderData;
  ms: TMemoryStream;
  ops: AnsiString;
begin
  if FDoFinalizeConnection then
  begin
    LogDebug(Classname, 'Executing DoFinalizeConnection at client ' + ClientRemoteAddr);
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
    if TConnectionManager.Instance.PendingRequest(Self, ops) >= 2 then
    begin
      LogDebug(Classname, 'Pending requests without response... closing connection to ' + ClientRemoteAddr +
        ' > ' + ops);
      Connected := false;
    end
    else
    begin
      LogDebug(Classname, 'Sending Hello to check connection to ' + ClientRemoteAddr + ' > ' + ops);
      Send_Hello(ntp_request, TConnectionManager.Instance.NewRequestId);
    end;
  end
  else if (Self is TNetServerClient) and (FLastDataReceivedTS = 0) and (FCreatedTime + EncodeTime(0, 1, 0, 0) < now)
  then
  begin
    // Disconnecting client without data...
    LogDebug(Classname, 'Disconnecting client without data ' + ClientRemoteAddr);
    Connected := false;
  end;
end;

function TNetConnectionBase.DoSendAndWaitForResponse(operation: Word; RequestId: Integer;
  SendDataBuffer, ReceiveDataBuffer: TStream; MaxWaitTime: Cardinal; var HeaderData: TNetHeaderData): Boolean;
var
  tc: Cardinal;
  was_waiting_for_response: Boolean;
  l: TList;
  i: Integer;
  xHandler: ICommandHandler;
  xObj: TObject;
begin
  try
    Result := false;
    HeaderData := TNetHeaderData.Empty;
    if FIsWaitingForResponse then
    begin
      LogDebug(Classname, 'Is waiting for response ...');
      exit;
    end;
    if not assigned(FTcpIpClient) then
      exit;
    if not Client.Connected then
      exit;
    tc := GetTickCount;
    if TPCThread.TryProtectEnterCriticalSection(Self, MaxWaitTime, FNetLock) then
    begin
      try
        was_waiting_for_response := RequestId > 0;
        try
          if was_waiting_for_response then
          begin
            FIsWaitingForResponse := true;
            Send(ntp_request, operation, 0, RequestId, SendDataBuffer);
          end;
          repeat
            if (MaxWaitTime > GetTickCount - tc) then
              MaxWaitTime := MaxWaitTime - (GetTickCount - tc)
            else
              MaxWaitTime := 1;
            tc := GetTickCount;
            if (ReadTcpClientBuffer(MaxWaitTime, HeaderData, ReceiveDataBuffer)) then
            begin
              l := TConnectionManager.Instance.NodeServersAddresses.LockList;
              try
                for i := 0 to l.Count - 1 do
                begin
                  if PNodeServerAddress(l[i])^.netConnection = Self then
                  begin
                    PNodeServerAddress(l[i])^.last_connection := (UnivDateTimeToUnix(DateTime2UnivDateTime(now)));
                    PNodeServerAddress(l[i])^.total_failed_attemps_to_connect := 0;
                  end;
                end;
              finally
                TConnectionManager.Instance.NodeServersAddresses.UnlockList;
              end;
              LogDebug(Classname, 'Received ' + CT_NetTransferType[HeaderData.HeaderType] + ' operation:' +
                HeaderData.OperationTxt + ' id:' + Inttostr(HeaderData.RequestId) + ' Buffer size:' +
                Inttostr(HeaderData.BufferDataLength));
              if (RequestId = HeaderData.RequestId) and (HeaderData.HeaderType = ntp_response) then
              begin
                Result := true;
              end
              else
              begin
                if FHandlers.ContainsKey(HeaderData.operation) then
                begin
                  try
                    Supports(FHandlers[HeaderData.operation].Create, ICommandHandler, xHandler);
                    xHandler.HandleCommand(HeaderData, ReceiveDataBuffer, Self);
                  except
                    on E: Exception do begin
                      {$IFDEF DEBUG}
                        {$IFNDEF FPC}
                          TLog.NewLog(ltError, Classname, E.Message +' '+ E.StackTrace);
                        {$ENDIF}
                      {$ENDIF}
                      DisconnectInvalidClient(false, E.Message);
                    end;
                  end;
                end
                else
                begin
                  DisconnectInvalidClient(false, 'Invalid operation: ' + (HeaderData.ToString));
                end;
              end;
            end
            else
              sleep(1);
          until (Result) or (GetTickCount > (MaxWaitTime + tc));
        finally
          if was_waiting_for_response then
            FIsWaitingForResponse := false;
        end;
      finally
        FNetLock.Release;
      end;
    end;
  except
    on E: Exception do
    begin
      E.Message := E.Message + ' DoSendAndWaitForResponse';
      raise;
    end;
  end;
end;

procedure TNetConnectionBase.FinalizeConnection;
begin
  if FDoFinalizeConnection then
    exit;
  LogDebug(Classname, 'Executing FinalizeConnection to ' + ClientRemoteAddr);
  FDoFinalizeConnection := true;
end;

function TNetConnectionBase.GetClient: TNetTcpIpClient;
begin
  if not assigned(FTcpIpClient) then
  begin
    TLog.NewLog(ltError, Classname, 'TcpIpClient=NIL');
    raise Exception.Create('TcpIpClient=NIL');
  end;
  Result := FTcpIpClient;
end;

function TNetConnectionBase.GetConnected: Boolean;
begin
  Result := assigned(FTcpIpClient) and (FTcpIpClient.Connected);
end;

function TNetConnectionBase.GetRemoteHost: AnsiString;
begin
  Result := FTcpIpClient.RemoteHost;
end;

procedure TNetConnectionBase.Notification(AComponent: TComponent; operation: TOperation);
begin
  inherited;
  if (operation = OpRemove) and (AComponent = FTcpIpClient) then
    FTcpIpClient := nil;
end;

function TNetConnectionBase.ReadTcpClientBuffer(MaxWaitMiliseconds: Cardinal; var HeaderData: TNetHeaderData;
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
  HeaderData := TNetHeaderData.Empty;
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
        if HeaderData.Protocol.protocol_version > cNetProtocol_Available then
        begin
          TNode.Node.NotifyNetClientMessage(nil, 'Detected a higher Net protocol version at ' + ClientRemoteAddr +
            ' (v ' + Inttostr(HeaderData.Protocol.protocol_version) + ' ' +
            Inttostr(HeaderData.Protocol.protocol_available) + ') ' +
            '... check that your version is Ok! Visit official download website for possible updates: https://github.com/MicroCoinHU/');
          DisconnectInvalidClient(false, Format('Invalid Net protocol version found: %d available: %d',
            [HeaderData.Protocol.protocol_version, HeaderData.Protocol.protocol_available]));
          Result := false;
          exit;
        end
        else
        begin
          if (FNetProtocolVersion.protocol_available > cNetProtocol_Available) and (not FAlertedForNewProtocolAvailable)
          then
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
          exit;
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
          LogDebug( Classname,
            Format('Not enough data received - Received %d bytes from TcpClient buffer of %s after max %d miliseconds. Elapsed: %d - HeaderData: %s',
            [FClientBufferRead.Size, Client.ClientRemoteAddr, MaxWaitMiliseconds, GetTickCount - tc,
            (HeaderData.ToString)]));
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
        TConnectionManager.Instance.IncStatistics(0, 0, 0, 1, t_bytes_read, 0)
      else
        TConnectionManager.Instance.IncStatistics(0, 0, 0, 0, t_bytes_read, 0);
    end
    else
    begin
      TConnectionManager.Instance.IncStatistics(0, 0, 0, 0, t_bytes_read, 0);
    end;
  end;
  if (Result) and (HeaderData.HeaderType = ntp_response) then
    TConnectionManager.Instance.UnRegisterRequest(Self, HeaderData.operation, HeaderData.RequestId);
end;

procedure TNetConnectionBase.Send(NetTranferType: TNetTransferType; operation, errorcode: Word; request_id: Integer;
  DataBuffer: TStream);
var
  l: Cardinal;
  w: Word;
  buffer: TMemoryStream;
  s: AnsiString;
begin
  buffer := TMemoryStream.Create;
  try
    l := cMagicNetIdentification;
    buffer.Write(l, 4);
    case NetTranferType of
      ntp_request:
        begin
          w := cMagicRequest;
          buffer.Write(w, 2);
          buffer.Write(operation, 2);
          w := 0;
          buffer.Write(w, 2);
          buffer.Write(request_id, 4);
        end;
      ntp_response:
        begin
          w := cMagicResponse;
          buffer.Write(w, 2);
          buffer.Write(operation, 2);
          buffer.Write(errorcode, 2);
          buffer.Write(request_id, 4);
        end;
      ntp_autosend:
        begin
          w := cMagicAutoSend;
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
    l := cNetProtocol_Version;
    buffer.Write(l, 2);
    l := cNetProtocol_Available;
    buffer.Write(l, 2);
    if assigned(DataBuffer) then
    begin
      l := DataBuffer.Size;
      buffer.Write(l, 4);
      DataBuffer.Position := 0;
      buffer.CopyFrom(DataBuffer, DataBuffer.Size);
      s := '(Data:' + Inttostr(DataBuffer.Size) + 'b) ';
    end else begin
      l := 0;
      buffer.Write(l, 4);
      s := '';
    end;
    buffer.Position := 0;
    TPCThread.ProtectEnterCriticalSection(Self, FNetLock);
    try
      LogDebug(Classname, 'Sending: ' + CT_NetTransferType[NetTranferType] + ' operation:' +
        Inttostr(operation) + ' id:' + Inttostr(request_id) + ' errorcode:' + Inttostr(errorcode) + ' Size:' +
        Inttostr(buffer.Size) + 'b ' + s + 'to ' + ClientRemoteAddr);
      (Client as TBufferedNetTcpIpClient).WriteBufferToSend(buffer);
      FLastDataSendedTS := GetTickCount;
      FRandomWaitSecondsSendHello := 90 + Random(60);
    finally
      FNetLock.Release;
    end;
    TConnectionManager.Instance.IncStatistics(0, 0, 0, 0, 0, buffer.Size);
  finally
    buffer.Free;
  end;
end;

procedure TNetConnectionBase.Send_Error(NetTranferType: TNetTransferType; operation, request_id: Integer;
  error_code: Integer; error_text: AnsiString);
var
  buffer: TStream;
begin
  buffer := TMemoryStream.Create;
  try
    buffer.WriteAnsiString(error_text);
    Send(NetTranferType, operation, error_code, request_id, buffer);
  finally
    buffer.Free;
  end;
end;

procedure TNetConnectionBase.SetClient(const Value: TNetTcpIpClient);
var
  old: TNetTcpIpClient;
begin
  if FTcpIpClient <> Value then
  begin
    if assigned(FTcpIpClient) then
    begin
      FTcpIpClient.OnConnect := nil;
      FTcpIpClient.OnDisconnect := nil;
      FTcpIpClient.RemoveFreeNotification(Self);
    end;
    TConnectionManager.Instance.UnRegisterRequest(Self, 0, 0);
    old := FTcpIpClient;
    FTcpIpClient := Value;
    if assigned(old) then
    begin
      if old.Owner = Self then
      begin
        old.Free;
      end;
    end;
  end;
  if assigned(FTcpIpClient) then
  begin
    FTcpIpClient.FreeNotification(Self);
    FTcpIpClient.OnConnect := TcpClient_OnConnect;
    FTcpIpClient.OnDisconnect := TcpClient_OnDisconnect;
  end;
  TConnectionManager.Instance.NotifyNetConnectionUpdated;
end;

procedure TNetConnectionBase.SetConnected(const Value: Boolean);
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

procedure TNetConnectionBase.TcpClient_OnConnect(Sender: TObject);
begin
  TConnectionManager.Instance.IncStatistics(1, 0, 1, 0, 0, 0);
  TLog.NewLog(ltInfo, Classname, 'Connected to a server ' + ClientRemoteAddr);
  TConnectionManager.Instance.NotifyNetConnectionUpdated;
end;

procedure TNetConnectionBase.TcpClient_OnDisconnect(Sender: TObject);
begin
  if Self is TNetServerClient then
    TConnectionManager.Instance.IncStatistics(-1, -1, 0, 0, 0, 0)
  else
  begin
    if FHasReceivedData then
      TConnectionManager.Instance.IncStatistics(-1, 0, -1, -1, 0, 0)
    else
      TConnectionManager.Instance.IncStatistics(-1, 0, -1, 0, 0, 0);
  end;
  TLog.NewLog(ltInfo, Classname, 'Disconnected from ' + ClientRemoteAddr);
  TConnectionManager.Instance.NotifyNetConnectionUpdated;
  if (FClientTimestampIp <> '') then
  begin
    TConnectionManager.Instance.NetworkAdjustedTime.RemoveIp(FClientTimestampIp);
  end;
end;

end.
