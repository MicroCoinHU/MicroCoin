unit UTCPIP;

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

{$IFDEF FPC}
  {$MODE delphi}
{$ENDIF}
{$I config.inc}
  uses blcksock, synsock, Classes, Sysutils, UThread, SyncObjs;

type

  { TNetTcpIpClient }

  TNetTcpIpClient = class(TComponent)
  private
    FTcpBlockSocket: TTCPBlockSocket;
    FConnected: Boolean;
    FRemoteHost: AnsiString;
    FRemotePort: Word;
    FBytesReceived, FBytesSent: Int64;
    FLock: TCriticalSection;
    FSendBufferLock: TCriticalSection;
    FOnConnect: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    FSocketError: Integer;
    FLastCommunicationTime: TDateTime;
    function GetConnected: Boolean;
    function GetRemoteHost: AnsiString;
    function GetRemotePort: Word;
    procedure SetRemoteHost(const Value: AnsiString);
    procedure SetRemotePort(const Value: Word);
    procedure SetOnConnect(const Value: TNotifyEvent);
    procedure SetOnDisconnect(const Value: TNotifyEvent);
    procedure SetSocketError(const Value: Integer);
    function GetClientRemoteAddr: AnsiString;
  strict protected
    procedure DoOnConnect; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Connect: Boolean;
    procedure Disconnect;

    function ReceiveBuf(var Buf; BufSize: Integer): Integer;
    function SendStream(Stream: TStream): Int64;
    procedure DoWaitForData(WaitMilliseconds: Integer; var HasData: Boolean); virtual;

    function WaitForData(WaitMilliseconds: Integer): Boolean;

    property BytesReceived: Int64 read FBytesReceived;
    property BytesSent: Int64 read FBytesSent;
    property RemoteHost: AnsiString read GetRemoteHost write SetRemoteHost;
    property RemotePort: Word read GetRemotePort write SetRemotePort;
    property Connected: Boolean read GetConnected;
    property OnConnect: TNotifyEvent read FOnConnect write SetOnConnect;
    property OnDisconnect: TNotifyEvent read FOnDisconnect write SetOnDisconnect;
    property SocketError: Integer read FSocketError write SetSocketError;
    property LastCommunicationTime: TDateTime read FLastCommunicationTime;
    property ClientRemoteAddr : AnsiString read GetClientRemoteAddr;
  end;

  TNetTcpIpClientClass = class of TNetTcpIpClient;

  TBufferedNetTcpIpClient = class;

  TBufferedNetTcpIpClientThread = class(TPCThread)
    FBufferedNetTcpIpClient: TBufferedNetTcpIpClient;
  protected
    procedure BCExecute; override;
  public
    constructor Create(ABufferedNetTcpIpClient: TBufferedNetTcpIpClient);
  end;

  TBufferedNetTcpIpClient = class(TNetTcpIpClient)
  private
    FSendBuffer: TMemoryStream;
    FReadBuffer: TMemoryStream;
    FCritical: TCriticalSection;
    FLastReadTC: Cardinal;
    FBufferedNetTcpIpClientThread: TBufferedNetTcpIpClientThread;
  protected
    function DoWaitForDataInherited(WaitMilliseconds: Integer): Boolean;
    procedure DoWaitForData(WaitMilliseconds: Integer; var HasData: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteBufferToSend(SendData: TStream);
    function ReadBufferLock: TMemoryStream;
    procedure ReadBufferUnlock;
    property LastReadTC: Cardinal read FLastReadTC;
  end;

  TNetTcpIpServer = class;
  TTcpIpServerListenerThread = class;

  TTcpIpSocketThread = class(TPCThread)
  private
    FSock: TTCPBlockSocket;
    FListenerThread: TTcpIpServerListenerThread;
  protected
    procedure BCExecute; override;
  public
    constructor Create(AListenerThread: TTcpIpServerListenerThread; ASocket: TSocket);
    destructor Destroy; override;
  end;

  TTcpIpServerListenerThread = class(TPCThread)
  private
    FNetTcpIpServerServer: TNetTcpIpServer;
    FServerSocket: TTCPBlockSocket;
    FTcpIpSocketsThread: TPCThreadList;
  protected
    procedure BCExecute; override;
  public
    constructor Create(ANetTcpIpServer: TNetTcpIpServer);
    destructor Destroy; override;
  end;

  { TNetTcpIpServer }

  TNetTcpIpServer = class(TObject)
  private
    FTcpIpServer: TTcpIpServerListenerThread;
    FPort: Word;
    FActive: Boolean;
    FNetClients: TPCThreadList;
    FMaxConnections: Integer;
    FNetTcpIpClientClass: TNetTcpIpClientClass;
    function GetActive: Boolean;
    procedure SetPort(const Value: Word);
    // When a connection is established to a new client, a TNetConnection is created (p2p)
    function GetPort: Word;
    procedure OnTcpServerAccept(Sender: TObject; ClientSocket: TTCPBlockSocket);
    procedure SetNetTcpIpClientClass(const Value: TNetTcpIpClientClass);
  protected
    procedure OnNewIncommingConnection(Sender: TObject; Client: TNetTcpIpClient); virtual;
    procedure SetActive(const Value: Boolean); virtual;
    procedure SetMaxConnections(AValue: Integer); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Active: Boolean read GetActive write SetActive;
    property Port: Word read GetPort write SetPort;
    property MaxConnections: Integer read FMaxConnections write SetMaxConnections;
    property NetTcpIpClientClass: TNetTcpIpClientClass read FNetTcpIpClientClass write SetNetTcpIpClientClass;
    function NetTcpIpClientsLock: TList;
    procedure NetTcpIpClientsUnlock;
    procedure WaitUntilNetTcpIpClientsFinalized;
  end;

implementation

uses
{$IFNDEF FPC}
    Windows,
{$ELSE}
  {LCLIntf, LCLType, LMessages,}
{$ENDIF}
  MicroCoin.Common.Config, ULog;

resourcestring
  rsCannotConnec = 'Cannot connect to a server at: %s Reason: %s';
  rsErrorConnect = 'Error Connecting to %s: %s';
  rsExceptionAtT = 'Exception at TNetTcpIpClient.Discconnect step %s - %s';
  rsErrorWaiting = 'Error WaitingForData from %s: %s';
  rsClosingConne = 'Closing connection from %s (Receiving error): %s %s';
  rsExceptionRec = 'Exception receiving buffer from %s %s (%s):%s';
  rsClosingConne2 = 'Closing connection from %s (Sending error): %s %s';

  { TNetTcpIpClient }

function TNetTcpIpClient.GetClientRemoteAddr: AnsiString;
begin
  if Assigned(FTcpBlockSocket) then
    Result := FRemoteHost + ':' + inttostr(FRemotePort)
  else
    Result := 'NIL';
end;

function TNetTcpIpClient.Connect: Boolean;
begin
  FLock.Acquire;
  try
    try
      FTcpBlockSocket.Connect(FRemoteHost, inttostr(FRemotePort));
      FConnected := FTcpBlockSocket.LastError = 0;
      if (FConnected) then
      begin
        FRemoteHost := FTcpBlockSocket.GetRemoteSinIP;
        FRemotePort := FTcpBlockSocket.GetRemoteSinPort;
        DoOnConnect;
      end
      else
        LogDebug(Classname, Format(rsCannotConnec, [ClientRemoteAddr, FTcpBlockSocket.GetErrorDescEx]));
    except
      on E: Exception do
      begin
        SocketError := FTcpBlockSocket.LastError;
        TLog.NewLog(lterror, Classname, Format(rsErrorConnect, [ClientRemoteAddr, FTcpBlockSocket.GetErrorDescEx]));
        Disconnect;
      end;
    end;
  finally
    FLock.Release;
  end;
  Result := FConnected;
end;

constructor TNetTcpIpClient.Create(AOwner: TComponent);
begin
  inherited;
  FOnConnect := nil;
  FOnDisconnect := nil;
  FTcpBlockSocket := nil;
  FSocketError := 0;
  FLastCommunicationTime := 0;
  FLock := TCriticalSection.Create;
  FSendBufferLock := TCriticalSection.Create;
  FTcpBlockSocket := TTCPBlockSocket.Create;
  FTcpBlockSocket.OnAfterConnect := OnConnect;
  FTcpBlockSocket.SocksTimeout := 5000; // Build 1.5.0 was 10000;
  FTcpBlockSocket.ConnectionTimeout := 5000; // Build 1.5.0 was default
  FRemoteHost := '';
  FRemotePort := 0;
  FBytesReceived := 0;
  FBytesSent := 0;
  FConnected := False;
end;

destructor TNetTcpIpClient.Destroy;
begin
  Disconnect;
  FreeAndNil(FSendBufferLock);
  FreeAndNil(FLock);
  inherited;
  FreeAndNil(FTcpBlockSocket);
{$IFDEF HIGHLOG}LogDebug(Classname, 'Destroying Socket end'); {$ENDIF}
end;

procedure TNetTcpIpClient.Disconnect;
begin
  if not FConnected then
    exit;
  try
    FLock.Acquire;
    try
      if not FConnected
      then exit;
      FTcpBlockSocket.CloseSocket;
      FConnected := False;
    finally
      FLock.Release;
    end;
    if Assigned(FOnDisconnect)
    then FOnDisconnect(Self)
    else TLog.NewLog(lterror, Classname, 'OnDisconnect is nil');
  except
    on E: Exception do
    begin
      E.Message := Format(rsExceptionAtT, [E.Message]);
      raise;
    end;
  end;
end;

procedure TNetTcpIpClient.DoOnConnect;
begin
  if (Assigned(FOnConnect))
  then FOnConnect(Self);
end;

procedure TNetTcpIpClient.DoWaitForData(WaitMilliseconds: Integer; var HasData: Boolean);
begin
  FLock.Acquire;
  try
    try
      HasData := FTcpBlockSocket.CanRead(WaitMilliseconds);
    except
      on E: Exception do
      begin
        SocketError := FTcpBlockSocket.LastError;
        HasData := False;
        TLog.NewLog(lterror, Classname, Format(rsErrorWaiting, [ClientRemoteAddr, FTcpBlockSocket.GetErrorDescEx]));
        Disconnect;
      end;
    end;
  finally
    FLock.Release;
  end;
end;

function TNetTcpIpClient.GetConnected: Boolean;
begin
  Result := FConnected;
end;

function TNetTcpIpClient.GetRemoteHost: AnsiString;
begin
  Result := FRemoteHost;
end;

function TNetTcpIpClient.GetRemotePort: Word;
begin
  Result := FRemotePort;
end;

function TNetTcpIpClient.ReceiveBuf(var Buf; BufSize: Integer): Integer;
begin
  Result := 0;
  FLock.Acquire;
  try
    try
      Result := FTcpBlockSocket.RecvBuffer(@Buf, BufSize);
      if (Result < 0) or (FTcpBlockSocket.LastError <> 0) then
      begin
        LogDebug(Classname, Format(rsClosingConne, [ClientRemoteAddr, inttostr(FTcpBlockSocket.LastError),
          FTcpBlockSocket.GetErrorDescEx]));
        Result := 0;
        Disconnect;
      end
      else if Result > 0 then
        inc(FBytesReceived, Result);
    except
      on E: Exception do
      begin
        SocketError := FTcpBlockSocket.LastError;
        TLog.NewLog(lterror, Classname, Format(rsExceptionRec, [ClientRemoteAddr, FTcpBlockSocket.GetErrorDescEx,
          E.Classname, E.Message]));
        Disconnect;
      end;
    end;
  finally
    FLock.Release;
  end;
  if Result > 0 then
    FLastCommunicationTime := Now;
end;

function TNetTcpIpClient.SendStream(Stream: TStream): Int64;
var
  sp: Int64;
  unlocked: Boolean;
begin
  sp := Stream.Position;
  Result := 0;
  unlocked := False;
  // In order to allow a big stream sending, will cut up in small blocks
  FSendBufferLock.Acquire;
  try
    try
      FTcpBlockSocket.SendStreamRaw(Stream);
      if FTcpBlockSocket.LastError <> 0 then
      begin
        LogDebug( Classname, Format(rsClosingConne2, [ClientRemoteAddr, inttostr(FTcpBlockSocket.LastError),
          FTcpBlockSocket.GetErrorDescEx]));
        Result := -1;
        unlocked := true;
        FSendBufferLock.Release;
        Disconnect;
      end
      else
      begin
        Result := Stream.Position - sp;
        inc(FBytesSent, Result);
      end;
    except
      on E: Exception do
      begin
        SocketError := FTcpBlockSocket.LastError;
        TLog.NewLog(lterror, Classname, 'Exception sending stream to ' + ClientRemoteAddr + ': ' +
          FTcpBlockSocket.GetErrorDescEx);
        unlocked := true;
        FSendBufferLock.Release;
        Disconnect;
      end;
    end;
  finally
    if not unlocked then
      FSendBufferLock.Release;
  end;
  if Result > 0 then
    FLastCommunicationTime := Now;
end;

procedure TNetTcpIpClient.SetOnConnect(const Value: TNotifyEvent);
begin
  FOnConnect := Value;
end;

procedure TNetTcpIpClient.SetOnDisconnect(const Value: TNotifyEvent);
begin
  FOnDisconnect := Value;
end;

procedure TNetTcpIpClient.SetRemoteHost(const Value: AnsiString);
begin
  FRemoteHost := Value;
end;

procedure TNetTcpIpClient.SetRemotePort(const Value: Word);
begin
  FRemotePort := Value;
end;

procedure TNetTcpIpClient.SetSocketError(const Value: Integer);
begin
  FSocketError := Value;
  if Value <> 0 then
    LogDebug(Classname, 'Error ' + inttohex(SocketError, 8) + ' with connection to ' + ClientRemoteAddr);
end;

function TNetTcpIpClient.WaitForData(WaitMilliseconds: Integer): Boolean;
begin
  DoWaitForData(WaitMilliseconds, Result);
end;

{ TBufferedNetTcpIpClientThread }

procedure TBufferedNetTcpIpClientThread.BCExecute;
var
  SendBuffStream: TStream;
  ReceiveBuffer: array [0 .. 4095] of byte;
  procedure DoReceiveBuf;
  var
    last_bytes_read: Integer;
    total_read, total_size: Int64;
    ms: TMemoryStream;
    lastpos: Int64;
  begin
    total_read := 0;
    total_size := 0;
    if FBufferedNetTcpIpClient.DoWaitForDataInherited(10) then
    begin
      last_bytes_read := 0;
      repeat
        if last_bytes_read <> 0 then
        begin
          // This is to prevent a 4096 buffer transmission only... and a loop
          if not FBufferedNetTcpIpClient.DoWaitForDataInherited(10) then
          begin
            if FBufferedNetTcpIpClient.SocketError <> 0 then
              FBufferedNetTcpIpClient.Disconnect;
            exit;
          end;
        end;

        last_bytes_read := FBufferedNetTcpIpClient.ReceiveBuf(ReceiveBuffer, sizeof(ReceiveBuffer));
        if (last_bytes_read > 0) then
        begin
          ms := FBufferedNetTcpIpClient.ReadBufferLock;
          try
            FBufferedNetTcpIpClient.FLastReadTC := GetTickCount;
            lastpos := ms.Position;
            ms.Position := ms.Size;
            ms.Write(ReceiveBuffer, last_bytes_read);
            ms.Position := lastpos;
            inc(total_read, last_bytes_read);
            total_size := ms.Size;
          finally
            FBufferedNetTcpIpClient.ReadBufferUnlock;
          end;
        end;
      until (last_bytes_read < sizeof(ReceiveBuffer)) or (Terminated) or (not FBufferedNetTcpIpClient.Connected);
      if total_read > 0 then
        LogDebug(Classname, Format('Received %d bytes. Buffer length: %d bytes', [total_read, total_size]));
    end
    else
    begin
      if FBufferedNetTcpIpClient.SocketError <> 0 then
        FBufferedNetTcpIpClient.Disconnect;
    end;
  end;
  procedure DoSendBuf;
  begin
    FBufferedNetTcpIpClient.FCritical.Acquire;
    try
      if FBufferedNetTcpIpClient.FSendBuffer.Size > 0 then
      begin
        SendBuffStream.Size := 0;
        SendBuffStream.CopyFrom(FBufferedNetTcpIpClient.FSendBuffer, 0);
        FBufferedNetTcpIpClient.FSendBuffer.Size := 0;
      end;
    finally
      FBufferedNetTcpIpClient.FCritical.Release;
    end;
    if (SendBuffStream.Size > 0) then
    begin
      SendBuffStream.Position := 0;
      FBufferedNetTcpIpClient.SendStream(SendBuffStream);
      LogDebug( Classname, Format('Sent %d bytes', [SendBuffStream.Size]));
      SendBuffStream.Size := 0;
    end;
  end;

begin
  SendBuffStream := TMemoryStream.Create;
  try
    while (not Terminated) do
    begin
      while (not Terminated) and (not FBufferedNetTcpIpClient.Connected) do
        sleep(100);
      if (FBufferedNetTcpIpClient.Connected) then
      begin
        // Receive data
        if (not Terminated) and (FBufferedNetTcpIpClient.Connected) then
          DoReceiveBuf;
        // Send Data
        if (not Terminated) and (FBufferedNetTcpIpClient.Connected) then
          DoSendBuf;
      end
      else
        FBufferedNetTcpIpClient.FLastReadTC := GetTickCount;
      // Sleep
      sleep(10); // Slepp 10 is better than sleep 1
    end;
  finally
    SendBuffStream.Free;
  end;
end;

constructor TBufferedNetTcpIpClientThread.Create(ABufferedNetTcpIpClient: TBufferedNetTcpIpClient);
begin
  FBufferedNetTcpIpClient := ABufferedNetTcpIpClient;
  inherited Create(False);
end;

{ TBufferedNetTcpIpClient }

constructor TBufferedNetTcpIpClient.Create(AOwner: TComponent);
begin
  inherited;
  FLastReadTC := GetTickCount;
  FCritical := TCriticalSection.Create;
  FSendBuffer := TMemoryStream.Create;
  FReadBuffer := TMemoryStream.Create;
  FBufferedNetTcpIpClientThread := TBufferedNetTcpIpClientThread.Create(Self);
end;

destructor TBufferedNetTcpIpClient.Destroy;
begin
  FBufferedNetTcpIpClientThread.Terminate;
  FBufferedNetTcpIpClientThread.WaitFor;
  FreeAndNil(FBufferedNetTcpIpClientThread);
  FreeAndNil(FCritical);
  FreeAndNil(FReadBuffer);
  FreeAndNil(FSendBuffer);
  inherited;
end;

procedure TBufferedNetTcpIpClient.DoWaitForData(WaitMilliseconds: Integer; var HasData: Boolean);
begin
  FCritical.Acquire;
  try
    if FReadBuffer.Size > 0 then
    begin
      HasData := true;
      exit;
    end;
  finally
    FCritical.Release;
  end;
  inherited DoWaitForData(WaitMilliseconds, HasData);
end;

function TBufferedNetTcpIpClient.DoWaitForDataInherited(WaitMilliseconds: Integer): Boolean;
begin
  inherited DoWaitForData(WaitMilliseconds, Result);
end;

function TBufferedNetTcpIpClient.ReadBufferLock: TMemoryStream;
begin
  FCritical.Acquire;
  Result := FReadBuffer;
end;

procedure TBufferedNetTcpIpClient.ReadBufferUnlock;
begin
  FCritical.Release;
end;

procedure TBufferedNetTcpIpClient.WriteBufferToSend(SendData: TStream);
var
  lastpos: Int64;
begin
  FCritical.Acquire;
  try
    lastpos := FSendBuffer.Position;
    FSendBuffer.Position := FSendBuffer.Size;
    SendData.Position := 0;
    FSendBuffer.CopyFrom(SendData, SendData.Size);
    FSendBuffer.Position := lastpos;
  finally
    FCritical.Release;
  end;
end;

{ TNetTcpIpServer }

constructor TNetTcpIpServer.Create;
begin
  FNetTcpIpClientClass := TNetTcpIpClient;
  FTcpIpServer := nil;
  FMaxConnections := cMaximumClients;
  FActive := False;
  FNetClients := TPCThreadList.Create('TNetTcpIpServer_NetClients');
end;

destructor TNetTcpIpServer.Destroy;
begin
  Active := False;
  inherited;
  FreeAndNil(FNetClients);
end;

function TNetTcpIpServer.GetActive: Boolean;
begin
  Result := Assigned(FTcpIpServer) and (FActive);
end;

procedure TNetTcpIpServer.SetMaxConnections(AValue: Integer);
begin
  if FMaxConnections = AValue then
    exit;
  FMaxConnections := AValue;
end;

function TNetTcpIpServer.GetPort: Word;
begin
  Result := FPort;
end;

function TNetTcpIpServer.NetTcpIpClientsLock: TList;
begin
  Result := FNetClients.LockList;
end;

procedure TNetTcpIpServer.NetTcpIpClientsUnlock;
begin
  FNetClients.UnlockList;
end;

procedure TNetTcpIpServer.OnNewIncommingConnection(Sender: TObject; Client: TNetTcpIpClient);
begin
  //
end;

procedure TNetTcpIpServer.OnTcpServerAccept(Sender: TObject; ClientSocket: TTCPBlockSocket);
var
  n: TNetTcpIpClient;
  oldSocket: TTCPBlockSocket;
begin
  n := FNetTcpIpClientClass.Create(nil);
  try
    n.FLock.Acquire;
    try
      oldSocket := n.FTcpBlockSocket;
      n.FTcpBlockSocket := ClientSocket;
      n.FConnected := true;
      n.RemoteHost := ClientSocket.GetRemoteSinIP;
      n.RemotePort := ClientSocket.GetRemoteSinPort;
      ClientSocket.SocksTimeout := 5000; // New 1.5.1
      ClientSocket.ConnectionTimeout := 5000; // New 1.5.1
    finally
      n.FLock.Release;
    end;
    FNetClients.Add(n);
    try
      OnNewIncommingConnection(Sender, n);
    finally
      FNetClients.Remove(n);
    end;
  finally
    n.FTcpBlockSocket := oldSocket;
    FreeAndNil(n);
  end;
end;

procedure TNetTcpIpServer.SetActive(const Value: Boolean);
begin
  if Value then
  begin
    if (Assigned(FTcpIpServer)) then
      exit;
    FTcpIpServer := TTcpIpServerListenerThread.Create(Self);
    FActive := true;
  end
  else
  begin
    if (not Assigned(FTcpIpServer)) then
      exit;
    FActive := False;
    FTcpIpServer.Terminate;
    FTcpIpServer.WaitFor;
    FreeAndNil(FTcpIpServer);
  end;
end;

procedure TNetTcpIpServer.SetNetTcpIpClientClass(const Value: TNetTcpIpClientClass);
begin
  if FNetTcpIpClientClass = Value then
    exit;
  FNetTcpIpClientClass := Value;
  Active := False;
end;

procedure TNetTcpIpServer.SetPort(const Value: Word);
begin
  FPort := Value;
end;

procedure TNetTcpIpServer.WaitUntilNetTcpIpClientsFinalized;
var
  l: TList;
begin
  if Active then
    Active := False;
  repeat
    l := FNetClients.LockList;
    try
      if (l.Count = 0) then
        exit;
    finally
      FNetClients.UnlockList;
    end;
    sleep(10);
  until False;
end;

{ TTcpIpServerListenerThread }

procedure TTcpIpServerListenerThread.BCExecute;
var
  ClientSocket: TSocket;
  ClientThread: TTcpIpSocketThread;
  lSockets: TList;
  i: Integer;
begin
  FServerSocket.CreateSocket;
  if FServerSocket.LastError <> 0 then
  begin
    TLog.NewLog(lterror, Classname, 'Error initializing the socket: ' + FServerSocket.GetErrorDescEx);
    exit;
  end;
  FServerSocket.Family := SF_IP4;
  FServerSocket.SetLinger(true, 10000);
  FServerSocket.Bind('0.0.0.0', inttostr(FNetTcpIpServerServer.Port));
  if FServerSocket.LastError <> 0 then
  begin
    TLog.NewLog(lterror, Classname, 'Cannot bind port ' + inttostr(FNetTcpIpServerServer.Port) + ': ' +
      FServerSocket.GetErrorDescEx);
    exit;
  end;
  FServerSocket.Listen;
  lSockets := TList.Create;
  try
    while (not Terminated) and (FNetTcpIpServerServer.Active) do
    begin
      if (FServerSocket.CanRead(100)) and (lSockets.Count < FNetTcpIpServerServer.MaxConnections) then
      begin
        ClientSocket := FServerSocket.Accept;
        if FServerSocket.LastError = 0 then
        begin
          ClientThread := TTcpIpSocketThread.Create(Self, ClientSocket);
          lSockets.Add(ClientThread);
          ClientThread.Suspended := False;
        end;
      end;
      // Clean finished threads
      for i := lSockets.Count - 1 downto 0 do
      begin
        ClientThread := TTcpIpSocketThread(lSockets[i]);
        if ClientThread.Terminated then
        begin
          lSockets.Delete(i);
          ClientThread.Free;
        end;
      end;
      // Wait
      sleep(10); // Sleep 10 is better than sleep 1
    end;
  finally
    // Finalize all threads
    for i := 0 to lSockets.Count - 1 do
    begin
      TTcpIpSocketThread(lSockets[i]).FListenerThread := nil;
      TTcpIpSocketThread(lSockets[i]).Terminate;
    end;
    // Wait until terminated...
    for i := 0 to lSockets.Count - 1 do
    begin
      TTcpIpSocketThread(lSockets[i]).WaitFor;
      TTcpIpSocketThread(lSockets[i]).Free;
    end;
    lSockets.Free;
  end;
end;

constructor TTcpIpServerListenerThread.Create(ANetTcpIpServer: TNetTcpIpServer);
begin
  FServerSocket := TTCPBlockSocket.Create;
  FNetTcpIpServerServer := ANetTcpIpServer;
  FNetTcpIpServerServer.FTcpIpServer := Self;
  inherited Create(False);
end;

destructor TTcpIpServerListenerThread.Destroy;
begin
  FNetTcpIpServerServer.FTcpIpServer := nil;
  FServerSocket.Free;
  inherited;
end;

{ TTcpIpSocketThread }

procedure TTcpIpSocketThread.BCExecute;
begin
  if (not Terminated) and (Assigned(FSock)) and (Assigned(FListenerThread)) then
    FListenerThread.FNetTcpIpServerServer.OnTcpServerAccept(Self, FSock);
end;

constructor TTcpIpSocketThread.Create(AListenerThread: TTcpIpServerListenerThread; ASocket: TSocket);
begin
  FSock := TTCPBlockSocket.Create;
  FSock.Socket := ASocket;
  FListenerThread := AListenerThread;
  inherited Create(true);
end;

destructor TTcpIpSocketThread.Destroy;
begin
  try
    if FSock.Socket <> INVALID_SOCKET then
    begin
      FSock.CloseSocket;
    end;
  except
    on E: Exception do
    begin
      TLog.NewLog(lterror, Classname, 'Exception closing socket (' + E.Classname + '):' + E.Message);
    end;
  end;
  try
    FreeAndNil(FSock);
  except
    on E: Exception do
    begin
      TLog.NewLog(lterror, Classname, 'Exception destroying socket (' + E.Classname + '):' + E.Message);
    end;
  end;
  inherited;
end;

end.
