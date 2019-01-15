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
| File:       MicroCoin.Net.ConnectionManager.pas                              |
| Created at: 2018-09-14                                                       |
| Purpose:    Net connection manager                                           |
|==============================================================================}
unit MicroCoin.Net.ConnectionManager;

{$ifdef FPC}
  {$mode delphi}
{$endif}

interface

uses Classes, SysUtils, UThread, UCrypto,
  MicroCoin.Net.Statistics,
  MicroCoin.Net.Time, MicroCoin.Net.Protocol,
  MicroCoin.BlockChain.BlockManager,
  MicroCoin.Net.Connection, UTime, MicroCoin.Common.Config,
  ULog, MicroCoin.Net.Events,
  MicroCoin.Net.ConnectionBase,
  MicroCoin.Net.INetNotificationSource, SyncObjs,
  MicroCoin.BlockChain.Blockheader, MicroCoin.Net.NodeServer;

type

  TConnectionManager = class;
  TNetClientsDestroyThread = class;

  TThreadCheckConnections = class(TPCThread)
  private
    FNetData: TConnectionManager;
    FLastCheckTS: Cardinal;
  protected
    procedure BCExecute; override;
  public
    constructor Create(NetData: TConnectionManager);
  end;

  TConnectionManager = class(TComponent, INetNotificationSource)
  private
    class var FNodeServersAddresses: TPCThreadList;
    class var FNetConnections: TPCThreadList;
    class var FDestroying : boolean;
  private
    FMaxNodeServersAddressesBuffer: Integer;
    FNetDataNotifyEventsThread: TNetDataNotifyEventsThread;
    FNodePrivateKey: TECPrivateKey;
    FLastRequestId: Cardinal;
    FRegisteredRequests: TPCThreadList;
    FIsDiscoveringServers: Boolean;
    FIsGettingNewBlockChainFromClient: Boolean;
    FOnNetConnectionsUpdated: TNotifyEvent;
    FOnNodeServersUpdated: TNotifyEvent;
    FOnBlackListUpdated: TNotifyEvent;
    FThreadCheckConnections: TThreadCheckConnections;
    FOnReceivedHelloMessage: TNotifyEvent;
    FNetStatistics: TNetStatistics;
    FOnStatisticsChanged: TNotifyEvent;
    FMaxRemoteOperationBlock: TBlockHeader;
    FFixedServers: TNodeServerAddressArray;
    FNetClientsDestroyThread: TNetClientsDestroyThread;
    FNetConnectionsActive: Boolean;
    FMaxConnections: Integer;
    FNetworkAdjustedTime: TNetworkAdjustedTime;
    class var FLock : TCriticalSection;
    class var FInstance: TConnectionManager;
    class var FIgnoreOldBlocks: boolean;
    class var FStatus: integer;
    class var FOperation: integer;
    procedure SetMaxNodeServersAddressesBuffer(AValue: Integer);
    procedure SetNetConnectionsActive(const Value: Boolean);
    function GetOnNetConnectionsUpdated: TNotifyEvent;
    function GetOnNodeServersUpdated: TNotifyEvent;
    function GetOnBlackListUpdated: TNotifyEvent;
    function GetOnReceivedHelloMessage: TNotifyEvent;
    function GetOnStatisticsChanged: TNotifyEvent;

  strict protected
    class function GetInstance: TConnectionManager; static;
  public
    class function ExtractHeaderInfo(buffer: TStream; var HeaderData: TNetHeaderData; DataBuffer: TStream;
      var IsValidHeaderButNeedMoreData: Boolean): Boolean;
    // Only 1 NetData
    class procedure ReleaseInstance; static;
    class function NetDataExists: Boolean;
    //
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class constructor CreateClass;
    class destructor DestroyClass;
    procedure IncStatistics(incActiveConnections, incClientsConnections, incServersConnections,
      incServersConnectionsWithResponse: Integer; incBytesReceived, incBytesSend: Int64);
    procedure Notification(AComponent: TComponent; operation: TOperation); override;
    function IndexOfNetClient(ListToSearch: TList; ip: AnsiString; port: Word; indexStart: Integer = 0): Integer;
    procedure DeleteNetClient(list: TList; index: Integer);
    procedure DiscoverServersTerminated(Sender: TObject);

    procedure CleanBlackList;
    function Bank: TBlockManager;
    function NewRequestId: Cardinal;
    procedure RegisterRequest(Sender: TNetConnectionBase; operation: Word; request_id: Cardinal);
    function UnRegisterRequest(Sender: TNetConnectionBase; operation: Word; request_id: Cardinal): Boolean;
    function PendingRequest(Sender: TNetConnectionBase; var requests_data: AnsiString): Integer;
    procedure AddServer(NodeServerAddress: TNodeServer);
    function IsBlackListed(const ip: AnsiString; port: Word): Boolean;
    //
    procedure DiscoverFixedServersOnly(const FixedServers: TNodeServerAddressArray);
    //
    function ConnectionsCountAll: Integer;
    function ConnectionsCountServerClients: Integer;
    function ConnectionsCountClients: Integer;
    function GetConnection(index: Integer; var netConnection: TNetConnection): Boolean;
    function ConnectionsCount(CountOnlyNetClients: Boolean): Integer;
    function Connection(index: Integer): TNetConnection;
    function ConnectionExistsAndActive(ObjectPointer: TObject): Boolean;
    function ConnectionExists(ObjectPointer: TObject): Boolean;
    function ConnectionLock(Sender: TObject; ObjectPointer: TObject; MaxWaitMiliseconds: Cardinal): Boolean;
    procedure ConnectionUnlock(ObjectPointer: TObject);
    function FindConnectionByClientRandomValue(Sender: TNetConnection): TNetConnection;
    procedure DiscoverServers;
    procedure DisconnectClients;
    procedure GetNewBlockChainFromClient(Connection: TNetConnection; const why: string);
    function GetValidNodeServers(OnlyWhereIConnected: Boolean; Max: Integer): TNodeServerAddressArray;
    procedure CleanNodeServersList;

    procedure NotifyNetConnectionUpdated;
    class procedure NotifyNodeServersUpdated;
    procedure NotifyBlackListUpdated;
    procedure NotifyReceivedHelloMessage;
    procedure NotifyStatisticsChanged;

    class property NetConnections: TPCThreadList read FNetConnections;
    property NetStatistics: TNetStatistics read FNetStatistics;
    property IsDiscoveringServers: Boolean read FIsDiscoveringServers;
    property IsGettingNewBlockChainFromClient: Boolean read FIsGettingNewBlockChainFromClient;
    property MaxRemoteOperationBlock: TBlockHeader read FMaxRemoteOperationBlock write FMaxRemoteOperationBlock;
    property NodePrivateKey: TECPrivateKey read FNodePrivateKey;

    property OnNetConnectionsUpdated: TNotifyEvent read FOnNetConnectionsUpdated write FOnNetConnectionsUpdated;
    property OnNodeServersUpdated: TNotifyEvent read FOnNodeServersUpdated write FOnNodeServersUpdated;
    property OnBlackListUpdated: TNotifyEvent read FOnBlackListUpdated write FOnBlackListUpdated;
    property OnReceivedHelloMessage: TNotifyEvent read FOnReceivedHelloMessage write FOnReceivedHelloMessage;
    property OnStatisticsChanged: TNotifyEvent read FOnStatisticsChanged write FOnStatisticsChanged;

    property NetConnectionsActive: Boolean read FNetConnectionsActive write SetNetConnectionsActive;
    property NetworkAdjustedTime: TNetworkAdjustedTime read FNetworkAdjustedTime;
    property MaxNodeServersAddressesBuffer: Integer read FMaxNodeServersAddressesBuffer  write SetMaxNodeServersAddressesBuffer;

    property MaxConnections: Integer read FMaxConnections write FMaxConnections;
    class property Instance: TConnectionManager read GetInstance;
    class property NodeServersAddresses: TPCThreadList read FNodeServersAddresses;
    class property Destroying : boolean read FDestroying;
    class property IgnoreOldBlocks : boolean read FIgnoreOldBlocks write FIgnoreOldBlocks;
    class property Operation : integer read FOperation write FOperation;
    class property Status : integer read FStatus write FStatus;
  end;

  TNetClientsDestroyThread = class(TPCThread)
  private
    FTerminatedAllConnections: Boolean;
  protected
    procedure BCExecute; override;
  public
    constructor Create(NetData: TConnectionManager);
    procedure WaitForTerminatedAllConnections;
  end;

implementation

uses MicroCoin.Node.Node, MicroCoin.Net.Utils,
  MicroCoin.Account.AccountKey, MicroCoin.BlockChain.Block,
  MicroCoin.Transaction.TransactionList, MicroCoin.Account.Storage,
  UChunk, MicroCoin.Common.Stream,
  MicroCoin.Net.Client, MicroCoin.Net.Discovery, MicroCoin.Transaction.Hashtree;

function SortNodeServerAddress(Item1, Item2: Pointer): Integer;
var
  P1, P2: PNodeServerAddress;
begin
  P1 := Item1;
  P2 := Item2;
  Result := AnsiCompareText(P1.ip, P2.ip);
  if Result = 0 then
    Result := P1.port - P2.port;
end;

procedure TConnectionManager.AddServer(NodeServerAddress: TNodeServer);
var
  P: PNodeServerAddress;
  i: Integer;
  l: TList;
  currunixtimestamp: Cardinal;
begin
  if trim(NodeServerAddress.ip) = '' then
    exit;
  // Protection against fill with invalid nodes
  currunixtimestamp := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
  // If not connected CT_LAST_CONNECTION_MAX_MINUTES minutes ago...
  if (NodeServerAddress.last_connection_by_server = 0) and (NodeServerAddress.last_connection > 0) and
    ((NodeServerAddress.last_connection + (cLAST_CONNECTION_MAX_MINUTES)) < (currunixtimestamp)) then
    exit;
  // If not connected CT_LAST_CONNECTION_BY_SERVER_MAX_MINUTES minutes ago...
  if (NodeServerAddress.last_connection = 0) and (NodeServerAddress.last_connection_by_server > 0) and
    ((NodeServerAddress.last_connection_by_server + (cLAST_CONNECTION_BY_SERVER_MAX_MINUTES)) < (currunixtimestamp))
  then
    exit;
  if (NodeServerAddress.last_connection_by_server > currunixtimestamp) or
    (NodeServerAddress.last_connection > currunixtimestamp) then
    exit;
  l := FNodeServersAddresses.LockList;
  try
    i := IndexOfNetClient(l, NodeServerAddress.ip, NodeServerAddress.port);
    if i >= 0 then
    begin
      P := PNodeServerAddress(l[i]);
      if NodeServerAddress.last_connection > P^.last_connection then
        P^.last_connection := NodeServerAddress.last_connection;
      if NodeServerAddress.last_connection_by_server > P^.last_connection_by_server then
        P^.last_connection_by_server := NodeServerAddress.last_connection_by_server;
      if NodeServerAddress.last_attempt_to_connect > P^.last_attempt_to_connect then
        P^.last_attempt_to_connect := NodeServerAddress.last_attempt_to_connect;
      exit;
    end;
    if (l.Count >= FMaxNodeServersAddressesBuffer) then
      exit; // In order to prevent fill buffer
    New(P);
    P^ := NodeServerAddress;
    l.Add(P);
    l.Sort(SortNodeServerAddress);
    Inc(FNetStatistics.NodeServersListCount);
    TLog.NewLog(ltdebug, Classname, 'Adding new server: ' + NodeServerAddress.ip + ':' +
      Inttostr(NodeServerAddress.port));
  finally
    FNodeServersAddresses.UnlockList;
  end;
  NotifyNodeServersUpdated;
end;

function TConnectionManager.Bank: TBlockManager;
begin
  Result := TNode.Node.BlockManager;
end;

procedure TConnectionManager.CleanBlackList;
var
  P, Pns: PNodeServerAddress;
  i, n, j: Integer;
  l, lns: TList;
begin
  CleanNodeServersList;
  // This procedure cleans old blacklisted IPs
  n := 0;
  l := FNodeServersAddresses.LockList;
  try
    for i := l.Count - 1 downto 0 do
    begin
      P := l[i];
      // Is an old blacklisted IP? (More than 1 hour)
      if (P^.is_blacklisted) and ((P^.last_connection + (cLAST_CONNECTION_MAX_MINUTES)) <
        (UnivDateTimeToUnix(DateTime2UnivDateTime(now)))) then
      begin
        l.Delete(i);
        P^:=Default(TNodeServer);
        Dispose(P);
        Inc(n);
        dec(FNetStatistics.NodeServersListCount);
        Inc(FNetStatistics.NodeServersDeleted);
      end;
    end;
  finally
    FNodeServersAddresses.UnlockList;
  end;
  if (n > 0) then
    NotifyBlackListUpdated;
end;

procedure TConnectionManager.CleanNodeServersList;
// Will mantain NodeServersAddesses with few nodes in order to keep memory and cpu speed
var
  i, j: Integer;
  nsa: TNodeServer;
  currunixtimestamp: Cardinal;
  l: TList;
  Aux: TNodeServerAddressArray;
begin
  currunixtimestamp := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
  l := FNodeServersAddresses.LockList;
  try
    i := l.Count - 1;
    while (i >= 0) do
    begin
      nsa := PNodeServerAddress(l[i])^;
      if (not(nsa.is_blacklisted)) // Not blacklisted
        and ((nsa.netConnection = nil) // No connection
        or // Connected but a lot of time without data...
        ((Assigned(nsa.netConnection)) and ((nsa.last_connection + (cLAST_CONNECTION_MAX_MINUTES)) <
        currunixtimestamp))) and ((nsa.total_failed_attemps_to_connect > 0) or (
        // I've not connected CT_LAST_CONNECTION_MAX_MINUTES minutes before
        ((nsa.last_connection + (cLAST_CONNECTION_MAX_MINUTES)) < (currunixtimestamp)) and
        // Others have connected CT_LAST_CONNECTION_BY_SERVER_MAX_MINUTES minutes before
        ((nsa.last_connection_by_server + (cLAST_CONNECTION_BY_SERVER_MAX_MINUTES)) < (currunixtimestamp)) and
        ((nsa.last_connection > 0) or (nsa.last_connection_by_server > 0)))) then
      begin
        DeleteNetClient(l, i);
        dec(FNetStatistics.NodeServersListCount);
        Inc(FNetStatistics.NodeServersDeleted);
      end;
      dec(i);
    end;
  finally
    FNodeServersAddresses.UnlockList;
  end;
end;

function TConnectionManager.Connection(index: Integer): TNetConnection;
var
  l: TList;
begin
  l := FNetConnections.LockList;
  try
    Result := TNetConnection(l[index]);
  finally
    FNetConnections.UnlockList;
  end;
end;

function TConnectionManager.ConnectionExists(ObjectPointer: TObject): Boolean;
var
  i: Integer;
  l: TList;
begin
  Result := false;
  l := FNetConnections.LockList;
  try
    for i := 0 to l.Count - 1 do
    begin
      if TObject(l[i]) = ObjectPointer then
      begin
        Result := true;
        exit;
      end;
    end;
  finally
    FNetConnections.UnlockList;
  end;
end;

function TConnectionManager.ConnectionExistsAndActive(ObjectPointer: TObject): Boolean;
var
  i: Integer;
  l: TList;
begin
  Result := false;
  l := FNetConnections.LockList;
  try
    for i := 0 to l.Count - 1 do
    begin
      if TObject(l[i]) = ObjectPointer then
      begin
        Result := (TNetConnection(ObjectPointer).Connected);
        exit;
      end;
    end;
  finally
    FNetConnections.UnlockList;
  end;
end;

function TConnectionManager.ConnectionLock(Sender: TObject; ObjectPointer: TObject;
  MaxWaitMiliseconds: Cardinal): Boolean;
var
  i: Integer;
  l: TList;
  nc: TNetConnection;
  lock: TPCCriticalSection;
begin
  Result := false;
  nc := nil;
  l := FNetConnections.LockList;
  try
    for i := 0 to l.Count - 1 do
    begin
      if (TObject(l[i]) = ObjectPointer) then
      begin
        if (not(TNetConnection(l[i]).DoFinalizeConnection)) and (TNetConnection(l[i]).Connected) then
        begin
          nc := TNetConnection(l[i]);
          exit;
        end
        else
          exit;
      end;
    end;
  finally
    FNetConnections.UnlockList;
    if Assigned(nc) then
    begin
      Result := TPCThread.TryProtectEnterCriticalSection(Sender, MaxWaitMiliseconds, nc.NetLock);
    end;
  end;
end;

function TConnectionManager.ConnectionsCount(CountOnlyNetClients: Boolean): Integer;
var
  i: Integer;
  l: TList;
begin
  l := FNetConnections.LockList;
  try
    if CountOnlyNetClients then
    begin
      Result := 0;
      for i := 0 to l.Count - 1 do
      begin
        if TObject(l[i]) is TNetClient then
          Inc(Result);
      end;
    end
    else
      Result := l.Count;
  finally
    FNetConnections.UnlockList;
  end;
end;

function TConnectionManager.ConnectionsCountAll: Integer;
var
  l: TList;
begin
  l := FNetConnections.LockList;
  try
    Result := l.Count;
  finally
    FNetConnections.UnlockList;
  end;
end;

function TConnectionManager.ConnectionsCountClients: Integer;
var
  l: TList;
  i: Integer;
begin
  Result := 0;
  l := FNetConnections.LockList;
  try
    for i := 0 to l.Count - 1 do
    begin
      if TObject(l[i]) is TNetClient then
        Inc(Result);
    end;
  finally
    FNetConnections.UnlockList;
  end;
end;

function TConnectionManager.ConnectionsCountServerClients: Integer;
var
  l: TList;
  i: Integer;
begin
  Result := 0;
  l := FNetConnections.LockList;
  try
    for i := 0 to l.Count - 1 do
    begin
      if TObject(l[i]) is TNetServerClient then
        Inc(Result);
    end;
  finally
    FNetConnections.UnlockList;
  end;
end;

procedure TConnectionManager.ConnectionUnlock(ObjectPointer: TObject);
var
  i: Integer;
  l: TList;
  nc: TNetConnection;
begin
  l := FNetConnections.LockList;
  try
    for i := 0 to l.Count - 1 do
    begin
      if TObject(l[i]) = ObjectPointer then
      begin
        TNetConnection(l[i]).NetLock.Release;
        exit;
      end;
    end;
  finally
    FNetConnections.UnlockList;
  end;
  try
    nc := (ObjectPointer as TNetConnection);
    if (not Assigned(nc.NetLock)) then
      raise Exception.Create('NetLock object not assigned');
    nc.NetLock.Release;
  except
    on E: Exception do
    begin
      TLog.NewLog(ltError, Classname, 'Error unlocking Object ' + IntToHex(PtrInt(ObjectPointer), 8) + ' Errors (' +
        E.Classname + '): ' + E.Message);
    end;
  end;
  TLog.NewLog(ltdebug, Classname, 'Unlocked a NetLock object out of connections list');
end;

class constructor TConnectionManager.CreateClass;
begin
  FLock := TCriticalSection.Create;
end;

constructor TConnectionManager.Create(AOwner: TComponent);
begin
  inherited;
  TLog.NewLog(ltInfo, Classname, 'TNetData.Create');
  FMaxConnections := cMaximumClients;
  FNetConnectionsActive := true;
  SetLength(FFixedServers, 0);
  FMaxRemoteOperationBlock := TBlockHeader.Empty;
  FNetStatistics := TNetStatistics.Empty;
  FOnStatisticsChanged := nil;
  FOnNetConnectionsUpdated := nil;
  FOnNodeServersUpdated := nil;
  FOnBlackListUpdated := nil;
  FOnReceivedHelloMessage := nil;
  FIsDiscoveringServers := false;
  FRegisteredRequests := TPCThreadList.Create('TNetData_RegisteredRequests');
  FNodeServersAddresses := TPCThreadList.Create('TNetData_NodeServersAddresses');
  FLastRequestId := 0;
  FNetConnections := TPCThreadList.Create('TNetData_NetConnections');
  FIsGettingNewBlockChainFromClient := false;
  FNodePrivateKey := TECPrivateKey.Create;
  FNodePrivateKey.GenerateRandomPrivateKey(cDefault_EC_OpenSSL_NID);
  FThreadCheckConnections := TThreadCheckConnections.Create(Self);
  FNetDataNotifyEventsThread := TNetDataNotifyEventsThread.Create(Self);
  FNetClientsDestroyThread := TNetClientsDestroyThread.Create(Self);
  FNetworkAdjustedTime := TNetworkAdjustedTime.Create;
  FMaxNodeServersAddressesBuffer := (cMAX_NODESERVERS_BUFFER div 2);
end;

procedure TConnectionManager.DeleteNetClient(list: TList; index: Integer);
var
  P: PNodeServerAddress;
begin
  P := list.Items[index];
  list.Delete(index);
  P^ := Default(TNodeServer);
  Dispose(P);
end;

class destructor TConnectionManager.DestroyClass;
begin
//  FreeAndNil(FLock);
end;

destructor TConnectionManager.Destroy;
var
  l: TList;
  i: Integer;
  tdc: TThreadDiscoverConnection;
begin
  FLock.Acquire;
  FDestroying := true;
  FLock.Release;
  TLog.NewLog(ltInfo, Classname, 'TNetData.Destroy START');
  FOnStatisticsChanged := nil;
  FOnNetConnectionsUpdated := nil;
  FOnNodeServersUpdated := nil;
  FOnBlackListUpdated := nil;
  FOnReceivedHelloMessage := nil;

  // First destroy ThreadCheckConnections to prevent a call to "DiscoverServers"
  TLog.NewLog(ltInfo, Classname, 'ThreadCheckConnections terminating...');
  FThreadCheckConnections.FreeOnTerminate := true;
  FThreadCheckConnections.Terminate;
//  FThreadCheckConnections.WaitFor;
//  FreeAndNil(FThreadCheckConnections);


  // Closing connections
  l := FNetConnections.LockList;
  try
    for i := 0 to l.Count - 1 do
    begin
      TNetConnection(l[i]).Connected := false;
      TNetConnection(l[i]).FinalizeConnection;
    end;
  finally
    FNetConnections.UnlockList;
  end;

  // Now finish all DiscoverConnection threads
  repeat
    tdc := TThreadDiscoverConnection(TPCThreadClass.GetThreadByClass(TThreadDiscoverConnection, nil));
    if Assigned(tdc) then
    begin
      tdc.FreeOnTerminate := true;
      if not tdc.Terminated
      then begin begin
        tdc.Terminate;
//        tdc.WaitFor;
//        tdc.Free;
      end;
      end;
      TLog.NewLog(ltInfo, Classname, 'TThreadDiscoverConnection finished');
    end;
  until not Assigned(tdc);
  FNetDataNotifyEventsThread.FreeOnTerminate := true;
  FNetDataNotifyEventsThread.Terminate;
//  FNetDataNotifyEventsThread.WaitFor;
//  FreeAndNil(FNetDataNotifyEventsThread);

  FNetClientsDestroyThread.WaitForTerminatedAllConnections;
  FNetClientsDestroyThread.Terminate;
  FNetClientsDestroyThread.WaitFor;
  FreeAndNil(FNetClientsDestroyThread);

  CleanBlackList;
  l := FNodeServersAddresses.LockList;
  try
    while (l.Count > 0) do
      DeleteNetClient(l, l.Count - 1);
  finally
    FNodeServersAddresses.UnlockList;
    FreeAndNil(FNodeServersAddresses);
  end;
  FreeAndNil(FNetConnections);
  FreeAndNil(FNodePrivateKey);
  SetLength(FFixedServers, 0);
  FreeAndNil(FRegisteredRequests);
  FreeAndNil(FNetworkAdjustedTime);
  inherited;
  TLog.NewLog(ltInfo, Classname, 'TNetData.Destroy END');
  FreeAndNil(FLock);
end;

procedure TConnectionManager.DisconnectClients;
var
  i: Integer;
  l: TList;
begin
  if not Assigned(FNetConnections)
  then exit;
  l := FNetConnections.LockList;
  try
    for i := l.Count - 1 downto 0 do
    begin
      if TObject(l[i]) is TNetClient then
      begin
        TNetClient(l[i]).Connected := false;
        TNetClient(l[i]).FinalizeConnection;
      end;
    end;
  finally
    FNetConnections.UnlockList;
  end;
end;

procedure TConnectionManager.DiscoverFixedServersOnly(const FixedServers: TNodeServerAddressArray);
var
  i: Integer;
  l: TList;
begin
  l := FNodeServersAddresses.LockList;
  try
    SetLength(FFixedServers, length(FixedServers));
    for i := low(FixedServers) to high(FixedServers) do
    begin
      FFixedServers[i] := FixedServers[i];
    end;
    for i := low(FixedServers) to high(FixedServers) do
    begin
      AddServer(FixedServers[i]);
    end;
  finally
    FNodeServersAddresses.UnlockList;
  end;
end;

procedure TConnectionManager.DiscoverServers;
  procedure sw(l: TList);
  var
    i, j, x, y: Integer;
  begin
    if l.Count <= 1 then
      exit;
    j := Random(l.Count) * 3;
    for i := 0 to j do
    begin
      x := Random(l.Count);
      y := Random(l.Count);
      if x <> y then
        l.Exchange(x, y);
    end;
  end;

var
  P: PNodeServerAddress;
  i, j, k: Integer;
  l, lns: TList;
  tdc: TThreadDiscoverConnection;
  canAdd: Boolean;
begin
  if not FNetConnectionsActive then
    exit;
  if TPCThread.ThreadClassFound(TThreadDiscoverConnection, nil) >= 0 then
  begin
    TLog.NewLog(ltInfo, Classname, 'Already discovering servers...');
    exit;
  end;
  CleanBlackList;
  if NetStatistics.ClientsConnections > 0 then
  begin
    j := cMinimumServersNeeded - NetStatistics.ServersConnectionsWithResponse;
  end
  else
  begin
    j := cMaximumNumberOfServers - NetStatistics.ServersConnectionsWithResponse;
  end;
  if j <= 0 then
    exit;
{$IFDEF HIGHLOG}TLog.NewLog(ltdebug, Classname, 'Discover servers start process searching up to ' + Inttostr(j) + ' servers'); {$ENDIF}
  // can discover up to j servers
  l := TList.Create;
  try
    lns := FNodeServersAddresses.LockList;
    try
      for i := 0 to lns.Count - 1 do
      begin
        P := lns[i];
        if (not Assigned(P.netConnection)) and (not IsBlackListed(P^.ip, P^.port)) and (not P^.its_myself) and
          ((P^.last_attempt_to_connect = 0) or ((P^.last_attempt_to_connect + EncodeTime(0, 3, 0, 0) < now))) and
          ((P^.total_failed_attemps_to_connect < 3) or (P^.last_attempt_to_connect + EncodeTime(0, 10, 0, 0) < now))
        then
        begin

          if length(FFixedServers) > 0 then
          begin
            canAdd := false;
            for k := low(FFixedServers) to high(FFixedServers) do
            begin
              if (FFixedServers[k].ip = P^.ip) and ((FFixedServers[k].port = P.port)) then
              begin
                canAdd := true;
                break;
              end;
            end;
          end
          else
            canAdd := true;
          if canAdd then
            l.Add(P);
        end;
      end;
      if l.Count <= 0 then
        exit;
      sw(l);
      if j >= l.Count then
        j := l.Count - 1;
      TLog.NewLog(ltdebug, Classname, 'Start discovering up to ' + Inttostr(j + 1) + ' servers... (max:' +
        Inttostr(l.Count) + ')');
      //
      for i := 0 to j do
      begin
        FIsDiscoveringServers := true;
        P := PNodeServerAddress(l[i]);
        tdc := TThreadDiscoverConnection.Create(P^, DiscoverServersTerminated);
      end;
    finally
      FNodeServersAddresses.UnlockList;
    end;
  finally
    l.Free;
  end;
end;

procedure TConnectionManager.DiscoverServersTerminated(Sender: TObject);
begin
  if Destroying then exit;

  NotifyNodeServersUpdated;
  if TPCThread.ThreadClassFound(TThreadDiscoverConnection, nil) >= 0 then
    exit;
  FIsDiscoveringServers := false;
  // If here, discover servers finished, so we can try to get/receive data
  TLog.NewLog(ltdebug, Classname,
    Format('Discovering servers finished. Now we have %d active connections and %d connections to other servers',
    [ConnectionsCount(false), ConnectionsCount(true)]));
  if TPCThread.ThreadClassFound(TThreadGetNewBlockChainFromClient, nil) >= 0 then
    exit;
  TThreadGetNewBlockChainFromClient.Create(false).FreeOnTerminate := true;
end;

class function TConnectionManager.ExtractHeaderInfo(buffer: TStream; var HeaderData: TNetHeaderData;
  DataBuffer: TStream; var IsValidHeaderButNeedMoreData: Boolean): Boolean;
var
  lastp: Integer;
  c: Cardinal;
  w: Word;
begin
  HeaderData := TNetHeaderData.Empty;
  Result := false;
  IsValidHeaderButNeedMoreData := false;
  lastp := buffer.Position;
  try
    if buffer.Size - buffer.Position < 22 then
      exit;
    buffer.Read(c, 4);
    if (c <> cMagicNetIdentification) then
      exit;
    buffer.Read(w, 2);
    case w of
      cMagicRequest:
        HeaderData.HeaderType := ntp_request;
      cMagicResponse:
        HeaderData.HeaderType := ntp_response;
      cMagicAutoSend:
        HeaderData.HeaderType := ntp_autosend;
    else
      HeaderData.HeaderType := ntp_unknown;
      exit;
    end;
    buffer.Read(HeaderData.Operation, 2);
    buffer.Read(HeaderData.ErrorCode, 2);
    buffer.Read(HeaderData.RequestId, 4);
    buffer.Read(HeaderData.Protocol.protocol_version, 2);
    buffer.Read(HeaderData.Protocol.protocol_available, 2);
    buffer.Read(c, 4);
    HeaderData.BufferDataLength := c;
    DataBuffer.Size := 0;
    if buffer.Size - buffer.Position < c then
    begin
      IsValidHeaderButNeedMoreData := true;
{$IFDEF HIGHLOG}
      TLog.NewLog(ltdebug, Classname, Format('Need more data! Buffer size (%d) - position (%d) < %d - Header info: %s',
        [buffer.Size, buffer.Position, c, HeaderDataToText(HeaderData)]));
{$ENDIF}
      exit;
    end;
    DataBuffer.CopyFrom(buffer, c);
    DataBuffer.Position := 0;
    //
    if HeaderData.HeaderType = ntp_response then
    begin
      HeaderData.IsError := HeaderData.ErrorCode <> 0;
      if HeaderData.IsError then
      begin
        DataBuffer.ReadAnsiString(HeaderData.ErrorText);
      end;
    end
    else
    begin
      HeaderData.IsError := HeaderData.ErrorCode <> 0;
      if HeaderData.IsError then
      begin
        DataBuffer.ReadAnsiString(HeaderData.ErrorText);
      end;
    end;
    if (HeaderData.IsError) then
    begin
      TLog.NewLog(ltError, Classname, 'Response with error (' + IntToHex(HeaderData.ErrorCode, 4) + '): ' +
        HeaderData.ErrorText + ' ...on ' + 'operation: ' + HeaderData.OperationTxt + ' id: ' +
        Inttostr(HeaderData.RequestId));
    end;
    Result := true;
  finally
    if not Result then
      buffer.Position := lastp;
  end;
end;

function TConnectionManager.FindConnectionByClientRandomValue(Sender: TNetConnection): TNetConnection;
var
  l: TList;
  i: Integer;
begin
  l := FNetConnections.LockList;
  try
    for i := 0 to l.Count - 1 do
    begin
      Result := TNetConnection(l[i]);
      if TAccountKey.EqualAccountKeys(Result.ClientPublicKey, Sender.ClientPublicKey) and (Sender <> Result) then
        exit;
    end;
  finally
    FNetConnections.UnlockList;
  end;
  Result := nil;
end;

class procedure TConnectionManager.ReleaseInstance;
begin
  if Assigned(FInstance)
  then FreeAndNil(FInstance);
end;

function TConnectionManager.GetConnection(index: Integer; var netConnection: TNetConnection): Boolean;
var
  l: TList;
begin
  Result := false;
  netConnection := nil;
  l := FNetConnections.LockList;
  try
    if (index >= 0) and (index < l.Count) then
    begin
      netConnection := TNetConnection(l[index]);
      Result := true;
      exit;
    end;
  finally
    FNetConnections.UnlockList;
  end;
end;

// TODO!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// Spaghetti code
// Move to other class!
procedure TConnectionManager.GetNewBlockChainFromClient(Connection: TNetConnection; const why: string);
const
  CT_LogSender = 'GetNewBlockChainFromClient';

  function Do_GetOperationsBlock(AssignToBank: TBlockManager; block_start, block_end, MaxWaitMilliseconds: Cardinal;
    OnlyOperationBlock: Boolean; BlocksList: TList): Boolean;
  var
    xSendData, xReceiveData: TMemoryStream;
    xHeaderData: TNetHeaderData;
    xBlock: TBlock;
    xRequestId, xCount, i: Cardinal;
    xErrors: AnsiString;
    xNetOperationNumber: Integer;
  begin
    Result := false;
    BlocksList.Clear;
    if (Connection.RemoteOperationBlock.Block < block_end) then
      block_end := Connection.RemoteOperationBlock.Block;
    // First receive operations from
    xSendData := TMemoryStream.Create;
    xReceiveData := TMemoryStream.Create;
    try
      if OnlyOperationBlock then
      begin
        xNetOperationNumber := cNetOp_GetOperationsBlock;
      end
      else
      begin
        xNetOperationNumber := cNetOp_GetBlocks;
      end;
      TLog.NewLog(ltdebug, CT_LogSender, Format('Sending %d from block %d to %d (Total: %d)',
        [xNetOperationNumber, block_start, block_end, block_end - block_start + 1]));
      xSendData.Write(block_start, 4);
      xSendData.Write(block_end, 4);
      xRequestId := TConnectionManager.Instance.NewRequestId;
      if Connection.DoSendAndWaitForResponse(xNetOperationNumber, xRequestId, xSendData, xReceiveData, MaxWaitMilliseconds,
        xHeaderData) then
      begin
        if xHeaderData.IsError then
          exit;
        if xReceiveData.Read(xCount, 4) < 4 then
          exit; // Error in data
        i := 0;
        while (i < xCount) do
        begin
          // decode data
          xBlock := TBlock.Create(AssignToBank);
          if xBlock.LoadBlockFromStream(xReceiveData, xErrors) then
          begin
            BlocksList.Add(xBlock);
          end
          else
          begin
            TLog.NewLog(ltError, CT_LogSender, Format('Error reading OperationBlock from received stream %d/%d: %s',
              [i + 1, xCount, xErrors]));
            xBlock.Free;
            break;
          end;
          Inc(i);
        end;
        Result := true;
      end
      else
      begin
        TLog.NewLog(ltError, CT_LogSender, Format('No received response after waiting %d request id %d operation %d',
          [MaxWaitMilliseconds, xRequestId, (xNetOperationNumber)]));
      end;
    finally
      xSendData.Free;
      xReceiveData.Free;
    end;
  end;

  function Do_GetOperationBlock(Block, MaxWaitMilliseconds: Cardinal; var OperationBlock: TBlockHeader): Boolean;
  var
    BlocksList: TList;
    i: Integer;
  begin
    OperationBlock := TBlockHeader.Empty;
    BlocksList := TList.Create;
    try
      Result := Do_GetOperationsBlock(TNode.Node.BlockManager, Block, Block, MaxWaitMilliseconds, true, BlocksList);
      if (Result) and (BlocksList.Count = 1) then
      begin
        OperationBlock := TBlock(BlocksList[0]).BlockHeader;
      end;
    finally
      for i := 0 to BlocksList.Count - 1 do
        TBlock(BlocksList[i]).Free;
      BlocksList.Free;
    end;
  end;

  function FindLastSameBlockByOperationsBlock(min, Max: Cardinal; var OperationBlock: TBlockHeader): Boolean;
  var
    i: Integer;
    ant_nblock: Int64;
    auxBlock, sbBlock: TBlockHeader;
    distinctmax, distinctmin: Cardinal;
    BlocksList: TList;
  begin
    Result := false;
    OperationBlock := TBlockHeader.Empty;
    repeat
      BlocksList := TList.Create;
      try
        if not Do_GetOperationsBlock(nil, min, Max, 5000, true, BlocksList) then
          exit;
        distinctmin := min;
        distinctmax := Max;
        ant_nblock := -1;
        for i := 0 to BlocksList.Count - 1 do
        begin
          auxBlock := TBlock(BlocksList[i]).BlockHeader;
          // Protection of invalid clients:
          if (auxBlock.Block < min) or (auxBlock.Block > Max) or (auxBlock.Block = ant_nblock) then
          begin
            Connection.DisconnectInvalidClient(false, 'Invalid response... ' + Inttostr(min) + '<' +
              Inttostr(auxBlock.Block) + '<' + Inttostr(Max) + ' ant:' + Inttostr(ant_nblock));
            exit;
          end;
          ant_nblock := auxBlock.Block;
          //
          sbBlock := TNode.Node.BlockManager.AccountStorage.Blocks[auxBlock.Block].Blockheader;
          if TBlock.Equals(sbBlock, auxBlock) then
          begin
            distinctmin := auxBlock.Block;
            OperationBlock := auxBlock;
          end
          else
          begin
            if auxBlock.Block <= distinctmax then
              distinctmax := auxBlock.Block - 1;
          end;
        end;
        min := distinctmin;
        Max := distinctmax;
      finally
        for i := 0 to BlocksList.Count - 1 do
        begin
          TBlock(BlocksList[i]).Free;
        end;
        BlocksList.Free;
      end;
    until (distinctmin = distinctmax);
    Result := (OperationBlock.proof_of_work <> TBlockHeader.Empty.proof_of_work);
  end;

  function GetNewBank(start_block: Int64): Boolean;
  var
    BlocksList: TList;
    i: Integer;
    tempfolder: AnsiString;
    OpComp, OpExecute: TBlock;
    oldBlockchainOperations: TTransactionHashTree;
    opsResume: TTransactionList;
    NewBlock: TAccountStorageEntry;
    errors: AnsiString;
    start, start_c: Cardinal;
    finished: Boolean;
    Bank: TBlockManager;
    ms: TMemoryStream;
    IsAScam: Boolean;
  begin
    IsAScam := false;
    TLog.NewLog(ltdebug, CT_LogSender, Format('GetNewBank(new_start_block:%d)', [start_block]));
    Bank := TBlockManager.Create(nil);
    try
      Bank.StorageClass := TNode.Node.BlockManager.StorageClass;
      Bank.Storage.Orphan := TNode.Node.BlockManager.Storage.Orphan;
      Bank.Storage.ReadOnly := true;
      Bank.Storage.CopyConfiguration(TNode.Node.BlockManager.Storage);
      if start_block >= 0 then
      begin
        // Restore a part
        Bank.DiskRestoreFromTransactions(start_block - 1);
        start := start_block;
      end
      else
      begin
        start := 0;
        start_block := 0;
      end;
      start_c := start;
      Bank.Storage.Orphan := FormatDateTime('yyyymmddhhnnss', DateTime2UnivDateTime(now));
      Bank.Storage.ReadOnly := false;
      // Receive new blocks:
      finished := false;
      repeat
        BlocksList := TList.Create;
        try
          finished := not Do_GetOperationsBlock(Bank, start, start + 50, 30000, false, BlocksList);
          i := 0;
          while (i < BlocksList.Count) and (not finished) do
          begin
            OpComp := TBlock(BlocksList[i]);
            ms := TMemoryStream.Create;
            OpExecute := TBlock.Create(Bank);
            try
              ms.SetSize(4*1024);
              OpComp.SaveBlockToStream(false, ms);
              ms.SetSize(ms.Position);
              ms.Position := 0;
              OpExecute.LoadBlockFromStream(ms, errors);
              if Bank.AddNewBlockToBlockChain(OpExecute,
                TConnectionManager.Instance.NetworkAdjustedTime.GetMaxAllowedTimestampForNewBlock, NewBlock, errors)
              then
              begin
                Inc(i);
              end
              else
              begin
                TLog.NewLog(ltError, CT_LogSender, 'Error creating new bank with client Operations. Block:' +
                  OpExecute.BlockHeader.ToString() + ' Error:' + errors);
                // Add to blacklist !
                Connection.DisconnectInvalidClient(false, 'Invalid BlockChain on Block ' +
                  OpExecute.BlockHeader.ToString() + ' with errors:' + errors);
                finished := true;
                IsAScam := true;
                break;
              end;
            finally
              ms.Free;
              OpExecute.Free;
            end;
          end;
        finally
          for i := 0 to BlocksList.Count - 1 do
            TBlock(BlocksList[i]).Free;
          BlocksList.Free;
        end;
        start := Bank.BlocksCount;
      until (Bank.BlocksCount = Connection.RemoteOperationBlock.Block + 1) or (finished);
      // New Build 1.5 more work vs more high
      // work = SUM(target) of all previous blocks (Int64)
      // -----------------------------
      // Before of version 1.5 was: "if Bank.BlocksCount>TNode.Node.Bank.BlocksCount then ..."
      // Starting on version 1.5 is: "if Bank.WORK > MyBank.WORK then ..."
      if Bank.AccountStorage.WorkSum > TNode.Node.BlockManager.AccountStorage.WorkSum then
      begin
        oldBlockchainOperations := TTransactionHashTree.Create;
        try
          TNode.Node.DisableNewBlocks;
          try
            // I'm an orphan blockchain...
            TLog.NewLog(ltInfo, CT_LogSender, 'New valid blockchain found. My block count=' +
              Inttostr(TNode.Node.BlockManager.BlocksCount) + ' work: ' +
              Inttostr(TNode.Node.BlockManager.AccountStorage.WorkSum) + ' found count=' + Inttostr(Bank.BlocksCount) +
              ' work: ' + Inttostr(Bank.AccountStorage.WorkSum) + ' starting at block ' + Inttostr(start_block));
            if TNode.Node.BlockManager.BlocksCount > 0 then
            begin
              OpExecute := TBlock.Create(nil);
              try
                for start := start_c to TNode.Node.BlockManager.BlocksCount - 1 do
                begin
                  if TNode.Node.BlockManager.LoadTransactions(OpExecute, start) then
                  begin
                    for i := 0 to OpExecute.TransactionCount - 1 do
                    begin
                      // TODO: NEED TO EXCLUDE OPERATIONS ALREADY INCLUDED IN BLOCKCHAIN?
                      oldBlockchainOperations.AddTransactionToHashTree(OpExecute.Transaction[i]);
                    end;
                    TLog.NewLog(ltInfo, CT_LogSender, 'Recovered ' + Inttostr(OpExecute.TransactionCount) +
                      ' operations from block ' + Inttostr(start));
                  end
                  else
                  begin
                    TLog.NewLog(ltError, CT_LogSender, 'Fatal error: Cannot read block ' + Inttostr(start));
                  end;
                end;
              finally
                OpExecute.Free;
              end;
            end;
            TNode.Node.BlockManager.Storage.MoveBlockChainBlocks(start_block,
              Inttostr(start_block) + '_' + FormatDateTime('yyyymmddhhnnss', DateTime2UnivDateTime(now)), nil);
            Bank.Storage.MoveBlockChainBlocks(start_block, TNode.Node.BlockManager.Storage.Orphan,
              TNode.Node.BlockManager.Storage);
            TNode.Node.BlockManager.DiskRestoreFromTransactions(cMaxBlocks);
          finally
            TNode.Node.EnableNewBlocks;
          end;
          // Finally add new operations:
          // Rescue old operations from old blockchain to new blockchain
          if oldBlockchainOperations.TransactionCount > 0 then
          begin
            TLog.NewLog(ltInfo, CT_LogSender, Format('Executing %d operations from block %d to %d',
              [oldBlockchainOperations.TransactionCount, start_c, TNode.Node.BlockManager.BlocksCount - 1]));
            opsResume := TTransactionList.Create;
            try
              // Re-add orphaned operations back into the pending pool.
              // NIL is passed as senderConnection since localnode is considered
              // the origin, and current sender needs these operations.
              i := TNode.Node.AddOperations(nil, oldBlockchainOperations, opsResume, errors);
              TLog.NewLog(ltInfo, CT_LogSender, Format('Executed %d/%d operations. Returned errors: %s',
                [i, oldBlockchainOperations.TransactionCount, errors]));
            finally
              opsResume.Free;
            end;
          end
          else
            TLog.NewLog(ltInfo, CT_LogSender, Format('No operations from block %d to %d',
              [start_c, TNode.Node.BlockManager.BlocksCount - 1]));
        finally
          oldBlockchainOperations.Free;
        end;
      end
      else
      begin
        if (not IsAScam) and (Connection.RemoteAccumulatedWork > TNode.Node.BlockManager.AccountStorage.WorkSum) then
        begin
          // Possible scammer!
          Connection.DisconnectInvalidClient(false,
            Format('Possible scammer! Says blocks:%d Work:%d - Obtained blocks:%d work:%d',
            [Connection.RemoteOperationBlock.Block + 1, Connection.RemoteAccumulatedWork, Bank.BlocksCount,
            Bank.AccountStorage.WorkSum]));
        end;
      end;
    finally
      Bank.Free;
    end;
  end;

  function DownloadAccountStorageChunk(ABlockscount: Cardinal; const AHeader: TRawBytes; AFromBlock, AToblock: Cardinal;
    AReceivedDataUnzipped: TStream; var AAccountStorageHeader: TAccountStorageHeader; var RErrors: AnsiString): Boolean;
  var
    SendData, ReceiveData: TStream;
    HeaderData: TNetHeaderData;
    request_id: Cardinal;
    c: Cardinal;
  begin
    Result := false;
    SendData := TMemoryStream.Create;
    ReceiveData := TMemoryStream.Create;
    try
      SendData.Write(ABlockscount, SizeOf(ABlockscount)); // 4 bytes for blockcount
      SendData.WriteAnsiString(AHeader);
      SendData.Write(AFromBlock, SizeOf(AFromBlock));
      c := AToblock;
      if (c >= ABlockscount) then
        c := ABlockscount - 1;
      SendData.Write(c, SizeOf(c));
      if (AFromBlock > c) or (c >= ABlockscount) then
      begin
        RErrors := 'ERROR DEV 20170727-1';
        exit;
      end;
      TLog.NewLog(ltdebug, CT_LogSender, Format('Call to GetSafeBox from blocks %d to %d of %d',
        [AFromBlock, c, ABlockscount]));
      request_id := TConnectionManager.Instance.NewRequestId;
      if Connection.DoSendAndWaitForResponse(cNetOp_GetAccountStorage, request_id, SendData, ReceiveData, 30000, HeaderData)
      then
      begin
        if HeaderData.IsError then
          exit;
        AReceivedDataUnzipped.Size := 0;
        if not TPCChunk.LoadAccountStorageFromChunk(ReceiveData, AReceivedDataUnzipped, AAccountStorageHeader, RErrors) then
        begin
          Connection.DisconnectInvalidClient(false, 'Invalid received chunk: ' + RErrors);
          exit;
        end;
        if (AAccountStorageHeader.AccountStorageHash <> AHeader) or (AAccountStorageHeader.StartBlock <> AFromBlock) or
          (AAccountStorageHeader.EndBlock <> c) or (AAccountStorageHeader.BlocksCount <> ABlockscount) or
          (AAccountStorageHeader.Protocol < cPROTOCOL_2) or (AAccountStorageHeader.Protocol > cBlockChain_Protocol_Available) then
        begin
          RErrors := Format
            ('Invalid received chunk based on call: Blockscount:%d %d - from:%d %d to %d %d - SafeboxHash:%s %s',
            [AAccountStorageHeader.BlocksCount, ABlockscount, AAccountStorageHeader.StartBlock, AFromBlock,
            AAccountStorageHeader.EndBlock, c, TCrypto.ToHexaString(AAccountStorageHeader.AccountStorageHash),
            TCrypto.ToHexaString(AHeader)]);
          Connection.DisconnectInvalidClient(false, 'Invalid received chunk: ' + RErrors);
          exit;
        end;
        Result := true;
      end
      else
        RErrors := 'No response on DownloadSafeBoxChunk';
    finally
      ReceiveData.Free;
      SendData.Free;
    end;
  end;

type
  TAccountStorageChunkData = record
    AccountStorageHeader: TAccountStorageHeader;
    ChunkStream: TStream;
  end;

  function DownloadAccountStorage(AIsMyBlockchainValid: Boolean): Boolean;
  var
    x_blockcount, xRequestId: Cardinal;
    xReceiveData, xReceiveChunk, xChunk1: TStream;
    xBlockHeader: TBlockHeader;
    xAccountStorageHeader: TAccountStorageHeader;
    xErrors: AnsiString;
    xChunks: array of TAccountStorageChunkData;
    i: Integer;
  begin
    Result := false;
    // Will try to download penultimate saved safebox
    x_blockcount := ((Connection.RemoteOperationBlock.Block div cSaveAccountStorageOnBlocks) - 1) *
      cSaveAccountStorageOnBlocks;
    if not Do_GetOperationBlock(x_blockcount, 5000, xBlockHeader) then
    begin
      Connection.DisconnectInvalidClient(false, Format('Cannot obtain operation block %d for downloading safebox',
        [x_blockcount]));
      exit;
    end;
    xReceiveData := TMemoryStream.Create;
    try
      SetLength(xChunks, 0);
      try
        // Will obtain chunks of 10000 blocks each
        for i := 0 to x_blockcount div 10000 do
        begin
          FOperation := 1;
          FStatus := ((100*i) div (x_blockcount div 10000));
          xReceiveChunk := TMemoryStream.Create;
          if (not DownloadAccountStorageChunk(x_blockcount, xBlockHeader.initial_safe_box_hash, (i * 10000), ((i + 1) * 10000) - 1,
            xReceiveChunk, xAccountStorageHeader, xErrors)) then
          begin
            xReceiveChunk.Free;
            TLog.NewLog(ltError, CT_LogSender, xErrors);
            exit;
          end;
          SetLength(xChunks, length(xChunks) + 1);
          xChunks[high(xChunks)].AccountStorageHeader := xAccountStorageHeader;
          xChunks[high(xChunks)].ChunkStream := xReceiveChunk;
        end;
        // Will concat safeboxs:
        xChunk1 := TMemoryStream.Create;
        try
          xChunk1.CopyFrom(xChunks[0].ChunkStream, 0);
          for i := 1 to high(xChunks) do
          begin
            FOperation := 2;
            FStatus := ((i*100) div high(xChunks));
            xReceiveData.Size := 0;
            xChunk1.Position := 0;
            xChunks[i].ChunkStream.Position := 0;
            if not TAccountStorage.ConcatStream(xChunk1, xChunks[i].ChunkStream, xReceiveData, xErrors) then
            begin
              TLog.NewLog(ltError, CT_LogSender, xErrors);
              exit;
            end;
            xChunk1.Size := 0;
            xChunk1.CopyFrom(xReceiveData, 0);
          end;
        finally
          xChunk1.Free;
        end;
      finally
        for i := 0 to high(xChunks) do
        begin
          xChunks[i].ChunkStream.Free;
        end;
        SetLength(xChunks, 0);
      end;
      // Now receiveData is the ALL safebox
      TNode.Node.DisableNewBlocks;
      try
        TNode.Node.BlockManager.AccountStorage.StartThreadSafe;
        try
          xReceiveData.Position := 0;
          FOperation := 3;
          FStatus := 0;
          if TNode.Node.BlockManager.LoadAccountsFromStream(xReceiveData, true, xErrors) then
          begin
            FOperation := 0;
            TLog.NewLog(ltInfo, Classname, 'Received new safebox!');
            if not AIsMyBlockchainValid then
            begin
              TNode.Node.BlockManager.Storage.EraseStorage;
            end;
            Connection.Send_GetBlocks(TNode.Node.BlockManager.BlocksCount, 100, xRequestId);
            Result := true;
          end
          else
          begin
            Connection.DisconnectInvalidClient(false, 'Cannot load from stream! ' + xErrors);
            exit;
          end;
        finally
          TNode.Node.BlockManager.AccountStorage.EndThreadSave;
        end;
      finally
        TNode.Node.EnableNewBlocks;
      end;
    finally
      xReceiveData.Free;
    end;
  end;

var
  rid: Cardinal;
  xMyBlockHeader, xClientBlockHeader: TBlockHeader;
begin
  // Protection against discovering servers...
  if FIsDiscoveringServers then
  begin
    TLog.NewLog(ltdebug, CT_LogSender, 'Is discovering servers...');
    exit;
  end;
  //
  if FIsGettingNewBlockChainFromClient then
  begin
    TLog.NewLog(ltdebug, CT_LogSender, 'Is getting new blockchain from client...');
    exit;
  end
  else
    TLog.NewLog(ltdebug, CT_LogSender, 'Starting receiving: ' + why);
  try
    FIsGettingNewBlockChainFromClient := true;
    FMaxRemoteOperationBlock := Connection.RemoteOperationBlock;
    if TNode.Node.BlockManager.BlocksCount = 0 then
    begin
      TLog.NewLog(ltdebug, CT_LogSender, 'I have no blocks');
      if Connection.RemoteOperationBlock.protocol_version >= cPROTOCOL_2 then
      begin
        if IgnoreOldBlocks then begin
          DownloadAccountStorage(true);
        end else begin
         Connection.Send_GetBlocks(0, 10, rid);
        end;
      end
      else
      begin
        Connection.Send_GetBlocks(0, 10, rid);
      end;
      exit;
    end;
    TLog.NewLog(ltdebug, CT_LogSender, 'Starting GetNewBlockChainFromClient at client:' + Connection.ClientRemoteAddr +
      ' with OperationBlock:' + Connection.RemoteOperationBlock.ToString() + ' (My block: ' +
      TNode.Node.BlockManager.LastBlock.ToString() + ')');
    // NOTE: FRemoteOperationBlock.block >= TNode.Node.Bank.BlocksCount
    // First capture same block than me (TNode.Node.Bank.BlocksCount-1) to check if i'm an orphan block...
    xMyBlockHeader := TNode.Node.BlockManager.LastBlock;
    if not Do_GetOperationBlock(xMyBlockHeader.Block, 5000, xClientBlockHeader) then
    begin
      TLog.NewLog(ltError, CT_LogSender, 'Cannot receive information about my block (' + Inttostr(xMyBlockHeader.Block)
        + ')...');
      // Disabled at Build 1.0.6 >  Connection.DisconnectInvalidClient(false,'Cannot receive information about my block ('+inttostr(my_op.block)+')... Invalid client. Disconnecting');
      exit;
    end;

    if (not TBlock.Equals(xMyBlockHeader, xClientBlockHeader)) then
    begin
      TLog.NewLog(ltInfo, CT_LogSender, 'My blockchain is not equal... received: ' +
        xClientBlockHeader.ToString() + ' My: ' + xMyBlockHeader.ToString());
      if not FindLastSameBlockByOperationsBlock(0, xClientBlockHeader.Block, xClientBlockHeader) then
      begin
        TLog.NewLog(ltInfo, CT_LogSender, 'No found base block to start process... Receiving ALL');
        if (Connection.RemoteOperationBlock.protocol_version >= cPROTOCOL_2) then
        begin
          DownloadAccountStorage(false);
        end
        else
        begin
          GetNewBank(-1);
        end;
      end
      else
      begin
        TLog.NewLog(ltInfo, CT_LogSender, 'Found base new block: ' + xClientBlockHeader.ToString());
        // Move operations to orphan folder... (temporal... waiting for a confirmation)
        GetNewBank(xClientBlockHeader.Block + 1);
      end;
    end
    else
    begin
      TLog.NewLog(ltInfo, CT_LogSender, 'My blockchain is ok! Need to download new blocks starting at ' +
        Inttostr(xMyBlockHeader.Block + 1));
      // High to new value:
      Connection.Send_GetBlocks(xMyBlockHeader.Block + 1, 100, rid);
    end;
  finally
    TLog.NewLog(ltdebug, CT_LogSender, 'Finalizing');
    FIsGettingNewBlockChainFromClient := false;
  end;
end;

function TConnectionManager.GetOnBlackListUpdated: TNotifyEvent;
begin
  Result := FOnBlackListUpdated;
end;

function TConnectionManager.GetOnNetConnectionsUpdated: TNotifyEvent;
begin
  Result := FOnNetConnectionsUpdated;
end;

function TConnectionManager.GetOnNodeServersUpdated: TNotifyEvent;
begin
  Result := FOnNodeServersUpdated;
end;

function TConnectionManager.GetOnReceivedHelloMessage: TNotifyEvent;
begin
  Result := FOnReceivedHelloMessage;
end;

function TConnectionManager.GetOnStatisticsChanged: TNotifyEvent;
begin
  Result := FOnStatisticsChanged;
end;

function TConnectionManager.GetValidNodeServers(OnlyWhereIConnected: Boolean; Max: Integer): TNodeServerAddressArray;
var
  i, j: Integer;
  nsa: TNodeServer;
  currunixtimestamp: Cardinal;
  l: TList;
  Aux: TNodeServerAddressArray;
begin
  SetLength(Result, 0);
  SetLength(Aux, 0);
  currunixtimestamp := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
  CleanNodeServersList;
  // Save other node servers
  l := FNodeServersAddresses.LockList;
  try
    for i := 0 to l.Count - 1 do
    begin
      nsa := PNodeServerAddress(l[i])^;
      if (not IsBlackListed(nsa.ip, 0)) and ( // I've connected 1h before
        ((nsa.last_connection > 0) and ((Assigned(nsa.netConnection)) or
        ((nsa.last_connection + (cLAST_CONNECTION_MAX_MINUTES)) > (currunixtimestamp)))) or
        // Others have connected 3h before
        ((nsa.last_connection_by_server > 0) and
        ((nsa.last_connection_by_server + (cLAST_CONNECTION_BY_SERVER_MAX_MINUTES)) > (currunixtimestamp))) or
        // Peer cache
        ((nsa.last_connection = 0) and (nsa.last_connection_by_server = 0))) and (
        // Never tried to connect or successfully connected
        (nsa.total_failed_attemps_to_connect = 0)) and ((not OnlyWhereIConnected) or (nsa.last_connection > 0)) then
      begin
        SetLength(Aux, length(Aux) + 1);
        Aux[high(Aux)] := nsa;
      end;
    end;
  finally
    FNodeServersAddresses.UnlockList;
  end;
  if (Max <= 0) or (length(Aux) < Max) then
  begin
    Result := Aux;
  end
  else
  begin
    for i := 1 to Max do
    begin
      j := Random(length(Aux));
      if Aux[j].ip <> '' then
      begin
        SetLength(Result, length(Result) + 1);
        Result[high(Result)] := Aux[j];
        Aux[j].ip := '';
      end;
    end;
  end;
end;

procedure TConnectionManager.IncStatistics(incActiveConnections, incClientsConnections, incServersConnections,
  incServersConnectionsWithResponse: Integer; incBytesReceived, incBytesSend: Int64);
begin
  // Multithread prevention
  FNodeServersAddresses.LockList;
  try
    FNetStatistics.ActiveConnections := FNetStatistics.ActiveConnections + incActiveConnections;
    FNetStatistics.ClientsConnections := FNetStatistics.ClientsConnections + incClientsConnections;
    FNetStatistics.ServersConnections := FNetStatistics.ServersConnections + incServersConnections;
    FNetStatistics.ServersConnectionsWithResponse := FNetStatistics.ServersConnectionsWithResponse +
      incServersConnectionsWithResponse;
    if (incActiveConnections > 0) then
      FNetStatistics.TotalConnections := FNetStatistics.TotalConnections + incActiveConnections;
    if (incClientsConnections > 0) then
      FNetStatistics.TotalClientsConnections := FNetStatistics.TotalClientsConnections + incClientsConnections;
    if (incServersConnections > 0) then
      FNetStatistics.TotalServersConnections := FNetStatistics.TotalServersConnections + incServersConnections;
    FNetStatistics.BytesReceived := FNetStatistics.BytesReceived + incBytesReceived;
    FNetStatistics.BytesSend := FNetStatistics.BytesSend + incBytesSend;
  finally
    FNodeServersAddresses.UnlockList;
  end;
  NotifyStatisticsChanged;
  if (incBytesReceived <> 0) or (incBytesSend <> 0) then
  begin
    NotifyNetConnectionUpdated;
  end;
end;

procedure TConnectionManager.SetMaxNodeServersAddressesBuffer(AValue: Integer);
begin
  if FMaxNodeServersAddressesBuffer = AValue then
    exit;
  if (AValue < cMIN_NODESERVERS_BUFFER) then
    FMaxNodeServersAddressesBuffer := cMIN_NODESERVERS_BUFFER
  else if (AValue > cMAX_NODESERVERS_BUFFER) then
    FMaxNodeServersAddressesBuffer := cMAX_NODESERVERS_BUFFER
  else
    FMaxNodeServersAddressesBuffer := AValue;
end;

function TConnectionManager.IndexOfNetClient(ListToSearch: TList; ip: AnsiString; port: Word;
  indexStart: Integer = 0): Integer;
var
  P: PNodeServerAddress;
begin
  if indexStart < 0 then
    indexStart := 0;
  for Result := indexStart to ListToSearch.Count - 1 do
  begin
    P := ListToSearch[Result];
    if (AnsiSameText(P^.ip, ip)) and ((port = 0) or (P^.port = port)) then
      exit;
  end;
  Result := -1;
end;

function TConnectionManager.IsBlackListed(const ip: AnsiString; port: Word): Boolean;
var
  l: TList;
  i: Integer;
begin
  Result := false;
  l := FNodeServersAddresses.LockList;
  try
    i := -1;
    repeat
      i := IndexOfNetClient(l, ip, port, i + 1);
      if (i >= 0) then
      begin
        if (PNodeServerAddress(l[i])^.is_blacklisted) then
        begin
          Result := not PNodeServerAddress(l[i])^.its_myself;
        end;
      end;
    until (i < 0) or (Result);
  finally
    FNodeServersAddresses.UnlockList;
  end;
end;

class function TConnectionManager.GetInstance: TConnectionManager;
begin
  Result := nil;
  Flock.Acquire;
  if FDestroying
  then exit;
  try
    if not Assigned(FInstance)
    then FInstance := TConnectionManager.Create(nil);
    Result := FInstance;
  finally
    Flock.Release;
  end;
end;

class function TConnectionManager.NetDataExists: Boolean;
begin
  Result := Assigned(FInstance);
end;

function TConnectionManager.NewRequestId: Cardinal;
begin
  Inc(FLastRequestId);
  Result := FLastRequestId;
end;

procedure TConnectionManager.Notification(AComponent: TComponent; operation: TOperation);
var
  l: TList;
begin
  inherited;
  if operation = OpRemove then
  begin
    if not(csDestroying in ComponentState) then
    begin
      l := FNetConnections.LockList;
      try
        if l.Remove(AComponent) >= 0 then
        begin
          NotifyNetConnectionUpdated;
        end;
      finally
        FNetConnections.UnlockList;
      end;
    end;
  end;
end;

procedure TConnectionManager.NotifyBlackListUpdated;
begin
  FNetDataNotifyEventsThread.NotifyOnBlackListUpdated := true;
end;

procedure TConnectionManager.NotifyNetConnectionUpdated;
begin
  FNetDataNotifyEventsThread.NotifyOnNetConnectionsUpdated := true;
end;

class procedure TConnectionManager.NotifyNodeServersUpdated;
begin
  if Assigned(FInstance)
  then FInstance.FNetDataNotifyEventsThread.NotifyOnNodeServersUpdated := true;
end;

procedure TConnectionManager.NotifyReceivedHelloMessage;
begin
  FNetDataNotifyEventsThread.NotifyOnReceivedHelloMessage := true;
end;

procedure TConnectionManager.NotifyStatisticsChanged;
begin
  FNetDataNotifyEventsThread.NotifyOnStatisticsChanged := true;
end;


function TConnectionManager.PendingRequest(Sender: TNetConnectionBase; var requests_data: AnsiString): Integer;
var
  P: PNetRequestRegistered;
  i: Integer;
  l: TList;
begin
  requests_data := '';
  l := FRegisteredRequests.LockList;
  try
    if Assigned(Sender) then
    begin
      Result := 0;
      for i := l.Count - 1 downto 0 do
      begin
        if (PNetRequestRegistered(l[i])^.NetClient = Sender) then
        begin
          requests_data := requests_data + 'Op:' + IntToStr(PNetRequestRegistered(l[i])^.operation) + ' Id:' +
            Inttostr(PNetRequestRegistered(l[i])^.RequestId) + ' - ';
          Inc(Result);
        end;
      end;
    end
    else
      Result := l.Count;
  finally
    FRegisteredRequests.UnlockList;
  end;
end;

procedure TConnectionManager.RegisterRequest(Sender: TNetConnectionBase; operation: Word; request_id: Cardinal);
var
  P: PNetRequestRegistered;
  l: TList;
begin
  l := FRegisteredRequests.LockList;
  try
    New(P);
    P^.NetClient := Sender;
    P^.operation := operation;
    P^.RequestId := request_id;
    P^.SendTime := now;
    l.Add(P);
    TLog.NewLog(ltdebug, Classname, 'Registering request to ' + Sender.ClientRemoteAddr + ' Op:' +
      IntToStr(operation) + ' Id:' + Inttostr(request_id) + ' Total pending:' + Inttostr(l.Count));
  finally
    FRegisteredRequests.UnlockList;
  end;
end;

procedure TConnectionManager.SetNetConnectionsActive(const Value: Boolean);
begin
  FNetConnectionsActive := Value;
  if FNetConnectionsActive then
    DiscoverServers
  else
    DisconnectClients;
end;

function TConnectionManager.UnRegisterRequest(Sender: TNetConnectionBase; operation: Word; request_id: Cardinal): Boolean;
var
  P: PNetRequestRegistered;
  i: Integer;
  l: TList;
begin
  Result := false;
  l := FRegisteredRequests.LockList;
  try
    for i := l.Count - 1 downto 0 do
    begin
      P := l[i];
      if (P^.NetClient = Sender) and (((operation = P^.operation) and (request_id = P^.RequestId)) or
        ((operation = 0) and (request_id = 0))) then
      begin
        l.Delete(i);
        Dispose(P);
        Result := true;
        if Assigned(Sender.TcpIpClient) then
        begin
          TLog.NewLog(ltdebug, Classname, 'Unregistering request to ' + Sender.ClientRemoteAddr + ' Op:' +
            IntToStr(operation) + ' Id:' + Inttostr(request_id) + ' Total pending:' + Inttostr(l.Count));
        end
        else
        begin
          TLog.NewLog(ltdebug, Classname, 'Unregistering request to (NIL) Op:' + IntToStr(operation) + ' Id:' +
            Inttostr(request_id) + ' Total pending:' + Inttostr(l.Count));
        end;
      end;
    end;
  finally
    FRegisteredRequests.UnlockList;
  end;
end;

procedure TThreadCheckConnections.BCExecute;
var
  l: TList;
  i, nactive, ndeleted, ntotal, nserverclients: Integer;
  netconn: TNetConnection;
  netserverclientstop: TNetServerClient;
  newstats: TNetStatistics;
begin
  newstats := TNetStatistics.Empty;
  FLastCheckTS := GetTickCount;
  while (not Terminated) do
  begin
    if ((GetTickCount > (FLastCheckTS + 1000)) and (not FNetData.FIsDiscoveringServers)) then
    begin
      nactive := 0;
      ndeleted := 0;
      ntotal := 0;
      nserverclients := 0;
      netserverclientstop := nil;
      FLastCheckTS := GetTickCount;
      if (FNetData.FNetConnections.TryLockList(100, l)) then
      begin
        try
          ntotal := l.Count;
          newstats := TNetStatistics.Empty;
          for i := l.Count - 1 downto 0 do
          begin
            netconn := TNetConnection(l.Items[i]);
            if (netconn is TNetClient) then
            begin
              if (netconn.Connected) then
              begin
                Inc(newstats.ServersConnections);
                if (netconn.HasReceivedData) then
                  Inc(newstats.ServersConnectionsWithResponse);
              end;
              if (not TNetClient(netconn).Connected) and (netconn.CreatedTime + EncodeTime(0, 0, 5, 0) < now) then
              begin
                // Free this!
                TNetClient(netconn).FinalizeConnection;
                Inc(ndeleted);
              end
              else
                Inc(nactive);
            end
            else if (netconn is TNetServerClient) then
            begin
              if (netconn.Connected) then
              begin
                Inc(newstats.ClientsConnections);
              end;
              Inc(nserverclients);
              if (not netconn.DoFinalizeConnection) then
              begin
                // Build 1.0.9 BUG-101 Only disconnect old versions prior to 1.0.9
                if not Assigned(netserverclientstop) then
                begin
                  netserverclientstop := TNetServerClient(netconn);
                end
                else if (netconn.CreatedTime < netserverclientstop.CreatedTime) then
                begin
                  netserverclientstop := TNetServerClient(netconn);
                end;
              end;
            end;
          end;
          // Update stats:
          FNetData.FNetStatistics.ActiveConnections := newstats.ClientsConnections + newstats.ServersConnections;
          FNetData.FNetStatistics.ClientsConnections := newstats.ClientsConnections;
          FNetData.FNetStatistics.ServersConnections := newstats.ServersConnections;
          FNetData.FNetStatistics.ServersConnectionsWithResponse := newstats.ServersConnectionsWithResponse;
          // Must stop clients?
          if (nserverclients > cMaximumNumberOfServers) and
          // This is to ensure there are more serverclients than clients
            ((nserverclients + nactive + ndeleted) >= FNetData.FMaxConnections) and (Assigned(netserverclientstop)) then
          begin
            TLog.NewLog(ltInfo, Classname,
              Format('Sending FinalizeConnection to NodeConnection %s created on %s (working time %s) - NetServerClients:%d Servers_active:%d Servers_deleted:%d',
              [netserverclientstop.Client.ClientRemoteAddr, FormatDateTime('hh:nn:ss', netserverclientstop.CreatedTime),
              FormatDateTime('hh:nn:ss', now - netserverclientstop.CreatedTime), nserverclients, nactive, ndeleted]));
            netserverclientstop.FinalizeConnection;
          end;
        finally
          FNetData.FNetConnections.UnlockList;
        end;
        if (nactive <= cMaximumNumberOfServers) and (not Terminated) then
        begin
          // Discover
          FNetData.DiscoverServers;
        end;
      end;
    end;
    sleep(100);
  end;
end;

constructor TThreadCheckConnections.Create(NetData: TConnectionManager);
begin
  FNetData := NetData;
  inherited Create(false);
end;

procedure TNetClientsDestroyThread.BCExecute;
var
  l, l_to_del: TList;
  i: Integer;
begin
  l_to_del := TList.Create;
  try
    while not Terminated do
    begin
      l_to_del.Clear;
      l := TConnectionManager.NetConnections.LockList;
      try
        FTerminatedAllConnections := l.Count = 0;
        for i := 0 to l.Count - 1 do
        begin
          if (TObject(l[i]) is TNetClient) and (not TNetConnection(l[i]).Connected) and
            (TNetConnection(l[i]).DoFinalizeConnection) and (not TNetConnection(l[i]).IsConnecting) then
          begin
            l_to_del.Add(l[i]);
          end;
        end;
      finally
        TConnectionManager.NetConnections.UnlockList;
      end;
      sleep(500); // Delay - Sleep time before destroying (1.5.3)
      if l_to_del.Count > 0 then
      begin
        TLog.NewLog(ltdebug, Classname, 'Destroying NetClients: ' + Inttostr(l_to_del.Count));
        for i := 0 to l_to_del.Count - 1 do
        begin
          try
            DebugStep := 'Destroying NetClient ' + TNetConnection(l_to_del[i]).ClientRemoteAddr;
            TNetConnection(l_to_del[i]).Free;
          except
            on E: Exception do
            begin
              TLog.NewLog(ltError, Classname, 'Exception destroying TNetConnection ' + IntToHex(PtrInt(l_to_del[i]), 8)
                + ': (' + E.Classname + ') ' + E.Message);
            end;
          end;
        end;
      end;
      sleep(100);
    end;
  finally
    l_to_del.Free;
  end;
end;

constructor TNetClientsDestroyThread.Create(NetData: TConnectionManager);
begin
  FTerminatedAllConnections := true;
  inherited Create(false);
end;

procedure TNetClientsDestroyThread.WaitForTerminatedAllConnections;
begin
  while (not FTerminatedAllConnections) do
  begin
    TLog.NewLog(ltdebug, Classname, 'Waiting all connections terminated');
    sleep(100);
  end;
end;

initialization
  TConnectionManager.Status := 0;
  TConnectionManager.Operation := 0;
end.
