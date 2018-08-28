unit MicroCoin.Net.Discovery;

{
  This unit contains code from PascalCoin:

  Copyright (c) Albert Molina 2016 - 2018 original code from PascalCoin https://pascalcoin.org/

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

interface

uses UThread, MicroCoin.Net.NodeServer, SysUtils, Classes;

type

  TThreadDiscoverConnection = class(TPCThread)
    FNodeServerAddress: TNodeServer;
  protected
    procedure BCExecute; override;
  public
    constructor Create(NodeServerAddress: TNodeServer; NotifyOnTerminate: TNotifyEvent);
  end;

implementation

uses MicroCoin.Net.Client, ULog, MicroCoin.Net.ConnectionManager;

procedure TThreadDiscoverConnection.BCExecute;
var
  nc: TNetClient;
  ok: Boolean;
  lns: TList;
  i: Integer;
  Pnsa: PNodeServerAddress;
begin
  if Terminated then
    exit;

  TLog.NewLog(ltInfo, Classname, 'Starting discovery of connection ' + FNodeServerAddress.ip + ':' + Inttostr(FNodeServerAddress.port));
  Pnsa := nil;
  DebugStep := 'Locking list';
  // Register attempt
  lns := TConnectionManager.NetData.NodeServersAddresses.LockList;
  try
    DebugStep := 'Searching net client';
    i := TConnectionManager.NetData.IndexOfNetClient(lns, FNodeServerAddress.ip, FNodeServerAddress.port);
    if i >= 0 then
    begin
      DebugStep := 'Searching client found';
      Pnsa := PNodeServerAddress(lns[i]);
      Pnsa.last_attempt_to_connect := now;
      Inc(Pnsa.total_failed_attemps_to_connect);
    end;
  finally
    TConnectionManager.NetData.NodeServersAddresses.UnlockList;
  end;
  DebugStep := 'Synchronizing notify';
  if Terminated then
    exit;
  TConnectionManager.NetData.NotifyNodeServersUpdated;
  // Try to connect
  ok := false;
  DebugStep := 'Trying to connect';
  if Terminated then
    exit;
  nc := TNetClient.Create(nil);
  try
    DebugStep := 'Connecting';
    if nc.ConnectTo(FNodeServerAddress.ip, FNodeServerAddress.port) then
    begin
      if Terminated then
        exit;
      sleep(500);
      DebugStep := 'Is connected now?';
      if Terminated then
        exit;
      ok := nc.Connected;
    end;
    if Terminated then
      exit;
  finally
    if (not ok) and (not Terminated) then
    begin
      DebugStep := 'Destroying non connected';
      nc.FinalizeConnection;
    end;
  end;
  DebugStep := 'Synchronizing notify final';
  if Terminated then
    exit;
  TConnectionManager.NetData.NotifyNodeServersUpdated;
end;

constructor TThreadDiscoverConnection.Create(NodeServerAddress: TNodeServer; NotifyOnTerminate: TNotifyEvent);
begin
  FNodeServerAddress := NodeServerAddress;
  inherited Create(true);
  OnTerminate := NotifyOnTerminate;
  FreeOnTerminate := true;
  Suspended := false;
end;

end.
