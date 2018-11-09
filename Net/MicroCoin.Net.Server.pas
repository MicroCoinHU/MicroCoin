unit MicroCoin.Net.Server;
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

uses UTCPIP, Sysutils, classes, ULog, UTime
{$IFNDEF FPC}
    , Windows
{$ENDIF}
    ;

type

  TNetServer = class(TNetTcpIpServer)
  private
  protected
    procedure OnNewIncommingConnection(Sender: TObject; Client: TNetTcpIpClient); override;
    procedure SetActive(const Value: Boolean); override;
    procedure SetMaxConnections(AValue: Integer); override;
  public
    constructor Create; override;
  end;

implementation

uses UConst, MicroCoin.Net.Client, MicroCoin.Net.ConnectionManager, MicroCoin.Net.Protocol;

constructor TNetServer.Create;
begin
  inherited;
  MaxConnections := cMaximumClients;
  NetTcpIpClientClass := TBufferedNetTcpIpClient;
  port := cNetServerPort;
end;

procedure TNetServer.OnNewIncommingConnection(Sender: TObject; Client: TNetTcpIpClient);
var
  n: TNetServerClient;
  DebugStep: string;
  tc: Cardinal;
begin
  DebugStep := '';
  try
    if not Client.Connected then
      exit;
    // NOTE: I'm in a separate thread
    // While in this function the ClientSocket connection will be active, when finishes the ClientSocket will be destroyed
    TLog.NewLog(ltInfo, Classname, 'Starting ClientSocket accept ' + Client.ClientRemoteAddr);
    n := TNetServerClient.Create(nil);
    try
      DebugStep := 'Assigning client';
      n.SetClient(Client);
      TConnectionManager.Instance.IncStatistics(1, 1, 0, 0, 0, 0);
      TConnectionManager.Instance.CleanBlackList;
      DebugStep := 'Checking blacklisted';
      if (TConnectionManager.Instance.IsBlackListed(Client.RemoteHost, 0)) then
      begin
        // Invalid!
        TLog.NewLog(ltInfo, Classname, 'Refusing Blacklist ip: ' + Client.ClientRemoteAddr);
        n.SendError(ntp_autosend, cNetOp_Error, 0, cNetError_IPBlackListed,
          'Your IP is blacklisted:' + Client.ClientRemoteAddr);
        // Wait some time before close connection
        sleep(5000);
      end
      else
      begin
        DebugStep := 'Processing buffer and sleep...';
        while (n.Connected) and (Active) do
        begin
          n.DoProcessBuffer;
          sleep(10);
        end;
      end;
    finally
      try
        TLog.NewLog(ltdebug, Classname, 'Finalizing ServerAccept ' + IntToHex(PtrInt(n), 8) + ' ' + n.ClientRemoteAddr);
        DebugStep := 'Disconnecting NetServerClient';
        n.Connected := false;
        tc := GetTickCount;
        repeat
          sleep(10);
          // 1.5.4 -> To prevent that not client disconnected (and not called OnDisconnect), increase sleep time
        until (not n.Connected) or (tc + 5000 < GetTickCount);
        sleep(5);
        DebugStep := 'Assigning old client';
        n.SetClient(NetTcpIpClientClass.Create(nil));
        sleep(500); // Delay - Sleep time before destroying (1.5.3)
        DebugStep := 'Freeing NetServerClient';
      finally
        n.Free;
      end;
    end;
  except
    on E: Exception do
    begin
      TLog.NewLog(ltError, Classname, 'Exception processing client thread at step: ' + DebugStep + ' - (' + E.Classname
        + ') ' + E.Message);
    end;
  end;
end;

procedure TNetServer.SetActive(const Value: Boolean);
begin
  if Value then
  begin
    TLog.NewLog(ltInfo, Classname, 'Activating server on port ' + Inttostr(port));
  end
  else
  begin
    TLog.NewLog(ltInfo, Classname, 'Closing server');
  end;
  inherited;
  if Active then
  begin
    // TNode.Node.AutoDiscoverNodes(CT_Discover_IPs);
  end
  else if TConnectionManager.NetDataExists then
  begin
    TConnectionManager.Instance.DisconnectClients;
  end;
end;

procedure TNetServer.SetMaxConnections(AValue: Integer);
begin
  inherited SetMaxConnections(AValue);
  TConnectionManager.Instance.MaxConnections := AValue;
end;

end.
