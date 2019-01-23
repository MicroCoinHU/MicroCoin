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
  | File:       MicroCoin.Net.Handlers.Hello.pas                                 |
  | Created at: 2018-09-21                                                       |
  | Purpose:    Hello message handler                                            |
  |============================================================================== }
unit MicroCoin.Net.Handlers.Hello;

interface

uses Classes, SysUtils, MicroCoin.Net.CommandHandler,
  MicroCoin.Net.ConnectionManager, MicroCoin.Common.Config, ULog,
  MicroCoin.Account.AccountKey, MicroCoin.Net.NodeServer, UTime,
  UThread, MicroCoin.Net.Utils,
  MicroCoin.Net.Protocol, MicroCoin.Net.Connection, MicroCoin.Node.Node;

type
  THelloHandler = class(TInterfacedObject, ICommandHandler)
  public
    procedure HandleCommand(AHeader: TNetHeaderData; AData: TStream; AConnection: TObject);
  end;

implementation

uses MicroCoin.Net.ConnectionBase;

{ THelloHandler }

procedure THelloHandler.HandleCommand(AHeader: TNetHeaderData; AData: TStream; AConnection: TObject);
var
  i: Integer;
  nsa: TNodeServer;
  Duplicate: TNetConnection;
  xHello: TNetMessage_Hello;
  Connection: TNetConnection;
begin
  Connection := AConnection as TNetConnection;
  Connection.RemoteAccumulatedWork := 0;
  try
    AData.Position := 0;
    xHello := TNetMessage_Hello.LoadFromStream(AData);
    xHello.RemoteHost := Connection.RemoteHost;
    Connection.ClientPublicKey := xHello.AccountKey;
    Connection.TimestampDiff :=
      Integer(Int64(xHello.timestamp) - Int64(TConnectionManager.Instance.NetworkAdjustedTime.GetAdjustedTime));
    if Connection.ClientTimestampIp = '' then
    begin
      Connection.ClientTimestampIp := xHello.RemoteHost;
      TConnectionManager.Instance.NetworkAdjustedTime.AddNewIp(Connection.ClientTimestampIp, xHello.timestamp);
      if (Abs(TConnectionManager.Instance.NetworkAdjustedTime.TimeOffset) > cBlockTimeStampTolerance) then
      begin
        TNode.Node.NotifyNetClientMessage(nil,
          Format('The detected network time is different from this system time in %d seconds! Please check your local time/timezone',
          [TConnectionManager.Instance.NetworkAdjustedTime.TimeOffset]));
      end;
      if (Abs(Connection.TimestampDiff) > cBlockTimeStampTolerance) then
      begin
        TLog.NewLog(ltError, Classname, 'Detected a node (' + Connection.ClientRemoteAddr +
          ') with incorrect timestamp: ' + Inttostr(xHello.timestamp) + ' offset ' +
          Inttostr(Connection.TimestampDiff));
      end;
    end;

    if (xHello.server_port > 0) and (not SameText(Connection.Client.RemoteHost, 'localhost')) and
      (not SameText(Connection.Client.RemoteHost, '127.0.0.1')) and
      (not SameText('192.168.', Copy(Connection.Client.RemoteHost, 1, 8))) and
      (not SameText('10.', Copy(Connection.Client.RemoteHost, 1, 3))) and
      (not TAccountKey.EqualAccountKeys(xHello.AccountKey, TConnectionManager.Instance.NodePrivateKey.PublicKey)) then
    begin
      nsa := TNodeServer.Empty;
      nsa.ip := xHello.RemoteHost;
      nsa.port := xHello.server_port;
      nsa.last_connection := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
      TConnectionManager.Instance.AddServer(nsa);
    end;

    Connection.RemoteOperationBlock := xHello.Block.BlockHeader;

    for i := low(xHello.nodeservers) to high(xHello.nodeservers) do
      if (xHello.nodeservers[i].last_connection_by_server > 0) and (i <= cMAX_NODESERVERS_ON_HELLO) then
        TConnectionManager.Instance.AddServer(xHello.nodeservers[i]);

    Connection.ClientAppVersion := xHello.client_version;
    Connection.RemoteAccumulatedWork := xHello.remote_work;

    if (Connection.RemoteAccumulatedWork > TNode.Node.BlockManager.AccountStorage.WorkSum) or
      ((Connection.RemoteAccumulatedWork = 0) and (TConnectionManager.Instance.MaxRemoteOperationBlock.Block <
      Connection.RemoteOperationBlock.Block)) then
    begin
      TConnectionManager.Instance.MaxRemoteOperationBlock := Connection.RemoteOperationBlock;
      if TPCThread.ThreadClassFound(TThreadGetNewBlockChainFromClient, nil) < 0 then
        TThreadGetNewBlockChainFromClient.Create(false).FreeOnTerminate := true;
    end;
    LogDebug( Classname, 'Received HELLO with height: ' + Inttostr(xHello.Block.BlockHeader.Block) +
      ' Accumulated work ' + Inttostr(Connection.RemoteAccumulatedWork));

    LogDebug( Classname, 'Hello received: ' + Connection.RemoteOperationBlock.ToString());
    if (AHeader.HeaderType in [ntp_request, ntp_response]) then
    begin
      if (AHeader.HeaderType = ntp_request) then
        Connection.Send_Hello(ntp_response, AHeader.RequestId);

      if (TAccountKey.EqualAccountKeys(Connection.ClientPublicKey, TConnectionManager.Instance.NodePrivateKey.PublicKey))
      then
      begin
        Connection.DisconnectInvalidClient(true, 'MySelf disconnecting...');
        exit;
      end;
      Duplicate := TConnectionManager.Instance.FindConnectionByClientRandomValue(Connection);
      if (Duplicate <> nil) and (Duplicate.Connected) then
      begin
        Connection.DisconnectInvalidClient(true, 'Duplicate connection with ' + Duplicate.ClientRemoteAddr);
        exit;
      end;
      TConnectionManager.Instance.NotifyReceivedHelloMessage;
    end
    else
    begin
      Connection.DisconnectInvalidClient(false, 'Invalid header type > ' + (AHeader.ToString));
      exit;
    end;
  finally
    xHello.Block.Free;
  end;
end;

initialization

TNetConnectionBase.AddHandler(cNetOp_Hello, THelloHandler);

end.
