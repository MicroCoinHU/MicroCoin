unit MicroCoin.Net.Client;

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

uses UThread, MicroCoin.Net.Connection, Sysutils, Classes, ULog, MicroCoin.Net.ConnectionManager;

type

  TNetClient = class;

  TNetClientThread = class(TPCThread)
  private
    FNetClient: TNetClient;
  protected
    procedure BCExecute; override;
  public
    constructor Create(NetClient: TNetClient; AOnTerminateThread: TNotifyEvent);
  end;

  TNetClient = class(TNetConnection)
  private
    FNetClientThread: TNetClientThread;
    procedure OnNetClientThreadTerminated(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TNetServerClient = class(TNetConnection);

implementation

procedure TNetClientThread.BCExecute;
begin
  while (not Terminated) do
  begin
    if FNetClient.Connected then
    begin
      FNetClient.DoProcessBuffer;
    end;
    sleep(1);
  end;
end;

constructor TNetClientThread.Create(NetClient: TNetClient; AOnTerminateThread: TNotifyEvent);
begin
  FNetClient := NetClient;
  inherited Create(false);
  OnTerminate := AOnTerminateThread;
end;

{ TNetClient }

constructor TNetClient.Create(AOwner: TComponent);
begin
  inherited;
  FNetClientThread := TNetClientThread.Create(Self, OnNetClientThreadTerminated);
  FNetClientThread.FreeOnTerminate := false;
end;

destructor TNetClient.Destroy;
begin
  LogDebug(Classname, 'Starting TNetClient.Destroy');
  if not assigned(FNetClientThread) then
    exit;
  FNetClientThread.OnTerminate := nil;
  if not FNetClientThread.Terminated then
  begin
    FNetClientThread.Terminate;
    FNetClientThread.WaitFor;
  end;
  FreeAndNil(FNetClientThread);
  inherited;
end;

procedure TNetClient.OnNetClientThreadTerminated(Sender: TObject);
begin
  // Close connection
  if TConnectionManager.Instance.ConnectionExistsAndActive(Self) then
  begin
    Connected := false;
  end;
end;

end.
