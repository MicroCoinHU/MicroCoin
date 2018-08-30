unit MicroCoin.Net.Events;
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

uses UThread, Classes;

type

  INetNotificationSource = interface
    ['{9CC4B8C1-2D7A-45B9-978B-DE92356A5818}']
    function GetOnNetConnectionsUpdated: TNotifyEvent;
    function GetOnNodeServersUpdated: TNotifyEvent;
    function GetOnBlackListUpdated: TNotifyEvent;
    function GetOnReceivedHelloMessage: TNotifyEvent;
    function GetOnStatisticsChanged: TNotifyEvent;

    property OnNetConnectionsUpdated: TNotifyEvent read GetOnNetConnectionsUpdated;
    property OnNodeServersUpdated: TNotifyEvent read GetOnNodeServersUpdated;
    property OnBlackListUpdated: TNotifyEvent read GetOnBlackListUpdated;
    property OnReceivedHelloMessage: TNotifyEvent read GetOnReceivedHelloMessage;
    property OnStatisticsChanged: TNotifyEvent read GetOnStatisticsChanged;
  end;

  TNetDataNotifyEventsThread = class(TPCThread)
  private
    FNetData: INetNotificationSource;
    FNotifyOnReceivedHelloMessage: Boolean;
    FNotifyOnStatisticsChanged: Boolean;
    FNotifyOnNetConnectionsUpdated: Boolean;
    FNotifyOnNodeServersUpdated: Boolean;
    FNotifyOnBlackListUpdated: Boolean;
  protected
    procedure SynchronizedNotify;
    procedure BCExecute; override;
  public
    constructor Create(ANetData: INetNotificationSource);

    property NotifyOnReceivedHelloMessage: Boolean read FNotifyOnReceivedHelloMessage
      write FNotifyOnReceivedHelloMessage;
    property NotifyOnStatisticsChanged: Boolean read FNotifyOnStatisticsChanged write FNotifyOnStatisticsChanged;
    property NotifyOnNetConnectionsUpdated: Boolean read FNotifyOnNetConnectionsUpdated
      write FNotifyOnNetConnectionsUpdated;
    property NotifyOnNodeServersUpdated: Boolean read FNotifyOnNodeServersUpdated write FNotifyOnNodeServersUpdated;
    property NotifyOnBlackListUpdated: Boolean read FNotifyOnBlackListUpdated write FNotifyOnBlackListUpdated;

  end;

implementation

procedure TNetDataNotifyEventsThread.BCExecute;
begin
  while (not Terminated) do
  begin
    if (FNotifyOnReceivedHelloMessage) or (FNotifyOnStatisticsChanged) or (FNotifyOnNetConnectionsUpdated) or
      (FNotifyOnNodeServersUpdated) or (FNotifyOnBlackListUpdated) then
    begin
      Synchronize(SynchronizedNotify);
    end;
    sleep(10);
  end;
end;

constructor TNetDataNotifyEventsThread.Create(ANetData: INetNotificationSource);
begin
  FNetData := ANetData;
  FNotifyOnReceivedHelloMessage := false;
  FNotifyOnStatisticsChanged := false;
  FNotifyOnNetConnectionsUpdated := false;
  FNotifyOnNodeServersUpdated := false;
  FNotifyOnBlackListUpdated := false;
  inherited Create(false);
end;

procedure TNetDataNotifyEventsThread.SynchronizedNotify;
begin
  if Terminated then
    exit;
  if not Assigned(FNetData) then
    exit;

  if FNotifyOnReceivedHelloMessage then
  begin
    FNotifyOnReceivedHelloMessage := false;
    if Assigned(FNetData.OnReceivedHelloMessage) then
      FNetData.OnReceivedHelloMessage(FNetData as TObject);
  end;
  if FNotifyOnStatisticsChanged then
  begin
    FNotifyOnStatisticsChanged := false;
    if Assigned(FNetData.OnStatisticsChanged) then
      FNetData.OnStatisticsChanged(FNetData as TObject);
  end;
  if FNotifyOnNetConnectionsUpdated then
  begin
    FNotifyOnNetConnectionsUpdated := false;
    if Assigned(FNetData.OnNetConnectionsUpdated) then
      FNetData.OnNetConnectionsUpdated(FNetData as TObject);
  end;
  if FNotifyOnNodeServersUpdated then
  begin
    FNotifyOnNodeServersUpdated := false;
    if Assigned(FNetData.OnNodeServersUpdated) then
      FNetData.OnNodeServersUpdated(FNetData as TObject);
  end;
  if FNotifyOnBlackListUpdated then
  begin
    FNotifyOnBlackListUpdated := false;
    if Assigned(FNetData.OnBlackListUpdated) then
      FNetData.OnBlackListUpdated(FNetData as TObject);
  end;
end;

end.
