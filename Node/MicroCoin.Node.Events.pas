unit MicroCoin.Node.Events;
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

uses MicroCoin.Node.Node, Sysutils, classes, MicroCoin.Net.Connection, UThread, UCrypto, ULog;

type

  TNodeMessageEvent = procedure(NetConnection: TNetConnection; MessageData: TRawBytes) of object;

  TNodeNotifyEvents = class;

  TThreadSafeNodeNotifyEvent = class(TPCThread)
  private
    FNodeNotifyEvents: TNodeNotifyEvents;
    FNotifyBlocksChanged: Boolean;
    FNotifyOperationsChanged: Boolean;
    procedure SynchronizedProcess;
  protected
    procedure BCExecute; override;
    constructor Create(ANodeNotifyEvents: TNodeNotifyEvents);
  end;

  { TNodeNotifyEvents is ThreadSafe and will only notify in the main thread }
  TNodeNotifyEvents = class(TComponent)
  private
    FNode: TNode;
    FPendingNotificationsList: TPCThreadList;
    FThreadSafeNodeNotifyEvent: TThreadSafeNodeNotifyEvent;
    FOnBlocksChanged: TNotifyEvent;
    FOnTransactionsChanged: TNotifyEvent;
    FMessages: TStringList;
    FOnNodeMessageEvent: TNodeMessageEvent;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetNode(const Value: TNode);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure NotifyBlocksChanged;
    procedure NotifyOperationsChanged;

    property Node: TNode read FNode write SetNode;
    property OnBlocksChanged: TNotifyEvent read FOnBlocksChanged write FOnBlocksChanged;
    property OnTransactionsChanged: TNotifyEvent read FOnTransactionsChanged write FOnTransactionsChanged;
    property OnNodeMessageEvent: TNodeMessageEvent read FOnNodeMessageEvent write FOnNodeMessageEvent;
    property Messages: TStringList read FMessages;
  end;

implementation

constructor TNodeNotifyEvents.Create(AOwner: TComponent);
begin
  inherited;
  FOnTransactionsChanged := nil;
  FOnBlocksChanged := nil;
  FOnNodeMessageEvent := nil;
  FMessages := TStringList.Create;
  FPendingNotificationsList := TPCThreadList.Create('TNodeNotifyEvents_PendingNotificationsList');
  FThreadSafeNodeNotifyEvent := TThreadSafeNodeNotifyEvent.Create(Self);
  FThreadSafeNodeNotifyEvent.FreeOnTerminate := true; // This is to prevent locking when freeing component
  Node := TNode.Node;
end;

destructor TNodeNotifyEvents.Destroy;
begin
  if Assigned(FNode) then
    FNode.NotifyList.Remove(Self);
  FThreadSafeNodeNotifyEvent.FNodeNotifyEvents := nil;
  FThreadSafeNodeNotifyEvent.Terminate;
  FreeAndNil(FPendingNotificationsList);
  FreeAndNil(FMessages);
  inherited;
end;

procedure TNodeNotifyEvents.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opremove) then
  begin
    if AComponent = FNode then
      FNode := nil;
  end;
end;

procedure TNodeNotifyEvents.NotifyBlocksChanged;
begin
  if Assigned(FThreadSafeNodeNotifyEvent) then
    FThreadSafeNodeNotifyEvent.FNotifyBlocksChanged := true;
end;

procedure TNodeNotifyEvents.NotifyOperationsChanged;
begin
  if Assigned(FThreadSafeNodeNotifyEvent) then
    FThreadSafeNodeNotifyEvent.FNotifyOperationsChanged := true;
end;

procedure TNodeNotifyEvents.SetNode(const Value: TNode);
begin
  if FNode = Value then
    exit;
  if Assigned(FNode) then
  begin
    FNode.RemoveFreeNotification(Self);
    FNode.NotifyList.Remove(Self);
  end;
  FNode := Value;
  if Assigned(FNode) then
  begin
    FNode.FreeNotification(Self);
    FNode.NotifyList.Add(Self);
  end;
end;

{ TThreadSafeNodeNotifyEvent }

procedure TThreadSafeNodeNotifyEvent.BCExecute;
begin
  while (not Terminated) and (Assigned(FNodeNotifyEvents)) do
  begin
    if (FNotifyOperationsChanged) or (FNotifyBlocksChanged) or (FNodeNotifyEvents.FMessages.Count > 0) then
      Synchronize(SynchronizedProcess);
    Sleep(100);
  end;
end;

constructor TThreadSafeNodeNotifyEvent.Create(ANodeNotifyEvents: TNodeNotifyEvents);
begin
  FNodeNotifyEvents := ANodeNotifyEvents;
  inherited Create(false);
end;

procedure TThreadSafeNodeNotifyEvent.SynchronizedProcess;
var
  i: Integer;
begin
  try
    if (Terminated) or (not Assigned(FNodeNotifyEvents)) then
      exit;
    if FNotifyBlocksChanged then
    begin
      FNotifyBlocksChanged := false;
      DebugStep := 'Notify OnBlocksChanged';
      if Assigned(FNodeNotifyEvents) and (Assigned(FNodeNotifyEvents.FOnBlocksChanged)) then
        FNodeNotifyEvents.FOnBlocksChanged(FNodeNotifyEvents);
    end;
    if FNotifyOperationsChanged then
    begin
      FNotifyOperationsChanged := false;
      DebugStep := 'Notify OnOperationsChanged';
      if Assigned(FNodeNotifyEvents) and (Assigned(FNodeNotifyEvents.FOnTransactionsChanged)) then
        FNodeNotifyEvents.FOnTransactionsChanged(FNodeNotifyEvents);
    end;
    if FNodeNotifyEvents.FMessages.Count > 0 then
    begin
      DebugStep := 'Notify OnNodeMessageEvent';
      if Assigned(FNodeNotifyEvents) and (Assigned(FNodeNotifyEvents.FOnNodeMessageEvent)) then
      begin
        for i := 0 to FNodeNotifyEvents.FMessages.Count - 1 do
        begin
          DebugStep := 'Notify OnNodeMessageEvent ' + IntToStr(i + 1) + '/' +
            IntToStr(FNodeNotifyEvents.FMessages.Count);
          FNodeNotifyEvents.FOnNodeMessageEvent(TNetConnection(FNodeNotifyEvents.FMessages.Objects[i]),
            FNodeNotifyEvents.FMessages.Strings[i]);
        end;
      end;
      FNodeNotifyEvents.FMessages.Clear;
    end;
  except
    on e: Exception do
    begin
      TLog.NewLog(lterror, Classname, 'Exception inside a Synchronized process: ' + e.Classname + ':' + e.Message +
        ' Step:' + DebugStep);
    end;
  end;
end;

end.
