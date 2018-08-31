unit MicroCoin.Transaction.Events;
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

uses UThread,
  MicroCoin.Net.Connection, MicroCoin.Transaction.HashTree;

type
  TNotifyTransactionThread = class(TPCThread)
    FNetConnection: TNetConnection;
  protected
    procedure BCExecute; override;
  public
    constructor Create(NetConnection: TNetConnection; MakeACopyOfOperationsHashTree: TTransactionHashTree);
    destructor Destroy; override;
  end;

implementation

uses MicroCoin.Net.ConnectionManager;

procedure TNotifyTransactionThread.BCExecute;
begin
  Sleep(Random(5000)); // Delay 0..5 seconds to allow receive data and don't send if not necessary
  if TConnectionManager.Instance.ConnectionLock(Self, FNetConnection, 500) then
  begin
    try
      if not FNetConnection.Connected then
        exit;
      FNetConnection.Send_AddOperations(nil);
    finally
      TConnectionManager.Instance.ConnectionUnlock(FNetConnection);
    end;
  end;
end;

constructor TNotifyTransactionThread.Create(NetConnection: TNetConnection;
  MakeACopyOfOperationsHashTree: TTransactionHashTree);
begin
  FNetConnection := NetConnection;
  FNetConnection.AddOperationsToBufferForSend(MakeACopyOfOperationsHashTree);
  inherited Create(false);
  FreeOnTerminate := true;
end;

destructor TNotifyTransactionThread.Destroy;
begin
  inherited;
end;

end.
