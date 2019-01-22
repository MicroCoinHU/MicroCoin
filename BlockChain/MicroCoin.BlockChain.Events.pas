unit MicroCoin.BlockChain.Events;
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

uses UThread, MicroCoin.Transaction.HashTree,
  MicroCoin.BlockChain.Block, ULog, SysUtils,
  MicroCoin.Transaction.Events, MicroCoin.Net.Connection;

type

  TNotifyNewBlockThread = class(TPCThread)
    FNetConnection: TNetConnection;
    FSanitizedHashTree: TTransactionHashTree;
    FNewBlockTransactions: TBlock;
  protected
    procedure BCExecute; override;
  public
    constructor Create(NetConnection: TNetConnection; MakeACopyOfNewBlockOperations: TBlock;
      MakeACopyOfSanitizedOperationsHashTree: TTransactionHashTree);
    destructor Destroy; override;
  end;

implementation

uses MicroCoin.Net.ConnectionManager;

procedure TNotifyNewBlockThread.BCExecute;
begin
  if TConnectionManager.Instance.ConnectionLock(Self, FNetConnection, 500) then
  begin
    try
      if not FNetConnection.Connected then
        exit;
      LogDebug(Classname, 'Sending new block found to ' + FNetConnection.Client.ClientRemoteAddr);
      FNetConnection.Send_NewBlockFound(FNewBlockTransactions);
      if not FNetConnection.Connected then
        exit;
      if FSanitizedHashTree.TransactionCount > 0 then
      begin
        LogDebug(Classname, 'Sending ' + IntToStr(FSanitizedHashTree.TransactionCount) +
          ' sanitized operations to ' + FNetConnection.ClientRemoteAddr);
        TNotifyTransactionThread.Create(FNetConnection, FSanitizedHashTree);
      end;
    finally
      TConnectionManager.Instance.ConnectionUnlock(FNetConnection);
    end;
  end;
end;

constructor TNotifyNewBlockThread.Create(NetConnection: TNetConnection; MakeACopyOfNewBlockOperations: TBlock;
  MakeACopyOfSanitizedOperationsHashTree: TTransactionHashTree);
begin
  FNetConnection := NetConnection;
  FSanitizedHashTree := TTransactionHashTree.Create;
  FSanitizedHashTree.CopyFromHashTree(MakeACopyOfSanitizedOperationsHashTree);
  FNewBlockTransactions := TBlock.Create(nil);
  FNewBlockTransactions.CopyFrom(MakeACopyOfNewBlockOperations);
  inherited Create(false);
  FreeOnTerminate := true;
end;

destructor TNotifyNewBlockThread.Destroy;
begin
  FreeAndNil(FSanitizedHashTree);
  FreeAndNil(FNewBlockTransactions);
  inherited;
end;

end.
