{ ==============================================================================|
  | MicroCoin                                                                    |
  | Copyright (c) 2019 MicroCoin Developers                                      |
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
  | File:       MicroCoin.Net.Handlers.NewTransaction.pas
  | Created at: 2019-01-22
  | Purpose:
  |============================================================================== }
unit MicroCoin.Net.Handlers.NewTransaction;

interface

uses Classes, SysUtils, MicroCoin.Net.CommandHandler,
  MicroCoin.Net.ConnectionManager, MicroCoin.Common.Config, ULog,
  MicroCoin.Account.AccountKey, MicroCoin.Net.NodeServer, UTime,
  UThread, MicroCoin.Net.Utils, UCrypto, UBaseTypes,
  MicroCoin.Transaction.Manager, MicroCoin.Transaction.ITransaction,
  MicroCoin.Transaction.HashTree,
  MicroCoin.Net.Protocol, MicroCoin.Net.Connection, MicroCoin.Node.Node;

type
  TNewTransactionHandler = class(TInterfacedObject, ICommandHandler)
  public
    procedure HandleCommand(AHeader: TNetHeaderData; AData: TStream; AConnection: TObject);
  end;

implementation

{ TNewTransactionHandler }

procedure TNewTransactionHandler.HandleCommand(AHeader: TNetHeaderData; AData: TStream; AConnection: TObject);
var
  c, i: Integer;
  errors: AnsiString;
  xMessage: TNetMessage_NewTransaction;
  Connection: TNetConnection;
  xTransaction: ITransaction;
begin
  Connection := AConnection as TNetConnection;
  if AHeader.HeaderType <> ntp_autosend then
    raise Exception.Create('Not autosend');
  xMessage := TNetMessage_NewTransaction.LoadFromStream(AData);

  try
    Connection.BufferLock.Acquire;
    try
      for i := 0 to xMessage.Transactions.TransactionCount - 1 do
      begin
        xTransaction := xMessage.Transactions.GetTransaction(i);
        Connection.BufferReceivedOperationsHash.Add(xTransaction.Sha256);
        c := Connection.BufferToSendOperations.IndexOf(xTransaction);
        if (c >= 0) then
          Connection.BufferToSendOperations.Delete(c);
      end;
    finally
      Connection.BufferLock.Release;
    end;
    TNode.Node.AddOperations(Connection, xMessage.Transactions, nil, errors);
  finally
    xMessage.Transactions.Free;
  end;
end;

initialization

TNetConnection.AddHandler(cNetOp_AddOperations, TNewTransactionHandler);

end.
