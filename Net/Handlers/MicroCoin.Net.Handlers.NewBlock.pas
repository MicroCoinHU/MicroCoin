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
  | File:       MicroCoin.Net.Handlers.NewBlock.pas                              |
  | Created at: 2018-09-21                                                       |
  | Purpose:    New block message handler                                        |
  |============================================================================== }
unit MicroCoin.Net.Handlers.NewBlock;

interface

uses Classes, SysUtils, MicroCoin.Net.CommandHandler,
  MicroCoin.Net.ConnectionManager, MicroCoin.Common.Config, ULog,
  MicroCoin.Account.AccountKey, MicroCoin.Net.NodeServer, UTime,
  UThread, MicroCoin.Net.Utils, UCrypto, UBaseTypes, MicroCoin.Account.Storage,
  MicroCoin.Net.Protocol, MicroCoin.Net.Connection, MicroCoin.Node.Node;

type
  TNewBlockHandler = class(TInterfacedObject, ICommandHandler)
  public
    procedure HandleCommand(AHeader: TNetHeaderData; AData: TStream; AConnection: TObject);
  end;

implementation

{ TNewBlockHandler }

procedure TNewBlockHandler.HandleCommand(AHeader: TNetHeaderData; AData: TStream; AConnection: TObject);
var
  xMessage: TNetMessage_NewBlock;
  Connection: TNetConnection;
  xStorageEntry: TAccountStorageEntry;
  xErrors: ansistring;
begin
  if AHeader.HeaderType <> ntp_autosend then
    raise Exception.Create('Not autosend');
  xMessage := TNetMessage_NewBlock.LoadFromStream(AData);
  Connection := AConnection as TNetConnection;
  Connection.RemoteAccumulatedWork := xMessage.RemoteWork;
  if xMessage.RemoteWork = 0 then
  begin
    if (xMessage.NewBlock.BlockHeader.Block > TNode.Node.BlockManager.BlocksCount) then
    begin
      TConnectionManager.Instance.GetNewBlockChainFromClient(Connection, Format('BlocksCount:%d > my BlocksCount:%d',
        [xMessage.NewBlock.BlockHeader.Block + 1, TNode.Node.BlockManager.BlocksCount]));
    end
    else if (xMessage.NewBlock.BlockHeader.Block = TNode.Node.BlockManager.BlocksCount) then
    begin
      if not TNode.Node.AddNewBlockChain(Connection, xMessage.NewBlock, xStorageEntry, xErrors) then
        TConnectionManager.Instance.GetNewBlockChainFromClient(Connection, 'Has a distinct block. ' + xErrors);
    end;
  end
  else
  begin
    if (xMessage.RemoteWork > TNode.Node.BlockManager.AccountStorage.WorkSum) then
    begin
      if (xMessage.NewBlock.BlockHeader.Block = TNode.Node.BlockManager.BlocksCount) then
      begin
        if not TNode.Node.AddNewBlockChain(Connection, xMessage.NewBlock, xStorageEntry, xErrors) then
        begin
          if (xMessage.NewBlock.BlockHeader.Block = TNode.Node.BlockManager.BlocksCount) then
            TConnectionManager.Instance.GetNewBlockChainFromClient(Connection,
              'Higher Work with same block height. I''m a orphan blockchain candidate');
        end;
      end
      else
      begin
        TConnectionManager.Instance.GetNewBlockChainFromClient(Connection,
          Format('Higher Work and distinct blocks count. Need to download BlocksCount:%d  my BlocksCount:%d',
          [xMessage.NewBlock.BlockHeader.Block + 1, TNode.Node.BlockManager.BlocksCount]));
      end;
    end;
  end;
  xMessage.NewBlock.Free;
end;

initialization

TNetConnection.AddHandler(cNetOp_NewBlock, TNewBlockHandler);

end.
