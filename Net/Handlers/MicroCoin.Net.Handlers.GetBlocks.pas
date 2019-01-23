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
  | File:       MicroCoin.Net.Handlers.GetBlocks.pas                             |
  | Created at: 2018-09-21                                                       |
  | Purpose:    GetBlocks message handler                                        |
  |============================================================================== }
unit MicroCoin.Net.Handlers.GetBlocks;

interface

uses Classes, SysUtils, MicroCoin.Net.CommandHandler,
  MicroCoin.Net.ConnectionManager, MicroCoin.Common.Config, ULog,
  MicroCoin.Account.AccountKey, MicroCoin.Net.NodeServer, UTime,
  UThread, MicroCoin.Net.Utils, UCrypto, UBaseTypes,
  MicroCoin.BlockChain.Block, MicroCoin.Account.Storage,
  MicroCoin.Net.Protocol, MicroCoin.Net.Connection, MicroCoin.Node.Node;

type
  TGetBlocksHandler = class(TInterfacedObject, ICommandHandler)
  private
    procedure HandleRequest(AHeader: TNetHeaderData; AData: TStream; AConnection: TObject);
    procedure HandleResponse(AHeader: TNetHeaderData; AData: TStream; AConnection: TObject);
  public
    procedure HandleCommand(AHeader: TNetHeaderData; AData: TStream; AConnection: TObject);
  end;

implementation

{ TGetBlocksHandler }

procedure TGetBlocksHandler.HandleCommand(AHeader: TNetHeaderData; AData: TStream; AConnection: TObject);
begin
  if AHeader.HeaderType = ntp_request then
    HandleRequest(AHeader, AData, AConnection)
  else if AHeader.HeaderType = ntp_response then
    HandleResponse(AHeader, AData, AConnection)
  else
    raise Exception.Create('Invalid request/response');
end;

procedure TGetBlocksHandler.HandleRequest(AHeader: TNetHeaderData; AData: TStream; AConnection: TObject);
var
  b: Cardinal;
  xBlock: TBlock;
  xMemoryStream: TMemoryStream;
  c: Cardinal;
  Connection: TNetConnection;
  xMessage: TNetMessage_GetBlocks;
  xResponse: TNetMessage_GetBlocks_Response;
begin
  xMessage := TNetMessage_GetBlocks.LoadFromStream(AData);

  if (xMessage.EndBlock >= TConnectionManager.Instance.Bank.BlocksCount) then
    xMessage.EndBlock := TConnectionManager.Instance.Bank.BlocksCount - 1;
  Connection := AConnection as TNetConnection;
  xMemoryStream := TMemoryStream.Create;
  try
    try
      xResponse.Count := xMessage.EndBlock - xMessage.StartBlock + 1;
      c:=0;
      for b := xMessage.StartBlock to xMessage.EndBlock do
      begin
        Inc(c);
        xBlock := TBlock.Create(nil);
        xBlock.BlockManager := TConnectionManager.Instance.Bank;
        if TConnectionManager.Instance.Bank.LoadTransactions(xBlock, b) then
          xResponse.Blocks[c - 1] := xBlock
        else
        begin
          Connection.Send_Error(ntp_response, AHeader.Operation, AHeader.RequestId, cNetError_InternalServerError,
            'Operations of block:' + Inttostr(b) + ' not found');
          exit;
        end;
      end;
      xResponse.SaveToStream(xMemoryStream);
      xMemoryStream.Position := 0;
      Connection.Send(ntp_response, AHeader.Operation, 0, AHeader.RequestId, xMemoryStream);
    finally
     // xBlock.Free;
    end;
  finally
    xMemoryStream.Free;
  end;
  LogDebug( Classname, 'Sending operations from block ' + Inttostr(xMessage.StartBlock) + ' to ' +
    Inttostr(xMessage.EndBlock));
end;

procedure TGetBlocksHandler.HandleResponse(AHeader: TNetHeaderData; AData: TStream; AConnection: TObject);
var
  xBlock: TBlock;
  i: Cardinal;
  xNewBlockAccount: TAccountStorageEntry;
  xErrors: AnsiString;
  xMessage: TNetMessage_GetBlocks_Response;
  Connection: TNetConnection;
begin
  xMessage := TNetMessage_GetBlocks_Response.LoadFromStream(AData);
  Connection := AConnection as TNetConnection;
  if xMessage.Count > 0 then
  begin
    for i := 0 to xMessage.Count - 1 do
    begin
      xBlock := xMessage.Blocks[i];
      if (xBlock.BlockHeader.Block = TNode.Node.BlockManager.BlocksCount) then
      begin
        if not(TNode.Node.BlockManager.AddNewBlockToBlockChain(xBlock,
          TConnectionManager.Instance.NetworkAdjustedTime.GetMaxAllowedTimestampForNewBlock, xNewBlockAccount, xErrors))
        then
        begin
          TLog.NewLog(ltInfo, Classname, 'Distinct operation block found! My:' +
            TNode.Node.BlockManager.AccountStorage.Blocks[TNode.Node.BlockManager.BlocksCount - 1].BlockHeader.ToString
            () + ' remote:' + xBlock.BlockHeader.ToString() + ' Errors: ' + xErrors);
        end;
      end else begin
        TLog.NewLog(ltError, Classname, 'Received a distinct block, finalizing: ' + xBlock.BlockHeader.ToString() +
          ' (My block: ' + TNode.Node.BlockManager.LastBlock.ToString() + ')');
        Connection.IsDownloadingBlocks := false;
        exit;
      end;
    end;
    Connection.IsDownloadingBlocks := false;
    if ((xMessage.Count > 0) and (Connection.RemoteOperationBlock.Block >= TNode.Node.BlockManager.BlocksCount)) then
      Connection.Send_GetBlocks(TNode.Node.BlockManager.BlocksCount, 100, i);
    TNode.Node.NotifyBlocksChanged;
  end;
end;

initialization

TNetConnection.AddHandler(cNetOp_GetBlocks, TGetBlocksHandler);

end.
