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
  | File:       MicroCoin.Net.Handlers.GetOpBlocks.pas
  | Created at: 2019-01-22
  | Purpose:
  |============================================================================== }

unit MicroCoin.Net.Handlers.GetOpBlocks;

interface

uses Classes, SysUtils, MicroCoin.Net.CommandHandler,
  MicroCoin.Net.ConnectionManager, MicroCoin.Common.Config, ULog,
  MicroCoin.Account.AccountKey, MicroCoin.Net.NodeServer, UTime,
  MicroCoin.BlockChain.Block,
  UThread, MicroCoin.Net.Utils, UCrypto, UBaseTypes, MicroCoin.BlockChain.BlockHeader,
  MicroCoin.Net.Protocol, MicroCoin.Net.Connection, MicroCoin.Node.Node;

type
  TGetOpBlockHandler = class(TInterfacedObject, ICommandHandler)
  public
    procedure HandleCommand(AHeader: TNetHeaderData; AData: TStream; AConnection: TObject);
  end;

implementation

{ TGetOpBlockHandler }

procedure TGetOpBlockHandler.HandleCommand(AHeader: TNetHeaderData; AData: TStream; AConnection: TObject);
const
  CT_Max_Positions = 10;
var
  inc_b, b, total_b: Cardinal;
  db, msops: TMemoryStream;
  errors, blocksstr: AnsiString;
  ob: TBlockHeader;
  xMessage: TNetMessage_GetBlocks;
  Connection: TNetConnection;
begin
  blocksstr := '';
  Connection := AConnection as TNetConnection;
  if AHeader.HeaderType <> ntp_request then
    exit; // Old response

  xMessage := TNetMessage_GetBlocks.LoadFromStream(AData);

  if (xMessage.StartBlock < 0) or (xMessage.StartBlock > xMessage.EndBlock) or
    (xMessage.StartBlock >= TNode.Node.BlockManager.BlocksCount) then
    raise Exception.CreateFmt('Invalid start (%d) or end (%d). Count: %d', [xMessage.StartBlock, xMessage.EndBlock,
      TNode.Node.BlockManager.BlocksCount]);

  if xMessage.StartBlock < TNode.Node.BlockManager.Storage.FirstBlock then
  begin
    xMessage.StartBlock := TNode.Node.BlockManager.Storage.FirstBlock;
    if xMessage.EndBlock < xMessage.StartBlock then
    begin
      errors := 'Block:' + Inttostr(xMessage.EndBlock) + ' not found';
      Connection.Send_Error(ntp_response, AHeader.Operation, AHeader.RequestId, cNetError_InternalServerError, errors);
      exit;
    end;
  end;

  if (xMessage.EndBlock >= TNode.Node.BlockManager.BlocksCount) then
    xMessage.EndBlock := TNode.Node.BlockManager.BlocksCount - 1;
  inc_b := ((xMessage.EndBlock - xMessage.StartBlock) div CT_Max_Positions) + 1;
  msops := TMemoryStream.Create;
  try
    b := xMessage.StartBlock;
    total_b := 0;
    repeat
      ob := TNode.Node.BlockManager.AccountStorage.Blocks[b].BlockHeader;
      if TBlock.SaveBlockToStream(ob, msops) then
      begin
        blocksstr := blocksstr + Inttostr(b) + ',';
        b := b + inc_b;
        Inc(total_b);
      end
      else
      begin
        errors := 'ERROR DEV 20170522-1 block:' + Inttostr(b);
        Connection.Send_Error(ntp_response, AHeader.Operation, AHeader.RequestId,
          cNetError_InternalServerError, errors);
        exit;
      end;
    until (b > xMessage.EndBlock);
    db := TMemoryStream.Create;
    try
      db.Write(total_b, 4);
      db.WriteBuffer(msops.Memory^, msops.Size);
      Connection.Send(ntp_response, AHeader.Operation, 0, AHeader.RequestId, db);
    finally
      db.Free;
    end;
  finally
    msops.Free;
  end;
  LogDebug(Classname, 'Sending ' + Inttostr(total_b) + ' operations block from block ' +
    Inttostr(xMessage.StartBlock) + ' to ' + Inttostr(xMessage.EndBlock) + ' ' + blocksstr);
end;

initialization

TNetConnection.AddHandler(cNetOp_GetOperationsBlock, TGetOpBlockHandler);

end.
