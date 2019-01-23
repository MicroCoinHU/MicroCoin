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
  | File:       MicroCoin.Net.Handlers.AccountStorage.pas
  | Created at: 2019-01-22
  | Purpose:
  |============================================================================== }
unit MicroCoin.Net.Handlers.AccountStorage;

interface

uses Classes, SysUtils, MicroCoin.Net.CommandHandler, UbaseTypes,
  MicroCoin.Net.ConnectionManager, MicroCoin.Common.Config, ULog,
  MicroCoin.Account.AccountKey, MicroCoin.Net.NodeServer, UTime, UChunk,
  UThread, MicroCoin.Net.Utils, UCrypto, MicroCoin.Account.Storage,
  MicroCoin.Net.Protocol, MicroCoin.Net.Connection, MicroCoin.Node.Node;

type
  TGetAccountStorageHandler = class(TInterfacedObject, ICommandHandler)
  public
    procedure HandleCommand(AHeader: TNetHeaderData; AData: TStream; AConnection: TObject);
  end;

implementation

{ TGetAccountStorageHandler }

procedure TGetAccountStorageHandler.HandleCommand(AHeader: TNetHeaderData; AData: TStream; AConnection: TObject);
var
  xAccountStorageStream: TStream;
  xResponseStream: TStream;
  antPos: Int64;
  xAccountStorageHeader: TAccountStorageHeader;
  xErrors: AnsiString;
  xMessage: TNetMessage_AccountStorage_Request;
  Connection: TNetConnection;
begin
  Connection := AConnection as TNetConnection;
  if AHeader.HeaderType <> ntp_request
  then raise Exception.Create('Not request');
  xAccountStorageStream := TNode.Node.BlockManager.Storage.CreateAccountStorageStream(xMessage.Count);
  try
    xResponseStream := TMemoryStream.Create;
    try
      if not Assigned(xAccountStorageStream) then
      begin
        Connection.Send_Error(ntp_response, AHeader.Operation, cNetError_AccountStorageNotFound, AHeader.RequestId,
          Format('Account storage entry for block %d not found', [xMessage.Count]));
        exit;
      end;
      antPos := xAccountStorageStream.Position;
      xAccountStorageHeader := TAccountStorageHeader.LoadFromStream(xAccountStorageStream);
      if xAccountStorageHeader.AccountStorageHash <> xMessage.Hash then
      begin
        raise Exception.CreateFmt('Invalid safeboxhash on GetSafeBox request (Real:%s > Requested:%s)',
          [TBaseType.ToHexaString(xAccountStorageHeader.AccountStorageHash), TBaseType.ToHexaString(xMessage.Hash)]);
      end;
      xAccountStorageStream.Position := antPos;
      if not TPCChunk.SaveChunkFromAccountStorage(xAccountStorageStream, xResponseStream, xMessage.StartBlock,
        xMessage.EndBlock, xErrors) then
      begin
        TLog.NewLog(ltError, Classname, 'Error saving chunk: ' + xErrors);
        exit;
      end;
      Connection.Send(ntp_response, AHeader.Operation, 0, AHeader.RequestId, xResponseStream);
    finally
      xResponseStream.Free;
    end;
  finally
    FreeAndNil(xAccountStorageStream);
  end;
end;

initialization

TNetConnection.AddHandler(cNetOp_GetAccountStorage, TGetAccountStorageHandler);

end.
