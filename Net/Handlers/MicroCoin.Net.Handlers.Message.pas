{==============================================================================|
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
| File:       MicroCoin.Net.Handlers.Message.pas                               |
| Created at: 2018-09-21                                                       |
| Purpose:    Message message handler                                          |
|==============================================================================}
unit MicroCoin.Net.Handlers.Message;

interface

uses Classes, SysUtils, MicroCoin.Net.CommandHandler,
     MicroCoin.Net.ConnectionManager, MicroCoin.Common.Config, ULog,
     MicroCoin.Account.AccountKey, MicroCoin.Net.NodeServer, UTime,
     UThread, MicroCoin.Net.Utils, UCrypto, UBaseTypes,
     MicroCoin.Net.Protocol, MicroCoin.Net.Connection, MicroCoin.Node.Node;

type
  TMessageHandler = class(TInterfacedObject, ICommandHandler)
  public
    procedure HandleCommand(AHeader: TNetHeaderData; AData: TStream; AConnection: TObject);
  end;


implementation

{ TMessageHandler }

procedure TMessageHandler.HandleCommand(AHeader: TNetHeaderData; AData: TStream;
  AConnection: TObject);
var
  Connection: TNetConnection;
  xMessage : TNetMessage_Message;
begin
  if AHeader.HeaderType <> ntp_autosend
  then raise Exception.Create('Not autosend');

  Connection := AConnection as TNetConnection;
  xMessage := TNetMessage_Message.LoadFromStream(AData);
  if Length(xMessage.Message)<1 then raise Exception.Create('Invalid Message');

  if TCrypto.IsHumanReadable(xMessage.Message) then
    TLog.NewLog(ltInfo, Classname, 'Received new message from ' + Connection.ClientRemoteAddr + ' Message (' +
      Inttostr(length(xMessage.Message)) + ' bytes): ' + xMessage.Message)
  else
    TLog.NewLog(ltInfo, Classname, 'Received new message from ' + Connection.ClientRemoteAddr + ' Message (' +
      Inttostr(length(xMessage.Message)) + ' bytes) in hexadecimal: ' + TBaseType.ToHexaString(xMessage.Message));
  try
    TNode.Node.NotifyNetClientMessage(Connection, xMessage.Message);
  except
    on E: Exception do
    begin
      TLog.NewLog(ltError, Classname, 'Error processing received message. ' + E.Classname + ' ' + E.Message);
    end;
  end;
end;

initialization
  TNetConnection.AddHandler(cNetOp_Message, TMessageHandler);
end.
