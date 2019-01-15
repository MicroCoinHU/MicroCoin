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
| This unit contains portions from PascalCoin                                  |
| Copyright (c) Albert Molina 2016 - 2018                                      |
|                                                                              |
| Distributed under the MIT software license, see the accompanying file        |
| LICENSE or visit http://www.opensource.org/licenses/mit-license.php.         |
|==============================================================================|
| File:       MicroCoin.Net.Protocol.pas                                       |
| Created at: 2018-09-14                                                       |
| Purpose:    Net protocol definitions and constants                           |
|==============================================================================}
unit MicroCoin.Net.Protocol;

{$ifdef FPC}
  {$mode delphi}
{$endif}


interface

uses MicroCoin.BlockChain.BlockHeader,
  MicroCoin.Net.NodeServer, SysUtils;

const

  cMagicRequest = $0001;
  cMagicResponse = $0002;
  cMagicAutoSend = $0003;

  cNetOp_Hello = $0001;
  // Sends my last operationblock + servers. Receive last operationblock + servers + same operationblock number of sender
  cNetOp_Error = $0002;
  cNetOp_Message = $0003;
  cNetOp_GetBlocks = $0010;
  cNetOp_GetOperationsBlock = $0005; // Sends from and to. Receive a number of OperationsBlock to check
  cNetOp_NewBlock = $0011;
  cNetOp_AddOperations = $0020;
  cNetOp_GetAccountStorage = $0021; // V2 Protocol: Allows to send/receive Safebox in chunk parts

  cNetError_InvalidProtocolVersion = $0001;
  cNetError_IPBlackListed = $0002;
  cNetError_InvalidDataBufferInfo = $0010;
  cNetError_InternalServerError = $0011;
  cNetError_InvalidNewAccount = $0012;
  cNetError_AccountStorageNotFound = $00020;

  cLAST_CONNECTION_BY_SERVER_MAX_MINUTES = 60 * 60 * 3;
  cLAST_CONNECTION_MAX_MINUTES = 60 * 60;
  cMAX_NODESERVERS_ON_HELLO = 10;
  cMIN_NODESERVERS_BUFFER = 50;
  cMAX_NODESERVERS_BUFFER = 300;

type
  TNetTransferType = (ntp_unknown, ntp_request, ntp_response, ntp_autosend);

const
  CT_NetTransferType: array [TNetTransferType] of AnsiString = ('Unknown', 'Request', 'Response', 'Autosend');

type
  TNetProtocolVersion = record
    protocol_version, protocol_available: Word;
  end;

  TNetHeaderData = record
  private
    function GetOperationTxt: AnsiString;
  public
    HeaderType: TNetTransferType;
    Protocol: TNetProtocolVersion;
    Operation: Word;
    RequestId: Cardinal;
    BufferDataLength: Cardinal;
    //
    IsError: Boolean;
    ErrorCode: Integer;
    ErrorText: AnsiString;
    class function Empty: TNetHeaderData; static;
    function ToString: AnsiString;
    property OperationTxt : AnsiString read GetOperationTxt;
  end;

  TNetMessage_Hello = record
    last_operation: TBlockHeader;
    servers_address: array of TNodeServer;
  end;

implementation

{ TNetHeaderData }

class function TNetHeaderData.Empty: TNetHeaderData;
begin
  Result.HeaderType := ntp_unknown;
  Result.Protocol.protocol_version := 0;
  Result.Protocol.protocol_available := 0;
  Result.Operation := 0;
  Result.RequestId := 0;
  Result.BufferDataLength := 0;
  Result.IsError := false;
  Result.ErrorCode := 0;
  Result.ErrorText := '';
end;

function TNetHeaderData.GetOperationTxt: AnsiString;
begin
  case operation of
    cNetOp_Hello:
      Result := 'HELLO';
    cNetOp_Error:
      Result := 'ERROR';
    cNetOp_GetBlocks:
      Result := 'GET BLOCKS';
    cNetOp_Message:
      Result := 'MESSAGE';
    cNetOp_GetOperationsBlock:
      Result := 'GET OPERATIONS BLOCK';
    cNetOp_NewBlock:
      Result := 'NEW BLOCK';
    cNetOp_AddOperations:
      Result := 'ADD OPERATIONS';
    cNetOp_GetAccountStorage:
      Result := 'GET SAFEBOX';
  else
    Result := 'UNKNOWN OPERATION ' + IntToHex(operation, 4);
  end;
end;

function TNetHeaderData.ToString: AnsiString;
begin
  Result := CT_NetTransferType[HeaderType] + ' Operation:' + OperationTxt;
  if IsError then
  begin
    Result := Result + ' ERRCODE:' + Inttostr(ErrorCode) + ' ERROR:' + ErrorText;
  end
  else
  begin
    Result := Result + ' ReqId:' + Inttostr(RequestId) + ' BufferSize:' +
      Inttostr(BufferDataLength);
  end;
end;

end.
