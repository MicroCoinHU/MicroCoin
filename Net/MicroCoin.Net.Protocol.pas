unit MicroCoin.Net.Protocol;
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

uses MicroCoin.BlockChain.BlockHeader,
  MicroCoin.Net.NodeServer;

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

end.
