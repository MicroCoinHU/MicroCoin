unit MicroCoin.Net.Protocol;
{
  This unit contains code from PascalCoin:

  Copyright (c) Albert Molina 2016 - 2018 original code from PascalCoin https://pascalcoin.org/

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

interface

uses MicroCoin.BlockChain.BlockHeader,
     MicroCoin.Net.NodeServer;

const

  CT_MagicRequest = $0001;
  CT_MagicResponse = $0002;
  CT_MagicAutoSend = $0003;

  CT_NetOp_Hello = $0001; // Sends my last operationblock + servers. Receive last operationblock + servers + same operationblock number of sender
  CT_NetOp_Error = $0002;
  CT_NetOp_Message = $0003;
  CT_NetOp_GetBlocks = $0010;
  CT_NetOp_GetOperationsBlock = $0005; // Sends from and to. Receive a number of OperationsBlock to check
  CT_NetOp_NewBlock = $0011;
  CT_NetOp_AddOperations = $0020;
  CT_NetOp_GetSafeBox = $0021; // V2 Protocol: Allows to send/receive Safebox in chunk parts

  CT_NetError_InvalidProtocolVersion = $0001;
  CT_NetError_IPBlackListed = $0002;
  CT_NetError_InvalidDataBufferInfo = $0010;
  CT_NetError_InternalServerError = $0011;
  CT_NetError_InvalidNewAccount = $0012;
  CT_NetError_SafeboxNotFound = $00020;

  CT_LAST_CONNECTION_BY_SERVER_MAX_MINUTES = 60 * 60 * 3;
  CT_LAST_CONNECTION_MAX_MINUTES = 60 * 60;
  CT_MAX_NODESERVERS_ON_HELLO = 10;
  CT_MIN_NODESERVERS_BUFFER = 50;
  CT_MAX_NODESERVERS_BUFFER = 300;

type
  TNetTransferType = (ntp_unknown, ntp_request, ntp_response, ntp_autosend);

const
  CT_NetTransferType: array [TNetTransferType] of AnsiString = ('Unknown', 'Request', 'Response', 'Autosend');

type
  TNetProtocolVersion = record
    protocol_version, protocol_available: Word;
  end;

  TNetHeaderData = record
    header_type: TNetTransferType;
    protocol: TNetProtocolVersion;
    operation: Word;
    request_id: Cardinal;
    buffer_data_length: Cardinal;
    //
    is_error: Boolean;
    error_code: Integer;
    error_text: AnsiString;
  end;

const
  CT_NetHeaderData: TNetHeaderData = (header_type: ntp_unknown; protocol: (protocol_version: 0; protocol_available: 0); operation: 0; request_id: 0; buffer_data_length: 0; is_error: false;
    error_code: 0; error_text: '');

type

  TNetMessage_Hello = record
    last_operation: TBlockHeader;
    servers_address: array of TNodeServer;
  end;



implementation

end.
