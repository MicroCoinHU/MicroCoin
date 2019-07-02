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

uses MicroCoin.BlockChain.BlockHeader, UbaseTypes,
  UECIES, MicroCoin.Common, MicroCoin.Transaction.HashTree,
  MicroCoin.Transaction.Manager, MicroCoin.Transaction.ITransaction,
  MicroCoin.Net.NodeServer, SysUtils, MicroCoin.Account.AccountKey,
  MicroCoin.Common.Stream, Classes, MicroCoin.BlockChain.Block;

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
    IsError: Boolean;
    ErrorCode: Integer;
    ErrorText: AnsiString;
    class function Empty: TNetHeaderData; static;
    function ToString: AnsiString;
    property OperationTxt : AnsiString read GetOperationTxt;
  end;

  TNetMessage_Hello = record
    server_port : word;
    accountKey : TAccountKey;
    timestamp: UInt32;
    RemoteHost: string;
    last_operation: TBlockHeader;
    Block : TBlock;
    nodeserver_count: UInt32;
    nodeservers : TNodeServerAddressArray;
    client_version: ansiString;
    remote_work: UInt64;
    class function LoadFromStream(AStream : TStream) : TNetMessage_Hello; static;
    procedure SaveToStream(AStream : TStream);
  end;

  TNetMessage_Message = record
    Message : AnsiString;
    class function LoadFromStream(AStream : TStream) : TNetMessage_Message; static;
  end;

  TNetMessage_NewBlock = record
    NewBlock : TBlock;
    RemoteWork : UInt64;
    class function LoadFromStream(AStream : TStream) : TNetMessage_NewBlock; static;
  end;

  TNetMessage_GetBlocks = record
    StartBlock: Cardinal;
    EndBlock: Cardinal;
    class function LoadFromStream(AStream : TStream) : TNetMessage_GetBlocks; static;
  end;

  TNetMessage_NewTransaction = record
    Count: Cardinal;
    Transactions: TTransactionHashTree;
    class function LoadFromStream(AStream : TStream) : TNetMessage_NewTransaction; static;
  end;

  TNetMessage_AccountStorage_Request = record
    Count: Cardinal;
    Hash: TRawBytes;
    StartBlock, EndBlock: Cardinal;
    class function LoadFromStream(AStream : TStream) : TNetMessage_AccountStorage_Request; static;
  end;

  TBlockArray = array of TBlock;
  TNetMessage_GetBlocks_Response = record
  private
    FCount: Int32;
    FBlocks: TBlockArray;
    FDestructor : IDestructor;
    procedure SetCount(const Value: Int32);
    class procedure CleanUp(ABlocks : TBlockArray); static;
  public
    class function LoadFromStream(AStream : TStream) : TNetMessage_GetBlocks_Response; static;
    procedure SaveToStream(AStream : TStream);
    property Count : Int32 read FCount Write SetCount;
    property Blocks: TBlockArray read FBlocks;
  end;

implementation

uses MicroCoin.Net.ConnectionManager, MicroCoin.Node.Node, MicroCoin.Crypto.Keys;

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
  if IsError
  then Result := Result + ' ERRCODE:' + Inttostr(ErrorCode) + ' ERROR:' + ErrorText
  else Result := Result + ' ReqId:' + Inttostr(RequestId) + ' BufferSize:' +
      Inttostr(BufferDataLength);
end;

{ TNetMessage_Hello }

class function TNetMessage_Hello.LoadFromStream(AStream: TStream): TNetMessage_Hello;
var
  xErrors: ansiString;
  i: Integer;
  xTmp: AnsiString;
begin

  if AStream.Read(Result.server_port, sizeof(Result.server_port)) <> sizeof(result.server_port)
  then raise Exception.Create('Invalid hello');

  Result.accountKey := TAccountKey.Empty;
  AStream.ReadAnsiString(xTmp);
  Result.accountKey := TAccountKey.FromRawString(xTmp);

  if not Result.accountKey.IsValidAccountKey(xErrors)
  then raise Exception.Create(xErrors);

  if AStream.Read(Result.timestamp, sizeof(result.timestamp)) <> sizeof(result.timestamp)
  then raise Exception.Create('Invalid hello');

  Result.Block := TBlock.Create(nil);

  if not Result.Block.LoadBlockFromStream(AStream, xErrors)
  then raise Exception.Create(xErrors);

  if AStream.Size >= AStream.Position + SizeOf(Result.nodeserver_count)
  then begin
    AStream.Read( Result.nodeserver_count, SizeOf(Result.nodeserver_count));
    SetLength(Result.nodeservers, Result.nodeserver_count);
    for i := 0 to Result.nodeserver_count - 1 do begin
      Result.nodeservers[i] := TNodeServer.LoadFromStream(AStream);
    end;
    AStream.ReadAnsiString(Result.client_version);
    AStream.Read(Result.remote_work, sizeof(Result.remote_work))
  end;

end;

procedure TNetMessage_Hello.SaveToStream(AStream: TStream);
var
  xNs: TNodeServer;
begin
  AStream.Write( server_port, SizeOf(server_port) );
  AStream.WriteAnsiString( accountKey.ToRawString );
  AStream.Write(timestamp, sizeof(timestamp));
  TBlock.SaveBlockToStream(Block.BlockHeader, AStream);
  AStream.Write(nodeserver_count, SizeOf(nodeserver_count));
  for xNs in nodeservers do begin
    AStream.WriteAnsiString(xNs.ip);
    AStream.Write(xNs.port, SizeOf(xNs.port));
    AStream.Write(xNs.last_connection, SizeOf(xNs.last_connection));
  end;
  AStream.WriteAnsiString(client_version);
  AStream.Write(remote_work, SizeOf(remote_work));
end;

{ TNetMessage_Message }

class function TNetMessage_Message.LoadFromStream(
  AStream: TStream): TNetMessage_Message;
var
  xMsg : AnsiString;
begin
  AStream.ReadAnsiString(xMsg);
  if not ECIESDecrypt(TConnectionManager.Instance.NodePrivateKey.EC_OpenSSL_NID,
    TConnectionManager.Instance.NodePrivateKey.PrivateKey, false, xMsg, Result.Message)
  then raise Exception.Create('Can''''t decrypt message');
end;

{ TNetMessage_NewBlock }

class function TNetMessage_NewBlock.LoadFromStream(
  AStream: TStream): TNetMessage_NewBlock;
var
  xErrors: ansiString;
begin
  Result.RemoteWork := 0;
  Result.NewBlock := TBlock.Create(nil);
  Result.NewBlock.BlockManager := TNode.Node.BlockManager;
  if not Result.NewBlock.LoadBlockFromStream(AStream, xErrors)
  then raise Exception.Create(xErrors);
  if AStream.Size > AStream.Position + SizeOf(Result.RemoteWork)
  then AStream.Read(Result.RemoteWork, SizeOf(Result.RemoteWork));
end;

{ TNetMessage_GetBlocks }

class function TNetMessage_GetBlocks.LoadFromStream(
  AStream: TStream): TNetMessage_GetBlocks;
begin
  AStream.Read(Result.StartBlock, SizeOf(Result.StartBlock));
  AStream.Read(Result.EndBlock, SizeOf(Result.EndBlock));
  if (Result.StartBlock > Result.EndBlock)
  then raise Exception.CreateFmt('Invalid structure start or end %d, %d',[Result.StartBlock, Result.EndBlock]);
end;

{ TNetMessage_GetBlocks_Response }

class procedure TNetMessage_GetBlocks_Response.CleanUp(ABlocks : TBlockArray);
var
  i : integer;
begin
  if Assigned(ABlocks) then
  for i := Low(ABlocks) to High(ABlocks) do begin
    if Assigned(ABlocks[i]) then ABlocks[i].Free;
  end;
  SetLength(ABlocks, 0);
end;

class function TNetMessage_GetBlocks_Response.LoadFromStream(
  AStream: TStream): TNetMessage_GetBlocks_Response;
var
  i: integer;
  xErrors: AnsiString;
begin
  AStream.Read(Result.FCount, SizeOf(Result.FCount));
  SetLength(Result.FBlocks, Result.FCount);
  for i := Low(Result.Blocks) to High(Result.Blocks)
  do begin
    Result.Blocks[i] := TBlock.Create(nil);
    Result.Blocks[i].BlockManager := TNode.Node.BlockManager;
    if not Result.Blocks[i].LoadBlockFromStream(AStream, xErrors)
    then raise Exception.Create(xErrors);
  end;
  if not Assigned(Result.FDestructor)
  then Result.FDestructor := TDestructor<TBlockArray>.Create(Result.Cleanup, Result.FBlocks);
end;

procedure TNetMessage_GetBlocks_Response.SaveToStream(AStream: TStream);
var
  xBlock: TBlock;
  xCnt : integer;
begin
  AStream.Write(FCount, SizeOf(FCount));
  xCnt := 0;
  for xBlock in Blocks do begin
    xBlock.SaveBlockToStream(false, AStream);
    inc(xCnt);
    if (AStream.Size > (1024 * 1024 * 2)) then begin
      AStream.Position := 0;
      AStream.Write(xCnt, SizeOf(xCnt));
      AStream.Position := AStream.Size;
      break;
    end;
  end;
end;

procedure TNetMessage_GetBlocks_Response.SetCount(const Value: Int32);
begin
  FCount := Value;
  SetLength(FBlocks, Value);
  if not Assigned(FDestructor)
  then FDestructor := TDestructor<TBlockArray>.Create(Cleanup, FBlocks);
end;

{ TNetMessage_NewTransaction }

class function TNetMessage_NewTransaction.LoadFromStream(
  AStream: TStream): TNetMessage_NewTransaction;
var
  i:integer;
  xType: byte;
  xTransactionClass: TTransactionClass;
  xTransaction: ITransaction;
begin
  AStream.Read(Result.Count, SizeOf(Result.Count));
  Result.Transactions := TTransactionHashTree.Create;
  for I := 0 to Result.Count - 1
  do begin
    AStream.Read(xType, 1);
    xTransactionClass := TTransactionManager.GetTransactionPlugin(xType);
    if not Assigned(xTransactionClass) then
      exit;
    xTransaction := xTransactionClass.Create;
    xTransaction.LoadFromNettransfer(AStream);
    Result.Transactions.AddTransactionToHashTree(xTransaction);
  end;
end;

{ TNetMessage_AccountStorage_Request }

class function TNetMessage_AccountStorage_Request.LoadFromStream(
  AStream: TStream): TNetMessage_AccountStorage_Request;
begin
  AStream.Read(Result.Count, SizeOf(Result.Count));
  AStream.ReadAnsiString(Result.Hash);
  AStream.Read(Result.StartBlock, SizeOf(Result.StartBlock));
  AStream.Read(Result.EndBlock, SizeOf(Result.EndBlock));
end;

end.
