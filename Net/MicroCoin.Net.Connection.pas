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
| File:       MicroCoin.Net.Connection.pas                                     |
| Created at: 2018-08-28                                                       |
| Purpose:    Net protocol implementation                                      |
| Todo:                                                                        |
|   - Clean up code                                                            |
|==============================================================================}

unit MicroCoin.Net.Connection;

{$ifdef FPC}
  {$mode delphi}
{$endif}


interface

uses SysUtils, Classes, UTCPIP, MicroCoin.BlockChain.BlockHeader, UThread,
  MicroCoin.Account.AccountKey, MicroCoin.Common.Lists, MicroCoin.Net.Protocol,
  MicroCoin.Net.NodeServer, UBaseTypes,
  MicroCoin.Transaction.ITransaction, SyncObjs,
  MicroCoin.Transaction.HashTree, MicroCoin.BlockChain.Block, ULog,
  MicroCoin.Net.ConnectionBase;

type

{$IFDEF FPC}
  TCriticalSection = SyncObjs.TCriticalSection;
{$ENDIF}

  TNetConnection = class(TNetConnectionBase)
  strict private
    FBufferLock: TCriticalSection;
    FBufferReceivedOperationsHash: TOrderedRawList;
    FBufferToSendOperations: TTransactionHashTree;
    FIsDownloadingBlocks: Boolean;
    FRemoteOperationBlock: TBlockHeader;
    FRemoteAccumulatedWork: UInt64;
    FClientPublicKey: TAccountKey;
    FTimestampDiff: Integer;
    FClientAppVersion: AnsiString;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    function AddOperationsToBufferForSend(Operations: TTransactionHashTree): Integer;

    function Send_Hello(NetTranferType: TNetTransferType; request_id: Integer): Boolean; override;
    function Send_NewBlockFound(const NewBlock: TBlock): Boolean; override;
    function Send_GetBlocks(StartAddress, quantity: Cardinal; var request_id: Cardinal): Boolean; override;
    function Send_AddOperations(Operations: TTransactionHashTree): Boolean; override;
    function Send_Message(const TheMessage: AnsiString): Boolean; override;

    property IsDownloadingBlocks: Boolean read FIsDownloadingBlocks write FIsDownloadingBlocks;
    property RemoteOperationBlock: TBlockHeader read FRemoteOperationBlock write FRemoteOperationBlock;
    property RemoteAccumulatedWork: UInt64 read FRemoteAccumulatedWork write FRemoteAccumulatedWork;
    property ClientPublicKey: TAccountKey read FClientPublicKey write FClientPublicKey;
    property TimestampDiff: Integer read FTimestampDiff write FTimestampDiff;
    property ClientAppVersion: AnsiString read FClientAppVersion write FClientAppVersion;
    property BufferLock: TCriticalSection read FBufferLock;
    property BufferReceivedOperationsHash: TOrderedRawList read FBufferReceivedOperationsHash;
    property BufferToSendOperations: TTransactionHashTree read FBufferToSendOperations;
  end;

  PNetRequestRegistered = ^TNetRequestRegistered;

  TNetRequestRegistered = record
    NetClient: TNetConnectionBase;
    operation: Word;
    RequestId: Cardinal;
    SendTime: TDateTime;
  end;

implementation

uses UTime, MicroCoin.Net.ConnectionManager, MicroCoin.Common.Config, UCrypto,
  UECIES, MicroCoin.Common.Stream,
  UChunk, MicroCoin.Net.Client,{$IFDEF MSWINDOWS} Windows,{$ENDIF} MicroCoin.Transaction.Base,
  MicroCoin.Net.Utils,
  MicroCoin.Transaction.Manager, MicroCoin.Node.Node, MicroCoin.Account.Storage;

constructor TNetConnection.Create(AOwner: TComponent);
begin
  FClientAppVersion := '';
  FBufferLock := TCriticalSection.Create();
  FBufferReceivedOperationsHash := TOrderedRawList.Create;
  FBufferToSendOperations := TTransactionHashTree.Create;
  FRemoteOperationBlock := TBlockHeader.Empty;
  FRemoteAccumulatedWork := 0;
  FClientPublicKey := TAccountKey.Empty;
  FTimestampDiff := 0;
  inherited;
end;

destructor TNetConnection.Destroy;
begin
    FreeAndNil(FBufferLock);
    FreeAndNil(FBufferReceivedOperationsHash);
    FreeAndNil(FBufferToSendOperations);
  inherited;
end;

function TNetConnection.Send_AddOperations(Operations: TTransactionHashTree): Boolean;
var
  data: TMemoryStream;
  c1, xRequestId: Cardinal;
  i, xNumberOfTransactionsToSend: Integer;
  xTransactionType: Byte;
begin
  Result := false;
  if not Connected
  then exit;
  NetLock.Acquire;
  try
    xNumberOfTransactionsToSend := 0;
    FBufferLock.Acquire;
    try
      if Assigned(Operations)
      then begin
        for i := 0 to Operations.TransactionCount - 1
        do begin
          if FBufferReceivedOperationsHash.IndexOf(Operations.GetTransaction(i).Sha256) < 0
          then begin
            FBufferReceivedOperationsHash.Add(Operations.GetTransaction(i).Sha256);
            if FBufferToSendOperations.IndexOf(Operations.GetTransaction(i)) < 0
            then FBufferToSendOperations.AddTransactionToHashTree(Operations.GetTransaction(i));
          end;
        end;
        xNumberOfTransactionsToSend := Operations.TransactionCount;
      end;
      if FBufferToSendOperations.TransactionCount > 0
      then begin
        LogDebug(Classname, Format('Sending %d Operations to %s (inProc:%d, Received:%d)',
          [FBufferToSendOperations.TransactionCount, ClientRemoteAddr, xNumberOfTransactionsToSend,
          FBufferReceivedOperationsHash.Count]));
        data := TMemoryStream.Create;
        try
          xRequestId := TConnectionManager.Instance.NewRequestId;
          c1 := FBufferToSendOperations.TransactionCount;
          data.Write(c1, 4);
          for i := 0 to FBufferToSendOperations.TransactionCount - 1 do
          begin
            xTransactionType := FBufferToSendOperations.GetTransaction(i).TransactionType;
            data.Write(xTransactionType, 1);
            FBufferToSendOperations.GetTransaction(i).SaveToNettransfer(data);
          end;
          Send(ntp_autosend, cNetOp_AddOperations, 0, xRequestId, data);
          FBufferToSendOperations.ClearHastThree;
        finally
          data.Free;
        end;
      end
      else
        LogDebug(Classname, Format('Not sending any operations to %s (inProc:%d, Received:%d, Sent:%d)',
          [ClientRemoteAddr, xNumberOfTransactionsToSend, FBufferReceivedOperationsHash.Count,
          FBufferToSendOperations.TransactionCount]));
    finally
      FBufferLock.Release;
    end;
  finally
    NetLock.Release;
  end;
  Result := Connected;
end;

function TNetConnection.Send_GetBlocks(StartAddress, quantity: Cardinal; var request_id: Cardinal): Boolean;
var
  data: TMemoryStream;
  c1, c2: Cardinal;
begin
  Result := false;
  request_id := 0;
  if (FRemoteOperationBlock.Block < TConnectionManager.Instance.Bank.BlocksCount) or (FRemoteOperationBlock.Block = 0)
  then
    exit;
  if not Connected then
    exit;
  // First receive operations from
  data := TMemoryStream.Create;
  try
    if TConnectionManager.Instance.Bank.BlocksCount = 0 then
      c1 := 0
    else
      c1 := StartAddress;
    if (quantity = 0) then
    begin
      if FRemoteOperationBlock.Block > 0 then
        c2 := FRemoteOperationBlock.Block
      else
        c2 := c1 + 100;
    end
    else
      c2 := c1 + quantity - 1;

    data.Write(c1, 4);
    data.Write(c2, 4);
    request_id := TConnectionManager.Instance.NewRequestId;
    TConnectionManager.Instance.RegisterRequest(Self, cNetOp_GetBlocks, request_id);
    LogDebug(Classname, Format('Send GET BLOCKS start:%d quantity:%d (from:%d to %d)',
      [StartAddress, quantity, StartAddress, quantity + StartAddress]));
    FIsDownloadingBlocks := quantity > 1;
    Send(ntp_request, cNetOp_GetBlocks, 0, request_id, data);
    Result := Connected;
  finally
    data.Free;
  end;
end;

function TNetConnection.Send_Hello(NetTranferType: TNetTransferType; request_id: Integer): Boolean;
{ HELLO command:
  - Operation stream
  - My Active server port (0 if no active). (2 bytes)
  - A Random Longint (4 bytes) to check if its myself connection to my server socket
  - My Unix Timestamp (4 bytes)
  - Registered node servers count
  (For each)
  - ip (string)
  - port (2 bytes)
  - last_connection UTS (4 bytes)
  - My Server port (2 bytes)
  - If this is a response:
  - If remote operation block is lower than me:
  - Send My Operation Stream in the same block thant requester
}
var
  xData: TMemoryStream;
  xMessage: TNetMessage_Hello;
begin
  Result := false;
  if not Connected then exit;
  xData := TMemoryStream.Create;
  try
    if NetTranferType = ntp_request
    then TConnectionManager.Instance.RegisterRequest(Self, cNetOp_Hello, request_id);

    if TNode.Node.NetServer.Active
    then xMessage.server_port := TNode.Node.NetServer.port
    else xMessage.server_port := 0;
    xMessage.accountKey := TConnectionManager.Instance.NodePrivateKey.PublicKey;
    xMessage.Timestamp := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
    xMessage.Block := TBlock.Create(nil);
    xMessage.Block.BlockHeader := TNode.Node.BlockManager.LastBlock;
    xMessage.nodeservers := TConnectionManager.Instance.GetValidNodeServers(true, cMAX_NODESERVERS_ON_HELLO);
    xMessage.nodeserver_count := Length(xMessage.nodeservers);
    xMessage.client_version := MicroCoin.Common.Config.ClientAppVersion{$IFDEF LINUX} + ' Linux'{$ELSE} + ' Windows'{$ENDIF}{$IFDEF FPC}{$IFDEF LCL} + ' '{$ELSE} + ' '{$ENDIF}{$ELSE}+' '{$ENDIF}{$IFDEF DEBUG}+' Debug'{$ELSE}+''{$ENDIF}{$IFDEF CONSOLE}+' daemon'{$ENDIF};
    xMessage.remote_work := TNode.Node.BlockManager.AccountStorage.WorkSum;    //
    xMessage.SaveToStream(xData);
    Send(NetTranferType, cNetOp_Hello, 0, request_id, xData);
    xMessage.Block.Free;
    SetLength(xMessage.nodeservers, 0);
    xMessage.nodeservers := nil;
    Result := Client.Connected;
  finally
    xData.Free;
  end;
end;

function TNetConnection.Send_Message(const TheMessage: AnsiString): Boolean;
var
  data: TStream;
  cyp: TRawBytes;
begin
  Result := false;
  if not Connected then
    exit;
  data := TMemoryStream.Create;
  try
    // Cypher message:
    cyp := ECIESEncrypt(FClientPublicKey, TheMessage);
    data.WriteAnsiString(cyp);
    Send(ntp_autosend, cNetOp_Message, 0, 0, data);
    Result := true;
  finally
    data.Free;
  end;
end;

function TNetConnection.Send_NewBlockFound(const NewBlock: TBlock): Boolean;
var
  data: TStream;
  request_id: Integer;
begin
  Result := false;
  if not Connected then
    exit;
  NetLock.Acquire;
  try
    // Clear buffers
    FBufferLock.Acquire;
    try
      FBufferReceivedOperationsHash.Clear;
      FBufferToSendOperations.ClearHastThree;
    finally
      FBufferLock.Release;
    end;
    // Checking if operationblock is the same to prevent double messaging...
    if FRemoteOperationBlock = NewBlock.BlockHeader then
    begin
      LogDebug(Classname, 'This connection has the same block, does not need to send');
      exit;
    end;
    if (TNode.Node.BlockManager.BlocksCount <> NewBlock.BlockHeader.Block + 1) then
    begin
      LogDebug(Classname, 'The block number ' + Inttostr(NewBlock.BlockHeader.Block) +
        ' is not equal to current blocks stored in bank (' + Inttostr(TNode.Node.BlockManager.BlocksCount) + '), finalizing');
      exit;
    end;
    data := TMemoryStream.Create;
    try
      request_id := TConnectionManager.Instance.NewRequestId;
      NewBlock.SaveBlockToStream(false, data);
      data.Write(TNode.Node.BlockManager.AccountStorage.WorkSum, SizeOf(TNode.Node.BlockManager.AccountStorage.WorkSum));
      Send(ntp_autosend, cNetOp_NewBlock, 0, request_id, data);
    finally
      data.Free;
    end;
  finally
    NetLock.Release;
  end;
  Result := Connected;
end;

function TNetConnection.AddOperationsToBufferForSend(Operations: TTransactionHashTree): Integer;
var
  i: Integer;
begin
  Result := 0;
  try
    FBufferLock.Acquire;
    try
      for i := 0 to Operations.TransactionCount - 1 do
      begin
        if FBufferReceivedOperationsHash.IndexOf(Operations.GetTransaction(i).Sha256) < 0 then
        begin
          FBufferReceivedOperationsHash.Add(Operations.GetTransaction(i).Sha256);
          if FBufferToSendOperations.IndexOf(Operations.GetTransaction(i)) < 0 then
          begin
            FBufferToSendOperations.AddTransactionToHashTree(Operations.GetTransaction(i));
            Inc(Result);
          end;
        end;
      end;
    finally
      FBufferLock.Release;
    end;
  except
    on E: Exception do
    begin
      TLog.NewLog(ltError, Classname, 'Error at AddOperationsToBufferForSend (' + E.Classname + '): ' + E.Message);
      Result := 0;
    end;
  end;
end;


end.
