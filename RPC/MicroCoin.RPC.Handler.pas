unit MicroCoin.RPC.Handler;

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

uses UThread, SysUtils, Classes, blcksock,
  Synautil, Math, UCrypto, MicroCoin.RPC.MethodHandler,
  MicroCoin.Account.Data, MicroCoin.BlockChain.Block,
  MicroCoin.RPC.PluginManager, MicroCoin.Node.Node,
  MicroCoin.RPC.Result, UbaseTypes,
  MicroCoin.Account.Storage, MicroCoin.BlockChain.BlockHeader,
  MicroCoin.Transaction.Itransaction,
  MicroCoin.Transaction.TransferMoney, MicroCoin.Transaction.ChangeKey,
  MicroCoin.Transaction.ListAccount, MicroCoin.Transaction.ChangeAccountInfo,
  MicroCoin.Transaction.TransactionList, MicroCoin.Transaction.HashTree,
  MicroCoin.Common.Lists, MicroCoin.Account.AccountKey, MicroCoin.Common,
  MicroCoin.Transaction.Base, MicroCoin.Transaction.Transaction,
  synsock, {$IFDEF fpc} fpjson, {$ELSE}System.Json, {$ENDIF} UJSONFunctions
  {$IFDEF USE_RTTI}{$IFDEF FPC}, TypInfo{$else}, RTTI {$endif}{$ENDIF};

const
  CT_RPC_ErrNum_InternalError = 100;
  CT_RPC_ErrNum_NotImplemented = 101;

  CT_RPC_ErrNum_MethodNotFound = 1001;
  CT_RPC_ErrNum_InvalidAccount = 1002;
  CT_RPC_ErrNum_InvalidBlock = 1003;
  CT_RPC_ErrNum_InvalidOperation = 1004;
  CT_RPC_ErrNum_InvalidPubKey = 1005;
  CT_RPC_ErrNum_InvalidAccountName = 1006;
  CT_RPC_ErrNum_NotFound = 1010;
  CT_RPC_ErrNum_WalletPasswordProtected = 1015;
  CT_RPC_ErrNum_InvalidData = 1016;

type

  TRPCHandler = class(TPCThread)
  private
    FSock: TTCPBlockSocket;
    FNode: TNode;
  public
    constructor Create(hsock: TSocket);
    destructor Destroy; override;
    procedure BCExecute; override;
    function ProcessMethod(const method: string; params: TPCJSONObject; jsonresponse: TPCJSONObject;
      var ErrorNum: integer; var ErrorDesc: string): Boolean;
  end;

implementation

uses UAES, MicroCoin.Net.ConnectionManager,
  MicroCoin.Net.NodeServer,
  MicroCoin.Net.Client, MicroCoin.Net.Connection, UWalletKeys, UECIES, MicroCoin.Common.Config, MicroCoin.RPC.Server, ULog, Variants,
  UTime;

constructor TRPCHandler.Create(hsock: TSocket);
begin
  FSock := TTCPBlockSocket.Create;
  FSock.socket := hsock;
  FreeOnTerminate := true;
  FNode := TNode.Node;
  // Priority:=tpNormal;
  inherited Create(false);
  FreeOnTerminate := true;
end;

destructor TRPCHandler.Destroy;
begin
  FSock.Free;
  inherited Destroy;
end;

procedure TRPCHandler.BCExecute;
var
  timeout: integer;
  s: string;
  method, uri, Protocol: string;
  size: integer;
  x, n: integer;
  resultcode: integer;
  inputdata: TBytes;
  js, jsresult: TPCJsonData;
  jsonobj, jsonresponse: TPCJSONObject;
  errNum: integer;
  errDesc: string;
  jsonrequesttxt, jsonresponsetxt, methodName, paramsTxt: AnsiString;
  valid: Boolean;
  i: integer;
  Headers: TStringList;
  tc: cardinal;
  callcounter: Int64;
begin
  callcounter := TRPCServer.Instance.GetNewCallCounter;
  tc := GetTickCount;
  methodName := '';
  paramsTxt := '';
  // IP Protection
  if (not TRPCServer.Instance.IsValidClientIP(FSock.GetRemoteSinIP, FSock.GetRemoteSinPort)) then
  begin
    TLog.NewLog(lterror, Classname, FSock.GetRemoteSinIP + ':' + Inttostr(FSock.GetRemoteSinPort) + ' INVALID IP');
    TRPCServer.Instance.AddRPCLog(FSock.GetRemoteSinIP + ':' + Inttostr(FSock.GetRemoteSinPort), ' INVALID IP');
    exit;
  end;
  errNum := CT_RPC_ErrNum_InternalError;
  errDesc := 'No data';
  valid := false;
  SetLength(inputdata, 0);
  Headers := TStringList.Create;
  jsonresponse := TPCJSONObject.Create;
  try
    timeout := 5000;
    resultcode := 200;
    repeat
      // read request line
      s := FSock.RecvString(timeout);
      if FSock.lasterror <> 0 then
        exit;
      if s = '' then
        exit;
      method := fetch(s, ' ');
      if (s = '') or (method = '') then
        exit;
      uri := fetch(s, ' ');
      if uri = '' then
        exit;
      Protocol := fetch(s, ' ');
      Headers.Clear;
      size := -1;
      // read request headers
      if Protocol <> '' then
      begin
        if pos('HTTP/1.1', Protocol) <> 1 then
        begin
          errDesc := 'Invalid protocol ' + Protocol;
          exit;
        end;
        repeat
          s := FSock.RecvString(timeout);
          if FSock.lasterror <> 0 then
            exit;
          if pos('CONTENT-LENGTH:', Uppercase(s)) = 1 then
            size := StrToIntDef(SeparateRight(s, ' '), -1);
        until s = '';
      end;
      // recv document...
      if size >= 0 then
      begin
        SetLength(inputdata, size);
        x := FSock.RecvBufferEx(inputdata, size, timeout);
        if FSock.lasterror <> 0 then
          exit;
        if (x <> size) and (x > 0) then
          SetLength(inputdata, x);
      end
      else
        SetLength(inputdata, 0);
      SetLength(jsonrequesttxt, length(inputdata));
      for i := 0 to high(inputdata) do
      begin
        jsonrequesttxt[i + 1] := AnsiChar(inputdata[i]);
      end;
      // Convert InputData to JSON object
      try
        js := TPCJsonData.ParseJSONValue(jsonrequesttxt);
      except
        on E: Exception do
        begin
          errDesc := 'Error decoding JSON: ' + E.Message;
          TLog.NewLog(lterror, Classname, FSock.GetRemoteSinIP + ':' + Inttostr(FSock.GetRemoteSinPort) +
            ' Error decoding JSON: ' + E.Message);
          exit;
        end;
      end;
      if Assigned(js) then
      begin
        try
          if (js is TPCJSONObject) then
          begin
            jsonobj := TPCJSONObject(js);
            errNum := 0;
            errDesc := '';
            try
              methodName := jsonobj.AsString('method', '');
              paramsTxt := jsonobj.GetAsObject('params').ToJSON(false);
              TLog.NewLog(ltinfo, Classname, FSock.GetRemoteSinIP + ':' + Inttostr(FSock.GetRemoteSinPort) +
                ' Processing method ' + jsonobj.AsString('method', ''));
              valid := ProcessMethod(jsonobj.AsString('method', ''), jsonobj.GetAsObject('params'), jsonresponse,
                errNum, errDesc);
              if not valid then
              begin
                if (errNum <> 0) or (errDesc <> '') then
                begin
                  jsonresponse.GetAsObject('error').GetAsVariant('code').Value := errNum;
                  jsonresponse.GetAsObject('error').GetAsVariant('message').Value := errDesc;
                end
                else
                begin
                  jsonresponse.GetAsObject('error').GetAsVariant('code').Value := CT_RPC_ErrNum_InternalError;
                  jsonresponse.GetAsObject('error').GetAsVariant('message').Value := 'Unknown error processing method';
                end;
              end;
            except
              on E: Exception do
              begin
                TLog.NewLog(lterror, Classname, 'Exception processing method' + jsonobj.AsString('method', '') + ' (' +
                  E.Classname + '): ' + E.Message);
                jsonresponse.GetAsObject('error').GetAsVariant('code').Value := CT_RPC_ErrNum_InternalError;
                jsonresponse.GetAsObject('error').GetAsVariant('message').Value := E.Message;
                valid := false;
              end;
            end;
            jsonresponse.GetAsVariant('id').Value := jsonobj.GetAsVariant('id').Value;
            jsonresponse.GetAsVariant('jsonrpc').Value := '2.0';
          end;
        finally
          js.Free;
        end;
      end
      else
      begin
        TLog.NewLog(lterror, Classname, FSock.GetRemoteSinIP + ':' + Inttostr(FSock.GetRemoteSinPort) +
          ' Received data is not a JSON: ' + jsonrequesttxt + ' (length ' + Inttostr(length(jsonrequesttxt)) +
          ' bytes)');
      end;
    until (FSock.lasterror <> 0) or (Protocol <> '');
  finally
    try
      // Send result:
      if FSock.lasterror = 0 then
      begin
        // Save JSON response:
        if (not valid) then
        begin
          if not Assigned(jsonresponse.FindName('error')) then
          begin
            jsonresponse.GetAsObject('error').GetAsVariant('code').Value := errNum;
            jsonresponse.GetAsObject('error').GetAsVariant('message').Value := errDesc;
          end;
        end;
        jsonresponsetxt := jsonresponse.ToJSON(false);
        FSock.SendString(Protocol + ' ' + Inttostr(resultcode) + CRLF);
        if (Protocol <> '') then
        begin
          Headers.Add('Server: MicroCoin HTTP JSON-RPC Server');
          Headers.Add('Content-Type: application/json;charset=utf-8');
          Headers.Add('Content-length: ' + Inttostr(length(jsonresponsetxt)));
          Headers.Add('Connection: close');
          Headers.Add('Access-Control-Allow-Origin: *');
          Headers.Add('Date: ' + Rfc822DateTime(now));
          Headers.Add('');
          for n := 0 to Headers.Count - 1 do
            FSock.SendString(Headers[n] + CRLF);
        end;
        if FSock.lasterror = 0 then
        begin
          FSock.SendBuffer(addr(jsonresponsetxt[1]), length(jsonresponsetxt));
        end;
      end;
      TRPCServer.Instance.AddRPCLog(FSock.GetRemoteSinIP + ':' + Inttostr(FSock.GetRemoteSinPort),
        'Method:' + methodName + ' Params:' + paramsTxt + ' ' + Inttostr(errNum) + ':' + errDesc + ' Time:' +
        FormatFloat('0.000', (GetTickCount - tc) / 1000));
    finally
      jsonresponse.Free;
      Headers.Free;
    end;
  end;
end;

function TRPCHandler.ProcessMethod(const method: string; params: TPCJSONObject; jsonresponse: TPCJSONObject;
  var ErrorNum: integer; var ErrorDesc: string): Boolean;
var
  _ro: TPCJSONObject;
  _ra: TPCJSONArray;
  function GetResultObject: TPCJSONObject;
  begin
    if not Assigned(_ro) then
    begin
      _ro := jsonresponse.GetAsObject('result');
      _ra := nil;
    end;
    Result := _ro;
  end;

  function GetResultArray: TPCJSONArray;
  begin
    if not Assigned(_ra) then
    begin
      _ra := jsonresponse.GetAsArray('result');
      _ro := nil;
    end;
    Result := _ra;
  end;

  function ToJSONCurrency(microCoins: Int64): Real;
  begin
    Result := RoundTo(microCoins / 10000, -4);
  end;

  function ToMicroCoins(jsonCurr: Real): Int64;
  begin
    Result := Round(jsonCurr * 10000);
  end;

  function HexaStringToOperationsHashTree(const HexaStringOperationsHashTree: AnsiString;
    out OperationsHashTree: TTransactionHashTree; var errors: AnsiString): Boolean;
  var
    raw: TRawBytes;
    ms: TMemoryStream;
  begin
    Result := false;
    raw := TBaseType.HexaToRaw(HexaStringOperationsHashTree);
    if (HexaStringOperationsHashTree <> '') and (raw = '') then
    begin
      errors := 'Invalid HexaString as operations';
      exit;
    end;
    ms := TMemoryStream.Create;
    try
      ms.WriteBuffer(raw[1], length(raw));
      ms.Position := 0;
      OperationsHashTree := TTransactionHashTree.Create;
      if (raw <> '') then
      begin
        if not OperationsHashTree.LoadFromStream(ms, false, false, errors) then
        begin
          FreeAndNil(OperationsHashTree);
          exit;
        end;
      end;
      Result := true;
    finally
      ms.Free;
    end;
  end;

  function OperationsHashTreeToHexaString(const OperationsHashTree: TTransactionHashTree): AnsiString;
  var
    ms: TMemoryStream;
    raw: TRawBytes;
  begin
    ms := TMemoryStream.Create;
    try
      OperationsHashTree.SaveToStream(ms, false);
      ms.Position := 0;
      SetLength(raw, ms.size);
      ms.ReadBuffer(raw[1], ms.size);
      Result := TBaseType.ToHexaString(raw);
    finally
      ms.Free;
    end;
  end;

  function GetBlock(nBlock: cardinal; jsonObject: TPCJSONObject): Boolean;
  var
    pcops: TBlock;
    ob: TBlockHeader;
  begin
    pcops := TBlock.Create(nil);
    try
      if FNode.BlockManager.BlocksCount <= nBlock then
      begin
        ErrorNum := CT_RPC_ErrNum_InvalidBlock;
        ErrorDesc := 'Cannot load Block: ' + Inttostr(nBlock);
        Result := false;
        exit;
      end;
      ob := FNode.BlockManager.AccountStorage.Blocks[nBlock].BlockHeader;

      jsonObject.GetAsVariant('block').Value := ob.Block;
      jsonObject.GetAsVariant('enc_pubkey').Value := TBaseType.ToHexaString(ob.account_key.ToRawString);
      jsonObject.GetAsVariant('reward').Value := ToJSONCurrency(ob.reward);
      jsonObject.GetAsVariant('fee').Value := ToJSONCurrency(ob.fee);
      jsonObject.GetAsVariant('ver').Value := ob.protocol_version;
      jsonObject.GetAsVariant('ver_a').Value := ob.protocol_available;
      jsonObject.GetAsVariant('timestamp').Value := Int64(ob.timestamp);
      jsonObject.GetAsVariant('target').Value := Int64(ob.compact_target);
      jsonObject.GetAsVariant('nonce').Value := Int64(ob.nonce);
      jsonObject.GetAsVariant('payload').Value := ob.block_payload;
      jsonObject.GetAsVariant('sbh').Value := TBaseType.ToHexaString(ob.initial_safe_box_hash);
      jsonObject.GetAsVariant('oph').Value := TBaseType.ToHexaString(ob.transactionHash);
      jsonObject.GetAsVariant('pow').Value := TBaseType.ToHexaString(ob.proof_of_work);
      jsonObject.GetAsVariant('hashratekhs').Value := FNode.BlockManager.AccountStorage.CalcBlockHashRateInKhs(ob.Block, 50);
      jsonObject.GetAsVariant('maturation').Value := FNode.BlockManager.BlocksCount - ob.Block - 1;
      if FNode.BlockManager.LoadTransactions(pcops, nBlock) then
      begin
        jsonObject.GetAsVariant('operations').Value := pcops.TransactionCount;
      end;
      Result := true;
    finally
      pcops.Free;
    end;
  end;

  procedure FillOperationResumeToJSONObject(const OPR: TTransactionData; jsonObject: TPCJSONObject);
  begin
    if not OPR.valid then
    begin
      jsonObject.GetAsVariant('valid').Value := OPR.valid;
    end;
    if (OPR.errors <> '') and (not OPR.valid) then
    begin
      jsonObject.GetAsVariant('errors').Value := OPR.errors;
    end;
    if OPR.valid then
    begin
      jsonObject.GetAsVariant('block').Value := OPR.Block;
      jsonObject.GetAsVariant('time').Value := OPR.time;
      jsonObject.GetAsVariant('opblock').Value := OPR.NOpInsideBlock;
      if (OPR.Block > 0) and (OPR.Block < FNode.BlockManager.BlocksCount) then
        jsonObject.GetAsVariant('maturation').Value := FNode.BlockManager.BlocksCount - OPR.Block - 1
      else
        jsonObject.GetAsVariant('maturation').Value := null;
    end;
    jsonObject.GetAsVariant('optype').Value := OPR.transactionType;
    jsonObject.GetAsVariant('subtype').Value := OPR.transactionSubtype;
    jsonObject.GetAsVariant('account').Value := OPR.AffectedAccount;
    jsonObject.GetAsVariant('signer_account').Value := OPR.SignerAccount;
    jsonObject.GetAsVariant('optxt').Value := OPR.TransactionAsString;
    jsonObject.GetAsVariant('amount').Value := ToJSONCurrency(OPR.Amount);
    jsonObject.GetAsVariant('fee').Value := ToJSONCurrency(OPR.fee);
    if (OPR.balance >= 0) and (OPR.valid) then
      jsonObject.GetAsVariant('balance').Value := ToJSONCurrency(OPR.balance);
    jsonObject.GetAsVariant('payload').Value := TBaseType.ToHexaString(OPR.OriginalPayload);
    if (OPR.transactionType = CT_Op_Transaction) then
    begin
      if OPR.SignerAccount >= 0 then
      begin
        jsonObject.GetAsVariant('sender_account').Value := OPR.SignerAccount;
      end;
      if OPR.DestAccount >= 0 then
      begin
        jsonObject.GetAsVariant('dest_account').Value := OPR.DestAccount;
      end;
    end;
    if OPR.newKey.EC_OpenSSL_NID > 0 then
    begin
      jsonObject.GetAsVariant('enc_pubkey').Value := TBaseType.ToHexaString(OPR.newKey.ToRawString);
    end;
    if (OPR.valid) and (OPR.OperationHash <> '') then
    begin
      jsonObject.GetAsVariant('ophash').Value := TBaseType.ToHexaString(OPR.OperationHash);
      if (OPR.Block < cProtocol_Upgrade_v2_MinBlock) then
      begin
        jsonObject.GetAsVariant('old_ophash').Value := TBaseType.ToHexaString(OPR.OperationHash_OLD);
      end;
    end;
  end;

  procedure FillOperationsHashTreeToJSONObject(const OperationsHashTree: TTransactionHashTree;
    jsonObject: TPCJSONObject);
  begin
    jsonObject.GetAsVariant('operations').Value := OperationsHashTree.TransactionCount;
    jsonObject.GetAsVariant('amount').Value := ToJSONCurrency(OperationsHashTree.TotalAmount);
    jsonObject.GetAsVariant('fee').Value := ToJSONCurrency(OperationsHashTree.TotalFee);
    jsonObject.GetAsVariant('rawoperations').Value := OperationsHashTreeToHexaString(OperationsHashTree);
  end;

  function GetAccountOperations(accountNumber: cardinal; jsonArray: TPCJSONArray;
    maxBlocksDepth, startReg, maxReg: integer): Boolean;
  var
    xList: TList;
    xTransaction: ITransaction;
    xTransactionData: TTransactionData;
    xJsonObject: TPCJSONObject;
    xTransactionList: TTransactionList;
    i, xCounter: integer;
  begin
    Result := false;
    if (startReg < -1) or (maxReg <= 0) then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Invalid start or max value';
      exit;
    end;
    xCounter := 0;
    xTransactionList := TTransactionList.Create;
    try
      if (startReg = -1) then
      begin
        // 1.5.5 change: If start=-1 then will include PENDING OPERATIONS, otherwise not.
        // Only will return pending operations if start=0, otherwise
        xList := TList.Create;
        try
          FNode.TransactionStorage.TransactionHashTree.GetTransactionsAffectingAccount(accountNumber, xList);
          for i := xList.Count - 1 downto 0 do
          begin
            xTransaction := FNode.TransactionStorage.TransactionHashTree.GetTransaction(PtrInt(xList[i]));
            if xTransaction.GetTransactionData(0, accountNumber, xTransactionData) then
            begin
              xTransactionData.NOpInsideBlock := i;
              xTransactionData.Block := FNode.TransactionStorage.BlockHeader.Block;
              xTransactionData.balance := FNode.TransactionStorage.AccountTransaction.Account(accountNumber).Balance;
              if (xCounter >= startReg) and (xCounter < maxReg) then
              begin
                xTransactionList.Add(xTransactionData);
              end;
              inc(xCounter);
            end;
          end;
        finally
          xList.Free;
        end;
      end;
      if (xCounter < maxReg) then
      begin
        if (startReg < 0) then
          startReg := 0; // Prevent -1 value
        FNode.GetStoredTransactionsFromAccount(xTransactionList, accountNumber, maxBlocksDepth, startReg,
          startReg + maxReg - 1);
      end;
      for i := 0 to xTransactionList.Count - 1 do
      begin
        xJsonObject := jsonArray.GetAsObject(jsonArray.Count);
        xTransactionData := xTransactionList[i];
        FillOperationResumeToJSONObject(xTransactionData, xJsonObject);
      end;
      Result := true;
    finally
      xTransactionList.Free;
    end;
  end;

  procedure GetConnections;
  var
    i: integer;
    l: TList;
    nc: TNetConnection;
    Obj: TPCJSONObject;
  begin
    l := TConnectionManager.Instance.NetConnections.LockList;
    try
      for i := 0 to l.Count - 1 do
      begin
        nc := TConnectionManager.Instance.Connection(i);
        Obj := jsonresponse.GetAsArray('result').GetAsObject(i);
        Obj.GetAsVariant('server').Value := not(nc is TNetServerClient);
        Obj.GetAsVariant('ip').Value := nc.Client.RemoteHost;
        Obj.GetAsVariant('port').Value := nc.Client.RemotePort;
        Obj.GetAsVariant('secs').Value := UnivDateTimeToUnix(now) - UnivDateTimeToUnix(nc.CreatedTime);
        Obj.GetAsVariant('sent').Value := nc.Client.BytesSent;
        Obj.GetAsVariant('recv').Value := nc.Client.BytesReceived;
        Obj.GetAsVariant('appver').Value := nc.ClientAppVersion;
        Obj.GetAsVariant('netver').Value := nc.NetProtocolVersion.protocol_version;
        Obj.GetAsVariant('netver_a').Value := nc.NetProtocolVersion.protocol_available;
        Obj.GetAsVariant('timediff').Value := nc.TimestampDiff;
      end;
    finally
      TConnectionManager.Instance.NetConnections.UnlockList;
    end;
  end;

// This function creates a TOpTransaction without looking for balance/private key of sender account
// It assumes that sender,target,sender_last_n_operation,senderAccountKey and targetAccountKey are correct
  function CreateOperationTransaction(Sender, target, sender_last_n_operation: cardinal; Amount, fee: UInt64;
    const senderAccounKey, targetAccountKey: TAccountKey; const RawPayload: TRawBytes;
    const Payload_method, EncodePwd: AnsiString): TTransferMoneyTransaction;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  var
    i: integer;
    f_raw: TRawBytes;
  begin
    Result := nil;
    i := TRPCServer.Instance.WalletKeys.IndexOfAccountKey(senderAccounKey);
    if i < 0 then
    begin
      ErrorDesc := 'Sender Public Key not found in wallet: ' + (senderAccounKey.AccountPublicKeyExport);
      ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
      exit;
    end;
    if (not Assigned(TRPCServer.Instance.WalletKeys.Key[i].PrivateKey)) then
    begin
      if TRPCServer.Instance.WalletKeys.Key[i].CryptedKey <> '' then
      begin
        // Wallet is password protected
        ErrorDesc := 'Wallet is password protected';
        ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      end
      else
      begin
        ErrorDesc := 'Wallet private key not found in Wallet';
        ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
      end;
      exit;
    end;
    //
    if (length(RawPayload) > 0) then
    begin
      if (Payload_method = 'none') then
        f_raw := RawPayload
      else if (Payload_method = 'dest') then
      begin
        f_raw := ECIESEncrypt(targetAccountKey, RawPayload);
      end
      else if (Payload_method = 'sender') then
      begin
        f_raw := ECIESEncrypt(senderAccounKey, RawPayload);
      end
      else if (Payload_method = 'aes') then
      begin
        f_raw := TAESComp.EVP_Encrypt_AES256(RawPayload, EncodePwd);
      end
      else
      begin
        ErrorNum := CT_RPC_ErrNum_InvalidOperation;
        ErrorDesc := 'Invalid encode payload method: ' + Payload_method;
        exit;
      end;
    end
    else
      f_raw := '';
    Result := TTransferMoneyTransaction.CreateTransaction(Sender, sender_last_n_operation + 1, target,
      TRPCServer.Instance.WalletKeys.Key[i].PrivateKey, Amount, fee, f_raw);
    if not Result.HasValidSignature then
    begin
      FreeAndNil(Result);
      ErrorNum := CT_RPC_ErrNum_InternalError;
      ErrorDesc := 'Invalid signature';
      exit;
    end;
  end;

  function OpSendTo(Sender, target: cardinal; Amount, fee: UInt64; const RawPayload: TRawBytes;
    const Payload_method, EncodePwd: AnsiString): Boolean;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  var
    opt: TTransferMoneyTransaction;
    sacc, tacc: TAccount;
    errors: AnsiString;
    OPR: TTransactionData;
  begin
    FNode.SequenceLock.Acquire; // Use lock to prevent N_Operation race-condition on concurrent sends
    try
      Result := false;
      if (Sender < 0) or (Sender >= FNode.BlockManager.AccountsCount) then
      begin
        if (Sender = cMaxAccountNumber) then
          ErrorDesc := 'Need sender'
        else
          ErrorDesc := 'Invalid sender account ' + Inttostr(Sender);
        ErrorNum := CT_RPC_ErrNum_InvalidAccount;
        exit;
      end;
      if (target < 0) or (target >= FNode.BlockManager.AccountsCount) then
      begin
        if (target = cMaxAccountNumber) then
          ErrorDesc := 'Need target'
        else
          ErrorDesc := 'Invalid target account ' + Inttostr(target);
        ErrorNum := CT_RPC_ErrNum_InvalidAccount;
        exit;
      end;
      sacc := FNode.TransactionStorage.AccountTransaction.Account(Sender);
      tacc := FNode.TransactionStorage.AccountTransaction.Account(target);

      opt := CreateOperationTransaction(Sender, target, sacc.NumberOfTransactions, Amount, fee, sacc.accountInfo.AccountKey,
        tacc.accountInfo.AccountKey, RawPayload, Payload_method, EncodePwd);
      if opt = nil then
        exit;
      try
        if not FNode.AddTransaction(nil, opt, errors) then
        begin
          ErrorDesc := 'Error adding operation: ' + errors;
          ErrorNum := CT_RPC_ErrNum_InvalidOperation;
          exit;
        end;
        opt.GetTransactionData(0, Sender, OPR);
        FillOperationResumeToJSONObject(OPR, GetResultObject);
        Result := true;
      finally
//        opt.Free;
      end;
    finally
      FNode.SequenceLock.Release;
    end;
  end;

  function SignOpSendTo(const HexaStringOperationsHashTree: TRawBytes; Sender, target: cardinal;
    const senderAccounKey, targetAccountKey: TAccountKey; last_sender_n_operation: cardinal; Amount, fee: UInt64;
    const RawPayload: TRawBytes; const Payload_method, EncodePwd: AnsiString): Boolean;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  var
    OperationsHashTree: TTransactionHashTree;
    errors: AnsiString;
    opt: TTransferMoneyTransaction;
  begin
    Result := false;
    if not HexaStringToOperationsHashTree(HexaStringOperationsHashTree, OperationsHashTree, errors) then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Error decoding param "rawoperations": ' + errors;
      exit;
    end;
    try
      opt := CreateOperationTransaction(Sender, target, last_sender_n_operation, Amount, fee, senderAccounKey,
        targetAccountKey, RawPayload, Payload_method, EncodePwd);
      if opt = nil then
        exit;
      try
        OperationsHashTree.AddTransactionToHashTree(opt);
        FillOperationsHashTreeToJSONObject(OperationsHashTree, GetResultObject);
        Result := true;
      finally
//        opt.Free;
      end;
    finally
      OperationsHashTree.Free;
    end;
  end;

// This function creates a TOpChangeKey without looking for private key of account
// It assumes that account_signer,account_last_n_operation, account_target and account_pubkey are correct
  function CreateOperationChangeKey(account_signer, account_last_n_operation, account_target: cardinal;
    const account_pubkey, new_pubkey: TAccountKey; fee: UInt64; RawPayload: TRawBytes;
    const Payload_method, EncodePwd: AnsiString): TChangeKeyTransaction;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  var
    i: integer;
    errors: AnsiString;
    f_raw: TRawBytes;
  begin
    Result := nil;
    i := TRPCServer.Instance.WalletKeys.IndexOfAccountKey(account_pubkey);
    if (i < 0) then
    begin
      ErrorDesc := 'Private key not found in wallet: ' + (account_pubkey.AccountPublicKeyExport);
      ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
      exit;
    end;
    if (not Assigned(TRPCServer.Instance.WalletKeys.Key[i].PrivateKey)) then
    begin
      if TRPCServer.Instance.WalletKeys.Key[i].CryptedKey <> '' then
      begin
        // Wallet is password protected
        ErrorDesc := 'Wallet is password protected';
        ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      end
      else
      begin
        ErrorDesc := 'Wallet private key not found in Wallet';
        ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      end;
      exit;
    end;
    if (length(RawPayload) > 0) then
    begin
      if (Payload_method = 'none') then
        f_raw := RawPayload
      else if (Payload_method = 'dest') then
      begin
        f_raw := ECIESEncrypt(new_pubkey, RawPayload);
      end
      else if (Payload_method = 'sender') then
      begin
        f_raw := ECIESEncrypt(account_pubkey, RawPayload);
      end
      else if (Payload_method = 'aes') then
      begin
        f_raw := TAESComp.EVP_Encrypt_AES256(RawPayload, EncodePwd);
      end
      else
      begin
        ErrorNum := CT_RPC_ErrNum_InvalidOperation;
        ErrorDesc := 'Invalid encode payload method: ' + Payload_method;
        exit;
      end;
    end
    else
      f_raw := '';
    if account_signer = account_target then
    begin
      Result := TChangeKeyTransaction.Create(account_signer, account_last_n_operation + 1, account_target,
        TRPCServer.Instance.WalletKeys.Key[i].PrivateKey, new_pubkey, fee, f_raw);
    end
    else
    begin
      Result := TChangeKeySignedTransaction.Create(account_signer, account_last_n_operation + 1, account_target,
        TRPCServer.Instance.WalletKeys.Key[i].PrivateKey, new_pubkey, fee, f_raw);
    end;
    if not Result.HasValidSignature then
    begin
      FreeAndNil(Result);
      ErrorNum := CT_RPC_ErrNum_InternalError;
      ErrorDesc := 'Invalid signature';
      exit;
    end;
  end;

  function ChangeAccountKey(account_signer, account_target: cardinal; const new_pub_key: TAccountKey; fee: UInt64;
    const RawPayload: TRawBytes; const Payload_method, EncodePwd: AnsiString): Boolean;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  var
    opck: TChangeKeyTransaction;
    acc_signer: TAccount;
    errors: AnsiString;
    OPR: TTransactionData;
  begin
    FNode.SequenceLock.Acquire; // Use lock to prevent N_Operation race-condition on concurrent invocations
    try
      Result := false;
      if (account_signer < 0) or (account_signer >= FNode.BlockManager.AccountsCount) then
      begin
        ErrorDesc := 'Invalid account ' + Inttostr(account_signer);
        ErrorNum := CT_RPC_ErrNum_InvalidAccount;
        exit;
      end;
      acc_signer := FNode.TransactionStorage.AccountTransaction.Account(account_signer);

      opck := CreateOperationChangeKey(account_signer, acc_signer.NumberOfTransactions, account_target,
        acc_signer.accountInfo.AccountKey, new_pub_key, fee, RawPayload, Payload_method, EncodePwd);
      if not Assigned(opck) then
        exit;
      try
        if not FNode.AddTransaction(nil, opck, errors) then
        begin
          ErrorDesc := 'Error adding operation: ' + errors;
          ErrorNum := CT_RPC_ErrNum_InvalidOperation;
          exit;
        end;
        opck.GetTransactionData(0, account_signer, OPR);
        FillOperationResumeToJSONObject(OPR, GetResultObject);
        Result := true;
      finally
//        opck.Free;
      end;
    finally
      FNode.SequenceLock.Release;
    end;
  end;

// This function creates a TOpListAccountForSale without looking for actual state (cold wallet)
// It assumes that account_number,account_last_n_operation and account_pubkey are correct
  function CreateOperationListAccountForSale(account_signer, account_last_n_operation, account_listed: cardinal;
    const account_signer_pubkey: TAccountKey; account_price: UInt64; locked_until_block: cardinal;
    account_to_pay: cardinal; const new_account_pubkey: TAccountKey; fee: UInt64; RawPayload: TRawBytes;
    const Payload_method, EncodePwd: AnsiString): TListAccountForSaleTransaction;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  var
    i: integer;
    errors: AnsiString;
    f_raw: TRawBytes;
  begin
    Result := nil;
    i := TRPCServer.Instance.WalletKeys.IndexOfAccountKey(account_signer_pubkey);
    if (i < 0) then
    begin
      ErrorDesc := 'Private key not found in wallet: ' + (account_signer_pubkey.AccountPublicKeyExport);
      ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
      exit;
    end;
    if (not Assigned(TRPCServer.Instance.WalletKeys.Key[i].PrivateKey)) then
    begin
      if TRPCServer.Instance.WalletKeys.Key[i].CryptedKey <> '' then
      begin
        // Wallet is password protected
        ErrorDesc := 'Wallet is password protected';
        ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      end
      else
      begin
        ErrorDesc := 'Wallet private key not found in Wallet';
        ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      end;
      exit;
    end;
    if (length(RawPayload) > 0) then
    begin
      if (Payload_method = 'none') then
        f_raw := RawPayload
      else if (Payload_method = 'dest') and (new_account_pubkey.EC_OpenSSL_NID <> CT_TECDSA_Public_Nul.EC_OpenSSL_NID)
      then
      begin
        // If using 'dest', only will apply if there is a fixed new public key, otherwise will use current public key of account
        f_raw := ECIESEncrypt(new_account_pubkey, RawPayload);
      end
      else if (Payload_method = 'dest') or (Payload_method = 'sender') then
      begin
        f_raw := ECIESEncrypt(account_signer_pubkey, RawPayload);
      end
      else if (Payload_method = 'aes') then
      begin
        f_raw := TAESComp.EVP_Encrypt_AES256(RawPayload, EncodePwd);
      end
      else
      begin
        ErrorNum := CT_RPC_ErrNum_InvalidOperation;
        ErrorDesc := 'Invalid encode payload method: ' + Payload_method;
        exit;
      end;
    end
    else
      f_raw := '';
    Result := TListAccountForSaleTransaction.CreateListAccountForSale(account_signer, account_last_n_operation + 1,
      account_listed, account_price, fee, account_to_pay, new_account_pubkey, locked_until_block,
      TRPCServer.Instance.WalletKeys.Key[i].PrivateKey, f_raw);
    if not Result.HasValidSignature then
    begin
      FreeAndNil(Result);
      ErrorNum := CT_RPC_ErrNum_InternalError;
      ErrorDesc := 'Invalid signature';
      exit;
    end;
  end;

// This function creates a TOpDelistAccountForSale without looking for actual state (cold wallet)
// It assumes that account_number,account_last_n_operation are correct
  function CreateOperationDelistAccountForSale(account_signer, account_last_n_operation, account_delisted: cardinal;
    const account_signer_pubkey: TAccountKey; fee: UInt64; RawPayload: TRawBytes;
    const Payload_method, EncodePwd: AnsiString): TDelistAccountTransaction;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  var
    i: integer;
    f_raw: TRawBytes;
  begin
    Result := nil;
    i := TRPCServer.Instance.WalletKeys.IndexOfAccountKey(account_signer_pubkey);
    if (i < 0) then
    begin
      ErrorDesc := 'Private key not found in wallet: ' + (account_signer_pubkey.AccountPublicKeyExport);
      ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
      exit;
    end;
    if (not Assigned(TRPCServer.Instance.WalletKeys.Key[i].PrivateKey)) then
    begin
      if TRPCServer.Instance.WalletKeys.Key[i].CryptedKey <> '' then
      begin
        // Wallet is password protected
        ErrorDesc := 'Wallet is password protected';
        ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      end
      else
      begin
        ErrorDesc := 'Wallet private key not found in Wallet';
        ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      end;
      exit;
    end;
    if (length(RawPayload) > 0) then
    begin
      if (Payload_method = 'none') then
        f_raw := RawPayload
      else if (Payload_method = 'dest') or (Payload_method = 'sender') then
      begin
        f_raw := ECIESEncrypt(account_signer_pubkey, RawPayload);
      end
      else if (Payload_method = 'aes') then
      begin
        f_raw := TAESComp.EVP_Encrypt_AES256(RawPayload, EncodePwd);
      end
      else
      begin
        ErrorNum := CT_RPC_ErrNum_InvalidOperation;
        ErrorDesc := 'Invalid encode payload method: ' + Payload_method;
        exit;
      end;
    end
    else
      f_raw := '';
    Result := TDelistAccountTransaction.CreateDelistAccountForSale(account_signer, account_last_n_operation + 1,
      account_delisted, fee, TRPCServer.Instance.WalletKeys.Key[i].PrivateKey, f_raw);
    if not Result.HasValidSignature then
    begin
      FreeAndNil(Result);
      ErrorNum := CT_RPC_ErrNum_InternalError;
      ErrorDesc := 'Invalid signature';
      exit;
    end;
  end;

// This function creates a TOpBuyAccount without looking for actual state (cold wallet)
// It assumes that account_number,account_last_n_operation and account_pubkey are correct
// Also asumes that amount is >= price and other needed conditions
  function CreateOperationBuyAccount(account_number, account_last_n_operation: cardinal;
    const account_pubkey: TAccountKey; account_to_buy: cardinal; account_price, Amount: UInt64;
    account_to_pay: cardinal; const new_account_pubkey: TAccountKey; fee: UInt64; RawPayload: TRawBytes;
    const Payload_method, EncodePwd: AnsiString): TBuyAccountTransaction;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  var
    i: integer;
    errors: AnsiString;
    f_raw: TRawBytes;
  begin
    Result := nil;
    i := TRPCServer.Instance.WalletKeys.IndexOfAccountKey(account_pubkey);
    if (i < 0) then
    begin
      ErrorDesc := 'Private key not found in wallet: ' + (account_pubkey.AccountPublicKeyExport);
      ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
      exit;
    end;
    if (not Assigned(TRPCServer.Instance.WalletKeys.Key[i].PrivateKey)) then
    begin
      if TRPCServer.Instance.WalletKeys.Key[i].CryptedKey <> '' then
      begin
        // Wallet is password protected
        ErrorDesc := 'Wallet is password protected';
        ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      end
      else
      begin
        ErrorDesc := 'Wallet private key not found in Wallet';
        ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      end;
      exit;
    end;
    if (length(RawPayload) > 0) then
    begin
      if (Payload_method = 'none') then
        f_raw := RawPayload
      else if (Payload_method = 'dest') then
      begin
        f_raw := ECIESEncrypt(new_account_pubkey, RawPayload);
      end
      else if (Payload_method = 'sender') then
      begin
        f_raw := ECIESEncrypt(account_pubkey, RawPayload);
      end
      else if (Payload_method = 'aes') then
      begin
        f_raw := TAESComp.EVP_Encrypt_AES256(RawPayload, EncodePwd);
      end
      else
      begin
        ErrorNum := CT_RPC_ErrNum_InvalidOperation;
        ErrorDesc := 'Invalid encode payload method: ' + Payload_method;
        exit;
      end;
    end
    else
      f_raw := '';
    Result := TBuyAccountTransaction.CreateBuy(account_number, account_last_n_operation + 1, account_to_buy,
      account_to_pay, account_price, Amount, fee, new_account_pubkey,
      TRPCServer.Instance.WalletKeys.Key[i].PrivateKey, f_raw);
    if not Result.HasValidSignature then
    begin
      FreeAndNil(Result);
      ErrorNum := CT_RPC_ErrNum_InternalError;
      ErrorDesc := 'Invalid signature';
      exit;
    end;
  end;

  function GetCardinalsValues(ordinals_coma_separated: string; cardinals: TOrderedList; var errors: AnsiString)
    : Boolean;
  var
    i, istart: integer;
    ctxt: string;
    an: cardinal;
  begin
    Result := false;
    cardinals.Clear;
    errors := '';
    ctxt := '';
    istart := 1;
    for i := 1 to length(ordinals_coma_separated) do
    begin
      case ordinals_coma_separated[i] of
        '0' .. '9', '-':
          ctxt := ctxt + ordinals_coma_separated[i];
        ',', ';':
          begin
            if Trim(ctxt) <> '' then
            begin
              if not TAccount.ParseAccountNumber(Trim(ctxt), an) then
              begin
                errors := 'Invalid account number at pos ' + Inttostr(istart) + ': ' + ctxt;
                exit;
              end;
              cardinals.Add(an);
            end;
            ctxt := '';
            istart := i + 1;
          end;
        ' ':
          ; // Continue...
      else
        errors := 'Invalid char at pos ' + Inttostr(i) + ': "' + ordinals_coma_separated[i] + '"';
        exit;
      end;
    end;
    //
    if (Trim(ctxt) <> '') then
    begin
      if not TAccount.ParseAccountNumber(Trim(ctxt), an) then
      begin
        errors := 'Invalid account number at pos ' + Inttostr(istart) + ': ' + ctxt;
        exit;
      end;
      cardinals.Add(an);
    end;
    if cardinals.Count = 0 then
    begin
      errors := 'No valid value';
      exit;
    end;
    Result := true;
  end;

  function ChangeAccountsKey(accounts_txt: string; const new_pub_key: TAccountKey; fee: UInt64;
    const RawPayload: TRawBytes; const Payload_method, EncodePwd: AnsiString): Boolean;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  var
    opck: TChangeKeyTransaction;
    acc: TAccount;
    i, ian: integer;
    errors: AnsiString;
    OPR: TTransactionData;
    accountsnumber: TOrderedList;
    operationsht: TTransactionHashTree;
    OperationsResumeList: TTransactionList;
  begin
    FNode.SequenceLock.Acquire; // Use lock to prevent N_Operation race-condition on concurrent invocations
    try
      Result := false;
      accountsnumber := TOrderedList.Create;
      try
        if not GetCardinalsValues(accounts_txt, accountsnumber, errors) then
        begin
          ErrorDesc := 'Error in accounts: ' + errors;
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          exit;
        end;
        operationsht := TTransactionHashTree.Create;
        try
          for ian := 0 to accountsnumber.Count - 1 do
          begin

            if (accountsnumber.Get(ian) < 0) or (accountsnumber.Get(ian) >= FNode.BlockManager.AccountsCount) then
            begin
              ErrorDesc := 'Invalid account ' + Inttostr(accountsnumber.Get(ian));
              ErrorNum := CT_RPC_ErrNum_InvalidAccount;
              exit;
            end;
            acc := FNode.TransactionStorage.AccountTransaction.Account(accountsnumber.Get(ian));
            opck := CreateOperationChangeKey(acc.AccountNumber, acc.NumberOfTransactions, acc.AccountNumber, acc.accountInfo.AccountKey,
              new_pub_key, fee, RawPayload, Payload_method, EncodePwd);
            if not Assigned(opck) then
              exit;
            try
              operationsht.AddTransactionToHashTree(opck);
            finally
//              opck.Free;
            end;
          end; // For
          // Ready to execute...
          OperationsResumeList := TTransactionList.Create;
          try
            i := FNode.AddOperations(nil, operationsht, OperationsResumeList, errors);
            if (i < 0) then
            begin
              ErrorNum := CT_RPC_ErrNum_InternalError;
              ErrorDesc := errors;
              exit;
            end;
            GetResultArray.Clear; // Inits an array
            for i := 0 to OperationsResumeList.Count - 1 do
            begin
              FillOperationResumeToJSONObject(OperationsResumeList[i], GetResultArray.GetAsObject(i));
            end;
          finally
            OperationsResumeList.Free;
          end;
          Result := true;
        finally
          operationsht.Free;
        end;
      finally
        accountsnumber.Free;
      end;
    finally
      FNode.SequenceLock.Release;
    end;
  end;

  function SignOpChangeKey(const HexaStringOperationsHashTree: TRawBytes; account_signer, account_target: cardinal;
    const actualAccounKey, newAccountKey: TAccountKey; last_n_operation: cardinal; fee: UInt64;
    const RawPayload: TRawBytes; const Payload_method, EncodePwd: AnsiString): Boolean;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  var
    OperationsHashTree: TTransactionHashTree;
    errors: AnsiString;
    opck: TChangeKeyTransaction;
  begin
    Result := false;
    if not HexaStringToOperationsHashTree(HexaStringOperationsHashTree, OperationsHashTree, errors) then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Error decoding param "rawoperations": ' + errors;
      exit;
    end;
    try
      opck := CreateOperationChangeKey(account_signer, last_n_operation, account_target, actualAccounKey, newAccountKey,
        fee, RawPayload, Payload_method, EncodePwd);
      if opck = nil then
        exit;
      try
        OperationsHashTree.AddTransactionToHashTree(opck);
        FillOperationsHashTreeToJSONObject(OperationsHashTree, GetResultObject);
        Result := true;
      finally
//        opck.Free;
      end;
    finally
      OperationsHashTree.Free;
    end;
  end;

  function OperationsInfo(const HexaStringOperationsHashTree: TRawBytes; jsonArray: TPCJSONArray): Boolean;
  var
    xTransactionHashTree: TTransactionHashTree;
    xErrors: AnsiString;
    xTransactionData: TTransactionData;
    xJsonObject: TPCJSONObject;
    xTransaction: ITransaction;
    i: integer;
  begin
    if not HexaStringToOperationsHashTree(HexaStringOperationsHashTree, xTransactionHashTree, xErrors) then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Error decoding param "rawoperations": ' + xErrors;
      exit;
    end;
    try
      jsonArray.Clear;
      for i := 0 to xTransactionHashTree.TransactionCount - 1 do
      begin
        xTransaction := xTransactionHashTree.GetTransaction(i);
        xJsonObject := jsonArray.GetAsObject(i);
        if xTransaction.GetTransactionData(0, xTransaction.SignerAccount, xTransactionData) then
        begin
          xTransactionData.NOpInsideBlock := i;
          xTransactionData.balance := -1;
        end
        else
          xTransactionData := TTransactionData.Empty;
        FillOperationResumeToJSONObject(xTransactionData, xJsonObject);
      end;
      Result := true;
    finally
      xTransactionHashTree.Free;
    end;
  end;

  function ExecuteOperations(const HexaStringOperationsHashTree: TRawBytes): Boolean;
  var
    OperationsHashTree: TTransactionHashTree;
    errors: AnsiString;
    i: integer;
    OperationsResumeList: TTransactionList;
  begin
    if not HexaStringToOperationsHashTree(HexaStringOperationsHashTree, OperationsHashTree, errors) then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Error decoding param "rawoperations": ' + errors;
      exit;
    end;
    try
      errors := '';
      OperationsResumeList := TTransactionList.Create;
      try
        i := FNode.AddOperations(nil, OperationsHashTree, OperationsResumeList, errors);
        if (i < 0) then
        begin
          ErrorNum := CT_RPC_ErrNum_InternalError;
          ErrorDesc := errors;
          exit;
        end;
        GetResultArray.Clear; // Inits an array
        for i := 0 to OperationsResumeList.Count - 1 do
        begin
          FillOperationResumeToJSONObject(OperationsResumeList[i], GetResultArray.GetAsObject(i));
        end;
      finally
        OperationsResumeList.Free;
      end;
      Result := true;
    finally
      OperationsHashTree.Free;
    end;
  end;

  procedure FillAccountObject(const Account: TAccount; jsonobj: TPCJSONObject);
  begin
    jsonobj.GetAsVariant('account').Value := Account.AccountNumber;
    jsonobj.GetAsVariant('enc_pubkey').Value := TBaseType.ToHexaString(Account.accountInfo.AccountKey.ToRawString);
    jsonobj.GetAsVariant('balance').Value := ToJSONCurrency(Account.Balance);
    jsonobj.GetAsVariant('n_operation').Value := Account.NumberOfTransactions;
    jsonobj.GetAsVariant('updated_b').Value := Account.UpdatedBlock;
    case Account.accountInfo.State of
      as_Normal:
        jsonobj.GetAsVariant('state').Value := 'normal';
      as_ForSale:
        begin
          jsonobj.GetAsVariant('state').Value := 'listed';
          jsonobj.GetAsVariant('locked_until_block').Value := Account.accountInfo.LockedUntilBlock;
          jsonobj.GetAsVariant('price').Value := Account.accountInfo.Price;
          jsonobj.GetAsVariant('seller_account').Value := Account.accountInfo.AccountToPay;
          jsonobj.GetAsVariant('private_sale').Value := (Account.accountInfo.NewPublicKey.EC_OpenSSL_NID <> 0);
          if not(Account.accountInfo.NewPublicKey.EC_OpenSSL_NID <> 0) then
          begin
            jsonobj.GetAsVariant('new_enc_pubkey').Value :=
              TBaseType.ToHexaString(Account.accountInfo.NewPublicKey.ToRawString);
          end;
        end
    else
      raise Exception.Create('ERROR DEV 20170425-1');
    end;
    jsonobj.GetAsVariant('name').Value := Account.Name;
    jsonobj.GetAsVariant('type').Value := Account.AccountType;
  end;

  procedure FillPublicKeyObject(const PubKey: TAccountKey; jsonobj: TPCJSONObject);
  begin
    jsonobj.GetAsVariant('ec_nid').Value := PubKey.EC_OpenSSL_NID;
    jsonobj.GetAsVariant('x').Value := TBaseType.ToHexaString(PubKey.x);
    jsonobj.GetAsVariant('y').Value := TBaseType.ToHexaString(PubKey.y);
    jsonobj.GetAsVariant('enc_pubkey').Value := TBaseType.ToHexaString(PubKey.ToRawString);
    jsonobj.GetAsVariant('b58_pubkey').Value := (PubKey.AccountPublicKeyExport);
  end;

  function DoEncrypt(RawPayload: TRawBytes; pub_key: TAccountKey; const Payload_method, EncodePwd: AnsiString): Boolean;
  var
    f_raw: TRawBytes;
  begin
    Result := false;
    if (length(RawPayload) > 0) then
    begin
      if (Payload_method = 'none') then
        f_raw := RawPayload
      else if (Payload_method = 'pubkey') then
      begin
        f_raw := ECIESEncrypt(pub_key, RawPayload);
      end
      else if (Payload_method = 'aes') then
      begin
        f_raw := TAESComp.EVP_Encrypt_AES256(RawPayload, EncodePwd);
      end
      else
      begin
        ErrorNum := CT_RPC_ErrNum_InvalidOperation;
        ErrorDesc := 'Invalid encode payload method: ' + Payload_method;
        exit;
      end;
    end
    else
      f_raw := '';
    jsonresponse.GetAsVariant('result').Value := TBaseType.ToHexaString(f_raw);
    Result := true;
  end;

  function DoDecrypt(RawEncryptedPayload: TRawBytes; jsonArrayPwds: TPCJSONArray): Boolean;
  var
    i: integer;
    pkey: TECPrivateKey;
    decrypted_payload: TRawBytes;
  begin
    Result := false;
    if RawEncryptedPayload = '' then
    begin
      GetResultObject.GetAsVariant('result').Value := false;
      GetResultObject.GetAsVariant('enc_payload').Value := '';
      // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
      Result := true;
      exit;
    end;
    for i := 0 to TRPCServer.Instance.WalletKeys.Count - 1 do
    begin
      pkey := TRPCServer.Instance.WalletKeys.Key[i].PrivateKey;
      if (Assigned(pkey)) then
      begin
        if ECIESDecrypt(pkey.EC_OpenSSL_NID, pkey.PrivateKey, false, RawEncryptedPayload, decrypted_payload) then
        begin
          GetResultObject.GetAsVariant('result').Value := true;
          GetResultObject.GetAsVariant('enc_payload').Value := TBaseType.ToHexaString(RawEncryptedPayload);
          GetResultObject.GetAsVariant('unenc_payload').Value := decrypted_payload;
          GetResultObject.GetAsVariant('unenc_hexpayload').Value := TBaseType.ToHexaString(decrypted_payload);
          GetResultObject.GetAsVariant('payload_method').Value := 'key';
          GetResultObject.GetAsVariant('enc_pubkey').Value := TBaseType.ToHexaString(pkey.PublicKey.ToRawString);
          // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
          Result := true;
          exit;
        end;
      end;
    end;
    for i := 0 to jsonArrayPwds.Count - 1 do
    begin
      if TAESComp.EVP_Decrypt_AES256(RawEncryptedPayload, jsonArrayPwds.GetAsVariant(i).AsString(''), decrypted_payload)
      then
      begin
        GetResultObject.GetAsVariant('result').Value := true;
        GetResultObject.GetAsVariant('enc_payload').Value := TBaseType.ToHexaString(RawEncryptedPayload);
        GetResultObject.GetAsVariant('unenc_payload').Value := decrypted_payload;
        GetResultObject.GetAsVariant('unenc_hexpayload').Value := TBaseType.ToHexaString(decrypted_payload);
        GetResultObject.GetAsVariant('payload_method').Value := 'pwd';
        GetResultObject.GetAsVariant('pwd').Value := jsonArrayPwds.GetAsVariant(i).AsString('');
        // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
        Result := true;
        exit;
      end;
    end;
    // Not found
    GetResultObject.GetAsVariant('result').Value := false;
    GetResultObject.GetAsVariant('enc_payload').Value := TBaseType.ToHexaString(RawEncryptedPayload);
    Result := true;
  end;

  function CapturePubKey(const prefix: string; var PubKey: TAccountKey; var errortxt: string): Boolean;
  var
    ansistr: AnsiString;
    auxpubkey: TAccountKey;
  begin
    PubKey := TAccount.Empty.AccountInfo.AccountKey;
    errortxt := '';
    Result := false;
    if (params.IndexOfName(prefix + 'b58_pubkey') >= 0) then
    begin
      if not TAccountKey.AccountPublicKeyImport(params.AsString(prefix + 'b58_pubkey', ''), PubKey, ansistr) then
      begin
        errortxt := 'Invalid value of param "' + prefix + 'b58_pubkey": ' + ansistr;
        exit;
      end;
      if (params.IndexOfName(prefix + 'enc_pubkey') >= 0) then
      begin
        auxpubkey := TAccountKey.FromRawString(TBaseType.HexaToRaw(params.AsString(prefix + 'enc_pubkey', '')));
        if (not TAccountKey.EqualAccountKeys(auxpubkey, PubKey)) then
        begin
          errortxt := 'Params "' + prefix + 'b58_pubkey" and "' + prefix +
            'enc_pubkey" public keys are not the same public key';
          exit;
        end;
      end;
    end
    else
    begin
      if (params.IndexOfName(prefix + 'enc_pubkey') < 0) then
      begin
        errortxt := 'Need param "' + prefix + 'enc_pubkey" or "' + prefix + 'b58_pubkey"';
        exit;
      end;
      PubKey := TAccountKey.FromRawString(TBaseType.HexaToRaw(params.AsString(prefix + 'enc_pubkey', '')));
    end;
    if not PubKey.IsValidAccountKey(ansistr) then
    begin
      errortxt := 'Invalid public key: ' + ansistr;
    end
    else
      Result := true;
  end;

  function SignListAccountForSaleEx(params: TPCJSONObject; OperationsHashTree: TTransactionHashTree;
    const actualAccounKey: TAccountKey; last_n_operation: cardinal): Boolean;
  // params:
  // "account_signer" is the account that signs operations and pays the fee
  // "account_target" is the account being listed
  // "locked_until_block" is until which block will be locked this account (Note: A locked account cannot change it's state until sold or finished lock)
  // "price" is the price
  // "seller_account" is the account to pay (seller account)
  // "new_b58_pubkey" or "new_enc_pubke" is the future public key for this sale (private sale), otherwise is open and everybody can buy
  var
    opSale: TListAccountForSaleTransaction;
    account_signer, account_target, seller_account: cardinal;
    locked_until_block: cardinal;
    price, fee: Int64;
    new_pubkey: TAccountKey;
  begin
    Result := false;
    account_signer := params.AsInteger('account_signer', MaxInt);
    if (account_signer >= MaxInt) then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Invalid "account_signer" account';
      exit;
    end;
    account_target := params.AsInteger('account_target', MaxInt);
    if (account_target >= MaxInt) then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Invalid "account_target" account';
      exit;
    end;
    seller_account := params.AsInteger('seller_account', MaxInt);
    if (seller_account >= MaxInt) then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Invalid "seller_account" to pay';
      exit;
    end;
    locked_until_block := params.AsInteger('locked_until_block', MaxInt);
    if (locked_until_block >= MaxInt) then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Invalid "locked_until_block" value';
      exit;
    end;
    price := ToMicroCoins(params.AsDouble('price', 0));
    if (price = 0) then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Invalid price value';
      exit;
    end;
    fee := ToMicroCoins(params.AsDouble('fee', 0));
    if (fee < 0) then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Invalid fee value';
      exit;
    end;
    if (params.IndexOfName('new_b58_pubkey') >= 0) or (params.IndexOfName('new_enc_pubkey') >= 0) then
    begin
      if not CapturePubKey('new_', new_pubkey, ErrorDesc) then
      begin
        ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
        exit;
      end;
    end
    else
      new_pubkey := CT_TECDSA_Public_Nul;
    opSale := CreateOperationListAccountForSale(account_signer, last_n_operation, account_target, actualAccounKey,
      price, locked_until_block, seller_account, new_pubkey, fee, TBaseType.HexaToRaw(params.AsString('payload', '')),
      params.AsString('payload_method', 'dest'), params.AsString('pwd', ''));
    if opSale = nil then
      exit;
    try
      OperationsHashTree.AddTransactionToHashTree(opSale);
      Result := true;
    finally
//      opSale.Free;
    end;
  end;

  function SignListAccountForSaleColdWallet(const HexaStringOperationsHashTree: TRawBytes;
    params: TPCJSONObject): Boolean;
  var
    errors: AnsiString;
    OperationsHashTree: TTransactionHashTree;
    accountpubkey: TAccountKey;
    last_n_operation: cardinal;
  begin
    Result := false;
    if not HexaStringToOperationsHashTree(HexaStringOperationsHashTree, OperationsHashTree, errors) then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Error decoding param previous operations hash tree raw value: ' + errors;
      exit;
    end;
    try
      if not CapturePubKey('signer_', accountpubkey, ErrorDesc) then
      begin
        ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
        ErrorDesc := 'Params "signer_b58_pubkey" or "signer_enc_pubkey" not found';
        exit;
      end;
      last_n_operation := params.AsCardinal('last_n_operation', 0);
      if not SignListAccountForSaleEx(params, OperationsHashTree, accountpubkey, last_n_operation) then
        exit
      else
        Result := true;
      FillOperationsHashTreeToJSONObject(OperationsHashTree, GetResultObject);
    finally
      OperationsHashTree.Free;
    end;
  end;

  function SignDelistAccountForSaleEx(params: TPCJSONObject; OperationsHashTree: TTransactionHashTree;
    const actualAccountKey: TAccountKey; last_n_operation: cardinal): Boolean;
  // params:
  // "account_signer" is the account that signs operations and pays the fee
  // "account_target" is the delisted account
  // "locked_until_block" is until which block will be locked this account (Note: A locked account cannot change it's state until sold or finished lock)
  // "price" is the price
  // "seller_account" is the account to pay
  // "new_b58_pubkey" or "new_enc_pubke" is the future public key for this sale (private sale), otherwise is open and everybody can buy
  var
    opDelist: TDelistAccountTransaction;
    account_signer, account_target: cardinal;
    fee: Int64;
  begin
    Result := false;
    account_signer := params.AsInteger('account_signer', MaxInt);
    if (account_signer >= MaxInt) then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Invalid "account_signer" account';
      exit;
    end;
    account_target := params.AsInteger('account_target', MaxInt);
    if (account_target >= MaxInt) then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Invalid "account_target" account';
      exit;
    end;
    fee := ToMicroCoins(params.AsDouble('fee', 0));
    if (fee < 0) then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Invalid fee value';
      exit;
    end;
    opDelist := CreateOperationDelistAccountForSale(account_signer, last_n_operation, account_target, actualAccountKey,
      fee, TBaseType.HexaToRaw(params.AsString('payload', '')), params.AsString('payload_method', 'dest'),
      params.AsString('pwd', ''));
    if opDelist = nil then
      exit;
    try
      OperationsHashTree.AddTransactionToHashTree(opDelist);
      Result := true;
    finally
//      opDelist.Free;
    end;
  end;

// This function creates a TOpChangeAccountInfo without looking for actual state (cold wallet)
// It assumes that account_number,account_last_n_operation and account_pubkey are correct
  function CreateOperationChangeAccountInfo(account_signer, account_last_n_operation, account_target: cardinal;
    const account_signer_pubkey: TAccountKey; changePubKey: Boolean; const new_account_pubkey: TAccountKey;
    changeName: Boolean; const new_name: TRawBytes; changeType: Boolean; new_type: Word; fee: UInt64;
    RawPayload: TRawBytes; const Payload_method, EncodePwd: AnsiString): TChangeAccountInfoTransaction;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  var
    i: integer;
    errors: AnsiString;
    f_raw: TRawBytes;
  begin
    Result := nil;
    i := TRPCServer.Instance.WalletKeys.IndexOfAccountKey(account_signer_pubkey);
    if (i < 0) then
    begin
      ErrorDesc := 'Private key not found in wallet: ' + (account_signer_pubkey.AccountPublicKeyExport);
      ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
      exit;
    end;
    if (not Assigned(TRPCServer.Instance.WalletKeys.Key[i].PrivateKey)) then
    begin
      if TRPCServer.Instance.WalletKeys.Key[i].CryptedKey <> '' then
      begin
        // Wallet is password protected
        ErrorDesc := 'Wallet is password protected';
        ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      end
      else
      begin
        ErrorDesc := 'Wallet private key not found in Wallet';
        ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      end;
      exit;
    end;
    if (length(RawPayload) > 0) then
    begin
      if (Payload_method = 'none') then
        f_raw := RawPayload
      else if (Payload_method = 'dest') and (new_account_pubkey.EC_OpenSSL_NID <> CT_TECDSA_Public_Nul.EC_OpenSSL_NID)
      then
      begin
        // If using 'dest', only will apply if there is a fixed new public key, otherwise will use current public key of account
        f_raw := ECIESEncrypt(new_account_pubkey, RawPayload);
      end
      else if (Payload_method = 'dest') or (Payload_method = 'sender') then
      begin
        f_raw := ECIESEncrypt(account_signer_pubkey, RawPayload);
      end
      else if (Payload_method = 'aes') then
      begin
        f_raw := TAESComp.EVP_Encrypt_AES256(RawPayload, EncodePwd);
      end
      else
      begin
        ErrorNum := CT_RPC_ErrNum_InvalidOperation;
        ErrorDesc := 'Invalid encode payload method: ' + Payload_method;
        exit;
      end;
    end
    else
      f_raw := '';
    Result := TChangeAccountInfoTransaction.CreateChangeAccountInfo(account_signer, account_last_n_operation + 1, account_target,
      TRPCServer.Instance.WalletKeys.Key[i].PrivateKey, changePubKey, new_account_pubkey, changeName, new_name,
      changeType, new_type, fee, f_raw);
    if not Result.HasValidSignature then
    begin
      FreeAndNil(Result);
      ErrorNum := CT_RPC_ErrNum_InternalError;
      ErrorDesc := 'Invalid signature';
      exit;
    end;
  end;

  function SignChangeAccountInfoEx(params: TPCJSONObject; OperationsHashTree: TTransactionHashTree;
    const actualAccountKey: TAccountKey; last_n_operation: cardinal): Boolean;
  // params:
  // "account_signer" is the account that signs operations and pays the fee
  // "account_target" is the target to change info
  // "new_b58_pubkey" or "new_enc_pubke" is the new public key for this account
  // "new_name" is the new account name
  // "new_type" is the new account type
  var
    opChangeInfo: TChangeAccountInfoTransaction;
    account_signer, account_target: cardinal;
    fee: Int64;
    ChangeKey, changeName, changeType: Boolean;
    new_name: AnsiString;
    new_type: Word;
    new_typeI: integer;
    new_pubkey: TAccountKey;
  begin
    Result := false;
    account_signer := params.AsInteger('account_signer', MaxInt);
    if (account_signer >= MaxInt) then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Invalid "account_signer" account';
      exit;
    end;
    account_target := params.AsInteger('account_target', MaxInt);
    if (account_target >= MaxInt) then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Invalid "account_target" account';
      exit;
    end;
    fee := ToMicroCoins(params.AsDouble('fee', 0));
    if (fee < 0) then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Invalid fee value';
      exit;
    end;
    if (params.IndexOfName('new_b58_pubkey') >= 0) or (params.IndexOfName('new_enc_pubkey') >= 0) then
    begin
      ChangeKey := true;
      if not CapturePubKey('new_', new_pubkey, ErrorDesc) then
      begin
        ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
        exit;
      end;
    end
    else
    begin
      new_pubkey := CT_TECDSA_Public_Nul;
      ChangeKey := false;
    end;
    if (params.IndexOfName('new_name') >= 0) then
    begin
      changeName := true;
      new_name := params.AsString('new_name', ' ');
    end
    else
    begin
      new_name := '';
      changeName := false;
    end;
    if (params.IndexOfName('new_type') >= 0) then
    begin
      changeType := true;
      new_typeI := params.AsInteger('new_type', -1);
      if (new_typeI < 0) or (new_typeI > 65536) then
      begin
        ErrorNum := CT_RPC_ErrNum_InvalidData;
        ErrorDesc := 'Invalid new type value ' + Inttostr(new_typeI);
        exit;
      end;
      new_type := new_typeI;
    end
    else
    begin
      new_type := 0;
      changeType := false;
    end;

    opChangeInfo := CreateOperationChangeAccountInfo(account_signer, last_n_operation, account_target, actualAccountKey,
      ChangeKey, new_pubkey, changeName, new_name, changeType, new_type, fee,
      TBaseType.HexaToRaw(params.AsString('payload', '')), params.AsString('payload_method', 'dest'),
      params.AsString('pwd', ''));
    if opChangeInfo = nil then
      exit;
    try
      OperationsHashTree.AddTransactionToHashTree(opChangeInfo);
      Result := true;
    finally
//      opChangeInfo.Free;
    end;
  end;

  function SignChangeAccountInfoColdWallet(const HexaStringOperationsHashTree: TRawBytes;
    params: TPCJSONObject): Boolean;
  var
    errors: AnsiString;
    OperationsHashTree: TTransactionHashTree;
    accountpubkey: TAccountKey;
    last_n_operation: cardinal;
  begin
    Result := false;
    if not HexaStringToOperationsHashTree(HexaStringOperationsHashTree, OperationsHashTree, errors) then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Error decoding param previous operations hash tree raw value: ' + errors;
      exit;
    end;
    try
      if not CapturePubKey('signer_', accountpubkey, ErrorDesc) then
      begin
        ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
        ErrorDesc := 'Params "signer_b58_pubkey" or "signer_enc_pubkey" not found';
        exit;
      end;
      last_n_operation := params.AsCardinal('last_n_operation', 0);
      if not SignChangeAccountInfoEx(params, OperationsHashTree, accountpubkey, last_n_operation) then
        exit
      else
        Result := true;
      FillOperationsHashTreeToJSONObject(OperationsHashTree, GetResultObject);
    finally
      OperationsHashTree.Free;
    end;
  end;

  function SignDelistAccountForSaleColdWallet(const HexaStringOperationsHashTree: TRawBytes;
    params: TPCJSONObject): Boolean;
  var
    errors: AnsiString;
    OperationsHashTree: TTransactionHashTree;
    accountpubkey: TAccountKey;
    last_n_operation: cardinal;
  begin
    Result := false;
    if not HexaStringToOperationsHashTree(HexaStringOperationsHashTree, OperationsHashTree, errors) then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Error decoding param previous operations hash tree raw value: ' + errors;
      exit;
    end;
    try
      if not CapturePubKey('signer_', accountpubkey, ErrorDesc) then
      begin
        ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
        ErrorDesc := 'Params "signer_b58_pubkey" or "signer_enc_pubkey" not found';
        exit;
      end;
      last_n_operation := params.AsCardinal('last_n_operation', 0);
      if not SignDelistAccountForSaleEx(params, OperationsHashTree, accountpubkey, last_n_operation) then
        exit
      else
        Result := true;
      FillOperationsHashTreeToJSONObject(OperationsHashTree, GetResultObject);
    finally
      OperationsHashTree.Free;
    end;
  end;

  function SignBuyAccountEx(params: TPCJSONObject; OperationsHashTree: TTransactionHashTree;
    const buyerAccountKey: TAccountKey; last_n_operation: cardinal): Boolean;
  // params:
  // "buyer_account" is the buyer account
  // "account_to_purchase" is the account to purchase
  // "price" is the price
  // "seller_account" is the account to pay
  // "new_b58_pubkey" or "new_enc_pubkey" is the future public key for this sale (private sale), otherwise is open and everybody can buy
  // "amount" is the transferred amount to pay (can exceed price)
  var
    opBuy: TBuyAccountTransaction;
    buyer_account, account_to_purchase, seller_account: cardinal;
    price, Amount, fee: Int64;
    new_pubkey: TAccountKey;
  begin
    Result := false;
    buyer_account := params.AsInteger('buyer_account', MaxInt);
    if (buyer_account >= MaxInt) then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Invalid buyer account';
      exit;
    end;
    account_to_purchase := params.AsInteger('account_to_purchase', MaxInt);
    if (account_to_purchase >= MaxInt) then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Invalid account to purchase';
      exit;
    end;
    seller_account := params.AsInteger('seller_account', MaxInt);
    if (seller_account >= MaxInt) then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Invalid seller account';
      exit;
    end;
    price := ToMicroCoins(params.AsDouble('price', 0));
    if (price < 0) then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Invalid price value';
      exit;
    end;
    Amount := ToMicroCoins(params.AsDouble('amount', 0));
    if (Amount < 0) then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Invalid amount value';
      exit;
    end;
    fee := ToMicroCoins(params.AsDouble('fee', 0));
    if (fee < 0) then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Invalid fee value';
      exit;
    end;
    if (params.IndexOfName('new_b58_pubkey') >= 0) or (params.IndexOfName('new_enc_pubkey') >= 0) then
    begin
      if not CapturePubKey('new_', new_pubkey, ErrorDesc) then
      begin
        ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
        exit;
      end;
    end
    else
      new_pubkey := CT_TECDSA_Public_Nul;
    opBuy := CreateOperationBuyAccount(buyer_account, last_n_operation, buyerAccountKey, account_to_purchase, price,
      Amount, seller_account, new_pubkey, fee, TBaseType.HexaToRaw(params.AsString('payload', '')),
      params.AsString('payload_method', 'dest'), params.AsString('pwd', ''));
    if opBuy = nil then
      exit;
    try
      OperationsHashTree.AddTransactionToHashTree(opBuy);
      Result := true;
    finally
//      opBuy.Free;
    end;
  end;

  function SignBuyAccountColdWallet(const HexaStringOperationsHashTree: TRawBytes; params: TPCJSONObject): Boolean;
  var
    errors: AnsiString;
    OperationsHashTree: TTransactionHashTree;
    accountpubkey: TAccountKey;
    last_n_operation: cardinal;
  begin
    Result := false;
    if not HexaStringToOperationsHashTree(HexaStringOperationsHashTree, OperationsHashTree, errors) then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Error decoding param previous operations hash tree raw value: ' + errors;
      exit;
    end;
    try
      if not CapturePubKey('signer_', accountpubkey, ErrorDesc) then
      begin
        ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
        ErrorDesc := 'Params "signer_b58_pubkey" or "signer_enc_pubkey" not found';
        exit;
      end;
      last_n_operation := params.AsCardinal('last_n_operation', 0);
      if not SignBuyAccountEx(params, OperationsHashTree, accountpubkey, last_n_operation) then
        exit
      else
        Result := true;
      FillOperationsHashTreeToJSONObject(OperationsHashTree, GetResultObject);
    finally
      OperationsHashTree.Free;
    end;
  end;

  function ListAccountForSale(params: TPCJSONObject): Boolean;
  var
    OperationsHashTree: TTransactionHashTree;
    account_signer, account_target: TAccount;
    opt: ITransaction;
    OPR: TTransactionData;
    errors: AnsiString;
    c_account: cardinal;
  begin
    FNode.SequenceLock.Acquire; // Use lock to prevent N_Operation race-condition on concurrent invocations
    try
      Result := false;
      OperationsHashTree := TTransactionHashTree.Create;
      try
        if (params.IndexOfName('account_signer') < 0) then
        begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Need account_signer param';
          exit;
        end;
        c_account := params.AsCardinal('account_signer', 0);
        if (c_account < 0) or (c_account >= FNode.BlockManager.AccountsCount) then
        begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Invalid account_signer ' + params.AsString('account_signer', '');
          exit;
        end;
        account_signer := FNode.TransactionStorage.AccountTransaction.Account(c_account);
        if (params.IndexOfName('account_target') < 0) then
        begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Need account_target param';
          exit;
        end;
        c_account := params.AsCardinal('account_target', 0);
        if (c_account < 0) or (c_account >= FNode.BlockManager.AccountsCount) then
        begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Invalid account_target ' + params.AsString('account_target', '');
          exit;
        end;
        account_target := FNode.TransactionStorage.AccountTransaction.Account(c_account);
        if (not TAccountKey.EqualAccountKeys(account_signer.accountInfo.AccountKey,
          account_target.accountInfo.AccountKey)) then
        begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'account_signer and account_target have distinct keys. Cannot sign';
          exit;
        end;
        if not SignListAccountForSaleEx(params, OperationsHashTree, account_signer.accountInfo.AccountKey,
          account_signer.NumberOfTransactions) then
          exit;
        opt := OperationsHashTree.GetTransaction(0);
        if not FNode.AddTransaction(nil, opt, errors) then
        begin
          ErrorNum := CT_RPC_ErrNum_InternalError;
          ErrorDesc := errors;
          exit;
        end
        else
          Result := true;
        opt.GetTransactionData(0, c_account, OPR);
        FillOperationResumeToJSONObject(OPR, GetResultObject);
      finally
        OperationsHashTree.Free;
      end;
    finally
      FNode.SequenceLock.Release;
    end;
  end;

  function DelistAccountForSale(params: TPCJSONObject): Boolean;
  var
    OperationsHashTree: TTransactionHashTree;
    account_signer, account_target: TAccount;
    opt: ITransaction;
    OPR: TTransactionData;
    errors: AnsiString;
    c_account: cardinal;
  begin
    FNode.SequenceLock.Acquire; // Use lock to prevent N_Operation race-condition on concurrent invocations
    try
      Result := false;
      OperationsHashTree := TTransactionHashTree.Create;
      try
        if (params.IndexOfName('account_signer') < 0) then
        begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Need account_signer param';
          exit;
        end;
        c_account := params.AsCardinal('account_signer', 0);
        if (c_account < 0) or (c_account >= FNode.BlockManager.AccountsCount) then
        begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Invalid account_signer ' + params.AsString('account_signer', '');
          exit;
        end;
        account_signer := FNode.TransactionStorage.AccountTransaction.Account(c_account);
        if (params.IndexOfName('account_target') < 0) then
        begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Need account_target param';
          exit;
        end;
        c_account := params.AsCardinal('account_target', 0);
        if (c_account < 0) or (c_account >= FNode.BlockManager.AccountsCount) then
        begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Invalid account_target ' + params.AsString('account_target', '');
          exit;
        end;
        account_target := FNode.TransactionStorage.AccountTransaction.Account(c_account);
        if (not TAccountKey.EqualAccountKeys(account_signer.accountInfo.AccountKey,
          account_target.accountInfo.AccountKey)) then
        begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'account_signer and account_target have distinct keys. Cannot sign';
          exit;
        end;
        if not SignDelistAccountForSaleEx(params, OperationsHashTree, account_signer.accountInfo.AccountKey,
          account_signer.NumberOfTransactions) then
          exit;
        opt := OperationsHashTree.GetTransaction(0);
        if not FNode.AddTransaction(nil, opt, errors) then
        begin
          ErrorNum := CT_RPC_ErrNum_InternalError;
          ErrorDesc := errors;
          exit;
        end
        else
          Result := true;
        opt.GetTransactionData(0, c_account, OPR);
        FillOperationResumeToJSONObject(OPR, GetResultObject);
      finally
        OperationsHashTree.Free;
      end;
    finally
      FNode.SequenceLock.Acquire;
    end;
  end;

  function BuyAccount(params: TPCJSONObject): Boolean;
  var
    OperationsHashTree: TTransactionHashTree;
    buyer_account: TAccount;
    opt: ITransaction;
    OPR: TTransactionData;
    errors: AnsiString;
    c_account: cardinal;
  begin
    FNode.SequenceLock.Acquire; // Use lock to prevent N_Operation race-condition on concurrent invocations
    try
      Result := false;
      OperationsHashTree := TTransactionHashTree.Create;
      try
        if (params.IndexOfName('buyer_account') < 0) then
        begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Need buyer_account param';
          exit;
        end;
        c_account := params.AsCardinal('buyer_account', 0);
        if (c_account < 0) or (c_account >= FNode.BlockManager.AccountsCount) then
        begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Invalid account ' + params.AsString('buyer_account', '');
          exit;
        end;
        buyer_account := FNode.TransactionStorage.AccountTransaction.Account(c_account);
        if not SignBuyAccountEx(params, OperationsHashTree, buyer_account.accountInfo.AccountKey,
          buyer_account.NumberOfTransactions) then
          exit;
        opt := OperationsHashTree.GetTransaction(0);
        if not FNode.AddTransaction(nil, opt, errors) then
        begin
          ErrorNum := CT_RPC_ErrNum_InternalError;
          ErrorDesc := errors;
          exit;
        end
        else
          Result := true;
        opt.GetTransactionData(0, c_account, OPR);
        FillOperationResumeToJSONObject(OPR, GetResultObject);
      finally
        OperationsHashTree.Free;
      end;
    finally
      FNode.SequenceLock.Release;
    end;
  end;

  function ChangeAccountInfo(params: TPCJSONObject): Boolean;
  var
    OperationsHashTree: TTransactionHashTree;
    account_signer, account_target: TAccount;
    opt: ITransaction;
    OPR: TTransactionData;
    errors: AnsiString;
    c_account: cardinal;
  begin
    FNode.SequenceLock.Acquire; // Use lock to prevent N_Operation race-condition on concurrent invocations
    try
      Result := false;
      OperationsHashTree := TTransactionHashTree.Create;
      try
        if (params.IndexOfName('account_signer') < 0) then
        begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Need account_signer param';
          exit;
        end;
        c_account := params.AsCardinal('account_signer', 0);
        if (c_account < 0) or (c_account >= FNode.BlockManager.AccountsCount) then
        begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Invalid account_signer ' + params.AsString('account_signer', '');
          exit;
        end;
        account_signer := FNode.TransactionStorage.AccountTransaction.Account(c_account);
        if (params.IndexOfName('account_target') < 0) then
        begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Need account_target param';
          exit;
        end;
        c_account := params.AsCardinal('account_target', 0);
        if (c_account < 0) or (c_account >= FNode.BlockManager.AccountsCount) then
        begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Invalid account_target ' + params.AsString('account_target', '');
          exit;
        end;
        account_target := FNode.TransactionStorage.AccountTransaction.Account(c_account);
        if (not TAccountKey.EqualAccountKeys(account_signer.accountInfo.AccountKey,
          account_target.accountInfo.AccountKey)) then
        begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'account_signer and account_target have distinct keys. Cannot sign';
          exit;
        end;
        if not SignChangeAccountInfoEx(params, OperationsHashTree, account_signer.accountInfo.AccountKey,
          account_signer.NumberOfTransactions) then
          exit;
        opt := OperationsHashTree.GetTransaction(0);
        if not FNode.AddTransaction(nil, opt, errors) then
        begin
          ErrorNum := CT_RPC_ErrNum_InternalError;
          ErrorDesc := errors;
          exit;
        end
        else
          Result := true;
        opt.GetTransactionData(0, c_account, OPR);
        FillOperationResumeToJSONObject(OPR, GetResultObject);
      finally
        OperationsHashTree.Free;
      end;
    finally
      FNode.SequenceLock.Release;
    end;
  end;

  function FindAccounts(params: TPCJSONObject; var output: TPCJSONArray): Boolean;
  var
    accountName: TRawBytes;
    accountType: integer;
    accountNumber: integer;
    start, max: integer;
    state: TAccountState;
    Account: TAccount;
    i: cardinal;
    errors2: AnsiString;
    errors: string;
    hasKey: Boolean;
    PubKey: TAccountKey;
  begin
    // Get Parameters
    Result := false;
    accountName := LowerCase(params.AsString('name', '')); // Convert to lowercase...
    accountType := params.AsInteger('type', -1);
    start := params.AsInteger('start', 0);
    max := params.AsInteger('max', 100);
    state := TAccountState(params.AsInteger('status', Longint(as_Normal)));
    hasKey := CapturePubKey('', PubKey, errors);
    // Validate Parameters
    if accountName <> '' then
    begin
      if not FNode.BlockManager.AccountStorage.IsValidAccountName(accountName, errors2) then
      begin
        ErrorNum := CT_RPC_ErrNum_InvalidAccountName;
        ErrorDesc := errors;
        exit;
      end;
    end;

    if max <= 0 then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := '"max" param must be greater than zero';
      exit;
    end;

    // Declare return result (empty by default)
    output := jsonresponse.GetAsArray('result');

    // Search by name
    if accountName <> '' then
    begin
      accountNumber := FNode.BlockManager.AccountStorage.FindAccountByName(accountName);
      if accountNumber >= 0 then
      begin
        Account := FNode.TransactionStorage.AccountTransaction.Account(accountNumber);
        if (accountType = -1) or (integer(Account.AccountType) = accountType) then
          FillAccountObject(Account, output.GetAsObject(output.Count));
      end;
    end
    else if state = as_ForSale then
    begin
      for i := start to FNode.BlockManager.AccountsCount - 1 do
      begin
        Account := FNode.TransactionStorage.AccountTransaction.Account(i);
        if Account.accountInfo.State = as_ForSale then
        begin
          // Found a match
          FillAccountObject(Account, output.GetAsObject(output.Count));
          if output.Count >= max then
            break;
        end;
      end;
    end
    else if hasKey then
    begin
      for i := start to FNode.BlockManager.AccountsCount - 1 do
      begin
        Account := FNode.TransactionStorage.AccountTransaction.Account(i);
        if TAccountKey.EqualAccountKeys(Account.accountInfo.AccountKey, PubKey) then
        begin
          // Found a match
          FillAccountObject(Account, output.GetAsObject(output.Count));
          if output.Count >= max then
            break;
        end;
      end;

    end
    else
    begin
      // Search by type
      for i := start to FNode.BlockManager.AccountsCount - 1 do
      begin
        Account := FNode.TransactionStorage.AccountTransaction.Account(i);
        if (accountType = -1) or (integer(Account.AccountType) = accountType) then
        begin
          // Found a match
          FillAccountObject(Account, output.GetAsObject(output.Count));
          if output.Count >= max then
            break;
        end;
      end;
    end;
    Result := true;
  end;

var
  c, c2: cardinal;
  i, j, k, l: integer;
  Account: TAccount;
  senderpubkey, destpubkey: TAccountKey;
  ansistr: AnsiString;
  nsaarr: TNodeServerAddressArray;
  pcops: TBlock;
  ecpkey: TECPrivateKey;
  OPR: TTransactionData;
  r: TRawBytes;
  ocl: TOrderedList;
  jsonarr: TPCJSONArray;
  jso: TPCJSONObject;
  Handler: THandler;
  handlerResult: TRPCResult;
begin
  _ro := nil;
  _ra := nil;
  ErrorNum := 0;
  ErrorDesc := '';
  Result := false;
  TLog.NewLog(ltdebug, Classname, 'Processing RPC-JSON method ' + method);

  Handler := TRPCPluginManager.GetHandler(method);

  if Assigned(Handler) then
  begin
    handlerResult := Handler(params);
    if handlerResult.Success then
    begin
      jsonresponse.Assign(handlerResult.Response);
      Result := true;
    end
    else
    begin
      ErrorNum := handlerResult.ErrorCode;
      ErrorDesc := handlerResult.ErrorMessage;
      Result := false;
    end;
  end
  else if (method = 'addnode') then
  begin
    // Param "nodes" contains ip's and ports in format "ip1:port1;ip2:port2 ...". If port is not specified, use default
    // Returns quantity of nodes added
    TNode.DecodeIpStringToNodeServerAddressArray(params.AsString('nodes', ''), nsaarr);
    for i := low(nsaarr) to high(nsaarr) do
    begin
      TConnectionManager.Instance.AddServer(nsaarr[i]);
    end;
    jsonresponse.GetAsVariant('result').Value := length(nsaarr);
    Result := true;
  end
  else if (method = 'getblock') then
  begin
    // Param "block" contains block number (0..getblockcount-1)
    // Returns JSON object with block information
    c := params.GetAsVariant('block').AsCardinal(cMaxBlocks);
    if (c >= 0) and (c < FNode.BlockManager.BlocksCount) then
    begin
      Result := GetBlock(c, GetResultObject);
    end
    else
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidBlock;
      if (c = cMaxBlocks) then
        ErrorDesc := 'Need block param'
      else
        ErrorDesc := 'Block not found: ' + Inttostr(c);
    end;
  end
  else if (method = 'getblocks') then
  begin
    // Param "start" "end" contains blocks number (0..getblockcount-1)
    // Returns JSON Array with blocks information (limited to 1000 blocks)
    // Sorted by DESCENDING blocknumber
    i := params.AsCardinal('last', 0);
    if (i > 0) then
    begin
      if (i > 1000) then
        i := 1000;
      c2 := FNode.BlockManager.BlocksCount - 1;
      if (FNode.BlockManager.BlocksCount >= i) then
        c := (FNode.BlockManager.BlocksCount) - i
      else
        c := 0;
    end
    else
    begin
      c := params.GetAsVariant('start').AsCardinal(cMaxBlocks);
      c2 := params.GetAsVariant('end').AsCardinal(cMaxBlocks);
      i := params.AsInteger('max', 0);
      if (c < FNode.BlockManager.BlocksCount) and (i > 0) and (i <= 1000) then
      begin
        if (c + i < FNode.BlockManager.BlocksCount) then
          c2 := c + i
        else
          c2 := FNode.BlockManager.BlocksCount - 1;
      end;
    end;
    if ((c >= 0) and (c < FNode.BlockManager.BlocksCount)) and (c2 >= c) and (c2 < FNode.BlockManager.BlocksCount) then
    begin
      i := 0;
      Result := true;
      while (c <= c2) and (Result) and (i < 1000) do
      begin
        Result := GetBlock(c2, jsonresponse.GetAsArray('result').GetAsObject(i));
        dec(c2);
        inc(i);
      end;
    end
    else
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidBlock;
      if (c > c2) then
        ErrorDesc := 'Block start > block end'
      else if (c = cMaxBlocks) or (c2 = cMaxBlocks) then
        ErrorDesc := 'Need param "last" or "start" and "end"/"max"'
      else if (c2 >= FNode.BlockManager.BlocksCount) then
        ErrorDesc := 'Block higher or equal to getblockccount: ' + Inttostr(c2)
      else
        ErrorDesc := 'Block not found: ' + Inttostr(c);
    end;
  end
  else if (method = 'getblockcount') then
  begin
    // Returns a number with Node blocks count
    jsonresponse.GetAsVariant('result').Value := FNode.BlockManager.BlocksCount;
    Result := true;
  end
  else if (method = 'getblockoperation') then
  begin
    // Param "block" contains block. Null = Pending operation
    // Param "opblock" contains operation inside a block: (0..getblock.operations-1)
    // Returns a JSON object with operation values as "Operation resume format"
    c := params.GetAsVariant('block').AsCardinal(cMaxBlocks);
    if (c >= 0) and (c < FNode.BlockManager.BlocksCount) then
    begin
      pcops := TBlock.Create(nil);
      try
        if not FNode.BlockManager.LoadTransactions(pcops, c) then
        begin
          ErrorNum := CT_RPC_ErrNum_InternalError;
          ErrorDesc := 'Cannot load Block: ' + Inttostr(c);
          exit;
        end;
        i := params.GetAsVariant('opblock').AsInteger(0);
        if (i < 0) or (i >= pcops.TransactionCount) then
        begin
          ErrorNum := CT_RPC_ErrNum_InvalidOperation;
          ErrorDesc := 'Block/Operation not found: ' + Inttostr(c) + '/' + Inttostr(i) + ' BlockOperations:' +
            Inttostr(pcops.TransactionCount);
          exit;
        end;
        if pcops.Transaction[i].GetTransactionData(c, pcops.Transaction[i].SignerAccount, OPR) then
        begin
          OPR.NOpInsideBlock := i;
          OPR.time := pcops.BlockHeader.timestamp;
          OPR.balance := -1;
          FillOperationResumeToJSONObject(OPR, GetResultObject);
        end;
        Result := true;
      finally
        pcops.Free;
      end;
    end
    else
    begin
      if (c = cMaxBlocks) then
        ErrorDesc := 'Need block param'
      else
        ErrorDesc := 'Block not found: ' + Inttostr(c);
      ErrorNum := CT_RPC_ErrNum_InvalidBlock;
    end;
  end
  else if (method = 'getblockoperations') then
  begin
    // Param "block" contains block
    // Returns a JSON array with items as "Operation resume format"
    c := params.GetAsVariant('block').AsCardinal(cMaxBlocks);
    if (c >= 0) and (c < FNode.BlockManager.BlocksCount) then
    begin
      pcops := TBlock.Create(nil);
      try
        if not FNode.BlockManager.LoadTransactions(pcops, c) then
        begin
          ErrorNum := CT_RPC_ErrNum_InternalError;
          ErrorDesc := 'Cannot load Block: ' + Inttostr(c);
          exit;
        end;
        jsonarr := GetResultArray;
        k := params.AsInteger('max', 100);
        j := params.AsInteger('start', 0);
        for i := 0 to pcops.TransactionCount - 1 do
        begin
          if (i >= j) then
          begin
            if pcops.Transaction[i].GetTransactionData(c, pcops.Transaction[i].SignerAccount, OPR) then
            begin
              OPR.NOpInsideBlock := i;
              OPR.time := pcops.BlockHeader.timestamp;
              OPR.balance := -1; // Don't include!
              FillOperationResumeToJSONObject(OPR, jsonarr.GetAsObject(jsonarr.Count));
            end;
          end;
          if (k > 0) and ((i + 1) >= (j + k)) then
            break;
        end;
        Result := true;
      finally
        pcops.Free;
      end;
    end
    else
    begin
      if (c = cMaxBlocks) then
        ErrorDesc := 'Need block param'
      else
        ErrorDesc := 'Block not found: ' + Inttostr(c);
      ErrorNum := CT_RPC_ErrNum_InvalidBlock;
    end;
  end
  else if (method = 'getaccountoperations') then
  begin
    // Returns all the operations affecting an account in "Operation resume format" as an array
    // Param "account" contains account number
    // Param "depht" (optional or "deep") contains max blocks deep to search (Default: 100)
    // Param "start" and "max" contains starting index and max operations respectively
    c := params.GetAsVariant('account').AsCardinal(cMaxAccountNumber);
    if ((c >= 0) and (c < FNode.BlockManager.AccountsCount)) then
    begin
      if (params.IndexOfName('depth') >= 0) then
        i := params.AsInteger('depth', 100)
      else
        i := params.AsInteger('deep', 100);
      Result := GetAccountOperations(c, GetResultArray, i, params.AsInteger('start', 0), params.AsInteger('max', 100));
    end
    else
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      if (c = cMaxAccountNumber) then
        ErrorDesc := 'Need account param'
      else
        ErrorDesc := 'Account not found: ' + Inttostr(c);
    end;
  end
  else if (method = 'getpendings') then
  begin
    // Returns all the operations pending to be included in a block in "Operation resume format" as an array
    // Create result
    GetResultArray;
    for i := FNode.TransactionStorage.TransactionCount - 1 downto 0 do
    begin
      if not FNode.TransactionStorage.Transaction[i].GetTransactionData(0, FNode.TransactionStorage.Transaction[i].SignerAccount, OPR) then
      begin
        ErrorNum := CT_RPC_ErrNum_InternalError;
        ErrorDesc := 'Error converting data';
        exit;
      end;
      OPR.NOpInsideBlock := i;
      OPR.balance := FNode.TransactionStorage.AccountTransaction.Account(FNode.TransactionStorage.Transaction[i].SignerAccount).Balance;
      FillOperationResumeToJSONObject(OPR, GetResultArray.GetAsObject(FNode.TransactionStorage.TransactionCount - 1 - i));
    end;
    Result := true;
  end
  else if (method = 'findoperation') then
  begin
    // Search for an operation based on "ophash"
    r := TBaseType.HexaToRaw(params.AsString('ophash', ''));
    if (r = '') then
    begin
      ErrorNum := CT_RPC_ErrNum_NotFound;
      ErrorDesc := 'param ophash not found or invalid value "' + params.AsString('ophash', '') + '"';
      exit;
    end;
    pcops := TBlock.Create(nil);
    try
      if not FNode.FindTransaction(pcops, r, c, i) then
      begin
        ErrorNum := CT_RPC_ErrNum_NotFound;
        ErrorDesc := 'ophash not found: "' + params.AsString('ophash', '') + '"';
        exit;
      end;
      if not pcops.Transaction[i].GetTransactionData(c, pcops.Transaction[i].SignerAccount, OPR) then
      begin
        ErrorNum := CT_RPC_ErrNum_InternalError;
        ErrorDesc := 'Error 20161026-1';
      end;
      OPR.NOpInsideBlock := i;
      OPR.time := pcops.BlockHeader.timestamp;
      OPR.balance := -1; // don't include
      FillOperationResumeToJSONObject(OPR, GetResultObject);
      Result := true;
    finally
      pcops.Free;
    end;
  end
  else if (method = 'sendto') then
  begin
    // Sends "amount" coins from "sender" to "target" with "fee"
    // If "payload" is present, it will be encoded using "payload_method"
    // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
    // Returns a JSON "Operation Resume format" object when successfull
    // Note: "ophash" will contain block "0" = "pending block"
    if not TRPCServer.Instance.WalletKeys.IsValidPassword then
    begin
      ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      ErrorDesc := 'Wallet is password protected. Unlock first';
      exit;
    end;
    Result := OpSendTo(params.AsCardinal('sender', cMaxAccountNumber), params.AsCardinal('target', cMaxAccountNumber),
      ToMicroCoins(params.AsDouble('amount', 0)), ToMicroCoins(params.AsDouble('fee', 0)),
      TBaseType.HexaToRaw(params.AsString('payload', '')), params.AsString('payload_method', 'dest'),
      params.AsString('pwd', ''));
  end
  else if (method = 'signsendto') then
  begin
    // Create a Transaction operation and adds it into a "rawoperations" (that can include
    // previous operations). This RPC method is usefull for cold storage, because doesn't
    // need to check or verify accounts status/public key, assuming that passed values
    // are ok.
    // Signs a transaction of "amount" coins from "sender" to "target" with "fee", using "sender_enc_pubkey" or "sender_b58_pubkey"
    // and "last_n_operation" of sender. Also, needs "target_enc_pubkey" or "target_b58_pubkey"
    // If "payload" is present, it will be encoded using "payload_method"
    // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
    // Returns a JSON "Operations info" containing old "rawoperations" plus new Transaction
    if not TRPCServer.Instance.WalletKeys.IsValidPassword then
    begin
      ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      ErrorDesc := 'Wallet is password protected. Unlock first';
      exit;
    end;
    if not CapturePubKey('sender_', senderpubkey, ErrorDesc) then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
      exit;
    end;
    if not CapturePubKey('target_', destpubkey, ErrorDesc) then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
      exit;
    end;
    Result := SignOpSendTo(params.AsString('rawoperations', ''), params.AsCardinal('sender', cMaxAccountNumber),
      params.AsCardinal('target', cMaxAccountNumber), senderpubkey, destpubkey, params.AsCardinal('last_n_operation', 0),
      ToMicroCoins(params.AsDouble('amount', 0)), ToMicroCoins(params.AsDouble('fee', 0)),
      TBaseType.HexaToRaw(params.AsString('payload', '')), params.AsString('payload_method', 'dest'),
      params.AsString('pwd', ''));
  end
  else if (method = 'changekey') then
  begin
    // Change key of "account" to "new_enc_pubkey" or "new_b58_pubkey" (encoded public key format) with "fee"
    // If "payload" is present, it will be encoded using "payload_method"
    // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
    // Returns a JSON "Operation Resume format" object when successfull
    // Note: "ophash" will contain block "0" = "pending block"
    if not TRPCServer.Instance.WalletKeys.IsValidPassword then
    begin
      ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      ErrorDesc := 'Wallet is password protected. Unlock first';
      exit;
    end;
    if params.IndexOfName('account') < 0 then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Need "account" param';
      exit;
    end;
    c := params.AsCardinal('account', cMaxAccountNumber);
    if params.IndexOfName('account_signer') >= 0 then
    begin
      c2 := params.AsCardinal('account_signer', cMaxAccountNumber);
    end
    else
      c2 := c;
    if not CapturePubKey('new_', Account.accountInfo.AccountKey, ErrorDesc) then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
      exit;
    end;
    Result := ChangeAccountKey(c2, c, Account.accountInfo.AccountKey, ToMicroCoins(params.AsDouble('fee', 0)),
      TBaseType.HexaToRaw(params.AsString('payload', '')), params.AsString('payload_method', 'dest'),
      params.AsString('pwd', ''));
  end
  else if (method = 'changekeys') then
  begin
    // Allows a massive change key operation
    // Change key of "accounts" to "new_enc_pubkey" or "new_b58_pubkey" (encoded public key format) with "fee"
    // If "payload" is present, it will be encoded using "payload_method"
    // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
    // Returns a JSON object with result information
    if not TRPCServer.Instance.WalletKeys.IsValidPassword then
    begin
      ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      ErrorDesc := 'Wallet is password protected. Unlock first';
      exit;
    end;
    if params.IndexOfName('accounts') < 0 then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Need "accounts" param';
      exit;
    end;
    if not CapturePubKey('new_', Account.accountInfo.AccountKey, ErrorDesc) then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
      exit;
    end;
    Result := ChangeAccountsKey(params.AsString('accounts', ''), Account.accountInfo.AccountKey,
      ToMicroCoins(params.AsDouble('fee', 0)), TBaseType.HexaToRaw(params.AsString('payload', '')),
      params.AsString('payload_method', 'dest'), params.AsString('pwd', ''));
  end
  else if (method = 'signchangekey') then
  begin
    // Create a Change Key operation and adds it into a "rawoperations" (that can include
    // previous operations). This RPC method is usefull for cold storage, because doesn't
    // need to check or verify accounts status/public key, assuming that passed values
    // are ok.
    // Signs a change key of "account" to "new_enc_pubkey" or "new_b58_pubkey" (encoded public key format) with "fee"
    // needs "old_enc_pubkey" or "old_b58_pubkey" that will be used to find private key in wallet to sign
    // and "last_n_operation" of account.
    // If "payload" is present, it will be encoded using "payload_method"
    // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
    // Returns a JSON "Operations info" containing old "rawoperations" plus new Transaction
    if not TRPCServer.Instance.WalletKeys.IsValidPassword then
    begin
      ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      ErrorDesc := 'Wallet is password protected. Unlock first';
      exit;
    end;
    if params.IndexOfName('account') < 0 then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Need "account" param';
      exit;
    end;
    c := params.AsCardinal('account', cMaxAccountNumber);
    if params.IndexOfName('account_signer') >= 0 then
    begin
      c2 := params.AsCardinal('account_signer', cMaxAccountNumber);
    end
    else
      c2 := c;
    if not CapturePubKey('old_', senderpubkey, ErrorDesc) then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
      exit;
    end;
    if not CapturePubKey('new_', destpubkey, ErrorDesc) then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
      exit;
    end;
    Result := SignOpChangeKey(params.AsString('rawoperations', ''), c2, c, senderpubkey, destpubkey,
      params.AsCardinal('last_n_operation', 0), ToMicroCoins(params.AsDouble('fee', 0)),
      TBaseType.HexaToRaw(params.AsString('payload', '')), params.AsString('payload_method', 'dest'),
      params.AsString('pwd', ''));
  end
  else if (method = 'listaccountforsale') then
  begin
    // Will put a single account in "for sale" state
    Result := ListAccountForSale(params);
  end
  else if (method = 'signlistaccountforsale') then
  begin
    Result := SignListAccountForSaleColdWallet(params.AsString('rawoperations', ''), params);
  end
  else if (method = 'delistaccountforsale') then
  begin
    Result := DelistAccountForSale(params);
  end
  else if (method = 'signdelistaccountforsale') then
  begin
    Result := SignDelistAccountForSaleColdWallet(params.AsString('rawoperations', ''), params);
  end
  else if (method = 'buyaccount') then
  begin
    Result := BuyAccount(params);
  end
  else if (method = 'signbuyaccount') then
  begin
    Result := SignBuyAccountColdWallet(params.AsString('rawoperations', ''), params);
  end
  else if (method = 'changeaccountinfo') then
  begin
    Result := ChangeAccountInfo(params);
  end
  else if (method = 'signchangeaccountinfo') then
  begin
    Result := SignChangeAccountInfoColdWallet(params.AsString('rawoperations', ''), params);
    //
  end
  else if (method = 'operationsinfo') then
  begin
    Result := OperationsInfo(params.AsString('rawoperations', ''), GetResultArray);
  end
  else if (method = 'executeoperations') then
  begin
    Result := ExecuteOperations(params.AsString('rawoperations', ''));

    //
  end
  else if (method = 'nodestatus') then
  begin
    // Returns a JSON Object with Node status
    GetResultObject.GetAsVariant('ready').Value := false;
    if FNode.IsReady(ansistr) then
    begin
      GetResultObject.GetAsVariant('ready_s').Value := ansistr;
      if TConnectionManager.Instance.NetStatistics.ActiveConnections > 0 then
      begin
        GetResultObject.GetAsVariant('ready').Value := true;
        if TConnectionManager.Instance.IsDiscoveringServers then
        begin
          GetResultObject.GetAsVariant('status_s').Value := 'Discovering servers';
        end
        else if TConnectionManager.Instance.IsGettingNewBlockChainFromClient then
        begin
          GetResultObject.GetAsVariant('status_s').Value := 'Obtaining new blockchain';
        end
        else
        begin
          GetResultObject.GetAsVariant('status_s').Value := 'Running';
        end;
      end
      else
      begin
        GetResultObject.GetAsVariant('ready_s').Value := 'Alone in the world...';
      end;
    end
    else
    begin
      GetResultObject.GetAsVariant('ready_s').Value := ansistr;
    end;
    GetResultObject.GetAsVariant('port').Value := FNode.NetServer.Port;
    GetResultObject.GetAsVariant('locked').Value := not TRPCServer.Instance.WalletKeys.IsValidPassword;
    GetResultObject.GetAsVariant('timestamp').Value := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
    GetResultObject.GetAsVariant('version').Value := ClientAppVersion;
    GetResultObject.GetAsObject('netprotocol').GetAsVariant('ver').Value := cNetProtocol_Version;
    GetResultObject.GetAsObject('netprotocol').GetAsVariant('ver_a').Value := cNetProtocol_Available;
    GetResultObject.GetAsVariant('blocks').Value := FNode.BlockManager.BlocksCount;
    GetResultObject.GetAsVariant('sbh').Value :=
      TBaseType.ToHexaString(FNode.BlockManager.LastBlock.initial_safe_box_hash);
    GetResultObject.GetAsVariant('pow').Value := TBaseType.ToHexaString(FNode.BlockManager.LastBlock.proof_of_work);
    GetResultObject.GetAsObject('netstats').GetAsVariant('active').Value :=
      TConnectionManager.Instance.NetStatistics.ActiveConnections;
    GetResultObject.GetAsObject('netstats').GetAsVariant('clients').Value :=
      TConnectionManager.Instance.NetStatistics.ClientsConnections;
    GetResultObject.GetAsObject('netstats').GetAsVariant('servers').Value :=
      TConnectionManager.Instance.NetStatistics.ServersConnectionsWithResponse;
    GetResultObject.GetAsObject('netstats').GetAsVariant('servers_t').Value :=
      TConnectionManager.Instance.NetStatistics.ServersConnections;
    GetResultObject.GetAsObject('netstats').GetAsVariant('total').Value :=
      TConnectionManager.Instance.NetStatistics.TotalConnections;
    GetResultObject.GetAsObject('netstats').GetAsVariant('tclients').Value :=
      TConnectionManager.Instance.NetStatistics.TotalClientsConnections;
    GetResultObject.GetAsObject('netstats').GetAsVariant('tservers').Value :=
      TConnectionManager.Instance.NetStatistics.TotalServersConnections;
    GetResultObject.GetAsObject('netstats').GetAsVariant('breceived').Value :=
      TConnectionManager.Instance.NetStatistics.BytesReceived;
    GetResultObject.GetAsObject('netstats').GetAsVariant('bsend').Value :=
      TConnectionManager.Instance.NetStatistics.BytesSend;
    nsaarr := TConnectionManager.Instance.GetValidNodeServers(true, 20);
    for i := low(nsaarr) to high(nsaarr) do
    begin
      jso := GetResultObject.GetAsArray('nodeservers').GetAsObject(i);
      jso.GetAsVariant('ip').Value := nsaarr[i].ip;
      jso.GetAsVariant('port').Value := nsaarr[i].Port;
      jso.GetAsVariant('lastcon').Value := nsaarr[i].last_connection;
      jso.GetAsVariant('attempts').Value := nsaarr[i].total_failed_attemps_to_connect;
    end;
    Result := true;
  end
  else if (method = 'encodepubkey') then
  begin
    // Creates a encoded public key based on params
    // Param "ec_nid" can be 714=secp256k1 715=secp384r1 729=secp283k1 716=secp521r1
    // Param "x","y" are x and y ec public keys values in hexadecimal based on ec_nid
    // Returns a hexadecimal value containing encoded public key
    Account.accountInfo.AccountKey.EC_OpenSSL_NID := params.AsInteger('ec_nid', 0);
    Account.accountInfo.AccountKey.x := TBaseType.HexaToRaw(params.AsString('x', ''));
    Account.accountInfo.AccountKey.y := TBaseType.HexaToRaw(params.AsString('y', ''));
    if (Account.accountInfo.AccountKey.EC_OpenSSL_NID = 0) or (Account.accountInfo.AccountKey.x = '') or
      (Account.accountInfo.AccountKey.y = '') then
    begin
      ErrorDesc := 'Need params "ec_nid","x","y" to encodepubkey';
      ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
      exit;
    end;
    if Account.accountInfo.AccountKey.IsValidAccountKey(ansistr) then
    begin
      jsonresponse.GetAsVariant('result').Value := TBaseType.ToHexaString(Account.accountInfo.AccountKey.ToRawString);
      Result := true;
    end
    else
    begin
      ErrorDesc := ansistr;
      ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
    end;
  end
  else if (method = 'decodepubkey') then
  begin
    // Returns "ec_nid", "x" and "y" of an encoded public key (x and y in hexadecimal)
    // Must provide:
    // - Param "enc_pubkey" is an hexadecimal encoded public key (see 'encodepubkey')
    // or
    // - Param "b58_pubkey" is a Base58 encoded public key
    if not CapturePubKey('', Account.accountInfo.AccountKey, ErrorDesc) then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
      exit;
    end;
    if (Account.accountInfo.AccountKey.IsValidAccountKey(ansistr)) then
    begin
      FillPublicKeyObject(Account.accountInfo.AccountKey, GetResultObject);
      Result := true;
    end
    else
    begin
      ErrorDesc := ansistr;
      ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
    end;
  end
  else if (method = 'payloadencrypt') then
  begin
    // Encrypts a "payload" using "payload_method"
    // "payload_method" types: "none","pubkey"(must provide "enc_pubkey" or "b58_pubkey"),"aes"(must provide "pwd" param)
    // If payload is "pubkey"
    // Returns an hexa string with encrypted payload
    if (params.AsString('payload', '') = '') then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Need param "payload"';
      exit;
    end;
    OPR.newKey := CT_TWalletKey_NUL.AccountKey;
    if (params.IndexOfName('enc_pubkey') >= 0) or (params.IndexOfName('b58_pubkey') >= 0) then
    begin
      if not(CapturePubKey('', OPR.newKey, ErrorDesc)) then
      begin
        ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
        exit;
      end;
    end;
    Result := DoEncrypt(TBaseType.HexaToRaw(params.AsString('payload', '')), OPR.newKey,
      params.AsString('payload_method', ''), params.AsString('pwd', ''));
  end
  else if (method = 'payloaddecrypt') then
  begin
    // Decrypts a "payload" searching for wallet private keys and for array of strings in "pwds" param
    // Returns an JSON Object with "result" (Boolean) and
    if (params.AsString('payload', '') = '') then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Need param "payload"';
      exit;
    end;
    if not TRPCServer.Instance.WalletKeys.IsValidPassword then
    begin
      ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      ErrorDesc := 'Wallet is password protected. Unlock first';
      exit;
    end;
    Result := DoDecrypt(TBaseType.HexaToRaw(params.AsString('payload', '')), params.GetAsArray('pwds'));
  end
  else if (method = 'getconnections') then
  begin
    // Returns an array of connections objects with info about state
    GetConnections;
    Result := true;
  end
  else if (method = 'addnewkey') then
  begin
    // Creates a new private key and stores it on the wallet, returning Public key JSON object
    // Param "ec_nid" can be 714=secp256k1 715=secp384r1 729=secp283k1 716=secp521r1. (Default = CT_Default_EC_OpenSSL_NID)
    // Param "name" is name for this address
    if not TRPCServer.Instance.WalletKeys.IsValidPassword then
    begin
      ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      ErrorDesc := 'Wallet is password protected. Unlock first';
      exit;
    end;
    ecpkey := TECPrivateKey.Create;
    try
      ecpkey.GenerateRandomPrivateKey(params.AsInteger('ec_nid', cDefault_EC_OpenSSL_NID));
      TRPCServer.Instance.WalletKeys.AddPrivateKey(params.AsString('name', DateTimeToStr(now)), ecpkey);
      FillPublicKeyObject(ecpkey.PublicKey, GetResultObject);
      Result := true;
    finally
      ecpkey.Free;
    end;
  end
  else if (method = 'lock') then
  begin
    jsonresponse.GetAsVariant('result').Value := TRPCServer.Instance.WalletKeys.LockWallet;
    Result := true;
  end
  else if (method = 'unlock') then
  begin
    // Unlocks the Wallet with "pwd" password
    // Returns Boolean if wallet is unlocked
    if (params.IndexOfName('pwd') < 0) then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Need param "pwd"';
      exit;
    end;
    if not TRPCServer.Instance.WalletKeys.IsValidPassword then
    begin
      TRPCServer.Instance.WalletKeys.WalletPassword := params.AsString('pwd', '');
    end;
    jsonresponse.GetAsVariant('result').Value := TRPCServer.Instance.WalletKeys.IsValidPassword;
    Result := true;
  end
  else if (method = 'setwalletpassword') then
  begin
    // Changes the Wallet password with "pwd" param
    // Must be unlocked first
    // Returns Boolean if wallet password changed
    if not TRPCServer.Instance.WalletKeys.IsValidPassword then
    begin
      ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      ErrorDesc := 'Wallet is password protected. Unlock first';
      exit;
    end;
    //
    if (params.IndexOfName('pwd') < 0) then
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Need param "pwd"';
      exit;
    end;
    TRPCServer.Instance.WalletKeys.WalletPassword := params.AsString('pwd', '');
    jsonresponse.GetAsVariant('result').Value := TRPCServer.Instance.WalletKeys.IsValidPassword;
    Result := true;
  end
  else if (method = 'stopnode') then
  begin
    // Stops communications to other nodes
    FNode.NetServer.Active := false;
    TConnectionManager.Instance.NetConnectionsActive := false;
    jsonresponse.GetAsVariant('result').Value := true;
    Result := true;
  end
  else if (method = 'startnode') then
  begin
    // Stops communications to other nodes
    FNode.NetServer.Active := true;
    TConnectionManager.Instance.NetConnectionsActive := true;
    jsonresponse.GetAsVariant('result').Value := true;
    Result := true;
  end
  else
  begin
    ErrorNum := CT_RPC_ErrNum_MethodNotFound;
    ErrorDesc := 'Method not found: "' + method + '"';
  end;
end;

end.
