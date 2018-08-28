unit MicroCoin.Account.RPC;

{
  This unit contains code from PascalCoin:

  Copyright (c) Albert Molina 2016 - 2018 original code from PascalCoin https://pascalcoin.org/

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

}

interface

uses Sysutils, classes, MicroCoin.RPC.Handler, MicroCOin.Account.Data,
     Uconst, MicroCoin.Common.Lists, MicroCoin.RPC.Server,
     MicroCoin.Node.Node,
     MicroCoin.Account.AccountKey, MicroCoin.RPC.PluginManager, UJsonFunctions, UCrypto;

type

  TRPCAccountPlugin = class(TRPCPlugin)
  strict protected
    procedure FillAccountObject(const Account: TAccount; jsonobj: TPCJSONObject);
    function CapturePubKey(AParams : TPCJSONObject; const prefix: string; var PubKey: TAccountKey; var errortxt: string): Boolean;
    procedure FillPublicKeyObject(const PubKey: TAccountKey; jsonobj: TPCJSONObject);
  public
    [RPCMethod('getaccount')]
    function GetAccount(AParams: TPCJSONObject): TRPCResult;
    [RPCMethod('findaccounts')]
    function FindAccounts(AParams : TPCJSONObject) : TRPCResult;
    [RPCMethod('getwalletaccounts')]
    function GetWalletAccounts(AParams : TPCJSONObject) : TRPCResult;
    [RPCMethod('getwalletaccountscount')]
    function GetWalletAccountsCount(AParams : TPCJSONObject) : TRPCResult;
    [RPCMethod('getwalletcoins')]
    function GetWalletCoins(AParams : TPCJSONObject): TRPCResult;
    [RPCMethod('getwalletpubkeys')]
    function GetWalletPubKeys(AParam : TPCJSONObject) : TRPCResult;
    [RPCMethod('getwalletpubkey')]
    function GetWalletPubKey(AParam : TPCJSONObject) : TRPCResult;
  end;

implementation

function TRPCAccountPlugin.CapturePubKey(AParams : TPCJSONObject; const prefix: string; var PubKey: TAccountKey; var errortxt: string): Boolean;
var
  ansistr: AnsiString;
  auxpubkey: TAccountKey;
begin
  PubKey := CT_Account_NUL.accountInfo.AccountKey;
  errortxt := '';
  Result := false;
  if (AParams.IndexOfName(prefix + 'b58_pubkey') >= 0) then
  begin
    if not TAccountKey.AccountPublicKeyImport(AParams.AsString(prefix + 'b58_pubkey', ''), PubKey, ansistr) then
    begin
      errortxt := 'Invalid value of param "' + prefix + 'b58_pubkey": ' + ansistr;
      exit;
    end;
    if (AParams.IndexOfName(prefix + 'enc_pubkey') >= 0) then
    begin
      auxpubkey := TAccountKey.FromRawString(TCrypto.HexaToRaw(AParams.AsString(prefix + 'enc_pubkey', '')));
      if (not TAccountKey.EqualAccountKeys(auxpubkey, PubKey)) then
      begin
        errortxt := 'Params "' + prefix + 'b58_pubkey" and "' + prefix + 'enc_pubkey" public keys are not the same public key';
        exit;
      end;
    end;
  end
  else
  begin
    if (AParams.IndexOfName(prefix + 'enc_pubkey') < 0) then
    begin
      errortxt := 'Need param "' + prefix + 'enc_pubkey" or "' + prefix + 'b58_pubkey"';
      exit;
    end;
    PubKey := TAccountKey.FromRawString(TCrypto.HexaToRaw(AParams.AsString(prefix + 'enc_pubkey', '')));
  end;
  if not PubKey.IsValidAccountKey(ansistr) then
  begin
    errortxt := 'Invalid public key: ' + ansistr;
  end
  else
    Result := true;
end;

procedure TRPCAccountPlugin.FillPublicKeyObject(const PubKey: TAccountKey; jsonobj: TPCJSONObject);
begin
  jsonobj.GetAsVariant('ec_nid').Value := PubKey.EC_OpenSSL_NID;
  jsonobj.GetAsVariant('x').Value := TCrypto.ToHexaString(PubKey.x);
  jsonobj.GetAsVariant('y').Value := TCrypto.ToHexaString(PubKey.y);
  jsonobj.GetAsVariant('enc_pubkey').Value := TCrypto.ToHexaString(PubKey.ToRawString);
  jsonobj.GetAsVariant('b58_pubkey').Value := (PubKey.AccountPublicKeyExport);
end;

procedure TRPCAccountPlugin.FillAccountObject(const Account: TAccount; jsonobj: TPCJSONObject);
begin
  jsonobj.GetAsVariant('account').Value := Account.Account;
  jsonobj.GetAsVariant('enc_pubkey').Value := TCrypto.ToHexaString(Account.accountInfo.AccountKey.ToRawString);
  jsonobj.GetAsVariant('balance').Value := ToJSONCurrency(Account.balance);
  jsonobj.GetAsVariant('n_operation').Value := Account.n_operation;
  jsonobj.GetAsVariant('updated_b').Value := Account.updated_block;
  case Account.accountInfo.state of
    as_Normal:
      jsonobj.GetAsVariant('state').Value := 'normal';
    as_ForSale:
      begin
        jsonobj.GetAsVariant('state').Value := 'listed';
        jsonobj.GetAsVariant('locked_until_block').Value := Account.accountInfo.locked_until_block;
        jsonobj.GetAsVariant('price').Value := Account.accountInfo.price;
        jsonobj.GetAsVariant('seller_account').Value := Account.accountInfo.account_to_pay;
        jsonobj.GetAsVariant('private_sale').Value := (Account.accountInfo.new_publicKey.EC_OpenSSL_NID <> 0);
        if not(Account.accountInfo.new_publicKey.EC_OpenSSL_NID <> 0) then
        begin
          jsonobj.GetAsVariant('new_enc_pubkey').Value := TCrypto.ToHexaString(Account.accountInfo.new_publicKey.ToRawString);
        end;
      end
  else
    raise Exception.Create('ERROR DEV 20170425-1');
  end;
  jsonobj.GetAsVariant('name').Value := Account.name;
  jsonobj.GetAsVariant('type').Value := Account.account_type;
end;



function TRPCAccountPlugin.FindAccounts(AParams: TPCJSONObject): TRPCResult;
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
  output: TPCJsonArray;
begin
  // Get Parameters
  Result.Success := false;
  Result.ErrorCode := CT_RPC_ErrNum_InternalError;
  accountName := LowerCase(AParams.AsString('name', '')); // Convert to lowercase...
  accountType := AParams.AsInteger('type', -1);
  start := AParams.AsInteger('start', 0);
  max := AParams.AsInteger('max', 100);
  state := TAccountState(AParams.AsInteger('status', Longint(as_Normal)));
  hasKey := CapturePubKey(AParams, '', PubKey, errors);
  // Validate Parameters
  if accountName <> '' then
  begin
    if not TNode.Node.Bank.AccountStorage.AccountNameIsValid(accountName, errors2) then
    begin
      Result.ErrorCode := CT_RPC_ErrNum_InvalidAccountName;
      Result.ErrorMessage := errors;
      Result.Success := false;
      exit;
    end;
  end;

  if max <= 0 then
  begin
    Result.ErrorCode := CT_RPC_ErrNum_InvalidData;
    Result.ErrorMessage := '"max" param must be greater than zero';
    Result.Success := false;
    exit;
  end;
  // Declare return result (empty by default)
  output := Result.Response.GetAsArray('result');

  // Search by name
  if accountName <> '' then
  begin
    accountNumber := TNode.Node.Bank.AccountStorage.FindAccountByName(accountName);
    if accountNumber >= 0 then
    begin
      Account := TNode.Node.Operations.SafeBoxTransaction.Account(accountNumber);
      if (accountType = -1) or (integer(Account.account_type) = accountType) then
        FillAccountObject(Account, output.GetAsObject(output.Count));
    end;
  end
  else if state = as_ForSale then
  begin
    for i := start to TNode.Node.Bank.AccountsCount - 1 do
    begin
      Account := TNode.Node.Operations.SafeBoxTransaction.Account(i);
      if Account.accountInfo.state = as_ForSale then
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
    for i := start to TNode.Node.Bank.AccountsCount - 1 do
    begin
      Account := TNode.Node.Operations.SafeBoxTransaction.Account(i);
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
    for i := start to TNode.Node.Bank.AccountsCount - 1 do
    begin
      Account := TNode.Node.Operations.SafeBoxTransaction.Account(i);
      if (accountType = -1) or (integer(Account.account_type) = accountType) then
      begin
        // Found a match
        FillAccountObject(Account, output.GetAsObject(output.Count));
        if output.Count >= max then
          break;
      end;
    end;
  end;
  Result.Success := true;
end;

function TRPCAccountPlugin.GetAccount(AParams: TPCJSONObject): TRPCResult;
var
  c: cardinal;
  Account: TAccount;
begin
  Result.Success := false;
  Result.ErrorCode := CT_RPC_ErrNum_InternalError;
  Result.ErrorMessage := '';
  c := AParams.GetAsVariant('account').AsCardinal(CT_MaxAccount);
  if (c >= 0) and (c < TNode.Node.Bank.AccountsCount) then
  begin
    Account := TNode.Node.Operations.SafeBoxTransaction.Account(c);
    FillAccountObject(Account, TPCJsonObject(Result.Response).GetAsObject('result'));
    Result.Success := true;
  end
  else
  begin
    Result.ErrorCode := CT_RPC_ErrNum_InvalidAccount;
    if (c = CT_MaxAccount) then
      Result.ErrorMessage := 'Need "account" param'
    else
     Result.ErrorMessage := 'Account not found: ' + Inttostr(c);
  end;
end;

function TRPCAccountPlugin.GetWalletAccounts(AParams: TPCJSONObject): TRPCResult;
var
  jsonArr : TPCJSONArray;
  i,j,k,l,c : integer;
  ocl : TOrderedList;
  Account : TAccount;
  key : TAccountKey;
begin
  Result.Success := false;
  Result.ErrorCode := CT_RPC_ErrNum_InternalError;
  jsonarr := Result.Response.GetAsArray('result');
  if (AParams.IndexOfName('enc_pubkey') >= 0) or (AParams.IndexOfName('b58_pubkey') >= 0) then
  begin
    if not(CapturePubKey(AParams, '', key, Result.ErrorMessage)) then
    begin
      Result.ErrorCode := CT_RPC_ErrNum_InvalidPubKey;
      Result.Success := False;
      Result.ErrorMessage :=  'Invalid public key';
      exit;
    end;
    i := TRPCServer.Instance.WalletKeys.AccountsKeyList.IndexOfAccountKey(key);
    if (i < 0) then
    begin
      Result.ErrorCode := CT_RPC_ErrNum_NotFound;
      Result.ErrorMessage := 'Public key not found in wallet';
      exit;
    end;
    ocl := TRPCServer.Instance.WalletKeys.AccountsKeyList.AccountKeyList[i];
    k := AParams.AsInteger('max', 100);
    l := AParams.AsInteger('start', 0);
    for j := 0 to ocl.Count - 1 do
    begin
      if (j >= l) then
      begin
        Account := TNode.Node.Operations.SafeBoxTransaction.Account(ocl.Get(j));
        FillAccountObject(Account, jsonarr.GetAsObject(jsonarr.Count));
      end;
      if (k > 0) and ((j + 1) >= (k + l)) then
        break;
    end;
    Result.Success := true;
  end
  else
  begin
    k := AParams.AsInteger('max', 100);
    l := AParams.AsInteger('start', 0);
    c := 0;
    for i := 0 to TRPCServer.Instance.WalletKeys.AccountsKeyList.Count - 1 do
    begin
      ocl := TRPCServer.Instance.WalletKeys.AccountsKeyList.AccountKeyList[i];
      for j := 0 to ocl.Count - 1 do
      begin
        if (c >= l) then
        begin
          Account := TNode.Node.Operations.SafeBoxTransaction.Account(ocl.Get(j));
          FillAccountObject(Account, jsonarr.GetAsObject(jsonarr.Count));
        end;
        inc(c);
        if (k > 0) and (c >= (k + l)) then
          break;
      end;
      if (k > 0) and (c >= (k + l)) then
        break;
    end;
    Result.Success := True;
  end;
end;

function TRPCAccountPlugin.GetWalletAccountsCount( AParams: TPCJSONObject): TRPCResult;
var
  ocl : TOrderedList;
  i,c : integer;
  Key : TAccountKey;
begin
  Result.Success := False;
  Result.ErrorCode := CT_RPC_ErrNum_InternalError;
  if (AParams.IndexOfName('enc_pubkey') >= 0) or (AParams.IndexOfName('b58_pubkey') >= 0) then
  begin
    if not(CapturePubKey(AParams, '', Key, Result.ErrorMessage)) then
    begin
      Result.ErrorCode := CT_RPC_ErrNum_InvalidPubKey;
      exit;
    end;
    i := TRPCServer.Instance.WalletKeys.AccountsKeyList.IndexOfAccountKey(Key);
    if (i < 0) then
    begin
      Result.ErrorCode := CT_RPC_ErrNum_NotFound;
      Result.ErrorMessage := 'Public key not found in wallet';
      exit;
    end;
    ocl := TRPCServer.Instance.WalletKeys.AccountsKeyList.AccountKeyList[i];
    Result.Response.GetAsVariant('result').Value := ocl.Count;
    Result.Success := true;
  end
  else
  begin
    Result.ErrorMessage := '';
    c := 0;
    for i := 0 to TRPCServer.Instance.WalletKeys.AccountsKeyList.Count - 1 do
    begin
      ocl := TRPCServer.Instance.WalletKeys.AccountsKeyList.AccountKeyList[i];
      inc(c, ocl.Count);
    end;
    Result.Response.GetAsVariant('result').Value := c;
    Result.Success := true;
  end;
end;

function TRPCAccountPlugin.GetWalletCoins(AParams: TPCJSONObject): TRPCResult;
var
 newKey : TAccountKey;
 i,j,c: integer;
 ocl : TOrderedList;
 Account : TAccount;
begin
  Result.Success := False;
  Result.ErrorCode := CT_RPC_ErrNum_InternalError;
  if (AParams.IndexOfName('enc_pubkey') >= 0) or (AParams.IndexOfName('b58_pubkey') >= 0) then
  begin
    if not(CapturePubKey(AParams, '', newKey, Result.ErrorMessage)) then
    begin
      Result.ErrorCode := CT_RPC_ErrNum_InvalidPubKey;
      exit;
    end;
    i := TRPCServer.Instance.WalletKeys.AccountsKeyList.IndexOfAccountKey(newKey);
    if (i < 0) then
    begin
      Result.ErrorCode := CT_RPC_ErrNum_NotFound;
      Result.ErrorMessage := 'Public key not found in wallet';
      exit;
    end;
    ocl := TRPCServer.Instance.WalletKeys.AccountsKeyList.AccountKeyList[i];
    Account.balance := 0;
    for j := 0 to ocl.Count - 1 do
    begin
      inc(Account.balance, TNode.Node.Operations.SafeBoxTransaction.Account(ocl.Get(j)).balance);
    end;
    Result.Response.GetAsVariant('result').Value := ToJSONCurrency(Account.balance);
    Result.Success := true;
  end
  else
  begin
    Result.ErrorMessage := '';
    c := 0;
    Account.balance := 0;
    for i := 0 to TRPCServer.Instance.WalletKeys.AccountsKeyList.Count - 1 do
    begin
      ocl := TRPCServer.Instance.WalletKeys.AccountsKeyList.AccountKeyList[i];
      for j := 0 to ocl.Count - 1 do
      begin
        inc(Account.balance, TNode.Node.Operations.SafeBoxTransaction.Account(ocl.Get(j)).balance);
      end;
    end;
    Result.Response.GetAsVariant('result').Value := ToJSONCurrency(Account.balance);
    Result.Success := true;
  end;
end;

function TRPCAccountPlugin.GetWalletPubKey(AParam: TPCJSONObject): TRPCResult;
var
  newKey : TAccountKey;
  i : integer;
begin
  Result.Success := false;
  if not(CapturePubKey(AParam, '', newKey, Result.ErrorMessage)) then
  begin
    Result.ErrorCode := CT_RPC_ErrNum_InvalidPubKey;
    exit;
  end;
  i := TRPCServer.Instance.WalletKeys.AccountsKeyList.IndexOfAccountKey(newKey);
  if (i < 0) then
  begin
    Result.ErrorCode := CT_RPC_ErrNum_NotFound;
    Result.ErrorMessage := 'Public key not found in wallet';
    exit;
  end;
  FillPublicKeyObject(TRPCServer.Instance.WalletKeys.AccountsKeyList.AccountKey[i], Result.Response.GetAsObject('result'));
  Result.Success := true;
end;

function TRPCAccountPlugin.GetWalletPubKeys(AParam: TPCJSONObject): TRPCResult;
var
  k, j, i : integer;
  jsonarr : TPCJSONArray;
  jso : TPCJsonObject;
begin
  Result.Success := false;
  Result.ErrorCode := CT_RPC_ErrNum_InternalError;
  k := AParam.AsInteger('max', 100);
  j := AParam.AsInteger('start', 0);
  jsonarr := Result.Response.GetAsArray('result');
  for i := 0 to TRPCServer.Instance.WalletKeys.Count - 1 do
  begin
    if (i >= j) then
    begin
      jso := jsonarr.GetAsObject(jsonarr.Count);
      jso.GetAsVariant('name').Value := TRPCServer.Instance.WalletKeys.Key[i].name;
      jso.GetAsVariant('can_use').Value := (TRPCServer.Instance.WalletKeys.Key[i].CryptedKey <> '');
      FillPublicKeyObject(TRPCServer.Instance.WalletKeys.Key[i].AccountKey, jso);
    end;
    if (k > 0) and ((i + 1) >= (j + k)) then
      break;
  end;
  Result.Success := true;
end;

initialization
 TRPCManager.RegisterPlugin(TRPCAccountPlugin.Create);

end.
