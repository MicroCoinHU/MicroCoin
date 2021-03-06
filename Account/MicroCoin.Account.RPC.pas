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
| File:       MicroCoin.Account.RPC.pas                                        |
| Created at: 2018-08-27                                                       |
| Purpose:    RPC plugin for manage accounts                                   |
|==============================================================================}

unit MicroCoin.Account.RPC;


{$ifdef FPC}
  {$mode delphi}
{$endif}

interface

uses Sysutils, Math, Classes, MicroCoin.RPC.Handler, MicroCoin.Account.Data, MicroCoin.Common.Config, MicroCoin.Common.Lists,
  MicroCoin.RPC.Server, MicroCoin.Node.Node, MicroCoin.Account.AccountKey, MicroCoin.RPC.PluginManager, UJsonFunctions,
  UBaseTypes, UCrypto, MicroCoin.Common, MicroCoin.RPC.Plugin, MicroCoin.RPC.Result, MicroCoin.Crypto.Keys;

type

  TRPCAccountPlugin = class(TRPCPlugin)
  strict protected
    procedure FillAccountObject(const AAccount: TAccount; AJsonObject: TPCJSONObject);
    function ExtractPubKey(AParams: TPCJSONObject; const APrefix: string; var APubKey: TAccountKey; var RErrorText: string): Boolean;
    procedure FillPublicKeyObject(const APubKey: TAccountKey; AJsonObject: TPCJSONObject);
    procedure RegisterHandlers; override;
  public
    {$ifdef USE_RTTI}[RPCMethod('getaccount')]{$endif}
    function GetAccount(AParams: TPCJSONObject): TRPCResult;
    {$ifdef USE_RTTI}[RPCMethod('findaccounts')]{$endif}
    function FindAccounts(AParams: TPCJSONObject): TRPCResult;
    {$ifdef USE_RTTI}[RPCMethod('getwalletaccounts')]{$endif}
    function GetWalletAccounts(AParams: TPCJSONObject): TRPCResult;
    {$ifdef USE_RTTI}[RPCMethod('getwalletaccountscount')]{$endif}
    function GetWalletAccountsCount(AParams: TPCJSONObject): TRPCResult;
    {$ifdef USE_RTTI}[RPCMethod('getwalletcoins')]{$endif}
    function GetWalletCoins(AParams: TPCJSONObject): TRPCResult;
    {$ifdef USE_RTTI}[RPCMethod('getwalletpubkeys')]{$endif}
    function GetWalletPubKeys(AParams: TPCJSONObject): TRPCResult;
    {$ifdef USE_RTTI}[RPCMethod('getwalletpubkey')]{$endif}
    function GetWalletPubKey(AParams: TPCJSONObject): TRPCResult;
  end;

implementation

function TRPCAccountPlugin.ExtractPubKey(AParams: TPCJSONObject; const APrefix: string; var APubKey: TAccountKey;
  var RErrorText: string): Boolean;
var
  xErrors: AnsiString;
  xPublicKey: TAccountKey;
begin
  APubKey := TAccount.Empty.accountInfo.AccountKey;
  RErrorText := '';
  Result := false;
  if (AParams.IndexOfName(APrefix + 'b58_pubkey') >= 0) then
  begin
    if not TAccountKey.AccountPublicKeyImport(AParams.AsString(APrefix + 'b58_pubkey', ''), APubKey, xErrors) then
    begin
      RErrorText := 'Invalid value of param "' + APrefix + 'b58_pubkey": ' + xErrors;
      exit;
    end;
    if (AParams.IndexOfName(APrefix + 'enc_pubkey') >= 0) then
    begin
      xPublicKey := TAccountKey.FromRawString(TBaseType.HexaToRaw(AParams.AsString(APrefix + 'enc_pubkey', '')));
      if (xPublicKey <> APubKey) then
      begin
        RErrorText := 'Params "' + APrefix + 'b58_pubkey" and "' + APrefix +
          'enc_pubkey" public keys are not the same public key';
        exit;
      end;
    end;
  end
  else
  begin
    if (AParams.IndexOfName(APrefix + 'enc_pubkey') < 0) then
    begin
      RErrorText := 'Need param "' + APrefix + 'enc_pubkey" or "' + APrefix + 'b58_pubkey"';
      exit;
    end;
    APubKey := TAccountKey.FromRawString(TBaseType.HexaToRaw(AParams.AsString(APrefix + 'enc_pubkey', '')));
  end;
  if not APubKey.IsValidAccountKey(xErrors) then
  begin
    RErrorText := 'Invalid public key: ' + xErrors;
  end
  else
    Result := true;
end;

procedure TRPCAccountPlugin.FillPublicKeyObject(const APubKey: TAccountKey; AJsonObject: TPCJSONObject);
begin
  AJsonObject.GetAsVariant('ec_nid').Value := APubKey.EC_OpenSSL_NID;
  AJsonObject.GetAsVariant('x').Value := TBaseType.ToHexaString(APubKey.x);
  AJsonObject.GetAsVariant('y').Value := TBaseType.ToHexaString(APubKey.y);
  AJsonObject.GetAsVariant('enc_pubkey').Value := TBaseType.ToHexaString(APubKey.ToRawString);
  AJsonObject.GetAsVariant('b58_pubkey').Value := (APubKey.AccountPublicKeyExport);
end;

procedure TRPCAccountPlugin.FillAccountObject(const AAccount: TAccount; AJsonObject: TPCJSONObject);
begin
  AJsonObject.GetAsVariant('account').Value := AAccount.AccountNumber;
  AJsonObject.GetAsVariant('enc_pubkey').Value := TBaseType.ToHexaString(AAccount.accountInfo.AccountKey.ToRawString);
  AJsonObject.GetAsVariant('balance').Value := TCurrencyUtils.ToJsonValue(AAccount.Balance);
  AJsonObject.GetAsVariant('n_operation').Value := AAccount.NumberOfTransactions;
  AJsonObject.GetAsVariant('updated_b').Value := AAccount.UpdatedBlock;
  case AAccount.accountInfo.State of
    as_Normal:
      AJsonObject.GetAsVariant('state').Value := 'normal';
    as_ForSale:
      begin
        AJsonObject.GetAsVariant('state').Value := 'listed';
        AJsonObject.GetAsVariant('locked_until_block').Value := AAccount.accountInfo.LockedUntilBlock;
        AJsonObject.GetAsVariant('price').Value := AAccount.accountInfo.Price;
        AJsonObject.GetAsVariant('seller_account').Value := AAccount.accountInfo.AccountToPay;
        AJsonObject.GetAsVariant('private_sale').Value := (AAccount.accountInfo.NewPublicKey.EC_OpenSSL_NID <> 0);
        if not(AAccount.accountInfo.NewPublicKey.EC_OpenSSL_NID <> 0) then
        begin
          AJsonObject.GetAsVariant('new_enc_pubkey').Value :=
            TBaseType.ToHexaString(AAccount.accountInfo.NewPublicKey.ToRawString);
        end;
      end
  else
    raise Exception.Create('ERROR DEV 20170425-1');
  end;
  AJsonObject.GetAsVariant('name').Value := AAccount.Name;
  AJsonObject.GetAsVariant('type').Value := AAccount.AccountType;
end;

function TRPCAccountPlugin.FindAccounts(AParams: TPCJSONObject): TRPCResult;
var
  xAccountName: TRawBytes;
  xAccountType: integer;
  xAccountNumber: integer;
  xStart, xMax: integer;
  xAccountState: TAccountState;
  xAccount: TAccount;
  i: cardinal;
  xErrors2: AnsiString;
  xErrors: string;
  xHasPublicKey: Boolean;
  xPubKey: TAccountKey;
  xOutput: TPCJsonArray;
begin
  Result := RPCResult;
  Result.Success := false;
  Result.ErrorCode := CT_RPC_ErrNum_InternalError;
  xAccountName := LowerCase(AParams.AsString('name', '')); // Convert to lowercase...
  xAccountType := AParams.AsInteger('type', -1);
  xStart := AParams.AsInteger('start', 0);
  xMax := AParams.AsInteger('max', 100);
  xAccountState := TAccountState(AParams.AsInteger('status', Longint(as_Normal)));
  xHasPublicKey := ExtractPubKey(AParams, '', xPubKey, xErrors);
  // Validate Parameters
  if xAccountName <> '' then
  begin
    if not TNode.Node.BlockManager.AccountStorage.IsValidAccountName(xAccountName, xErrors2) then
    begin
      Result.ErrorCode := CT_RPC_ErrNum_InvalidAccountName;
      Result.ErrorMessage := xErrors;
      Result.Success := false;
      exit;
    end;
  end;

  if xMax <= 0 then
  begin
    Result.ErrorCode := CT_RPC_ErrNum_InvalidData;
    Result.ErrorMessage := '"max" param must be greater than zero';
    Result.Success := false;
    exit;
  end;
  // Declare return result (empty by default)
  xOutput := Result.Response.GetAsArray('result');

  // Search by name
  if xAccountName <> '' then
  begin
    xAccountNumber := TNode.Node.BlockManager.AccountStorage.FindAccountByName(xAccountName);
    if xAccountNumber >= 0 then
    begin
      xAccount := TNode.Node.TransactionStorage.AccountTransaction.Account(xAccountNumber);
      if (xAccountType = -1) or (integer(xAccount.AccountType) = xAccountType) then
        FillAccountObject(xAccount, xOutput.GetAsObject(xOutput.Count));
    end;
  end
  else if xAccountState = as_ForSale then
  begin
    for i := xStart to TNode.Node.BlockManager.AccountsCount - 1 do
    begin
      xAccount := TNode.Node.TransactionStorage.AccountTransaction.Account(i);
      if xAccount.accountInfo.State = as_ForSale then
      begin
        // Found a match
        FillAccountObject(xAccount, xOutput.GetAsObject(xOutput.Count));
        if xOutput.Count >= xMax then
          break;
      end;
    end;
  end
  else if xHasPublicKey then
  begin
    for i := xStart to TNode.Node.BlockManager.AccountsCount - 1 do
    begin
      xAccount := TNode.Node.TransactionStorage.AccountTransaction.Account(i);
      if xAccount.accountInfo.AccountKey = xPubKey then
      begin
        // Found a match
        FillAccountObject(xAccount, xOutput.GetAsObject(xOutput.Count));
        if xOutput.Count >= xMax then
          break;
      end;
    end;

  end
  else
  begin
    // Search by type
    for i := xStart to TNode.Node.BlockManager.AccountsCount - 1 do
    begin
      xAccount := TNode.Node.TransactionStorage.AccountTransaction.Account(i);
      if (xAccountType = -1) or (integer(xAccount.AccountType) = xAccountType) then
      begin
        // Found a match
        FillAccountObject(xAccount, xOutput.GetAsObject(xOutput.Count));
        if xOutput.Count >= xMax then
          break;
      end;
    end;
  end;
  Result.Success := true;
end;

function TRPCAccountPlugin.GetAccount(AParams: TPCJSONObject): TRPCResult;
var
  xAccountNumber: cardinal;
  xAccount: TAccount;
begin
  Result := RPCResult;
  Result.Success := false;
  Result.ErrorCode := CT_RPC_ErrNum_InternalError;
  Result.ErrorMessage := '';
  xAccountNumber := AParams.GetAsVariant('account').AsCardinal(cMaxAccountNumber);
  if (xAccountNumber < TNode.Node.BlockManager.AccountsCount) then
  begin
    xAccount := TNode.Node.TransactionStorage.AccountTransaction.Account(xAccountNumber);
    FillAccountObject(xAccount, TPCJSONObject(Result.Response).GetAsObject('result'));
    Result.Success := true;
  end
  else
  begin
    Result.ErrorCode := CT_RPC_ErrNum_InvalidAccount;
    if (xAccountNumber = cMaxAccountNumber) then
      Result.ErrorMessage := 'Need "account" param'
    else
      Result.ErrorMessage := 'Account not found: ' + Inttostr(xAccountNumber);
  end;
end;

function TRPCAccountPlugin.GetWalletAccounts(AParams: TPCJSONObject): TRPCResult;
var
  jsonArr: TPCJsonArray;
  i, j, k, l, c: integer;
  ocl: TOrderedList;
  xAccount: TAccount;
  xKey: TAccountKey;
begin
  Result := RPCResult;
  Result.Success := false;
  Result.ErrorCode := CT_RPC_ErrNum_InternalError;
  jsonArr := Result.Response.GetAsArray('result');
  if (AParams.IndexOfName('enc_pubkey') >= 0) or (AParams.IndexOfName('b58_pubkey') >= 0) then
  begin
    if not(ExtractPubKey(AParams, '', xKey, Result.ErrorMessage)) then
    begin
      Result.ErrorCode := CT_RPC_ErrNum_InvalidPubKey;
      Result.Success := false;
      Result.ErrorMessage := 'Invalid public key';
      exit;
    end;
    i := TRPCServer.Instance.WalletKeys.AccountsKeyList.IndexOfAccountKey(xKey);
    if (i < 0) then
    begin
      Result.ErrorCode := CT_RPC_ErrNum_NotFound;
      Result.ErrorMessage := 'Public key not found in wallet';
      exit;
    end;
    ocl := TRPCServer.Instance.WalletKeys.AccountsKeyList.AccountList[i];
    k := AParams.AsInteger('max', 100);
    l := AParams.AsInteger('start', 0);
    for j := 0 to ocl.Count - 1 do
    begin
      if (j >= l) then
      begin
        xAccount := TNode.Node.TransactionStorage.AccountTransaction.Account(ocl.Get(j));
        FillAccountObject(xAccount, jsonArr.GetAsObject(jsonArr.Count));
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
      ocl := TRPCServer.Instance.WalletKeys.AccountsKeyList.AccountList[i];
      for j := 0 to ocl.Count - 1 do
      begin
        if (c >= l) then
        begin
          xAccount := TNode.Node.TransactionStorage.AccountTransaction.Account(ocl.Get(j));
          FillAccountObject(xAccount, jsonArr.GetAsObject(jsonArr.Count));
        end;
        inc(c);
        if (k > 0) and (c >= (k + l)) then
          break;
      end;
      if (k > 0) and (c >= (k + l)) then
        break;
    end;
    Result.Success := true;
  end;
end;

function TRPCAccountPlugin.GetWalletAccountsCount(AParams: TPCJSONObject): TRPCResult;
var
  ocl: TOrderedList;
  i, c: integer;
  xAccountKey: TAccountKey;
begin
  Result := RPCResult;
  Result.Success := false;
  Result.ErrorCode := CT_RPC_ErrNum_InternalError;
  if (AParams.IndexOfName('enc_pubkey') >= 0) or (AParams.IndexOfName('b58_pubkey') >= 0) then
  begin
    if not(ExtractPubKey(AParams, '', xAccountKey, Result.ErrorMessage)) then
    begin
      Result.ErrorCode := CT_RPC_ErrNum_InvalidPubKey;
      exit;
    end;
    i := TRPCServer.Instance.WalletKeys.AccountsKeyList.IndexOfAccountKey(xAccountKey);
    if (i < 0) then
    begin
      Result.ErrorCode := CT_RPC_ErrNum_NotFound;
      Result.ErrorMessage := 'Public key not found in wallet';
      exit;
    end;
    ocl := TRPCServer.Instance.WalletKeys.AccountsKeyList.AccountList[i];
    Result.Response.GetAsVariant('result').Value := ocl.Count;
    Result.Success := true;
  end
  else
  begin
    Result.ErrorMessage := '';
    c := 0;
    for i := 0 to TRPCServer.Instance.WalletKeys.AccountsKeyList.Count - 1 do
    begin
      ocl := TRPCServer.Instance.WalletKeys.AccountsKeyList.AccountList[i];
      inc(c, ocl.Count);
    end;
    Result.Response.GetAsVariant('result').Value := c;
    Result.Success := true;
  end;
end;

function TRPCAccountPlugin.GetWalletCoins(AParams: TPCJSONObject): TRPCResult;
var
  xNewKey: TAccountKey;
  i, j: integer;
  xOrderedList: TOrderedList;
  xAccount: TAccount;
begin
  Result := RPCResult;
  Result.Success := false;
  Result.ErrorCode := CT_RPC_ErrNum_InternalError;
  if (AParams.IndexOfName('enc_pubkey') >= 0) or (AParams.IndexOfName('b58_pubkey') >= 0) then
  begin
    if not(ExtractPubKey(AParams, '', xNewKey, Result.ErrorMessage)) then
    begin
      Result.ErrorCode := CT_RPC_ErrNum_InvalidPubKey;
      exit;
    end;
    i := TRPCServer.Instance.WalletKeys.AccountsKeyList.IndexOfAccountKey(xNewKey);
    if (i < 0) then
    begin
      Result.ErrorCode := CT_RPC_ErrNum_NotFound;
      Result.ErrorMessage := 'Public key not found in wallet';
      exit;
    end;
    xOrderedList := TRPCServer.Instance.WalletKeys.AccountsKeyList.AccountList[i];
    xAccount.Balance := 0;
    for j := 0 to xOrderedList.Count - 1 do
    begin
      inc(xAccount.Balance, TNode.Node.TransactionStorage.AccountTransaction.Account(xOrderedList.Get(j)).Balance);
    end;
    Result.Response.GetAsVariant('result').Value := TCurrencyUtils.ToJsonValue(xAccount.Balance);
    Result.Success := true;
  end
  else
  begin
    Result.ErrorMessage := '';
    xAccount.Balance := 0;
    for i := 0 to TRPCServer.Instance.WalletKeys.AccountsKeyList.Count - 1 do
    begin
      xOrderedList := TRPCServer.Instance.WalletKeys.AccountsKeyList.AccountList[i];
      for j := 0 to xOrderedList.Count - 1 do
      begin
        inc(xAccount.Balance, TNode.Node.TransactionStorage.AccountTransaction.Account(xOrderedList.Get(j)).Balance);
      end;
    end;
    Result.Response.GetAsVariant('result').Value := TCurrencyUtils.ToJsonValue(xAccount.Balance);
    Result.Success := true;
  end;
end;

function TRPCAccountPlugin.GetWalletPubKey(AParams: TPCJSONObject): TRPCResult;
var
  xNewKey: TAccountKey;
  i: integer;
begin
  Result := RPCResult;
  Result.Success := false;
  if not(ExtractPubKey(AParams, '', xNewKey, Result.ErrorMessage)) then
  begin
    Result.ErrorCode := CT_RPC_ErrNum_InvalidPubKey;
    exit;
  end;
  i := TRPCServer.Instance.WalletKeys.AccountsKeyList.IndexOfAccountKey(xNewKey);
  if (i < 0) then
  begin
    Result.ErrorCode := CT_RPC_ErrNum_NotFound;
    Result.ErrorMessage := 'Public key not found in wallet';
    exit;
  end;
  FillPublicKeyObject(TRPCServer.Instance.WalletKeys.AccountsKeyList.AccountKey[i],
    Result.Response.GetAsObject('result'));
  Result.Success := true;
end;

function TRPCAccountPlugin.GetWalletPubKeys(AParams: TPCJSONObject): TRPCResult;
var
  k, j, i: integer;
  jsonArr: TPCJsonArray;
  jso: TPCJSONObject;
begin
  Result := RPCResult;
  Result.Success := false;
  Result.ErrorCode := CT_RPC_ErrNum_InternalError;
  k := AParams.AsInteger('max', 100);
  j := AParams.AsInteger('start', 0);
  jsonArr := Result.Response.GetAsArray('result');
  for i := 0 to TRPCServer.Instance.WalletKeys.Count - 1 do
  begin
    if (i >= j) then
    begin
      jso := jsonArr.GetAsObject(jsonArr.Count);
      jso.GetAsVariant('name').Value := TRPCServer.Instance.WalletKeys.key[i].name;
      jso.GetAsVariant('can_use').Value := (TRPCServer.Instance.WalletKeys.key[i].CryptedKey <> '');
      FillPublicKeyObject(TRPCServer.Instance.WalletKeys.key[i].AccountKey, jso);
    end;
    if (k > 0) and ((i + 1) >= (j + k)) then
      break;
  end;
  Result.Success := true;
end;

procedure TRPCAccountPlugin.RegisterHandlers;
begin
  TRPCPluginManager.RegisterHandler('GetAccount',   self.GetAccount);
  TRPCPluginManager.RegisterHandler('FindAccounts', self.FindAccounts);
  TRPCPluginManager.RegisterHandler('GetWalletAccounts', self.GetWalletAccounts);
  TRPCPluginManager.RegisterHandler('GetWalletAccountsCount', self.GetWalletAccountsCount);
  TRPCPluginManager.RegisterHandler('GetWalletCoins', self.GetWalletCoins);
  TRPCPluginManager.RegisterHandler('GetWalletPubKey', self.GetWalletPubKey);
  TRPCPluginManager.RegisterHandler('GetWalletPubKeys', self.GetWalletPubKeys);
end;

initialization

TRPCPluginManager.RegisterPlugin(TRPCAccountPlugin.Create);

end.
