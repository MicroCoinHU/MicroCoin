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
| File:       MicroCoin.Transaction.ChangeAccountInfo.pas                      |
| Created at: 2018-09-04                                                       |
| Purpose:    Transaction Plugin for Change Account Information                |
|==============================================================================}

unit MicroCoin.Transaction.ChangeAccountInfo;


{$ifdef FPC}
  {$mode delphi}
{$endif}

interface

uses ucrypto, MicroCoin.Transaction.Transaction, SysUtils,
  MicroCoin.Account.Data, MicroCoin.Account.Storage, MicroCoin.BlockChain.BlockHeader, MicroCoin.Account.Transaction,
  MicroCoin.Common, Classes, MicroCoin.Account.AccountKey, MicroCoin.Transaction.Base, UConst, ULog,
  MicroCoin.Transaction.Manager;

type

  TOpChangeAccountInfoType = (public_key, account_name, account_type);
  TOpChangeAccountInfoTypes = set of TOpChangeAccountInfoType;

  TChangeAccountInfoTransaction = class(TTransaction)
  protected type
    TChangeAccountInfoTransactionData = record
      account_signer, account_target: Cardinal;
      n_operation: Cardinal;
      fee: UInt64;
      payload: TRawBytes;
      public_key: TECDSA_Public;
      changes_type: TOpChangeAccountInfoTypes;
      // bits mask. $0001 = New account key , $0002 = New name , $0004 = New type
      new_accountkey: TAccountKey;
      // If (changes_mask and $0001)=$0001 then change account key
      new_name: TRawBytes; // If (changes_mask and $0002)=$0002 then change name
      new_type: Word; // If (changes_mask and $0004)=$0004 then change type
      sign: TECDSA_SIG;
    end;
  private
    FData: TChangeAccountInfoTransactionData;
  protected
    procedure InitializeData; override;
    function SaveToStream(Stream: TStream; SaveExtendedData: Boolean): Boolean; override;
    function LoadFromStream(Stream: TStream; LoadExtendedData: Boolean): Boolean; override;
    function GetAmount: Int64; override;
    function GetFee: UInt64; override;
    function GetPayload: TRawBytes; override;
    function GetSignerAccount: Cardinal; override;
    function GetDestinationAccount: Int64; override;
    function GetNumberOfTransactions: Cardinal; override;
    function GetTransactionType: Byte; override;
  public
    class function GetHashForSignature(const ATransactionData: TChangeAccountInfoTransactionData): TRawBytes;
    class function DoSignTransaction(key: TECPrivateKey; var ATransactionData: TChangeAccountInfoTransactionData): Boolean;

    function GetBuffer(UseProtocolV2: Boolean): TRawBytes; override;
    function ApplyTransaction(AccountTransaction: TAccountTransaction; var errors: AnsiString): Boolean; override;
    procedure AffectedAccounts(list: TList); override;
    function GetTransactionData(Block: Cardinal; Affected_account_number: Cardinal; var TransactionData: TTransactionData): Boolean; override;
    function ToString: string; override;

    constructor CreateChangeAccountInfo(account_signer, n_operation, account_target: Cardinal; key: TECPrivateKey;
      change_key: Boolean; const new_account_key: TAccountKey; change_name: Boolean; const new_name: TRawBytes;
      change_type: Boolean; const new_type: Word; fee: UInt64; payload: TRawBytes);


    property Data: TChangeAccountInfoTransactionData read FData;
  end;

implementation

const
  CT_TOpChangeAccountInfoData_NUL: TChangeAccountInfoTransaction.TChangeAccountInfoTransactionData = (account_signer: 0; account_target: 0; n_operation: 0;
    fee: 0; payload: ''; public_key: (EC_OpenSSL_NID: 0; x: ''; y: ''); changes_type: [];
    new_accountkey: (EC_OpenSSL_NID: 0; x: ''; y: ''); new_name: ''; new_type: 0; sign: (r: ''; s: ''));


procedure TChangeAccountInfoTransaction.InitializeData;
begin
  inherited InitializeData;
  FData := CT_TOpChangeAccountInfoData_NUL;
end;

function TChangeAccountInfoTransaction.SaveToStream(Stream: TStream; SaveExtendedData: Boolean): Boolean;
var
  b: Byte;
begin
  Stream.Write(FData.account_signer, Sizeof(FData.account_signer));
  Stream.Write(FData.account_target, Sizeof(FData.account_target));
  Stream.Write(FData.n_operation, Sizeof(FData.n_operation));
  Stream.Write(FData.fee, Sizeof(FData.fee));
  TStreamOp.WriteAnsiString(Stream, FData.payload);
  TStreamOp.WriteAccountKey(Stream, FData.public_key);
  b := 0;
  if (public_key in FData.changes_type) then
    b := b or $01;
  if (account_name in FData.changes_type) then
    b := b or $02;
  if (account_type in FData.changes_type) then
    b := b or $04;
  Stream.Write(b, Sizeof(b));
  TStreamOp.WriteAccountKey(Stream, FData.new_accountkey);
  TStreamOp.WriteAnsiString(Stream, FData.new_name);
  Stream.Write(FData.new_type, Sizeof(FData.new_type));
  TStreamOp.WriteAnsiString(Stream, FData.sign.r);
  TStreamOp.WriteAnsiString(Stream, FData.sign.s);
  Result := true;
end;

function TChangeAccountInfoTransaction.LoadFromStream(Stream: TStream; LoadExtendedData: Boolean): Boolean;
var
  b: Byte;
begin
  Result := False;
  if Stream.Size - Stream.Position < 20 then
    exit;
  Stream.Read(FData.account_signer, Sizeof(FData.account_signer));
  Stream.Read(FData.account_target, Sizeof(FData.account_target));
  Stream.Read(FData.n_operation, Sizeof(FData.n_operation));
  Stream.Read(FData.fee, Sizeof(FData.fee));
  if TStreamOp.ReadAnsiString(Stream, FData.payload) < 0 then
    exit;
  if TStreamOp.ReadAccountKey(Stream, FData.public_key) < 0 then
    exit;
  Stream.Read(b, Sizeof(b));
  FData.changes_type := [];
  if (b and $01) = $01 then
    FData.changes_type := FData.changes_type + [public_key];
  if (b and $02) = $02 then
    FData.changes_type := FData.changes_type + [account_name];
  if (b and $04) = $04 then
    FData.changes_type := FData.changes_type + [account_type];
  // Check
  if (b and $F8) <> 0 then
    exit;
  if TStreamOp.ReadAccountKey(Stream, FData.new_accountkey) < 0 then
    exit;
  if TStreamOp.ReadAnsiString(Stream, FData.new_name) < 0 then
    exit;
  Stream.Read(FData.new_type, Sizeof(FData.new_type));
  if TStreamOp.ReadAnsiString(Stream, FData.sign.r) < 0 then
    exit;
  if TStreamOp.ReadAnsiString(Stream, FData.sign.s) < 0 then
    exit;
  Result := true;
end;

class function TChangeAccountInfoTransaction.GetHashForSignature(const ATransactionData: TChangeAccountInfoTransactionData): TRawBytes;
var
  Stream: TMemoryStream;
  b: Byte;
begin
  Stream := TMemoryStream.Create;
  try
    Stream.Write(ATransactionData.account_signer, Sizeof(ATransactionData.account_signer));
    Stream.Write(ATransactionData.account_target, Sizeof(ATransactionData.account_target));
    Stream.Write(ATransactionData.n_operation, Sizeof(ATransactionData.n_operation));
    Stream.Write(ATransactionData.fee, Sizeof(ATransactionData.fee));
    TStreamOp.WriteAnsiString(Stream, ATransactionData.payload);
    TStreamOp.WriteAccountKey(Stream, ATransactionData.public_key);
    b := 0;
    if (public_key in ATransactionData.changes_type) then
      b := b or $01;
    if (account_name in ATransactionData.changes_type) then
      b := b or $02;
    if (account_type in ATransactionData.changes_type) then
      b := b or $04;
    Stream.Write(b, Sizeof(b));
    TStreamOp.WriteAccountKey(Stream, ATransactionData.new_accountkey);
    TStreamOp.WriteAnsiString(Stream, ATransactionData.new_name);
    Stream.Write(ATransactionData.new_type, Sizeof(ATransactionData.new_type));
    Stream.Position := 0;
    setlength(Result, Stream.Size);
    Stream.ReadBuffer(Result[1], Stream.Size);
  finally
    Stream.Free;
  end;
end;

class function TChangeAccountInfoTransaction.DoSignTransaction(key: TECPrivateKey; var ATransactionData: TChangeAccountInfoTransactionData): Boolean;
var
  raw: TRawBytes;
  _sign: TECDSA_SIG;
begin
  if not Assigned(key.PrivateKey) then
  begin
    Result := False;
    ATransactionData.sign.r := '';
    ATransactionData.sign.s := '';
    exit;
  end;
  raw := GetHashForSignature(ATransactionData);
  try
    _sign := TCrypto.ECDSASign(key, raw);
    ATransactionData.sign := _sign;
    Result := true;
  except
    ATransactionData.sign.r := '';
    ATransactionData.sign.s := '';
    Result := False;
  end;
  setlength(raw, 0);
end;

function TChangeAccountInfoTransaction.GetTransactionType: Byte;
begin
  Result := CT_Op_ChangeAccountInfo;
end;

function TChangeAccountInfoTransaction.GetBuffer(UseProtocolV2: Boolean): TRawBytes;
begin
  Result := inherited GetBuffer(true);
end;

function TChangeAccountInfoTransaction.ApplyTransaction(AccountTransaction: TAccountTransaction; var errors: AnsiString)
  : Boolean;
var
  account_signer, account_target: TAccount;
begin
  Result := False;
  if (FData.account_signer >= AccountTransaction.FreezedAccountStorage.AccountsCount) then
  begin
    errors := 'Invalid account number';
    exit;
  end;
  if TAccount.IsAccountBlockedByProtocol(FData.account_signer, AccountTransaction.FreezedAccountStorage.BlocksCount)
  then
  begin
    errors := 'account is blocked for protocol';
    exit;
  end;
  if (FData.account_signer <> FData.account_target) then
  begin
    if (FData.account_target >= AccountTransaction.FreezedAccountStorage.AccountsCount) then
    begin
      errors := 'Invalid account target number';
      exit;
    end;
    if TAccount.IsAccountBlockedByProtocol(FData.account_target, AccountTransaction.FreezedAccountStorage.BlocksCount)
    then
    begin
      errors := 'Target account is blocked for protocol';
      exit;
    end;
  end;
  if (FData.fee < 0) or (FData.fee > CT_MaxTransactionFee) then
  begin
    errors := 'Invalid fee: ' + Inttostr(FData.fee);
    exit;
  end;
  account_signer := AccountTransaction.Account(FData.account_signer);
  account_target := AccountTransaction.Account(FData.account_target);
  if ((account_signer.numberOfTransactions + 1) <> FData.n_operation) then
  begin
    errors := 'Invalid n_operation';
    exit;
  end;
  if (account_signer.balance < FData.fee) then
  begin
    errors := 'Insuficient founds';
    exit;
  end;
  if (length(FData.payload) > CT_MaxPayloadSize) then
  begin
    errors := 'Invalid Payload size:' + Inttostr(length(FData.payload)) + ' (Max: ' + Inttostr(CT_MaxPayloadSize) + ')';
    if (AccountTransaction.FreezedAccountStorage.CurrentProtocol >= CT_PROTOCOL_2) then
    begin
      exit; // BUG from protocol 1
    end;
  end;
  // Is locked? Protocol 2 check
  if (account_signer.accountInfo.IsLocked(AccountTransaction.FreezedAccountStorage.BlocksCount)) then
  begin
    errors := 'Account signer is currently locked';
    exit;
  end;
  if (AccountTransaction.FreezedAccountStorage.CurrentProtocol < CT_PROTOCOL_2) then
  begin
    errors := 'NOT ALLOWED ON PROTOCOL 1';
    exit;
  end;
  if (public_key in FData.changes_type) then
  begin
    if not FData.new_accountkey.IsValidAccountKey(errors) then
    begin
      exit;
    end;
  end;
  if (account_name in FData.changes_type) then
  begin
    if (FData.new_name <> '') then
    begin
      if not TAccountStorage.AccountNameIsValid(FData.new_name, errors) then
        exit;
    end;
  end
  else
  begin
    if (FData.new_name <> '') then
    begin
      errors := 'Invalid data in new_name field';
      exit;
    end;
  end;
  if (FData.changes_type = []) then
  begin
    errors := 'No change';
    exit;
  end;
  if (FData.public_key.EC_OpenSSL_NID <> CT_TECDSA_Public_Nul.EC_OpenSSL_NID) and
    (not TAccountKey.EqualAccountKeys(FData.public_key, account_signer.accountInfo.AccountKey)) then
  begin
    errors := Format('Invalid public key for account %d. Distinct from SafeBox public key! %s <> %s',
      [FData.account_signer, TCrypto.ToHexaString(FData.public_key.ToRawString),
      TCrypto.ToHexaString(account_signer.accountInfo.AccountKey.ToRawString)]);
    exit;
  end;
  if (FData.account_signer <> FData.account_target) then
  begin
    if (account_target.accountInfo.IsLocked(AccountTransaction.FreezedAccountStorage.BlocksCount)) then
    begin
      errors := 'Account target is currently locked';
      exit;
    end;
    // Check have same public key
    if not TAccountKey.EqualAccountKeys(account_signer.accountInfo.AccountKey, account_target.accountInfo.AccountKey)
    then
    begin
      errors := 'Signer and target accounts have different public key';
      exit;
    end;
  end;

  if not TCrypto.ECDSAVerify(account_signer.accountInfo.AccountKey, GetHashForSignature(FData), FData.sign) then
  begin
    errors := 'Invalid sign';
    FHasValidSignature := False;
    exit;
  end
  else
    FHasValidSignature := true;
  FPrevious_Signer_updated_block := account_signer.updated_block;
  FPrevious_Destination_updated_block := account_target.updated_block;
  if (public_key in FData.changes_type) then
  begin
    account_target.accountInfo.AccountKey := FData.new_accountkey;
  end;
  if (account_name in FData.changes_type) then
  begin
    account_target.name := FData.new_name;
  end;
  if (account_type in FData.changes_type) then
  begin
    account_target.account_type := FData.new_type;
  end;
  Result := AccountTransaction.UpdateAccountInfo(FData.account_signer, FData.n_operation, FData.account_target,
    account_target.accountInfo, account_target.name, account_target.account_type, FData.fee, errors);
end;

function TChangeAccountInfoTransaction.GetAmount: Int64;
begin
  Result := 0;
end;

function TChangeAccountInfoTransaction.GetFee: UInt64;
begin
  Result := FData.fee;
end;

function TChangeAccountInfoTransaction.GetPayload: TRawBytes;
begin
  Result := FData.payload;
end;

function TChangeAccountInfoTransaction.GetSignerAccount: Cardinal;
begin
  Result := FData.account_signer;
end;

function TChangeAccountInfoTransaction.GetTransactionData(Block, Affected_account_number: Cardinal;
  var TransactionData: TTransactionData): Boolean;

var
  s: string;
begin
  TransactionData := TTransactionData.Empty;
  TransactionData.DestAccount := GetDestinationAccount;
  s := '';
  if (public_key in Data.changes_type) then
  begin
    s := 'key';
  end;
  if (account_name in Data.changes_type) then
  begin
    if s <> '' then
      s := s + ',';
    s := s + 'name';
  end;
  if (account_type in Data.changes_type) then
  begin
    if s <> '' then
      s := s + ',';
    s := s + 'type';
  end;
  TransactionData.OperationTxt := 'Changed ' + s + ' of account ' + TAccount.AccountNumberToAccountTxtNumber
    (GetDestinationAccount);
  TransactionData.transactionSubtype := CT_OpSubtype_ChangeAccountInfo;
  Result := true;
  TransactionData.OriginalPayload := GetPayload;
  if TCrypto.IsHumanReadable(TransactionData.OriginalPayload) then
    TransactionData.PrintablePayload := TransactionData.OriginalPayload
  else
    TransactionData.PrintablePayload := TCrypto.ToHexaString(TransactionData.OriginalPayload);
  TransactionData.OperationHash := TransactionHash(Block);
  if (Block < CT_Protocol_Upgrade_v2_MinBlock) then
  begin
    TransactionData.OperationHash_OLD := TransactionHash_OLD(Block);
  end;
  TransactionData.valid := true;
end;

function TChangeAccountInfoTransaction.GetDestinationAccount: Int64;
begin
  Result := FData.account_target;
end;

function TChangeAccountInfoTransaction.GetNumberOfTransactions: Cardinal;
begin
  Result := FData.n_operation;
end;

procedure TChangeAccountInfoTransaction.AffectedAccounts(list: TList);
begin
  list.Add(TObject(FData.account_signer));
  if (FData.account_target <> FData.account_signer) then
    list.Add(TObject(FData.account_target));
end;

constructor TChangeAccountInfoTransaction.CreateChangeAccountInfo(account_signer, n_operation, account_target: Cardinal;
  key: TECPrivateKey; change_key: Boolean; const new_account_key: TAccountKey; change_name: Boolean;
  const new_name: TRawBytes; change_type: Boolean; const new_type: Word; fee: UInt64; payload: TRawBytes);
begin
  inherited Create;
  FData.account_signer := account_signer;
  FData.account_target := account_target;
  FData.n_operation := n_operation;
  FData.fee := fee;
  FData.payload := payload;
  // V2: No need to store public key because it's at safebox. Saving at least 64 bytes!
  // FData.public_key:=key.PublicKey;
  FData.changes_type := [];
  if change_key then
  begin
    FData.changes_type := FData.changes_type + [public_key];
    FData.new_accountkey := new_account_key;
  end;
  if change_name then
  begin
    FData.changes_type := FData.changes_type + [account_name];
    FData.new_name := new_name;
  end;
  if change_type then
  begin
    FData.changes_type := FData.changes_type + [account_type];
    FData.new_type := new_type;
  end;
  if not DoSignTransaction(key, FData) then
  begin
    TLog.NewLog(lterror, Classname, 'Error signing a new Change Info operation');
    FHasValidSignature := False;
  end
  else
    FHasValidSignature := true;
end;

function TChangeAccountInfoTransaction.toString: string;
var
  s: string;
begin
  s := '';
  if (public_key in FData.changes_type) then
    s := 'new public key ' + TAccountKey.GetECInfoTxt(FData.new_accountkey.EC_OpenSSL_NID);
  if (account_name in FData.changes_type) then
  begin
    if s <> '' then
      s := s + ', ';
    s := s + 'new name to "' + FData.new_name + '"';
  end;
  if (account_type in FData.changes_type) then
  begin
    if s <> '' then
      s := s + ', ';
    s := s + 'new type to ' + Inttostr(FData.new_type);
  end;
  Result := Format('Change account %s info: %s fee:%s (n_op:%d) payload size:%d',
    [TAccount.AccountNumberToAccountTxtNumber(FData.account_target), s, TCurrencyUtils.CurrencyToString(FData.fee),
    FData.n_operation, length(FData.payload)]);
end;

initialization

TTransactionManager.RegisterTransactionPlugin(TChangeAccountInfoTransaction, CT_Op_ChangeAccountInfo);

end.
