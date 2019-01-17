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

uses ucrypto, MicroCoin.Transaction.Transaction, SysUtils, MicroCoin.Crypto.Keys,
  MicroCoin.Account.Data, MicroCoin.Account.Storage, MicroCoin.BlockChain.BlockHeader, MicroCoin.Account.Transaction,
  MicroCoin.Common, Classes, MicroCoin.Account.AccountKey, MicroCoin.Transaction.Base, MicroCoin.Common.Config, ULog,
  MicroCoin.Transaction.Manager, UBaseTypes;

type

  TChangeAccountInfoType = (public_key, account_name, account_type);
  TChangeAccountInfoTypes = set of TChangeAccountInfoType;

  TChangeAccountInfoTransaction = class(TTransaction)
  protected type
    TChangeAccountInfoTransactionData = record
      SignerAccount, TargetAccount: Cardinal;
      NumberOfTransactions: Cardinal;
      Fee: UInt64;
      Payload: TRawBytes;
      PublicKey: TECPublicKey;
      ChangeType: TChangeAccountInfoTypes;
      // bits mask. $0001 = New account key , $0002 = New name , $0004 = New type
      NewAccountKey: TAccountKey;
      // If (changes_mask and $0001)=$0001 then change account key
      NewName: TRawBytes; // If (changes_mask and $0002)=$0002 then change name
      NewType: Word; // If (changes_mask and $0004)=$0004 then change type
      Signature: TECDSA_SIG;
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
    class function DoSignTransaction(key: TECKeyPair; var ATransactionData: TChangeAccountInfoTransactionData): Boolean;

    function GetBuffer(UseProtocolV2: Boolean): TRawBytes; override;
    function ApplyTransaction(AccountTransaction: TAccountTransaction; var errors: AnsiString): Boolean; override;
    procedure AffectedAccounts(list: TList); override;
    function GetTransactionData(Block: Cardinal; Affected_account_number: Cardinal; var TransactionData: TTransactionData): Boolean; override;
    function ToString: string; override;

    constructor CreateChangeAccountInfo(account_signer, n_operation, account_target: Cardinal; key: TECKeyPair;
      change_key: Boolean; const new_account_key: TAccountKey; change_name: Boolean; const new_name: TRawBytes;
      change_type: Boolean; const new_type: Word; fee: UInt64; payload: TRawBytes);


    property Data: TChangeAccountInfoTransactionData read FData;
  end;

implementation

uses MicroCoin.Common.Stream;

const
  CT_TOpChangeAccountInfoData_NUL: TChangeAccountInfoTransaction.TChangeAccountInfoTransactionData = (SignerAccount: 0; TargetAccount: 0; NumberOfTransactions: 0;
    Fee: 0; Payload: ''; PublicKey: (EC_OpenSSL_NID: 0; x: ''; y: ''); ChangeType: [];
    NewAccountKey: (EC_OpenSSL_NID: 0; x: ''; y: ''); NewName: ''; NewType: 0; Signature: (r: ''; s: ''));


procedure TChangeAccountInfoTransaction.InitializeData;
begin
  inherited InitializeData;
  FData := CT_TOpChangeAccountInfoData_NUL;
end;

function TChangeAccountInfoTransaction.SaveToStream(Stream: TStream; SaveExtendedData: Boolean): Boolean;
var
  b: Byte;
begin
  Stream.Write(FData.SignerAccount, Sizeof(FData.SignerAccount));
  Stream.Write(FData.TargetAccount, Sizeof(FData.TargetAccount));
  Stream.Write(FData.NumberOfTransactions, Sizeof(FData.NumberOfTransactions));
  Stream.Write(FData.Fee, Sizeof(FData.Fee));
  Stream.WriteAnsiString(FData.Payload);
  Stream.WriteAccountKey(FData.PublicKey);
  b := 0;
  if (public_key in FData.ChangeType) then
    b := b or $01;
  if (account_name in FData.ChangeType) then
    b := b or $02;
  if (account_type in FData.ChangeType) then
    b := b or $04;
  Stream.Write(b, Sizeof(b));
  Stream.WriteAccountKey(FData.NewAccountKey);
  Stream.WriteAnsiString(FData.NewName);
  Stream.Write(FData.NewType, Sizeof(FData.NewType));
  Stream.WriteAnsiString(FData.Signature.r);
  Stream.WriteAnsiString(FData.Signature.s);
  Result := true;
end;

function TChangeAccountInfoTransaction.LoadFromStream(Stream: TStream; LoadExtendedData: Boolean): Boolean;
var
  b: Byte;
begin
  Result := False;
  if Stream.Size - Stream.Position < 20 then
    exit;
  Stream.Read(FData.SignerAccount, Sizeof(FData.SignerAccount));
  Stream.Read(FData.TargetAccount, Sizeof(FData.TargetAccount));
  Stream.Read(FData.NumberOfTransactions, Sizeof(FData.NumberOfTransactions));
  Stream.Read(FData.Fee, Sizeof(FData.Fee));
  if Stream.ReadAnsiString(FData.Payload) < 0 then
    exit;
  if Stream.ReadAccountKey(FData.PublicKey) < 0 then
    exit;
  Stream.Read(b, Sizeof(b));
  FData.ChangeType := [];
  if (b and $01) = $01 then
    FData.ChangeType := FData.ChangeType + [public_key];
  if (b and $02) = $02 then
    FData.ChangeType := FData.ChangeType + [account_name];
  if (b and $04) = $04 then
    FData.ChangeType := FData.ChangeType + [account_type];
  // Check
  if (b and $F8) <> 0 then
    exit;
  if Stream.ReadAccountKey(FData.NewAccountKey) < 0 then
    exit;
  if Stream.ReadAnsiString(FData.NewName) < 0 then
    exit;
  Stream.Read(FData.NewType, Sizeof(FData.NewType));
  if Stream.ReadAnsiString(FData.Signature.r) < 0 then
    exit;
  if Stream.ReadAnsiString(FData.Signature.s) < 0 then
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
    Stream.Write(ATransactionData.SignerAccount, Sizeof(ATransactionData.SignerAccount));
    Stream.Write(ATransactionData.TargetAccount, Sizeof(ATransactionData.TargetAccount));
    Stream.Write(ATransactionData.NumberOfTransactions, Sizeof(ATransactionData.NumberOfTransactions));
    Stream.Write(ATransactionData.Fee, Sizeof(ATransactionData.Fee));
    Stream.WriteAnsiString(ATransactionData.Payload);
    Stream.WriteAccountKey(ATransactionData.PublicKey);
    b := 0;
    if (public_key in ATransactionData.ChangeType) then
      b := b or $01;
    if (account_name in ATransactionData.ChangeType) then
      b := b or $02;
    if (account_type in ATransactionData.ChangeType) then
      b := b or $04;
    Stream.Write(b, Sizeof(b));
    Stream.WriteAccountKey(ATransactionData.NewAccountKey);
    Stream.WriteAnsiString(ATransactionData.NewName);
    Stream.Write(ATransactionData.NewType, Sizeof(ATransactionData.NewType));
    Stream.Position := 0;
    setlength(Result, Stream.Size);
    Stream.ReadBuffer(Result[1], Stream.Size);
  finally
    Stream.Free;
  end;
end;

class function TChangeAccountInfoTransaction.DoSignTransaction(key: TECKeyPair; var ATransactionData: TChangeAccountInfoTransactionData): Boolean;
var
  raw: TRawBytes;
  _sign: TECDSA_SIG;
begin
  if not Assigned(key.PrivateKey) then
  begin
    Result := False;
    ATransactionData.Signature.r := '';
    ATransactionData.Signature.s := '';
    exit;
  end;
  raw := GetHashForSignature(ATransactionData);
  try
    _sign := TCrypto.ECDSASign(key, raw);
    ATransactionData.Signature := _sign;
    Result := true;
  except
    ATransactionData.Signature.r := '';
    ATransactionData.Signature.s := '';
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
  if (FData.SignerAccount >= AccountTransaction.FreezedAccountStorage.AccountsCount) then
  begin
    errors := 'Invalid account number';
    exit;
  end;
  if TAccount.IsAccountBlockedByProtocol(FData.SignerAccount, AccountTransaction.FreezedAccountStorage.BlocksCount)
  then
  begin
    errors := 'account is blocked for protocol';
    exit;
  end;
  if (FData.SignerAccount <> FData.TargetAccount) then
  begin
    if (FData.TargetAccount >= AccountTransaction.FreezedAccountStorage.AccountsCount) then
    begin
      errors := 'Invalid account target number';
      exit;
    end;
    if TAccount.IsAccountBlockedByProtocol(FData.TargetAccount, AccountTransaction.FreezedAccountStorage.BlocksCount)
    then
    begin
      errors := 'Target account is blocked for protocol';
      exit;
    end;
  end;
  if (FData.Fee < 0) or (FData.Fee > cMaxTransactionFee) then
  begin
    errors := 'Invalid fee: ' + Inttostr(FData.Fee);
    exit;
  end;
  account_signer := AccountTransaction.Account(FData.SignerAccount);
  account_target := AccountTransaction.Account(FData.TargetAccount);
  if ((account_signer.NumberOfTransactions + 1) <> FData.NumberOfTransactions) then
  begin
    errors := 'Invalid n_operation';
    exit;
  end;
  if (account_signer.Balance < FData.Fee) then
  begin
    errors := 'Insuficient founds';
    exit;
  end;
  if (length(FData.Payload) > cMaxPayloadSize) then
  begin
    errors := 'Invalid Payload size:' + Inttostr(length(FData.Payload)) + ' (Max: ' + Inttostr(cMaxPayloadSize) + ')';
    if (AccountTransaction.FreezedAccountStorage.CurrentProtocol >= cPROTOCOL_2) then
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
  if (AccountTransaction.FreezedAccountStorage.CurrentProtocol < cPROTOCOL_2) then
  begin
    errors := 'NOT ALLOWED ON PROTOCOL 1';
    exit;
  end;
  if (public_key in FData.ChangeType) then
  begin
    if not FData.NewAccountKey.IsValidAccountKey(errors) then
    begin
      exit;
    end;
  end;
  if (account_name in FData.ChangeType) then
  begin
    if (FData.NewName <> '') then
    begin
      if not TAccountStorage.IsValidAccountName(FData.NewName, errors) then
        exit;
    end;
  end
  else
  begin
    if (FData.NewName <> '') then
    begin
      errors := 'Invalid data in new_name field';
      exit;
    end;
  end;
  if (FData.ChangeType = []) then
  begin
    errors := 'No change';
    exit;
  end;
  if (FData.PublicKey.EC_OpenSSL_NID <> TAccountKey.Empty.EC_OpenSSL_NID) and
    (not TAccountKey.EqualAccountKeys(FData.PublicKey, account_signer.accountInfo.AccountKey)) then
  begin
    errors := Format('Invalid public key for account %d. Distinct from SafeBox public key! %s <> %s',
      [FData.SignerAccount, TBaseType.ToHexaString(FData.PublicKey.ToRawString),
      TBaseType.ToHexaString(account_signer.accountInfo.AccountKey.ToRawString)]);
    exit;
  end;
  if (FData.SignerAccount <> FData.TargetAccount) then
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

  if not TCrypto.ECDSAVerify(account_signer.accountInfo.AccountKey, GetHashForSignature(FData), FData.Signature) then
  begin
    errors := 'Invalid sign';
    FHasValidSignature := False;
    exit;
  end
  else
    FHasValidSignature := true;
  FPrevious_Signer_updated_block := account_signer.UpdatedBlock;
  FPrevious_Destination_updated_block := account_target.UpdatedBlock;
  if (public_key in FData.ChangeType) then
  begin
    account_target.accountInfo.AccountKey := FData.NewAccountKey;
  end;
  if (account_name in FData.ChangeType) then
  begin
    account_target.Name := FData.NewName;
  end;
  if (account_type in FData.ChangeType) then
  begin
    account_target.AccountType := FData.NewType;
  end;
  Result := AccountTransaction.UpdateAccountInfo(FData.SignerAccount, FData.NumberOfTransactions, FData.TargetAccount,
    account_target.accountInfo, account_target.Name, account_target.AccountType, FData.Fee, errors);
end;

function TChangeAccountInfoTransaction.GetAmount: Int64;
begin
  Result := 0;
end;

function TChangeAccountInfoTransaction.GetFee: UInt64;
begin
  Result := FData.Fee;
end;

function TChangeAccountInfoTransaction.GetPayload: TRawBytes;
begin
  Result := FData.Payload;
end;

function TChangeAccountInfoTransaction.GetSignerAccount: Cardinal;
begin
  Result := FData.SignerAccount;
end;

function TChangeAccountInfoTransaction.GetTransactionData(Block, Affected_account_number: Cardinal;
  var TransactionData: TTransactionData): Boolean;

var
  s: string;
begin
  TransactionData := TTransactionData.Empty;
  TransactionData.DestAccount := GetDestinationAccount;
  s := '';
  if (public_key in Data.ChangeType) then
  begin
    s := 'key';
  end;
  if (account_name in Data.ChangeType) then
  begin
    if s <> '' then
      s := s + ',';
    s := s + 'name';
  end;
  if (account_type in Data.ChangeType) then
  begin
    if s <> '' then
      s := s + ',';
    s := s + 'type';
  end;
  TransactionData.TransactionAsString := 'Changed ' + s + ' of account ' + TAccount.AccountNumberToString
    (GetDestinationAccount);
  TransactionData.transactionSubtype := CT_OpSubtype_ChangeAccountInfo;
  Result := true;
  TransactionData.OriginalPayload := GetPayload;
  if TCrypto.IsHumanReadable(TransactionData.OriginalPayload) then
    TransactionData.PrintablePayload := TransactionData.OriginalPayload
  else
    TransactionData.PrintablePayload := TBaseType.ToHexaString(TransactionData.OriginalPayload);
  TransactionData.OperationHash := TransactionHash(Block);
  if (Block < cProtocol_Upgrade_v2_MinBlock) then
  begin
    TransactionData.OperationHash_OLD := TransactionHash_OLD(Block);
  end;
  TransactionData.valid := true;
end;

function TChangeAccountInfoTransaction.GetDestinationAccount: Int64;
begin
  Result := FData.TargetAccount;
end;

function TChangeAccountInfoTransaction.GetNumberOfTransactions: Cardinal;
begin
  Result := FData.NumberOfTransactions;
end;

procedure TChangeAccountInfoTransaction.AffectedAccounts(list: TList);
begin
  list.Add(TObject(FData.SignerAccount));
  if (FData.TargetAccount <> FData.SignerAccount) then
    list.Add(TObject(FData.TargetAccount));
end;

constructor TChangeAccountInfoTransaction.CreateChangeAccountInfo(account_signer, n_operation, account_target: Cardinal;
  key: TECKeyPair; change_key: Boolean; const new_account_key: TAccountKey; change_name: Boolean;
  const new_name: TRawBytes; change_type: Boolean; const new_type: Word; fee: UInt64; payload: TRawBytes);
begin
  inherited Create;
  FData.SignerAccount := account_signer;
  FData.TargetAccount := account_target;
  FData.NumberOfTransactions := n_operation;
  FData.Fee := fee;
  FData.Payload := payload;
  // V2: No need to store public key because it's at safebox. Saving at least 64 bytes!
  // FData.public_key:=key.PublicKey;
  FData.ChangeType := [];
  if change_key then
  begin
    FData.ChangeType := FData.ChangeType + [public_key];
    FData.NewAccountKey := new_account_key;
  end;
  if change_name then
  begin
    FData.ChangeType := FData.ChangeType + [account_name];
    FData.NewName := new_name;
  end;
  if change_type then
  begin
    FData.ChangeType := FData.ChangeType + [account_type];
    FData.NewType := new_type;
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
  if (public_key in FData.ChangeType) then
    s := 'new public key ' + TAccountKey.GetECInfoTxt(FData.NewAccountKey.EC_OpenSSL_NID);
  if (account_name in FData.ChangeType) then
  begin
    if s <> '' then
      s := s + ', ';
    s := s + 'new name to "' + FData.NewName + '"';
  end;
  if (account_type in FData.ChangeType) then
  begin
    if s <> '' then
      s := s + ', ';
    s := s + 'new type to ' + Inttostr(FData.NewType);
  end;
  Result := Format('Change account %s info: %s fee:%s (n_op:%d) payload size:%d',
    [TAccount.AccountNumberToString(FData.TargetAccount), s, TCurrencyUtils.CurrencyToString(FData.Fee),
    FData.NumberOfTransactions, length(FData.Payload)]);
end;

initialization

TTransactionManager.RegisterTransactionPlugin(TChangeAccountInfoTransaction, CT_Op_ChangeAccountInfo);

end.
