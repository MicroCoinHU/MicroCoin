unit MicroCoin.Transaction.ChangeAccountInfo;

{
  This unit contains code from PascalCoin:

  Copyright (c) Albert Molina 2016 - 2018 original code from PascalCoin https://pascalcoin.org/

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

}

interface

uses uaccounts, ucrypto, MicroCoin.Transaction.Transaction, SysUtils,
  Classes, MicroCoin.Account.AccountKey, MicroCoin.Transaction.Base, UConst, ULog, MicroCoin.Transaction.Manager;

type

  TOpChangeAccountInfoType = (public_key, account_name, account_type);
  TOpChangeAccountInfoTypes = set of TOpChangeAccountInfoType;

  TOpChangeAccountInfoData = record
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

  TOpChangeAccountInfo = class(TTransaction)
  private
    FData: TOpChangeAccountInfoData;
  protected
    procedure InitializeData; override;
    function SaveToStream(Stream: TStream; SaveExtendedData: Boolean)
      : Boolean; override;
    function LoadFromStream(Stream: TStream; LoadExtendedData: Boolean)
      : Boolean; override;
  public
    class function GetOperationHashToSign(const op: TOpChangeAccountInfoData)
      : TRawBytes;
    class function DoSignOperation(key: TECPrivateKey;
      var op: TOpChangeAccountInfoData): Boolean;
    function GetOpType: Byte; override;
    function GetBufferForOpHash(UseProtocolV2: Boolean): TRawBytes; override;
    function ApplyTransaction(AccountTransaction: TPCSafeBoxTransaction;
      var errors: AnsiString): Boolean; override;
    function GetAmount: Int64; override;
    function GetFee: UInt64; override;
    function GetPayload: TRawBytes; override;
    function GetSignerAccount: Cardinal; override;
    function GetDestinationAccount: Int64; override;
    function GetNumberOfTransactions: Cardinal; override;
    procedure AffectedAccounts(list: TList); override;
    constructor CreateChangeAccountInfo(account_signer, n_operation,
      account_target: Cardinal; key: TECPrivateKey; change_key: Boolean;
      const new_account_key: TAccountKey; change_name: Boolean;
      const new_name: TRawBytes; change_type: Boolean; const new_type: Word;
      fee: UInt64; payload: TRawBytes);
    function GetTransactionData(Block: Cardinal;
      Affected_account_number: Cardinal; var TransactionData: TTransactionData)
      : Boolean; override;
    property Data: TOpChangeAccountInfoData read FData;
    function toString: string; override;
  end;

const
  CT_TOpChangeAccountInfoData_NUL: TOpChangeAccountInfoData = (account_signer: 0; account_target: 0; n_operation: 0; fee: 0; payload: ''; public_key: (EC_OpenSSL_NID: 0; x: ''; y: '');
    changes_type: [];
    new_accountkey: (EC_OpenSSL_NID: 0; x: ''; y: ''); new_name: ''; new_type: 0;
    sign: (r: ''; s: ''));

implementation

procedure TOpChangeAccountInfo.InitializeData;
begin
  inherited InitializeData;
  FData := CT_TOpChangeAccountInfoData_NUL;
end;

function TOpChangeAccountInfo.SaveToStream(Stream: TStream; SaveExtendedData: Boolean): Boolean;
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

function TOpChangeAccountInfo.LoadFromStream(Stream: TStream; LoadExtendedData: Boolean): Boolean;
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

class function TOpChangeAccountInfo.GetOperationHashToSign(const op: TOpChangeAccountInfoData): TRawBytes;
var
  Stream: TMemoryStream;
  b: Byte;
begin
  Stream := TMemoryStream.Create;
  try
    Stream.Write(op.account_signer, Sizeof(op.account_signer));
    Stream.Write(op.account_target, Sizeof(op.account_target));
    Stream.Write(op.n_operation, Sizeof(op.n_operation));
    Stream.Write(op.fee, Sizeof(op.fee));
    TStreamOp.WriteAnsiString(Stream, op.payload);
    TStreamOp.WriteAccountKey(Stream, op.public_key);
    b := 0;
    if (public_key in op.changes_type) then
      b := b or $01;
    if (account_name in op.changes_type) then
      b := b or $02;
    if (account_type in op.changes_type) then
      b := b or $04;
    Stream.Write(b, Sizeof(b));
    TStreamOp.WriteAccountKey(Stream, op.new_accountkey);
    TStreamOp.WriteAnsiString(Stream, op.new_name);
    Stream.Write(op.new_type, Sizeof(op.new_type));
    Stream.Position := 0;
    setlength(Result, Stream.Size);
    Stream.ReadBuffer(Result[1], Stream.Size);
  finally
    Stream.Free;
  end;
end;

class function TOpChangeAccountInfo.DoSignOperation(key: TECPrivateKey; var op: TOpChangeAccountInfoData): Boolean;
var
  raw: TRawBytes;
  _sign: TECDSA_SIG;
begin
  if not Assigned(key.PrivateKey) then
  begin
    Result := False;
    op.sign.r := '';
    op.sign.s := '';
    exit;
  end;
  raw := GetOperationHashToSign(op);
  try
    _sign := TCrypto.ECDSASign(key, raw);
    op.sign := _sign;
    Result := true;
  except
    op.sign.r := '';
    op.sign.s := '';
    Result := False;
  end;
  setlength(raw, 0);
end;

function TOpChangeAccountInfo.GetOpType: Byte;
begin
  Result := CT_Op_ChangeAccountInfo;
end;

function TOpChangeAccountInfo.GetBufferForOpHash(UseProtocolV2: Boolean): TRawBytes;
begin
  Result := inherited GetBufferForOpHash(true);
end;

function TOpChangeAccountInfo.ApplyTransaction(AccountTransaction: TPCSafeBoxTransaction; var errors: AnsiString): Boolean;
var
  account_signer, account_target: TAccount;
begin
  Result := False;
  if (FData.account_signer >= AccountTransaction.FreezedSafeBox.AccountsCount) then
  begin
    errors := 'Invalid account number';
    exit;
  end;
  if TAccountComp.IsAccountBlockedByProtocol(FData.account_signer, AccountTransaction.FreezedSafeBox.BlocksCount) then
  begin
    errors := 'account is blocked for protocol';
    exit;
  end;
  if (FData.account_signer <> FData.account_target) then
  begin
    if (FData.account_target >= AccountTransaction.FreezedSafeBox.AccountsCount) then
    begin
      errors := 'Invalid account target number';
      exit;
    end;
    if TAccountComp.IsAccountBlockedByProtocol(FData.account_target, AccountTransaction.FreezedSafeBox.BlocksCount) then
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
  if ((account_signer.n_operation + 1) <> FData.n_operation) then
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
    if (AccountTransaction.FreezedSafeBox.CurrentProtocol >= CT_PROTOCOL_2) then
    begin
      exit; // BUG from protocol 1
    end;
  end;
  // Is locked? Protocol 2 check
  if (account_signer.accountInfo.IsLocked(AccountTransaction.FreezedSafeBox.BlocksCount)) then
  begin
    errors := 'Account signer is currently locked';
    exit;
  end;
  if (AccountTransaction.FreezedSafeBox.CurrentProtocol < CT_PROTOCOL_2) then
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
      if not TPCSafeBox.ValidAccountName(FData.new_name, errors) then
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
  if (FData.public_key.EC_OpenSSL_NID <> CT_TECDSA_Public_Nul.EC_OpenSSL_NID) and (not TAccountKey.EqualAccountKeys(FData.public_key, account_signer.accountInfo.AccountKey)) then
  begin
    errors := Format('Invalid public key for account %d. Distinct from SafeBox public key! %s <> %s', [
      FData.account_signer,
      TCrypto.ToHexaString(FData.public_key.ToRawString),
      TCrypto.ToHexaString(account_signer.accountInfo.AccountKey.ToRawString)]);
    exit;
  end;
  if (FData.account_signer <> FData.account_target) then
  begin
    if (account_target.accountInfo.IsLocked(AccountTransaction.FreezedSafeBox.BlocksCount)) then
    begin
      errors := 'Account target is currently locked';
      exit;
    end;
    // Check have same public key
    if not TAccountKey.EqualAccountKeys(account_signer.accountInfo.AccountKey, account_target.accountInfo.AccountKey) then
    begin
      errors := 'Signer and target accounts have different public key';
      exit;
    end;
  end;

  if not TCrypto.ECDSAVerify(account_signer.accountInfo.AccountKey, GetOperationHashToSign(FData), FData.sign) then
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
    account_target.accountInfo,
    account_target.name,
    account_target.account_type,
    FData.fee, errors);
end;

function TOpChangeAccountInfo.GetAmount: Int64;
begin
  Result := 0;
end;

function TOpChangeAccountInfo.GetFee: UInt64;
begin
  Result := FData.fee;
end;

function TOpChangeAccountInfo.GetPayload: TRawBytes;
begin
  Result := FData.payload;
end;

function TOpChangeAccountInfo.GetSignerAccount: Cardinal;
begin
  Result := FData.account_signer;
end;

function TOpChangeAccountInfo.GetTransactionData(Block,
  Affected_account_number: Cardinal;
  var TransactionData: TTransactionData): Boolean;

var
  s: string;

begin
  TransactionData.DestAccount := GetDestinationAccount;
  s := '';
  if (public_key in Data.changes_type)
  then
  begin
    s := 'key';
  end;
  if (account_name in Data.changes_type)
  then
  begin
    if s <> '' then
      s := s + ',';
    s := s + 'name';
  end;
  if (account_type in Data.changes_type)
  then
  begin
    if s <> '' then
      s := s + ',';
    s := s + 'type';
  end;
  TransactionData.OperationTxt := 'Changed ' + s + ' of account ' +
    TAccountComp.AccountNumberToAccountTxtNumber
    (GetDestinationAccount);
  TransactionData.OpSubtype := CT_OpSubtype_ChangeAccountInfo;
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

function TOpChangeAccountInfo.GetDestinationAccount: Int64;
begin
  Result := FData.account_target;
end;

function TOpChangeAccountInfo.GetNumberOfTransactions: Cardinal;
begin
  Result := FData.n_operation;
end;

procedure TOpChangeAccountInfo.AffectedAccounts(list: TList);
begin
  list.Add(TObject(FData.account_signer));
  if (FData.account_target <> FData.account_signer) then
    list.Add(TObject(FData.account_target));
end;

constructor TOpChangeAccountInfo.CreateChangeAccountInfo(account_signer, n_operation,
  account_target: Cardinal; key: TECPrivateKey; change_key: Boolean;
  const new_account_key: TAccountKey; change_name: Boolean;
  const new_name: TRawBytes; change_type: Boolean; const new_type: Word;
  fee: UInt64; payload: TRawBytes);
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
  if not DoSignOperation(key, FData) then
  begin
    TLog.NewLog(lterror, Classname, 'Error signing a new Change Info operation');
    FHasValidSignature := False;
  end
  else
    FHasValidSignature := true;
end;

function TOpChangeAccountInfo.toString: string;
var
  s: string;
begin
  s := '';
  if (public_key in FData.changes_type) then
    s := 'new public key ' + TAccountComp.GetECInfoTxt(FData.new_accountkey.EC_OpenSSL_NID);
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
  Result := Format('Change account %s info: %s fee:%s (n_op:%d) payload size:%d', [
    TAccountComp.AccountNumberToAccountTxtNumber(FData.account_target),
    s,
    TAccountComp.FormatMoney(FData.fee), FData.n_operation, length(FData.payload)]);
end;

initialization

TTransactionManager.RegisterOperationClass(TOpChangeAccountInfo, CT_Op_ChangeAccountInfo);

end.
