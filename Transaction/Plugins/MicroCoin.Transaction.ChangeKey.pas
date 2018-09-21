unit MicroCoin.Transaction.ChangeKey;

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

uses MicroCoin.Transaction.Base, MicroCoin.Transaction.Transaction,
  MicroCoin.Account.AccountKey, MicroCoin.Account.Transaction,
  MicroCoin.Common, MicroCoin.Account.Data,
  Sysutils, classes, UCrypto, ULog, UConst, MicroCoin.Transaction.Manager;

type

  TChangeKeyTransaction = class(TTransaction)
  protected type
    ChangeKeyTransactionData = record
      account_signer, account_target: Cardinal;
      n_operation: Cardinal;
      fee: UInt64;
      payload: TRawBytes;
      public_key: TECDSA_Public;
      new_accountkey: TAccountKey;
      sign: TECDSA_SIG;
    end;
  private
    FData: ChangeKeyTransactionData;
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
    constructor Create(account_signer, n_operation, account_target: Cardinal; key: TECPrivateKey;
      new_account_key: TAccountKey; fee: UInt64; payload: TRawBytes);

    class function GetHashToSignature(const AErrors: ChangeKeyTransactionData): TRawBytes;
    class function DoSignTransaction(AKey: TECPrivateKey; var ATransaction: ChangeKeyTransactionData): Boolean;

    function GetBuffer(UseProtocolV2: Boolean): TRawBytes; override;
    function ApplyTransaction(AAccountTransaction: TAccountTransaction; var RErrors: AnsiString): Boolean; override;
    procedure AffectedAccounts(AList: TList); override;
    function GetTransactionData(ABlock: Cardinal; Affected_account_number: Cardinal;
      var TransactionData: TTransactionData): Boolean; override;
    function toString: string; override;

    property Data: ChangeKeyTransactionData read FData;
  end;

  TChangeKeySignedTransaction = class(TChangeKeyTransaction)
  protected
    function GetTransactionType: Byte; override;
  public
    function GetTransactionData(Block: Cardinal; Affected_account_number: Cardinal;
      var TransactionData: TTransactionData): Boolean; override;
  end;

implementation

const
  CT_TOpChangeKeyData_NUL: TChangeKeyTransaction.ChangeKeyTransactionData = (account_signer: 0; account_target: 0; n_operation: 0; fee: 0;
    payload: ''; public_key: (EC_OpenSSL_NID: 0; x: ''; y: ''); new_accountkey: (EC_OpenSSL_NID: 0; x: ''; y: '');
    sign: (r: ''; s: ''));

procedure TChangeKeyTransaction.AffectedAccounts(AList: TList);
begin
  AList.Add(TObject(FData.account_signer));
  if (FData.account_target <> FData.account_signer) then
    AList.Add(TObject(FData.account_target));
end;

constructor TChangeKeyTransaction.Create(account_signer, n_operation, account_target: Cardinal; key: TECPrivateKey;
  new_account_key: TAccountKey; fee: UInt64; payload: TRawBytes);
begin
  inherited Create;
  FData.account_signer := account_signer;
  FData.account_target := account_target;
  if (TransactionType = CT_Op_Changekey) then
  begin
    if (account_signer <> account_target) then
      raise Exception.Create('ERROR DEV 20170530-4');
  end
  else if (TransactionType = CT_Op_ChangeKeySigned) then
  begin
    // Allowed signer<>target
  end
  else
    raise Exception.Create('ERROR DEV 20170530-5');
  FData.n_operation := n_operation;
  FData.fee := fee;
  FData.payload := payload;
  // V2: No need to store public key because it's at safebox. Saving at least 64 bytes!
  // FData.public_key := key.PublicKey;
  FData.new_accountkey := new_account_key;
  if not DoSignTransaction(key, FData) then
  begin
    TLog.NewLog(lterror, Classname, 'Error signing a new Change key');
    FHasValidSignature := false;
  end
  else
    FHasValidSignature := true;
end;

function TChangeKeyTransaction.ApplyTransaction(AAccountTransaction: TAccountTransaction;
  var RErrors: AnsiString): Boolean;
var
  account_signer, account_target: TAccount;
begin
  Result := false;
  if (FData.account_signer >= AAccountTransaction.FreezedAccountStorage.AccountsCount) then
  begin
    RErrors := 'Invalid account number';
    Exit;
  end;
  if TAccount.IsAccountBlockedByProtocol(FData.account_signer, AAccountTransaction.FreezedAccountStorage.BlocksCount)
  then
  begin
    RErrors := 'account is blocked for protocol';
    Exit;
  end;
  if (FData.account_signer <> FData.account_target) then
  begin
    if (FData.account_target >= AAccountTransaction.FreezedAccountStorage.AccountsCount) then
    begin
      RErrors := 'Invalid account target number';
      Exit;
    end;
    if TAccount.IsAccountBlockedByProtocol(FData.account_target, AAccountTransaction.FreezedAccountStorage.BlocksCount)
    then
    begin
      RErrors := 'Target account is blocked for protocol';
      Exit;
    end;
  end;
  if (FData.fee < 0) or (FData.fee > CT_MaxTransactionFee) then
  begin
    RErrors := 'Invalid fee: ' + Inttostr(FData.fee);
    Exit;
  end;
  account_signer := AAccountTransaction.Account(FData.account_signer);
  account_target := AAccountTransaction.Account(FData.account_target);
  if ((account_signer.numberOfTransactions + 1) <> FData.n_operation) then
  begin
    RErrors := 'Invalid n_operation';
    Exit;
  end;
  if (account_signer.balance < FData.fee) then
  begin
    RErrors := 'Insuficient founds';
    Exit;
  end;
  if (length(FData.payload) > CT_MaxPayloadSize) then
  begin
    RErrors := 'Invalid Payload size:' + Inttostr(length(FData.payload)) + ' (Max: ' + Inttostr(CT_MaxPayloadSize) + ')';
    if (AAccountTransaction.FreezedAccountStorage.CurrentProtocol >= CT_PROTOCOL_2) then
    begin
      Exit; // BUG from protocol 1
    end;
  end;
  // Is locked? Protocol 2 check
  if (account_signer.accountInfo.IsLocked(AAccountTransaction.FreezedAccountStorage.BlocksCount)) then
  begin
    RErrors := 'Account signer is currently locked';
    Exit;
  end;
  if not FData.new_accountkey.IsValidAccountKey(RErrors) then
  begin
    Exit;
  end;
  // NEW v2 protocol protection: Does not allow to change key for same key
  if (AAccountTransaction.FreezedAccountStorage.CurrentProtocol >= CT_PROTOCOL_2) then
  begin
    if (TAccountKey.EqualAccountKeys(account_target.accountInfo.AccountKey, FData.new_accountkey)) then
    begin
      RErrors := 'New public key is the same public key';
      Exit;
    end;
  end;
  // Build 1.4
  if (FData.public_key.EC_OpenSSL_NID <> CT_TECDSA_Public_Nul.EC_OpenSSL_NID) and
    (not TAccountKey.EqualAccountKeys(FData.public_key, account_signer.accountInfo.AccountKey)) then
  begin
    RErrors := Format('Invalid public key for account %d. Distinct from SafeBox public key! %s <> %s',
      [FData.account_signer, TCrypto.ToHexaString(FData.public_key.ToRawString),
      TCrypto.ToHexaString(account_signer.accountInfo.AccountKey.ToRawString)]);
    Exit;
  end;
  if (FData.account_signer <> FData.account_target) then
  begin
    if (account_target.accountInfo.IsLocked(AAccountTransaction.FreezedAccountStorage.BlocksCount)) then
    begin
      RErrors := 'Account target is currently locked';
      Exit;
    end;
    // Check have same public key
    if not TAccountKey.EqualAccountKeys(account_signer.accountInfo.AccountKey, account_target.accountInfo.AccountKey)
    then
    begin
      RErrors := 'Signer and target accounts have different public key';
      Exit;
    end;
    if (AAccountTransaction.FreezedAccountStorage.CurrentProtocol < CT_PROTOCOL_2) then
    begin
      RErrors := 'NOT ALLOWED ON PROTOCOL 1';
      Exit;
    end;
  end;

  if not TCrypto.ECDSAVerify(account_signer.accountInfo.AccountKey, GetHashToSignature(FData), FData.sign) then
  begin
    RErrors := 'Invalid sign';
    FHasValidSignature := false;
    Exit;
  end
  else
    FHasValidSignature := true;
  FPrevious_Signer_updated_block := account_signer.updated_block;
  FPrevious_Destination_updated_block := account_target.updated_block;
  account_target.accountInfo.AccountKey := FData.new_accountkey;
  // Set to normal:
  account_target.accountInfo.state := as_Normal;
  account_target.accountInfo.locked_until_block := 0;
  account_target.accountInfo.price := 0;
  account_target.accountInfo.account_to_pay := 0;
  account_target.accountInfo.new_publicKey := CT_TECDSA_Public_Nul;
  Result := AAccountTransaction.UpdateAccountInfo(FData.account_signer, FData.n_operation, FData.account_target,
    account_target.accountInfo, account_target.name, account_target.account_type, FData.fee, RErrors);
end;

class function TChangeKeyTransaction.DoSignTransaction(AKey: TECPrivateKey; var ATransaction: ChangeKeyTransactionData): Boolean;
var
  s: AnsiString;
  _sign: TECDSA_SIG;
begin
  s := GetHashToSignature(ATransaction);
  try
    _sign := TCrypto.ECDSASign(AKey, s);
    ATransaction.sign := _sign;
    Result := true;
  except
    on E: Exception do
    begin
      Result := false;
      TLog.NewLog(lterror, Classname, 'Error signing ChangeKey operation: ' + E.Message);
    end;
  end;
end;

function TChangeKeyTransaction.GetBuffer(UseProtocolV2: Boolean): TRawBytes;
var
  ms: TMemoryStream;
  s: AnsiString;
begin
  if UseProtocolV2 then
    Result := inherited GetBuffer(UseProtocolV2)
  else
  begin
    ms := TMemoryStream.Create;
    try
      ms.Write(FData.account_signer, Sizeof(FData.account_signer));
      // Protocol 1 does not allow signer/target. signer=target always
      ms.Write(FData.n_operation, Sizeof(FData.n_operation));
      ms.Write(FData.fee, Sizeof(FData.fee));
      if length(FData.payload) > 0 then
        ms.WriteBuffer(FData.payload[1], length(FData.payload));
      ms.Write(FData.public_key.EC_OpenSSL_NID, Sizeof(FData.public_key.EC_OpenSSL_NID));
      if length(FData.public_key.x) > 0 then
        ms.WriteBuffer(FData.public_key.x[1], length(FData.public_key.x));
      if length(FData.public_key.y) > 0 then
        ms.WriteBuffer(FData.public_key.y[1], length(FData.public_key.y));
      s := FData.new_accountkey.ToRawString;
      if length(s) > 0 then
        ms.WriteBuffer(s[1], length(s));
      if length(FData.sign.r) > 0 then
        ms.WriteBuffer(FData.sign.r[1], length(FData.sign.r));
      if length(FData.sign.s) > 0 then
        ms.WriteBuffer(FData.sign.s[1], length(FData.sign.s));
      ms.Position := 0;
      setlength(Result, ms.Size);
      ms.ReadBuffer(Result[1], ms.Size);
    finally
      ms.Free;
    end;
  end;
end;

class function TChangeKeyTransaction.GetHashToSignature(const AErrors: ChangeKeyTransactionData): TRawBytes;
var
  ms: TMemoryStream;
  s: AnsiString;
begin
  ms := TMemoryStream.Create;
  try
    ms.Write(AErrors.account_signer, Sizeof(AErrors.account_signer));
    if (AErrors.account_signer <> AErrors.account_target) then
      ms.Write(AErrors.account_target, Sizeof(AErrors.account_target));
    ms.Write(AErrors.n_operation, Sizeof(AErrors.n_operation));
    ms.Write(AErrors.fee, Sizeof(AErrors.fee));
    if length(AErrors.payload) > 0 then
      ms.WriteBuffer(AErrors.payload[1], length(AErrors.payload));
    ms.Write(AErrors.public_key.EC_OpenSSL_NID, Sizeof(AErrors.public_key.EC_OpenSSL_NID));
    if length(AErrors.public_key.x) > 0 then
      ms.WriteBuffer(AErrors.public_key.x[1], length(AErrors.public_key.x));
    if length(AErrors.public_key.y) > 0 then
      ms.WriteBuffer(AErrors.public_key.y[1], length(AErrors.public_key.y));
    s := AErrors.new_accountkey.ToRawString;
    if length(s) > 0 then
      ms.WriteBuffer(s[1], length(s));
    ms.Position := 0;
    setlength(Result, ms.Size);
    ms.ReadBuffer(Result[1], ms.Size);
  finally
    ms.Free;
  end;
end;

procedure TChangeKeyTransaction.InitializeData;
begin
  inherited;
  FData := CT_TOpChangeKeyData_NUL;
end;

function TChangeKeyTransaction.LoadFromStream(Stream: TStream; LoadExtendedData: Boolean): Boolean;
var
  s: AnsiString;
begin
  Result := false;
  if Stream.Size - Stream.Position < 16 then
    Exit; // Invalid stream
  Stream.Read(FData.account_signer, Sizeof(FData.account_signer));
  if (TransactionType = CT_Op_Changekey) then
  begin
    FData.account_target := FData.account_signer;
  end
  else if (TransactionType = CT_Op_ChangeKeySigned) then
  begin
    Stream.Read(FData.account_target, Sizeof(FData.account_target));
  end
  else
    raise Exception.Create('ERROR DEV 20170530-1');
  Stream.Read(FData.n_operation, Sizeof(FData.n_operation));
  Stream.Read(FData.fee, Sizeof(FData.fee));
  if TStreamOp.ReadAnsiString(Stream, FData.payload) < 0 then
    Exit;
  if Stream.Read(FData.public_key.EC_OpenSSL_NID, Sizeof(FData.public_key.EC_OpenSSL_NID)) < 0 then
    Exit;
  if TStreamOp.ReadAnsiString(Stream, FData.public_key.x) < 0 then
    Exit;
  if TStreamOp.ReadAnsiString(Stream, FData.public_key.y) < 0 then
    Exit;
  if TStreamOp.ReadAnsiString(Stream, s) < 0 then
    Exit;
  FData.new_accountkey := TAccountKey.FromRawString(s);
  if TStreamOp.ReadAnsiString(Stream, FData.sign.r) < 0 then
    Exit;
  if TStreamOp.ReadAnsiString(Stream, FData.sign.s) < 0 then
    Exit;
  Result := true;
end;

function TChangeKeyTransaction.GetAmount: Int64;
begin
  Result := 0;
end;

function TChangeKeyTransaction.GetFee: UInt64;
begin
  Result := FData.fee;
end;

function TChangeKeyTransaction.GetPayload: TRawBytes;
begin
  Result := FData.payload;
end;

function TChangeKeyTransaction.GetTransactionType : Byte;
begin
  Result := CT_Op_Changekey;
end;

function TChangeKeyTransaction.SaveToStream(Stream: TStream; SaveExtendedData: Boolean): Boolean;
begin
  Stream.Write(FData.account_signer, Sizeof(FData.account_signer));
  if (TransactionType = CT_Op_Changekey) then
  begin
    if FData.account_target <> FData.account_signer then
      raise Exception.Create('ERROR DEV 20170530-2');
  end
  else if (TransactionType = CT_Op_ChangeKeySigned) then
  begin
    Stream.Write(FData.account_target, Sizeof(FData.account_target));
  end
  else
    raise Exception.Create('ERROR DEV 20170530-3');
  Stream.Write(FData.n_operation, Sizeof(FData.n_operation));
  Stream.Write(FData.fee, Sizeof(FData.fee));
  TStreamOp.WriteAnsiString(Stream, FData.payload);
  Stream.Write(FData.public_key.EC_OpenSSL_NID, Sizeof(FData.public_key.EC_OpenSSL_NID));
  TStreamOp.WriteAnsiString(Stream, FData.public_key.x);
  TStreamOp.WriteAnsiString(Stream, FData.public_key.y);
  TStreamOp.WriteAnsiString(Stream, FData.new_accountkey.ToRawString);
  TStreamOp.WriteAnsiString(Stream, FData.sign.r);
  TStreamOp.WriteAnsiString(Stream, FData.sign.s);
  Result := true;
end;

function TChangeKeyTransaction.GetSignerAccount: Cardinal;
begin
  Result := FData.account_signer;
end;

function TChangeKeyTransaction.GetTransactionData(ABlock, Affected_account_number: Cardinal;
  var TransactionData: TTransactionData): Boolean;
begin
  TransactionData := TTransactionData.Empty;
  TransactionData.transactionSubtype := CT_OpSubtype_ChangeKey;
  TransactionData.newKey := Data.new_accountkey;
  TransactionData.DestAccount := Data.account_target;
  TransactionData.OperationTxt := 'Change Key to ' + TAccountKey.GetECInfoTxt(TransactionData.newKey.EC_OpenSSL_NID);
  Result := true;
  TransactionData.OriginalPayload := GetPayload;
  if TCrypto.IsHumanReadable(TransactionData.OriginalPayload) then
    TransactionData.PrintablePayload := TransactionData.OriginalPayload
  else
    TransactionData.PrintablePayload := TCrypto.ToHexaString(TransactionData.OriginalPayload);
  TransactionData.OperationHash := TransactionHash(ABlock);
  if (ABlock < CT_Protocol_Upgrade_v2_MinBlock) then
  begin
    TransactionData.OperationHash_OLD := TransactionHash_OLD(ABlock);
  end;
  TransactionData.valid := true;
end;

function TChangeKeyTransaction.GetDestinationAccount: Int64;
begin
  Result := FData.account_target;
end;

function TChangeKeyTransaction.GetNumberOfTransactions: Cardinal;
begin
  Result := FData.n_operation;
end;

function TChangeKeyTransaction.toString: string;
begin
  Result := Format('Change key of %s to new key: %s fee:%s (n_op:%d) payload size:%d',
    [TAccount.AccountNumberToAccountTxtNumber(FData.account_target),
    TAccountKey.GetECInfoTxt(FData.new_accountkey.EC_OpenSSL_NID), TCurrencyUtils.CurrencyToString(FData.fee),
    FData.n_operation, length(FData.payload)]);
end;

function TChangeKeySignedTransaction.GetTransactionType: Byte;
begin
  Result := CT_Op_ChangeKeySigned;
end;

function TChangeKeySignedTransaction.GetTransactionData(Block, Affected_account_number: Cardinal;
  var TransactionData: TTransactionData): Boolean;
begin
  TransactionData := TTransactionData.Empty;
  TransactionData.transactionSubtype := CT_OpSubtype_ChangeKeySigned;
  TransactionData.newKey := Data.new_accountkey;
  TransactionData.DestAccount := Data.account_target;
  TransactionData.OperationTxt := 'Change ' + TAccount.AccountNumberToAccountTxtNumber(TransactionData.DestAccount) +
    ' account key to ' + TAccountKey.GetECInfoTxt(TransactionData.newKey.EC_OpenSSL_NID);
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

initialization

TTransactionManager.RegisterTransactionPlugin(TChangeKeyTransaction, CT_Op_Changekey);
TTransactionManager.RegisterTransactionPlugin(TChangeKeySignedTransaction, CT_Op_ChangeKeySigned);

end.
