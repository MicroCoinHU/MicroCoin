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
  MicroCoin.Crypto.Keys,
  MicroCoin.Common, MicroCoin.Account.Data, UBaseTypes,
  Sysutils, classes, UCrypto, ULog, MicroCoin.Common.Config, MicroCoin.Transaction.Manager;

type

  TChangeKeyTransaction = class(TTransaction)
  protected type
    ChangeKeyTransactionData = record
      SignerAccount, TargetAccount: Cardinal;
      NumberOfTransactions: Cardinal;
      Fee: UInt64;
      Payload: TRawBytes;
      PublicKey: TECPublicKey;
      NewAccountKey: TAccountKey;
      Signature: TECDSA_SIG;
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
    constructor Create(account_signer, n_operation, account_target: Cardinal; key: TECKeyPair;
      new_account_key: TAccountKey; fee: UInt64; payload: TRawBytes);

    class function GetHashToSignature(const AData: ChangeKeyTransactionData): TRawBytes;
    class function DoSignTransaction(AKey: TECKeyPair; var ATransaction: ChangeKeyTransactionData): Boolean;

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

uses MicroCoin.Common.Stream;

const
  CT_TOpChangeKeyData_NUL: TChangeKeyTransaction.ChangeKeyTransactionData = (SignerAccount: 0; TargetAccount: 0; NumberOfTransactions: 0; Fee: 0;
    Payload: ''; PublicKey: (EC_OpenSSL_NID: 0; x: ''; y: ''); NewAccountKey: (EC_OpenSSL_NID: 0; x: ''; y: '');
    Signature: (r: ''; s: ''));

procedure TChangeKeyTransaction.AffectedAccounts(AList: TList);
begin
  AList.Add(TObject(FData.SignerAccount));
  if (FData.TargetAccount <> FData.SignerAccount)
  then AList.Add(TObject(FData.TargetAccount));
end;

constructor TChangeKeyTransaction.Create(account_signer, n_operation, account_target: Cardinal; key: TECKeyPair;
  new_account_key: TAccountKey; fee: UInt64; payload: TRawBytes);
begin
  inherited Create;
  FData.SignerAccount := account_signer;
  FData.TargetAccount := account_target;
  if (TransactionType = CT_Op_Changekey)
  then begin
    if (account_signer <> account_target)
    then raise Exception.Create('ERROR DEV 20170530-4');
  end
  else if (TransactionType <> CT_Op_ChangeKeySigned)
  then raise Exception.Create('ERROR DEV 20170530-5');

  FData.NumberOfTransactions := n_operation;
  FData.Fee := fee;
  FData.Payload := payload;
  FData.NewAccountKey := new_account_key;
  if not DoSignTransaction(key, FData)
  then begin
    TLog.NewLog(lterror, Classname, 'Error signing a new Change key');
    FHasValidSignature := false;
  end else FHasValidSignature := true;
end;

function TChangeKeyTransaction.ApplyTransaction(AAccountTransaction: TAccountTransaction;
  var RErrors: AnsiString): Boolean;
var
  account_signer, account_target: TAccount;
begin
  Result := false;

  if (FData.SignerAccount >= AAccountTransaction.FreezedAccountStorage.AccountsCount)
  then begin
    RErrors := 'Invalid account number';
    Exit;
  end;

  if TAccount.IsAccountBlockedByProtocol(FData.SignerAccount, AAccountTransaction.FreezedAccountStorage.BlocksCount)
  then begin
    RErrors := 'account is blocked for protocol';
    Exit;
  end;

  if (FData.SignerAccount <> FData.TargetAccount)
  then begin
    if (FData.TargetAccount >= AAccountTransaction.FreezedAccountStorage.AccountsCount)
    then begin
      RErrors := 'Invalid account target number';
      Exit;
    end;
    if TAccount.IsAccountBlockedByProtocol(FData.TargetAccount, AAccountTransaction.FreezedAccountStorage.BlocksCount)
    then begin
      RErrors := 'Target account is blocked for protocol';
      Exit;
    end;
  end;

  if (FData.Fee < 0) or (FData.Fee > cMaxTransactionFee)
  then begin
    RErrors := 'Invalid fee: ' + Inttostr(FData.Fee);
    Exit;
  end;

  account_signer := AAccountTransaction.Account(FData.SignerAccount);
  account_target := AAccountTransaction.Account(FData.TargetAccount);
  if ((account_signer.NumberOfTransactions + 1) <> FData.NumberOfTransactions)
  then begin
    RErrors := 'Invalid n_operation';
    Exit;
  end;

  if (account_signer.Balance < FData.Fee)
  then begin
    RErrors := 'Insuficient founds';
    Exit;
  end;

  if (length(FData.Payload) > cMaxPayloadSize)
  then begin
    RErrors := 'Invalid Payload size:' + Inttostr(length(FData.Payload)) + ' (Max: ' + Inttostr(cMaxPayloadSize) + ')';
    if (AAccountTransaction.FreezedAccountStorage.CurrentProtocol >= cPROTOCOL_2)
    then Exit;
  end;

  if (account_signer.accountInfo.IsLocked(AAccountTransaction.FreezedAccountStorage.BlocksCount))
  then begin
    RErrors := 'Account signer is currently locked';
    Exit;
  end;

  if not FData.NewAccountKey.IsValidAccountKey(RErrors)
  then Exit;

  if (AAccountTransaction.FreezedAccountStorage.CurrentProtocol >= cPROTOCOL_2)
  then begin
    if (TAccountKey.EqualAccountKeys(account_target.accountInfo.AccountKey, FData.NewAccountKey))
    then begin
      RErrors := 'New public key is the same public key';
      Exit;
    end;
  end;

  if (FData.PublicKey.EC_OpenSSL_NID <> TAccountKey.Empty.EC_OpenSSL_NID) and
    (not TAccountKey.EqualAccountKeys(FData.PublicKey, account_signer.accountInfo.AccountKey))
  then begin
    RErrors := Format('Invalid public key for account %d. Distinct from SafeBox public key! %s <> %s',
      [FData.SignerAccount, TBaseType.ToHexaString(FData.PublicKey.ToRawString),
      TBaseType.ToHexaString(account_signer.accountInfo.AccountKey.ToRawString)]);
    Exit;
  end;

  if (FData.SignerAccount <> FData.TargetAccount)
  then begin
    if (account_target.accountInfo.IsLocked(AAccountTransaction.FreezedAccountStorage.BlocksCount))
    then begin
      RErrors := 'Account target is currently locked';
      Exit;
    end;
    if not TAccountKey.EqualAccountKeys(account_signer.accountInfo.AccountKey, account_target.accountInfo.AccountKey)
    then begin
      RErrors := 'Signer and target accounts have different public key';
      Exit;
    end;
    if (AAccountTransaction.FreezedAccountStorage.CurrentProtocol < cPROTOCOL_2)
    then begin
      RErrors := 'NOT ALLOWED ON PROTOCOL 1';
      Exit;
    end;
  end;

  if not TCrypto.ECDSAVerify(account_signer.accountInfo.AccountKey, GetHashToSignature(FData), FData.Signature)
  then begin
    RErrors := 'Invalid sign';
    FHasValidSignature := false;
    Exit;
  end else FHasValidSignature := true;

  FPrevious_Signer_updated_block := account_signer.UpdatedBlock;
  FPrevious_Destination_updated_block := account_target.UpdatedBlock;
  account_target.accountInfo.AccountKey := FData.NewAccountKey;
  account_target.accountInfo.State := as_Normal;
  account_target.accountInfo.LockedUntilBlock := 0;
  account_target.accountInfo.Price := 0;
  account_target.accountInfo.AccountToPay := 0;
  account_target.accountInfo.NewPublicKey := TAccountKey.Empty;
  Result := AAccountTransaction.UpdateAccountInfo(FData.SignerAccount, FData.NumberOfTransactions, FData.TargetAccount,
    account_target.accountInfo, account_target.Name, account_target.AccountType, FData.Fee, RErrors);
end;

class function TChangeKeyTransaction.DoSignTransaction(AKey: TECKeyPair; var ATransaction: ChangeKeyTransactionData): Boolean;
var
  s: AnsiString;
  _sign: TECDSA_SIG;
begin
  Result := false;
  s := GetHashToSignature(ATransaction);
  try
    _sign := TCrypto.ECDSASign(AKey, s);
    ATransaction.Signature := _sign;
    Result := true;
  except
    on E: Exception do
      TLog.NewLog(lterror, Classname, 'Error signing ChangeKey operation: ' + E.Message);
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
      ms.Write(FData.SignerAccount, Sizeof(FData.SignerAccount));
      ms.Write(FData.NumberOfTransactions, Sizeof(FData.NumberOfTransactions));
      ms.Write(FData.Fee, Sizeof(FData.Fee));
      if length(FData.Payload) > 0
      then ms.WriteBuffer(FData.Payload[1], length(FData.Payload));
      ms.Write(FData.PublicKey.EC_OpenSSL_NID, Sizeof(FData.PublicKey.EC_OpenSSL_NID));
      if length(FData.PublicKey.x) > 0
      then ms.WriteBuffer(FData.PublicKey.x[1], length(FData.PublicKey.x));
      if length(FData.PublicKey.y) > 0
      then ms.WriteBuffer(FData.PublicKey.y[1], length(FData.PublicKey.y));
      s := FData.NewAccountKey.ToRawString;
      if length(s) > 0
      then ms.WriteBuffer(s[1], length(s));
      if length(FData.Signature.r) > 0
      then ms.WriteBuffer(FData.Signature.r[1], length(FData.Signature.r));
      if length(FData.Signature.s) > 0
      then ms.WriteBuffer(FData.Signature.s[1], length(FData.Signature.s));
      ms.Position := 0;
      setlength(Result, ms.Size);
      ms.ReadBuffer(Result[1], ms.Size);
    finally
      ms.Free;
    end;
  end;
end;

class function TChangeKeyTransaction.GetHashToSignature(const AData: ChangeKeyTransactionData): TRawBytes;
var
  ms: TMemoryStream;
  s: AnsiString;
begin
  ms := TMemoryStream.Create;
  try
    ms.Write(AData.SignerAccount, Sizeof(AData.SignerAccount));
    if (AData.SignerAccount <> AData.TargetAccount)
    then ms.Write(AData.TargetAccount, Sizeof(AData.TargetAccount));
    ms.Write(AData.NumberOfTransactions, Sizeof(AData.NumberOfTransactions));
    ms.Write(AData.Fee, Sizeof(AData.Fee));
    if length(AData.Payload) > 0
    then  ms.WriteBuffer(AData.Payload[1], length(AData.Payload));
    ms.Write(AData.PublicKey.EC_OpenSSL_NID, Sizeof(AData.PublicKey.EC_OpenSSL_NID));
    if length(AData.PublicKey.x) > 0
    then ms.WriteBuffer(AData.PublicKey.x[1], length(AData.PublicKey.x));
    if length(AData.PublicKey.y) > 0
    then ms.WriteBuffer(AData.PublicKey.y[1], length(AData.PublicKey.y));
    s := AData.NewAccountKey.ToRawString;
    if length(s) > 0
    then ms.WriteBuffer(s[1], length(s));
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
  if Stream.Size - Stream.Position < 16
  then Exit;
  Stream.Read(FData.SignerAccount, Sizeof(FData.SignerAccount));
  if (TransactionType = CT_Op_Changekey)
  then FData.TargetAccount := FData.SignerAccount
  else if (TransactionType = CT_Op_ChangeKeySigned)
       then Stream.Read(FData.TargetAccount, Sizeof(FData.TargetAccount))
       else raise Exception.Create('ERROR DEV 20170530-1');
  Stream.Read(FData.NumberOfTransactions, Sizeof(FData.NumberOfTransactions));
  Stream.Read(FData.Fee, Sizeof(FData.Fee));
  if Stream.ReadAnsiString(FData.Payload) < 0
  then Exit;
  if Stream.Read(FData.PublicKey.EC_OpenSSL_NID, Sizeof(FData.PublicKey.EC_OpenSSL_NID)) < 0
  then Exit;
  if Stream.ReadAnsiString(FData.PublicKey.x) < 0
  then Exit;
  if Stream.ReadAnsiString(FData.PublicKey.y) < 0
  then Exit;
  if Stream.ReadAnsiString(s) < 0
  then Exit;
  FData.NewAccountKey := TAccountKey.FromRawString(s);
  if Stream.ReadAnsiString(FData.Signature.r) < 0
  then Exit;
  if Stream.ReadAnsiString(FData.Signature.s) < 0
  then Exit;
  Result := true;
end;

function TChangeKeyTransaction.GetAmount: Int64;
begin
  Result := 0;
end;

function TChangeKeyTransaction.GetFee: UInt64;
begin
  Result := FData.Fee;
end;

function TChangeKeyTransaction.GetPayload: TRawBytes;
begin
  Result := FData.Payload;
end;

function TChangeKeyTransaction.GetTransactionType : Byte;
begin
  Result := CT_Op_Changekey;
end;

function TChangeKeyTransaction.SaveToStream(Stream: TStream; SaveExtendedData: Boolean): Boolean;
begin
  Stream.Write(FData.SignerAccount, Sizeof(FData.SignerAccount));
  if (TransactionType = CT_Op_Changekey)
  then begin
    if FData.TargetAccount <> FData.SignerAccount
    then raise Exception.Create('ERROR DEV 20170530-2');
  end
  else if (TransactionType = CT_Op_ChangeKeySigned)
       then Stream.Write(FData.TargetAccount, Sizeof(FData.TargetAccount))
       else raise Exception.Create('ERROR DEV 20170530-3');

  Stream.Write(FData.NumberOfTransactions, Sizeof(FData.NumberOfTransactions));
  Stream.Write(FData.Fee, Sizeof(FData.Fee));
  Stream.WriteAnsiString(FData.Payload);
  Stream.Write(FData.PublicKey.EC_OpenSSL_NID, Sizeof(FData.PublicKey.EC_OpenSSL_NID));
  Stream.WriteAnsiString(FData.PublicKey.x);
  Stream.WriteAnsiString(FData.PublicKey.y);
  Stream.WriteAnsiString(FData.NewAccountKey.ToRawString);
  Stream.WriteAnsiString(FData.Signature.r);
  Stream.WriteAnsiString(FData.Signature.s);
  Result := true;
end;

function TChangeKeyTransaction.GetSignerAccount: Cardinal;
begin
  Result := FData.SignerAccount;
end;

function TChangeKeyTransaction.GetTransactionData(ABlock, Affected_account_number: Cardinal;
  var TransactionData: TTransactionData): Boolean;
begin
  TransactionData := TTransactionData.Empty;
  TransactionData.transactionSubtype := CT_OpSubtype_ChangeKey;
  TransactionData.newKey := Data.NewAccountKey;
  TransactionData.DestAccount := Data.TargetAccount;
  TransactionData.TransactionAsString := 'Change Key to ' + TAccountKey.GetECInfoTxt(TransactionData.newKey.EC_OpenSSL_NID);
  Result := true;
  TransactionData.OriginalPayload := GetPayload;
  if TCrypto.IsHumanReadable(TransactionData.OriginalPayload)
  then TransactionData.PrintablePayload := TransactionData.OriginalPayload
  else TransactionData.PrintablePayload := TBaseType.ToHexaString(TransactionData.OriginalPayload);
  TransactionData.OperationHash := TransactionHash(ABlock);
  if (ABlock < cProtocol_Upgrade_v2_MinBlock)
  then TransactionData.OperationHash_OLD := TransactionHash_OLD(ABlock);
  TransactionData.valid := true;
end;

function TChangeKeyTransaction.GetDestinationAccount: Int64;
begin
  Result := FData.TargetAccount;
end;

function TChangeKeyTransaction.GetNumberOfTransactions: Cardinal;
begin
  Result := FData.NumberOfTransactions;
end;

function TChangeKeyTransaction.toString: string;
begin
  Result := Format('Change key of %s to new key: %s fee:%s (n_op:%d) payload size:%d',
    [TAccount.AccountNumberToString(FData.TargetAccount),
    TAccountKey.GetECInfoTxt(FData.NewAccountKey.EC_OpenSSL_NID), TCurrencyUtils.CurrencyToString(FData.Fee),
    FData.NumberOfTransactions, length(FData.Payload)]);
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
  TransactionData.newKey := Data.NewAccountKey;
  TransactionData.DestAccount := Data.TargetAccount;
  TransactionData.TransactionAsString := 'Change ' + TAccount.AccountNumberToString(TransactionData.DestAccount) +
    ' account key to ' + TAccountKey.GetECInfoTxt(TransactionData.newKey.EC_OpenSSL_NID);
  Result := true;
  TransactionData.OriginalPayload := GetPayload;
  if TCrypto.IsHumanReadable(TransactionData.OriginalPayload)
  then TransactionData.PrintablePayload := TransactionData.OriginalPayload
  else TransactionData.PrintablePayload := TBaseType.ToHexaString(TransactionData.OriginalPayload);
  TransactionData.OperationHash := TransactionHash(Block);
  if (Block < cProtocol_Upgrade_v2_MinBlock)
  then TransactionData.OperationHash_OLD := TransactionHash_OLD(Block);
  TransactionData.valid := true;
end;

initialization

TTransactionManager.RegisterTransactionPlugin(TChangeKeyTransaction, CT_Op_Changekey);
TTransactionManager.RegisterTransactionPlugin(TChangeKeySignedTransaction, CT_Op_ChangeKeySigned);

end.
