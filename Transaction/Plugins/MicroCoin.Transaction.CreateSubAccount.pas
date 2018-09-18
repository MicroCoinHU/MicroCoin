{==============================================================================|
| MicroCoin                                                                    |
| Copyright (c) 2017-2018 MicroCoin Developers                                 |
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
| File:       MicroCoin.Transaction.CreateSubAccount.pas                       |
| Created at: 2018-09-18                                                       |
| Purpose:    Create subaccount transaction                                    |
|==============================================================================}
unit MicroCoin.Transaction.CreateSubAccount;

interface

uses ucrypto, MicroCoin.Transaction.Transaction, SysUtils,
  MicroCoin.Account.Data, MicroCoin.Account.Storage, MicroCoin.BlockChain.BlockHeader, MicroCoin.Account.Transaction,
  MicroCoin.Common, Classes, MicroCoin.Account.AccountKey, MicroCoin.Transaction.Base, UConst, ULog,
  MicroCoin.Transaction.Manager;

type

  TCreateSubAccountTransaction = class(TTransaction)
  strict private type
  TCreateSubAccountData = record
      account_signer: Cardinal;
      account_number: Cardinal;
      n_operation: Cardinal;
      fee: UInt64;
      payload: TRawBytes;
      public_key: TECDSA_Public;
      sign: TECDSA_SIG;
  end;
  strict private FData: TCreateSubAccountData;
  strict protected
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
    class function GetHashForSignature(const ATransaction: TCreateSubAccountData): TRawBytes; static;
  public
    function GetBuffer(UseProtocolV2: Boolean): TRawBytes; override;
    function ApplyTransaction(AccountTransaction: TAccountTransaction; var errors: AnsiString): Boolean; override;
    procedure AffectedAccounts(list: TList); override;
    function GetTransactionData(Block: Cardinal; Affected_account_number: Cardinal; var TransactionData: TTransactionData): Boolean; override;
    function ToString: string; override;
  end;

implementation

{ TCreateSubAccountTransaction }

procedure TCreateSubAccountTransaction.AffectedAccounts(list: TList);
begin
  list.Add(TObject(FData.account_signer))
end;

function TCreateSubAccountTransaction.ApplyTransaction(AccountTransaction: TAccountTransaction; var errors: AnsiString): Boolean;
var
  xTargetAccount: TAccount;
begin

  Result := False;

  if (FData.account_signer >= AccountTransaction.FreezedAccountStorage.AccountsCount)
  then begin
    errors := 'Invalid account number';
    exit;
  end;

  if TAccount.IsAccountBlockedByProtocol(FData.account_signer, AccountTransaction.FreezedAccountStorage.BlocksCount)
  then begin
    errors := 'account is blocked for protocol';
    exit;
  end;

  if (FData.fee < 0) or (FData.fee > CT_MaxTransactionFee)
  then begin
    errors := 'Invalid fee: ' + Inttostr(FData.fee);
    exit;
  end;

  xTargetAccount := AccountTransaction.Account(FData.account_signer);

  if (FData.public_key.EC_OpenSSL_NID <> CT_TECDSA_Public_Nul.EC_OpenSSL_NID) and
    (not TAccountKey.EqualAccountKeys(FData.public_key, xTargetAccount.accountInfo.AccountKey)) then
  begin
    errors := Format('Invalid public key for account %d. Distinct from SafeBox public key! %s <> %s',
      [FData.account_signer, TCrypto.ToHexaString(FData.public_key.ToRawString),
      TCrypto.ToHexaString(xTargetAccount.accountInfo.AccountKey.ToRawString)]);
    exit;
  end;

  if ((xTargetAccount.n_operation + 1) <> FData.n_operation)
  then begin
    errors := 'Invalid n_operation';
    exit;
  end;

  if (xTargetAccount.balance < FData.fee)
  then begin
    errors := 'Insuficient founds';
    exit;
  end;

  if (length(FData.payload) > CT_MaxPayloadSize)
  then begin
    errors := 'Invalid Payload size:' + Inttostr(length(FData.payload)) + ' (Max: ' + Inttostr(CT_MaxPayloadSize) + ')';
    if (AccountTransaction.FreezedAccountStorage.CurrentProtocol >= CT_PROTOCOL_2)
    then exit;
  end;

  if (xTargetAccount.accountInfo.IsLocked(AccountTransaction.FreezedAccountStorage.BlocksCount))
  then begin
    errors := 'Account signer is currently locked';
    exit;
  end;

  if (AccountTransaction.FreezedAccountStorage.CurrentProtocol < CT_PROTOCOL_2)
  then begin
    errors := 'NOT ALLOWED ON PROTOCOL 1';
    exit;
  end;

  if not TCrypto.ECDSAVerify(xTargetAccount.accountInfo.AccountKey, GetHashForSignature(FData), FData.sign)
  then begin
    errors := 'Invalid sign';
    FHasValidSignature := False;
    exit;
  end
  else FHasValidSignature := true;

end;

function TCreateSubAccountTransaction.GetAmount: Int64;
begin
  Result := 0;
end;

function TCreateSubAccountTransaction.GetBuffer(UseProtocolV2: Boolean): TRawBytes;
begin
  Result := inherited GetBuffer(true);
end;

function TCreateSubAccountTransaction.GetDestinationAccount: Int64;
begin
  Result := FData.account_signer;
end;

function TCreateSubAccountTransaction.GetFee: UInt64;
begin
  Result := FData.fee;
end;

function TCreateSubAccountTransaction.GetNumberOfTransactions: Cardinal;
begin
  Result := FData.n_operation;
end;

class function TCreateSubAccountTransaction.GetHashForSignature(
  const ATransaction: TCreateSubAccountData): TRawBytes;
var
  Stream: TMemoryStream;
begin

  Stream := TMemoryStream.Create;

  try
    Stream.Write(ATransaction.account_signer, SizeOf(ATransaction.account_signer));
    Stream.Write(ATransaction.account_number, SizeOf(ATransaction.account_number));
    Stream.Write(ATransaction.n_operation, Sizeof(ATransaction.n_operation));
    Stream.Write(ATransaction.fee, Sizeof(ATransaction.fee));

    if TStreamOp.WriteAnsiString(Stream, ATransaction.payload) < 0
    then exit;

    if TStreamOp.WriteAccountKey(Stream, ATransaction.public_key) < 0
    then exit;

    if TStreamOp.WriteAnsiString(Stream, ATransaction.sign.r) < 0
    then exit;

    if TStreamOp.WriteAnsiString(Stream, ATransaction.sign.s) < 0
    then exit;

    Stream.Position := 0;
    setlength(Result, Stream.Size);
    Stream.ReadBuffer(Result[1], Stream.Size);

  finally
    FreeAndNil(Stream);
  end;

end;

function TCreateSubAccountTransaction.GetTransactionType: Byte;
begin
  Result := CT_Op_CreateSubAccount;
end;

function TCreateSubAccountTransaction.GetPayload: TRawBytes;
begin
  Result := FData.payload;
end;

function TCreateSubAccountTransaction.GetSignerAccount: Cardinal;
begin
  Result := FData.account_signer;
end;

function TCreateSubAccountTransaction.GetTransactionData(Block,
  Affected_account_number: Cardinal;
  var TransactionData: TTransactionData): Boolean;
var
  s: string;
begin
  Result := true;
  TransactionData := TTransactionData.Empty;
  TransactionData.DestAccount := GetDestinationAccount;
  s := '';
  TransactionData.OperationTxt := Format('Create sub account (%d)', [FData.account_number]);
  TransactionData.OpSubtype := CT_OpSubtype_CreateSubAccount;
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

procedure TCreateSubAccountTransaction.InitializeData;
begin
  inherited InitializeData;
  FData.account_signer := 0;
  FData.account_number := 0;
  FData.n_operation := 0;
  FData.fee := 0;
  FData.payload := '';
  FData.public_key := CT_TECDSA_Public_Nul;
  FData.sign.r := '';
  FData.sign.s := '';
end;

function TCreateSubAccountTransaction.LoadFromStream(Stream: TStream;
  LoadExtendedData: Boolean): Boolean;
begin
  Result := false;

  Stream.Read(FData.account_signer, SizeOf(FData.account_signer));
  Stream.Read(FData.account_number, SizeOf(FData.account_number));
  Stream.Read(FData.n_operation, Sizeof(FData.n_operation));
  Stream.Read(FData.fee, Sizeof(FData.fee));

  if TStreamOp.ReadAnsiString(Stream, FData.payload) < 0
  then exit;

  if TStreamOp.ReadAccountKey(Stream, FData.public_key) < 0
  then exit;

  if TStreamOp.ReadAnsiString(Stream, FData.sign.r) < 0
  then exit;

  if TStreamOp.ReadAnsiString(Stream, FData.sign.s) < 0
  then exit;

  Result := true;
end;

function TCreateSubAccountTransaction.SaveToStream(Stream: TStream;
  SaveExtendedData: Boolean): Boolean;
begin

  Result := false;

  Stream.Write(FData.account_signer, SizeOf(FData.account_signer));
  Stream.Write(FData.account_number, SizeOf(FData.account_number));
  Stream.Write(FData.n_operation, Sizeof(FData.n_operation));
  Stream.Write(FData.fee, Sizeof(FData.fee));

  if TStreamOp.WriteAnsiString(Stream, FData.payload) < 0
  then exit;

  if TStreamOp.WriteAccountKey(Stream, FData.public_key) < 0
  then exit;

  if TStreamOp.WriteAnsiString(Stream, FData.sign.r) < 0
  then exit;

  if TStreamOp.WriteAnsiString(Stream, FData.sign.s) < 0
  then exit;

  Result := true;

end;

function TCreateSubAccountTransaction.ToString: string;
begin
  Result := Format('Create sub account %d for account %s',[ FData.account_number, TAccount.AccountNumberToAccountTxtNumber(FData.account_signer)]);
end;

initialization
  TTransactionManager.RegisterTransactionPlugin(TCreateSubAccountTransaction, CT_Op_CreateSubAccount);
end.
