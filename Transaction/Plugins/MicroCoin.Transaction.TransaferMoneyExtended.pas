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
| File:       MicroCoin.Transaction.TransaferMoneyExtended.pas                 |
| Created at: 2018-09-29                                                       |
| Purpose:    Extended version of transafer money transaction with subaccounts |
|==============================================================================}

unit MicroCoin.Transaction.TransaferMoneyExtended;

{$ifdef FPC}
  {$mode delphi}
{$endif}

interface

uses MicroCoin.Transaction.Base,
  MicroCoin.Common,
  MicroCoin.Account.Data,
  MicroCoin.Account.AccountKey,
  MicroCoin.Transaction.Transaction, MicroCoin.Account.Transaction,
  Sysutils, classes, UCrypto, ULog, UConst, MicroCoin.Transaction.Manager;


type
  TTransferMoneyExtended = class(TTransaction)
  protected type
    TTransferMoneyExtendedData = record
      SenderAccount: Cardinal;
      Subaccount : Cardinal;
      NumberOfTransactions: Cardinal;
      TargetAccount: Cardinal;
      TargetSubAccount : Cardinal;
      Amount: UInt64;
      Fee: UInt64;
      Payload: TRawBytes;
      PublicKey: TECDSA_Public;
      Signature: TECDSA_SIG;
      function CheckIsValid(AAccountTransaction : TAccountTransaction; RErros: string) : boolean;
    end;
  protected
    FData : TTransferMoneyExtendedData;
    procedure InitializeData; override;
    function SaveToStream(Stream: TStream; SaveExtendedData: Boolean): Boolean; override;
    function LoadFromStream(Stream: TStream; LoadExtendedData: Boolean): Boolean; override;
    function GetTransactionType: Byte; override;
    function GetAmount: Int64; override;
    function GetFee: UInt64; override;
    function GetPayload: TRawBytes; override;
    function GetSignerAccount: Cardinal; override;
    function GetDestinationAccount: Int64; override;
    function GetSellerAccount: Int64; override;
    function GetNumberOfTransactions: Cardinal; override;
    class function GetTransactionHashForSignature(const trans: TTransferMoneyExtendedData): TRawBytes;
    class function SignTransaction(AKey: TECPrivateKey; var RTransactionData: TTransferMoneyExtendedData): Boolean;
  public
    function GetBuffer(UseProtocolV2: Boolean): TRawBytes; override;
    function ToString: string; override;
    function ApplyTransaction(AccountTransaction: TAccountTransaction; var errors: AnsiString): Boolean; override;
    procedure AffectedAccounts(list: TList); override;
    function GetTransactionData(ABlock: Cardinal; AAffectedAccountNumber: Cardinal; var TransactionData: TTransactionData): Boolean; override;
    constructor CreateTransaction(ASenderAccount, ASubAccount,
                                  ANumberOfTransactions, ATargetAccount, ATargetSubAccount: Cardinal; AKey: TECPrivateKey; AAmount, AFee: UInt64;
                                  APayload: TRawBytes);
  end;

implementation

{ TTransferMoneyExtended.TTransferMoneyExtendedData }

function TTransferMoneyExtended.TTransferMoneyExtendedData.CheckIsValid(
  AAccountTransaction: TAccountTransaction; RErros: string): boolean;
begin
  Result := false;

  if (SenderAccount >= AAccountTransaction.FreezedAccountStorage.AccountsCount) then
  begin
    RErros := Format('Invalid sender %d', [SenderAccount]);
    exit;
  end;

  if (TargetAccount >= AAccountTransaction.FreezedAccountStorage.AccountsCount) then
  begin
    RErros := Format('Invalid target %d', [TargetAccount]);
    exit;
  end;

  if (SenderAccount = TargetAccount) and (Subaccount = TargetSubAccount)  then
  begin
    RErros := Format('Sender=Target %d', [SenderAccount]);
    exit;
  end;

  if TAccount.IsAccountBlockedByProtocol(SenderAccount, AAccountTransaction.FreezedAccountStorage.BlocksCount) then
  begin
    RErros := Format('sender (%d) is blocked for protocol', [SenderAccount]);
    exit;
  end;

  if TAccount.IsAccountBlockedByProtocol(TargetAccount, AAccountTransaction.FreezedAccountStorage.BlocksCount) then
  begin
    RErros := Format('target (%d) is blocked for protocol', [TargetAccount]);
    exit;
  end;

  if (Amount <= 0) or (Amount > cMaxTransactionAmount) then
  begin
    RErros := Format('Invalid amount %d (0 or max: %d)', [Amount, cMaxTransactionAmount]);
    exit;
  end;

  if (Fee < 0) or (Fee > cMaxTransactionFee) then
  begin
    RErros := Format('Invalid fee %d (max %d)', [Fee, cMaxTransactionFee]);
    exit;
  end;

  if (length(Payload) > cMaxPayloadSize) then
  begin
    RErros := 'Invalid Payload size:' + inttostr(length(Payload)) + ' (Max: ' + inttostr(cMaxPayloadSize) + ')';
    if (AAccountTransaction.FreezedAccountStorage.CurrentProtocol >= cPROTOCOL_2)
    then exit;
  end;

  Result := true;

end;

{ TTransferMoneyExtended }

procedure TTransferMoneyExtended.AffectedAccounts(list: TList);
begin
  list.Add(TObject(FData.SenderAccount));
  list.Add(TObject(FData.TargetAccount));
end;

function TTransferMoneyExtended.ApplyTransaction( AccountTransaction: TAccountTransaction; var errors: AnsiString): Boolean;
var
  xSenderAccount : TAccount;
  xTargetAccount : TAccount;
  pSender, pTarget : PAccount;
  xTotalAmount : UInt64;
  _h : TRawBytes;
begin

  Result := false;
  errors := '';

  if not FData.CheckIsValid(AccountTransaction, errors)
  then exit;

  xSenderAccount := AccountTransaction.Account(FData.SenderAccount);
  xTargetAccount := AccountTransaction.Account(FData.TargetAccount);
  xTotalAmount := FData.Amount + FData.Fee;

  if ((xSenderAccount.NumberOfTransactions + 1) <> FData.NumberOfTransactions)
  then begin
    errors := Format('Invalid n_operation %d (expected %d)', [FData.NumberOfTransactions, xSenderAccount.NumberOfTransactions + 1]);
    Exit;
  end;

  if Length(xSenderAccount.SubAccounts) < FData.Subaccount
  then begin
    errors := Format('Invalid subaccount %d/%d', [FData.SenderAccount, FData.Subaccount]);
    exit;
  end;
  if Length(xTargetAccount.SubAccounts) < FData.TargetSubAccount
  then begin
    errors := Format('Invalid subaccount %d/%d', [FData.TargetAccount, FData.TargetSubAccount]);
    exit;
  end;

  if (xSenderAccount.Balance < xTotalAmount)
  then begin
    errors := Format('Insuficient founds %d < (%d + %d = %d)', [xSenderAccount.Balance, FData.Amount, FData.Fee, xTotalAmount]);
    exit;
  end;

  if xSenderAccount.SubAccounts[FData.Subaccount-1].Balance < xTotalAmount
  then begin
    errors := Format('Insuficient founds %d < (%d + %d = %d)', [xSenderAccount.Balance, FData.Amount, FData.Fee, xTotalAmount]);
    exit;
  end;

  if xTargetAccount.SubAccounts[FData.TargetSubAccount-1].Balance + FData.Amount > xTargetAccount.SubAccounts[FData.TargetSubAccount-1].TotalLimit
  then begin
     errors := 'Subaccount limit exceeds.';
     exit;
  end;

  if (xTargetAccount.Balance + FData.Amount > cMaxWalletAmount)
  then begin
    errors := Format('Target cannot accept this transaction due to max amount %d+%d=%d > %d',
      [xTargetAccount.Balance, FData.Amount, xTargetAccount.Balance + FData.Amount, cMaxWalletAmount]);
    exit;
  end;

  if (xSenderAccount.accountInfo.IsLocked(AccountTransaction.FreezedAccountStorage.BlocksCount))
  then begin
    errors := 'Sender Account is currently locked';
    exit;
  end;

  if (FData.PublicKey.EC_OpenSSL_NID <> CT_TECDSA_Public_Nul.EC_OpenSSL_NID) and
    (not TAccountKey.EqualAccountKeys(FData.PublicKey, xSenderAccount.SubAccounts[FData.Subaccount-1].AccountKey))
  then begin
    errors := Format('Invalid sender public key for account %d. Distinct from SafeBox public key! %s <> %s',
      [FData.SenderAccount, TCrypto.ToHexaString((FData.PublicKey.ToRawString)),
      TCrypto.ToHexaString(xSenderAccount.accountInfo.AccountKey.ToRawString)]);
    exit;
  end;

  _h := GetTransactionHashForSignature(FData);

  if (not TCrypto.ECDSAVerify(xSenderAccount.SubAccounts[FData.Subaccount-1].AccountKey, _h, FData.Signature))
  then begin
    errors := 'Invalid signature';
    FHasValidSignature := false;
    exit;
  end else FHasValidSignature := true;

  FPrevious_Signer_updated_block := xSenderAccount.UpdatedBlock;
  FPrevious_Destination_updated_block := xTargetAccount.UpdatedBlock;

  Result := AccountTransaction.TransferAmount(FData.SenderAccount, FData.TargetAccount, FData.NumberOfTransactions,
                                              FData.Amount, FData.Fee, errors, FData.Subaccount, FData.TargetSubAccount);
{
  if Result then begin
    pSender := AccountTransaction.GetInternalAccount(xSenderAccount.AccountNumber);
    pTarget := AccountTransaction.GetInternalAccount(xTargetAccount.AccountNumber);
    if FData.Subaccount>0
    then pSender^.SubAccounts[FData.Subaccount-1].Balance := pSender^.SubAccounts[FData.Subaccount-1].Balance - FData.Amount;
    if FData.TargetSubAccount>0
    then pTarget^.SubAccounts[FData.TargetSubAccount-1].Balance := pTarget^.SubAccounts[FData.TargetSubAccount-1].Balance + FData.Amount;
  end;
}
  Result := true;
end;

constructor TTransferMoneyExtended.CreateTransaction(ASenderAccount,
  ASubAccount, ANumberOfTransactions, ATargetAccount,
  ATargetSubAccount: Cardinal; AKey: TECPrivateKey; AAmount, AFee: UInt64;
  APayload: TRawBytes);
begin
  FData.SenderAccount := ASenderAccount;
  FData.TargetAccount := ATargetAccount;
  FData.Subaccount := ASubAccount;
  FData.TargetSubAccount := ATargetSubAccount;
  FData.NumberOfTransactions := ANumberOfTransactions;
  FData.Amount := AAmount;
  FData.Fee := AFee;
  FData.Payload := APayload;
  if not SignTransaction(AKey, FData) then
  begin
    TLog.NewLog(lterror, Classname, 'Error signing a new Transaction');
    FHasValidSignature := false;
  end
 else FHasValidSignature := true;
end;

function TTransferMoneyExtended.GetAmount: Int64;
begin
  Result := FData.Amount;
end;

function TTransferMoneyExtended.GetBuffer(UseProtocolV2: Boolean): TRawBytes;
begin
  Result := inherited GetBuffer(true);
end;

function TTransferMoneyExtended.GetDestinationAccount: Int64;
begin
  Result := FData.TargetAccount;
end;

function TTransferMoneyExtended.GetFee: UInt64;
begin
  Result := FData.Fee;
end;

function TTransferMoneyExtended.GetNumberOfTransactions: Cardinal;
begin
  Result := FData.NumberOfTransactions;
end;

function TTransferMoneyExtended.GetPayload: TRawBytes;
begin
  Result := FData.Payload;
end;

function TTransferMoneyExtended.GetSellerAccount: Int64;
begin
  Result := 0;
end;

function TTransferMoneyExtended.GetSignerAccount: Cardinal;
begin
  Result := FData.SenderAccount;
end;

function TTransferMoneyExtended.GetTransactionData(ABlock,
  AAffectedAccountNumber: Cardinal;
  var TransactionData: TTransactionData): Boolean;
begin
  Result := true;
  TransactionData := TTransactionData.Empty;
  TransactionData.transactionType := GetTransactionType;
  TransactionData.transactionSubtype := CT_Op_Transaction_Extended;
  TransactionData.DestAccount := GetDestinationAccount;
  TransactionData.Amount := GetAmount;
  TransactionData.Fee := GetFee;
  TransactionData.TransactionAsString := ToString;
  TransactionData.transactionSubtype := 0;
  TransactionData.OriginalPayload := GetPayload;
  if TCrypto.IsHumanReadable(TransactionData.OriginalPayload) then
    TransactionData.PrintablePayload := TransactionData.OriginalPayload
  else
    TransactionData.PrintablePayload := TCrypto.ToHexaString(TransactionData.OriginalPayload);
  TransactionData.OperationHash := TransactionHash(ABlock);
  if (ABlock < cProtocol_Upgrade_v2_MinBlock) then
  begin
    TransactionData.OperationHash_OLD := TransactionHash_OLD(ABlock);
  end;
  TransactionData.valid := true;
end;

class function TTransferMoneyExtended.GetTransactionHashForSignature(
  const trans: TTransferMoneyExtendedData): TRawBytes;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  Stream.Write(trans.SenderAccount, SizeOf(trans.SenderAccount));
  Stream.Write(trans.Subaccount, SizeOf(trans.Subaccount));
  Stream.Write(trans.NumberOfTransactions, SizeOf(trans.NumberOfTransactions));
  Stream.Write(trans.TargetAccount, SizeOf(trans.TargetAccount));
  Stream.Write(trans.TargetSubAccount, SizeOf(trans.TargetSubAccount));
  Stream.Write(trans.Amount, SizeOf(trans.Amount));
  Stream.Write(trans.Fee, SizeOf(trans.Fee));
  TStreamOp.WriteAnsiString(Stream, trans.Payload);
  Stream.Write(trans.PublicKey.EC_OpenSSL_NID, SizeOf(trans.PublicKey.EC_OpenSSL_NID));
  TStreamOp.WriteAnsiString(Stream, trans.PublicKey.x);
  TStreamOp.WriteAnsiString(Stream, trans.PublicKey.y);
  TStreamOp.WriteAnsiString(Stream, trans.Signature.r);
  TStreamOp.WriteAnsiString(Stream, trans.Signature.s);
  SetLength(Result, Stream.Size);
  Stream.Position := 0;
  Stream.ReadBuffer(Result[1], Stream.Size);
  Stream.Free;
end;

function TTransferMoneyExtended.GetTransactionType: Byte;
begin
  Result := CT_Op_Transaction_Extended;
end;

procedure TTransferMoneyExtended.InitializeData;
begin
  inherited;
  FData := Default(TTransferMoneyExtended.TTransferMoneyExtendedData);
end;

function TTransferMoneyExtended.LoadFromStream(Stream: TStream;
  LoadExtendedData: Boolean): Boolean;
begin

  Result := false;

  Stream.Read(FData.SenderAccount, Sizeof(FData.SenderAccount));
  Stream.Read(FData.Subaccount, Sizeof(FData.Subaccount));
  Stream.Read(FData.NumberOfTransactions, Sizeof(FData.NumberOfTransactions));
  Stream.Read(FData.TargetAccount, Sizeof(FData.TargetAccount));
  Stream.Read(FData.TargetSubAccount, Sizeof(FData.TargetSubAccount));
  Stream.Read(FData.Amount, Sizeof(FData.Amount));
  Stream.Read(FData.Fee, Sizeof(FData.Fee));

  if TStreamOp.ReadAnsiString(Stream, FData.Payload) < 0
  then exit;

  if Stream.Read(FData.PublicKey.EC_OpenSSL_NID, Sizeof(FData.PublicKey.EC_OpenSSL_NID)) < 0
  then exit;

  if TStreamOp.ReadAnsiString(Stream, FData.PublicKey.x) < 0
  then exit;

  if TStreamOp.ReadAnsiString(Stream, FData.PublicKey.y) < 0
  then exit;

  if TStreamOp.ReadAnsiString(Stream, FData.Signature.r) < 0
  then exit;

  if TStreamOp.ReadAnsiString(Stream, FData.Signature.s) < 0
  then exit;

  Result := true;

end;

function TTransferMoneyExtended.SaveToStream(Stream: TStream; SaveExtendedData: Boolean): Boolean;
begin

  Result := false;

  Stream.Write(FData.SenderAccount, SizeOf(FData.SenderAccount));
  Stream.Write(FData.Subaccount, SizeOf(FData.Subaccount));
  Stream.Write(FData.NumberOfTransactions, SizeOf(FData.NumberOfTransactions));
  Stream.Write(FData.TargetAccount, SizeOf(FData.TargetAccount));
  Stream.Write(FData.TargetSubAccount, SizeOf(FData.TargetSubAccount));
  Stream.Write(FData.Amount, SizeOf(FData.Amount));
  Stream.Write(FData.Fee, SizeOf(FData.Fee));
  TStreamOp.WriteAnsiString(Stream, FData.Payload);
  Stream.Write(FData.PublicKey.EC_OpenSSL_NID, SizeOf(FData.PublicKey.EC_OpenSSL_NID));
  TStreamOp.WriteAnsiString(Stream, FData.PublicKey.x);
  TStreamOp.WriteAnsiString(Stream, FData.PublicKey.y);
  TStreamOp.WriteAnsiString(Stream, FData.Signature.r);
  TStreamOp.WriteAnsiString(Stream, FData.Signature.s);

  Result := true;

end;

class function TTransferMoneyExtended.SignTransaction(AKey: TECPrivateKey;
  var RTransactionData: TTransferMoneyExtendedData): Boolean;
var
  s: AnsiString;
  _sign: TECDSA_SIG;
begin
  if not Assigned(AKey.PrivateKey) then
  begin
    Result := false;
    RTransactionData.Signature.r := '';
    RTransactionData.Signature.s := '';
    Exit;
  end;
  s := GetTransactionHashForSignature(RTransactionData);
  try
    _sign := TCrypto.ECDSASign(AKey, s);
    RTransactionData.Signature := _sign;
    Result := true;
  except
    RTransactionData.Signature.r := '';
    RTransactionData.Signature.s := '';
    Result := false;
  end;
  SetLength(s, 0);
end;

function TTransferMoneyExtended.ToString: string;
  function sub(a,b : Cardinal) : string;
  begin
    if b=0
    then Result := TAccount.AccountNumberToString(a)
    else Result := Format('%s/%d',[TAccount.AccountNumberToString(a), b]);
  end;
begin
  Result := Format('Send %s MCC from %s to %s',[
         TCurrencyUtils.CurrencyToString( FData.Amount ),
         sub(FData.SenderAccount, FData.Subaccount),
         sub(FData.TargetAccount, FData.TargetSubAccount) ]);
end;

initialization

  TTransactionManager.RegisterTransactionPlugin(TTransferMoneyExtended, CT_Op_Transaction_Extended);

end.
