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
  MicroCoin.Crypto.Keys,
  MicroCoin.Account.Data, MicroCoin.Account.Storage, MicroCoin.BlockChain.BlockHeader, MicroCoin.Account.Transaction,
  MicroCoin.Common, Classes, MicroCoin.Account.AccountKey, MicroCoin.Transaction.Base, MicroCoin.Common.Config, ULog,
  MicroCoin.Transaction.Manager, UBaseTypes;

type

  TCreateSubAccountTransaction = class(TTransaction)
  strict private type
  TCreateSubAccountData = record
      SignerAccount: Cardinal;
      TargetAccount: Cardinal;
      NumberOfTransactions: Cardinal;
      Fee: UInt64;
      Payload: TRawBytes;
      PublicKey: TECDSA_Public;
      Signature: TECDSA_SIG;
      Balance: UInt64;
      TotalLimit: UInt64;
      DailyLimit: UInt64;
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
    constructor Create(AAccountNumber : Cardinal; Afee: UInt64; ANTransactions: Cardinal;
                       AKey: TECPrivateKey; APublicKey: TECDSA_Public; ABalance: UInt64;
                       ATotalLimit, ADailyLimit : UInt64); reintroduce;
    function GetBuffer(UseProtocolV2: Boolean): TRawBytes; override;
    function ApplyTransaction(AccountTransaction: TAccountTransaction; var errors: AnsiString): Boolean; override;
    procedure AffectedAccounts(list: TList); override;
    function GetTransactionData(Block: Cardinal; Affected_account_number: Cardinal; var TransactionData: TTransactionData): Boolean; override;
    function ToString: string; override;
  end;

implementation

uses MicroCoin.Common.Stream;

{ TCreateSubAccountTransaction }

procedure TCreateSubAccountTransaction.AffectedAccounts(list: TList);
begin
  list.Add(TObject(PtrInt(FData.SignerAccount)))
end;

function TCreateSubAccountTransaction.ApplyTransaction(AccountTransaction: TAccountTransaction; var errors: AnsiString): Boolean;
var
  xTargetAccount: TAccount;
  xAccount: PAccount;
begin

  Result := False;

  if (FData.SignerAccount >= AccountTransaction.FreezedAccountStorage.AccountsCount)
  then begin
    errors := 'Invalid account number';
    exit;
  end;

  if TAccount.IsAccountBlockedByProtocol(FData.SignerAccount, AccountTransaction.FreezedAccountStorage.BlocksCount)
  then begin
    errors := 'account is blocked for protocol';
    exit;
  end;

  if (FData.Fee < 0) or (FData.Fee > cMaxTransactionFee)
  then begin
    errors := 'Invalid fee: ' + Inttostr(FData.Fee);
    exit;
  end;

  xTargetAccount := AccountTransaction.Account(FData.SignerAccount);
{
  if (FData.PublicKey.EC_OpenSSL_NID <> CT_TECDSA_Public_Nul.EC_OpenSSL_NID) and
    (not TAccountKey.EqualAccountKeys(FData.PublicKey, xTargetAccount.accountInfo.AccountKey)) then
  begin
    errors := Format('Invalid public key for account %d. Distinct from SafeBox public key! %s <> %s',
      [FData.SignerAccount, TCrypto.ToHexaString(FData.PublicKey.ToRawString),
      TCrypto.ToHexaString(xTargetAccount.accountInfo.AccountKey.ToRawString)]);
    exit;
  end;
}
  if ((xTargetAccount.NumberOfTransactions + 1) <> FData.NumberOfTransactions)
  then begin
    errors := 'Invalid n_operation';
    exit;
  end;

  if (xTargetAccount.Balance < FData.Fee)
  then begin
    errors := 'Insuficient founds';
    exit;
  end;
{$IFDEF EXTENDEDACCOUNT}
  if xTargetAccount.AvailableBalance-FData.Fee < FData.Balance
  then begin
    errors := 'Insuficient founds';
    exit;
  end;
{$ENDIF}

  if (length(FData.Payload) > cMaxPayloadSize)
  then begin
    errors := 'Invalid Payload size:' + Inttostr(length(FData.Payload)) + ' (Max: ' + Inttostr(cMaxPayloadSize) + ')';
    if (AccountTransaction.FreezedAccountStorage.CurrentProtocol >= cPROTOCOL_2)
    then exit;
  end;

  if (xTargetAccount.accountInfo.IsLocked(AccountTransaction.FreezedAccountStorage.BlocksCount))
  then begin
    errors := 'Account signer is currently locked';
    exit;
  end;

  if (AccountTransaction.FreezedAccountStorage.CurrentProtocol < cPROTOCOL_2)
  then begin
    errors := 'NOT ALLOWED ON PROTOCOL 1';
    exit;
  end;

  if not TCrypto.ECDSAVerify(xTargetAccount.accountInfo.AccountKey, GetHashForSignature(FData), FData.Signature)
  then begin
    errors := 'Invalid signature';
    FHasValidSignature := False;
    exit;
  end
  else FHasValidSignature := true;

  FPrevious_Signer_updated_block := xTargetAccount.UpdatedBlock;
  FPrevious_Destination_updated_block := xTargetAccount.UpdatedBlock;

  xAccount :=  AccountTransaction.GetInternalAccount(xTargetAccount.AccountNumber);
  xAccount^.NumberOfTransactions := FData.NumberOfTransactions;
  xAccount^.Balance := xAccount^.Balance - FData.Fee;
  xAccount^.PreviusUpdatedBlock := xAccount^.UpdatedBlock;
  xAccount^.UpdatedBlock := AccountTransaction.FreezedAccounts.BlocksCount;
  {$IFDEF EXTENDEDACCOUNT}
    SetLength(xAccount^.SubAccounts, Length(xAccount^.SubAccounts)+1);
    xAccount^.SubAccounts[High(xAccount^.SubAccounts)].AccountKey := FData.PublicKey;
    xAccount^.SubAccounts[High(xAccount^.SubAccounts)].Balance := FData.balance;
    xAccount^.SubAccounts[High(xAccount^.SubAccounts)].DailyLimit := FData.DailyLimit;
    xAccount^.SubAccounts[High(xAccount^.SubAccounts)].TotalLimit := FData.TotalLimit;
  {$ENDIF}
  Result := true;
end;

constructor TCreateSubAccountTransaction.Create(AAccountNumber: Cardinal;
  Afee: uInt64; ANTransactions: Cardinal; AKey: TECPrivateKey; APublicKey: TECDSA_Public; ABalance : UInt64;
  ATotalLimit, ADailyLimit : UInt64);
var
  s: AnsiString;
begin
  inherited Create;
  FData.SignerAccount := AAccountNumber;
  FData.NumberOfTransactions := ANTransactions;
  FData.Fee := Afee;
  FData.Payload := '';
  FData.PublicKey := APublicKey;
  FData.Balance := ABalance;
  FData.TotalLimit := ATotalLimit;
  FData.DailyLimit := ADailyLimit;
  s := GetHashForSignature(FData);
  FData.Signature := TCrypto.ECDSASign(Akey, s);
  self.Previous_Signer_updated_block
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
  Result := FData.SignerAccount;
end;

function TCreateSubAccountTransaction.GetFee: UInt64;
begin
  Result := FData.Fee;
end;

function TCreateSubAccountTransaction.GetNumberOfTransactions: Cardinal;
begin
  Result := FData.NumberOfTransactions;
end;

class function TCreateSubAccountTransaction.GetHashForSignature(
  const ATransaction: TCreateSubAccountData): TRawBytes;
var
  Stream: TMemoryStream;
begin

  Stream := TMemoryStream.Create;

  try
    Stream.Write(ATransaction.SignerAccount, SizeOf(ATransaction.SignerAccount));
//    Stream.Write(ATransaction.account_number, SizeOf(ATransaction.account_number));
    Stream.Write(ATransaction.NumberOfTransactions, Sizeof(ATransaction.NumberOfTransactions));
    Stream.Write(ATransaction.Fee, Sizeof(ATransaction.Fee));

    if Stream.WriteAnsiString(ATransaction.Payload) < 0
    then exit;

    if Stream.WriteAccountKey(ATransaction.PublicKey) < 0
    then exit;

    if Stream.WriteAnsiString(ATransaction.Signature.r) < 0
    then exit;

    if Stream.WriteAnsiString(ATransaction.Signature.s) < 0
    then exit;

    Stream.Write(ATransaction.Balance, SizeOf(ATransaction.Balance));
    Stream.Write(ATransaction.TotalLimit, SizeOf(ATransaction.TotalLimit));
    Stream.Write(ATransaction.DailyLimit, SizeOf(ATransaction.DailyLimit));

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
  Result := FData.Payload;
end;

function TCreateSubAccountTransaction.GetSignerAccount: Cardinal;
begin
  Result := FData.SignerAccount;
end;

function TCreateSubAccountTransaction.GetTransactionData(Block,
  Affected_account_number: Cardinal;
  var TransactionData: TTransactionData): Boolean;
var
  s: string;
begin
  Result := true;
  TransactionData := TTransactionData.Empty;
  TransactionData.transactionType := GetTransactionType;
  TransactionData.transactionSubtype := CT_Op_CreateSubAccount;
  TransactionData.DestAccount := GetDestinationAccount;
  s := '';
  TransactionData.TransactionAsString := ToString;
  TransactionData.transactionSubtype := CT_OpSubtype_CreateSubAccount;
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

procedure TCreateSubAccountTransaction.InitializeData;
begin
  inherited InitializeData;
  FData.SignerAccount := 0;
  FData.TargetAccount := 0;
  FData.NumberOfTransactions := 0;
  FData.Fee := 0;
  FData.Payload := '';
  FData.PublicKey := CT_TECDSA_Public_Nul;
  FData.Signature.r := '';
  FData.Signature.s := '';
  FData.Balance := 0;
end;

function TCreateSubAccountTransaction.LoadFromStream(Stream: TStream;
  LoadExtendedData: Boolean): Boolean;
begin
  Result := false;

  Stream.Read(FData.SignerAccount, SizeOf(FData.SignerAccount));
  Stream.Read(FData.TargetAccount, SizeOf(FData.TargetAccount));
  Stream.Read(FData.NumberOfTransactions, Sizeof(FData.NumberOfTransactions));
  Stream.Read(FData.Fee, Sizeof(FData.Fee));

  if Stream.ReadAnsiString(FData.Payload) < 0
  then exit;

  if Stream.ReadAccountKey(FData.PublicKey) < 0
  then exit;

  if Stream.ReadAnsiString(FData.Signature.r) < 0
  then exit;

  if Stream.ReadAnsiString(FData.Signature.s) < 0
  then exit;

  Stream.Read(FData.Balance, SizeOf(FData.Balance));
  Stream.Read(FData.TotalLimit, SizeOf(FData.TotalLimit));
  Stream.Read(FData.DailyLimit, SizeOf(FData.DailyLimit));

  Result := true;
end;

function TCreateSubAccountTransaction.SaveToStream(Stream: TStream; SaveExtendedData: Boolean): Boolean;
begin
  Result := false;

  Stream.Write(FData.SignerAccount, SizeOf(FData.SignerAccount));
  Stream.Write(FData.TargetAccount, SizeOf(FData.TargetAccount));
  Stream.Write(FData.NumberOfTransactions, Sizeof(FData.NumberOfTransactions));
  Stream.Write(FData.Fee, Sizeof(FData.Fee));

  if Stream.WriteAnsiString(FData.Payload) < 0
  then exit;

  if Stream.WriteAccountKey(FData.PublicKey) < 0
  then exit;

  if Stream.WriteAnsiString(FData.Signature.r) < 0
  then exit;

  if Stream.WriteAnsiString(FData.Signature.s) < 0
  then exit;

  Stream.Write(FData.Balance, SizeOf(FData.Balance));
  Stream.Write(FData.TotalLimit, SizeOf(FData.TotalLimit));
  Stream.Write(FData.DailyLimit, SizeOf(FData.DailyLimit));

  Result := true;
end;

function TCreateSubAccountTransaction.ToString: string;
begin
  Result := Format('Create subaccount %d for account %s',[
    FData.TargetAccount,
    TAccount.AccountNumberToString(FData.SignerAccount)]);
end;

{$IFDEF EXTENDEDACCOUNT}
initialization
  TTransactionManager.RegisterTransactionPlugin(TCreateSubAccountTransaction, CT_Op_CreateSubAccount);
{$ENDIF}
end.
