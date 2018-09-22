unit MicroCoin.Transaction.ListAccount;

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

uses MicroCoin.Transaction.Base,
  MicroCoin.Common,
  MicroCoin.Account.Data, MicroCoin.Account.Transaction,
  MicroCoin.Account.AccountKey,
  MicroCoin.Transaction.Transaction,
  Sysutils, classes, UCrypto,
  ULog, UConst, MicroCoin.Transaction.Manager;

type

  TListAccountTransactionType = (lat_Unknown, lat_ListForSale, lat_DelistAccount);

  TListAccountTransaction = class(TTransaction)
  protected
    type TListAccountTransactionData = record
      SignerAccount, TargetAccount: Cardinal;
      TransactionType: TListAccountTransactionType;
      NumberOfTransactions: Cardinal;
      AccountPrice: UInt64;
      AccountToPay: Cardinal;
      Fee: UInt64;
      Payload: TRawBytes;
      PublicKey: TAccountKey;
      NewPublicKey: TAccountKey;
      // If EC_OpenSSL_NID=0 then is OPEN, otherwise is for only 1 public key
      LockedUntilBlock: Cardinal; //
      Signature: TECDSA_SIG;
    end;
  private
    FData: TListAccountTransactionData;
  protected
    procedure InitializeData; override;
    function SaveToStream(Stream: TStream; SaveExtendedData: Boolean): Boolean; override;
    function LoadFromStream(Stream: TStream; LoadExtendedData: Boolean): Boolean; override;
    function GetAmount: Int64; override;
    function GetFee: UInt64; override;
    function GetPayload: TRawBytes; override;
    function GetSignerAccount: Cardinal; override;
    function GetDestinationAccount: Int64; override;
    function GetSellerAccount: Int64; override;
    function GetNumberOfTransactions: Cardinal; override;
  public
    class function GetTransactionHashForSignature(const ATransaction: TListAccountTransactionData): TRawBytes;
    class function DoSignTransaction(key: TECPrivateKey; var ATransaction: TListAccountTransactionData): Boolean;
    function IsPrivateSale: Boolean;
    function IsDelist: Boolean; virtual; abstract;
    function GetBuffer(UseProtocolV2: Boolean): TRawBytes; override;
    function ApplyTransaction(AccountTransaction: TAccountTransaction; var errors: AnsiString): Boolean; override;
    procedure AffectedAccounts(list: TList); override;
    function toString: string; override;
    property Data: TListAccountTransactionData read FData;
  end;

  TListAccountForSaleTransaction = class(TListAccountTransaction)
  protected
    function GetTransactionType: Byte; override;
  public
    constructor CreateListAccountForSale(account_signer, ANumberOfTransactions, account_target: Cardinal;
      account_price, fee: UInt64; account_to_pay: Cardinal; new_public_key: TAccountKey; locked_until_block: Cardinal;
      key: TECPrivateKey; payload: TRawBytes);
    function IsDelist: Boolean; override;
    function GetTransactionData(Block: Cardinal; Affected_account_number: Cardinal;
      var TransactionData: TTransactionData): Boolean; override;
  end;

  TDelistAccountTransaction = class(TListAccountTransaction)
  public
    function GetTransactionType: Byte; override;
    constructor CreateDelistAccountForSale(account_signer, ANumberOfTransactions, account_target: Cardinal; fee: UInt64;
      key: TECPrivateKey; payload: TRawBytes);
    function IsDelist: Boolean; override;
    function GetTransactionData(Block: Cardinal; Affected_account_number: Cardinal;
      var TransactionData: TTransactionData): Boolean; override;
  end;

implementation

const
  CT_TOpListAccountData_NUL: TListAccountTransaction.TListAccountTransactionData = (SignerAccount: 0; TargetAccount: 0; TransactionType: lat_Unknown;
    NumberOfTransactions: 0; AccountPrice: 0; AccountToPay: 0; Fee: 0; Payload: '';
    PublicKey: (EC_OpenSSL_NID: 0; x: ''; y: ''); NewPublicKey: (EC_OpenSSL_NID: 0; x: ''; y: '');
    LockedUntilBlock: 0; Signature: (r: ''; s: ''));

procedure TListAccountTransaction.AffectedAccounts(list: TList);
begin
  list.Add(TObject(FData.SignerAccount));
  if FData.SignerAccount <> FData.TargetAccount then
    list.Add(TObject(FData.TargetAccount));
end;

function TListAccountTransaction.ApplyTransaction(AccountTransaction: TAccountTransaction; var errors: AnsiString): Boolean;
var
  account_signer, account_target: TAccount;
begin
  Result := false;
  if (AccountTransaction.FreezedAccountStorage.CurrentProtocol < CT_PROTOCOL_2) then
  begin
    errors := 'List/Delist Account is not allowed on Protocol 1';
    exit;
  end;
  if (FData.SignerAccount >= AccountTransaction.FreezedAccountStorage.AccountsCount) then
  begin
    errors := 'Invalid signer account number';
    exit;
  end;
  if (FData.TargetAccount >= AccountTransaction.FreezedAccountStorage.AccountsCount) then
  begin
    errors := 'Invalid target account number';
    exit;
  end;
  if TAccount.IsAccountBlockedByProtocol(FData.SignerAccount, AccountTransaction.FreezedAccountStorage.BlocksCount)
  then
  begin
    errors := 'Signer account is blocked for protocol';
    exit;
  end;
  if TAccount.IsAccountBlockedByProtocol(FData.TargetAccount, AccountTransaction.FreezedAccountStorage.BlocksCount)
  then
  begin
    errors := 'Target account is blocked for protocol';
    exit;
  end;
  if not IsDelist then
  begin
    if (FData.AccountToPay >= AccountTransaction.FreezedAccountStorage.AccountsCount) then
    begin
      errors := 'Invalid account to pay number';
      exit;
    end;
    if (FData.TargetAccount = FData.AccountToPay) then
    begin
      errors := 'Account to pay is itself';
      exit;
    end;
    if TAccount.IsAccountBlockedByProtocol(FData.AccountToPay, AccountTransaction.FreezedAccountStorage.BlocksCount)
    then
    begin
      errors := 'Account to pay is blocked for protocol';
      exit;
    end;
    if (FData.AccountPrice <= 0) then
    begin
      errors := 'Account for sale price must be > 0';
      exit;
    end;
    if (FData.LockedUntilBlock > (AccountTransaction.FreezedAccountStorage.BlocksCount +
      CT_MaxFutureBlocksLockedAccount)) then
    begin
      errors := 'Invalid locked block: Current block ' + Inttostr(AccountTransaction.FreezedAccountStorage.BlocksCount)
        + ' cannot lock to block ' + Inttostr(FData.LockedUntilBlock);
      exit;
    end;
    if IsPrivateSale then
    begin
      if not FData.NewPublicKey.IsValidAccountKey(errors) then
      begin
        errors := 'Invalid new public key: ' + errors;
        exit;
      end;
    end;
  end;
  if (FData.Fee < 0) or (FData.Fee > CT_MaxTransactionFee) then
  begin
    errors := 'Invalid fee: ' + Inttostr(FData.Fee);
    exit;
  end;
  account_signer := AccountTransaction.Account(FData.SignerAccount);
  account_target := AccountTransaction.Account(FData.TargetAccount);
  if (FData.SignerAccount <> FData.TargetAccount) then
  begin
    // Both accounts must have same PUBLIC KEY!
    if not TAccountKey.EqualAccountKeys(account_signer.accountInfo.AccountKey, account_target.accountInfo.AccountKey)
    then
    begin
      errors := 'Signer and affected accounts have different public key';
      exit;
    end;
  end;
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
  if (length(FData.Payload) > CT_MaxPayloadSize) then
  begin
    errors := 'Invalid Payload size:' + Inttostr(length(FData.Payload)) + ' (Max: ' + Inttostr(CT_MaxPayloadSize) + ')';
    exit;
  end;
  // Is locked?
  if (account_signer.accountInfo.IsLocked(AccountTransaction.FreezedAccountStorage.BlocksCount)) then
  begin
    errors := 'Signer account is currently locked';
    exit;
  end;
  if (account_target.accountInfo.IsLocked(AccountTransaction.FreezedAccountStorage.BlocksCount)) then
  begin
    errors := 'Target account is currently locked';
    exit;
  end;
  if (IsPrivateSale) then
  begin
    if TAccountKey.EqualAccountKeys(account_target.accountInfo.AccountKey, FData.NewPublicKey) then
    begin
      errors := 'New public key for private sale is the same public key';
      exit;
    end;
  end;
  //
  // Build 1.4
  if (FData.PublicKey.EC_OpenSSL_NID <> CT_TECDSA_Public_Nul.EC_OpenSSL_NID) and
    (not TAccountKey.EqualAccountKeys(FData.PublicKey, account_signer.accountInfo.AccountKey)) then
  begin
    errors := Format('Invalid public key for account %d. Distinct from SafeBox public key! %s <> %s',
      [FData.SignerAccount, TCrypto.ToHexaString(FData.PublicKey.ToRawString),
      TCrypto.ToHexaString(account_signer.accountInfo.AccountKey.ToRawString)]);
    exit;
  end;
  if not TCrypto.ECDSAVerify(account_signer.accountInfo.AccountKey, GetTransactionHashForSignature(FData), FData.Signature) then
  begin
    errors := 'Invalid sign';
    FHasValidSignature := false;
    exit;
  end
  else
    FHasValidSignature := true;
  FPrevious_Signer_updated_block := account_signer.UpdatedBlock;
  FPrevious_Destination_updated_block := account_target.UpdatedBlock;
  if IsDelist then
  begin
    account_target.accountInfo.State := as_Normal;
    account_target.accountInfo.LockedUntilBlock := CT_AccountInfo_NUL.LockedUntilBlock;
    account_target.accountInfo.Price := CT_AccountInfo_NUL.Price;
    account_target.accountInfo.AccountToPay := CT_AccountInfo_NUL.AccountToPay;
    account_target.accountInfo.NewPublicKey := CT_AccountInfo_NUL.NewPublicKey;
  end
  else
  begin
    account_target.accountInfo.State := as_ForSale;
    account_target.accountInfo.LockedUntilBlock := FData.LockedUntilBlock;
    account_target.accountInfo.Price := FData.AccountPrice;
    account_target.accountInfo.AccountToPay := FData.AccountToPay;
    account_target.accountInfo.NewPublicKey := FData.NewPublicKey;
  end;
  Result := AccountTransaction.UpdateAccountInfo(FData.SignerAccount, FData.NumberOfTransactions, FData.TargetAccount,
    account_target.accountInfo, account_target.Name, account_target.AccountType, FData.Fee, errors);
end;

class function TListAccountTransaction.DoSignTransaction(key: TECPrivateKey; var ATransaction: TListAccountTransactionData): Boolean;
var
  s: AnsiString;
  _sign: TECDSA_SIG;
begin
  s := GetTransactionHashForSignature(ATransaction);
  try
    _sign := TCrypto.ECDSASign(key, s);
    ATransaction.Signature := _sign;
    Result := true;
  except
    on E: Exception do
    begin
      Result := false;
      TLog.NewLog(lterror, ClassName, 'Error signing Account for sale operation: ' + E.Message);
    end;
  end;
end;

function TListAccountTransaction.GetBuffer(UseProtocolV2: Boolean): TRawBytes;
begin
  // This Operation is new from protocol V2, so we cannot hash it as a previous protocol!
  Result := inherited GetBuffer(true);
end;

class function TListAccountTransaction.GetTransactionHashForSignature(const ATransaction: TListAccountTransactionData): TRawBytes;
var
  ms: TMemoryStream;
  s: AnsiString;
begin
  ms := TMemoryStream.Create;
  try
    ms.Write(ATransaction.SignerAccount, Sizeof(ATransaction.SignerAccount));
    ms.Write(ATransaction.TargetAccount, Sizeof(ATransaction.TargetAccount));
    ms.Write(ATransaction.NumberOfTransactions, Sizeof(ATransaction.NumberOfTransactions));
    ms.Write(ATransaction.AccountPrice, Sizeof(ATransaction.AccountPrice));
    ms.Write(ATransaction.AccountToPay, Sizeof(ATransaction.AccountToPay));
    ms.Write(ATransaction.Fee, Sizeof(ATransaction.Fee));
    if length(ATransaction.Payload) > 0 then
      ms.WriteBuffer(ATransaction.Payload[1], length(ATransaction.Payload));
    ms.Write(ATransaction.PublicKey.EC_OpenSSL_NID, Sizeof(ATransaction.PublicKey.EC_OpenSSL_NID));
    if length(ATransaction.PublicKey.x) > 0 then
      ms.WriteBuffer(ATransaction.PublicKey.x[1], length(ATransaction.PublicKey.x));
    if length(ATransaction.PublicKey.y) > 0 then
      ms.WriteBuffer(ATransaction.PublicKey.y[1], length(ATransaction.PublicKey.y));
    s := ATransaction.NewPublicKey.ToRawString;
    if length(s) > 0 then
      ms.WriteBuffer(s[1], length(s));
    ms.Write(ATransaction.LockedUntilBlock, Sizeof(ATransaction.LockedUntilBlock));
    ms.Position := 0;
    setlength(Result, ms.Size);
    ms.ReadBuffer(Result[1], ms.Size);
  finally
    ms.Free;
  end;
end;

procedure TListAccountTransaction.InitializeData;
begin
  inherited;
  FData := CT_TOpListAccountData_NUL;
end;

function TListAccountTransaction.IsPrivateSale: Boolean;
begin
  Result := (not IsDelist) and (FData.NewPublicKey.EC_OpenSSL_NID <> 0);
end;

function TListAccountTransaction.LoadFromStream(Stream: TStream; LoadExtendedData: Boolean): Boolean;
var
  s: AnsiString;
  w: Word;
begin
  Result := false;
  if Stream.Size - Stream.Position < 14 then
    exit; // Invalid stream
  Stream.Read(FData.SignerAccount, Sizeof(FData.SignerAccount));
  Stream.Read(FData.TargetAccount, Sizeof(FData.TargetAccount));
  Stream.Read(w, 2);
  case w of
    CT_Op_ListAccountForSale:
      FData.TransactionType := lat_ListForSale;
    CT_Op_DelistAccount:
      FData.TransactionType := lat_DelistAccount;
  else
    exit; // Invalid data info
  end;
  Stream.Read(FData.NumberOfTransactions, Sizeof(FData.NumberOfTransactions));
  if (FData.TransactionType = lat_ListForSale) then
  begin
    Stream.Read(FData.AccountPrice, Sizeof(FData.AccountPrice));
    Stream.Read(FData.AccountToPay, Sizeof(FData.AccountToPay));
    if Stream.Read(FData.PublicKey.EC_OpenSSL_NID, Sizeof(FData.PublicKey.EC_OpenSSL_NID)) < 0 then
      exit;
    if TStreamOp.ReadAnsiString(Stream, FData.PublicKey.x) < 0 then
      exit;
    if TStreamOp.ReadAnsiString(Stream, FData.PublicKey.y) < 0 then
      exit;
    if TStreamOp.ReadAnsiString(Stream, s) < 0 then
      exit;
    FData.NewPublicKey := TAccountKey.FromRawString(s);
    Stream.Read(FData.LockedUntilBlock, Sizeof(FData.LockedUntilBlock));
  end;
  Stream.Read(FData.Fee, Sizeof(FData.Fee));
  if TStreamOp.ReadAnsiString(Stream, FData.Payload) < 0 then
    exit;
  if TStreamOp.ReadAnsiString(Stream, FData.Signature.r) < 0 then
    exit;
  if TStreamOp.ReadAnsiString(Stream, FData.Signature.s) < 0 then
    exit;
  Result := true;
end;

function TListAccountTransaction.GetNumberOfTransactions: Cardinal;
begin
  Result := FData.NumberOfTransactions;
end;

function TListAccountTransaction.GetAmount: Int64;
begin
  Result := 0;
end;

function TListAccountTransaction.GetFee: UInt64;
begin
  Result := FData.Fee;
end;

function TListAccountTransaction.GetPayload: TRawBytes;
begin
  Result := FData.Payload;
end;

function TListAccountTransaction.SaveToStream(Stream: TStream; SaveExtendedData: Boolean): Boolean;
var
  w: Word;
begin
  Stream.Write(FData.SignerAccount, Sizeof(FData.SignerAccount));
  Stream.Write(FData.TargetAccount, Sizeof(FData.TargetAccount));
  case FData.TransactionType of
    lat_ListForSale:
      w := CT_Op_ListAccountForSale;
    lat_DelistAccount:
      w := CT_Op_DelistAccount;
  else
    raise Exception.Create('ERROR DEV 20170412-1');
  end;
  Stream.Write(w, 2);
  Stream.Write(FData.NumberOfTransactions, Sizeof(FData.NumberOfTransactions));
  if FData.TransactionType = lat_ListForSale then
  begin
    Stream.Write(FData.AccountPrice, Sizeof(FData.AccountPrice));
    Stream.Write(FData.AccountToPay, Sizeof(FData.AccountToPay));
    Stream.Write(FData.PublicKey.EC_OpenSSL_NID, Sizeof(FData.PublicKey.EC_OpenSSL_NID));
    TStreamOp.WriteAnsiString(Stream, FData.PublicKey.x);
    TStreamOp.WriteAnsiString(Stream, FData.PublicKey.y);
    TStreamOp.WriteAnsiString(Stream, FData.NewPublicKey.ToRawString);
    Stream.Write(FData.LockedUntilBlock, Sizeof(FData.LockedUntilBlock));
  end;
  Stream.Write(FData.Fee, Sizeof(FData.Fee));
  TStreamOp.WriteAnsiString(Stream, FData.Payload);
  TStreamOp.WriteAnsiString(Stream, FData.Signature.r);
  TStreamOp.WriteAnsiString(Stream, FData.Signature.s);
  Result := true;
end;

function TListAccountTransaction.GetSignerAccount: Cardinal;
begin
  Result := FData.SignerAccount;
end;

function TListAccountTransaction.GetDestinationAccount: Int64;
begin
  Result := FData.TargetAccount;
end;

function TListAccountTransaction.GetSellerAccount: Int64;
begin
  case FData.TransactionType of
    lat_ListForSale:
      Result := FData.AccountToPay;
  else
    Result := inherited GetSellerAccount;
  end;
end;

function TListAccountTransaction.toString: string;
begin
  case FData.TransactionType of
    lat_ListForSale:
      begin
        if (FData.NewPublicKey.EC_OpenSSL_NID = CT_TECDSA_Public_Nul.EC_OpenSSL_NID) then
        begin
          Result := Format('List account %s for sale price %s locked until block:%d fee:%s (n_op:%d) payload size:%d',
            [TAccount.AccountNumberToString(FData.TargetAccount),
            TCurrencyUtils.CurrencyToString(FData.AccountPrice), FData.LockedUntilBlock,
            TCurrencyUtils.CurrencyToString(FData.Fee), FData.NumberOfTransactions, length(FData.Payload)])
        end
        else
        begin
          Result := Format
            ('List account %s for private sale price %s reserved for %s locked until block:%d fee:%s (n_op:%d) payload size:%d',
            [TAccount.AccountNumberToString(FData.TargetAccount),
            TCurrencyUtils.CurrencyToString(FData.AccountPrice),
            TAccountKey.GetECInfoTxt(FData.NewPublicKey.EC_OpenSSL_NID), FData.LockedUntilBlock,
            TCurrencyUtils.CurrencyToString(FData.Fee), FData.NumberOfTransactions, length(FData.Payload)])
        end;
      end;
    lat_DelistAccount:
      begin
        Result := Format('Delist account %s for sale fee:%s (n_op:%d) payload size:%d',
          [TAccount.AccountNumberToString(FData.TargetAccount), TCurrencyUtils.CurrencyToString(FData.Fee),
          FData.NumberOfTransactions, length(FData.Payload)])
      end;
  else
    Result := 'ERROR DEV 20170414-2';
  end;
end;

constructor TListAccountForSaleTransaction.CreateListAccountForSale(account_signer, ANumberOfTransactions, account_target: Cardinal;
  account_price, fee: UInt64; account_to_pay: Cardinal; new_public_key: TAccountKey; locked_until_block: Cardinal;
  key: TECPrivateKey; payload: TRawBytes);
begin
  inherited Create;
  FData.SignerAccount := account_signer;
  FData.TargetAccount := account_target;
  FData.TransactionType := lat_ListForSale;
  FData.NumberOfTransactions := ANumberOfTransactions;
  FData.AccountPrice := account_price;
  FData.AccountToPay := account_to_pay;
  FData.Fee := fee;
  FData.Payload := payload;
  // V2: No need to store public key because it's at safebox. Saving at least 64 bytes!
  // FData.public_key := key.PublicKey;
  FData.NewPublicKey := new_public_key;
  FData.LockedUntilBlock := locked_until_block;
  if not DoSignTransaction(key, FData) then
  begin
    TLog.NewLog(lterror, ClassName, 'Error signing a new list account for sale operation');
    FHasValidSignature := false;
  end
  else
    FHasValidSignature := true;
end;

function TListAccountForSaleTransaction.IsDelist: Boolean;
begin
  Result := false;
end;

function TListAccountForSaleTransaction.GetTransactionType: Byte;
begin
  Result := CT_Op_ListAccountForSale;
end;

function TListAccountForSaleTransaction.GetTransactionData(Block, Affected_account_number: Cardinal;
  var TransactionData: TTransactionData): Boolean;
begin
  TransactionData := TTransactionData.Empty;
  if IsPrivateSale then
  begin
    TransactionData.transactionSubtype := CT_OpSubtype_ListAccountForPrivateSale;
    TransactionData.TransactionAsString := 'List account ' + TAccount.AccountNumberToString(Data.TargetAccount) +
      ' for private sale price ' + TCurrencyUtils.CurrencyToString(Data.AccountPrice) + ' MCC pay to ' +
      TAccount.AccountNumberToString(Data.AccountToPay);
  end
  else
  begin
    TransactionData.transactionSubtype := CT_OpSubtype_ListAccountForPublicSale;
    TransactionData.TransactionAsString := 'List account ' + TAccount.AccountNumberToString(Data.TargetAccount) +
      ' for sale price ' + TCurrencyUtils.CurrencyToString(Data.AccountPrice) + ' MCC pay to ' +
      TAccount.AccountNumberToString(Data.AccountToPay);
  end;
  TransactionData.newKey := Data.NewPublicKey;
  TransactionData.SellerAccount := GetSellerAccount;
  Result := true;
end;

constructor TDelistAccountTransaction.CreateDelistAccountForSale(account_signer, ANumberOfTransactions, account_target: Cardinal;
  fee: UInt64; key: TECPrivateKey; payload: TRawBytes);
begin
  inherited Create;
  FData.SignerAccount := account_signer;
  FData.TargetAccount := account_target;
  FData.TransactionType := lat_DelistAccount;
  FData.NumberOfTransactions := ANumberOfTransactions;
  FData.Fee := fee;
  FData.Payload := payload;
  if not DoSignTransaction(key, FData) then
  begin
    TLog.NewLog(lterror, ClassName, 'Error signing a delist account operation');
    FHasValidSignature := false;
  end
  else
    FHasValidSignature := true;
end;

function TDelistAccountTransaction.IsDelist: Boolean;
begin
  Result := true;
end;

function TDelistAccountTransaction.GetTransactionType: Byte;
begin
  Result := CT_Op_DelistAccount;
end;

function TDelistAccountTransaction.GetTransactionData(Block, Affected_account_number: Cardinal;
  var TransactionData: TTransactionData): Boolean;
begin
  TransactionData := TTransactionData.Empty;
  TransactionData.transactionSubtype := CT_OpSubtype_DelistAccount;
  TransactionData.TransactionAsString := 'Delist account ' + TAccount.AccountNumberToString(Data.TargetAccount) +
    ' for sale';
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

TTransactionManager.RegisterTransactionPlugin(TListAccountForSaleTransaction, CT_Op_ListAccountForSale);
TTransactionManager.RegisterTransactionPlugin(TDelistAccountTransaction, CT_Op_DelistAccount);

end.
