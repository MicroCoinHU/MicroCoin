unit MicroCoin.Transaction.TransferMoney;

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
  MicroCoin.Common, MicroCoin.Crypto.Keys,
  MicroCoin.Account.Data, UBaseTypes,
  MicroCoin.Account.AccountKey,
  MicroCoin.Transaction.Transaction, MicroCoin.Account.Transaction,
  Sysutils, classes, UCrypto, ULog, MicroCoin.Common.Config, MicroCoin.Transaction.Manager;

type

  TTransferMoneyTransactionStyle = (Transaction, transaction_with_auto_buy_account, buy_account);

  TTransferMoneyTransaction = class(TTransaction)
  protected type
    TTransferMoneyTransactionData = record
      SenderAccount: Cardinal;
      NumberOfTransactions: Cardinal;
      TargetAccount: Cardinal;
      Amount: UInt64;
      Fee: UInt64;
      Payload: TRawBytes;
      PublicKey: TECPublicKey;
      Signature: TECDSA_SIG;
      //
      TransactionStyle: TTransferMoneyTransactionStyle;
      AccountPrice: UInt64;
      SellerAccount: Cardinal;
      NewAccountKey: TAccountKey;
      function CheckIsValid(AAccountTransaction : TAccountTransaction; RErros: string) : boolean;
    end;
  private
    FData: TTransferMoneyTransactionData;
  protected
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
  public
    class function GetTransactionHashForSignature(const trans: TTransferMoneyTransactionData): TRawBytes;
    class function SignTransaction(AKey: TECKeyPair; var RTransactionData: TTransferMoneyTransactionData): Boolean;
    constructor CreateTransaction(ASenderAccount, ANumberOfTransactions, ATargetAccount: Cardinal; AKey: TECKeyPair; AAmount, AFee: UInt64;
      APayload: TRawBytes);
    function GetBuffer(UseProtocolV2: Boolean): TRawBytes; override;
    function ToString: string; override;
    function ApplyTransaction(AccountTransaction: TAccountTransaction; var errors: AnsiString): Boolean; override;
    procedure AffectedAccounts(list: TList); override;
    function GetTransactionData(ABlock: Cardinal; AAffectedAccountNumber: Cardinal; var TransactionData: TTransactionData): Boolean; override;
    //
    property Data: TTransferMoneyTransactionData read FData;
  end;

  TBuyAccountTransaction = class(TTransferMoneyTransaction)
  protected
    procedure InitializeData; override;
    function GetTransactionType: Byte; override;
  public
    constructor CreateBuy(account_number, n_operation, account_to_buy, account_to_pay: Cardinal;
      price, amount, fee: UInt64; new_public_key: TAccountKey; key: TECKeyPair; payload: TRawBytes);
    function GetTransactionData(Block: Cardinal; Affected_account_number: Cardinal;
      var TransactionData: TTransactionData): Boolean; override;
  end;


implementation

uses MicroCoin.Common.Stream, MicroCoin.Node.Node;

const
  CT_TOpTransactionData_NUL: TTransferMoneyTransaction.TTransferMoneyTransactionData = (SenderAccount: 0; NumberOfTransactions: 0; TargetAccount: 0; Amount: 0; Fee: 0;
    Payload: ''; PublicKey: (EC_OpenSSL_NID: 0; x: ''; y: ''); Signature: (r: ''; s: ''); TransactionStyle: Transaction;
    AccountPrice: 0; SellerAccount: 0; NewAccountKey: (EC_OpenSSL_NID: 0; x: ''; y: ''));

procedure TTransferMoneyTransaction.AffectedAccounts(list: TList);
begin
  list.Add(TObject(FData.SenderAccount));
  list.Add(TObject(FData.TargetAccount));
  if (FData.TransactionStyle in [transaction_with_auto_buy_account, buy_account]) then
  begin
    list.Add(TObject(FData.SellerAccount));
  end;
end;

constructor TTransferMoneyTransaction.CreateTransaction(ASenderAccount, ANumberOfTransactions, ATargetAccount: Cardinal; AKey: TECKeyPair;
  AAmount, AFee: UInt64; APayload: TRawBytes);
begin
  inherited Create;
  FData.SenderAccount := ASenderAccount;
  FData.NumberOfTransactions := ANumberOfTransactions;
  FData.TargetAccount := ATargetAccount;
  FData.Amount := AAmount;
  FData.Fee := AFee;
  FData.Payload := APayload;
  if not SignTransaction(AKey, FData) then
  begin
    TLog.NewLog(lterror, Classname, 'Error signing a new Transaction');
    FHasValidSignature := false;
  end
  else
    FHasValidSignature := true;
end;

function TTransferMoneyTransaction.ApplyTransaction(AccountTransaction: TAccountTransaction;
  var errors: AnsiString): Boolean;
var
  xTotalAmount: UInt64;
  xSenderAccount, xTargetAccount, xSellerAccount: TAccount;
  _h: TRawBytes;
  _IsBuyTransaction: Boolean;
begin
  Result := false;
  errors := '';
  if not FData.CheckIsValid(AccountTransaction, errors)
  then exit;

  xSenderAccount := AccountTransaction.Account(FData.SenderAccount);
  xTargetAccount := AccountTransaction.Account(FData.TargetAccount);
  xTotalAmount := FData.Amount + FData.Fee;

  if ((xSenderAccount.NumberOfTransactions + 1) <> FData.NumberOfTransactions) then
  begin
    errors := Format('Invalid n_operation %d (expected %d)', [FData.NumberOfTransactions, xSenderAccount.NumberOfTransactions + 1]);
    exit;
  end;

  if (xSenderAccount.Balance < xTotalAmount) then
  begin
    errors := Format('Insuficient founds %d < (%d + %d = %d)', [xSenderAccount.Balance, FData.Amount, FData.Fee, xTotalAmount]);
    exit;
  end;

  if (xTargetAccount.Balance + FData.Amount > cMaxWalletAmount) then
  begin
    errors := Format('Target cannot accept this transaction due to max amount %d+%d=%d > %d',
      [xTargetAccount.Balance, FData.Amount, xTargetAccount.Balance + FData.Amount, cMaxWalletAmount]);
    exit;
  end;
  // Is locked? Protocol 2 check
  if (xSenderAccount.accountInfo.IsLocked(AccountTransaction.FreezedAccountStorage.BlocksCount)) then
  begin
    errors := 'Sender Account is currently locked';
    exit;
  end;
  // Build 1.4
  if (FData.PublicKey.EC_OpenSSL_NID <> TAccountKey.Empty.EC_OpenSSL_NID) and
    (FData.PublicKey <> xSenderAccount.accountInfo.AccountKey) then
  begin
    errors := Format('Invalid sender public key for account %d. Distinct from SafeBox public key! %s <> %s',
      [FData.SenderAccount, TBaseType.ToHexaString((FData.PublicKey.ToRawString)),
      TBaseType.ToHexaString(xSenderAccount.accountInfo.AccountKey.ToRawString)]);
    exit;
  end;
  // Check signature
  _h := GetTransactionHashForSignature(FData);
  if (not TCrypto.ECDSAVerify(xSenderAccount.accountInfo.AccountKey, _h, FData.Signature)) then
  begin
    errors := 'Invalid sign';
    FHasValidSignature := false;
    Exit;
  end
  else
    FHasValidSignature := true;
  //
  FPrevious_Signer_updated_block := xSenderAccount.UpdatedBlock;
  FPrevious_Destination_updated_block := xTargetAccount.UpdatedBlock;
  // Is buy account ?
  if (FData.TransactionStyle = buy_account) then
  begin
    if (AccountTransaction.FreezedAccountStorage.CurrentProtocol < cPROTOCOL_2) then
    begin
      errors := 'Buy account is not allowed on Protocol 1';
      Exit;
    end;
    xSellerAccount := AccountTransaction.Account(FData.SellerAccount);
    if not(xTargetAccount.accountInfo.IsAccountForSale) then
    begin
      errors := Format('%d is not for sale', [xTargetAccount.AccountNumber]);
      Exit;
    end;
    // Check that seller is the expected seller
    if (xTargetAccount.accountInfo.AccountToPay <> xSellerAccount.AccountNumber) then
    begin
      errors := Format('Seller account %d is not expected account %d',
        [FData.SellerAccount, xTargetAccount.accountInfo.AccountToPay]);
      Exit;
    end;
    if (xTargetAccount.Balance + FData.Amount < xTargetAccount.accountInfo.Price) then
    begin
      errors := Format('Account %d balance (%d) + amount (%d) < price (%d)',
        [xTargetAccount.AccountNumber, xTargetAccount.Balance, FData.Amount, xTargetAccount.accountInfo.Price]);
      Exit;
    end;
    if (FData.AccountPrice <> xTargetAccount.accountInfo.Price) then
    begin
      errors := Format('Signed price (%d) is not the same of account price (%d)',
        [FData.AccountPrice, xTargetAccount.accountInfo.Price]);
      Exit;
    end;
    if not(FData.NewAccountKey.IsValidAccountKey(errors))
    then Exit;
    _IsBuyTransaction := true;
    FPrevious_Seller_updated_block := xSellerAccount.UpdatedBlock;

  end
  else if
  // Is automatic buy account?
    (FData.TransactionStyle = transaction_with_auto_buy_account) or
  // New automatic buy ?
    ((AccountTransaction.FreezedAccountStorage.CurrentProtocol >= cPROTOCOL_2) and
    (FData.TransactionStyle = Transaction) and (xTargetAccount.accountInfo.IsAccountForSaleAcceptingTransactions) and
    (xTargetAccount.Balance + FData.Amount >= xTargetAccount.accountInfo.Price))
    then begin
    if (AccountTransaction.FreezedAccountStorage.CurrentProtocol < cPROTOCOL_2)
    then begin
      errors := 'Tx-Buy account is not allowed on Protocol 1';
      Exit;
    end;
    if not(FData.NewAccountKey.IsValidAccountKey(errors))
    then Exit;
    _IsBuyTransaction := true; // Automatic buy
    // Fill the purchase data
    FData.TransactionStyle := transaction_with_auto_buy_account;
    // Set this data!
    FData.AccountPrice := xTargetAccount.accountInfo.Price;
    FData.SellerAccount := xTargetAccount.accountInfo.AccountToPay;
    xSellerAccount := AccountTransaction.Account(xTargetAccount.accountInfo.AccountToPay);
    FPrevious_Seller_updated_block := xSellerAccount.UpdatedBlock;
    FData.NewAccountKey := xTargetAccount.accountInfo.NewPublicKey;
  end
  else _IsBuyTransaction := false;
  if (_IsBuyTransaction)
  then begin
    if (AccountTransaction.FreezedAccountStorage.CurrentProtocol < cPROTOCOL_2)
    then begin
      errors := 'NOT ALLOWED ON PROTOCOL 1';
      Exit;
    end;
    Result := AccountTransaction.BuyAccount(xSenderAccount.AccountNumber, xTargetAccount.AccountNumber, xSellerAccount.AccountNumber, FData.NumberOfTransactions,
      FData.Amount, xTargetAccount.accountInfo.Price, FData.Fee, FData.NewAccountKey, errors);
  end
  else begin
    Result := AccountTransaction.TransferAmount(FData.SenderAccount, FData.TargetAccount, FData.NumberOfTransactions, FData.Amount,
      FData.Fee, errors);
  end;
end;

class function TTransferMoneyTransaction.SignTransaction(AKey: TECKeyPair;
  var RTransactionData: TTransferMoneyTransactionData): Boolean;
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

function TTransferMoneyTransaction.GetBuffer(UseProtocolV2: Boolean): TRawBytes;
var
  ms: TMemoryStream;
begin
  if UseProtocolV2
  then Result := inherited GetBuffer(UseProtocolV2)
  else
  begin
    ms := TMemoryStream.Create;
    try
      ms.Write(FData.SenderAccount, Sizeof(FData.SenderAccount));
      ms.Write(FData.NumberOfTransactions, Sizeof(FData.NumberOfTransactions));
      ms.Write(FData.TargetAccount, Sizeof(FData.TargetAccount));
      ms.Write(FData.Amount, Sizeof(FData.Amount));
      ms.Write(FData.Fee, Sizeof(FData.Fee));
      if length(FData.Payload) > 0
      then ms.WriteBuffer(FData.Payload[1], length(FData.Payload));
      ms.Write(FData.PublicKey.EC_OpenSSL_NID, Sizeof(FData.PublicKey.EC_OpenSSL_NID));
      if length(FData.PublicKey.x) > 0
      then ms.WriteBuffer(FData.PublicKey.x[1], length(FData.PublicKey.x));
      if length(FData.PublicKey.y) > 0
      then ms.WriteBuffer(FData.PublicKey.y[1], length(FData.PublicKey.y));
      if length(FData.Signature.r) > 0
      then ms.WriteBuffer(FData.Signature.r[1], length(FData.Signature.r));
      if length(FData.Signature.s) > 0
      then ms.WriteBuffer(FData.Signature.s[1], length(FData.Signature.s));
      SetLength(Result, ms.Size);
      ms.Position := 0;
      ms.ReadBuffer(Result[1], ms.Size);
    finally
      ms.Free;
    end;
  end;
end;

function TTransferMoneyTransaction.GetTransactionData(ABlock: Cardinal; AAffectedAccountNumber: Cardinal;
  var TransactionData: TTransactionData): Boolean;
begin
  Result := false;
  TransactionData := TTransactionData.Empty;
  TransactionData.Block := ABlock;
  if self.GetSignerAccount = AAffectedAccountNumber
  then TransactionData.fee := (-1) * Int64(GetFee);
  TransactionData.AffectedAccount := AAffectedAccountNumber;
  TransactionData.transactionType := TransactionType;
  TransactionData.SignerAccount := GetSignerAccount;
  TransactionData.DestAccount := Data.TargetAccount;
  if (Data.TransactionStyle = transaction_with_auto_buy_account)
  then begin
    if Data.SenderAccount = AAffectedAccountNumber
    then begin
      TransactionData.transactionSubtype := CT_OpSubtype_BuyTransactionBuyer;
      TransactionData.TransactionAsString := 'Tx-Out (MCCA ' + TAccount.AccountNumberToString(Data.TargetAccount) +
        ' Purchase) ' + TCurrencyUtils.CurrencyToString(Data.Amount) + ' MCC from ' +
        TAccount.AccountNumberToString(Data.SenderAccount) + ' to ' + TAccount.AccountNumberToString
        (Data.TargetAccount);
      if (Data.SenderAccount = Data.SellerAccount)
      then TransactionData.amount := (Int64(Data.Amount) - (Data.AccountPrice)) * (-1)
      else TransactionData.amount := Int64(Data.Amount) * (-1);
      Result := true;
    end
    else if Data.TargetAccount = AAffectedAccountNumber
    then begin
      TransactionData.transactionSubtype := CT_OpSubtype_BuyTransactionTarget;
      TransactionData.TransactionAsString := 'Tx-In (MCCA ' + TAccount.AccountNumberToString(Data.TargetAccount) +
        ' Purchase) ' + TCurrencyUtils.CurrencyToString(Data.Amount) + ' MCC from ' +
        TAccount.AccountNumberToString(Data.SenderAccount) + ' to ' + TAccount.AccountNumberToString
        (Data.TargetAccount);
      TransactionData.amount := Int64(Data.Amount) - Int64(Data.AccountPrice);
      TransactionData.fee := 0;
      Result := true;
    end
    else if Data.SellerAccount = AAffectedAccountNumber
    then begin
      TransactionData.transactionSubtype := CT_OpSubtype_BuyTransactionSeller;
      TransactionData.TransactionAsString := 'Tx-In Sold account ' + TAccount.AccountNumberToString(Data.TargetAccount) +
        ' price ' + TCurrencyUtils.CurrencyToString(Data.AccountPrice) + ' MCC';
      TransactionData.amount := Data.AccountPrice;
      TransactionData.fee := 0;
      Result := true;
    end else Exit;
  end else begin
    if Data.SenderAccount = AAffectedAccountNumber
    then begin
      TransactionData.transactionSubtype := CT_OpSubtype_TransactionSender;
      TransactionData.TransactionAsString := 'Tx-Out ' + TCurrencyUtils.CurrencyToString(Data.Amount) + ' MCC from ' +
          TAccount.AccountNumberToString(Data.SenderAccount) + ' to ' + TAccount.AccountNumberToString(Data.TargetAccount);
      TransactionData.amount := Int64(Data.Amount) * (-1);
      Result := true;
    end
    else if Data.TargetAccount = AAffectedAccountNumber
         then begin
           TransactionData.transactionSubtype := CT_OpSubtype_TransactionReceiver;
           TransactionData.TransactionAsString := 'Tx-In ' + TCurrencyUtils.CurrencyToString(Data.Amount) + ' MCC from ' +
             TAccount.AccountNumberToString(Data.SenderAccount) + ' to ' + TAccount.AccountNumberToString
             (Data.TargetAccount);
           TransactionData.amount := Data.Amount;
           TransactionData.fee := 0;
           Result := true;
         end else Exit;
  end;
  TransactionData.OriginalPayload := GetPayload;
  if TCrypto.IsHumanReadable(TransactionData.OriginalPayload)
  then TransactionData.PrintablePayload := TransactionData.OriginalPayload
  else TransactionData.PrintablePayload := TBaseType.ToHexaString(TransactionData.OriginalPayload);
  TransactionData.OperationHash := TransactionHash(ABlock);
  if (ABlock < cProtocol_Upgrade_v2_MinBlock)
  then TransactionData.OperationHash_OLD := TransactionHash_OLD(ABlock);
  TransactionData.valid := true;
end;

class function TTransferMoneyTransaction.GetTransactionHashForSignature(const trans: TTransferMoneyTransactionData)
  : TRawBytes;
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    ms.Write(trans.SenderAccount, Sizeof(trans.SenderAccount));
    ms.Write(trans.NumberOfTransactions, Sizeof(trans.NumberOfTransactions));
    ms.Write(trans.TargetAccount, Sizeof(trans.TargetAccount));
    ms.Write(trans.Amount, Sizeof(trans.Amount));
    ms.Write(trans.Fee, Sizeof(trans.Fee));
    if length(trans.Payload) > 0
    then ms.WriteBuffer(trans.Payload[1], length(trans.Payload));
    ms.Write(trans.PublicKey.EC_OpenSSL_NID, Sizeof(trans.PublicKey.EC_OpenSSL_NID));
    if length(trans.PublicKey.x) > 0
    then ms.WriteBuffer(trans.PublicKey.x[1], length(trans.PublicKey.x));
    if length(trans.PublicKey.y) > 0
    then ms.WriteBuffer(trans.PublicKey.y[1], length(trans.PublicKey.y));
    if trans.TransactionStyle = buy_account
    then begin
      ms.Write(trans.AccountPrice, Sizeof(trans.AccountPrice));
      ms.Write(trans.SellerAccount, Sizeof(trans.SellerAccount));
      ms.Write(trans.NewAccountKey.EC_OpenSSL_NID, Sizeof(trans.NewAccountKey.EC_OpenSSL_NID));
      if length(trans.NewAccountKey.x) > 0
      then ms.WriteBuffer(trans.NewAccountKey.x[1], length(trans.NewAccountKey.x));
      if length(trans.NewAccountKey.y) > 0
      then ms.WriteBuffer(trans.NewAccountKey.y[1], length(trans.NewAccountKey.y));
    end;
    SetLength(Result, ms.Size);
    ms.Position := 0;
    ms.ReadBuffer(Result[1], ms.Size);
  finally
    ms.Free;
  end;
end;

procedure TTransferMoneyTransaction.InitializeData;
begin
  inherited;
  FData := CT_TOpTransactionData_NUL;
end;

function TTransferMoneyTransaction.LoadFromStream(Stream: TStream; LoadExtendedData: Boolean): Boolean;
var
  b: Byte;
begin
  Result := false;
  if Stream.Size - Stream.Position < 28
  then Exit; // Invalid stream
  Stream.Read(FData.SenderAccount, Sizeof(FData.SenderAccount));
  Stream.Read(FData.NumberOfTransactions, Sizeof(FData.NumberOfTransactions));
  Stream.Read(FData.TargetAccount, Sizeof(FData.TargetAccount));
  Stream.Read(FData.Amount, Sizeof(FData.Amount));
  Stream.Read(FData.Fee, Sizeof(FData.Fee));
  if Stream.ReadAnsiString(FData.Payload) < 0
  then exit;

  if Stream.Read(FData.PublicKey.EC_OpenSSL_NID, Sizeof(FData.PublicKey.EC_OpenSSL_NID)) < 0
  then exit;

  if Stream.ReadAnsiString(FData.PublicKey.x) < 0
  then exit;

  if Stream.ReadAnsiString(FData.PublicKey.y) < 0
  then exit;

  if ((LoadExtendedData) or (self is TBuyAccountTransaction)) then
  begin
    if Stream.Read(b, 1) <> 1
    then Exit;
    case b of
      0: FData.TransactionStyle := Transaction;
      1: FData.TransactionStyle := transaction_with_auto_buy_account;
      2: begin
           FData.TransactionStyle := buy_account;
           if (not(self is TBuyAccountTransaction))
           then exit;
         end
      else Exit;
    end;
    if (FData.TransactionStyle in [transaction_with_auto_buy_account, buy_account]) then
    begin
      Stream.Read(FData.AccountPrice, Sizeof(FData.AccountPrice));
      Stream.Read(FData.SellerAccount, Sizeof(FData.SellerAccount));
      if Stream.Read(FData.NewAccountKey.EC_OpenSSL_NID, Sizeof(FData.NewAccountKey.EC_OpenSSL_NID)) < 0
      then Exit;
      if Stream.ReadAnsiString(FData.NewAccountKey.x) < 0
      then Exit;
      if Stream.ReadAnsiString(FData.NewAccountKey.y) < 0
      then Exit;
    end;
  end;
  if Stream.ReadAnsiString(FData.Signature.r) < 0
  then Exit;
  if Stream.ReadAnsiString(FData.Signature.s) < 0
  then Exit;
  Result := true;
end;

function TTransferMoneyTransaction.GetAmount: Int64;
begin
  Result := FData.Amount;
end;

function TTransferMoneyTransaction.GetFee: UInt64;
begin
  Result := FData.Fee;
end;

function TTransferMoneyTransaction.GetPayload: TRawBytes;
begin
  Result := FData.Payload;
end;

function TTransferMoneyTransaction.GetTransactionType: Byte;
begin
  Result := CT_Op_Transaction;
end;

function TTransferMoneyTransaction.SaveToStream(Stream: TStream; SaveExtendedData: Boolean): Boolean;
var
  b: Byte;
begin
  Stream.Write(FData.SenderAccount, Sizeof(FData.SenderAccount));
  Stream.Write(FData.NumberOfTransactions, Sizeof(FData.NumberOfTransactions));
  Stream.Write(FData.TargetAccount, Sizeof(FData.TargetAccount));
  Stream.Write(FData.Amount, Sizeof(FData.Amount));
  Stream.Write(FData.Fee, Sizeof(FData.Fee));
  Stream.WriteAnsiString(FData.Payload);
  Stream.Write(FData.PublicKey.EC_OpenSSL_NID, Sizeof(FData.PublicKey.EC_OpenSSL_NID));
  Stream.WriteAnsiString(FData.PublicKey.x);
  Stream.WriteAnsiString(FData.PublicKey.y);
  if ((SaveExtendedData) or (self is TBuyAccountTransaction))
  then begin
    case FData.TransactionStyle of
      Transaction: b := 0;
      transaction_with_auto_buy_account: b := 1;
      buy_account: b := 2;
    else
      raise Exception.Create('ERROR DEV 20170424-1');
    end;
    Stream.Write(b, 1);
    if (FData.TransactionStyle in [transaction_with_auto_buy_account, buy_account])
    then begin
      Stream.Write(FData.AccountPrice, Sizeof(FData.AccountPrice));
      Stream.Write(FData.SellerAccount, Sizeof(FData.SellerAccount));
      Stream.Write(FData.NewAccountKey.EC_OpenSSL_NID, Sizeof(FData.NewAccountKey.EC_OpenSSL_NID));
      Stream.WriteAnsiString(FData.NewAccountKey.x);
      Stream.WriteAnsiString(FData.NewAccountKey.y);
    end;
  end;
  Stream.WriteAnsiString(FData.Signature.r);
  Stream.WriteAnsiString(FData.Signature.s);
  Result := true;
end;

function TTransferMoneyTransaction.GetSignerAccount: Cardinal;
begin
  Result := FData.SenderAccount;
end;

function TTransferMoneyTransaction.GetDestinationAccount: Int64;
begin
  Result := FData.TargetAccount;
end;

function TTransferMoneyTransaction.GetSellerAccount: Int64;
begin
  case FData.TransactionStyle of
    transaction_with_auto_buy_account,
    buy_account: Result := FData.SellerAccount;
  else
    Result := inherited GetSellerAccount;
  end;
end;

function TTransferMoneyTransaction.GetNumberOfTransactions: Cardinal;
begin
  Result := FData.NumberOfTransactions;
end;

function TTransferMoneyTransaction.ToString: string;
begin
  case FData.TransactionStyle of
    Transaction:
      Result := Format('Transaction from %s to %s. Amount: %s Fee: %s',
        [TAccount.AccountNumberToString(FData.SenderAccount), TAccount.AccountNumberToString(FData.TargetAccount),
        TCurrencyUtils.CurrencyToString(FData.Amount), TCurrencyUtils.CurrencyToString(FData.Fee)]);
    transaction_with_auto_buy_account:
      Result := Format('Transaction/Buy account %s by %s paying %s to %s amount:%s fee:%s',
        [TAccount.AccountNumberToString(FData.TargetAccount), TAccount.AccountNumberToString(FData.SenderAccount),
        TCurrencyUtils.CurrencyToString(FData.AccountPrice), TAccount.AccountNumberToString(FData.SellerAccount),
        TCurrencyUtils.CurrencyToString(FData.Amount), TCurrencyUtils.CurrencyToString(FData.Fee)]);
    buy_account:
      Result := Format('Buy account %s by %s paying %s to %s amount:%s fee:%s',
        [TAccount.AccountNumberToString(FData.TargetAccount), TAccount.AccountNumberToString(FData.SenderAccount),
        TCurrencyUtils.CurrencyToString(FData.AccountPrice), TAccount.AccountNumberToString(FData.SellerAccount),
        TCurrencyUtils.CurrencyToString(FData.Amount), TCurrencyUtils.CurrencyToString(FData.Fee)]);
  else
    raise Exception.Create('ERROR DEV 20170424-2');
  end;
end;

constructor TBuyAccountTransaction.CreateBuy(account_number, n_operation, account_to_buy, account_to_pay: Cardinal;
  price, amount, fee: UInt64; new_public_key: TAccountKey; key: TECKeyPair; payload: TRawBytes);
begin
  inherited Create;
  FData.SenderAccount := account_number;
  FData.NumberOfTransactions := n_operation;
  FData.TargetAccount := account_to_buy;
  FData.Amount := amount;
  FData.Fee := fee;
  FData.Payload := payload;
  FData.TransactionStyle := buy_account;
  FData.AccountPrice := price;
  FData.SellerAccount := account_to_pay;
  FData.NewAccountKey := new_public_key;
  if not SignTransaction(key, FData)
  then begin
    TLog.NewLog(lterror, Classname, 'Error signing a new Buy operation');
    FHasValidSignature := false;
  end else FHasValidSignature := true;
end;

procedure TBuyAccountTransaction.InitializeData;
begin
  inherited;
  FData.TransactionStyle := buy_account;
end;

function TBuyAccountTransaction.GetTransactionType : Byte;
begin
  Result := CT_Op_BuyAccount;
end;

function TBuyAccountTransaction.GetTransactionData(Block, Affected_account_number: Cardinal;
  var TransactionData: TTransactionData): Boolean;
begin
  TransactionData := TTransactionData.Empty;
  TransactionData.DestAccount := Data.TargetAccount;
  if Data.SenderAccount = Affected_account_number
  then begin
    TransactionData.transactionSubtype := CT_OpSubtype_BuyAccountBuyer;
    TransactionData.TransactionAsString := 'Buy account ' + TAccount.AccountNumberToString(Data.TargetAccount) + ' for ' +
      TCurrencyUtils.CurrencyToString(Data.AccountPrice) + ' MCC';
    TransactionData.amount := Int64(Data.Amount) * (-1);
    Result := true;
  end else if Data.TargetAccount = Affected_account_number
  then begin
    TransactionData.transactionSubtype := CT_OpSubtype_BuyAccountTarget;
    TransactionData.TransactionAsString := 'Purchased account ' + TAccount.AccountNumberToString(Data.TargetAccount) +
      ' by ' + TAccount.AccountNumberToString(Data.SenderAccount) + ' for ' +
      TCurrencyUtils.CurrencyToString(Data.AccountPrice) + ' MCC';
    TransactionData.amount := Int64(Data.Amount) - Int64(Data.AccountPrice);
    TransactionData.fee := 0;
    Result := true;
  end
  else if Data.SellerAccount = Affected_account_number
  then begin
    TransactionData.transactionSubtype := CT_OpSubtype_BuyAccountSeller;
    TransactionData.TransactionAsString := 'Sold account ' + TAccount.AccountNumberToString(Data.TargetAccount) + ' by ' +
      TAccount.AccountNumberToString(Data.SenderAccount) + ' for ' +
      TCurrencyUtils.CurrencyToString(Data.AccountPrice) + ' MCC';
    TransactionData.amount := Data.AccountPrice;
    TransactionData.fee := 0;
    Result := true;
  end else Exit(false);
  TransactionData.OriginalPayload := GetPayload;
  if TCrypto.IsHumanReadable(TransactionData.OriginalPayload)
  then TransactionData.PrintablePayload := TransactionData.OriginalPayload
  else TransactionData.PrintablePayload := TBaseType.ToHexaString(TransactionData.OriginalPayload);
  TransactionData.OperationHash := TransactionHash(Block);
  if (Block < cProtocol_Upgrade_v2_MinBlock)
  then TransactionData.OperationHash_OLD := TransactionHash_OLD(Block);
  TransactionData.valid := true;
end;

{ TTransferMoneyTransaction.TTransferMoneyTransactionData }

function TTransferMoneyTransaction.TTransferMoneyTransactionData.CheckIsValid(
  AAccountTransaction: TAccountTransaction; RErros: string): boolean;
begin

  Result := false;

  if (SenderAccount >= AAccountTransaction.FreezedAccountStorage.AccountsCount)
  then begin
    RErros := Format('Invalid sender %d', [SenderAccount]);
    Exit;
  end;
  if (TargetAccount >= AAccountTransaction.FreezedAccountStorage.AccountsCount)
  then begin
    RErros := Format('Invalid target %d', [TargetAccount]);
    Exit;
  end;
  if (SenderAccount = TargetAccount)
  then begin
    RErros := Format('Sender=Target %d', [SenderAccount]);
    Exit;
  end;
  if TAccount.IsAccountBlockedByProtocol(SenderAccount, AAccountTransaction.FreezedAccountStorage.BlocksCount)
  then begin
    RErros := Format('sender (%d) is blocked for protocol', [SenderAccount]);
    Exit;
  end;
  if TAccount.IsAccountBlockedByProtocol(TargetAccount, AAccountTransaction.FreezedAccountStorage.BlocksCount)
  then begin
    RErros := Format('target (%d) is blocked for protocol', [TargetAccount]);
    Exit;
  end;
  if (Amount <= 0) or (Amount > cMaxTransactionAmount)
  then begin
    RErros := Format('Invalid amount %d (0 or max: %d)', [Amount, cMaxTransactionAmount]);
    Exit;
  end;
  if (Fee < 0) or (Fee > cMaxTransactionFee)
  then begin
    RErros := Format('Invalid fee %d (max %d)', [Fee, cMaxTransactionFee]);
    Exit;
  end;
  if (length(Payload) > cMaxPayloadSize)
  then begin
    RErros := 'Invalid Payload size:' + IntToStr(length(Payload)) + ' (Max: ' + inttostr(cMaxPayloadSize) + ')';
    if (AAccountTransaction.FreezedAccountStorage.CurrentProtocol >= cPROTOCOL_2)
    then begin
      Exit; // BUG from protocol 1
    end;
  end;

  Result := true;

end;

initialization

TTransactionManager.RegisterTransactionPlugin(TTransferMoneyTransaction, CT_Op_Transaction);
TTransactionManager.RegisterTransactionPlugin(TBuyAccountTransaction, CT_Op_BuyAccount);

end.
