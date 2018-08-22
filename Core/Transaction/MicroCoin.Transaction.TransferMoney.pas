unit MicroCoin.Transaction.TransferMoney;

{
  This unit contains code from PascalCoin:

  Copyright (c) Albert Molina 2016 - 2018 original code from PascalCoin https://pascalcoin.org/

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

}

interface

uses MicroCoin.Transaction.Base,
  MicroCoin.Account.AccountKey,
  MicroCoin.Transaction.Transaction,
  Sysutils, classes, UAccounts, UCrypto, ULog, UConst, MicroCoin.Transaction.Manager;

type

  TTransferMoneyTransactionStyle = (Transaction, transaction_with_auto_buy_account, buy_account);

  TTransferMoneyTransactionData = record
    sender: Cardinal;
    n_operation: Cardinal;
    target: Cardinal;
    amount: UInt64;
    fee: UInt64;
    payload: TRawBytes;
    public_key: TECDSA_Public;
    sign: TECDSA_SIG;
    // Protocol 2
    // Next values will only be filled after this operation is executed
    opTransactionStyle: TTransferMoneyTransactionStyle;
    AccountPrice: UInt64;
    SellerAccount: Cardinal;
    new_accountkey: TAccountKey;
  end;

  TTransferMoneyTransaction = class(TTransaction)
  private
    FData: TTransferMoneyTransactionData;
  protected
    procedure InitializeData; override;
    function SaveToStream(Stream: TStream; SaveExtendedData: Boolean)
      : Boolean; override;
    function LoadFromStream(Stream: TStream; LoadExtendedData: Boolean)
      : Boolean; override;
  public
    function GetBufferForOpHash(UseProtocolV2: Boolean): TRawBytes; override;
    function ApplyTransaction(AccountTransaction: TPCSafeBoxTransaction;
      var errors: AnsiString): Boolean; override;
    procedure AffectedAccounts(list: TList); override;
    //
    class function GetTransactionHashToSign(const trans: TTransferMoneyTransactionData)
      : TRawBytes;
    class function DoSignOperation(key: TECPrivateKey;
      var trans: TTransferMoneyTransactionData): Boolean;
    function GetOpType: Byte; override;
    function GetAmount: Int64; override;
    function GetFee: UInt64; override;
    function GetPayload: TRawBytes; override;
    function GetSignerAccount: Cardinal; override;
    function GetDestinationAccount: Int64; override;
    function GetSellerAccount: Int64; override;
    function GetNumberOfTransactions: Cardinal; override;
    function GetTransactionData(Block: Cardinal;
      Affected_account_number: Cardinal; var TransactionData: TTransactionData)
      : Boolean; override;
    property Data: TTransferMoneyTransactionData read FData;
    constructor CreateTransaction(sender, n_operation, target: Cardinal;
      key: TECPrivateKey; amount, fee: UInt64; payload: TRawBytes);
    function ToString: string; override;
  end;

  TBuyAccountTransaction = class(TTransferMoneyTransaction)
  protected
    procedure InitializeData; override;
  public
    function GetOpType: Byte; override;
    constructor CreateBuy(account_number, n_operation, account_to_buy,
      account_to_pay: Cardinal; price, amount, fee: UInt64;
      new_public_key: TAccountKey; key: TECPrivateKey; payload: TRawBytes);
    function GetTransactionData(Block: Cardinal;
      Affected_account_number: Cardinal; var TransactionData: TTransactionData)
      : Boolean; override;
  end;

const
  CT_TOpTransactionData_NUL: TTransferMoneyTransactionData = (sender: 0; n_operation: 0; target: 0; amount: 0; fee: 0; payload: ''; public_key: (EC_OpenSSL_NID: 0; x: ''; y: ''); sign: (r: ''; s: '');
    opTransactionStyle: Transaction; AccountPrice: 0; SellerAccount: 0; new_accountkey: (EC_OpenSSL_NID: 0; x: ''; y: ''));

implementation

procedure TTransferMoneyTransaction.AffectedAccounts(list: TList);
begin
  list.Add(TObject(FData.sender));
  list.Add(TObject(FData.target));
  if (FData.opTransactionStyle in [transaction_with_auto_buy_account,
    buy_account]) then
  begin
    list.Add(TObject(FData.SellerAccount));
  end;
end;

constructor TTransferMoneyTransaction.CreateTransaction(sender, n_operation,
  target: Cardinal; key: TECPrivateKey; amount, fee: UInt64;
  payload: TRawBytes);
begin
  inherited Create;
  FData.sender := sender;
  FData.n_operation := n_operation;
  FData.target := target;
  FData.amount := amount;
  FData.fee := fee;
  FData.payload := payload;
  // V2: No need to store public key because it's at safebox. Saving at least 64 bytes!
  // FData.public_key := key.PublicKey;
  if not DoSignOperation(key, FData) then
  begin
    TLog.NewLog(lterror, Classname, 'Error signing a new Transaction');
    FHasValidSignature := false;
  end
  else
    FHasValidSignature := true;
end;

function TTransferMoneyTransaction.ApplyTransaction(AccountTransaction
  : TPCSafeBoxTransaction; var errors: AnsiString): Boolean;
var
  s_new, t_new: Int64;
  totalamount: Cardinal;
  sender, target, seller: TAccount;
  _h: TRawBytes;
  _IsBuyTransaction: Boolean;
begin
  Result := false;
  errors := '';
  //
  if (FData.sender >= AccountTransaction.FreezedSafeBox.AccountsCount) then
  begin
    errors := Format('Invalid sender %d', [FData.sender]);
    Exit;
  end;
  if (FData.target >= AccountTransaction.FreezedSafeBox.AccountsCount) then
  begin
    errors := Format('Invalid target %d', [FData.target]);
    Exit;
  end;
  if (FData.sender = FData.target) then
  begin
    errors := Format('Sender=Target %d', [FData.sender]);
    Exit;
  end;
  if TAccountComp.IsAccountBlockedByProtocol(FData.sender,
    AccountTransaction.FreezedSafeBox.BlocksCount) then
  begin
    errors := Format('sender (%d) is blocked for protocol', [FData.sender]);
    Exit;
  end;
  if TAccountComp.IsAccountBlockedByProtocol(FData.target,
    AccountTransaction.FreezedSafeBox.BlocksCount) then
  begin
    errors := Format('target (%d) is blocked for protocol', [FData.target]);
    Exit;
  end;
  if (FData.amount <= 0) or (FData.amount > CT_MaxTransactionAmount) then
  begin
    errors := Format('Invalid amount %d (0 or max: %d)',
      [FData.amount, CT_MaxTransactionAmount]);
    Exit;
  end;
  if (FData.fee < 0) or (FData.fee > CT_MaxTransactionFee) then
  begin
    errors := Format('Invalid fee %d (max %d)',
      [FData.fee, CT_MaxTransactionFee]);
    Exit;
  end;
  if (length(FData.payload) > CT_MaxPayloadSize) then
  begin
    errors := 'Invalid Payload size:' + inttostr(length(FData.payload)) +
      ' (Max: ' + inttostr(CT_MaxPayloadSize) + ')';
    if (AccountTransaction.FreezedSafeBox.CurrentProtocol >= CT_PROTOCOL_2) then
    begin
      Exit; // BUG from protocol 1
    end;
  end;
  sender := AccountTransaction.Account(FData.sender);
  target := AccountTransaction.Account(FData.target);
  if ((sender.n_operation + 1) <> FData.n_operation) then
  begin
    errors := Format('Invalid n_operation %d (expected %d)',
      [FData.n_operation, sender.n_operation + 1]);
    Exit;
  end;
  totalamount := FData.amount + FData.fee;
  if (sender.balance < totalamount) then
  begin
    errors := Format('Insuficient founds %d < (%d + %d = %d)',
      [sender.balance, FData.amount, FData.fee, totalamount]);
    Exit;
  end;
  if (target.balance + FData.amount > CT_MaxWalletAmount) then
  begin
    errors := Format
      ('Target cannot accept this transaction due to max amount %d+%d=%d > %d',
      [target.balance, FData.amount, target.balance + FData.amount,
      CT_MaxWalletAmount]);
    Exit;
  end;
  // Is locked? Protocol 2 check
  if (sender.accountInfo.IsLocked(AccountTransaction.FreezedSafeBox.BlocksCount)) then
  begin
    errors := 'Sender Account is currently locked';
    Exit;
  end;
  // Build 1.4
  if (FData.public_key.EC_OpenSSL_NID <> CT_TECDSA_Public_Nul.EC_OpenSSL_NID)
    and (not TAccountKey.EqualAccountKeys(FData.public_key,
    sender.accountInfo.AccountKey)) then
  begin
    errors := Format
      ('Invalid sender public key for account %d. Distinct from SafeBox public key! %s <> %s',
      [FData.sender, TCrypto.ToHexaString(
      (FData.public_key.ToRawString)),
      TCrypto.ToHexaString(sender.accountInfo.AccountKey.ToRawString)]);
    Exit;
  end;
  // Check signature
  _h := GetTransactionHashToSign(FData);
  if (not TCrypto.ECDSAVerify(sender.accountInfo.AccountKey, _h, FData.sign))
  then
  begin
    errors := 'Invalid sign';
    FHasValidSignature := false;
    Exit;
  end
  else
    FHasValidSignature := true;
  //
  FPrevious_Signer_updated_block := sender.updated_block;
  FPrevious_Destination_updated_block := target.updated_block;
  // Is buy account ?
  if (FData.opTransactionStyle = buy_account) then
  begin
    if (AccountTransaction.FreezedSafeBox.CurrentProtocol < CT_PROTOCOL_2) then
    begin
      errors := 'Buy account is not allowed on Protocol 1';
      Exit;
    end;
    seller := AccountTransaction.Account(FData.SellerAccount);
    if not(target.accountInfo.IsAccountForSale) then
    begin
      errors := Format('%d is not for sale', [target.Account]);
      Exit;
    end;
    // Check that seller is the expected seller
    if (target.accountInfo.account_to_pay <> seller.Account) then
    begin
      errors := Format('Seller account %d is not expected account %d',
        [FData.SellerAccount, target.accountInfo.account_to_pay]);
      Exit;
    end;
    if (target.balance + FData.amount < target.accountInfo.price) then
    begin
      errors := Format('Account %d balance (%d) + amount (%d) < price (%d)',
        [target.Account, target.balance, FData.amount,
        target.accountInfo.price]);
      Exit;
    end;
    if (FData.AccountPrice <> target.accountInfo.price) then
    begin
      errors := Format
        ('Signed price (%d) is not the same of account price (%d)',
        [FData.AccountPrice, target.accountInfo.price]);
      Exit;
    end;
    if not(FData.new_accountkey.IsValidAccountKey(errors)) then
      Exit; // BUG 20171511
    _IsBuyTransaction := true;
    FPrevious_Seller_updated_block := seller.updated_block;
  end
  else if
  // Is automatic buy account?
    (FData.opTransactionStyle = transaction_with_auto_buy_account) or
  // New automatic buy ?
    ((AccountTransaction.FreezedSafeBox.CurrentProtocol >= CT_PROTOCOL_2) and
    (FData.opTransactionStyle = Transaction) and
    (target.accountInfo.IsAccountForSaleAcceptingTransactions) and
    (target.balance + FData.amount >= target.accountInfo.price)) then
  begin
    if (AccountTransaction.FreezedSafeBox.CurrentProtocol < CT_PROTOCOL_2) then
    begin
      errors := 'Tx-Buy account is not allowed on Protocol 1';
      Exit;
    end;
    if not(FData.new_accountkey.IsValidAccountKey(errors)) then
      Exit; // BUG 20171511
    _IsBuyTransaction := true; // Automatic buy
    // Fill the purchase data
    FData.opTransactionStyle := transaction_with_auto_buy_account;
    // Set this data!
    FData.AccountPrice := target.accountInfo.price;
    FData.SellerAccount := target.accountInfo.account_to_pay;
    seller := AccountTransaction.Account(target.accountInfo.account_to_pay);
    FPrevious_Seller_updated_block := seller.updated_block;
    FData.new_accountkey := target.accountInfo.new_publicKey;
  end
  else
  begin
    _IsBuyTransaction := false;
  end;
  if (_IsBuyTransaction) then
  begin
    if (AccountTransaction.FreezedSafeBox.CurrentProtocol < CT_PROTOCOL_2) then
    begin
      errors := 'NOT ALLOWED ON PROTOCOL 1';
      Exit;
    end;
    Result := AccountTransaction.BuyAccount(sender.Account, target.Account,
      seller.Account, FData.n_operation, FData.amount, target.accountInfo.price,
      FData.fee, FData.new_accountkey, errors);
  end
  else
  begin
    Result := AccountTransaction.TransferAmount(FData.sender, FData.target,
      FData.n_operation, FData.amount, FData.fee, errors);
  end;
end;

class function TTransferMoneyTransaction.DoSignOperation(key: TECPrivateKey;
  var trans: TTransferMoneyTransactionData): Boolean;
var
  s: AnsiString;
  _sign: TECDSA_SIG;
begin
  if not Assigned(key.PrivateKey) then
  begin
    Result := false;
    trans.sign.r := '';
    trans.sign.s := '';
    Exit;
  end;
  s := GetTransactionHashToSign(trans);
  try
    _sign := TCrypto.ECDSASign(key, s);
    trans.sign := _sign;
    Result := true;
  except
    trans.sign.r := '';
    trans.sign.s := '';
    Result := false;
  end;
  SetLength(s, 0);
end;

function TTransferMoneyTransaction.GetBufferForOpHash(UseProtocolV2: Boolean)
  : TRawBytes;
var
  ms: TMemoryStream;
begin
  if UseProtocolV2 then
    Result := inherited GetBufferForOpHash(UseProtocolV2)
  else
  begin
    ms := TMemoryStream.Create;
    try
      ms.Write(FData.sender, Sizeof(FData.sender));
      ms.Write(FData.n_operation, Sizeof(FData.n_operation));
      ms.Write(FData.target, Sizeof(FData.target));
      ms.Write(FData.amount, Sizeof(FData.amount));
      ms.Write(FData.fee, Sizeof(FData.fee));
      if length(FData.payload) > 0 then
        ms.WriteBuffer(FData.payload[1], length(FData.payload));
      ms.Write(FData.public_key.EC_OpenSSL_NID,
        Sizeof(FData.public_key.EC_OpenSSL_NID));
      if length(FData.public_key.x) > 0 then
        ms.WriteBuffer(FData.public_key.x[1], length(FData.public_key.x));
      if length(FData.public_key.y) > 0 then
        ms.WriteBuffer(FData.public_key.y[1], length(FData.public_key.y));
      if length(FData.sign.r) > 0 then
        ms.WriteBuffer(FData.sign.r[1], length(FData.sign.r));
      if length(FData.sign.s) > 0 then
        ms.WriteBuffer(FData.sign.s[1], length(FData.sign.s));
      SetLength(Result, ms.Size);
      ms.Position := 0;
      ms.ReadBuffer(Result[1], ms.Size);
    finally
      ms.Free;
    end;
  end;
end;

function TTransferMoneyTransaction.GetTransactionData(Block: Cardinal;
  Affected_account_number: Cardinal;
  var TransactionData: TTransactionData): Boolean;
var
  spayload: AnsiString;
  s: AnsiString;
begin
  TransactionData := TTransactionData.Empty;
  TransactionData.Block := Block;
  if self.GetSignerAccount = Affected_account_number then
  begin
    TransactionData.fee := (-1) * Int64(GetFee);
  end;
  TransactionData.AffectedAccount := Affected_account_number;
  TransactionData.OpType := OpType;
  TransactionData.SignerAccount := GetSignerAccount;
  Result := false;
  TransactionData.DestAccount := Data.target;
  if (Data.opTransactionStyle = transaction_with_auto_buy_account) then
  begin
    if Data.sender = Affected_account_number then
    begin
      TransactionData.OpSubtype := CT_OpSubtype_BuyTransactionBuyer;
      TransactionData.OperationTxt := 'Tx-Out (MCCA ' +
        TAccountComp.AccountNumberToAccountTxtNumber(Data.target) +
        ' Purchase) ' + TAccountComp.FormatMoney(TTransferMoneyTransaction(self)
        .Data.amount) + ' MCC from ' +
        TAccountComp.AccountNumberToAccountTxtNumber(Data.sender) + ' to ' +
        TAccountComp.AccountNumberToAccountTxtNumber(Data.target);
      if (Data.sender = Data.SellerAccount) then
      begin
        // Valid calc when sender is the same than seller
        TransactionData.amount :=
          (Int64(Data.amount) - (Data.AccountPrice)) * (-1);
      end
      else
        TransactionData.amount := Int64(Data.amount) * (-1);
      Result := true;
    end
    else if Data.target = Affected_account_number then
    begin
      TransactionData.OpSubtype := CT_OpSubtype_BuyTransactionTarget;
      TransactionData.OperationTxt := 'Tx-In (MCCA ' +
        TAccountComp.AccountNumberToAccountTxtNumber(Data.target) +
        ' Purchase) ' + TAccountComp.FormatMoney(Data.amount) + ' MCC from ' +
        TAccountComp.AccountNumberToAccountTxtNumber(Data.sender) + ' to ' +
        TAccountComp.AccountNumberToAccountTxtNumber(Data.target);
      TransactionData.amount := Int64(Data.amount) - Int64(Data.AccountPrice);
      TransactionData.fee := 0;
      Result := true;
    end
    else if Data.SellerAccount = Affected_account_number then
    begin
      TransactionData.OpSubtype := CT_OpSubtype_BuyTransactionSeller;
      TransactionData.OperationTxt := 'Tx-In Sold account ' +
        TAccountComp.AccountNumberToAccountTxtNumber(Data.target) + ' price ' +
        TAccountComp.FormatMoney(Data.AccountPrice) + ' MCC';
      TransactionData.amount := Data.AccountPrice;
      TransactionData.fee := 0;
      Result := true;
    end
    else
      Exit;
  end
  else
  begin
    if Data.sender = Affected_account_number then
    begin
      TransactionData.OpSubtype := CT_OpSubtype_TransactionSender;
      TransactionData.OperationTxt := 'Tx-Out ' + TAccountComp.FormatMoney
        (Data.amount) + ' MCC from ' +
        TAccountComp.AccountNumberToAccountTxtNumber(Data.sender) + ' to ' +
        TAccountComp.AccountNumberToAccountTxtNumber(Data.target);
      TransactionData.amount := Int64(Data.amount) * (-1);
      Result := true;
    end
    else if Data.target = Affected_account_number then
    begin
      TransactionData.OpSubtype := CT_OpSubtype_TransactionReceiver;
      TransactionData.OperationTxt := 'Tx-In ' + TAccountComp.FormatMoney
        (Data.amount) + ' MCC from ' +
        TAccountComp.AccountNumberToAccountTxtNumber(Data.sender) + ' to ' +
        TAccountComp.AccountNumberToAccountTxtNumber(Data.target);
      TransactionData.amount := Data.amount;
      TransactionData.fee := 0;
      Result := true;
    end
    else
      Exit;
  end;
  TransactionData.OriginalPayload := GetPayload;
  if TCrypto.IsHumanReadable(TransactionData.OriginalPayload) then
    TransactionData.PrintablePayload := TransactionData.OriginalPayload
  else
    TransactionData.PrintablePayload :=
      TCrypto.ToHexaString(TransactionData.OriginalPayload);
  TransactionData.OperationHash := TransactionHash(Block);
  if (Block < CT_Protocol_Upgrade_v2_MinBlock) then
  begin
    TransactionData.OperationHash_OLD := TransactionHash_OLD(Block);
  end;
  TransactionData.valid := true;
end;

class function TTransferMoneyTransaction.GetTransactionHashToSign
  (const trans: TTransferMoneyTransactionData): TRawBytes;
var
  ms: TMemoryStream;
  s: string;
begin
  ms := TMemoryStream.Create;
  try
    ms.Write(trans.sender, Sizeof(trans.sender));
    ms.Write(trans.n_operation, Sizeof(trans.n_operation));
    ms.Write(trans.target, Sizeof(trans.target));
    ms.Write(trans.amount, Sizeof(trans.amount));
    ms.Write(trans.fee, Sizeof(trans.fee));
    if length(trans.payload) > 0 then
      ms.WriteBuffer(trans.payload[1], length(trans.payload));
    ms.Write(trans.public_key.EC_OpenSSL_NID,
      Sizeof(trans.public_key.EC_OpenSSL_NID));
    if length(trans.public_key.x) > 0 then
      ms.WriteBuffer(trans.public_key.x[1], length(trans.public_key.x));
    if length(trans.public_key.y) > 0 then
      ms.WriteBuffer(trans.public_key.y[1], length(trans.public_key.y));
    if trans.opTransactionStyle = buy_account then
    begin
      ms.Write(trans.AccountPrice, Sizeof(trans.AccountPrice));
      ms.Write(trans.SellerAccount, Sizeof(trans.SellerAccount));
      ms.Write(trans.new_accountkey.EC_OpenSSL_NID,
        Sizeof(trans.new_accountkey.EC_OpenSSL_NID));
      if length(trans.new_accountkey.x) > 0 then
        ms.WriteBuffer(trans.new_accountkey.x[1],
          length(trans.new_accountkey.x));
      if length(trans.new_accountkey.y) > 0 then
        ms.WriteBuffer(trans.new_accountkey.y[1],
          length(trans.new_accountkey.y));
    end;
    SetLength(Result, ms.Size);
    ms.Position := 0;
    ms.ReadBuffer(Result[1], ms.Size);
    s := TCrypto.ToHexaString(Result);
  finally
    ms.Free;
  end;
end;

procedure TTransferMoneyTransaction.InitializeData;
begin
  inherited;
  FData := CT_TOpTransactionData_NUL;
end;

function TTransferMoneyTransaction.LoadFromStream(Stream: TStream;
  LoadExtendedData: Boolean): Boolean;
var
  b: Byte;
begin
  Result := false;
  if Stream.Size - Stream.Position < 28 then
    Exit; // Invalid stream
  Stream.Read(FData.sender, Sizeof(FData.sender));
  Stream.Read(FData.n_operation, Sizeof(FData.n_operation));
  Stream.Read(FData.target, Sizeof(FData.target));
  Stream.Read(FData.amount, Sizeof(FData.amount));
  Stream.Read(FData.fee, Sizeof(FData.fee));
  if TStreamOp.ReadAnsiString(Stream, FData.payload) < 0 then
    Exit;
  if Stream.Read(FData.public_key.EC_OpenSSL_NID,
    Sizeof(FData.public_key.EC_OpenSSL_NID)) < 0 then
    Exit;
  if TStreamOp.ReadAnsiString(Stream, FData.public_key.x) < 0 then
    Exit;
  if TStreamOp.ReadAnsiString(Stream, FData.public_key.y) < 0 then
    Exit;
  if ((LoadExtendedData) or (self is TBuyAccountTransaction)) then
  begin
    if Stream.Read(b, 1) <> 1 then
    begin
      Exit;
    end;
    case b of
      0:
        FData.opTransactionStyle := Transaction;
      1:
        FData.opTransactionStyle := transaction_with_auto_buy_account;
      2:
        begin
          FData.opTransactionStyle := buy_account;
          if (not(self is TBuyAccountTransaction)) then
            Exit;
        end
    else
      Exit;
    end;
    if (FData.opTransactionStyle in [transaction_with_auto_buy_account,
      buy_account]) then
    begin
      Stream.Read(FData.AccountPrice, Sizeof(FData.AccountPrice));
      Stream.Read(FData.SellerAccount, Sizeof(FData.SellerAccount));
      if Stream.Read(FData.new_accountkey.EC_OpenSSL_NID,
        Sizeof(FData.new_accountkey.EC_OpenSSL_NID)) < 0 then
        Exit;
      if TStreamOp.ReadAnsiString(Stream, FData.new_accountkey.x) < 0 then
        Exit;
      if TStreamOp.ReadAnsiString(Stream, FData.new_accountkey.y) < 0 then
        Exit;
    end;
  end;
  if TStreamOp.ReadAnsiString(Stream, FData.sign.r) < 0 then
    Exit;
  if TStreamOp.ReadAnsiString(Stream, FData.sign.s) < 0 then
    Exit;
  Result := true;
end;

function TTransferMoneyTransaction.GetAmount: Int64;
begin
  Result := FData.amount;
end;

function TTransferMoneyTransaction.GetFee: UInt64;
begin
  Result := FData.fee;
end;

function TTransferMoneyTransaction.GetPayload: TRawBytes;
begin
  Result := FData.payload;
end;

function TTransferMoneyTransaction.GetOpType: Byte;
begin
  Result := CT_Op_Transaction;
end;

function TTransferMoneyTransaction.SaveToStream(Stream: TStream;
  SaveExtendedData: Boolean): Boolean;
var
  b: Byte;
begin
  Stream.Write(FData.sender, Sizeof(FData.sender));
  Stream.Write(FData.n_operation, Sizeof(FData.n_operation));
  Stream.Write(FData.target, Sizeof(FData.target));
  Stream.Write(FData.amount, Sizeof(FData.amount));
  Stream.Write(FData.fee, Sizeof(FData.fee));
  TStreamOp.WriteAnsiString(Stream, FData.payload);
  Stream.Write(FData.public_key.EC_OpenSSL_NID,
    Sizeof(FData.public_key.EC_OpenSSL_NID));
  TStreamOp.WriteAnsiString(Stream, FData.public_key.x);
  TStreamOp.WriteAnsiString(Stream, FData.public_key.y);
  if ((SaveExtendedData) or (self is TBuyAccountTransaction)) then
  begin
    case FData.opTransactionStyle of
      Transaction:
        b := 0;
      transaction_with_auto_buy_account:
        b := 1;
      buy_account:
        b := 2;
    else
      raise Exception.Create('ERROR DEV 20170424-1');
    end;
    Stream.Write(b, 1);
    if (FData.opTransactionStyle in [transaction_with_auto_buy_account,
      buy_account]) then
    begin
      Stream.Write(FData.AccountPrice, Sizeof(FData.AccountPrice));
      Stream.Write(FData.SellerAccount, Sizeof(FData.SellerAccount));
      Stream.Write(FData.new_accountkey.EC_OpenSSL_NID,
        Sizeof(FData.new_accountkey.EC_OpenSSL_NID));
      TStreamOp.WriteAnsiString(Stream, FData.new_accountkey.x);
      TStreamOp.WriteAnsiString(Stream, FData.new_accountkey.y);
    end;
  end;
  TStreamOp.WriteAnsiString(Stream, FData.sign.r);
  TStreamOp.WriteAnsiString(Stream, FData.sign.s);
  // (Stream as TMemoryStream).SaveToFile('streamy');
  Result := true;
end;

function TTransferMoneyTransaction.GetSignerAccount: Cardinal;
begin
  Result := FData.sender;
end;

function TTransferMoneyTransaction.GetDestinationAccount: Int64;
begin
  Result := FData.target;
end;

function TTransferMoneyTransaction.GetSellerAccount: Int64;
begin
  case FData.opTransactionStyle of
    transaction_with_auto_buy_account, buy_account:
      Result := FData.SellerAccount;
  else
    Result := inherited GetSellerAccount;
  end;
end;

function TTransferMoneyTransaction.GetNumberOfTransactions: Cardinal;
begin
  Result := FData.n_operation;
end;

function TTransferMoneyTransaction.ToString: string;
begin
  case FData.opTransactionStyle of
    Transaction:
      Result := Format
        ('Transaction from %s to %s amount:%s fee:%s (n_op:%d) payload size:%d',
        [TAccountComp.AccountNumberToAccountTxtNumber(FData.sender),
        TAccountComp.AccountNumberToAccountTxtNumber(FData.target),
        TAccountComp.FormatMoney(FData.amount),
        TAccountComp.FormatMoney(FData.fee), FData.n_operation,
        length(FData.payload)]);
    transaction_with_auto_buy_account:
      Result := Format
        ('Transaction/Buy account %s by %s paying %s to %s amount:%s fee:%s (n_op:%d) payload size:%d',
        [TAccountComp.AccountNumberToAccountTxtNumber(FData.target),
        TAccountComp.AccountNumberToAccountTxtNumber(FData.sender),
        TAccountComp.FormatMoney(FData.AccountPrice),
        TAccountComp.AccountNumberToAccountTxtNumber(FData.SellerAccount),
        TAccountComp.FormatMoney(FData.amount),
        TAccountComp.FormatMoney(FData.fee), FData.n_operation,
        length(FData.payload)]);
    buy_account:
      Result := Format
        ('Buy account %s by %s paying %s to %s amount:%s fee:%s (n_op:%d) payload size:%d',
        [TAccountComp.AccountNumberToAccountTxtNumber(FData.target),
        TAccountComp.AccountNumberToAccountTxtNumber(FData.sender),
        TAccountComp.FormatMoney(FData.AccountPrice),
        TAccountComp.AccountNumberToAccountTxtNumber(FData.SellerAccount),
        TAccountComp.FormatMoney(FData.amount),
        TAccountComp.FormatMoney(FData.fee), FData.n_operation,
        length(FData.payload)]);
  else
    raise Exception.Create('ERROR DEV 20170424-2');
  end;
end;

constructor TBuyAccountTransaction.CreateBuy(account_number, n_operation, account_to_buy,
  account_to_pay: Cardinal; price, amount, fee: UInt64;
  new_public_key: TAccountKey; key: TECPrivateKey; payload: TRawBytes);
begin
  inherited Create;
  FData.sender := account_number;
  FData.n_operation := n_operation;
  FData.target := account_to_buy;
  FData.amount := amount;
  FData.fee := fee;
  FData.payload := payload;
  // V2: No need to store public key because it's at safebox. Saving at least 64 bytes!
  // FData.public_key := key.PublicKey;
  FData.opTransactionStyle := buy_account;
  FData.AccountPrice := price;
  FData.SellerAccount := account_to_pay;
  FData.new_accountkey := new_public_key;
  if not DoSignOperation(key, FData) then
  begin
    TLog.NewLog(lterror, Classname, 'Error signing a new Buy operation');
    FHasValidSignature := false;
  end
  else
    FHasValidSignature := true;
end;

procedure TBuyAccountTransaction.InitializeData;
begin
  inherited;
  FData.opTransactionStyle := buy_account;
end;

function TBuyAccountTransaction.GetOpType: Byte;
begin
  Result := CT_Op_BuyAccount;
end;

function TBuyAccountTransaction.GetTransactionData(Block, Affected_account_number
  : Cardinal; var TransactionData: TTransactionData): Boolean;
begin
  TransactionData.DestAccount := Data.target;
  if Data.sender = Affected_account_number then
  begin
    TransactionData.OpSubtype := CT_OpSubtype_BuyAccountBuyer;
    TransactionData.OperationTxt := 'Buy account ' +
      TAccountComp.AccountNumberToAccountTxtNumber(Data.target) + ' for ' +
      TAccountComp.FormatMoney(Data.AccountPrice) + ' MCC';
    TransactionData.amount := Int64(Data.amount) * (-1);
    Result := true;
  end
  else if Data.target = Affected_account_number then
  begin
    TransactionData.OpSubtype := CT_OpSubtype_BuyAccountTarget;
    TransactionData.OperationTxt := 'Purchased account ' +
      TAccountComp.AccountNumberToAccountTxtNumber(Data.target) + ' by ' +
      TAccountComp.AccountNumberToAccountTxtNumber(Data.sender) + ' for ' +
      TAccountComp.FormatMoney(Data.AccountPrice) + ' MCC';
    TransactionData.amount := Int64(Data.amount) - Int64(Data.AccountPrice);
    TransactionData.fee := 0;
    Result := true;
  end
  else if Data.SellerAccount = Affected_account_number then
  begin
    TransactionData.OpSubtype := CT_OpSubtype_BuyAccountSeller;
    TransactionData.OperationTxt := 'Sold account ' +
      TAccountComp.AccountNumberToAccountTxtNumber(Data.target) + ' by ' +
      TAccountComp.AccountNumberToAccountTxtNumber(Data.sender) + ' for ' +
      TAccountComp.FormatMoney(Data.AccountPrice) + ' MCC';
    TransactionData.amount := Data.AccountPrice;
    TransactionData.fee := 0;
    Result := true;
  end
  else
    Exit;
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

TTransactionManager.RegisterOperationClass(TTransferMoneyTransaction, CT_Op_Transaction);
TTransactionManager.RegisterOperationClass(TBuyAccountTransaction, CT_Op_BuyAccount);

end.
