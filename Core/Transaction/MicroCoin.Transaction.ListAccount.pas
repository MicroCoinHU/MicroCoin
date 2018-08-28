unit MicroCoin.Transaction.ListAccount;

{
  This unit contains code from PascalCoin:

  Copyright (c) Albert Molina 2016 - 2018 original code from PascalCoin https://pascalcoin.org/

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

}

interface

uses MicroCoin.Transaction.Base,
  MicroCoin.Common,
  MicroCoin.Account.Data, MicroCoin.Account.Transaction,
  MicroCoin.Account.AccountKey,
  MicroCoin.Transaction.Transaction,
  Sysutils, classes, UCrypto,
  ULog, UConst, MicroCoin.Transaction.Manager;

type

  TOpListAccountOperationType = (lat_Unknown, lat_ListForSale, lat_DelistAccount);

  TOpListAccountData = record
    account_signer, account_target: Cardinal;
    operation_type: TOpListAccountOperationType;
    n_operation: Cardinal;
    account_price: UInt64;
    account_to_pay: Cardinal;
    fee: UInt64;
    payload: TRawBytes;
    public_key: TAccountKey;
    new_public_key: TAccountKey;
    // If EC_OpenSSL_NID=0 then is OPEN, otherwise is for only 1 public key
    locked_until_block: Cardinal; //
    sign: TECDSA_SIG;
  end;

  TOpListAccount = class(TTransaction)
  private
    FData: TOpListAccountData;
  protected
    procedure InitializeData; override;
    function SaveToStream(Stream: TStream; SaveExtendedData: Boolean): Boolean; override;
    function LoadFromStream(Stream: TStream; LoadExtendedData: Boolean): Boolean; override;
  public
    class function GetOperationHashToSign(const operation: TOpListAccountData): TRawBytes;
    class function DoSignOperation(key: TECPrivateKey; var operation: TOpListAccountData): Boolean;
    function IsPrivateSale: Boolean;
    function IsDelist: Boolean; virtual; abstract;
    function GetBufferForOpHash(UseProtocolV2: Boolean): TRawBytes; override;
    function ApplyTransaction(AccountTransaction: TAccountTransaction; var errors: AnsiString): Boolean; override;
    function GetAmount: Int64; override;
    function GetFee: UInt64; override;
    function GetPayload: TRawBytes; override;
    function GetSignerAccount: Cardinal; override;
    function GetDestinationAccount: Int64; override;
    function GetSellerAccount: Int64; override;
    function GetNumberOfTransactions: Cardinal; override;
    procedure AffectedAccounts(list: TList); override;
    property Data: TOpListAccountData read FData;
    function toString: string; override;
  end;

  TOpListAccountForSale = class(TOpListAccount)
  public
    function GetOpType: Byte; override;
    constructor CreateListAccountForSale(account_signer, n_operation, account_target: Cardinal;
      account_price, fee: UInt64; account_to_pay: Cardinal; new_public_key: TAccountKey; locked_until_block: Cardinal;
      key: TECPrivateKey; payload: TRawBytes);
    function IsDelist: Boolean; override;
    function GetTransactionData(Block: Cardinal; Affected_account_number: Cardinal;
      var TransactionData: TTransactionData): Boolean; override;
  end;

  TOpDelistAccountForSale = class(TOpListAccount)
  public
    function GetOpType: Byte; override;
    constructor CreateDelistAccountForSale(account_signer, n_operation, account_target: Cardinal; fee: UInt64;
      key: TECPrivateKey; payload: TRawBytes);
    function IsDelist: Boolean; override;
    function GetTransactionData(Block: Cardinal; Affected_account_number: Cardinal;
      var TransactionData: TTransactionData): Boolean; override;
  end;

const
  CT_TOpListAccountData_NUL: TOpListAccountData = (account_signer: 0; account_target: 0; operation_type: lat_Unknown;
    n_operation: 0; account_price: 0; account_to_pay: 0; fee: 0; payload: '';
    public_key: (EC_OpenSSL_NID: 0; x: ''; y: ''); new_public_key: (EC_OpenSSL_NID: 0; x: ''; y: '');
    locked_until_block: 0; sign: (r: ''; s: ''));

implementation

procedure TOpListAccount.AffectedAccounts(list: TList);
begin
  list.Add(TObject(FData.account_signer));
  if FData.account_signer <> FData.account_target then
    list.Add(TObject(FData.account_target));
end;

function TOpListAccount.ApplyTransaction(AccountTransaction: TAccountTransaction; var errors: AnsiString): Boolean;
var
  account_signer, account_target: TAccount;
begin
  Result := false;
  if (AccountTransaction.FreezedAccountStorage.CurrentProtocol < CT_PROTOCOL_2) then
  begin
    errors := 'List/Delist Account is not allowed on Protocol 1';
    exit;
  end;
  if (FData.account_signer >= AccountTransaction.FreezedAccountStorage.AccountsCount) then
  begin
    errors := 'Invalid signer account number';
    exit;
  end;
  if (FData.account_target >= AccountTransaction.FreezedAccountStorage.AccountsCount) then
  begin
    errors := 'Invalid target account number';
    exit;
  end;
  if TAccount.IsAccountBlockedByProtocol(FData.account_signer, AccountTransaction.FreezedAccountStorage.BlocksCount)
  then
  begin
    errors := 'Signer account is blocked for protocol';
    exit;
  end;
  if TAccount.IsAccountBlockedByProtocol(FData.account_target, AccountTransaction.FreezedAccountStorage.BlocksCount)
  then
  begin
    errors := 'Target account is blocked for protocol';
    exit;
  end;
  if not IsDelist then
  begin
    if (FData.account_to_pay >= AccountTransaction.FreezedAccountStorage.AccountsCount) then
    begin
      errors := 'Invalid account to pay number';
      exit;
    end;
    if (FData.account_target = FData.account_to_pay) then
    begin
      errors := 'Account to pay is itself';
      exit;
    end;
    if TAccount.IsAccountBlockedByProtocol(FData.account_to_pay, AccountTransaction.FreezedAccountStorage.BlocksCount)
    then
    begin
      errors := 'Account to pay is blocked for protocol';
      exit;
    end;
    if (FData.account_price <= 0) then
    begin
      errors := 'Account for sale price must be > 0';
      exit;
    end;
    if (FData.locked_until_block > (AccountTransaction.FreezedAccountStorage.BlocksCount +
      CT_MaxFutureBlocksLockedAccount)) then
    begin
      errors := 'Invalid locked block: Current block ' + Inttostr(AccountTransaction.FreezedAccountStorage.BlocksCount)
        + ' cannot lock to block ' + Inttostr(FData.locked_until_block);
      exit;
    end;
    if IsPrivateSale then
    begin
      if not FData.new_public_key.IsValidAccountKey(errors) then
      begin
        errors := 'Invalid new public key: ' + errors;
        exit;
      end;
    end;
  end;
  if (FData.fee < 0) or (FData.fee > CT_MaxTransactionFee) then
  begin
    errors := 'Invalid fee: ' + Inttostr(FData.fee);
    exit;
  end;
  account_signer := AccountTransaction.Account(FData.account_signer);
  account_target := AccountTransaction.Account(FData.account_target);
  if (FData.account_signer <> FData.account_target) then
  begin
    // Both accounts must have same PUBLIC KEY!
    if not TAccountKey.EqualAccountKeys(account_signer.accountInfo.AccountKey, account_target.accountInfo.AccountKey)
    then
    begin
      errors := 'Signer and affected accounts have different public key';
      exit;
    end;
  end;
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
    if TAccountKey.EqualAccountKeys(account_target.accountInfo.AccountKey, FData.new_public_key) then
    begin
      errors := 'New public key for private sale is the same public key';
      exit;
    end;
  end;
  //
  // Build 1.4
  if (FData.public_key.EC_OpenSSL_NID <> CT_TECDSA_Public_Nul.EC_OpenSSL_NID) and
    (not TAccountKey.EqualAccountKeys(FData.public_key, account_signer.accountInfo.AccountKey)) then
  begin
    errors := Format('Invalid public key for account %d. Distinct from SafeBox public key! %s <> %s',
      [FData.account_signer, TCrypto.ToHexaString(FData.public_key.ToRawString),
      TCrypto.ToHexaString(account_signer.accountInfo.AccountKey.ToRawString)]);
    exit;
  end;
  if not TCrypto.ECDSAVerify(account_signer.accountInfo.AccountKey, GetOperationHashToSign(FData), FData.sign) then
  begin
    errors := 'Invalid sign';
    FHasValidSignature := false;
    exit;
  end
  else
    FHasValidSignature := true;
  FPrevious_Signer_updated_block := account_signer.updated_block;
  FPrevious_Destination_updated_block := account_target.updated_block;
  if IsDelist then
  begin
    account_target.accountInfo.state := as_Normal;
    account_target.accountInfo.locked_until_block := CT_AccountInfo_NUL.locked_until_block;
    account_target.accountInfo.price := CT_AccountInfo_NUL.price;
    account_target.accountInfo.account_to_pay := CT_AccountInfo_NUL.account_to_pay;
    account_target.accountInfo.new_publicKey := CT_AccountInfo_NUL.new_publicKey;
  end
  else
  begin
    account_target.accountInfo.state := as_ForSale;
    account_target.accountInfo.locked_until_block := FData.locked_until_block;
    account_target.accountInfo.price := FData.account_price;
    account_target.accountInfo.account_to_pay := FData.account_to_pay;
    account_target.accountInfo.new_publicKey := FData.new_public_key;
  end;
  Result := AccountTransaction.UpdateAccountInfo(FData.account_signer, FData.n_operation, FData.account_target,
    account_target.accountInfo, account_target.name, account_target.account_type, FData.fee, errors);
end;

class function TOpListAccount.DoSignOperation(key: TECPrivateKey; var operation: TOpListAccountData): Boolean;
var
  s: AnsiString;
  _sign: TECDSA_SIG;
begin
  s := GetOperationHashToSign(operation);
  try
    _sign := TCrypto.ECDSASign(key, s);
    operation.sign := _sign;
    Result := true;
  except
    on E: Exception do
    begin
      Result := false;
      TLog.NewLog(lterror, ClassName, 'Error signing Account for sale operation: ' + E.Message);
    end;
  end;
end;

function TOpListAccount.GetBufferForOpHash(UseProtocolV2: Boolean): TRawBytes;
begin
  // This Operation is new from protocol V2, so we cannot hash it as a previous protocol!
  Result := inherited GetBufferForOpHash(true);
end;

class function TOpListAccount.GetOperationHashToSign(const operation: TOpListAccountData): TRawBytes;
var
  ms: TMemoryStream;
  s: AnsiString;
begin
  ms := TMemoryStream.Create;
  try
    ms.Write(operation.account_signer, Sizeof(operation.account_signer));
    ms.Write(operation.account_target, Sizeof(operation.account_target));
    ms.Write(operation.n_operation, Sizeof(operation.n_operation));
    ms.Write(operation.account_price, Sizeof(operation.account_price));
    ms.Write(operation.account_to_pay, Sizeof(operation.account_to_pay));
    ms.Write(operation.fee, Sizeof(operation.fee));
    if length(operation.payload) > 0 then
      ms.WriteBuffer(operation.payload[1], length(operation.payload));
    ms.Write(operation.public_key.EC_OpenSSL_NID, Sizeof(operation.public_key.EC_OpenSSL_NID));
    if length(operation.public_key.x) > 0 then
      ms.WriteBuffer(operation.public_key.x[1], length(operation.public_key.x));
    if length(operation.public_key.y) > 0 then
      ms.WriteBuffer(operation.public_key.y[1], length(operation.public_key.y));
    s := operation.new_public_key.ToRawString;
    if length(s) > 0 then
      ms.WriteBuffer(s[1], length(s));
    ms.Write(operation.locked_until_block, Sizeof(operation.locked_until_block));
    ms.Position := 0;
    setlength(Result, ms.Size);
    ms.ReadBuffer(Result[1], ms.Size);
  finally
    ms.Free;
  end;
end;

procedure TOpListAccount.InitializeData;
begin
  inherited;
  FData := CT_TOpListAccountData_NUL;
end;

function TOpListAccount.IsPrivateSale: Boolean;
begin
  Result := (not IsDelist) and (FData.new_public_key.EC_OpenSSL_NID <> 0);
end;

function TOpListAccount.LoadFromStream(Stream: TStream; LoadExtendedData: Boolean): Boolean;
var
  s: AnsiString;
  w: Word;
begin
  Result := false;
  if Stream.Size - Stream.Position < 14 then
    exit; // Invalid stream
  Stream.Read(FData.account_signer, Sizeof(FData.account_signer));
  Stream.Read(FData.account_target, Sizeof(FData.account_target));
  Stream.Read(w, 2);
  case w of
    CT_Op_ListAccountForSale:
      FData.operation_type := lat_ListForSale;
    CT_Op_DelistAccount:
      FData.operation_type := lat_DelistAccount;
  else
    exit; // Invalid data info
  end;
  Stream.Read(FData.n_operation, Sizeof(FData.n_operation));
  if (FData.operation_type = lat_ListForSale) then
  begin
    Stream.Read(FData.account_price, Sizeof(FData.account_price));
    Stream.Read(FData.account_to_pay, Sizeof(FData.account_to_pay));
    if Stream.Read(FData.public_key.EC_OpenSSL_NID, Sizeof(FData.public_key.EC_OpenSSL_NID)) < 0 then
      exit;
    if TStreamOp.ReadAnsiString(Stream, FData.public_key.x) < 0 then
      exit;
    if TStreamOp.ReadAnsiString(Stream, FData.public_key.y) < 0 then
      exit;
    if TStreamOp.ReadAnsiString(Stream, s) < 0 then
      exit;
    FData.new_public_key := TAccountKey.FromRawString(s);
    Stream.Read(FData.locked_until_block, Sizeof(FData.locked_until_block));
  end;
  Stream.Read(FData.fee, Sizeof(FData.fee));
  if TStreamOp.ReadAnsiString(Stream, FData.payload) < 0 then
    exit;
  if TStreamOp.ReadAnsiString(Stream, FData.sign.r) < 0 then
    exit;
  if TStreamOp.ReadAnsiString(Stream, FData.sign.s) < 0 then
    exit;
  Result := true;
end;

function TOpListAccount.GetNumberOfTransactions: Cardinal;
begin
  Result := FData.n_operation;
end;

function TOpListAccount.GetAmount: Int64;
begin
  Result := 0;
end;

function TOpListAccount.GetFee: UInt64;
begin
  Result := FData.fee;
end;

function TOpListAccount.GetPayload: TRawBytes;
begin
  Result := FData.payload;
end;

function TOpListAccount.SaveToStream(Stream: TStream; SaveExtendedData: Boolean): Boolean;
var
  w: Word;
begin
  Stream.Write(FData.account_signer, Sizeof(FData.account_signer));
  Stream.Write(FData.account_target, Sizeof(FData.account_target));
  case FData.operation_type of
    lat_ListForSale:
      w := CT_Op_ListAccountForSale;
    lat_DelistAccount:
      w := CT_Op_DelistAccount;
  else
    raise Exception.Create('ERROR DEV 20170412-1');
  end;
  Stream.Write(w, 2);
  Stream.Write(FData.n_operation, Sizeof(FData.n_operation));
  if FData.operation_type = lat_ListForSale then
  begin
    Stream.Write(FData.account_price, Sizeof(FData.account_price));
    Stream.Write(FData.account_to_pay, Sizeof(FData.account_to_pay));
    Stream.Write(FData.public_key.EC_OpenSSL_NID, Sizeof(FData.public_key.EC_OpenSSL_NID));
    TStreamOp.WriteAnsiString(Stream, FData.public_key.x);
    TStreamOp.WriteAnsiString(Stream, FData.public_key.y);
    TStreamOp.WriteAnsiString(Stream, FData.new_public_key.ToRawString);
    Stream.Write(FData.locked_until_block, Sizeof(FData.locked_until_block));
  end;
  Stream.Write(FData.fee, Sizeof(FData.fee));
  TStreamOp.WriteAnsiString(Stream, FData.payload);
  TStreamOp.WriteAnsiString(Stream, FData.sign.r);
  TStreamOp.WriteAnsiString(Stream, FData.sign.s);
  Result := true;
end;

function TOpListAccount.GetSignerAccount: Cardinal;
begin
  Result := FData.account_signer;
end;

function TOpListAccount.GetDestinationAccount: Int64;
begin
  Result := FData.account_target;
end;

function TOpListAccount.GetSellerAccount: Int64;
begin
  case FData.operation_type of
    lat_ListForSale:
      Result := FData.account_to_pay;
  else
    Result := inherited GetSellerAccount;
  end;
end;

function TOpListAccount.toString: string;
begin
  case FData.operation_type of
    lat_ListForSale:
      begin
        if (FData.new_public_key.EC_OpenSSL_NID = CT_TECDSA_Public_Nul.EC_OpenSSL_NID) then
        begin
          Result := Format('List account %s for sale price %s locked until block:%d fee:%s (n_op:%d) payload size:%d',
            [TAccount.AccountNumberToAccountTxtNumber(FData.account_target),
            TCurrencyUtils.FormatMoney(FData.account_price), FData.locked_until_block,
            TCurrencyUtils.FormatMoney(FData.fee), FData.n_operation, length(FData.payload)])
        end
        else
        begin
          Result := Format
            ('List account %s for private sale price %s reserved for %s locked until block:%d fee:%s (n_op:%d) payload size:%d',
            [TAccount.AccountNumberToAccountTxtNumber(FData.account_target),
            TCurrencyUtils.FormatMoney(FData.account_price),
            TAccountKey.GetECInfoTxt(FData.new_public_key.EC_OpenSSL_NID), FData.locked_until_block,
            TCurrencyUtils.FormatMoney(FData.fee), FData.n_operation, length(FData.payload)])
        end;
      end;
    lat_DelistAccount:
      begin
        Result := Format('Delist account %s for sale fee:%s (n_op:%d) payload size:%d',
          [TAccount.AccountNumberToAccountTxtNumber(FData.account_target), TCurrencyUtils.FormatMoney(FData.fee),
          FData.n_operation, length(FData.payload)])
      end;
  else
    Result := 'ERROR DEV 20170414-2';
  end;
end;

constructor TOpListAccountForSale.CreateListAccountForSale(account_signer, n_operation, account_target: Cardinal;
  account_price, fee: UInt64; account_to_pay: Cardinal; new_public_key: TAccountKey; locked_until_block: Cardinal;
  key: TECPrivateKey; payload: TRawBytes);
begin
  inherited Create;
  FData.account_signer := account_signer;
  FData.account_target := account_target;
  FData.operation_type := lat_ListForSale;
  FData.n_operation := n_operation;
  FData.account_price := account_price;
  FData.account_to_pay := account_to_pay;
  FData.fee := fee;
  FData.payload := payload;
  // V2: No need to store public key because it's at safebox. Saving at least 64 bytes!
  // FData.public_key := key.PublicKey;
  FData.new_public_key := new_public_key;
  FData.locked_until_block := locked_until_block;
  if not DoSignOperation(key, FData) then
  begin
    TLog.NewLog(lterror, ClassName, 'Error signing a new list account for sale operation');
    FHasValidSignature := false;
  end
  else
    FHasValidSignature := true;
end;

function TOpListAccountForSale.IsDelist: Boolean;
begin
  Result := false;
end;

function TOpListAccountForSale.GetOpType: Byte;
begin
  Result := CT_Op_ListAccountForSale;
end;

function TOpListAccountForSale.GetTransactionData(Block, Affected_account_number: Cardinal;
  var TransactionData: TTransactionData): Boolean;
begin
  if IsPrivateSale then
  begin
    TransactionData.OpSubtype := CT_OpSubtype_ListAccountForPrivateSale;
    TransactionData.OperationTxt := 'List account ' + TAccount.AccountNumberToAccountTxtNumber(Data.account_target) +
      ' for private sale price ' + TCurrencyUtils.FormatMoney(Data.account_price) + ' MCC pay to ' +
      TAccount.AccountNumberToAccountTxtNumber(Data.account_to_pay);
  end
  else
  begin
    TransactionData.OpSubtype := CT_OpSubtype_ListAccountForPublicSale;
    TransactionData.OperationTxt := 'List account ' + TAccount.AccountNumberToAccountTxtNumber(Data.account_target) +
      ' for sale price ' + TCurrencyUtils.FormatMoney(Data.account_price) + ' MCC pay to ' +
      TAccount.AccountNumberToAccountTxtNumber(Data.account_to_pay);
  end;
  TransactionData.newKey := Data.new_public_key;
  TransactionData.SellerAccount := GetSellerAccount;
  Result := true;
end;

constructor TOpDelistAccountForSale.CreateDelistAccountForSale(account_signer, n_operation, account_target: Cardinal;
  fee: UInt64; key: TECPrivateKey; payload: TRawBytes);
begin
  inherited Create;
  FData.account_signer := account_signer;
  FData.account_target := account_target;
  FData.operation_type := lat_DelistAccount;
  FData.n_operation := n_operation;
  FData.fee := fee;
  FData.payload := payload;
  if not DoSignOperation(key, FData) then
  begin
    TLog.NewLog(lterror, ClassName, 'Error signing a delist account operation');
    FHasValidSignature := false;
  end
  else
    FHasValidSignature := true;
end;

function TOpDelistAccountForSale.IsDelist: Boolean;
begin
  Result := true;
end;

function TOpDelistAccountForSale.GetOpType: Byte;
begin
  Result := CT_Op_DelistAccount;
end;

function TOpDelistAccountForSale.GetTransactionData(Block, Affected_account_number: Cardinal;
  var TransactionData: TTransactionData): Boolean;
begin
  TransactionData.OpSubtype := CT_OpSubtype_DelistAccount;
  TransactionData.OperationTxt := 'Delist account ' + TAccount.AccountNumberToAccountTxtNumber(Data.account_target) +
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

TTransactionManager.RegisterTransactionPlugin(TOpListAccountForSale, CT_Op_ListAccountForSale);
TTransactionManager.RegisterTransactionPlugin(TOpDelistAccountForSale, CT_Op_DelistAccount);

end.
