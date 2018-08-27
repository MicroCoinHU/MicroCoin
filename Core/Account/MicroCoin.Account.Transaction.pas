unit MicroCoin.Account.Transaction;

{
  This unit contains code from PascalCoin:

  Copyright (c) Albert Molina 2016 - 2018 original code from PascalCoin https://pascalcoin.org/

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

}

interface

uses Sysutils, classes, UCrypto, MicroCoin.Common.Lists, UConst,
     MicroCoin.Account.AccountKey, MicroCoin.Account.Data, MicroCoin.Common,
     MicroCoin.Account.Storage, MicroCoin.BlockChain.BlockHeader, ULog;

type

  TAccountTransaction = class
  private
    FOrderedList: TOrderedAccountList;
    FFreezedAccounts: TAccountStorage;
    FTotalBalance: Int64;
    FTotalFee: Int64;
    FPreviusHash: TRawBytes;
    FAccountNames_Deleted: TOrderedRawList;
    FAccountNames_Added: TOrderedRawList;
    function GetInternalAccount(account_number: Cardinal): PAccount;
  protected
  public
    constructor Create(AccountStorage: TAccountStorage);
    destructor Destroy; override;
    function TransferAmount(sender, target: Cardinal; n_operation: Cardinal; amount, fee: UInt64; var errors: AnsiString): Boolean;
    function UpdateAccountInfo(signer_account, signer_n_operation, target_account: Cardinal; AccountInfo: TAccountInfo; newName: TRawBytes; newType: Word; fee: UInt64; var errors: AnsiString)
      : Boolean;
    function BuyAccount(buyer, account_to_buy, seller: Cardinal; n_operation: Cardinal; amount, account_price, fee: UInt64; const new_account_key: TAccountKey; var errors: AnsiString): Boolean;
    function Commit(const operationBlock: TBlockHeader; var errors: AnsiString): Boolean;
    function Account(account_number: Cardinal): TAccount;
    procedure Rollback;
    function CheckIntegrity: Boolean;
    property FreezedAccountStorage: TAccountStorage read FFreezedAccounts;
    property TotalFee: Int64 read FTotalFee;
    property TotalBalance: Int64 read FTotalBalance;
    procedure CopyFrom(transaction: TAccountTransaction);
    procedure CleanTransaction;
    function ModifiedCount: Integer;
    function Modified(Index: Integer): TAccount;
    property FreezedAccounts : TAccountStorage read FFreezedAccounts;
  end;


implementation

function TAccountTransaction.Account(account_number: Cardinal): TAccount;
var
  i: Integer;
begin
  if FOrderedList.Find(account_number, i)
  then begin
    Result := FOrderedList.Get(i);
  end else
  begin
    Result := FreezedAccountStorage.Account(account_number);
  end;
end;

function TAccountTransaction.BuyAccount(buyer, account_to_buy,
  seller: Cardinal; n_operation: Cardinal; amount, account_price, fee: UInt64;
  const new_account_key: TAccountKey; var errors: AnsiString): Boolean;
var
  PaccBuyer, PaccAccountToBuy, PaccSeller: PAccount;
begin
  Result := false;
  errors := '';
  if not CheckIntegrity then
  begin
    errors := 'Invalid integrity in accounts transaction';
    exit;
  end;
  if (buyer < 0) or (buyer >= (FFreezedAccounts.blocksCount * CT_AccountsPerBlock)) or
    (account_to_buy < 0) or (account_to_buy >= (FFreezedAccounts.blocksCount * CT_AccountsPerBlock)) or
    (seller < 0) or (seller >= (FFreezedAccounts.blocksCount * CT_AccountsPerBlock)) then
  begin
    errors := 'Invalid account number on buy';
    exit;
  end;
  if TAccount.IsAccountBlockedByProtocol(buyer, FFreezedAccounts.blocksCount) then
  begin
    errors := 'Buyer account is blocked for protocol';
    exit;
  end;
  if TAccount.IsAccountBlockedByProtocol(account_to_buy, FFreezedAccounts.blocksCount) then
  begin
    errors := 'Account to buy is blocked for protocol';
    exit;
  end;
  if TAccount.IsAccountBlockedByProtocol(seller, FFreezedAccounts.blocksCount) then
  begin
    errors := 'Seller account is blocked for protocol';
    exit;
  end;
  PaccBuyer := GetInternalAccount(buyer);
  PaccAccountToBuy := GetInternalAccount(account_to_buy);
  PaccSeller := GetInternalAccount(seller);
  if (PaccBuyer^.n_operation + 1 <> n_operation) then
  begin
    errors := 'Incorrect n_operation';
    exit;
  end;
  if (PaccBuyer^.balance < (amount + fee)) then
  begin
    errors := 'Insuficient founds';
    exit;
  end;
  if (fee > CT_MaxTransactionFee) then
  begin
    errors := 'Max fee';
    exit;
  end;
  if (PaccBuyer^.AccountInfo.IsLocked(FFreezedAccounts.blocksCount)) then
  begin
    errors := 'Buyer account is locked until block ' + inttostr(PaccBuyer^.AccountInfo.locked_until_block);
    exit;
  end;
  if not(PaccAccountToBuy^.AccountInfo.IsAccountForSale) then
  begin
    errors := 'Account is not for sale';
    exit;
  end;
  if (PaccAccountToBuy^.AccountInfo.new_publicKey.EC_OpenSSL_NID <> CT_TECDSA_Public_Nul.EC_OpenSSL_NID) and
    (not TAccountKey.EqualAccountKeys(PaccAccountToBuy^.AccountInfo.new_publicKey, new_account_key)) then
  begin
    errors := 'New public key is not equal to allowed new public key for account';
    exit;
  end;
  // Buy an account applies when account_to_buy.amount + operation amount >= price
  // Then, account_to_buy.amount will be (account_to_buy.amount + amount - price)
  // and buyer.amount will be buyer.amount + price
  if (PaccAccountToBuy^.AccountInfo.price > (PaccAccountToBuy^.balance + amount)) then
  begin
    errors := 'Account price ' + TCurrencyUtils.FormatMoney(PaccAccountToBuy^.AccountInfo.price) + ' < balance ' +
      TCurrencyUtils.FormatMoney(PaccAccountToBuy^.balance) + ' + amount ' + TCurrencyUtils.FormatMoney(amount);
    exit;
  end;

  if PaccBuyer^.updated_block <> FFreezedAccounts.blocksCount then
  begin
    PaccBuyer^.previous_updated_block := PaccBuyer^.updated_block;
    PaccBuyer^.updated_block := FFreezedAccounts.blocksCount;
  end;

  if PaccAccountToBuy^.updated_block <> FFreezedAccounts.blocksCount then
  begin
    PaccAccountToBuy^.previous_updated_block := PaccAccountToBuy^.updated_block;
    PaccAccountToBuy^.updated_block := FFreezedAccounts.blocksCount;
  end;

  if PaccSeller^.updated_block <> FFreezedAccounts.blocksCount then
  begin
    PaccSeller^.previous_updated_block := PaccSeller^.updated_block;
    PaccSeller^.updated_block := FFreezedAccounts.blocksCount;
  end;

  // Inc buyer n_operation
  PaccBuyer^.n_operation := n_operation;
  // Set new balance values
  PaccBuyer^.balance := PaccBuyer^.balance - (amount + fee);
  PaccAccountToBuy^.balance := PaccAccountToBuy^.balance + amount - PaccAccountToBuy^.AccountInfo.price;
  PaccSeller^.balance := PaccSeller^.balance + PaccAccountToBuy^.AccountInfo.price;

  // After buy, account will be unlocked and set to normal state and new account public key changed
  PaccAccountToBuy^.AccountInfo := CT_AccountInfo_NUL;
  PaccAccountToBuy^.AccountInfo.state := as_Normal;
  PaccAccountToBuy^.AccountInfo.AccountKey := new_account_key;

  Dec(FTotalBalance, fee);
  inc(FTotalFee, fee);
  Result := true;
end;

function TAccountTransaction.CheckIntegrity: Boolean;
begin
  Result := FPreviusHash = FFreezedAccounts.AccountStorageHash;
end;

procedure TAccountTransaction.CleanTransaction;
begin
  FOrderedList.Clear;
  FPreviusHash := FFreezedAccounts.AccountStorageHash;
  FTotalBalance := FFreezedAccounts.TotalBalance;
  FTotalFee := 0;
  FAccountNames_Added.Clear;
  FAccountNames_Deleted.Clear;
end;

function TAccountTransaction.Commit(const operationBlock: TBlockHeader;
  var errors: AnsiString): Boolean;
var
  i, j: Integer;
  b: TAccountStorageEntry;
  Pa: PAccount;
begin
  Result := false;
  errors := '';
  FFreezedAccounts.StartThreadSafe;
  try
    if not CheckIntegrity then
    begin
      errors := 'Invalid integrity in accounts transaction on commit';
      exit;
    end;
    for i := 0 to FOrderedList.Count - 1 do
    begin
      Pa := FOrderedList.GetPointer(i);
      FreezedAccounts.SetAccount(Pa^.Account,
        Pa^.AccountInfo,
        Pa^.name,
        Pa^.account_type,
        Pa^.balance,
        Pa^.n_operation);
    end;
    //
    if (FFreezedAccounts.TotalBalance <> FTotalBalance) then
    begin
      TLog.NewLog(ltError, Classname, Format('Invalid integrity balance! StrongBox:%d Transaction:%d', [FFreezedAccounts.TotalBalance, FTotalBalance]));
    end;
    if (FreezedAccounts.TotalFee <> TotalFee) then
    begin
      TLog.NewLog(ltError, Classname, Format('Invalid integrity fee! StrongBox:%d Transaction:%d', [FFreezedAccounts.TotalFee, FTotalFee]));
    end;
    b := FFreezedAccounts.AddNew(operationBlock);
    if (b.accounts[0].balance <> (operationBlock.reward + FTotalFee)) then
    begin
      TLog.NewLog(ltError, Classname, Format('Invalid integrity reward! Account:%d Balance:%d  Reward:%d Fee:%d (Reward+Fee:%d)',
        [b.accounts[0].Account, b.accounts[0].balance, operationBlock.reward, FTotalFee, operationBlock.reward + FTotalFee]));
    end;
    CleanTransaction;
    //
    if (FFreezedAccounts.CurrentProtocol < CT_PROTOCOL_2) and (operationBlock.protocol_version = CT_PROTOCOL_2) then
    begin
      // First block with new protocol!
      if FFreezedAccounts.CanUpgradeToProtocol2 then
      begin
        TLog.NewLog(ltInfo, Classname, 'Protocol upgrade to v2');
        if not FFreezedAccounts.DoUpgradeToProtocol2 then
        begin
          raise Exception.Create('Cannot upgrade to protocol v2 !');
        end;
      end;
    end;
    Result := true;
  finally
    FFreezedAccounts.EndThreadSave;
  end;
end;

procedure TAccountTransaction.CopyFrom(transaction: TAccountTransaction);
var
  i: Integer;
  P: PAccount;
begin
  if transaction = Self then
    exit;
  if transaction.FFreezedAccounts <> FFreezedAccounts then
    raise Exception.Create('Invalid Freezed accounts to copy');
  CleanTransaction;
  for i := 0 to transaction.FOrderedList.Count - 1 do
  begin
    P := transaction.FOrderedList.GetPointer(i);
    FOrderedList.Add(P^);
  end;
  FPreviusHash := transaction.FPreviusHash;
  FTotalBalance := transaction.FTotalBalance;
  FTotalFee := transaction.FTotalFee;
end;

constructor TAccountTransaction.Create(AccountStorage: TAccountStorage);
begin
  FOrderedList := TOrderedAccountList.Create;
  FFreezedAccounts := AccountStorage;
  FPreviusHash := AccountStorage.AccountStorageHash;
  FTotalBalance := FFreezedAccounts.TotalBalance;
  FTotalFee := 0;
  FAccountNames_Added := TOrderedRawList.Create;
  FAccountNames_Deleted := TOrderedRawList.Create;
end;

destructor TAccountTransaction.Destroy;
begin
  CleanTransaction;
  FreeAndNil(FOrderedList);
  FreeAndNil(FAccountNames_Added);
  FreeAndNil(FAccountNames_Deleted);
  inherited;
end;

function TAccountTransaction.GetInternalAccount(account_number: Cardinal): PAccount;
var
  i: Integer;
  P: PAccount;
begin
  if FOrderedList.Find(account_number, i)
  then begin
    Result := FOrderedList.GetPointer(i)
  end else begin
    i := FOrderedList.Add(FreezedAccountStorage.Account(account_number));
    Result := FOrderedList.GetPointer(i);
  end;
end;

function TAccountTransaction.Modified(Index: Integer): TAccount;
begin
  Result := FOrderedList.Get(index);
end;

function TAccountTransaction.ModifiedCount: Integer;
begin
  Result := FOrderedList.Count;
end;

procedure TAccountTransaction.Rollback;
begin
  CleanTransaction;
end;

function TAccountTransaction.TransferAmount(sender, target: Cardinal;
  n_operation: Cardinal; amount, fee: UInt64; var errors: AnsiString): Boolean;
var
  intSender, intTarget: Integer;
  PaccSender, PaccTarget: PAccount;
begin
  Result := false;
  errors := '';
  if not CheckIntegrity then
  begin
    errors := 'Invalid integrity in accounts transaction';
    exit;
  end;
  if (sender < 0) or (sender >= (FFreezedAccounts.blocksCount * CT_AccountsPerBlock)) or
    (target < 0) or (target >= (FFreezedAccounts.blocksCount * CT_AccountsPerBlock)) then
  begin
    errors := 'Invalid sender or target on transfer';
    exit;
  end;
  if TAccount.IsAccountBlockedByProtocol(sender, FFreezedAccounts.blocksCount) then
  begin
    errors := 'Sender account is blocked for protocol';
    exit;
  end;
  if TAccount.IsAccountBlockedByProtocol(target, FFreezedAccounts.blocksCount) then
  begin
    errors := 'Target account is blocked for protocol';
    exit;
  end;
  PaccSender := GetInternalAccount(sender);
  PaccTarget := GetInternalAccount(target);
  if (PaccSender^.n_operation + 1 <> n_operation) then
  begin
    errors := 'Incorrect n_operation';
    exit;
  end;
  if (PaccSender^.balance < (amount + fee)) then
  begin
    errors := 'Insuficient founds';
    exit;
  end;
  if ((PaccTarget^.balance + amount) > CT_MaxWalletAmount) then
  begin
    errors := 'Max account balance';
    exit;
  end;
  if (fee > CT_MaxTransactionFee) then
  begin
    errors := 'Max fee';
    exit;
  end;
  if (PaccSender^.AccountInfo.IsLocked(FFreezedAccounts.blocksCount)) then
  begin
    errors := 'Sender account is locked until block ' + inttostr(PaccSender^.AccountInfo.locked_until_block);
    exit;
  end;

  if PaccSender^.updated_block <> FFreezedAccounts.blocksCount then
  begin
    PaccSender^.previous_updated_block := PaccSender^.updated_block;
    PaccSender^.updated_block := FFreezedAccounts.blocksCount;
  end;

  if PaccTarget^.updated_block <> FFreezedAccounts.blocksCount then
  begin
    PaccTarget^.previous_updated_block := PaccTarget.updated_block;
    PaccTarget^.updated_block := FFreezedAccounts.blocksCount;
  end;

  PaccSender^.n_operation := n_operation;
  PaccSender^.balance := PaccSender^.balance - (amount + fee);
  PaccTarget^.balance := PaccTarget^.balance + (amount);

  Dec(FTotalBalance, fee);
  inc(FTotalFee, fee);
  Result := true;
end;

function TAccountTransaction.UpdateAccountInfo(signer_account, signer_n_operation, target_account: Cardinal;
  AccountInfo: TAccountInfo; newName: TRawBytes; newType: Word; fee: UInt64; var errors: AnsiString): Boolean;
var
  i: Integer;
  P_signer, P_target: PAccount;
begin
  Result := false;
  errors := '';
  if (signer_account < 0) or (signer_account >= (FFreezedAccounts.blocksCount * CT_AccountsPerBlock)) or
    (target_account < 0) or (target_account >= (FFreezedAccounts.blocksCount * CT_AccountsPerBlock)) then
  begin
    errors := 'Invalid account';
    exit;
  end;
  if (TAccount.IsAccountBlockedByProtocol(signer_account, FFreezedAccounts.blocksCount)) or
    (TAccount.IsAccountBlockedByProtocol(target_account, FFreezedAccounts.blocksCount)) then
  begin
    errors := 'account is blocked for protocol';
    exit;
  end;
  P_signer := GetInternalAccount(signer_account);
  P_target := GetInternalAccount(target_account);
  if (P_signer^.n_operation + 1 <> signer_n_operation) then
  begin
    errors := 'Incorrect n_operation';
    exit;
  end;
  if (P_signer^.balance < fee) then
  begin
    errors := 'Insuficient founds';
    exit;
  end;
  if (P_signer^.AccountInfo.IsLocked(FFreezedAccounts.blocksCount)) then
  begin
    errors := 'Signer account is locked until block ' + inttostr(P_signer^.AccountInfo.locked_until_block);
    exit;
  end;
  if (P_target^.AccountInfo.IsLocked(FFreezedAccounts.blocksCount)) then
  begin
    errors := 'Target account is locked until block ' + inttostr(P_target^.AccountInfo.locked_until_block);
    exit;
  end;
  if P_signer^.updated_block <> FFreezedAccounts.blocksCount then
  begin
    P_signer^.previous_updated_block := P_signer^.updated_block;
    P_signer^.updated_block := FFreezedAccounts.blocksCount;
  end;
  if (signer_account <> target_account) then
  begin
    if P_target^.updated_block <> FFreezedAccounts.blocksCount then
    begin
      P_target^.previous_updated_block := P_target^.updated_block;
      P_target^.updated_block := FFreezedAccounts.blocksCount;
    end;
  end;
  if not TAccountKey.EqualAccountKeys(P_signer^.AccountInfo.AccountKey, P_target^.AccountInfo.AccountKey) then
  begin
    errors := 'Signer and target have diff key';
    exit;
  end;
  if (newName <> P_target^.name) then
  begin
    // NEW NAME CHANGE CHECK:
    if (newName <> '') then
    begin
      if not FFreezedAccounts.AccountNameIsValid(newName, errors) then
      begin
        errors := 'Invalid account name "' + newName + '" length:' + inttostr(length(newName)) + ': ' + errors;
        exit;
      end;
      i := FFreezedAccounts.FindAccountByName(newName);
      if (i >= 0) then
      begin
        // This account name is in the safebox... check if deleted:
        i := FAccountNames_Deleted.IndexOf(newName);
        if i < 0 then
        begin
          errors := 'Account name "' + newName + '" is in current use';
          exit;
        end;
      end;
      i := FAccountNames_Added.IndexOf(newName);
      if (i >= 0) then
      begin
        // This account name is added in this transaction! (perhaps deleted also, but does not allow to "double add same name in same block")
        errors := 'Account name "' + newName + '" is in same transaction';
        exit;
      end;
    end;
    // Ok, include
    if (P_target^.name <> '') then
    begin
      // In use in the safebox, mark as deleted
      FAccountNames_Deleted.Add(P_target^.name, target_account);
    end;
    if (newName <> '') then
    begin
      FAccountNames_Added.Add(newName, target_account);
    end;
  end;

  P_signer^.n_operation := signer_n_operation;
  P_target^.AccountInfo := AccountInfo;
  P_target^.name := newName;
  P_target^.account_type := newType;
  Dec(P_signer^.balance, fee); // Signer is who pays the fee
  Dec(FTotalBalance, fee);
  inc(FTotalFee, fee);
  Result := true;
end;


end.
