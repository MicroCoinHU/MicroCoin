{==============================================================================|
| MicroCoin                                                                    |
| Copyright (c) 2018 MicroCoin Developers                                      |
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
| This unit contains portions from PascalCoin                                  |
| Copyright (c) Albert Molina 2016 - 2018                                      |
|                                                                              |
| Distributed under the MIT software license, see the accompanying file        |
| LICENSE or visit http://www.opensource.org/licenses/mit-license.php.         |
|==============================================================================|
| File:       MicroCoin.Account.Transaction.pas                                |
| Created at: 2018-08-24                                                       |
| Purpose:    Account transaction managment                                    |
|==============================================================================}

unit MicroCoin.Account.Transaction;

{$ifdef FPC}
  {$mode delphi}
{$endif}

interface

uses Sysutils, classes, UCrypto, MicroCoin.Common.Lists, MicroCoin.Common.Config,
  MicroCoin.Account.AccountKey, MicroCoin.Account.Data, MicroCoin.Common,
  MicroCoin.Account.Storage, UbaseTypes, MicroCoin.BlockChain.BlockHeader, ULog;

type

  TAccountTransaction = class
  private
    FOrderedList: TOrderedAccountList;
    FFreezedAccounts: TAccountStorage;
    FTotalBalance: UInt64;
    FTotalFee: UInt64;
    FPreviusHash: TRawBytes;
    FAccountNames_Deleted: TOrderedRawList;
    FAccountNames_Added: TOrderedRawList;
  public
    constructor Create(AAccountStorage: TAccountStorage);
    destructor Destroy; override;
    function GetInternalAccount(AAccountNumber: Cardinal): PAccount;
    function TransferAmount(ASender, ATarget: Cardinal; ANumberOfTransactions: Cardinal; AAmount, AFee: UInt64;
      var RErrors: AnsiString): Boolean;
    function UpdateAccountInfo(ASignerAccount, ASignerAccountNumberOfTransactions, ATargetAccount: Cardinal; AAccountInfo: TAccountInfo;
      ANewName: TRawBytes; ANewType: Word; AFee: UInt64; var RErrors: AnsiString): Boolean;
    function BuyAccount(ABuyer, AAccountToBuy, ASeller: Cardinal; ANumberOfTransactions: Cardinal;
      AAmount, AAccountPrice, AFee: UInt64; const ANewKey: TAccountKey; var RErrors: AnsiString): Boolean;
    function Commit(const FBlockLock: TBlockHeader; var RErrors: AnsiString): Boolean;
    function Account(AAccountNumber: Cardinal): TAccount;
    procedure Rollback;
    procedure CopyFrom(Transaction: TAccountTransaction);
    procedure CleanTransaction;
    function CheckIntegrity: Boolean;
    function ModifiedCount: Integer;
    function Modified(Index: Integer): TAccount;
    property FreezedAccountStorage: TAccountStorage read FFreezedAccounts;
    property TotalFee: UInt64 read FTotalFee;
    property TotalBalance: UInt64 read FTotalBalance;
    property FreezedAccounts: TAccountStorage read FFreezedAccounts;
  end;

implementation

function TAccountTransaction.Account(AAccountNumber: Cardinal): TAccount;
var
  i: Integer;
begin
  if FOrderedList.Find(AAccountNumber, i) then
  begin
    Result := FOrderedList.Get(i);
  end
  else
  begin
    Result := FreezedAccountStorage.Accounts[AAccountNumber];
  end;
end;

function TAccountTransaction.BuyAccount(ABuyer, AAccountToBuy, ASeller: Cardinal; ANumberOfTransactions: Cardinal;
  AAmount, AAccountPrice, AFee: UInt64; const ANewKey: TAccountKey; var RErrors: AnsiString): Boolean;
var
  PaccBuyer, PaccAccountToBuy, PaccSeller: PAccount;
begin
  Result := false;
  RErrors := '';
  if not CheckIntegrity then
  begin
    RErrors := 'Invalid integrity in accounts transaction';
    exit;
  end;
  if (ABuyer >= (FFreezedAccounts.BlocksCount * cAccountsPerBlock)) or
    (AAccountToBuy >= (FFreezedAccounts.BlocksCount * cAccountsPerBlock)) or
    (ASeller >= (FFreezedAccounts.BlocksCount * cAccountsPerBlock)) then
  begin
    RErrors := 'Invalid account number on buy';
    exit;
  end;
  if TAccount.IsAccountBlockedByProtocol(ABuyer, FFreezedAccounts.BlocksCount) then
  begin
    RErrors := 'Buyer account is blocked for protocol';
    exit;
  end;
  if TAccount.IsAccountBlockedByProtocol(AAccountToBuy, FFreezedAccounts.BlocksCount) then
  begin
    RErrors := 'Account to buy is blocked for protocol';
    exit;
  end;
  if TAccount.IsAccountBlockedByProtocol(ASeller, FFreezedAccounts.BlocksCount) then
  begin
    RErrors := 'Seller account is blocked for protocol';
    exit;
  end;
  PaccBuyer := GetInternalAccount(ABuyer);
  PaccAccountToBuy := GetInternalAccount(AAccountToBuy);
  PaccSeller := GetInternalAccount(ASeller);
  if (PaccBuyer^.NumberOfTransactions + 1 <> ANumberOfTransactions) then
  begin
    RErrors := 'Incorrect n_operation';
    exit;
  end;
  if (PaccBuyer^.Balance < (AAmount + AFee)) then
  begin
    RErrors := 'Insuficient founds';
    exit;
  end;
  if (AFee > cMaxTransactionFee) then
  begin
    RErrors := 'Max fee';
    exit;
  end;
  if (PaccBuyer^.AccountInfo.IsLocked(FFreezedAccounts.BlocksCount)) then
  begin
    RErrors := 'Buyer account is locked until block ' + inttostr(PaccBuyer^.AccountInfo.LockedUntilBlock);
    exit;
  end;
  if not(PaccAccountToBuy^.AccountInfo.IsAccountForSale) then
  begin
    RErrors := 'Account is not for sale';
    exit;
  end;
  if (PaccAccountToBuy^.AccountInfo.NewPublicKey.EC_OpenSSL_NID <> TAccountKey.Empty.EC_OpenSSL_NID) and
    (PaccAccountToBuy^.AccountInfo.NewPublicKey <> ANewKey) then
  begin
    RErrors := 'New public key is not equal to allowed new public key for account';
    exit;
  end;
  // Buy an account applies when account_to_buy.amount + operation amount >= price
  // Then, account_to_buy.amount will be (account_to_buy.amount + amount - price)
  // and buyer.amount will be buyer.amount + price
  if (PaccAccountToBuy^.AccountInfo.Price > (PaccAccountToBuy^.Balance + AAmount)) then
  begin
    RErrors := 'Account price ' + TCurrencyUtils.CurrencyToString(PaccAccountToBuy^.AccountInfo.Price) + ' < balance ' +
      TCurrencyUtils.CurrencyToString(PaccAccountToBuy^.Balance) + ' + amount ' + TCurrencyUtils.CurrencyToString(AAmount);
    exit;
  end;

  if PaccBuyer^.UpdatedBlock <> FFreezedAccounts.BlocksCount then
  begin
    PaccBuyer^.PreviusUpdatedBlock := PaccBuyer^.UpdatedBlock;
    PaccBuyer^.UpdatedBlock := FFreezedAccounts.BlocksCount;
  end;

  if PaccAccountToBuy^.UpdatedBlock <> FFreezedAccounts.BlocksCount then
  begin
    PaccAccountToBuy^.PreviusUpdatedBlock := PaccAccountToBuy^.UpdatedBlock;
    PaccAccountToBuy^.UpdatedBlock := FFreezedAccounts.BlocksCount;
  end;

  if PaccSeller^.UpdatedBlock <> FFreezedAccounts.BlocksCount then
  begin
    PaccSeller^.PreviusUpdatedBlock := PaccSeller^.UpdatedBlock;
    PaccSeller^.UpdatedBlock := FFreezedAccounts.BlocksCount;
  end;

  // Inc buyer n_operation
  PaccBuyer^.NumberOfTransactions := ANumberOfTransactions;
  // Set new balance values
  PaccBuyer^.Balance := PaccBuyer^.Balance - (AAmount + AFee);
  PaccAccountToBuy^.Balance := PaccAccountToBuy^.Balance + AAmount - PaccAccountToBuy^.AccountInfo.Price;
  PaccSeller^.Balance := PaccSeller^.Balance + PaccAccountToBuy^.AccountInfo.Price;

  // After buy, account will be unlocked and set to normal state and new account public key changed
  PaccAccountToBuy^.AccountInfo := CT_AccountInfo_NUL;
  PaccAccountToBuy^.AccountInfo.State := as_Normal;
  PaccAccountToBuy^.AccountInfo.AccountKey := ANewKey;

  Dec(FTotalBalance, AFee);
  inc(FTotalFee, AFee);
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

function TAccountTransaction.Commit(const FBlockLock: TBlockHeader; var RErrors: AnsiString): Boolean;
var
  i: Integer;
  b: TAccountStorageEntry;
  Pa : PAccount;
begin
  Result := false;
  RErrors := '';
  FFreezedAccounts.StartThreadSafe;
  try
    if not CheckIntegrity then
    begin
      RErrors := 'Invalid integrity in accounts transaction on commit';
      exit;
    end;
    for i := 0 to FOrderedList.Count - 1 do
    begin
      Pa := FOrderedList.GetPointer(i);
      FreezedAccounts.UpdateAccount(Pa^.AccountNumber, Pa^.AccountInfo, Pa^.Name, Pa^.AccountType, Pa^.Balance, Pa^.NumberOfTransactions);
    end;
    //
    if (FFreezedAccounts.TotalBalance <> FTotalBalance) then
    begin
      TLog.NewLog(ltError, Classname, Format('Invalid integrity balance! StrongBox:%d Transaction:%d',
        [FFreezedAccounts.TotalBalance, FTotalBalance]));
    end;
    if (FreezedAccounts.TotalFee <> TotalFee) then
    begin
      TLog.NewLog(ltError, Classname, Format('Invalid integrity fee! StrongBox:%d Transaction:%d',
        [FFreezedAccounts.TotalFee, FTotalFee]));
    end;
    b := FFreezedAccounts.AddNew(FBlockLock);
    if (b.Accounts[0].Balance <> (FBlockLock.reward + FTotalFee)) then
    begin
      TLog.NewLog(ltError, Classname,
        Format('Invalid integrity reward! Account:%d Balance:%d  Reward:%d Fee:%d (Reward+Fee:%d)',
        [b.Accounts[0].AccountNumber, b.Accounts[0].Balance, FBlockLock.reward, FTotalFee,
        FBlockLock.reward + FTotalFee]));
    end;
    CleanTransaction;
    //
    if (FFreezedAccounts.CurrentProtocol < cPROTOCOL_2) and (FBlockLock.protocol_version = cPROTOCOL_2) then
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

procedure TAccountTransaction.CopyFrom(Transaction: TAccountTransaction);
var
  i: Integer;
  P: PAccount;
begin
  if Transaction = Self then
    exit;
  if Transaction.FFreezedAccounts <> FFreezedAccounts then
    raise Exception.Create('Invalid Freezed accounts to copy');
  CleanTransaction;
  for i := 0 to Transaction.FOrderedList.Count - 1 do
  begin
    P := Transaction.FOrderedList.GetPointer(i);
    FOrderedList.Add(P^);
  end;
  FPreviusHash := Transaction.FPreviusHash;
  FTotalBalance := Transaction.FTotalBalance;
  FTotalFee := Transaction.FTotalFee;
end;

constructor TAccountTransaction.Create(AAccountStorage: TAccountStorage);
begin
  FOrderedList := TOrderedAccountList.Create;
  FFreezedAccounts := AAccountStorage;
  FPreviusHash := AAccountStorage.AccountStorageHash;
  FTotalBalance := FFreezedAccounts.TotalBalance;
  FTotalFee := 0;
  FAccountNames_Added := TOrderedRawList.Create;
  FAccountNames_Deleted := TOrderedRawList.Create;
end;

destructor TAccountTransaction.Destroy;
begin
  //CleanTransaction;
  if assigned(FOrderedList) then FreeAndNil(FOrderedList);
  FreeAndNil(FAccountNames_Added);
  FreeAndNil(FAccountNames_Deleted);
  inherited;
end;

function TAccountTransaction.GetInternalAccount(AAccountNumber: Cardinal): PAccount;
var
  i: Integer;
begin
  if FOrderedList.Find(AAccountNumber, i)
  then begin
    Result := FOrderedList.GetPointer(i)
  end else begin
    i := FOrderedList.Add(FreezedAccountStorage.Accounts[AAccountNumber]);
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

function TAccountTransaction.TransferAmount(ASender, ATarget: Cardinal; ANumberOfTransactions: Cardinal; AAmount, AFee: UInt64;
  var RErrors: AnsiString): Boolean;
var
  PaccSender, PaccTarget: PAccount;
begin
  Result := false;
  RErrors := '';
  if not CheckIntegrity then
  begin
    RErrors := 'Invalid integrity in accounts transaction';
    exit;
  end;
  if (ASender >= (FFreezedAccounts.BlocksCount * cAccountsPerBlock)) or
    (ATarget >= (FFreezedAccounts.BlocksCount * cAccountsPerBlock)) then
  begin
    RErrors := 'Invalid sender or target on transfer';
    exit;
  end;
  if TAccount.IsAccountBlockedByProtocol(ASender, FFreezedAccounts.BlocksCount) then
  begin
    RErrors := 'Sender account is blocked for protocol';
    exit;
  end;
  if TAccount.IsAccountBlockedByProtocol(ATarget, FFreezedAccounts.BlocksCount) then
  begin
    RErrors := 'Target account is blocked for protocol';
    exit;
  end;
  PaccSender := GetInternalAccount(ASender);
  PaccTarget := GetInternalAccount(ATarget);
  if (PaccSender^.NumberOfTransactions + 1 <> ANumberOfTransactions) then
  begin
    RErrors := 'Incorrect n_operation';
    exit;
  end;
  if (PaccSender^.Balance < (AAmount + AFee)) then
  begin
    RErrors := 'Insuficient founds';
    exit;
  end;
  if ((PaccTarget^.Balance + AAmount) > cMaxWalletAmount) then
  begin
    RErrors := 'Max account balance';
    exit;
  end;
  if (AFee > cMaxTransactionFee) then
  begin
    RErrors := 'Max fee';
    exit;
  end;
  if (PaccSender^.AccountInfo.IsLocked(FFreezedAccounts.BlocksCount)) then
  begin
    RErrors := 'Sender account is locked until block ' + inttostr(PaccSender^.AccountInfo.LockedUntilBlock);
    exit;
  end;

  if PaccSender^.UpdatedBlock <> FFreezedAccounts.BlocksCount then
  begin
    PaccSender^.PreviusUpdatedBlock := PaccSender^.UpdatedBlock;
    PaccSender^.UpdatedBlock := FFreezedAccounts.BlocksCount;
  end;

  if PaccTarget^.UpdatedBlock <> FFreezedAccounts.BlocksCount then
  begin
    PaccTarget^.PreviusUpdatedBlock := PaccTarget.UpdatedBlock;
    PaccTarget^.UpdatedBlock := FFreezedAccounts.BlocksCount;
  end;

  PaccSender^.NumberOfTransactions := ANumberOfTransactions;
  PaccSender^.Balance := PaccSender^.Balance - (AAmount + AFee);
  PaccTarget^.Balance := PaccTarget^.Balance + (AAmount);
  Dec(FTotalBalance, AFee);
  inc(FTotalFee, AFee);
  Result := true;
end;

function TAccountTransaction.UpdateAccountInfo(ASignerAccount, ASignerAccountNumberOfTransactions, ATargetAccount: Cardinal;
  AAccountInfo: TAccountInfo; ANewName: TRawBytes; ANewType: Word; AFee: UInt64; var RErrors: AnsiString): Boolean;
var
  i: Integer;
  P_signer, P_target: PAccount;
begin
  Result := false;
  RErrors := '';
  if (ASignerAccount >= (FFreezedAccounts.BlocksCount * cAccountsPerBlock)) or
    (ATargetAccount >= (FFreezedAccounts.BlocksCount * cAccountsPerBlock)) then
  begin
    RErrors := 'Invalid account';
    exit;
  end;
  if (TAccount.IsAccountBlockedByProtocol(ASignerAccount, FFreezedAccounts.BlocksCount)) or
    (TAccount.IsAccountBlockedByProtocol(ATargetAccount, FFreezedAccounts.BlocksCount)) then
  begin
    RErrors := 'account is blocked for protocol';
    exit;
  end;
  P_signer := GetInternalAccount(ASignerAccount);
  P_target := GetInternalAccount(ATargetAccount);
  if (P_signer^.NumberOfTransactions + 1 <> ASignerAccountNumberOfTransactions)
  then begin
    RErrors := 'Incorrect n_operation';
    exit;
  end;
  if (P_signer^.Balance < AFee)
  then begin
    RErrors := 'Insuficient founds';
    exit;
  end;
  if (P_signer^.AccountInfo.IsLocked(FFreezedAccounts.BlocksCount))
  then begin
    RErrors := 'Signer account is locked until block ' + inttostr(P_signer^.AccountInfo.LockedUntilBlock);
    exit;
  end;
  if (P_target^.AccountInfo.IsLocked(FFreezedAccounts.BlocksCount))
  then begin
    RErrors := 'Target account is locked until block ' + inttostr(P_target^.AccountInfo.LockedUntilBlock);
    exit;
  end;
  if P_signer^.UpdatedBlock <> FFreezedAccounts.BlocksCount
  then begin
    P_signer^.PreviusUpdatedBlock := P_signer^.UpdatedBlock;
    P_signer^.UpdatedBlock := FFreezedAccounts.BlocksCount;
  end;
  if (ASignerAccount <> ATargetAccount)
  then begin
    if P_target^.UpdatedBlock <> FFreezedAccounts.BlocksCount
    then begin
      P_target^.PreviusUpdatedBlock := P_target^.UpdatedBlock;
      P_target^.UpdatedBlock := FFreezedAccounts.BlocksCount;
    end;
  end;
  if P_signer^.AccountInfo.AccountKey <> P_target^.AccountInfo.AccountKey
  then begin
    RErrors := 'Signer and target have diff key';
    exit;
  end;
  if (ANewName <> P_target^.Name)
  then begin
    // NEW NAME CHANGE CHECK:
    if (ANewName <> '') then
    begin
      if not FFreezedAccounts.IsValidAccountName(ANewName, RErrors) then
      begin
        RErrors := 'Invalid account name "' + ANewName + '" length:' + inttostr(length(ANewName)) + ': ' + RErrors;
        exit;
      end;
      i := FFreezedAccounts.FindAccountByName(ANewName);
      if (i >= 0) then
      begin
        // This account name is in the safebox... check if deleted:
        i := FAccountNames_Deleted.IndexOf(ANewName);
        if i < 0 then
        begin
          RErrors := 'Account name "' + ANewName + '" is in current use';
          exit;
        end;
      end;
      i := FAccountNames_Added.IndexOf(ANewName);
      if (i >= 0) then
      begin
        // This account name is added in this transaction! (perhaps deleted also, but does not allow to "double add same name in same block")
        RErrors := 'Account name "' + ANewName + '" is in same transaction';
        exit;
      end;
    end;
    // Ok, include
    if (P_target^.Name <> '')
    then FAccountNames_Deleted.Add(P_target^.Name, ATargetAccount);
    if (ANewName <> '')
    then FAccountNames_Added.Add(ANewName, ATargetAccount);
  end;

  P_signer^.NumberOfTransactions := ASignerAccountNumberOfTransactions;
  P_target^.AccountInfo := AAccountInfo;
  P_target^.Name := ANewName;
  P_target^.AccountType := ANewType;
  Dec(P_signer^.Balance, AFee); // Signer is who pays the fee
  Dec(FTotalBalance, AFee);
  inc(FTotalFee, AFee);
  Result := true;
end;

end.
