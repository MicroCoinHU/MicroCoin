unit MicroCoin.Transaction.RecoverFounds;

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
  MicroCoin.Common, MicroCoin.Account.Transaction,
  MicroCoin.Account.Data,
  Sysutils, classes, UCrypto, ULog, UConst, MicroCoin.Transaction.Manager;

type

  TRecoverFoundsData = record
    Account: Cardinal;
    NumberOfTransactions: Cardinal;
    Fee: UInt64;
  end;

  TRecoverFoundsTransaction = class(TTransaction)
  private
    FData: TRecoverFoundsData;
  protected
    procedure InitializeData; override;
    function SaveToStream(Stream: TStream; SaveExtendedData: Boolean): Boolean; override;
    function LoadFromStream(Stream: TStream; LoadExtendedData: Boolean): Boolean; override;
    function GetAmount: Int64; override;
    function GetFee: UInt64; override;
    function GetPayload: TRawBytes; override;
    function GetSignerAccount: Cardinal; override;
    function GetNumberOfTransactions: Cardinal; override;
    function GetTransactionType: Byte; override;
  public
    constructor Create(account_number, n_operation: Cardinal; fee: UInt64);

    function GetBuffer(UseProtocolV2: Boolean): TRawBytes; override;
    function ApplyTransaction(AccountTransaction: TAccountTransaction; var errors: AnsiString): Boolean; override;
    procedure AffectedAccounts(list: TList); override;
    function GetTransactionData(Block: Cardinal; Affected_account_number: Cardinal;
      var TransactionData: TTransactionData): Boolean; override;
    function toString: string; override;

    property Data: TRecoverFoundsData read FData;
  end;

const
  CT_TOpRecoverFoundsData_NUL: TRecoverFoundsData = (Account: 0; NumberOfTransactions: 0; Fee: 0);

implementation

procedure TRecoverFoundsTransaction.AffectedAccounts(list: TList);
begin
  list.Add(TObject(FData.Account));
end;

constructor TRecoverFoundsTransaction.Create(account_number, n_operation: Cardinal; fee: UInt64);
begin
  inherited Create;
  FData.Account := account_number;
  FData.NumberOfTransactions := n_operation;
  FData.Fee := fee;
  FHasValidSignature := true; // Recover founds doesn't need a signature
end;

function TRecoverFoundsTransaction.ApplyTransaction(AccountTransaction: TAccountTransaction;
  var errors: AnsiString): Boolean;
var
  acc: TAccount;
begin
  Result := false;
  if TAccount.IsAccountBlockedByProtocol(FData.Account, AccountTransaction.FreezedAccountStorage.BlocksCount) then
  begin
    errors := 'account is blocked for protocol';
    Exit;
  end;
  acc := AccountTransaction.Account(FData.Account);
  if (acc.UpdatedBlock + CT_RecoverFoundsWaitInactiveCount >= AccountTransaction.FreezedAccountStorage.BlocksCount)
  then
  begin
    errors := Format('Account is active to recover founds! Account %d Updated %d + %d >= BlockCount : %d',
      [FData.Account, acc.UpdatedBlock, CT_RecoverFoundsWaitInactiveCount,
      AccountTransaction.FreezedAccountStorage.BlocksCount]);
    Exit;
  end;
  // Build 1.0.8 ... there was a BUG. Need to prevent recent created accounts
  if (TAccount.AccountBlock(FData.Account) + CT_RecoverFoundsWaitInactiveCount >=
    AccountTransaction.FreezedAccountStorage.BlocksCount) then
  begin
    errors := Format('AccountBlock is active to recover founds! AccountBlock %d + %d >= BlockCount : %d',
      [TAccount.AccountBlock(FData.Account), CT_RecoverFoundsWaitInactiveCount,
      AccountTransaction.FreezedAccountStorage.BlocksCount]);
    Exit;
  end;
  if ((acc.NumberOfTransactions + 1) <> FData.NumberOfTransactions) then
  begin
    errors := 'Invalid n_operation';
    Exit;
  end;
  if (FData.Fee <= 0) or (FData.Fee > CT_MaxTransactionFee) then
  begin
    errors := 'Invalid fee ' + Inttostr(FData.Fee);
    Exit;
  end;
  if (acc.Balance < FData.Fee) then
  begin
    errors := 'Insuficient founds';
    Exit;
  end;
  FPrevious_Signer_updated_block := acc.UpdatedBlock;
  Result := AccountTransaction.TransferAmount(FData.Account, FData.Account, FData.NumberOfTransactions, 0, FData.Fee, errors);
end;

function TRecoverFoundsTransaction.GetBuffer(UseProtocolV2: Boolean): TRawBytes;
var
  ms: TMemoryStream;
begin
  if UseProtocolV2 then
    Result := inherited GetBuffer(UseProtocolV2)
  else
  begin
    ms := TMemoryStream.Create;
    try
      ms.Write(FData.Account, Sizeof(FData.Account));
      ms.Write(FData.NumberOfTransactions, Sizeof(FData.NumberOfTransactions));
      ms.Write(FData.Fee, Sizeof(FData.Fee));
      ms.Position := 0;
      SetLength(Result, ms.Size);
      ms.ReadBuffer(Result[1], ms.Size);
    finally
      ms.Free;
    end;
  end;
end;

procedure TRecoverFoundsTransaction.InitializeData;
begin
  inherited;
  FData := CT_TOpRecoverFoundsData_NUL;
end;

function TRecoverFoundsTransaction.LoadFromStream(Stream: TStream; LoadExtendedData: Boolean): Boolean;
begin
  Result := false;
  if Stream.Size - Stream.Position < 16 then
    Exit;
  Stream.Read(FData.Account, Sizeof(FData.Account));
  Stream.Read(FData.NumberOfTransactions, Sizeof(FData.NumberOfTransactions));
  Stream.Read(FData.Fee, Sizeof(FData.Fee));
  Result := true;
end;

function TRecoverFoundsTransaction.GetAmount: Int64;
begin
  Result := 0;
end;

function TRecoverFoundsTransaction.GetFee: UInt64;
begin
  Result := FData.Fee;
end;

function TRecoverFoundsTransaction.GetPayload: TRawBytes;
begin
  Result := '';
end;

function TRecoverFoundsTransaction.GetTransactionType: Byte;
begin
  Result := CT_Op_Recover;
end;

function TRecoverFoundsTransaction.SaveToStream(Stream: TStream; SaveExtendedData: Boolean): Boolean;
begin
  Stream.Write(FData.Account, Sizeof(FData.Account));
  Stream.Write(FData.NumberOfTransactions, Sizeof(FData.NumberOfTransactions));
  Stream.Write(FData.Fee, Sizeof(FData.Fee));
  Result := true;
end;

function TRecoverFoundsTransaction.GetSignerAccount: Cardinal;
begin
  Result := FData.Account;
end;

function TRecoverFoundsTransaction.GetTransactionData(Block, Affected_account_number: Cardinal;
  var TransactionData: TTransactionData): Boolean;
begin
  TransactionData := TTransactionData.Empty;
  TransactionData.transactionSubtype := CT_OpSubtype_Recover;
  TransactionData.TransactionAsString := 'Recover founds';
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

function TRecoverFoundsTransaction.GetNumberOfTransactions: Cardinal;
begin
  Result := FData.NumberOfTransactions;
end;

function TRecoverFoundsTransaction.toString: string;
begin
  Result := Format('Recover founds of account %s fee:%s (n_op:%d)',
    [TAccount.AccountNumberToString(FData.Account), TCurrencyUtils.CurrencyToString(FData.Fee),
    FData.NumberOfTransactions]);
end;

initialization

TTransactionManager.RegisterTransactionPlugin(TRecoverFoundsTransaction, CT_Op_Recover);

end.
