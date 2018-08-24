unit MicroCoin.Transaction.RecoverFounds;

{
  This unit contains code from PascalCoin:

  Copyright (c) Albert Molina 2016 - 2018 original code from PascalCoin https://pascalcoin.org/

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

}

interface

uses MicroCoin.Transaction.Base, MicroCoin.Transaction.Transaction,
  MicroCoin.Common, MicroCoin.Account.Transaction,
  MicroCoin.Account,
  Sysutils, classes, UCrypto, ULog, UConst, MicroCoin.Transaction.Manager;

type

  TRecoverFoundsData = record
    account: Cardinal;
    n_operation: Cardinal;
    fee: UInt64;
  end;

  TRecoverFoundsTransaction = class(TTransaction)
  private
    FData: TRecoverFoundsData;
  protected
    procedure InitializeData; override;
    function SaveToStream(Stream: TStream; SaveExtendedData: Boolean): Boolean; override;
    function LoadFromStream(Stream: TStream; LoadExtendedData: Boolean): Boolean; override;
  public
    function GetOpType: Byte; override;

    function GetBufferForOpHash(UseProtocolV2: Boolean): TRawBytes; override;
    function ApplyTransaction(AccountTransaction: TAccountTransaction; var errors: AnsiString): Boolean; override;
    function GetAmount: Int64; override;
    function GetFee: UInt64; override;
    function GetPayload: TRawBytes; override;
    function GetSignerAccount: Cardinal; override;
    function GetNumberOfTransactions: Cardinal; override;
    procedure AffectedAccounts(list: TList); override;
    constructor Create(account_number, n_operation: Cardinal; fee: UInt64);
    function GetTransactionData(Block: Cardinal;
      Affected_account_number: Cardinal;
      var TransactionData: TTransactionData): Boolean; override;
    property Data: TRecoverFoundsData read FData;
    function toString: string; override;

  end;

const
  CT_TOpRecoverFoundsData_NUL: TRecoverFoundsData = (account: 0; n_operation: 0; fee: 0);

implementation

procedure TRecoverFoundsTransaction.AffectedAccounts(list: TList);
begin
  list.Add(TObject(FData.account));
end;

constructor TRecoverFoundsTransaction.Create(account_number, n_operation: Cardinal; fee: UInt64);
begin
  inherited Create;
  FData.account := account_number;
  FData.n_operation := n_operation;
  FData.fee := fee;
  FHasValidSignature := true; // Recover founds doesn't need a signature
end;

function TRecoverFoundsTransaction.ApplyTransaction(AccountTransaction: TAccountTransaction; var errors: AnsiString): Boolean;
var
  acc: TAccount;
begin
  Result := false;
  if TAccount.IsAccountBlockedByProtocol(FData.account, AccountTransaction.FreezedAccountStorage.BlocksCount) then
  begin
    errors := 'account is blocked for protocol';
    Exit;
  end;
  acc := AccountTransaction.account(FData.account);
  if (acc.updated_block + CT_RecoverFoundsWaitInactiveCount >= AccountTransaction.FreezedAccountStorage.BlocksCount) then
  begin
    errors := Format('Account is active to recover founds! Account %d Updated %d + %d >= BlockCount : %d', [FData.account, acc.updated_block, CT_RecoverFoundsWaitInactiveCount,
      AccountTransaction.FreezedAccountStorage.BlocksCount]);
    Exit;
  end;
  // Build 1.0.8 ... there was a BUG. Need to prevent recent created accounts
  if (TAccount.AccountBlock(FData.account) + CT_RecoverFoundsWaitInactiveCount >= AccountTransaction.FreezedAccountStorage.BlocksCount) then
  begin
    errors := Format('AccountBlock is active to recover founds! AccountBlock %d + %d >= BlockCount : %d', [TAccount.AccountBlock(FData.account), CT_RecoverFoundsWaitInactiveCount,
      AccountTransaction.FreezedAccountStorage.BlocksCount]);
    Exit;
  end;
  if ((acc.n_operation + 1) <> FData.n_operation) then
  begin
    errors := 'Invalid n_operation';
    Exit;
  end;
  if (FData.fee <= 0) or (FData.fee > CT_MaxTransactionFee) then
  begin
    errors := 'Invalid fee ' + Inttostr(FData.fee);
    Exit;
  end;
  if (acc.balance < FData.fee) then
  begin
    errors := 'Insuficient founds';
    Exit;
  end;
  FPrevious_Signer_updated_block := acc.updated_block;
  Result := AccountTransaction.TransferAmount(FData.account, FData.account, FData.n_operation, 0, FData.fee, errors);
end;

function TRecoverFoundsTransaction.GetBufferForOpHash(UseProtocolV2: Boolean): TRawBytes;
var
  ms: TMemoryStream;
begin
  if UseProtocolV2 then
    Result := inherited GetBufferForOpHash(UseProtocolV2)
  else
  begin
    ms := TMemoryStream.Create;
    try
      ms.Write(FData.account, Sizeof(FData.account));
      ms.Write(FData.n_operation, Sizeof(FData.n_operation));
      ms.Write(FData.fee, Sizeof(FData.fee));
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
  Stream.Read(FData.account, Sizeof(FData.account));
  Stream.Read(FData.n_operation, Sizeof(FData.n_operation));
  Stream.Read(FData.fee, Sizeof(FData.fee));
  Result := true;
end;

function TRecoverFoundsTransaction.GetAmount: Int64;
begin
  Result := 0;
end;

function TRecoverFoundsTransaction.GetFee: UInt64;
begin
  Result := FData.fee;
end;

function TRecoverFoundsTransaction.GetPayload: TRawBytes;
begin
  Result := '';
end;

function TRecoverFoundsTransaction.GetOpType: Byte;
begin
  Result := CT_Op_Recover;
end;

function TRecoverFoundsTransaction.SaveToStream(Stream: TStream; SaveExtendedData: Boolean): Boolean;
begin
  Stream.Write(FData.account, Sizeof(FData.account));
  Stream.Write(FData.n_operation, Sizeof(FData.n_operation));
  Stream.Write(FData.fee, Sizeof(FData.fee));
  Result := true;
end;

function TRecoverFoundsTransaction.GetSignerAccount: Cardinal;
begin
  Result := FData.account;
end;

function TRecoverFoundsTransaction.GetTransactionData(Block,
  Affected_account_number: Cardinal;
  var TransactionData: TTransactionData): Boolean;
begin
  TransactionData.OpSubtype := CT_OpSubtype_Recover;
  TransactionData.OperationTxt := 'Recover founds';
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
  Result := FData.n_operation;
end;

function TRecoverFoundsTransaction.toString: string;
begin
  Result := Format('Recover founds of account %s fee:%s (n_op:%d)', [
    TAccount.AccountNumberToAccountTxtNumber(FData.account),
    TCurrencyUtils.FormatMoney(FData.fee), FData.n_operation]);
end;

initialization

TTransactionManager.RegisterTransactionPlugin(TRecoverFoundsTransaction, CT_Op_Recover);

end.
