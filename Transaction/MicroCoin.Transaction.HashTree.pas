unit MicroCoin.Transaction.HashTree;

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

uses SysUtils, UCrypto, Classes, MicroCoin.Transaction.Base, UThread,
  MicroCoin.Transaction.ITransaction, UBaseTypes,
  MicroCoin.Transaction.Manager, MicroCoin.Transaction.Transaction, SyncObjs;

type
  //TODO: Refcount
  TTransactionHashTree = class
  private
    FHashTreeTransactions: TPCThreadList;
    FHashTree: TRawBytes;
    FOnChanged: TNotifyEvent;
    FTotalAmount: Int64;
    FTotalFee: Int64;
    procedure InternalAddTransactionToHashTree(AList: TList; ATransactioon: ITransaction);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddTransactionToHashTree(ATransaction: ITransaction);
    procedure ClearHastThree;
    function TransactionCount: Integer;
    function GetTransaction(index: Integer): ITransaction;
    function GetTransactionsAffectingAccount(account_number: Cardinal; list: TList): Integer;
    procedure CopyFromHashTree(Sender: TTransactionHashTree);
    function SaveToStream(Stream: TStream; SaveToStorage: Boolean): Boolean;
    function LoadFromStream(Stream: TStream; LoadingFromStorage, LoadProtocolV2: Boolean;
      var errors: AnsiString): Boolean;
    function IndexOf(ATransaction: ITransaction): Integer;
    function TransactionCountsWithoutFeeBySameSigner(account_number: Cardinal): Integer;
    procedure Delete(index: Integer);

    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property HashTree: TRawBytes read FHashTree;
    property TotalAmount: Int64 read FTotalAmount;
    property TotalFee: Int64 read FTotalFee;
  end;

type
  TTransactionHashTreeReg = record
    transaction: ITransaction;
  end;

  PTransacionHashTreeReg = ^TTransactionHashTreeReg;

implementation

procedure TTransactionHashTree.AddTransactionToHashTree(ATransaction: ITransaction);
var
  l: TList;
begin
  l := FHashTreeTransactions.LockList;
  try
    InternalAddTransactionToHashTree(l, ATransaction);
  finally
    FHashTreeTransactions.UnlockList;
  end;
end;

procedure TTransactionHashTree.ClearHastThree;
var
  l: TList;
  i: Integer;
  P: PTransacionHashTreeReg;
begin
  l := FHashTreeTransactions.LockList;
  try
    FTotalAmount := 0;
    FTotalFee := 0;
    try
      for i := 0 to l.Count - 1 do
      begin
        P := l[i];
        P^.transaction := nil;
        P^:=Default(TTransactionHashTreeReg);
        Dispose(P);
        P := nil;
      end;
    finally
      l.Clear;
      FHashTree := TCrypto.DoSha256('');
    end;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  finally
    FHashTreeTransactions.UnlockList;
  end;
end;

procedure TTransactionHashTree.CopyFromHashTree(Sender: TTransactionHashTree);
var
  i: Integer;
  lme, lsender: TList;
  PSender: PTransacionHashTreeReg;
  lastNE: TNotifyEvent;
begin
  if (Sender = Self) then
  begin
    exit;
  end;
  ClearHastThree;
  lme := FHashTreeTransactions.LockList;
  try
    lastNE := FOnChanged;
    FOnChanged := nil;
    try
      lsender := Sender.FHashTreeTransactions.LockList;
      try
        for i := 0 to lsender.Count - 1 do
        begin
          PSender := lsender[i];
          InternalAddTransactionToHashTree(lme, PSender^.transaction);
        end;
      finally
        Sender.FHashTreeTransactions.UnlockList;
      end;
    finally
      FOnChanged := lastNE;
    end;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  finally
    FHashTreeTransactions.UnlockList;
  end;
end;

constructor TTransactionHashTree.Create;
begin
  FOnChanged := nil;
  FTotalAmount := 0;
  FTotalFee := 0;
  FHashTree := TCrypto.DoSha256('');
  FHashTreeTransactions := TPCThreadList.Create('TOperationsHashTree_HashTreeOperations');
end;

procedure TTransactionHashTree.Delete(index: Integer);
var
  l: TList;
  P: PTransacionHashTreeReg;
  i: Integer;
begin
  l := FHashTreeTransactions.LockList;
  try
    P := l[index];
    l.Delete(index);
    // P^.op.Free;
    P^.transaction := nil;
    P^:=Default(TTransactionHashTreeReg);
    Dispose(P);
    // Recalc operations hash
    FTotalAmount := 0;
    FTotalFee := 0;
    FHashTree := '';
    for i := 0 to l.Count - 1 do
    begin
      P := l[i];
      // Include to hash tree
      FHashTree := TCrypto.DoSha256(FHashTree + P^.transaction.Sha256);
      P^.transaction.Tag := i;
      inc(FTotalAmount, P^.transaction.Amount);
      inc(FTotalFee, P^.transaction.Fee);
    end;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  finally
    FHashTreeTransactions.UnlockList;
  end;
end;

destructor TTransactionHashTree.Destroy;
begin
  FOnChanged := nil;
  ClearHastThree;
  FreeAndNil(FHashTreeTransactions);
  SetLength(FHashTree, 0);
  inherited;
end;

function TTransactionHashTree.GetTransaction(index: Integer): ITransaction;
var
  l: TList;
begin
  l := FHashTreeTransactions.LockList;
  try
    Result := PTransacionHashTreeReg(l[index])^.transaction;
  finally
    FHashTreeTransactions.UnlockList;
  end;
end;

function TTransactionHashTree.GetTransactionsAffectingAccount(account_number: Cardinal; list: TList): Integer;
// This function retrieves operations from HashTree that affeccts to an account_number
var
  l, intl: TList;
  i: Integer;
begin
  list.Clear;
  l := FHashTreeTransactions.LockList;
  try
    intl := TList.Create;
    try
      for i := 0 to l.Count - 1 do
      begin
        intl.Clear;
        PTransacionHashTreeReg(l[i])^.transaction.AffectedAccounts(intl);
        if intl.IndexOf(TObject(account_number)) >= 0 then
          list.Add(TObject(i));
      end;
    finally
      intl.Free;
    end;
    Result := list.Count;
  finally
    FHashTreeTransactions.UnlockList;
  end;
end;

function TTransactionHashTree.IndexOf(ATransaction: ITransaction): Integer;
var
  l: TList;
  OpSha256: TRawBytes;
begin
  OpSha256 := ATransaction.Sha256;
  l := FHashTreeTransactions.LockList;
  try
    for Result := 0 to l.Count - 1 do
    begin
      if PTransacionHashTreeReg(l[Result])^.transaction.Sha256 = OpSha256 then
        exit;
    end;
    Result := -1;
  finally
    FHashTreeTransactions.UnlockList;
  end;
end;

function TTransactionHashTree.TransactionCountsWithoutFeeBySameSigner(account_number: Cardinal): Integer;
var
  l: TList;
  i: Integer;
begin
  Result := 0;
  l := FHashTreeTransactions.LockList;
  try
    for i := 0 to l.Count - 1 do
    begin
      if (PTransacionHashTreeReg(l[i])^.transaction.SignerAccount = account_number) and (PTransacionHashTreeReg(l[i])^.transaction.Fee = 0)
      then inc(Result);
    end;
  finally
    FHashTreeTransactions.UnlockList;
  end;
end;

procedure TTransactionHashTree.InternalAddTransactionToHashTree(AList: TList; ATransactioon: ITransaction);
var
  msCopy: TMemoryStream;
  h: TRawBytes;
  P: PTransacionHashTreeReg;
  op2: ITransaction;
begin
  msCopy := TMemoryStream.Create;
  try
    New(P);
    P^.transaction := TTransactionManager.CreateTransaction(ATransactioon.TransactionType);
    P^.transaction.InitializeData;
    ATransactioon.SaveToStream(msCopy, true);
    msCopy.Position := 0;
    P^.transaction.LoadFromStream(msCopy, true);
    P^.transaction.Previous_Signer_updated_block := ATransactioon.Previous_Signer_updated_block;
    P^.transaction.Previous_Destination_updated_block := ATransactioon.Previous_Destination_updated_block;
    P^.transaction.Previous_Seller_updated_block := ATransactioon.Previous_Seller_updated_block;
    h := ATransactioon.Sha256;
    P^.transaction.Tag := AList.Count;
    // Include to hash tree
    FHashTree := TCrypto.DoSha256(FHashTree + h);
    AList.Add(P);
  finally
    msCopy.Free;
  end;
  inc(FTotalAmount, ATransactioon.Amount);
  inc(FTotalFee, ATransactioon.Fee);
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TTransactionHashTree.LoadFromStream(Stream: TStream; LoadingFromStorage, LoadProtocolV2: Boolean;
  var errors: AnsiString): Boolean;
var
  c, i: Cardinal;
  xTransactionType: Cardinal;
  xTransaction: ITransaction;
  xTransactionClass: TTransactionClass;
  xLastEvent: TNotifyEvent;
begin
  Result := false;
  //
  if Stream.Read(c, 4) < 4 then
  begin
    errors := 'Cannot read operations count';
    exit;
  end;
  xLastEvent := FOnChanged;
  FOnChanged := nil;
  try
    // c = operations count
    for i := 1 to c do
    begin
      errors := 'Invalid operation structure ' + Inttostr(i) + '/' + Inttostr(c);
      if Stream.Size - Stream.Position < 4 then
        exit;
      Stream.Read(xTransactionType, 4);
      xTransactionClass := TTransactionManager.GetTransactionPlugin(xTransactionType);
      if not Assigned(xTransactionClass) then
      begin
        errors := errors + ' optype not valid:' + InttoHex(xTransactionType, 4);
        exit;
      end;
      errors := 'Invalid operation load from stream ' + Inttostr(i) + '/' + Inttostr(c) + ' Class:' + xTransactionClass.Classname;
      xTransaction := xTransactionClass.Create;
      if LoadingFromStorage then
      begin
        if not xTransaction.LoadFromStorage(Stream, LoadProtocolV2) then
          exit;
      end
      else if not xTransaction.LoadFromNettransfer(Stream) then
      begin
        exit;
      end;
      AddTransactionToHashTree(xTransaction);
    end;
  finally
    FOnChanged := xLastEvent;
  end;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
  Result := true;
end;

function TTransactionHashTree.TransactionCount: Integer;
var
  l: TList;
begin
  l := FHashTreeTransactions.LockList;
  try
    Result := l.Count;
  finally
    FHashTreeTransactions.UnlockList;
  end;
end;

function TTransactionHashTree.SaveToStream(Stream: TStream; SaveToStorage: Boolean): Boolean;
var
  c, i, xTransactionType: Cardinal;
  xTransaction: ITransaction;
  xList: TList;
begin
  xList := FHashTreeTransactions.LockList;
  try
    c := xList.Count;
    Stream.Write(c, 4);
    // c = operations count
    for i := 1 to c do
    begin
      xTransaction := GetTransaction(i - 1);
      xTransactionType := xTransaction.TransactionType;
      Stream.Write(xTransactionType, 4);
      if SaveToStorage then
        xTransaction.SaveToStorage(Stream)
      else
        xTransaction.SaveToNettransfer(Stream);
    end;
    Result := true;
  finally
    FHashTreeTransactions.UnlockList;
  end;
end;

end.
