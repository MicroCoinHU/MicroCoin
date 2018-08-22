unit MicroCoin.Transaction.HashTree;

{
  This unit contains code from PascalCoin:

  Copyright (c) Albert Molina 2016 - 2018 original code from PascalCoin https://pascalcoin.org/

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

}

interface

uses SysUtils, UCrypto, Classes, MicroCoin.Transaction.Base, UThread,
     MicroCoin.Transaction.Manager;
type
    TTransactionHashTree = Class
  private
    FHashTreeTransactions: TPCThreadList;
    FHashTree: TRawBytes;
    FOnChanged: TNotifyEvent;
    FTotalAmount: Int64;
    FTotalFee: Int64;
    Procedure InternalAddTransactionToHashTree(list: TList; op: ITransaction);
  public
    constructor Create;
    destructor Destroy; Override;
    procedure AddTransactionToHashTree(op: ITransaction);
    procedure ClearHastThree;
    property HashTree: TRawBytes read FHashTree;
    function OperationsCount: Integer;
    function GetOperation(index: Integer): ITransaction;
    function GetTransactionsAffectingAccount(account_number: Cardinal;
      list: TList): Integer;
    procedure CopyFromHashTree(Sender: TTransactionHashTree);
    property TotalAmount: Int64 read FTotalAmount;
    property TotalFee: Int64 read FTotalFee;
    function SaveToStream(Stream: TStream;
      SaveToStorage: Boolean): Boolean;
    function LoadFromStream(Stream: TStream;
      LoadingFromStorage, LoadProtocolV2: Boolean;
      var errors: AnsiString): Boolean;
    function IndexOf(op: ITransaction): Integer;
    function TransactionCountsWithoutFeeBySameSigner(account_number
      : Cardinal): Integer;
    procedure Delete(index: Integer);
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

Type
  TOperationHashTreeReg = Record
    op: ITransaction;
  end;

  POperationHashTreeReg = ^TOperationHashTreeReg;

implementation

procedure TTransactionHashTree.AddTransactionToHashTree(op: ITransaction);
Var
  l: TList;
begin
  l := FHashTreeTransactions.LockList;
  try
    InternalAddTransactionToHashTree(l, op);
  finally
    FHashTreeTransactions.UnlockList;
  end;
end;

procedure TTransactionHashTree.ClearHastThree;
var
  l: TList;
  i: Integer;
  P: POperationHashTreeReg;
begin
  l := FHashTreeTransactions.LockList;
  try
    FTotalAmount := 0;
    FTotalFee := 0;
    Try
      for i := 0 to l.Count - 1 do
      begin
        P := l[i];
        // P^.op.Free;
        Dispose(P);
      end;
    Finally
      l.Clear;
      FHashTree := TCrypto.DoSha256('');
    End;
    If Assigned(FOnChanged) then
      FOnChanged(Self);
  finally
    FHashTreeTransactions.UnlockList;
  end;
end;

procedure TTransactionHashTree.CopyFromHashTree(Sender: TTransactionHashTree);
Var
  i: Integer;
  lme, lsender: TList;
  PSender: POperationHashTreeReg;
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
    FOnChanged := Nil;
    try
      lsender := Sender.FHashTreeTransactions.LockList;
      try
        for i := 0 to lsender.Count - 1 do
        begin
          PSender := lsender[i];
          InternalAddTransactionToHashTree(lme, PSender^.op);
        end;
      finally
        Sender.FHashTreeTransactions.UnlockList;
      end;
    finally
      FOnChanged := lastNE;
    end;
    If Assigned(FOnChanged) then
      FOnChanged(Self);
  finally
    FHashTreeTransactions.UnlockList;
  end;
end;

constructor TTransactionHashTree.Create;
begin
  FOnChanged := Nil;
  FTotalAmount := 0;
  FTotalFee := 0;
  FHashTree := TCrypto.DoSha256('');
  FHashTreeTransactions := TPCThreadList.Create
    ('TOperationsHashTree_HashTreeOperations');
end;

procedure TTransactionHashTree.Delete(index: Integer);
Var
  l: TList;
  P: POperationHashTreeReg;
  i: Integer;
begin
  l := FHashTreeTransactions.LockList;
  try
    P := l[index];
    l.Delete(index);
    // P^.op.Free;
    Dispose(P);
    // Recalc operations hash
    FTotalAmount := 0;
    FTotalFee := 0;
    FHashTree := '';
    for i := 0 to l.Count - 1 do
    begin
      P := l[i];
      // Include to hash tree
      FHashTree := TCrypto.DoSha256(FHashTree + P^.op.Sha256);
      P^.op.Tag := i;
      inc(FTotalAmount, P^.op.Amount);
      inc(FTotalFee, P^.op.Fee);
    end;
    If Assigned(FOnChanged) then
      FOnChanged(Self);
  finally
    FHashTreeTransactions.UnlockList;
  end;
end;

destructor TTransactionHashTree.Destroy;
begin
  FOnChanged := Nil;
  ClearHastThree;
  FreeAndNil(FHashTreeTransactions);
  SetLength(FHashTree, 0);
  inherited;
end;

function TTransactionHashTree.GetOperation(index: Integer): ITransaction;
Var
  l: TList;
begin
  l := FHashTreeTransactions.LockList;
  try
    Result := POperationHashTreeReg(l[index])^.op;
  finally
    FHashTreeTransactions.UnlockList;
  end;
end;

function TTransactionHashTree.GetTransactionsAffectingAccount(account_number
  : Cardinal; list: TList): Integer;
// This function retrieves operations from HashTree that affeccts to an account_number
Var
  l, intl: TList;
  i, j: Integer;
begin
  list.Clear;
  l := FHashTreeTransactions.LockList;
  try
    intl := TList.Create;
    try
      for i := 0 to l.Count - 1 do
      begin
        intl.Clear;
        POperationHashTreeReg(l[i])^.op.AffectedAccounts(intl);
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

function TTransactionHashTree.IndexOf(op: ITransaction): Integer;
Var
  l: TList;
  OpSha256: TRawBytes;
begin
  OpSha256 := op.Sha256;
  l := FHashTreeTransactions.LockList;
  Try
    for Result := 0 to l.Count - 1 do
    begin
      if POperationHashTreeReg(l[Result])^.op.Sha256 = OpSha256 then
        exit;
    end;
    Result := -1;
  Finally
    FHashTreeTransactions.UnlockList;
  End;
end;

function TTransactionHashTree.TransactionCountsWithoutFeeBySameSigner
  (account_number: Cardinal): Integer;
Var
  l: TList;
  i: Integer;
begin
  Result := 0;
  l := FHashTreeTransactions.LockList;
  Try
    for i := 0 to l.Count - 1 do
    begin
      if (POperationHashTreeReg(l[i])^.op.SignerAccount = account_number) And
        (POperationHashTreeReg(l[i])^.op.Fee = 0) then
        inc(Result);
    end;
  Finally
    FHashTreeTransactions.UnlockList;
  End;
end;

procedure TTransactionHashTree.InternalAddTransactionToHashTree(list: TList; op: ITransaction);
Var
  msCopy: TMemoryStream;
  h: TRawBytes;
  P: POperationHashTreeReg;
begin
  msCopy := TMemoryStream.Create;
  try
    New(P);
    Supports(TInterfacedObject(op).NewInstance, ITransaction, P^.op);
    //P^.op := TInterFacedObject() as ITransaction;
    P^.op.InitializeData;
    op.SaveToStream(msCopy, true);
    msCopy.Position := 0;
    P^.op.LoadFromStream(msCopy, true);
    P^.op.Previous_Signer_updated_block := op.Previous_Signer_updated_block;
    P^.op.Previous_Destination_updated_block := op.Previous_Destination_updated_block;
    P^.op.Previous_Seller_updated_block := op.Previous_Seller_updated_block;
    h := op.Sha256;
    P^.op.tag := list.Count;
    // Include to hash tree
    FHashTree := TCrypto.DoSha256(FHashTree + h);
    list.Add(P);
  finally
    msCopy.Free;
  end;
  inc(FTotalAmount, op.Amount);
  inc(FTotalFee, op.Fee);
  If Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TTransactionHashTree.LoadFromStream(Stream: TStream;
  LoadingFromStorage, LoadProtocolV2: Boolean; var errors: AnsiString): Boolean;
Var
  c, i: Cardinal;
  OpType: Cardinal;
  bcop: ITransaction;
  j: Integer;
  OpClass: TPCOperationClass;
  lastNE: TNotifyEvent;
begin
  Result := false;
  //
  If Stream.Read(c, 4) < 4 then
  begin
    errors := 'Cannot read operations count';
    exit;
  end;
  lastNE := FOnChanged;
  FOnChanged := Nil;
  try
    // c = operations count
    for i := 1 to c do
    begin
      errors := 'Invalid operation structure ' + Inttostr(i) + '/' +
        Inttostr(c);
      if Stream.Size - Stream.Position < 4 then
        exit;
      Stream.Read(OpType, 4);
      OpClass := TTransactionManager.GetOperationClassByOpType(OpType);
      if Not Assigned(OpClass) then
      begin
        errors := errors + ' optype not valid:' + InttoHex(OpType, 4);
        exit;
      end;
      errors := 'Invalid operation load from stream ' + Inttostr(i) + '/' +
        Inttostr(c) + ' Class:' + OpClass.Classname;
      bcop := OpClass.Create;
      Try
        if LoadingFromStorage then
        begin
          If not bcop.LoadFromStorage(Stream, LoadProtocolV2) then
            exit;
        end
        else if not bcop.LoadFromNettransfer(Stream) then
        begin
          exit;
        end;
        AddTransactionToHashTree(bcop);
      Finally
       // FreeAndNil(bcop);
      end;
    end;
  finally
    FOnChanged := lastNE;
  end;
  If Assigned(FOnChanged) then
    FOnChanged(Self);
  Result := true;
end;

function TTransactionHashTree.OperationsCount: Integer;
Var
  l: TList;
begin
  l := FHashTreeTransactions.LockList;
  try
    Result := l.Count;
  finally
    FHashTreeTransactions.UnlockList;
  end;
end;

function TTransactionHashTree.SaveToStream(Stream: TStream;
  SaveToStorage: Boolean): Boolean;
Var
  c, i, OpType: Cardinal;
  bcop: ITransaction;
  l: TList;
begin
  l := FHashTreeTransactions.LockList;
  Try
    c := l.Count;
    Stream.Write(c, 4);
    // c = operations count
    for i := 1 to c do
    begin
      bcop := GetOperation(i - 1);
      OpType := bcop.OpType;
      Stream.Write(OpType, 4);
      if SaveToStorage then
        bcop.SaveToStorage(Stream)
      else
        bcop.SaveToNettransfer(Stream);
    end;
    Result := true;
  Finally
    FHashTreeTransactions.UnlockList;
  End;
end;

end.
