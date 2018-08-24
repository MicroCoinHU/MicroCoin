unit MicroCoin.Transaction.TransactionList;

{
  This unit contains code from PascalCoin:

  Copyright (c) Albert Molina 2016 - 2018 original code from PascalCoin https://pascalcoin.org/

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

}

interface

uses SysUtils, Classes, MicroCoin.Transaction.Base, UThread,
     MicroCoin.Account.Storage, MicroCoin.Account.AccountKey,
     MicroCoin.Common.Lists, UCrypto, UConst, ULog;

type
  TTransactionList = class
  private
    FList: TPCThreadList;
    function GetTransactionData(index: Integer): TTransactionData;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Const TransactionData: TTransactionData);
    function Count: Integer;
    procedure Delete(index: Integer);
    procedure Clear;
    property TransactionData[index: Integer]: TTransactionData read GetTransactionData; default;
  End;



implementation

Type
  POperationResume = ^TTransactionData;

procedure TTransactionList.Add(const TransactionData: TTransactionData);
Var
  P: POperationResume;
begin
  New(P);
  P^ := TransactionData;
  FList.Add(P);
end;

procedure TTransactionList.Clear;
Var
  P: POperationResume;
  i: Integer;
  l: TList;
begin
  l := FList.LockList;
  try
    for i := 0 to l.Count - 1 do
    begin
      P := l[i];
      Dispose(P);
    end;
    l.Clear;
  finally
    FList.UnlockList;
  end;
end;

function TTransactionList.Count: Integer;
Var
  l: TList;
begin
  l := FList.LockList;
  Try
    Result := l.Count;
  Finally
    FList.UnlockList;
  End;
end;

constructor TTransactionList.Create;
begin
  FList := TPCThreadList.Create('TOperationsResumeList_List');
end;

procedure TTransactionList.Delete(index: Integer);
Var
  P: POperationResume;
  l: TList;
begin
  l := FList.LockList;
  Try
    P := l[index];
    l.Delete(index);
    Dispose(P);
  Finally
    FList.UnlockList;
  End;
end;

destructor TTransactionList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

function TTransactionList.GetTransactionData(index: Integer)
  : TTransactionData;
Var
  l: TList;
begin
  l := FList.LockList;
  try
    if index < l.Count then
      Result := POperationResume(l[index])^
    else
      Result := TTransactionData.Empty;
  finally
    FList.UnlockList;
  end;
end;


end.
