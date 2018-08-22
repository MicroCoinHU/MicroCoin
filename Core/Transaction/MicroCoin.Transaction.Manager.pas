unit MicroCoin.Transaction.Manager;

{
  This unit contains code from PascalCoin:

  Copyright (c) Albert Molina 2016 - 2018 original code from PascalCoin https://pascalcoin.org/

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

}

interface

uses SysUtils, Classes, MicroCoin.Transaction.Transaction, MicroCoin.Transaction.Base, ULog;

type

  TPCOperationClass = class of TTransaction;

  TTransactionPlugin = record
    TransactionClass : TPCOperationClass;
    OpType : byte;
  end;

  TTransactionManager = class
  private
    class var _OperationsClass : array of TTransactionPlugin;
  public
    class function RegisterOperationClass(OpClass: TPCOperationClass; OpType: byte): Boolean; static;
    class function IndexOfOperationClass(OpClass: TPCOperationClass): Integer; static;
    class function IndexOfOperationClassByOpType(OpType: Cardinal): Integer; static;
    class function GetOperationClassByOpType(OpType: Cardinal) : TPCOperationClass; static;
  end;

implementation

class function TTransactionManager.RegisterOperationClass(OpClass: TPCOperationClass; OpType: byte): Boolean;
var
  i: Integer;
begin
  i := IndexOfOperationClass(OpClass);
  if i >= 0 then exit;
  SetLength(_OperationsClass, length(_OperationsClass) + 1);
  _OperationsClass[high(_OperationsClass)].TransactionClass := OpClass;
  _OperationsClass[high(_OperationsClass)].OpType := OpType;
end;

class function TTransactionManager.IndexOfOperationClass(OpClass: TPCOperationClass): Integer;
begin
  for Result := low(_OperationsClass) to high(_OperationsClass) do
  begin
    if (_OperationsClass[Result].TransactionClass = OpClass) then
      exit;
  end;
  Result := -1;
end;

class function TTransactionManager.IndexOfOperationClassByOpType(OpType: Cardinal): Integer;
begin
  for Result := low(_OperationsClass) to high(_OperationsClass) do
  begin
    if (_OperationsClass[Result].OpType = OpType) then
      exit;
  end;
  Result := -1;
end;

class function TTransactionManager.GetOperationClassByOpType(OpType: Cardinal) : TPCOperationClass;
Var
  i: Integer;
begin
  i := IndexOfOperationClassByOpType(OpType);
  if i < 0 then
    Result := Nil
  else
    Result := TPCOperationClass(_OperationsClass[i].TransactionClass);
end;


end.
