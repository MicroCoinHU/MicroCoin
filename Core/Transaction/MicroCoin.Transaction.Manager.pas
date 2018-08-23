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
    class var FTransactionPluginList : array of TTransactionPlugin;
  public
    class function RegisterTransactionPlugin(OpClass: TPCOperationClass; OpType: byte): Boolean; static;
    class function IndexOfTransactionPlugin(OpClass: TPCOperationClass): Integer; static;
    class function IndexOfTransactionPluginByOpType(OpType: Cardinal): Integer; static;
    class function GetTransactionPluginByOpType(OpType: Cardinal) : TPCOperationClass; static;
  end;

implementation

class function TTransactionManager.RegisterTransactionPlugin(OpClass: TPCOperationClass; OpType: byte): Boolean;
var
  i: Integer;
begin
  i := IndexOfTransactionPlugin(OpClass);
  if i >= 0 then exit;
  SetLength(FTransactionPluginList, length(FTransactionPluginList) + 1);
  FTransactionPluginList[high(FTransactionPluginList)].TransactionClass := OpClass;
  FTransactionPluginList[high(FTransactionPluginList)].OpType := OpType;
end;

class function TTransactionManager.IndexOfTransactionPlugin(OpClass: TPCOperationClass): Integer;
begin
  for Result := low(FTransactionPluginList) to high(FTransactionPluginList) do
  begin
    if (FTransactionPluginList[Result].TransactionClass = OpClass) then
      exit;
  end;
  Result := -1;
end;

class function TTransactionManager.IndexOfTransactionPluginByOpType(OpType: Cardinal): Integer;
begin
  for Result := low(FTransactionPluginList) to high(FTransactionPluginList) do
  begin
    if (FTransactionPluginList[Result].OpType = OpType) then
      exit;
  end;
  Result := -1;
end;

class function TTransactionManager.GetTransactionPluginByOpType(OpType: Cardinal) : TPCOperationClass;
Var
  i: Integer;
begin
  i := IndexOfTransactionPluginByOpType(OpType);
  if i < 0 then
    Result := Nil
  else
    Result := TPCOperationClass(FTransactionPluginList[i].TransactionClass);
end;


end.
