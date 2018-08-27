unit MicroCoin.Transaction.Manager;

{
  Copyright (c) 2018 MicroCoin Developers

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

interface

uses SysUtils, Classes, MicroCoin.Transaction.Transaction, MicroCoin.Transaction.Base, ULog;

type

  TTransactionClass = class of TTransaction;

  TTransactionPlugin = record
    TransactionClass: TTransactionClass;
    TransactionType: byte;
  end;

  TTransactionManager = class
  private
    class var FTransactionPlugins: array of TTransactionPlugin;
    class function HasTransactionPlugin(ATransactionClass: TTransactionClass): boolean; overload; static;
    class function HasTransactionPlugin(ATransactionType: Cardinal): boolean; overload; static;
  public
    class function RegisterTransactionPlugin(ATransactionClass: TTransactionClass; ATransactionType: byte): Boolean; static;
    class function GetTransactionPlugin(ATransactionType: Cardinal): TTransactionClass; static;
  end;

implementation

class function TTransactionManager.RegisterTransactionPlugin(ATransactionClass: TTransactionClass; ATransactionType: byte): Boolean;
var
  i: Integer;
begin
  if not HasTransactionPlugin(ATransactionClass)
  then begin
    SetLength(FTransactionPlugins, length(FTransactionPlugins) + 1);
    FTransactionPlugins[high(FTransactionPlugins)].TransactionClass := ATransactionClass;
    FTransactionPlugins[high(FTransactionPlugins)].TransactionType := ATransactionType;
  end;
end;

class function TTransactionManager.HasTransactionPlugin(ATransactionClass: TTransactionClass): boolean;
var i : integer;
begin
  Result := false;
  for i := low(FTransactionPlugins) to high(FTransactionPlugins) do
  begin
    if (FTransactionPlugins[i].TransactionClass = ATransactionClass) then
    begin
      Result := true;
      break;
    end;
  end;
end;

class function TTransactionManager.HasTransactionPlugin(ATransactionType: Cardinal): boolean;
var
  i : integer;
begin
  Result := false;
  for i := low(FTransactionPlugins) to high(FTransactionPlugins) do
  begin
    if (FTransactionPlugins[i].TransactionType = ATransactionType) then
    begin
      Result := true;
      break;
    end;
  end;
end;

class function TTransactionManager.GetTransactionPlugin(ATransactionType: Cardinal): TTransactionClass;
var i : integer;
begin
  Result := nil;
  for i := Low(FTransactionPlugins) to High(FTransactionPlugins) do
  begin
    if ATransactionType = FTransactionPlugins[i].TransactionType
    then begin
      Result := FTransactionPlugins[i].TransactionClass;
      break;
    end;
  end;
end;

end.
