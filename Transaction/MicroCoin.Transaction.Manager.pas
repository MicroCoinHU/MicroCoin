{ ==============================================================================|
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
  | File:       MicroCoin.Transaction.Manager.pas                                |
  | Created at: 2018-08-22                                                       |
  | Purpose:    Transaction plugin managment                                     |
  |============================================================================== }

unit MicroCoin.Transaction.Manager;

{$ifdef FPC}
{$mode delphi}
{$endif}

interface

uses SysUtils, Classes, MicroCoin.Transaction.Transaction, MicroCoin.Transaction.Base, ULog,
  MicroCoin.Transaction.ITransaction;

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
    class function RegisterTransactionPlugin(ATransactionClass: TTransactionClass; ATransactionType: byte)
      : boolean; static;
    class function GetTransactionPlugin(ATransactionType: Cardinal): TTransactionClass; static;
    class function CreateTransaction(ATransactionType: Cardinal): ITransaction; static;
    class destructor Destroy;
  end;

implementation

class function TTransactionManager.RegisterTransactionPlugin(ATransactionClass: TTransactionClass;
  ATransactionType: byte): boolean;
begin
  Result := false;
  if not HasTransactionPlugin(ATransactionClass) then
  begin
    SetLength(FTransactionPlugins, length(FTransactionPlugins) + 1);
    FTransactionPlugins[high(FTransactionPlugins)].TransactionClass := ATransactionClass;
    FTransactionPlugins[high(FTransactionPlugins)].TransactionType := ATransactionType;
    Result := True;
  end;
end;

class function TTransactionManager.HasTransactionPlugin(ATransactionClass: TTransactionClass): boolean;
var
  i: Integer;
begin
  Result := false;
  for i := low(FTransactionPlugins) to high(FTransactionPlugins) do
  begin
    if (FTransactionPlugins[i].TransactionClass = ATransactionClass) then
    begin
      Result := True;
      break;
    end;
  end;
end;

class function TTransactionManager.HasTransactionPlugin(ATransactionType: Cardinal): boolean;
var
  i: Integer;
begin
  Result := false;
  for i := low(FTransactionPlugins) to high(FTransactionPlugins) do
  begin
    if (FTransactionPlugins[i].TransactionType = ATransactionType) then
    begin
      Result := True;
      break;
    end;
  end;
end;

class function TTransactionManager.CreateTransaction(ATransactionType: Cardinal): ITransaction;
begin
  Result := TTransactionManager.GetTransactionPlugin(ATransactionType).Create;
end;

class destructor TTransactionManager.Destroy;
begin
  SetLength(TTransactionManager.FTransactionPlugins, 0);
  FTransactionPlugins := nil;
end;

class function TTransactionManager.GetTransactionPlugin(ATransactionType: Cardinal): TTransactionClass;
var
  i: Integer;
begin
  Result := nil;
  for i := low(FTransactionPlugins) to high(FTransactionPlugins) do
  begin
    if ATransactionType = FTransactionPlugins[i].TransactionType then
    begin
      Result := FTransactionPlugins[i].TransactionClass;
      break;
    end;
  end;
end;

end.
