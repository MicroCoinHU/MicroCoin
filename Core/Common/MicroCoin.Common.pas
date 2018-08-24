unit MicroCoin.Common;

{
  This unit contains code from PascalCoin:

  Copyright (c) Albert Molina 2016 - 2018 original code from PascalCoin https://pascalcoin.org/

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

}

interface

uses SysUtils;

type

  TErrorCode = (ecSuccess, ecError, ecException, ecCancel, ecUnknown);

  TResult<T> = record
  public
    IsSuccess : boolean;
    ErrorCode : TErrorCode;
    ErrorMessage : string;
    PreferredException : ExceptClass;
    OriginalException : Exception;
    Payload : T;
    procedure RaiseException;
    class function CreateFromException(E : Exception) : TResult<T>; static;
  end;

  TCurrencyUtils = class
  public
    class function FormatMoney(Money: Int64): AnsiString;
    class function TxtToMoney(const moneytxt : AnsiString; var money : Int64) : Boolean;
  end;

implementation

class function TCurrencyUtils.FormatMoney(Money: Int64): AnsiString;
begin
  Result := FormatFloat('#,###0.0000', (Money / 10000));
end;

class function TCurrencyUtils.TxtToMoney(Const moneytxt : AnsiString; var money : Int64) : Boolean;
var
  s: AnsiString;
begin
  money := 0;
  if trim(moneytxt) = '' then
  begin
    Result := true;
    exit;
  end;
  try
    if pos(FormatSettings.DecimalSeparator, moneytxt) <= 0 then
    begin
      s := StringReplace(moneytxt, FormatSettings.ThousandSeparator, FormatSettings.DecimalSeparator, [rfReplaceAll]);
    end
    else
    begin
      s := StringReplace(moneytxt, FormatSettings.ThousandSeparator, '', [rfReplaceAll]);
    end;
    money := Round(StrToFloat(s) * 10000);
    Result := true;
  except on E:Exception do begin
      Result := false;
    end;
  end;
end;

{ TResult }

class function TResult<T>.CreateFromException(E: Exception): TResult<T>;
begin
  Result.IsSuccess := false;
  Result.ErrorCode := ecException;
  Result.ErrorMessage := E.Message;
  Result.PreferredException := ExceptClass( E.ClassType );
end;

procedure TResult<T>.RaiseException;
begin
  if ErrorCode = ecException then
    if OriginalException <> nil then
      raise OriginalException;
  if PreferredException <> nil then
    raise PreferredException.Create(ErrorMessage);
end;

end.
