{==============================================================================|
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
| This unit contains portions from PascalCoin                                  |
| Copyright (c) Albert Molina 2016 - 2018                                      |
|                                                                              |
| Distributed under the MIT software license, see the accompanying file        |
| LICENSE or visit http://www.opensource.org/licenses/mit-license.php.         |
|==============================================================================|
| File:       MicroCoin.Common.pas                                             |
| Created at: 2018-08-23                                                       |
| Purpose:    Common routines, types and classes                               |
|==============================================================================}

unit MicroCoin.Common;

{$ifdef FPC}
  {$mode delphi}
{$endif}

interface

uses SysUtils;

type

  TErrorCode = (ecSuccess, ecError, ecException, ecCancel, ecUnknown);

{$ifdef USE_GENERICS}
  TResult<T> = record
  public
    IsSuccess: boolean;
    ErrorCode: TErrorCode;
    ErrorMessage: string;
    PreferredException: ExceptClass;
    OriginalException: Exception;
    Payload: T;
    procedure RaiseException;
    class function CreateFromException(E: Exception): TResult<T>; static;
  end;
{$endif}

  TCurrencyUtils = class
  public
    class function FormatMoney(Money: UInt64): AnsiString; static;
    class function TxtToMoney(const moneytxt: AnsiString; var Money: Int64): boolean; static;
    class function ToJSONCurrency(microCoins: UInt64): Real; static;
    class function ToMicroCoins(jsonCurr: Real): Int64; static;
  end;

function BinStrCompare(const Str1, Str2: AnsiString): Integer;
Function TBytesToString(Const bytes: TBytes): AnsiString;

{$IFDEF FPC}
procedure CopyMemory(Destination: Pointer; Source: Pointer; Length: DWORD); inline;
{$ENDIF}

implementation

uses Math;

{$IFDEF FPC}
procedure CopyMemory(Destination: Pointer; Source: Pointer; Length: DWORD);
begin
  Move(Source^, Destination^, Length);
end;
{$ENDIF}

Function TBytesToString(Const bytes: TBytes): AnsiString;
Var
  i: Integer;
Begin
  Result := '';
  for i := 0 to high(bytes) do
  begin
    if (bytes[i] < 32) then
      Result := Result + '#' + IntToHex(bytes[i], 2)
    else if bytes[i] = ord('#') then
      Result := Result + '##'
    else
      Result := Result + ansichar(bytes[i]);
  end;
End;

function BinStrCompare(const Str1, Str2: AnsiString): Integer;
var
  Str1Len, Str2Len, i: Integer;
begin
  Str1Len := Length(Str1);
  Str2Len := Length(Str2);
  if (Str1Len < Str2Len) then
    Result := -1
  else if (Str1Len > Str2Len) then
    Result := 1
  else
  begin
    Result := 0;
    for i := 1 to Str1Len do
    begin
      if Str1[i] = Str2[i] then
        continue;
      if Str1[i] < Str2[i] then
      begin
        Result := -1;
        break;
      end
      else if Str1[i] > Str2[i] then
      begin
        Result := 1;
        break;
      end
    end;
  end;
end;

class function TCurrencyUtils.ToJSONCurrency(microCoins: UInt64): Real;
begin
  Result := RoundTo(microCoins / 10000, -4);
end;

class function TCurrencyUtils.ToMicroCoins(jsonCurr: Real): Int64;
begin
  Result := Round(jsonCurr * 10000);
end;

class function TCurrencyUtils.FormatMoney(Money: UInt64): AnsiString;
begin
  Result := FormatFloat('#,###0.0000', (Money / 10000));
end;

class function TCurrencyUtils.TxtToMoney(const moneytxt: AnsiString;
  var Money: Int64): boolean;
var
  s: AnsiString;
begin
  Money := 0;
  if trim(moneytxt) = '' then
  begin
    Result := true;
    exit;
  end;
  try
    if pos(FormatSettings.DecimalSeparator, moneytxt) <= 0 then
    begin
      s := StringReplace(moneytxt, FormatSettings.ThousandSeparator,
        FormatSettings.DecimalSeparator, [rfReplaceAll]);
    end
    else
    begin
      s := StringReplace(moneytxt, FormatSettings.ThousandSeparator, '',
        [rfReplaceAll]);
    end;
    Money := Round(StrToFloat(s) * 10000);
    Result := true;
  except
    on E: Exception do
    begin
      Result := false;
    end;
  end;
end;

{$ifdef USE_GENERICS}
{ TResult }

class function TResult<T>.CreateFromException(E: Exception): TResult<T>;
begin
  Result.IsSuccess := false;
  Result.ErrorCode := ecException;
  Result.ErrorMessage := E.Message;
  Result.PreferredException := ExceptClass(E.ClassType);
end;

procedure TResult<T>.RaiseException;
begin
  if ErrorCode = ecException then
    if OriginalException <> nil then
      raise OriginalException;
  if PreferredException <> nil then
    raise PreferredException.Create(ErrorMessage);
end;
{$ENDIF}

end.
