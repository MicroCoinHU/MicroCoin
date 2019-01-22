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
| File:       MicroCoin.Common.Stream.pas                                      |
| Created at: 2019-01-15                                                       |
| Purpose:    Stream extension methods                                         |
|==============================================================================}

unit MicroCoin.Common.Stream;

interface

uses Classes, ulog, sysutils, MicroCoin.Account.AccountKey, UCrypto;

type
  TStreamHelper = class helper for TStream
  public
    function WriteAnsiString(const value: AnsiString): Integer; overload;
    function ReadAnsiString(var value: AnsiString): Integer; overload;
    function WriteAccountKey(const value: TAccountKey): Integer;
    function ReadAccountKey(var value: TAccountKey): Integer;
  end;


implementation

{ TStreamHelper }

function TStreamHelper.ReadAccountKey(var value: TAccountKey): Integer;
begin
  Result := -1;
  if Size - Position < 2 then
  begin
    value := TAccountKey.Empty;
    exit;
  end;
  Read(value.EC_OpenSSL_NID, SizeOf(value.EC_OpenSSL_NID));
  if (ReadAnsiString(value.x) <= 0) then
  begin
    value := TAccountKey.Empty;
    exit;
  end;
  if (ReadAnsiString(value.y) <= 0) then
  begin
    value := TAccountKey.Empty;
    exit;
  end;
  Result := value.EC_OpenSSL_NID;
end;

function TStreamHelper.ReadAnsiString(var value: AnsiString): Integer;
var
  l: Word;
  s: array of AnsiChar;
begin
  if Size - Position < 2 then
  begin
    value := '';
    Result := -1;
    exit;
  end;
  Read(l, 2);
  if Size - Position < l then
  begin
    Position := Position - 2; // Go back!
    value := '';
    Result := -1;
    exit;
  end;
  SetLength(s, l);
  ReadBuffer(s[0], l);
  value := '';
  SetString(value, PAnsiChar(@s[0]), Length(s));
  SetLength(s, 0);
  Finalize(s);
  Result := l + 2;
end;

function TStreamHelper.WriteAccountKey(const value: TAccountKey): Integer;
begin
  Result := Write(value.EC_OpenSSL_NID, SizeOf(value.EC_OpenSSL_NID));
  Result := Result + WriteAnsiString(value.x);
  Result := Result + WriteAnsiString(value.y);
end;

function TStreamHelper.WriteAnsiString(const value: AnsiString): Integer;
var
  l: Word;
begin
  if (length(value) > (256 * 256)) then
  begin
    TLog.NewLog(lterror, Classname, 'Invalid stream size! ' + inttostr(length(value)));
    raise Exception.Create('Invalid stream size! ' + inttostr(length(value)));
  end;

  l := length(value);
  Write(l, 2);
  if (l > 0) then
    WriteBuffer(value[1], length(value));
  Result := l + 2;
end;

end.
