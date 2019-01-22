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
| File:       MicroCoin.Crypto.BigNum.pas                                      |
| Created at: 2019-01-17                                                       |
| Purpose:    Wrapper types for openssl bignum routines                        |
|==============================================================================}
{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

unit MicroCoin.Crypto.BigNum;

interface

uses Classes, SysUtils, OpenSSL, OpenSSLdef, Ulog, UBaseTypes, MicroCoin.Crypto.Errors;

type
  TProc<T> = procedure (Arg1: T);
  IBigIntegerDestructor = interface
    ['{DD3C21C5-5523-439E-9A37-E8752E94047E}']
  end;

  TBigIntegerDestructor = class;

  { BigInteger }

  BigInteger = record
  private
    FBN: PBIGNUM;
    FDestructor : IBigIntegerDestructor;
    procedure SetHexaValue(const Value: AnsiString);
    function GetHexaValue: AnsiString;
    procedure SetValue(const Value: Int64);
    function GetValue: Int64;
    function GetDecimalValue: AnsiString;
    procedure SetDecimalValue(const Value: AnsiString);
    function GetRawValue: TRawBytes;
    procedure SetRawValue(const Value: TRawBytes);
    constructor Create(initialValue: Int64); overload;
    constructor Create(hexaValue: AnsiString); overload;
    class procedure cleanup(ABN:PBIGNUM); static;
  public
    function Copy: BigInteger;
    procedure Divide(dividend, remainder: BigInteger); overload;
    function LShift(nbits: Integer): BigInteger;
    function RShift(nbits: Integer): BigInteger;
    function CompareTo(BN: BigInteger): Integer;
    function ToInt64(var int: Int64): BigInteger;
    function ToDecimal: AnsiString;

    property hexaValue: AnsiString read GetHexaValue write SetHexaValue;
    property RawValue: TRawBytes read GetRawValue write SetRawValue;
    property DecimalValue: AnsiString read GetDecimalValue write SetDecimalValue;
    property Value: Int64 read GetValue write SetValue;

    function IsZero: Boolean; deprecated;
    function Add(BN: BigInteger): BigInteger; overload; deprecated;
    function Add(int: Int64): BigInteger; overload; deprecated;
    function Sub(BN: BigInteger): BigInteger; overload; deprecated;
    function Sub(int: Int64): BigInteger; overload; deprecated;
    function Multiply(BN: BigInteger): BigInteger; overload; deprecated;
    function Multiply(int: Int64): BigInteger; overload; deprecated;
    function Divide(int: Int64): BigInteger; overload; deprecated;
    function Divide(BN: BigInteger): BigInteger; overload; deprecated;

    class function HexaToDecimal(hexa: AnsiString): AnsiString; static;
    class function TargetToHashRate(EncodedTarget: Cardinal): BigInteger; static;

    // Operators
    class operator Implicit(ABigNum : BigInteger) : PBIGNUM;
    class operator Implicit(ABigNum : BigInteger) : PPBIGNUM;
    class operator Implicit(ABigNum : PBIGNUM) : BigInteger;
    class operator Implicit(AInteger : integer) : BigInteger;
    class operator Implicit(AHexa : AnsiString) : BigInteger;
    class operator Implicit(AInteger : Int64) : BigInteger;
    class operator Implicit(ABN : BigInteger) : Int64;

    class operator Explicit(ABigNum : PBIGNUM) : BigInteger;

    class operator Add(ABN : BigInteger; BBN: BigInteger) : BigInteger;
    class operator Subtract(ABN : BigInteger; BBN: BigInteger) : BigInteger;
    class operator Multiply(ABN, BBN: BigInteger) : BigInteger;
    class operator Divide(ABN, BBN: BigInteger) : BigInteger;

    class operator Equal(ABN, BBN : BigInteger) : boolean;
    class operator NotEqual(ABN, BBN : BigInteger) : boolean;
    class operator GreaterThanOrEqual(ABN, BBN : BigInteger) : boolean;
    class operator GreaterThan(ABN, BBN : BigInteger) : boolean;
    class operator LessThan(ABN, BBN : BigInteger) : boolean;
    class operator LessThanOrEqual(ABN, BBN : BigInteger) : boolean;

  end;

  TBigIntegerDestructor = class(TInterfacedObject, IBigIntegerDestructor)
  private
    FDestoyer : TProc<PBIGNUM>;
    FData : PBIGNUM;
  public
    constructor Create(ADestructor : TProc<PBIGNUM>; AData : PBIGNUM);
    destructor Destroy; override;
  end;



implementation

{ BigInteger }

function BigInteger.Add(BN: BigInteger): BigInteger;
begin
  BN_add(FBN, BN.FBN, FBN);
  Result := Self;
end;

function BigInteger.Add(int: Int64): BigInteger;
var
  BN: BigInteger;
begin
  BN := int;
  Result := Add(BN);
end;

class operator BigInteger.Add(ABN: BigInteger; BBN: BigInteger): BigInteger;
begin
  Result := 0;
  BN_add(Result, ABN, BBN);
end;

function BigInteger.CompareTo(BN: BigInteger): Integer;
begin
  Result := BN_cmp(FBN, BN);
end;

function BigInteger.Copy: BigInteger;
begin
  Result := BigInteger.Create(0);
  BN_copy(Result, FBN);
end;

constructor BigInteger.Create(hexaValue: AnsiString);
begin
  Create(0);
  SetHexaValue(hexaValue);
end;

class procedure BigInteger.cleanup(ABN: PBIGNUM);
begin
  BN_free(ABN);
end;

constructor BigInteger.Create(initialValue: Int64);
begin
  FBN := BN_new;
  FDestructor := TBigIntegerDestructor.Create(cleanup, FBN);
  SetValue(initialValue);
end;

procedure BigInteger.Divide(dividend, remainder: BigInteger);
var
  ctx: PBN_CTX;
begin
  ctx := BN_CTX_new;
  BN_div(FBN, remainder.FBN, FBN, dividend.FBN, ctx);
  BN_CTX_free(ctx);
end;

function BigInteger.Divide(int: Int64): BigInteger;
var
  BN: BigInteger;
begin
  BN := int;
  Result := Divide(BN);
end;

function BigInteger.Divide(BN: BigInteger): BigInteger;
var
  _div, _rem: PBIGNUM;
  ctx: PBN_CTX;
begin
  _div := BN_new;
  _rem := BN_new;
  ctx := BN_CTX_new;
  BN_div(FBN, _rem, FBN, BN.FBN, ctx);
  BN_free(_div);
  BN_free(_rem);
  BN_CTX_free(ctx);
  Result := Self;
end;

function BigInteger.GetDecimalValue: AnsiString;
var
  p: PAnsiChar;
begin
  p := BN_bn2dec(FBN);
  Result := strpas(p);
  OpenSSL_free(p);
end;

function BigInteger.GetHexaValue: AnsiString;
var
  p: PAnsiChar;
begin
  p := BN_bn2hex(FBN);
  Result := strpas(p);
  OpenSSL_free(p);
end;

function BigInteger.GetRawValue: TRawBytes;
var
  p: PAnsiChar;
  i: Integer;
begin
  i := BN_num_bytes(FBN);
  SetLength(Result, i);
  p := @Result[1];
  i := BN_bn2bin(FBN, p);
end;

function BigInteger.GetValue: Int64;
var
  p: PAnsiChar;
  a: AnsiString;
  err: Integer;
begin
  p := BN_bn2dec(FBN);
  a := strpas(p);
  OpenSSL_free(p);
  val(a, Result, err);
end;

class operator BigInteger.GreaterThan(ABN, BBN: BigInteger): boolean;
begin
  Result := BN_cmp(ABN, BBN) > 0;
end;

class operator BigInteger.GreaterThanOrEqual(ABN, BBN : BigInteger): boolean;
begin
  Result := BN_cmp(ABN, BBN) <> -1;
end;

class function BigInteger.HexaToDecimal(hexa: AnsiString): AnsiString;
var
  BN: BigInteger;
begin
  BN := hexa;
  Result := BN.ToDecimal;
end;

class operator BigInteger.Implicit(ABigNum: BigInteger): PBIGNUM;
begin
  if ABigNum.FBN = nil
  then ABigNum := BigInteger.Create(0);
  Result := ABigNum.FBN;
end;

class operator BigInteger.Implicit(ABigNum: BigInteger): PPBIGNUM;
begin
  Result := @ABigNum.FBN;
end;

class operator BigInteger.Implicit(ABigNum: PBIGNUM): BigInteger;
begin
  Result.FBN := ABigNum;
end;

class operator BigInteger.Implicit(AInteger: integer): BigInteger;
begin
  Result := BigInteger.Create(AInteger);
end;

class operator BigInteger.Implicit(AInteger: Int64): BigInteger;
begin
  Result := BigInteger.Create(AInteger);
end;

function BigInteger.IsZero: Boolean;
begin
  Result := FBN.top = 0;
end;

class operator BigInteger.LessThan(ABN, BBN: BigInteger): boolean;
begin
  Result := BN_cmp(ABN, BBN) < 0;
end;

class operator BigInteger.LessThanOrEqual(ABN, BBN: BigInteger): boolean;
begin
  Result := BN_cmp(ABN, BBN) < 1;
end;

function BigInteger.LShift(nbits: Integer): BigInteger;
begin
  if BN_lshift(FBN, FBN, nbits) <> 1 then
    raise ECryptoException.Create('Error on LShift');
  Result := Self;
end;

function BigInteger.Multiply(int: Int64): BigInteger;
var
  n: BigInteger;
  ctx: PBN_CTX;
begin
  n := int;
 ctx := BN_CTX_new;
  try
    if BN_mul(FBN, FBN, n.FBN, ctx) <> 1
    then raise ECryptoException.Create('Error on multiply');
    Result := Self;
  finally
    BN_CTX_free(ctx);
  end;
end;

function BigInteger.RShift(nbits: Integer): BigInteger;
begin
  if BN_rshift(FBN, FBN, nbits) <> 1 then
    raise ECryptoException.Create('Error on LShift');
  Result := Self;
end;

function BigInteger.Multiply(BN: BigInteger): BigInteger;
var
  ctx: PBN_CTX;
begin
  ctx := BN_CTX_new;
  if BN_mul(FBN, FBN, BN.FBN, ctx) <> 1 then
    raise ECryptoException.Create('Error on multiply');
  BN_CTX_free(ctx);
  Result := Self;
end;

procedure BigInteger.SetDecimalValue(const Value: AnsiString);
begin
  if BN_dec2bn(@FBN, PAnsiChar(Value)) = 0 then
    raise ECryptoException.Create('Error on dec2bn');
end;

procedure BigInteger.SetHexaValue(const Value: AnsiString);
var
  i: Integer;
  bf: PAnsiChar;
  bn:PBignum;
begin
  BN := BN_new;
  i := BN_hex2bn(@BN, PAnsiChar(value));
  FBN := BN;
  if i = 0 then
  begin
    bf := PansiChar(stralloc(500));
    ERR_error_string(ERR_get_error(),bf);
    TLog.NewLog(lterror, 'OpenSSL', 'Invalid Hexadecimal value:' + Value+' '+bf);
    raise ECryptoException.Create('Invalid Hexadecimal value:' + Value+' '+bf);
  end;
end;

procedure BigInteger.SetRawValue(const Value: TRawBytes);
var
  p: PBIGNUM;
begin
  p := BN_bin2bn(PAnsiChar(Value), length(Value), FBN);
  if (p <> FBN) or (p = nil) then
    raise ECryptoException.Create('Error decoding Raw value to BigNum "' + TBaseType.ToHexaString(Value) + '" (' +
      inttostr(length(Value)) + ')' + #10 + ERR_error_string(ERR_get_error(), nil));
end;

procedure BigInteger.SetValue(const Value: Int64);
var
  a: UInt64;
begin
  if Value < 0 then
    a := (Value * (-1))
  else
    a := Value;
  if BN_set_word(FBN, a) <> 1 then
    raise ECryptoException.Create('Error on set Value');
  if Value < 0 then
    BN_set_negative(FBN, 1)
  else
    BN_set_negative(FBN, 0);
end;

class operator BigInteger.Subtract(ABN: BigInteger; BBN: BigInteger
  ): BigInteger;
begin
  BN_sub(Result, ABN, BBN);
end;

function BigInteger.Sub(BN: BigInteger): BigInteger;
begin
  BN_sub(FBN, FBN, BN.FBN);
  Result := Self;
end;

function BigInteger.Sub(int: Int64): BigInteger;
var
  BN: BigInteger;
begin
  BN := int;
  Result := Sub(BN);
end;

class function BigInteger.TargetToHashRate(EncodedTarget: Cardinal): BigInteger;
var
  bn1, bn2: BigInteger;
  part_A, part_B: Cardinal;
  ctx: PBN_CTX;
begin
  { Target is 2 parts: First byte (A) is "0" bits on the left. Bytes 1,2,3 (B) are number after first "1" bit
    Example: Target 23FEBFCE
    Part_A: 23  -> 35 decimal
    Part_B: FEBFCE
    Target to Hash rate Formula:
    Result = 2^Part_A + ( (2^(Part_A-24)) * Part_B )
  }
  Result := 2;
  part_A := EncodedTarget shr 24;
  bn1 := part_A;
  ctx := BN_CTX_new;
  try
    if BN_exp(Result.FBN, Result.FBN, bn1.FBN, ctx) <> 1 then
      raise Exception.Create('Error 20161017-3');
  finally
    BN_CTX_free(ctx);
  end;
  if part_A <= 24 then
    exit;
  //
  part_B := (EncodedTarget shl 8) shr 8;
  bn2 := 2;
  bn1 := (part_A - 24);
  ctx := BN_CTX_new;
  try
    if BN_exp(bn2.FBN, bn2.FBN, bn1.FBN, ctx) <> 1 then
      raise Exception.Create('Error 20161017-4');
  finally
    BN_CTX_free(ctx);
  end;
  bn2.Multiply(part_B);
  Result.Add(bn2);
end;

function BigInteger.ToDecimal: AnsiString;
var
  p: PAnsiChar;
begin
  p := BN_bn2dec(FBN);
  Result := strpas(p);
  OpenSSL_free(p);
end;

function BigInteger.ToInt64(var int: Int64): BigInteger;
var
  s: AnsiString;
  err: Integer;
  p: PAnsiChar;
begin
  p := BN_bn2dec(FBN);
  s := strpas(p);
  OpenSSL_free(p);
  val(s, int, err);
  if err <> 0 then
    int := 0;
  Result := Self;
end;

{ TRecordDestructor }

constructor TBigIntegerDestructor.Create(ADestructor: TProc<PBIGNUM>; AData : PBIGNUM);
begin
  FDestoyer := ADestructor;
  FData := AData;
end;

destructor TBigIntegerDestructor.Destroy;
begin
  if Assigned(FDestoyer)
  then FDestoyer(FData);
  inherited;
end;

class operator BigInteger.Multiply(ABN, BBN: BigInteger): BigInteger;
begin
  Result := ABN.Copy;
  Result := Result.Multiply(BBN);
end;

class operator BigInteger.NotEqual(ABN, BBN: BigInteger): boolean;
begin
  Result := BN_cmp(ABN, BBN) <> 0;
end;

class operator BigInteger.Divide(ABN, BBN: BigInteger): BigInteger;
begin
  Result := ABN.Copy;
  Result := Result.Divide(BBN);
end;

class operator BigInteger.Equal(ABN, BBN: BigInteger): boolean;
begin
  Result := BN_cmp(ABN, BBN) = 0;
end;

class operator BigInteger.Explicit(ABigNum: PBIGNUM): BigInteger;
begin
  Result := BigInteger.Create(0);
  Result.FBN := ABigNum;
end;

class operator BigInteger.Implicit(ABN: BigInteger): Int64;
begin
  Result := ABN.Value;
end;

class operator BigInteger.Implicit(AHexa: AnsiString): BigInteger;
begin
  Result := BigInteger.Create(AHexa);
end;

end.
