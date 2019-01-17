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
| File:       MicroCoin.Account.AccountKey.pas                                 |
| Created at: 2019-01-17                                                       |
| Purpose:    ECDSA Key managment and helper routines                          |
|==============================================================================}

unit MicroCoin.Crypto.Keys;

interface

uses Classes, SysUtils, OpenSSL, OpenSSLdef, UBasetypes, MicroCoin.Crypto.Errors, MicroCoin.Crypto.BigNum;

type

  TECDSA_Public = record
    EC_OpenSSL_NID: Word;
    x: TRawBytes;
    y: TRawBytes;
  end;

  PECDSA_Public = ^TECDSA_Public;

  //TODO: Convert to record
  TECPrivateKey = class
  private
    FPrivateKey: PEC_KEY;
    FEC_OpenSSL_NID: Word;
    procedure SetPrivateKey(const Value: PEC_KEY);
    function GetPublicKey: TECDSA_Public;
    function GetPublicKeyPoint: PEC_POINT;
  public
    constructor Create;
    destructor Destroy; override;
    procedure GenerateRandomPrivateKey(EC_OpenSSL_NID: Word);
    function ExportToRaw: TRawBytes;
    class function IsValidPublicKey(PubKey: TECDSA_Public): Boolean;
    class function ImportFromRaw(const raw: TRawBytes): TECPrivateKey; static;
    property PrivateKey: PEC_KEY read FPrivateKey;
    property PublicKey: TECDSA_Public read GetPublicKey;
    property PublicKeyPoint: PEC_POINT read GetPublicKeyPoint;
    function SetPrivateKeyFromHexa(EC_OpenSSL_NID: Word; hexa: AnsiString): Boolean;
    property EC_OpenSSL_NID: Word read FEC_OpenSSL_NID;
  end;

implementation

uses ULog, MicroCoin.Common.Config, MicroCoin.Account.AccountKey, MicroCoin.Common.Stream;

{ TECPrivateKey }

constructor TECPrivateKey.Create;
begin
  FPrivateKey := nil;
  FEC_OpenSSL_NID := cDefault_EC_OpenSSL_NID;
end;

destructor TECPrivateKey.Destroy;
begin
  if Assigned(FPrivateKey) then
    EC_KEY_free(FPrivateKey);
  inherited;
end;

//TODO: remove stream
function TECPrivateKey.ExportToRaw: TRawBytes;
var
  ms: TStream;
  aux: TRawBytes;
begin
  ms := TMemoryStream.Create;
  try
    ms.Write(FEC_OpenSSL_NID, sizeof(FEC_OpenSSL_NID));
    SetLength(aux, BN_num_bytes(EC_KEY_get0_private_key(FPrivateKey)));
    BN_bn2bin(EC_KEY_get0_private_key(FPrivateKey), @aux[1]);
    ms.WriteAnsiString(aux);
    SetLength(Result, ms.Size);
    ms.Position := 0;
    ms.Read(Result[1], ms.Size);
  finally
    ms.Free;
  end;
end;

procedure TECPrivateKey.GenerateRandomPrivateKey(EC_OpenSSL_NID: Word);
var
  i: Integer;
begin
  if Assigned(FPrivateKey)
  then EC_KEY_free(FPrivateKey);
  FEC_OpenSSL_NID := EC_OpenSSL_NID;
  FPrivateKey := EC_KEY_new_by_curve_name(EC_OpenSSL_NID);
  i := EC_KEY_generate_key(FPrivateKey);
  if i <> 1 then
    raise ECryptoException.Create('Error generating new Random Private Key');
end;

function TECPrivateKey.GetPublicKey: TECDSA_Public;
var
  BNx, BNy: PBIGNUM;
  ctx: PBN_CTX;
begin
  Result.EC_OpenSSL_NID := FEC_OpenSSL_NID;
  ctx := BN_CTX_new;
  BNx := BN_new;
  BNy := BN_new;
  try
    EC_POINT_get_affine_coordinates_GFp(EC_KEY_get0_group(FPrivateKey), EC_KEY_get0_public_key(FPrivateKey), BNx,
      BNy, ctx);
    Result.x := BigInteger(BNx).RawValue;
    Result.y := BigInteger(BNy).RawValue;
  finally
    BN_CTX_free(ctx);
    BN_free(BNx);
    BN_free(BNy);
  end;
end;

function TECPrivateKey.GetPublicKeyPoint: PEC_POINT;
begin
  Result := EC_KEY_get0_public_key(FPrivateKey);
end;

class function TECPrivateKey.ImportFromRaw(const raw: TRawBytes): TECPrivateKey;
var
  ms: TStream;
  aux: TRawBytes;
  BNx: PBIGNUM;
  ECID: Word;
  PAC: PAnsiChar;
begin
  Result := nil;
  ms := TMemoryStream.Create;
  try
    ms.WriteBuffer(raw[1], length(raw));
    ms.Position := 0;
    if ms.Read(ECID, sizeof(ECID)) <> sizeof(ECID) then
      exit;
    if ms.ReadAnsiString(aux) < 0 then
      exit;
    BNx := BN_bin2bn(PAnsiChar(aux), length(aux), nil);
    if Assigned(BNx) then
    begin
      try
        PAC := BN_bn2hex(BNx);
        try
          Result := TECPrivateKey.Create;
          try
            Result.SetPrivateKeyFromHexa(ECID, PAC);
          except
            FreeAndNil(Result);
            raise;
          end;
        finally
          OpenSSL_free(PAC);
        end;
      finally
        BN_free(BNx);
      end;
    end;
  finally
    ms.Free;
  end;
end;

class function TECPrivateKey.IsValidPublicKey(PubKey: TECDSA_Public): Boolean;
var
  BNx, BNy: PBIGNUM;
  ECG: PEC_GROUP;
  ctx: PBN_CTX;
  pub_key: PEC_POINT;
begin
  BNx := BN_bin2bn(PAnsiChar(PubKey.x), length(PubKey.x), nil);
  try
    BNy := BN_bin2bn(PAnsiChar(PubKey.y), length(PubKey.y), nil);
    try
      ECG := EC_GROUP_new_by_curve_name(PubKey.EC_OpenSSL_NID);
      try
        pub_key := EC_POINT_new(ECG);
        try
          ctx := BN_CTX_new;
          try
            Result := EC_POINT_set_affine_coordinates_GFp(ECG, pub_key, BNx, BNy, ctx) = 1;
          finally
            BN_CTX_free(ctx);
          end;
        finally
          EC_POINT_free(pub_key);
        end;
      finally
        EC_GROUP_free(ECG);
      end;
    finally
      BN_free(BNy);
    end;
  finally
    BN_free(BNx);
  end;
end;

procedure TECPrivateKey.SetPrivateKey(const Value: PEC_KEY);
begin
  if Assigned(FPrivateKey) then
    EC_KEY_free(FPrivateKey);
  FPrivateKey := Value;
end;

function TECPrivateKey.SetPrivateKeyFromHexa(EC_OpenSSL_NID: Word; hexa: AnsiString): Boolean;
var
  bn: PBIGNUM;
  ctx: PBN_CTX;
  pub_key: PEC_POINT;
begin
  Result := false;
  bn := BN_new;
  try
    if BN_hex2bn(@bn, PAnsiChar(hexa)) = 0 then
      raise ECryptoException.Create('Invalid Hexadecimal value:' + hexa);

    if Assigned(FPrivateKey) then
      EC_KEY_free(FPrivateKey);
    FEC_OpenSSL_NID := EC_OpenSSL_NID;
    FPrivateKey := EC_KEY_new_by_curve_name(EC_OpenSSL_NID);

    if EC_KEY_set_private_key(FPrivateKey, bn) <> 1 then
      raise ECryptoException.Create('Invalid num to set as private key');
    //
    ctx := BN_CTX_new;
    pub_key := EC_POINT_new(EC_KEY_get0_group(FPrivateKey));
    try
      if EC_POINT_mul(EC_KEY_get0_group(FPrivateKey), pub_key, bn, nil, nil, ctx) <> 1 then
        raise ECryptoException.Create('Error obtaining public key');
      EC_KEY_set_public_key(FPrivateKey, pub_key);
      Result := true;
    finally
      BN_CTX_free(ctx);
      EC_POINT_free(pub_key);
    end;
  finally
    BN_free(bn);
  end;
end;

end.
