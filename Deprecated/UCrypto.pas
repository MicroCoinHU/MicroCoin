unit UCrypto;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}
{
  Copyright (c) Albert Molina 2016 - 2018 original code from PascalCoin https://pascalcoin.org/

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

}

{$I config.inc}

interface

uses Classes, SysUtils, MicroCoin.Crypto.Keys, OpenSSL, OpenSSLdef, UBasetypes,
  MicroCoin.Crypto.Errors, MicroCoin.Crypto.BigNum;

type

  TECDSA_SIG = record
    r: TRawBytes;
    s: TRawBytes;
  end; { record }

  TCrypto = class
  private
    class function ECDSAVerify(EC_OpenSSL_NID: Word; PubKey: EC_POINT; const digest: AnsiString; Signature: TECDSA_SIG) : Boolean; overload;
  public
    class function DoSha256(p: PAnsiChar; plength: Cardinal): TRawBytes; overload;
    class function DoSha256(const TheMessage: AnsiString): TRawBytes; overload;
    class procedure DoDoubleSha256(p: PAnsiChar; plength: Cardinal; var ResultSha256: TRawBytes); overload;
    class function DoRipeMD160_HEXASTRING(const TheMessage: AnsiString): TRawBytes; overload;
    class function DoRipeMD160AsRaw(p: PAnsiChar; plength: Cardinal): TRawBytes; overload;
    class function DoRipeMD160AsRaw(const TheMessage: AnsiString): TRawBytes; overload;
    class function PrivateKey2Hexa(Key: TECKeyPair): AnsiString;
    class function ECDSASign(Key: TECKeyPair; const digest: AnsiString): TECDSA_SIG;
    class function ECDSAVerify(PubKey: TECPublicKey; const digest: AnsiString; Signature: TECDSA_SIG): Boolean;
      overload;
    class procedure InitCrypto;
    class function IsHumanReadable(const ReadableText: TRawBytes): Boolean;
  end;

implementation

uses ULog, MicroCoin.Common.Config, MicroCoin.Account.AccountKey,
  MicroCoin.Common.Stream;

var
  _initialized: Boolean = false;

procedure _DoInit;
begin
  if not(_initialized) then
  begin
    _initialized := true;
    InitSSLFunctions;
  end;
end;

{ TCrypto }

class procedure TCrypto.DoDoubleSha256(p: PAnsiChar; plength: Cardinal; var ResultSha256: TRawBytes);
var
  PS: PAnsiChar;
begin
  if length(ResultSha256) <> 32
  then SetLength(ResultSha256, 32);
  PS := @ResultSha256[1];
  SHA256(p, plength, PS);
  SHA256(PS, 32, PS);
end;

class function TCrypto.DoRipeMD160_HEXASTRING(const TheMessage: AnsiString): TRawBytes;
var
  PS: PAnsiChar;
  PC: PAnsiChar;
  i: Integer;
begin
  GetMem(PS, 33);
  RIPEMD160(PAnsiChar(TheMessage), length(TheMessage), PS);
  PC := PS;
  Result := '';
  for i := 1 to 20
  do begin
    Result := Result + IntToHex(PtrInt(PC^), 2);
    inc(PC);
  end;
  FreeMem(PS, 33);
end;

class function TCrypto.DoRipeMD160AsRaw(p: PAnsiChar; plength: Cardinal): TRawBytes;
var
  PS: PAnsiChar;
begin
  SetLength(Result, 20);
  PS := @Result[1];
  RIPEMD160(p, plength, PS);
end;

class function TCrypto.DoRipeMD160AsRaw(const TheMessage: AnsiString): TRawBytes;
var
  PS: PAnsiChar;
begin
  SetLength(Result, 20);
  PS := @Result[1];
  RIPEMD160(PAnsiChar(TheMessage), length(TheMessage), PS);
end;

class function TCrypto.DoSha256(p: PAnsiChar; plength: Cardinal): TRawBytes;
var
  PS: PAnsiChar;
begin
  SetLength(Result, 32);
  PS := @Result[1];
  SHA256(p, plength, PS);
end;

class function TCrypto.DoSha256(const TheMessage: AnsiString): TRawBytes;
var
  PS: PAnsiChar;
begin
  SetLength(Result, 32);
  PS := @Result[1];
  SHA256(PAnsiChar(TheMessage), length(TheMessage), PS);
  PS := nil;
end;

class function TCrypto.ECDSASign(Key: TECKeyPair; const digest: AnsiString): TECDSA_SIG;
var
  PECS: PECDSA_SIG;
begin
  PECS := ECDSA_do_sign(PAnsiChar(digest), length(digest), Key.PrivateKey);
  try
    if PECS = nil
    then raise ECryptoException.Create('Error signing');
    Result.r := BigInteger(PECS^._r).RawValue;
    Result.s := BigInteger(PECS^._s).RawValue;
  finally
    ECDSA_SIG_free(PECS);
  end;
end;

class function TCrypto.ECDSAVerify(EC_OpenSSL_NID: Word; PubKey: EC_POINT; const digest: AnsiString;
  Signature: TECDSA_SIG): Boolean;
var
  PECS: PECDSA_SIG;
  PK: PEC_KEY;
begin
  PECS := ECDSA_SIG_new;
  try
    BN_bin2bn(PAnsiChar(Signature.r), length(Signature.r), PECS^._r);
    BN_bin2bn(PAnsiChar(Signature.s), length(Signature.s), PECS^._s);
    PK := EC_KEY_new_by_curve_name(EC_OpenSSL_NID);
    EC_KEY_set_public_key(PK, @PubKey);
    case ECDSA_do_verify(PAnsiChar(digest), length(digest), PECS, PK) of
      1: Result := true;
      0: Result := false;
    else raise ECryptoException.Create('Error on Verify');
    end;
    EC_KEY_free(PK);
  finally
    ECDSA_SIG_free(PECS);
  end;
end;

class function TCrypto.ECDSAVerify(PubKey: TECPublicKey; const digest: AnsiString; Signature: TECDSA_SIG): Boolean;
var
  BNx, BNy: PBIGNUM;
  ECG: PEC_GROUP;
  ctx: PBN_CTX;
  pub_key: PEC_POINT;
begin
  BNx := BN_bin2bn(PAnsiChar(PubKey.x), length(PubKey.x), nil);
  BNy := BN_bin2bn(PAnsiChar(PubKey.y), length(PubKey.y), nil);
  ECG := EC_GROUP_new_by_curve_name(PubKey.EC_OpenSSL_NID);
  pub_key := EC_POINT_new(ECG);
  ctx := BN_CTX_new;

  if EC_POINT_set_affine_coordinates_GFp(ECG, pub_key, BNx, BNy, ctx) = 1
  then Result := ECDSAVerify(PubKey.EC_OpenSSL_NID, pub_key^, digest, Signature)
  else Result := false;

  BN_CTX_free(ctx);
  EC_POINT_free(pub_key);
  EC_GROUP_free(ECG);
  BN_free(BNx);
  BN_free(BNy);
end;

class procedure TCrypto.InitCrypto;
begin
  _DoInit;
end;

class function TCrypto.IsHumanReadable(const ReadableText: TRawBytes): Boolean;
var
  i: Integer;
begin
  Result := true;
  for i := 1 to length(ReadableText)
  do begin
    if (ord(ReadableText[i]) < 32) or (ord(ReadableText[i]) >= 255)
    then begin
      Result := false;
      Exit;
    end;
  end;
end;

class function TCrypto.PrivateKey2Hexa(Key: TECKeyPair): AnsiString;
begin
  Result := BigInteger(EC_KEY_get0_private_key(Key.PrivateKey)).hexaValue;
end;

end.
