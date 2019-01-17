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

uses
 Classes, SysUtils, OpenSSL, OpenSSLdef, UBasetypes, MicroCoin.Crypto.Errors;

Type

  TECDSA_SIG = record
     r: TRawBytes;
     s: TRawBytes;
  end; { record }

  TECDSA_Public = record
     EC_OpenSSL_NID : Word;
     x: TRawBytes;
     y: TRawBytes;
  end;

  PECDSA_Public = ^TECDSA_Public;

  TECPrivateKey = Class
  private
    FPrivateKey: PEC_KEY;
    FEC_OpenSSL_NID : Word;
    procedure SetPrivateKey(const Value: PEC_KEY);
    function GetPublicKey: TECDSA_Public;
    function GetPublicKeyPoint: PEC_POINT;
  public
    Constructor Create;
    Procedure GenerateRandomPrivateKey(EC_OpenSSL_NID : Word);
    Destructor Destroy; override;
    Property PrivateKey : PEC_KEY read FPrivateKey;
    Property PublicKey : TECDSA_Public read GetPublicKey;
    Property PublicKeyPoint : PEC_POINT read GetPublicKeyPoint;
    Function SetPrivateKeyFromHexa(EC_OpenSSL_NID : Word; hexa : AnsiString) : Boolean;
    Property EC_OpenSSL_NID : Word Read FEC_OpenSSL_NID;
    class function IsValidPublicKey(PubKey : TECDSA_Public) : Boolean;
    Function ExportToRaw : TRawBytes;
    class Function ImportFromRaw(Const raw : TRawBytes) : TECPrivateKey; static;
  end;

  TCrypto = class
  private
    class function ECDSAVerify(EC_OpenSSL_NID : Word; PubKey : EC_POINT; const digest : AnsiString; Signature : TECDSA_SIG) : Boolean; overload;
  public
    class function DoSha256(p : PAnsiChar; plength : Cardinal) : TRawBytes; overload;
    class function DoSha256(const TheMessage : AnsiString) : TRawBytes; overload;
    class procedure DoDoubleSha256(p : PAnsiChar; plength : Cardinal; Var ResultSha256 : TRawBytes); overload;
    class function DoRipeMD160_HEXASTRING(const TheMessage : AnsiString) : TRawBytes; overload;
    class function DoRipeMD160AsRaw(p : PAnsiChar; plength : Cardinal) : TRawBytes; overload;
    class function DoRipeMD160AsRaw(const TheMessage : AnsiString) : TRawBytes; overload;
    class function PrivateKey2Hexa(Key : TECPrivateKey) : AnsiString;
    class function ECDSASign(Key : TECPrivateKey; const digest : AnsiString) : TECDSA_SIG;
    class function ECDSAVerify(PubKey : TECDSA_Public; const digest : AnsiString; Signature : TECDSA_SIG) : Boolean; overload;
    class procedure InitCrypto;
    class function IsHumanReadable(const ReadableText : TRawBytes) : Boolean;
  End;

Const
  CT_TECDSA_Public_Nul : TECDSA_Public = (EC_OpenSSL_NID:0;x:'';y:'');

implementation

uses
  ULog, MicroCoin.Common.Config, MicroCoin.Account.AccountKey,MicroCoin.Common.Stream;

Var _initialized : Boolean = false;

Procedure _DoInit;
Begin
  if Not (_initialized) then begin
    _initialized := true;
    InitSSLFunctions;
  end;
End;

{ TECPrivateKey }

constructor TECPrivateKey.Create;
begin
  FPrivateKey := Nil;
  FEC_OpenSSL_NID := cDefault_EC_OpenSSL_NID;
end;

destructor TECPrivateKey.Destroy;
begin
  if Assigned(FPrivateKey) then EC_KEY_free(FPrivateKey);
  inherited;
end;

function TECPrivateKey.ExportToRaw: TRawBytes;
Var ms : TStream;
  aux : TRawBytes;
begin
  ms := TMemoryStream.Create;
  Try
    ms.Write(FEC_OpenSSL_NID,sizeof(FEC_OpenSSL_NID));
    SetLength(aux,BN_num_bytes(EC_KEY_get0_private_key(FPrivateKey)));
    BN_bn2bin(EC_KEY_get0_private_key(FPrivateKey),@aux[1]);
    ms.WriteAnsiString(aux);
    SetLength(Result,ms.Size);
    ms.Position := 0;
    ms.Read(Result[1],ms.Size);
  Finally
    ms.Free;
  End;
end;

procedure TECPrivateKey.GenerateRandomPrivateKey(EC_OpenSSL_NID : Word);
Var i : Integer;
begin
  if Assigned(FPrivateKey) then EC_KEY_free(FPrivateKey);
  FEC_OpenSSL_NID := EC_OpenSSL_NID;
  FPrivateKey := EC_KEY_new_by_curve_name(EC_OpenSSL_NID);
  i := EC_KEY_generate_key(FPrivateKey);
  if i<>1 then Raise ECryptoException.Create('Error generating new Random Private Key');
end;

function TECPrivateKey.GetPublicKey: TECDSA_Public;
var
  BNx,BNy : PBIGNUM;
  ctx : PBN_CTX;
begin
  Result.EC_OpenSSL_NID := FEC_OpenSSL_NID;
  ctx := BN_CTX_new;
  BNx := BN_new;
  BNy := BN_new;
  Try
    EC_POINT_get_affine_coordinates_GFp(EC_KEY_get0_group(FPrivateKey),EC_KEY_get0_public_key(FPrivateKey),BNx,BNy,ctx);
    SetLength(Result.x,BN_num_bytes(BNx));
    BN_bn2bin(BNx,@Result.x[1]);
    SetLength(Result.y,BN_num_bytes(BNy));
    BN_bn2bin(BNy,@Result.y[1]);
  Finally
    BN_CTX_free(ctx);
    BN_free(BNx);
    BN_free(BNy);
  End;
end;

function TECPrivateKey.GetPublicKeyPoint: PEC_POINT;
begin
  Result := EC_KEY_get0_public_key(FPrivateKey);
end;

class function TECPrivateKey.ImportFromRaw(const raw: TRawBytes): TECPrivateKey;
Var ms : TStream;
  aux : TRawBytes;
  BNx : PBIGNUM;
  ECID : Word;
  PAC : PAnsiChar;
begin
  Result := Nil;
  ms := TMemoryStream.Create;
  Try
    ms.WriteBuffer(raw[1],length(raw));
    ms.Position := 0;
    if ms.Read(ECID,sizeof(ECID))<>sizeof(ECID) then exit;
    If ms.ReadAnsiString(aux)<0 then exit;
    BNx := BN_bin2bn(PAnsiChar(aux),length(aux),nil);
    if assigned(BNx) then begin
      try
        PAC := BN_bn2hex(BNx);
        try
          Result := TECPrivateKey.Create;
          Try
            Result.SetPrivateKeyFromHexa(ECID,PAC);
          Except
            FreeAndNil(Result);
            Raise;
          end;
        finally
          OpenSSL_free(PAC);
        end;
      finally
        BN_free(BNx);
      end;
    end;
  Finally
    ms.Free;
  End;
end;

class function TECPrivateKey.IsValidPublicKey(PubKey: TECDSA_Public): Boolean;
Var BNx,BNy : PBIGNUM;
  ECG : PEC_GROUP;
  ctx : PBN_CTX;
  pub_key : PEC_POINT;
begin
  BNx := BN_bin2bn(PAnsiChar(PubKey.x),length(PubKey.x),nil);
  try
    BNy := BN_bin2bn(PAnsiChar(PubKey.y),length(PubKey.y),nil);
    try
      ECG := EC_GROUP_new_by_curve_name(PubKey.EC_OpenSSL_NID);
      try
        pub_key := EC_POINT_new(ECG);
        try
          ctx := BN_CTX_new;
          try
            Result := EC_POINT_set_affine_coordinates_GFp(ECG,pub_key,BNx,BNy,ctx)=1;
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
  if Assigned(FPrivateKey) then EC_KEY_free(FPrivateKey);
  FPrivateKey := Value;
end;

function TECPrivateKey.SetPrivateKeyFromHexa(EC_OpenSSL_NID : Word; hexa : AnsiString) : Boolean;
var bn : PBIGNUM;
  ctx : PBN_CTX;
  pub_key : PEC_POINT;
begin
  Result := false;
  bn := BN_new;
  try
    if BN_hex2bn(@bn,PAnsiChar(hexa))=0 then Raise ECryptoException.Create('Invalid Hexadecimal value:'+hexa);

    if Assigned(FPrivateKey) then EC_KEY_free(FPrivateKey);
    FEC_OpenSSL_NID := EC_OpenSSL_NID;
    FPrivateKey := EC_KEY_new_by_curve_name(EC_OpenSSL_NID);

    if EC_KEY_set_private_key(FPrivateKey,bn)<>1 then raise ECryptoException.Create('Invalid num to set as private key');
    //
    ctx := BN_CTX_new;
    pub_key := EC_POINT_new(EC_KEY_get0_group(FPrivateKey));
    try
      if EC_POINT_mul(EC_KEY_get0_group(FPrivateKey),pub_key,bn,nil,nil,ctx)<>1 then raise ECryptoException.Create('Error obtaining public key');
      EC_KEY_set_public_key(FPrivateKey,pub_key);
      Result := true;
    finally
      BN_CTX_free(ctx);
      EC_POINT_free(pub_key);
    end;
  finally
    BN_free(bn);
  end;
end;

{ TCrypto }

{ New at Build 1.0.2
  Note: Delphi is slowly when working with Strings (allowing space)... so to
  increase speed we use a String as a pointer, and only increase speed if
  needed. Also the same with functions "GetMem" and "FreeMem" }
class procedure TCrypto.DoDoubleSha256(p: PAnsiChar; plength: Cardinal; var ResultSha256: TRawBytes);
Var PS : PAnsiChar;
begin
  If length(ResultSha256)<>32 then SetLength(ResultSha256,32);
  PS := @ResultSha256[1];
  SHA256(p,plength,PS);
  SHA256(PS,32,PS);
end;

class function TCrypto.DoRipeMD160_HEXASTRING(const TheMessage: AnsiString): TRawBytes;
Var PS : PAnsiChar;
  PC : PAnsiChar;
  i : Integer;
begin
  GetMem(PS,33);
  RIPEMD160(PAnsiChar(TheMessage),Length(TheMessage),PS);
  PC := PS;
  Result := '';
  for I := 1 to 20 do begin
    Result := Result + IntToHex(PtrInt(PC^),2);
    inc(PC);
  end;
  FreeMem(PS,33);
end;

class function TCrypto.DoRipeMD160AsRaw(p: PAnsiChar; plength: Cardinal): TRawBytes;
Var PS : PAnsiChar;
begin
  SetLength(Result,20);
  PS := @Result[1];
  RIPEMD160(p,plength,PS);
end;

class function TCrypto.DoRipeMD160AsRaw(const TheMessage: AnsiString): TRawBytes;
Var PS : PAnsiChar;
begin
  SetLength(Result,20);
  PS := @Result[1];
  RIPEMD160(PAnsiChar(TheMessage),Length(TheMessage),PS);
end;

class function TCrypto.DoSha256(p: PAnsiChar; plength: Cardinal): TRawBytes;
Var PS : PAnsiChar;
begin
  SetLength(Result,32);
  PS := @Result[1];
  SHA256(p,plength,PS);
end;

class function TCrypto.DoSha256(const TheMessage: AnsiString): TRawBytes;
Var PS : PAnsiChar;
begin
  SetLength(Result,32);
  PS := @Result[1];
  SHA256(PAnsiChar(TheMessage),Length(TheMessage),PS);
  PS:=nil;
end;

class function TCrypto.ECDSASign(Key: TECPrivateKey; const digest: AnsiString): TECDSA_SIG;
Var PECS : PECDSA_SIG;
  p : PAnsiChar;
  i : Integer;
  {$IFDEF OpenSSL10}
  {$ELSE}
  bnr,bns : PPBIGNUM;
  {$ENDIF}
begin
  PECS := ECDSA_do_sign(PAnsiChar(digest),length(digest),Key.FPrivateKey);
  Try
    if PECS = Nil then raise ECryptoException.Create('Error signing');

    {$IFDEF OpenSSL10}
    i := BN_num_bytes(PECS^._r);
    SetLength(Result.r,i);
    p := @Result.r[1];
    i := BN_bn2bin(PECS^._r,p);

    i := BN_num_bytes(PECS^._s);
    SetLength(Result.s,i);
    p := @Result.s[1];
    i := BN_bn2bin(PECS^._s,p);
    {$ELSE}
    new(bnr);
    new(bns);
    ECDSA_SIG_get0(PECS,bnr,bns);
    i := BN_num_bytes(bnr^);
    SetLength(Result.r,i);
    p := @Result.r[1];
    i := BN_bn2bin(bnr^,p);
    i := BN_num_bytes(bns^);
    SetLength(Result.s,i);
    p := @Result.s[1];
    i := BN_bn2bin(bns^,p);
    dispose(bnr);
    dispose(bns);
    {$ENDIF}
  Finally
    ECDSA_SIG_free(PECS);
  End;
end;

class function TCrypto.ECDSAVerify(EC_OpenSSL_NID : Word; PubKey: EC_POINT; const digest: AnsiString; Signature: TECDSA_SIG): Boolean;
Var PECS : PECDSA_SIG;
  PK : PEC_KEY;
  {$IFDEF OpenSSL10}
  {$ELSE}
  bnr,bns : PBIGNUM;
  {$ENDIF}
begin
  PECS := ECDSA_SIG_new;
  Try
    {$IFDEF OpenSSL10}
    BN_bin2bn(PAnsiChar(Signature.r),length(Signature.r),PECS^._r);
    BN_bin2bn(PAnsiChar(Signature.s),length(Signature.s),PECS^._s);
    {$ELSE}
{    ECDSA_SIG_get0(PECS,@bnr,@bns);
    BN_bin2bn(PAnsiChar(Signature.r),length(Signature.r),bnr);
    BN_bin2bn(PAnsiChar(Signature.s),length(Signature.s),bns);}
    bnr := BN_bin2bn(PAnsiChar(Signature.r),length(Signature.r),nil);
    bns := BN_bin2bn(PAnsiChar(Signature.s),length(Signature.s),nil);
    if ECDSA_SIG_set0(PECS,bnr,bns)<>1 then Raise Exception.Create('Dev error 20161019-1 '+ERR_error_string(ERR_get_error(),nil));
    {$ENDIF}

    PK := EC_KEY_new_by_curve_name(EC_OpenSSL_NID);
    EC_KEY_set_public_key(PK,@PubKey);
    Case ECDSA_do_verify(PAnsiChar(digest),length(digest),PECS,PK) of
      1 : Result := true;
      0 : Result := false;
    Else
      raise ECryptoException.Create('Error on Verify');
    End;
    EC_KEY_free(PK);
  Finally
    ECDSA_SIG_free(PECS);
  End;
end;

class function TCrypto.ECDSAVerify(PubKey: TECDSA_Public; const digest: AnsiString; Signature: TECDSA_SIG): Boolean;
Var BNx,BNy : PBIGNUM;
  ECG : PEC_GROUP;
  ctx : PBN_CTX;
  pub_key : PEC_POINT;
begin
  BNx := BN_bin2bn(PAnsiChar(PubKey.x),length(PubKey.x),nil);
  BNy := BN_bin2bn(PAnsiChar(PubKey.y),length(PubKey.y),nil);

  ECG := EC_GROUP_new_by_curve_name(PubKey.EC_OpenSSL_NID);
  pub_key := EC_POINT_new(ECG);
  ctx := BN_CTX_new;
  if EC_POINT_set_affine_coordinates_GFp(ECG,pub_key,BNx,BNy,ctx)=1 then begin
    Result := ECDSAVerify(PubKey.EC_OpenSSL_NID, pub_key^,digest,signature);
  end else begin
    Result := false;
  end;
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
Var i : Integer;
Begin
  Result := true;
  for i := 1 to length(ReadableText) do begin
    if (ord(ReadableText[i])<32) Or (ord(ReadableText[i])>=255) then begin
      Result := false;
      Exit;
    end;
  end;
end;

class function TCrypto.PrivateKey2Hexa(Key: TECPrivateKey): AnsiString;
Var p : PAnsiChar;
begin
  p := BN_bn2hex(EC_KEY_get0_private_key(Key.FPrivateKey));
//  p := BN_bn2hex(Key^.priv_key);
  Result := strpas(p);
  OPENSSL_free(p);
end;


end.
