unit UAES;

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

{ This unit is used to encrypt/decrypt using AES256
  Basic source code found at internet:
  http://stackoverflow.com/questions/9723963/delphi-pascal-example-for-calling-openssl-evp-functions

  Original source code probably copyright of Marco Ferrante (ferrante@disi.unige.it) and "shunty" user:
  http://stackoverflow.com/users/197962/shunty

}

{$I config.inc}

interface

uses
  SysUtils, UCrypto;

type
  TAESComp = class
  private
  public
    class function EVP_Encrypt_AES256(Value: TBytes; APassword: TBytes): TBytes; overload;
    class function EVP_Decrypt_AES256(const Value: TBytes; APassword: TBytes; var Decrypted: TBytes): Boolean; overload;
    class function EVP_Encrypt_AES256(const TheMessage, APassword: AnsiString): AnsiString; overload;
    class function EVP_Decrypt_AES256(const EncryptedMessage: TRawBytes; APassword: AnsiString; var Decrypted: AnsiString): Boolean; overload;
  end;

{$IFDEF FPC}
procedure CopyMemory(Destination: Pointer; Source: Pointer; Length: DWORD);
{$ENDIF}

implementation

uses
{$IFNDEF FPC}
  Windows,
{$ELSE}
  {LCLIntf, LCLType, LMessages,}
{$ENDIF}
  OpenSSL, OpenSSLdef;

{$IFDEF FPC}

procedure CopyMemory(Destination: Pointer; Source: Pointer; Length: DWORD);
begin
  Move(Source^, Destination^, Length);
end;
{$ENDIF}


const
  SALT_MAGIC: AnsiString = 'Salted__';
  SALT_MAGIC_LEN: integer = 8;
  SALT_SIZE = 8;

function EVP_GetSalt: TBytes;
begin
  SetLength(result, PKCS5_SALT_LEN);
  RAND_pseudo_bytes(@result[0], PKCS5_SALT_LEN);
end;

function EVP_GetKeyIV(APassword: TBytes; ACipher: PEVP_CIPHER; const ASalt: TBytes; out Key, IV: TBytes): Boolean;
var
  pctx: PEVP_MD_CTX;
{$IFDEF OpenSSL10}
  ctx: EVP_MD_CTX;
{$ENDIF}
  hash: PEVP_MD;
  mdbuff: TBytes;
  mds: integer;
  nkey, niv: integer;
begin
  result := false;
  hash := EVP_sha256;
  mds := 0;
  SetLength(mdbuff, EVP_MAX_MD_SIZE);

  nkey := ACipher.key_len;
  niv := ACipher.iv_len;
  SetLength(Key, nkey);
  SetLength(IV, nkey); // Max size to start then reduce it at the end

  Assert(hash.md_size >= nkey);
  Assert(hash.md_size >= niv);

  // This is pretty much the same way that EVP_BytesToKey works. But that
  // allows multiple passes through the hashing loop and also allows to
  // choose different hashing methods. We have no need for this. The
  // OpenSSL docs say it is out of date and internet sources suggest using
  // something like PKCS5_v2_PBE_keyivgen and/or PKCS5_PBKDF2_HMAC_SHA1
  // but this method is easy to port to the DEC and DCP routines and easy to
  // use in other environments. Ultimately the Key and IV rely on the password
  // and the salt and can be easily reformed.

  // This method relies on the fact that the hashing method produces a key of
  // the correct size. EVP_BytesToKey goes through muptiple hashing passes if
  // necessary to make the key big enough when using smaller hashes.
{$IFDEF OpenSSL10}
  EVP_MD_CTX_init(@ctx);
  pctx := @ctx;
{$ELSE}
  pctx := EVP_MD_CTX_new;
{$ENDIF}
  try
    // Key first
    if EVP_DigestInit_ex(pctx, hash, nil) <> 1 then
      exit;
    if EVP_DigestUpdate(pctx, @APassword[0], Length(APassword)) <> 1 then
      exit;
    if (ASalt <> nil) then
    begin
      if EVP_DigestUpdate(pctx, @ASalt[0], Length(ASalt)) <> 1 then
        exit;
    end;
    if (EVP_DigestFinal_ex(pctx, @Key[0], mds) <> 1) then
      exit;

    // Derive IV next
    if EVP_DigestInit_ex(pctx, hash, nil) <> 1 then
      exit;
    if EVP_DigestUpdate(pctx, @Key[0], mds) <> 1 then
      exit;
    if EVP_DigestUpdate(pctx, @APassword[0], Length(APassword)) <> 1 then
      exit;
    if (ASalt <> nil) then
    begin
      if EVP_DigestUpdate(pctx, @ASalt[0], Length(ASalt)) <> 1 then
        exit;
    end;
    if EVP_DigestFinal_ex(pctx, @IV[0], mds) <> 1 then
      exit;

    SetLength(IV, niv);
    result := true;
  finally
{$IFDEF OpenSSL10}
    EVP_MD_CTX_cleanup(pctx);
{$ELSE}
    EVP_MD_CTX_free(pctx);
{$ENDIF}
  end;
end;

{ TAESComp }

class function TAESComp.EVP_Decrypt_AES256(const EncryptedMessage: TRawBytes; APassword: AnsiString; var Decrypted: AnsiString): Boolean;
var
  bytes_encrypted, bytes_password, bytes_result: TBytes;
begin
  SetLength(bytes_encrypted, Length(EncryptedMessage));
  CopyMemory(bytes_encrypted, @EncryptedMessage[1], Length(EncryptedMessage));
  SetLength(bytes_password, Length(APassword));
  CopyMemory(bytes_password, @APassword[1], Length(APassword));
  result := EVP_Decrypt_AES256(bytes_encrypted, bytes_password, bytes_result);
  if result then
  begin
    SetLength(Decrypted, Length(bytes_result));
    CopyMemory(@Decrypted[1], bytes_result, Length(bytes_result));
  end
  else
    Decrypted := '';
end;

class function TAESComp.EVP_Decrypt_AES256(const Value: TBytes; APassword: TBytes; var Decrypted: TBytes): Boolean;
var
  cipher: PEVP_CIPHER;
  pctx: PEVP_CIPHER_CTX;
{$IFDEF OpenSSL10}
  ctx: EVP_CIPHER_CTX;
{$ENDIF}
  salt, Key, IV, buf: TBytes;
  l: integer;
  src_start, buf_start, out_len: integer;
begin
  result := false;
  cipher := EVP_aes_256_cbc;
  SetLength(salt, SALT_SIZE);
  // First read the magic text and the salt - if any
  if (Length(Value) >= SALT_MAGIC_LEN) and (AnsiString(TEncoding.ASCII.GetString(Value, 0, SALT_MAGIC_LEN)) = SALT_MAGIC) then
  begin
    Move(Value[SALT_MAGIC_LEN], salt[0], SALT_SIZE);
    if not EVP_GetKeyIV(APassword, cipher, salt, Key, IV) then
      exit;
    src_start := SALT_MAGIC_LEN + SALT_SIZE;
  end
  else
  begin
    if not EVP_GetKeyIV(APassword, cipher, nil, Key, IV) then
      exit;
    src_start := 0;
  end;
  {$IFDEF OpenSSL10}
    {$IFDEF LINUX}
      new(pctx);
      EVP_CIPHER_CTX_init(pctx);
    {$ELSE}
      EVP_CIPHER_CTX_init(@ctx);
      pctx := @ctx;
    {$ENDIF}
  {$ELSE}
  pctx := EVP_CIPHER_CTX_new;
  {$ENDIF}
  try
    if EVP_DecryptInit(pctx, cipher, @Key[0], @IV[0]) <> 1 then
      exit;
    SetLength(buf, Length(Value));
    buf_start := 0;
    if EVP_DecryptUpdate(pctx, @buf[buf_start], out_len, @Value[src_start], Length(Value) - src_start) <> 1 then
      exit;
    Inc(buf_start, out_len);
    if EVP_DecryptFinal(pctx, @buf[buf_start], out_len) <> 1 then
      exit;
    Inc(buf_start, out_len);
    SetLength(buf, buf_start);
    Decrypted := buf;
    result := true;
  finally
{$IFDEF OpenSSL10}
    EVP_CIPHER_CTX_cleanup(pctx);
{$ELSE}
    EVP_CIPHER_CTX_free(pctx);
{$ENDIF}
  end;
end;

class function TAESComp.EVP_Encrypt_AES256(const TheMessage, APassword: AnsiString): AnsiString;
var
  bytes_message, bytes_password, bytes_result: TBytes;
begin
  SetLength(bytes_message, Length(TheMessage));
  CopyMemory(bytes_message, @TheMessage[1], Length(TheMessage));
  SetLength(bytes_password, Length(APassword));
  CopyMemory(bytes_password, @APassword[1], Length(APassword));
  bytes_result := EVP_Encrypt_AES256(bytes_message, bytes_password);
  SetLength(result, Length(bytes_result));
  CopyMemory(@result[1], bytes_result, Length(bytes_result));
end;

class function TAESComp.EVP_Encrypt_AES256(Value, APassword: TBytes): TBytes;
var
  cipher: PEVP_CIPHER;
  pctx: PEVP_CIPHER_CTX;
{$IFDEF OpenSSL10}
  ctx: EVP_CIPHER_CTX;
{$ENDIF}
  salt, Key, IV, buf: TBytes;
  block_size: integer;
  buf_start, out_len: integer;
begin
  cipher := EVP_aes_256_cbc;
  salt := EVP_GetSalt;
  EVP_GetKeyIV(APassword, cipher, salt, Key, IV);

  {$IFDEF OpenSSL10}
    {$IFDEF LINUX}
      new(pctx);
      EVP_CIPHER_CTX_init(pctx);
    {$ELSE}
      EVP_CIPHER_CTX_init(@ctx);
      pctx := @ctx;
    {$ENDIF}
  {$ELSE}
  pctx := EVP_CIPHER_CTX_new;
  {$ENDIF}
  try
    EVP_EncryptInit(pctx, cipher, @Key[0], @IV[0]);
    block_size := EVP_CIPHER_CTX_block_size(pctx);
    SetLength(buf, Length(Value) + block_size + SALT_MAGIC_LEN + PKCS5_SALT_LEN);
    buf_start := 0;
    Move(PAnsiChar(SALT_MAGIC)^, buf[buf_start], SALT_MAGIC_LEN);
    Inc(buf_start, SALT_MAGIC_LEN);
    Move(salt[0], buf[buf_start], PKCS5_SALT_LEN);
    Inc(buf_start, PKCS5_SALT_LEN);
    EVP_EncryptUpdate(pctx, @buf[buf_start], out_len, @Value[0], Length(Value));
    Inc(buf_start, out_len);
    EVP_EncryptFinal(pctx, @buf[buf_start], out_len);
    Inc(buf_start, out_len);
    SetLength(buf, buf_start);
    result := buf;
  finally
{$IFDEF OpenSSL10}
    EVP_CIPHER_CTX_cleanup(pctx);
{$ELSE}
    EVP_CIPHER_CTX_free(pctx);
{$ENDIF}
  end;
end;

end.
