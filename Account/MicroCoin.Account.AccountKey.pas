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
| Created at: 2018-08-22                                                       |
| Purpose:    Account Key managment and helper routines                        |
| Todo:                                                                        |
|   - Eliminate TStreamOp                                                      |
|==============================================================================}

unit MicroCoin.Account.AccountKey;


{$ifdef FPC}
  {$mode delphi}
{$endif}

interface

uses UCrypto, sysutils, classes, uconst, OpenSSL, ULog, MicroCoin.Common.Lists;

type

  TAccountKey = TECDSA_Public;
  PAccountKey = ^TAccountKey;

  TAccountKeyHelper = record helper for TAccountKey
    function IsValidAccountKey(var errors: AnsiString): Boolean;
    function ToRawString: TRawBytes; overload; inline;
    procedure ToRawString(var dest: TRawBytes); overload;
    class function FromRawString(const rawaccstr: TRawBytes): TAccountKey; overload; static; inline;
    class procedure FromRawString(const rawaccstr: TRawBytes; var dest: TAccountKey); overload; static;
    class function FromPrivateKey(key: TECPrivateKey): TAccountKey; static; inline;
    class function AccountKeyFromImport(const HumanReadable: AnsiString; var RAccountKey: TAccountKey; var errors: AnsiString): Boolean; static;
    function AccountPublicKeyExport: AnsiString;
    class function AccountPublicKeyImport(const HumanReadable: AnsiString; var RAccountKey: TAccountKey; var errors: AnsiString): Boolean; static;
    class function EqualAccountKeys(const account1, account2: TAccountKey): Boolean; static;
    class function GetECInfoTxt(const EC_OpenSSL_NID: Word): AnsiString; static;
    class procedure ValidsEC_OpenSSL_NID(list: TList); static;
    function Equals(other: TAccountKey) : boolean; inline;
  end;

const
  CT_Base58: AnsiString = '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz';

implementation

uses MicroCoin.Common.Stream;

class procedure TAccountKeyHelper.ValidsEC_OpenSSL_NID(list: TList);
begin
  list.Clear;
  list.Add(TObject(cNID_secp256k1)); // = 714
  list.Add(TObject(cNID_secp384r1)); // = 715
  list.Add(TObject(cNID_sect283k1)); // = 729
  list.Add(TObject(cNID_secp521r1)); // = 716
end;

class function TAccountKeyHelper.GetECInfoTxt(const EC_OpenSSL_NID: Word): AnsiString;
begin
  case EC_OpenSSL_NID of
    cNID_secp256k1:
      begin
        Result := 'secp256k1';
      end;
    cNID_secp384r1:
      begin
        Result := 'secp384r1';
      end;
    cNID_sect283k1:
      begin
        Result := 'secp283k1';
      end;
    cNID_secp521r1:
      begin
        Result := 'secp521r1';
      end
  else
    Result := '(Unknown ID:' + inttostr(EC_OpenSSL_NID) + ')';
  end;
end;

function TAccountKeyHelper.ToRawString: TRawBytes;
begin
  ToRawString(Result);
end;

procedure TAccountKeyHelper.ToRawString(var dest: TRawBytes);
var
  s: TMemoryStream;
begin
  s := TMemoryStream.Create;
  try
    s.WriteAccountKey(self);
    SetLength(dest, s.Size);
    s.Position := 0;
    s.Read(dest[1], s.Size);
  finally
    s.Free;
  end;
end;

class function TAccountKeyHelper.FromPrivateKey(key: TECPrivateKey): TAccountKey;
begin
  Result := key.PublicKey;
end;

class function TAccountKeyHelper.FromRawString(const rawaccstr: TRawBytes): TAccountKey;
begin
  TAccountKey.FromRawString(rawaccstr, Result);
end;

class procedure TAccountKeyHelper.FromRawString(const rawaccstr: TRawBytes; var dest: TAccountKey);
var
  ms: TMemoryStream;
begin
  if length(rawaccstr) = 0 then
  begin
    dest := CT_TECDSA_Public_Nul;
  end else begin
    ms := TMemoryStream.Create;
    try
      ms.WriteBuffer(rawaccstr[1], length(rawaccstr));
      ms.Position := 0;
      ms.ReadAccountKey(dest);
    finally
      ms.Free;
    end;
  end;
end;

function TAccountKeyHelper.IsValidAccountKey(var errors: AnsiString): Boolean;
begin
  errors := '';
  case self.EC_OpenSSL_NID of
    cNID_secp256k1, cNID_secp384r1, cNID_sect283k1, cNID_secp521r1:
      begin
        Result := TECPrivateKey.IsValidPublicKey(self);
        if not Result then
        begin
          errors := Format('Invalid AccountKey type:%d - Length x:%d y:%d Error:%s',
            [self.EC_OpenSSL_NID, length(x), length(y), ERR_error_string(ERR_get_error(), nil)]);
        end;
      end;
  else
    errors := Format('Invalid AccountKey type:%d (Unknown type) - Length x:%d y:%d',
      [self.EC_OpenSSL_NID, length(x), length(y)]);
    Result := False;
  end;
  if (errors = '') and (not Result) then
    errors := ERR_error_string(ERR_get_error(), nil);
end;

class function TAccountKeyHelper.AccountKeyFromImport(const HumanReadable: AnsiString; var RAccountKey: TAccountKey;
  var errors: AnsiString): Boolean;
var
  raw: TRawBytes;
  BN, BNAux, BNBase: TBigNum;
  i, j: Integer;
  s1, s2: AnsiString;
begin
  Result := False;
  errors := 'Invalid length';
  RAccountKey := CT_TECDSA_Public_Nul;
  if length(HumanReadable) < 20 then
    exit;
  BN := TBigNum.Create(0);
  BNAux := TBigNum.Create;
  BNBase := TBigNum.Create(1);
  try
    for i := length(HumanReadable) downto 1 do
    begin
      if (HumanReadable[i] <> ' ') then
      begin
        j := pos(HumanReadable[i], CT_Base58);
        if j = 0 then
        begin
          errors := 'Invalid char "' + HumanReadable[i] + '" at pos ' + inttostr(i) + '/' +
            inttostr(length(HumanReadable));
          exit;
        end;
        BNAux.value := j - 1;
        BNAux.Multiply(BNBase);
        BN.Add(BNAux);
        BNBase.Multiply(length(CT_Base58));
      end;
    end;
    // Last 8 hexa chars are the checksum of others
    s1 := Copy(BN.HexaValue, 3, length(BN.HexaValue));
    s2 := Copy(s1, length(s1) - 7, 8);
    s1 := Copy(s1, 1, length(s1) - 8);
    raw := TCrypto.HexaToRaw(s1);
    s1 := TCrypto.ToHexaString(TCrypto.DoSha256(raw));
    if Copy(s1, 1, 8) <> s2 then
    begin
      // Invalid checksum
      errors := 'Invalid checksum';
      exit;
    end;
    try
      RAccountKey := TAccountKey.FromRawString(raw);
      Result := true;
      errors := '';
    except
      // Nothing to do... invalid
      errors := 'Error on conversion from Raw to Account key';
    end;
  finally
    BN.Free;
    BNBase.Free;
    BNAux.Free;
  end;
end;

function TAccountKeyHelper.AccountPublicKeyExport: AnsiString;
var
  raw: TRawBytes;
  BN, BNMod, BNDiv: TBigNum;
begin
  Result := '';
  raw := ToRawString;
  BN := TBigNum.Create;
  BNMod := TBigNum.Create;
  BNDiv := TBigNum.Create(length(CT_Base58));
  try
    BN.HexaValue := '01' + TCrypto.ToHexaString(raw) + TCrypto.ToHexaString(Copy(TCrypto.DoSha256(raw), 1, 4));
    while (not BN.IsZero) do
    begin
      BN.Divide(BNDiv, BNMod);
      if (BNMod.value >= 0) and (BNMod.value < length(CT_Base58)) then
        Result := CT_Base58[Byte(BNMod.value) + 1] + Result
      else
        raise Exception.Create('Error converting to Base 58');
    end;
  finally
    BN.Free;
    BNMod.Free;
    BNDiv.Free;
  end;
end;

class function TAccountKeyHelper.AccountPublicKeyImport(const HumanReadable: AnsiString; var RAccountKey: TAccountKey;
  var errors: AnsiString): Boolean;
var
  raw: TRawBytes;
  BN, BNAux, BNBase: TBigNum;
  i, j: Integer;
  s1, s2: AnsiString;
begin
  Result := False;
  errors := 'Invalid length';
  RAccountKey := CT_TECDSA_Public_Nul;
  if length(HumanReadable) >= 20
  then begin
    BN := TBigNum.Create(0);
    BNAux := TBigNum.Create;
    BNBase := TBigNum.Create(1);
    try
      for i := length(HumanReadable) downto 1 do
      begin
        j := pos(HumanReadable[i], CT_Base58);
        if j = 0 then
        begin
          errors := 'Invalid char "' + HumanReadable[i] + '" at pos ' + inttostr(i) + '/' +
            inttostr(length(HumanReadable));
          exit;
        end;
        BNAux.value := j - 1;
        BNAux.Multiply(BNBase);
        BN.Add(BNAux);
        BNBase.Multiply(length(CT_Base58));
      end;
      // Last 8 hexa chars are the checksum of others
      s1 := Copy(BN.HexaValue, 3, length(BN.HexaValue));
      s2 := Copy(s1, length(s1) - 7, 8);
      s1 := Copy(s1, 1, length(s1) - 8);
      raw := TCrypto.HexaToRaw(s1);
      s1 := TCrypto.ToHexaString(TCrypto.DoSha256(raw));
      if Copy(s1, 1, 8) <> s2 then
      begin
        // Invalid checksum
        errors := 'Invalid checksum';
        exit;
      end;
      try
        RAccountKey := TAccountKey.FromRawString(raw);
        Result := true;
        errors := '';
      except
        // Nothing to do... invalid
        errors := 'Error on conversion from Raw to Account key';
      end;
    finally
      BN.Free;
      BNBase.Free;
      BNAux.Free;
    end;
  end;
end;

class function TAccountKeyHelper.EqualAccountKeys(const account1, account2: TAccountKey): Boolean;
begin
  Result := (account1.EC_OpenSSL_NID = account2.EC_OpenSSL_NID) and (account1.x = account2.x) and
    (account1.y = account2.y);
end;

function TAccountKeyHelper.Equals(other: TAccountKey): boolean;
begin
  Result := TAccountKey.EqualAccountKeys(self, other);
end;

end.
