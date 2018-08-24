unit MicroCoin.Account.AccountKey;

{
  This unit contains code from PascalCoin:

  Copyright (c) Albert Molina 2016 - 2018 original code from PascalCoin https://pascalcoin.org/

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

}

interface

uses UCrypto, sysutils, classes, uconst, UOpenSSL, ULog, MicroCoin.Common.Lists;

type

  TAccountKey = TECDSA_Public;
  PAccountKey = ^TAccountKey;

  TAccountKeyHelper = record helper for TAccountKey
    function IsValidAccountKey(var errors : AnsiString): Boolean;
    function ToRawString: TRawBytes; overload;
    procedure ToRawString(var dest: TRawBytes); overload;
    class function FromRawString(const rawaccstr: TRawBytes): TAccountKey; overload; static;
    class procedure FromRawString(const rawaccstr: TRawBytes; var dest: TAccountKey); overload; static;
    class function FromPrivateKey(key: TECPrivateKey): TAccountKey; static;
    class function AccountKeyFromImport(Const HumanReadable : AnsiString; var account : TAccountKey; var errors : AnsiString) : Boolean; static;
    function AccountPublicKeyExport : AnsiString;
    class function AccountPublicKeyImport(Const HumanReadable : AnsiString; var account : TAccountKey; var errors : AnsiString) : Boolean; static;
    class function EqualAccountKeys(const account1,account2 : TAccountKey) : Boolean; static;
    class function GetECInfoTxt(const EC_OpenSSL_NID: Word): AnsiString; static;
    class procedure ValidsEC_OpenSSL_NID(list: TList); static;
 end;

  TStreamOp = Class
  public
    class Function WriteAnsiString(Stream: TStream; const value: AnsiString): Integer; overload;
    class Function ReadAnsiString(Stream: TStream; var value: AnsiString): Integer; overload;
    class Function WriteAccountKey(Stream: TStream; const value: TAccountKey): Integer;
    class Function ReadAccountKey(Stream: TStream; var value : TAccountKey): Integer;
  end;




const CT_Base58 : AnsiString = '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz';


implementation

class procedure TAccountKeyHelper.ValidsEC_OpenSSL_NID(list: TList);
begin
  list.Clear;
  list.Add(TObject(CT_NID_secp256k1)); // = 714
  list.Add(TObject(CT_NID_secp384r1)); // = 715
  list.Add(TObject(CT_NID_sect283k1)); // = 729
  list.Add(TObject(CT_NID_secp521r1)); // = 716
end;

class function TAccountKeyHelper.GetECInfoTxt(const EC_OpenSSL_NID: Word): AnsiString;
begin
  case EC_OpenSSL_NID of
    CT_NID_secp256k1:
      begin
        Result := 'secp256k1';
      end;
    CT_NID_secp384r1:
      begin
        Result := 'secp384r1';
      end;
    CT_NID_sect283k1:
      begin
        Result := 'secp283k1';
      end;
    CT_NID_secp521r1:
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
Var s : TMemoryStream;
begin
  s := TMemoryStream.Create;
  try
    TStreamOp.WriteAccountKey(s,self);
    SetLength(dest,s.Size);
    s.Position := 0;
    s.Read(dest[1],s.Size);
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
  TAccountKey.FromRawString(rawaccstr,Result);
end;

class procedure TAccountKeyHelper.FromRawString(const rawaccstr: TRawBytes; var dest: TAccountKey);
Var ms : TMemoryStream;
begin
  if length(rawaccstr)=0 then begin
    dest := CT_TECDSA_Public_Nul;
    exit;
  end;
  ms := TMemoryStream.Create;
  try
    ms.WriteBuffer(rawaccstr[1],length(rawaccstr));
    ms.Position := 0;
    TStreamOp.ReadAccountKey(ms,dest);
  finally
    ms.Free;
  end;
end;

function TAccountKeyHelper.IsValidAccountKey(var errors: AnsiString): Boolean;
begin
  errors := '';
  case self.EC_OpenSSL_NID of
    CT_NID_secp256k1,CT_NID_secp384r1,CT_NID_sect283k1,CT_NID_secp521r1 : begin
      Result := TECPrivateKey.IsValidPublicKey(self);
      if Not Result then begin
        errors := Format('Invalid AccountKey type:%d - Length x:%d y:%d Error:%s',[self.EC_OpenSSL_NID,length(x),length(y),  ERR_error_string(ERR_get_error(),nil)]);
      end;
    end;
  else
    errors := Format('Invalid AccountKey type:%d (Unknown type) - Length x:%d y:%d',[self.EC_OpenSSL_NID,length(x),length(y)]);
    Result := False;
  end;
  if (errors='') And (Not Result) then errors := ERR_error_string(ERR_get_error(),nil);
end;


class function TAccountKeyHelper.AccountKeyFromImport(const HumanReadable: AnsiString; var account: TAccountKey; var errors : AnsiString): Boolean;
Var raw : TRawBytes;
  BN, BNAux, BNBase : TBigNum;
  i,j : Integer;
  s1,s2 : AnsiString;
  i64 : Int64;
  b : Byte;
begin
  result := false;
  errors := 'Invalid length';
  account := CT_TECDSA_Public_Nul;
  if length(HumanReadable)<20 then exit;
  BN := TBigNum.Create(0);
  BNAux := TBigNum.Create;
  BNBase := TBigNum.Create(1);
  try
    for i := length(HumanReadable) downto 1 do begin
      if (HumanReadable[i]<>' ') then begin
        j := pos(HumanReadable[i],CT_Base58);
        if j=0 then begin
          errors := 'Invalid char "'+HumanReadable[i]+'" at pos '+inttostr(i)+'/'+inttostr(length(HumanReadable));
          exit;
        end;
        BNAux.Value := j-1;
        BNAux.Multiply(BNBase);
        BN.Add(BNAux);
        BNBase.Multiply(length(CT_Base58));
      end;
    end;
    // Last 8 hexa chars are the checksum of others
    s1 := Copy(BN.HexaValue,3,length(BN.HexaValue));
    s2 := copy(s1,length(s1)-7,8);
    s1 := copy(s1,1,length(s1)-8);
    raw := TCrypto.HexaToRaw(s1);
    s1 := TCrypto.ToHexaString( TCrypto.DoSha256(raw) );
    if copy(s1,1,8)<>s2 then begin
      // Invalid checksum
      errors := 'Invalid checksum';
      exit;
    end;
    try
      account := TAccountKey.FromRawString(raw);
      Result := true;
      errors := '';
    except
      // Nothing to do... invalid
      errors := 'Error on conversion from Raw to Account key';
    end;
  Finally
    BN.Free;
    BNBase.Free;
    BNAux.Free;
  end;
end;


function TAccountKeyHelper.AccountPublicKeyExport: AnsiString;
Var raw : TRawBytes;
  BN, BNMod, BNDiv : TBigNum;
  i : Integer;
begin
  Result := '';
  raw := ToRawString;
  BN := TBigNum.Create;
  BNMod := TBigNum.Create;
  BNDiv := TBigNum.Create(Length(CT_Base58));
  try
    BN.HexaValue := '01'+TCrypto.ToHexaString( raw )+TCrypto.ToHexaString(Copy(TCrypto.DoSha256(raw),1,4));
    while (Not BN.IsZero) do begin
      BN.Divide(BNDiv,BNMod);
      If (BNMod.Value>=0) And (BNMod.Value<length(CT_Base58)) then Result := CT_Base58[Byte(BNMod.Value)+1] + Result
      else raise Exception.Create('Error converting to Base 58');
    end;
  finally
    BN.Free;
    BNMod.Free;
    BNDiv.Free;
  end;
end;

class function TAccountKeyHelper.AccountPublicKeyImport(
  const HumanReadable: AnsiString; var account: TAccountKey;
  var errors: AnsiString): Boolean;
Var raw : TRawBytes;
  BN, BNAux, BNBase : TBigNum;
  i,j : Integer;
  s1,s2 : AnsiString;
  i64 : Int64;
  b : Byte;
begin
  result := false;
  errors := 'Invalid length';
  account := CT_TECDSA_Public_Nul;
  if length(HumanReadable)<20 then exit;
  BN := TBigNum.Create(0);
  BNAux := TBigNum.Create;
  BNBase := TBigNum.Create(1);
  try
    for i := length(HumanReadable) downto 1 do begin
      j := pos(HumanReadable[i],CT_Base58);
      if j=0 then begin
        errors := 'Invalid char "'+HumanReadable[i]+'" at pos '+inttostr(i)+'/'+inttostr(length(HumanReadable));
        exit;
      end;
      BNAux.Value := j-1;
      BNAux.Multiply(BNBase);
      BN.Add(BNAux);
      BNBase.Multiply(length(CT_Base58));
    end;
    // Last 8 hexa chars are the checksum of others
    s1 := Copy(BN.HexaValue,3,length(BN.HexaValue));
    s2 := copy(s1,length(s1)-7,8);
    s1 := copy(s1,1,length(s1)-8);
    raw := TCrypto.HexaToRaw(s1);
    s1 := TCrypto.ToHexaString( TCrypto.DoSha256(raw) );
    if copy(s1,1,8)<>s2 then begin
      // Invalid checksum
      errors := 'Invalid checksum';
      exit;
    end;
    try
      account := TAccountKey.FromRawString(raw);
      Result := true;
      errors := '';
    except
      // Nothing to do... invalid
      errors := 'Error on conversion from Raw to Account key';
    end;
  Finally
    BN.Free;
    BNBase.Free;
    BNAux.Free;
  end;
end;

class function TAccountKeyHelper.EqualAccountKeys(const account1, account2: TAccountKey): Boolean;
begin
  Result := (account1.EC_OpenSSL_NID=account2.EC_OpenSSL_NID) And
    (account1.x=account2.x) And (account1.y=account2.y);
end;

class function TStreamOp.ReadAccountKey(Stream: TStream; var value: TAccountKey): Integer;
begin
  if Stream.Size - Stream.Position < 2 then begin
    value := CT_TECDSA_Public_Nul;
    Result := -1;
    exit;
  end;
  stream.Read(value.EC_OpenSSL_NID,SizeOf(value.EC_OpenSSL_NID));
  if (ReadAnsiString(stream,value.x)<=0) then begin
    value := CT_TECDSA_Public_Nul;
    exit;
  end;
  if (ReadAnsiString(stream,value.y)<=0) then begin
    value := CT_TECDSA_Public_Nul;
    exit;
  end;
  Result := value.EC_OpenSSL_NID;
end;

class function TStreamOp.ReadAnsiString(Stream: TStream; var value: AnsiString): Integer;
Var
  l: Word;
begin
  if Stream.Size - Stream.Position < 2 then begin
    value := '';
    Result := -1;
    exit;
  end;
  Stream.Read(l, 2);
  if Stream.Size - Stream.Position < l then begin
    Stream.Position := Stream.Position - 2; // Go back!
    value := '';
    Result := -1;
    exit;
  end;
  SetLength(value, l);
  Stream.ReadBuffer(value[1], l);
  Result := l+2;
end;

class function TStreamOp.WriteAccountKey(Stream: TStream; const value: TAccountKey): Integer;
begin
  Result := stream.Write(value.EC_OpenSSL_NID, SizeOf(value.EC_OpenSSL_NID));
  Result := Result + WriteAnsiString(stream,value.x);
  Result := Result + WriteAnsiString(stream,value.y);
end;

class function TStreamOp.WriteAnsiString(Stream: TStream; const value: AnsiString): Integer;
Var
  l: Word;
begin
  if (Length(value)>(256*256)) then begin
    TLog.NewLog(lterror,Classname,'Invalid stream size! '+Inttostr(Length(value)));
    raise Exception.Create('Invalid stream size! '+Inttostr(Length(value)));
  end;

  l := Length(value);
  Stream.Write(l, 2);
  if (l > 0) then
    Stream.WriteBuffer(value[1], Length(value));
  Result := l+2;
end;

end.
