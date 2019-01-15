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
  if Size - Position < 2 then
  begin
    value := CT_TECDSA_Public_Nul;
    Result := -1;
    exit;
  end;
  Read(value.EC_OpenSSL_NID, SizeOf(value.EC_OpenSSL_NID));
  if (ReadAnsiString(value.x) <= 0) then
  begin
    value := CT_TECDSA_Public_Nul;
    exit;
  end;
  if (ReadAnsiString(value.y) <= 0) then
  begin
    value := CT_TECDSA_Public_Nul;
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
