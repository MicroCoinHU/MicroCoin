{==============================================================================|
| MicroCoin                                                                    |
| Copyright (c) 2017-2018 MicroCoin Developers                                 |
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
| File:       MicroCoin.Application.Settings.pas                               |
| Created at: 2018-09-17                                                       |
| Purpose:    Application settings manager classes                             |
|==============================================================================}

unit MicroCoin.Application.Settings;

{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}


interface

uses
  Classes, MicroCoin.Account.AccountKey;

type
  TAppSettingType = (stString, stInteger, stLongWord, stInt64, stBoolean, stStream);

  TAppSettings = class;

  // TODO: Variant!!!!
  TAppSettingsEntry = class
  public const
    apTheme = 'AppTheme';
    apGridAccountsStream = 'GridAccountsStreamV2';
    apGridAccountsPos = 'GridAccountsPos';
    apDefaultFee = 'DefaultFee';
    apInternetServerPort = 'InternetServerPort';
{$IFDEF TESTNET}CT_PARAM_AutomaticMineWhenConnectedToNodes = 'AutomaticMineWhenConnectedToNodes'; {$ENDIF}
    apMinerPrivateKeyType = 'MinerPrivateKeyType';
    apMinerPrivateKeySelectedPublicKey = 'MinerPrivateKeySelectedPublicKey';
    apSaveDebugLogs = 'SaveDebugLogs';
    apShowLogs = 'ShowLogs';
    apMinerName = 'MinerName';
    apFirstTime = 'FirstTime';
    apSaveLogFiles = 'SaveLogFiles';
    apShowModalMessages = 'ShowModalMessages';
{$IFDEF TESTNET}CT_PARAM_MaxCPUs = 'MaxCPUs'; {$ENDIF}
    apPeerCache = 'PeerCache';
    apTryToConnectOnlyWithThisFixedServers = 'TryToConnectOnlyWithFixedServers';
    apJSONRPCMinerServerPort = 'JSONRPCMinerServerPort';
    apJSONRPCMinerServerActive = 'JSONRPCMinerServerActive';
    apJSONRPCEnabled = 'JSONRPCEnabled';
    apJSONRPCAllowedIPs = 'JSONRPCAllowedIPs';
    apNotifyOnNewTransaction = 'NotifyNewTransaction';
    apIgnoreOldBlocks = 'IgnoreOldBlocks';
  strict private
    FAppSettings: TAppSettings;
    FName: AnsiString;
    FValue: Variant;
    FType: TAppSettingType;
    procedure SetName(const Value: AnsiString);
    procedure SetValue(const Value: Variant);
    procedure SetType(const Value: TAppSettingType);
    function GetIsNull: Boolean;
  public
    constructor Create(AName: AnsiString);
    function LoadFromStream(AStream: TStream): Boolean;
    procedure SaveToStream(AStream: TStream);
    procedure SetAsInteger(AValue: Integer);
    procedure SetAsCardinal(AValue: Cardinal);
    procedure SetAsString(AValue: AnsiString);
    procedure SetAsInt64(AValue: Int64);
    procedure SetAsBoolean(AValue: Boolean);
    procedure SetAsStream(AStream: TStream);
    function GetAsString(const ADefault: AnsiString): AnsiString;
    function GetAsBoolean(const ADefault: Boolean): Boolean;
    function GetAsInteger(const ADefault: Integer): Integer;
    function GetAsInt64(const ADefault: Int64): Int64;
    function GetAsStream(AStream: TStream): Integer;
    property Name: AnsiString read FName write SetName;
    property IsNull: Boolean read GetIsNull;
    property Value: Variant read FValue write SetValue;
    property ValueType: TAppSettingType read FType write SetType;
    property AppSettings : TAppSettings read FAppSettings write FAppSettings;
  end;

  TAppSettings = class
  private
    FStream: TStream;
    FSettings: TList;
    FFileName: AnsiString;
    function LoadFromStream(AStream: TStream): Boolean;
    procedure SaveToStream(AStream: TStream);
    function Get(AName: AnsiString): TAppSettingsEntry;
    procedure InternalClear;
    function IndexOf(const AName: AnsiString): Integer;
    procedure SetFileName(const Value: AnsiString);
    procedure Save;
  public
    constructor Create;
    destructor Destroy; override;
    class function AppParams: TAppSettings;
    procedure Clear;
    procedure Delete(const AName: AnsiString);
    function Count: Integer;
    function Entry(index: Integer): TAppSettingsEntry;
    function Find(const AName: AnsiString): TAppSettingsEntry;
    property FileName: AnsiString read FFileName write SetFileName;
    property Entries[Name: AnsiString]: TAppSettingsEntry read Get;
  end;

implementation

uses
  Variants, SysUtils, MicroCoin.Common.Stream;

const
  cAppParamsFileMagic = 'TAppParams';

var
  _appParams: TAppSettings;

  { TAppParam }

constructor TAppSettingsEntry.Create(AName: AnsiString);
begin
  FAppSettings := nil;
  FName := AName;
  FValue := Null;
end;

function TAppSettingsEntry.GetAsBoolean(const ADefault: Boolean): Boolean;
begin
  if IsNull then
    Result := ADefault
  else
  begin
    try
      Result := FValue;
    except
      Result := ADefault;
    end;
  end;
end;

function TAppSettingsEntry.GetAsInt64(const ADefault: Int64): Int64;
begin
  if IsNull then
    Result := ADefault
  else
  begin
    try
      Result := FValue;
    except
      Result := ADefault;
    end;
  end;
end;

function TAppSettingsEntry.GetAsInteger(const ADefault: Integer): Integer;
begin
  if IsNull then
    Result := ADefault
  else
  begin
    try
      Result := FValue;
    except
      Result := ADefault;
    end;
  end;
end;

function TAppSettingsEntry.GetAsStream(AStream: TStream): Integer;
var
  s: AnsiString;
begin
  AStream.Size := 0;
  if IsNull then
    Result := 0
  else
  begin
    s := VarToStrDef(FValue, '');
    AStream.Size := 0;
    AStream.WriteBuffer(s[1], length(s));
    AStream.Position := 0;
    Result := AStream.Size;
  end;
end;

function TAppSettingsEntry.GetAsString(const ADefault: AnsiString): AnsiString;
begin
  if IsNull then
    Result := ADefault
  else
    Result := VarToStrDef(FValue, ADefault);
end;

function TAppSettingsEntry.GetIsNull: Boolean;
begin
  Result := VarIsNull(FValue);
end;

function TAppSettingsEntry.LoadFromStream(AStream: TStream): Boolean;
var
  xByteSettingType: Byte;
  xSettingsType: TAppSettingType;
  s: AnsiString;
  i: Integer;
  c: Cardinal;
  i64: Int64;
begin
  Result := false;
  if AStream.ReadAnsiString(FName) < 0 then
    exit;
  AStream.Read(xByteSettingType, 1);
  if (xByteSettingType >= Integer(low(xSettingsType))) and (xByteSettingType <= Integer(high(xSettingsType)))
  then xSettingsType := TAppSettingType(xByteSettingType)
  else xSettingsType := stString;
  FType := xSettingsType;
  AStream.Read(xByteSettingType, 1);
  if xByteSettingType = 0 then
    FValue := Null
  else
  begin
    case xSettingsType of
      stString:
        begin
          if AStream.ReadAnsiString(s) < 0 then
            exit;
          FValue := s;
        end;
      stInteger:
        begin
          if AStream.Read(i, sizeof(i)) < sizeof(i) then
            exit;
          FValue := i;
        end;
      stLongWord:
        begin
          if AStream.Read(c, sizeof(c)) < sizeof(c) then
            exit;
          FValue := c;
        end;
      stInt64:
        begin
          if AStream.Read(i64, sizeof(i64)) < sizeof(i64) then
            exit;
          FValue := i64;
        end;
      stBoolean:
        begin
          if AStream.Read(xByteSettingType, sizeof(xByteSettingType)) < sizeof(xByteSettingType) then
            exit;
          if xByteSettingType = 0 then
            FValue := false
          else
            FValue := true;
        end;
      stStream:
        begin
          if AStream.ReadAnsiString(s) < 0 then
            exit;
          FValue := s;
        end
    else
      raise Exception.Create('Development error 20160613-1');
    end;
  end;
  Result := true;
end;

procedure TAppSettingsEntry.SaveToStream(AStream: TStream);
var
  b: Byte;
  i: Integer;
  c: Cardinal;
  i64: Int64;
begin
  AStream.WriteAnsiString(FName);
  b := Byte(FType);
  AStream.Write(b, 1);
  if IsNull then
  begin
    b := 0;
    AStream.Write(b, 1);
  end
  else
  begin
    b := 1;
    AStream.Write(b, 1);
    case FType of
      stString:
        begin
          AStream.WriteAnsiString(VarToStr(FValue));
        end;
      stInteger:
        begin
          i := FValue;
          AStream.Write(i, sizeof(i));
        end;
      stLongWord:
        begin
          c := FValue;
          AStream.Write(c, sizeof(c));
        end;
      stInt64:
        begin
          i64 := FValue;
          AStream.Write(i64, sizeof(i64));
        end;
      stBoolean:
        begin
          if FValue then
            b := 1
          else
            b := 0;
          AStream.Write(b, sizeof(b));
        end;
      stStream:
        begin
          AStream.WriteAnsiString(VarToStrDef(FValue, ''));
        end
    else
      raise Exception.Create('Development error 20160613-2');
    end;
  end;
end;

procedure TAppSettingsEntry.SetAsBoolean(AValue: Boolean);
begin
  FType := stBoolean;
  FValue := AValue;
  if Assigned(FAppSettings) then
    FAppSettings.Save;
end;

procedure TAppSettingsEntry.SetAsCardinal(AValue: Cardinal);
begin
  FType := stLongWord;
  FValue := AValue;
  if Assigned(FAppSettings) then
    FAppSettings.Save;
end;

procedure TAppSettingsEntry.SetAsInt64(AValue: Int64);
begin
  FType := stInt64;
  FValue := AValue;
  if Assigned(FAppSettings) then
    FAppSettings.Save;
end;

procedure TAppSettingsEntry.SetAsInteger(AValue: Integer);
begin
  FType := stInteger;
  FValue := AValue;
  if Assigned(FAppSettings) then
    FAppSettings.Save;
end;

procedure TAppSettingsEntry.SetAsStream(AStream: TStream);
var
  s: AnsiString;
begin
  AStream.Position := 0;
  setlength(s, AStream.Size);
  AStream.ReadBuffer(s[1], AStream.Size);
  FType := stString;
  FValue := s;
  if Assigned(FAppSettings) then
    FAppSettings.Save;
end;

procedure TAppSettingsEntry.SetAsString(AValue: AnsiString);
begin
  if (FType = stString) and (GetAsString('') = AValue) then
    exit;

  FType := stString;
  FValue := AValue;
  if Assigned(FAppSettings) then
    FAppSettings.Save;
end;

procedure TAppSettingsEntry.SetName(const Value: AnsiString);
begin
  FName := Value;
  if Assigned(FAppSettings) then
    FAppSettings.Save;
end;

procedure TAppSettingsEntry.SetType(const Value: TAppSettingType);
begin
  FType := Value;
  if Assigned(FAppSettings) then
    FAppSettings.Save;
end;

procedure TAppSettingsEntry.SetValue(const Value: Variant);
begin
  FValue := Value;
  if Assigned(FAppSettings) then
    FAppSettings.Save;
end;

{ TAppParams }

class function TAppSettings.AppParams: TAppSettings;
begin
  if not Assigned(_appParams) then
  begin
    _appParams := TAppSettings.Create();
  end;
  Result := _appParams;
end;

procedure TAppSettings.Clear;
begin
  InternalClear;
  Save;
end;

function TAppSettings.Count: Integer;
begin
  Result := FSettings.Count;
end;

constructor TAppSettings.Create;
begin
  inherited;
  FSettings := TList.Create;
  FFileName := '';
  FStream := nil;
  if _appParams = nil then
    _appParams := Self;

end;

procedure TAppSettings.Delete(const AName: AnsiString);
var
  P: TAppSettingsEntry;
  i: Integer;
begin
  i := IndexOf(AName);
  if i < 0 then
    exit;
  P := FSettings[i];
  FSettings.Delete(i);
  P.Free;
  Save;
end;

destructor TAppSettings.Destroy;
begin
  FreeAndNil(FStream);
  InternalClear;
  FSettings.Free;
  inherited;
  if _appParams = Self then
    _appParams := nil;

end;

function TAppSettings.Find(const AName: AnsiString): TAppSettingsEntry;
var
  i: Integer;
begin
  i := IndexOf(AName);
  if i >= 0 then
    Result := FSettings[i]
  else
    Result := nil;
end;

function TAppSettings.Get(AName: AnsiString): TAppSettingsEntry;
var
  i: Integer;
  P: TAppSettingsEntry;
begin
  i := IndexOf(AName);
  if i < 0 then
  begin
    P := TAppSettingsEntry.Create(AName);
    P.AppSettings := Self;
    FSettings.Add(P);
  end
  else
    P := FSettings[i];
  Result := P;
end;

function TAppSettings.IndexOf(const AName: AnsiString): Integer;
begin
  for Result := 0 to FSettings.Count - 1 do
  begin
    if AnsiSameText(AName, TAppSettingsEntry(FSettings[Result]).Name) then
      exit;
  end;
  Result := -1;
end;

procedure TAppSettings.InternalClear;
var
  P: TAppSettingsEntry;
  i: Integer;
begin
  for i := 0 to FSettings.Count - 1 do
  begin
    P := FSettings[i];
    P.Free;
  end;
  FSettings.Clear;
end;

function TAppSettings.LoadFromStream(AStream: TStream): Boolean;
var
  s: AnsiString;
  i, c: Integer;
  P: TAppSettingsEntry;
begin
  Result := false;
  InternalClear;
  if AStream.ReadAnsiString(s) < 0 then
    exit;
  if s <> cAppParamsFileMagic then
    raise Exception.Create('Invalid file type');
  AStream.Read(c, sizeof(c));
  for i := 0 to c - 1 do
  begin
    P := TAppSettingsEntry(TAppSettingsEntry.NewInstance);
    P.AppSettings := Self;
    FSettings.Add(P);
    if not P.LoadFromStream(AStream) then
      exit;
  end;
  Result := true;
end;

function TAppSettings.Entry(index: Integer): TAppSettingsEntry;
begin
  Result := TAppSettingsEntry(FSettings[index]);
end;

procedure TAppSettings.Save;
begin
  if Assigned(FStream) then
  begin
    FStream.Position := 0;
    FStream.Size := 0;
    SaveToStream(FStream);
  end;
end;

procedure TAppSettings.SaveToStream(AStream: TStream);
var
  s: AnsiString;
  i: Integer;
begin
  s := cAppParamsFileMagic;
  AStream.WriteAnsiString(s);
  i := FSettings.Count;
  AStream.Write(i, sizeof(i));
  for i := 0 to FSettings.Count - 1 do
  begin
    TAppSettingsEntry(FSettings[i]).SaveToStream(AStream);
  end;
end;

procedure TAppSettings.SetFileName(const Value: AnsiString);
var
  fm: Word;
begin
  if FFileName = Value then
    exit;
  if Assigned(FStream) then
    FStream.Free;
  FStream := nil;
  FFileName := Value;
  if Value <> '' then
  begin
    if FileExists(Value) then
      fm := fmOpenReadWrite
    else
      fm := fmCreate;

    FStream := TFileStream.Create(Value, fm + fmShareExclusive);
    try
      LoadFromStream(FStream);
    except
      // Do nothing
    end;
  end;
end;

initialization

_appParams := nil;

end.
