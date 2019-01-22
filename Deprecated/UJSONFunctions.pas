unit UJSONFunctions;

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

interface

uses
{$IFDEF FPC}
    fpjson, jsonparser,
{$ELSE}
  System.Json,
{$ENDIF}
  SysUtils, DateUtils, Variants, Classes, ULog;

type
{$IFDEF FPC}
  TJSONValue = TJSONData;
{$ENDIF}

  TPCJSONData = class
  private
    FParent: TPCJSONData;
  protected
    function ToJSONFormatted(pretty: Boolean; const prefix: AnsiString): AnsiString; virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    class function ParseJSONValue(const JSONObject: string): TPCJSONData; overload;
    class function ParseJSONValue(const JSONObject: TBytes): TPCJSONData; overload;
    class function _GetCount: Integer;
    function ToJSON(pretty: Boolean): AnsiString;
    procedure SaveToStream(Stream: TStream);
    procedure Assign(PCJSONData: TPCJSONData);
  end;

  TPCJSONDataClass = class of TPCJSONData;

  { TPCJSONVariantValue }

  TPCJSONVariantValue = class(TPCJSONData)
  private
    FOldValue: Variant;
    FWritable: Boolean;
    FValue: Variant;
    procedure SetValue(const Value: Variant);
  protected
    function ToJSONFormatted(pretty: Boolean; const prefix: AnsiString): AnsiString; override;
  public
    constructor Create; override;
    constructor CreateFromJSONValue(JSONValue: TJSONValue);
    property Value: Variant read FValue write SetValue;
    function AsString(DefValue: string): string;
    function AsInteger(DefValue: Integer): Integer;
    function AsInt64(DefValue: Int64): Int64;
    function AsDouble(DefValue: Double): Double;
    function AsBoolean(DefValue: Boolean): Boolean;
    function AsDateTime(DefValue: TDateTime): TDateTime;
    function AsCurrency(DefValue: Currency): Currency;
    function AsCardinal(DefValue: Cardinal): Cardinal;
    function IsNull: Boolean;
  end;

  TPCJSONNameValue = class(TPCJSONData)
  private
    FName: string;
    FValue: TPCJSONData;
    FFreeValue: Boolean;
    procedure SetValue(const Value: TPCJSONData);
  protected
    function ToJSONFormatted(pretty: Boolean; const prefix: AnsiString): AnsiString; override;
  public
    constructor Create(AName: string);
    destructor Destroy; override;
    property name: string read FName;
    property Value: TPCJSONData read FValue write SetValue;
  end;

  TPCJSONArray = class;
  TPCJSONObject = class;

  TPCJSONList = class(TPCJSONData)
  private
    FList: TList;
    function GetItems(Index: Integer): TPCJSONData;
    procedure SetItems(Index: Integer; const Value: TPCJSONData);
  protected
    function GetIndexAsVariant(Index: Integer): TPCJSONVariantValue;
    function GetIndexAsArray(Index: Integer): TPCJSONArray;
    function GetIndexAsObject(Index: Integer): TPCJSONObject;
    procedure CheckCanInsert(Index: Integer; PCJSONData: TPCJSONData); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Items[index: Integer]: TPCJSONData read GetItems write SetItems;
    procedure Insert(Index: Integer; PCJSONData: TPCJSONData);
    procedure Delete(Index: Integer);
    function Count: Integer;
    procedure Clear;
  end;

  TPCJSONArray = class(TPCJSONList)
  private
    procedure GrowToIndex(Index: Integer);
    function GetItemOfType(Index: Integer; DataClass: TPCJSONDataClass): TPCJSONData;
  protected
    function ToJSONFormatted(pretty: Boolean; const prefix: AnsiString): AnsiString; override;
  public
    constructor Create; override;
    constructor CreateFromJSONArray(JSONArray: TJSONArray);
    destructor Destroy; override;
    function GetAsVariant(Index: Integer): TPCJSONVariantValue;
    function GetAsObject(Index: Integer): TPCJSONObject;
    function GetAsArray(Index: Integer): TPCJSONArray;
  end;

  { TPCJSONObject }

  TPCJSONObject = class(TPCJSONList)
  private
    function GetIndexOrCreateName(Name: string): Integer;
    function GetByName(Name: string): TPCJSONNameValue;
  protected
    function ToJSONFormatted(pretty: Boolean; const prefix: AnsiString): AnsiString; override;
    procedure CheckCanInsert(Index: Integer; PCJSONData: TPCJSONData); override;
    procedure CheckValidName(Name: string);
  public
    constructor Create; override;
    constructor CreateFromJSONObject(JSONObject: TJSONObject);
    destructor Destroy; override;
    function FindName(Name: string): TPCJSONNameValue;
    function IndexOfName(Name: string): Integer;
    procedure DeleteName(Name: string);
    function GetAsVariant(Name: string): TPCJSONVariantValue;
    function GetAsObject(Name: string): TPCJSONObject;
    function GetAsArray(Name: string): TPCJSONArray;
    function AsString(ParamName: string; DefValue: string): string;
    function AsInteger(ParamName: string; DefValue: Integer): Integer;
    function AsCardinal(ParamName: string; DefValue: Cardinal): Cardinal;
    function AsInt64(ParamName: string; DefValue: Int64): Int64;
    function AsDouble(ParamName: string; DefValue: Double): Double;
    function AsBoolean(ParamName: string; DefValue: Boolean): Boolean;
    function AsDateTime(ParamName: string; DefValue: TDateTime): TDateTime;
    function AsCurrency(ParamName: string; DefValue: Currency): Currency;
    function SaveAsStream(ParamName: string; Stream: TStream): Integer;
    function LoadAsStream(ParamName: string; Stream: TStream): Integer;
    function GetNameValue(Index: Integer): TPCJSONNameValue;
    function IsNull(ParamName: string): Boolean;
    procedure SetAs(Name: string; Value: TPCJSONData);
  end;

  EPCParametresError = class(Exception);

implementation

function UTF8JSONEncode(plainTxt: string; includeSeparator: Boolean): string;
var
  ws: WideString;
  i: Integer;
begin
  ws := UTF8Encode(plainTxt);
  { ALERT:
    UTF8Encode function deletes last char if equal to #0, so we put it manually
  }
  if copy(plainTxt, length(plainTxt), 1) = #0 then
    ws := ws + #0;
  i := 1;
  result := '"';
  while i <= length(ws) do
  begin
    case ws[i] of
      '/', '\', '"':
        result := result + '\' + ws[i];
      #8:
        result := result + '\b';
      #9:
        result := result + '\t';
      #10:
        result := result + '\n';
      #13:
        result := result + '\r';
      #12:
        result := result + '\f';
    else
      if (ord(ws[i]) < 32) or (ord(ws[i]) > 122) then
        result := result + '\u' + inttohex(ord(ws[i]), 4)
      else
        result := result + ws[i];
    end;
    inc(i);
  end;
  result := result + '"';
end;

{ TPCJSONArray }

constructor TPCJSONArray.Create;
begin
  inherited;

end;

constructor TPCJSONArray.CreateFromJSONArray(JSONArray: TJSONArray);
var
  i: Integer;
begin
  Create;
{$IFDEF FPC}
  for i := 0 to JSONArray.Count - 1 do
  begin
    if (JSONArray.Items[i] is TJSONArray) then
    begin
      Insert(i, TPCJSONArray.CreateFromJSONArray(TJSONArray(JSONArray.Items[i])));
    end
    else if (JSONArray.Items[i] is TJSONObject) then
    begin
      Insert(i, TPCJSONObject.CreateFromJSONObject(TJSONObject(JSONArray.Items[i])));
    end
    else if (JSONArray.Items[i] is TJSONValue) then
    begin
      Insert(i, TPCJSONVariantValue.CreateFromJSONValue(TJSONValue(JSONArray.Items[i])));
    end
    else
      raise EPCParametresError.Create('Invalid TJSON Data: ' + JSONArray.Items[i].ClassName);
  end;
{$ELSE}
  for i := 0 to JSONArray.Size - 1 do
  begin
    if (JSONArray.Get(i) is TJSONArray) then
    begin
      Insert(i, TPCJSONArray.CreateFromJSONArray(TJSONArray(JSONArray.Get(i))));
    end
    else if (JSONArray.Get(i) is TJSONObject) then
    begin
      Insert(i, TPCJSONObject.CreateFromJSONObject(TJSONObject(JSONArray.Get(i))));
    end
    else if (JSONArray.Get(i) is TJSONValue) then
    begin
      Insert(i, TPCJSONVariantValue.CreateFromJSONValue(TJSONValue(JSONArray.Get(i))));
    end
    else
      raise EPCParametresError.Create('Invalid TJSON Data: ' + JSONArray.Get(i).ClassName);
  end;
{$ENDIF}
end;

destructor TPCJSONArray.Destroy;
begin
  inherited;
end;

function TPCJSONArray.GetAsArray(Index: Integer): TPCJSONArray;
begin
  result := GetItemOfType(index, TPCJSONArray) as TPCJSONArray;
end;

function TPCJSONArray.GetAsObject(Index: Integer): TPCJSONObject;
begin
  result := GetItemOfType(index, TPCJSONObject) as TPCJSONObject;
end;

function TPCJSONArray.GetAsVariant(Index: Integer): TPCJSONVariantValue;
begin
  result := GetItemOfType(index, TPCJSONVariantValue) as TPCJSONVariantValue;
end;

function TPCJSONArray.GetItemOfType(Index: Integer; DataClass: TPCJSONDataClass): TPCJSONData;
var
  V, New: TPCJSONData;
begin
  GrowToIndex(index);
  V := GetItems(index);
  if not(V is DataClass) then
  begin
    New := DataClass.Create;
    Items[index] := New;
    V := New;
  end;
  result := V as DataClass;
end;

procedure TPCJSONArray.GrowToIndex(Index: Integer);
begin
  while (index >= Count) do
    Insert(Count, TPCJSONVariantValue.Create);
end;

function TPCJSONArray.ToJSONFormatted(pretty: Boolean; const prefix: AnsiString): AnsiString;
var
  i: Integer;
begin
  if pretty then
    result := prefix + '['
  else
    result := '[';
  for i := 0 to Count - 1 do
  begin
    if (i > 0) then
    begin
      result := result + ',';
      if pretty then
        result := result + #10 + prefix;
    end;
    result := result + Items[i].ToJSONFormatted(pretty, prefix + '   ');
  end;
  result := result + ']';
end;

{ TPCJSONList }

procedure TPCJSONList.CheckCanInsert(Index: Integer; PCJSONData: TPCJSONData);
begin
  if (index < 0) or (index > Count) then
    raise Exception.Create('Invalid insert at index ' + Inttostr(index) + ' (Count:' + Inttostr(Count) + ')');
end;

procedure TPCJSONList.Clear;
begin
  while (FList.Count > 0) do
    Delete(FList.Count - 1);
end;

function TPCJSONList.Count: Integer;
begin
  result := FList.Count;
end;

constructor TPCJSONList.Create;
begin
  inherited;
  FParent := nil;
  FList := TList.Create;
end;

procedure TPCJSONList.Delete(Index: Integer);
var
  M: TPCJSONData;
begin
  M := GetItems(index);
  FList.Delete(index);
  M.Free;
end;

destructor TPCJSONList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TPCJSONList.GetIndexAsArray(Index: Integer): TPCJSONArray;
var
  D: TPCJSONData;
begin
  D := GetItems(index);
  if (not(D is TPCJSONArray)) then
  begin
    result := TPCJSONArray.Create;
    SetItems(index, result);
    D.Free;
  end
  else
    result := TPCJSONArray(D);
end;

function TPCJSONList.GetIndexAsObject(Index: Integer): TPCJSONObject;
var
  D: TPCJSONData;
begin
  D := GetItems(index);
  if (not(D is TPCJSONObject)) then
  begin
    result := TPCJSONObject.Create;
    SetItems(index, result);
    D.Free;
  end
  else
    result := TPCJSONObject(D);
end;

function TPCJSONList.GetIndexAsVariant(Index: Integer): TPCJSONVariantValue;
var
  D: TPCJSONData;
begin
  D := GetItems(index);
  if (not(D is TPCJSONVariantValue)) then
  begin
    result := TPCJSONVariantValue.Create;
    SetItems(index, result);
    D.Free;
  end
  else
    result := TPCJSONVariantValue(D);
end;

function TPCJSONList.GetItems(Index: Integer): TPCJSONData;
begin
  result := FList.Items[index];
end;

procedure TPCJSONList.Insert(Index: Integer; PCJSONData: TPCJSONData);
begin
  CheckCanInsert(index, PCJSONData);
  FList.Insert(index, PCJSONData);
end;

procedure TPCJSONList.SetItems(Index: Integer; const Value: TPCJSONData);
var
  OldP: TPCJSONData;
begin
  OldP := FList.Items[index];
  try
    FList.Items[index] := Value;
  finally
    OldP.Free;
  end;
end;

{ TPCJSONVariantValue }

function VariantToDouble(Value: Variant): Double;
var
  s: string;
begin
  result := 0;
  case varType(Value) of
    varSmallint, varInteger, varSingle, varDouble, varCurrency:
      result := Value;
  else
    begin
      s := VarToStr(Value);
      if s = '' then
        Abort
      else
        result := StrToFloat(s);
    end;
  end;
end;

function TPCJSONVariantValue.AsBoolean(DefValue: Boolean): Boolean;
begin
  try
    result := VarAsType(Value, varBoolean);
  except
    result := DefValue;
  end;
end;

function TPCJSONVariantValue.AsCurrency(DefValue: Currency): Currency;
begin
  try
    result := VariantToDouble(Value);
  except
    result := DefValue;
  end;
end;

function TPCJSONVariantValue.AsCardinal(DefValue: Cardinal): Cardinal;
begin
  result := Cardinal(StrToIntDef(VarToStrDef(Value, ''), DefValue));
end;

function TPCJSONVariantValue.AsDateTime(DefValue: TDateTime): TDateTime;
begin
  try
    result := VarAsType(Value, varDate);
  except
    result := DefValue;
  end;
end;

function TPCJSONVariantValue.AsDouble(DefValue: Double): Double;
begin
  try
    result := VariantToDouble(Value);
  except
    result := DefValue;
  end;
end;

function TPCJSONVariantValue.AsInt64(DefValue: Int64): Int64;
begin
  result := StrToInt64Def(VarToStrDef(Value, ''), DefValue);
end;

function TPCJSONVariantValue.AsInteger(DefValue: Integer): Integer;
begin
  result := StrToIntDef(VarToStrDef(Value, ''), DefValue);
end;

function TPCJSONVariantValue.AsString(DefValue: string): string;
begin
  try
    case varType(Value) of
      varNull:
        result := '';
      varSmallint, varInteger:
        begin
          result := Inttostr(Value);
        end;
      varSingle, varDouble, varCurrency:
        begin
          result := FloatToStr(VariantToDouble(Value));
        end;
      varDate:
        result := DateTimeToStr(Value);
    else
      result := VarToStr(Value);
    end;
  except
    result := DefValue;
  end;
end;

constructor TPCJSONVariantValue.Create;
begin
  inherited;
  FValue := Null;
  FOldValue := Unassigned;
  FWritable := False;
end;

constructor TPCJSONVariantValue.CreateFromJSONValue(JSONValue: TJSONValue);
{$IFNDEF FPC}
var
  D: Double;
  i64: Integer;
  ds, ts: Char;
{$ENDIF}
begin
  Create;
{$IFDEF FPC}
  Value := JSONValue.Value;
{$ELSE}
  if JSONValue is TJSONNumber then
  begin
    D := TJSONNumber(JSONValue).AsDouble;
    if Pos('.', JSONValue.ToString) > 0 then
      i64 := 0
    else
      i64 := TJSONNumber(JSONValue).AsInt;
    ds := FormatSettings.DecimalSeparator;
    ts := FormatSettings.ThousandSeparator;
    FormatSettings.DecimalSeparator := '.';
    FormatSettings.ThousandSeparator := ',';
    try
      if FormatFloat('0.###########', D) = Inttostr(i64) then
        Value := i64
      else
        Value := D;
    finally
      FormatSettings.DecimalSeparator := ds;
      FormatSettings.ThousandSeparator := ts;
    end;
  end
  else if JSONValue is TJSONTrue then
    Value := true
  else if JSONValue is TJSONFalse then
    Value := False
  else if JSONValue is TJSONNull then
    Value := Null
  else
    Value := JSONValue.Value;
{$ENDIF}
end;

function TPCJSONVariantValue.IsNull: Boolean;
begin
  result := VarIsNull(FValue) or VarIsEmpty(FValue);
end;

procedure TPCJSONVariantValue.SetValue(const Value: Variant);
begin
  FOldValue := FValue;
  FValue := Value;
end;

function TPCJSONVariantValue.ToJSONFormatted(pretty: Boolean; const prefix: AnsiString): AnsiString;
var
  ds, ts: Char;
begin
  case varType(Value) of
    varSmallint, varInteger, varByte, varWord, varLongWord, varInt64:
      result := VarToStr(Value);
    varBoolean:
      if (Value) then
        result := 'true'
      else
        result := 'false';
    varNull:
      result := 'null';
    varDate, varDouble:
      begin
        ds := FormatSettings.DecimalSeparator;
        ts := FormatSettings.ThousandSeparator;
        FormatSettings.DecimalSeparator := '.';
        FormatSettings.ThousandSeparator := ',';
        try
          result := FormatFloat('0.###########', Value);
        finally
          FormatSettings.DecimalSeparator := ds;
          FormatSettings.ThousandSeparator := ts;
        end;
      end
  else
    result := UTF8JSONEncode(VarToStr(Value), true);
  end;
end;

{ TPCJSONObject }

function TPCJSONObject.AsBoolean(ParamName: string; DefValue: Boolean): Boolean;
var
  V: Variant;
  VV: TPCJSONVariantValue;
begin
  VV := GetAsVariant(ParamName);
  if (varType(VV.Value) = varNull) and (varType(VV.FOldValue) = varEmpty) then
  begin
    result := DefValue;
    Exit;
  end;
  V := GetAsVariant(ParamName).Value;
  try
    if VarIsNull(V) then
      result := DefValue
    else
      result := VarAsType(V, varBoolean);
  except
    result := DefValue;
  end;
end;

function TPCJSONObject.AsCardinal(ParamName: string; DefValue: Cardinal): Cardinal;
begin
  result := Cardinal(AsInt64(ParamName, DefValue));
end;

function TPCJSONObject.AsCurrency(ParamName: string; DefValue: Currency): Currency;
var
  V: Variant;
  VV: TPCJSONVariantValue;
begin
  VV := GetAsVariant(ParamName);
  if (varType(VV.Value) = varNull) and (varType(VV.FOldValue) = varEmpty) then
  begin
    result := DefValue;
    Exit;
  end;
  V := GetAsVariant(ParamName).Value;
  try
    if VarIsNull(V) then
      result := DefValue
    else
      result := VariantToDouble(V);
  except
    result := DefValue;
  end;
end;

function TPCJSONObject.AsDateTime(ParamName: string; DefValue: TDateTime): TDateTime;
var
  V: Variant;
  VV: TPCJSONVariantValue;
begin
  VV := GetAsVariant(ParamName);
  if (varType(VV.Value) = varNull) and (varType(VV.FOldValue) = varEmpty) then
  begin
    result := DefValue;
    Exit;
  end;
  V := GetAsVariant(ParamName).Value;
  try
    if VarIsNull(V) then
      result := DefValue
    else
      result := VarAsType(V, varDate);
  except
    result := DefValue;
  end;
end;

function TPCJSONObject.AsDouble(ParamName: string; DefValue: Double): Double;
var
  V: Variant;
  VV: TPCJSONVariantValue;
begin
  VV := GetAsVariant(ParamName);
  if (varType(VV.Value) = varNull) and (varType(VV.FOldValue) = varEmpty) then
  begin
    result := DefValue;
    Exit;
  end;
  V := GetAsVariant(ParamName).Value;
  try
    if VarIsNull(V) then
      result := DefValue
    else
      result := VariantToDouble(V);
  except
    result := DefValue;
  end;
end;

function TPCJSONObject.AsInt64(ParamName: string; DefValue: Int64): Int64;
var
  V: Variant;
  VV: TPCJSONVariantValue;
begin
  VV := GetAsVariant(ParamName);
  if (varType(VV.Value) = varNull) and (varType(VV.FOldValue) = varEmpty) then
  begin
    result := DefValue;
    Exit;
  end;
  V := GetAsVariant(ParamName).Value;
  try
    if VarIsNull(V) then
      result := DefValue
    else
      result := StrToInt64Def(VarToStrDef(V, ''), DefValue);
  except
    result := DefValue;
  end;
end;

function TPCJSONObject.AsInteger(ParamName: string; DefValue: Integer): Integer;
var
  V: Variant;
  VV: TPCJSONVariantValue;
begin
  VV := GetAsVariant(ParamName);
  if (varType(VV.Value) = varNull) and (varType(VV.FOldValue) = varEmpty) then
  begin
    result := DefValue;
    Exit;
  end;
  V := GetAsVariant(ParamName).Value;
  try
    if VarIsNull(V) then
      result := DefValue
    else
      result := StrToIntDef(VarToStrDef(V, ''), DefValue);
  except
    result := DefValue;
  end;
end;

function TPCJSONObject.AsString(ParamName: string; DefValue: string): string;
var
  V: Variant;
  VV: TPCJSONVariantValue;
begin
  VV := GetAsVariant(ParamName);
  if (varType(VV.Value) = varNull) and (varType(VV.FOldValue) = varEmpty) then
  begin
    result := DefValue;
    Exit;
  end;
  V := GetAsVariant(ParamName).Value;
  try
    case varType(V) of
      varNull:
        result := '';
      varSmallint, varInteger:
        begin
          result := Inttostr(V);
        end;
      varSingle, varDouble, varCurrency:
        begin
          result := FloatToStr(VariantToDouble(V));
        end;
      varDate:
        result := DateTimeToStr(V);
    else
      result := VarToStr(V);
    end;
  except
    result := DefValue;
  end;
end;

procedure TPCJSONObject.CheckCanInsert(Index: Integer; PCJSONData: TPCJSONData);
begin
  inherited;
  if not Assigned(PCJSONData) then
    raise Exception.Create('Object is nil');
  if not(PCJSONData is TPCJSONNameValue) then
    raise Exception.Create('Object inside a ' + TPCJSONData.ClassName + ' must be a ' + TPCJSONNameValue.ClassName +
      ' (currently ' + PCJSONData.ClassName + ')');
end;

procedure TPCJSONObject.CheckValidName(Name: string);
var
  i: Integer;
begin
  for i := 1 to length(name) do
  begin
    if i = 1 then
    begin
      if not(name[i] in ['a' .. 'z', 'A' .. 'Z', '0' .. '9', '_', '.']) then
        raise Exception.Create(Format('Invalid char %s at pos %d/%d', [name[i], i, length(name)]));
    end
    else
    begin
      if not(name[i] in ['a' .. 'z', 'A' .. 'Z', '0' .. '9', '_', '-', '.']) then
        raise Exception.Create(Format('Invalid char %s at pos %d/%d', [name[i], i, length(name)]));
    end;
  end;
end;

constructor TPCJSONObject.Create;
begin
  inherited;
end;

constructor TPCJSONObject.CreateFromJSONObject(JSONObject: TJSONObject);
var
  i, i2: Integer;
{$IFDEF FPC}
  AName: TJSONStringType;
{$ENDIF}
begin
  Create;
{$IFDEF FPC}
  for i := 0 to JSONObject.Count - 1 do
  begin
    AName := JSONObject.Names[i];
    i2 := GetIndexOrCreateName(JSONObject.Names[i]);
    if (JSONObject.Types[AName] = jtArray) then
    begin
      (Items[i2] as TPCJSONNameValue).Value := TPCJSONArray.CreateFromJSONArray(JSONObject.Arrays[AName]);
    end
    else if (JSONObject.Types[AName] = jtObject) then
    begin
      (Items[i2] as TPCJSONNameValue).Value := TPCJSONObject.CreateFromJSONObject(JSONObject.Objects[AName]);
    end
    else if (JSONObject.Types[AName] in [jtBoolean, jtNull, jtNumber, jtString]) then
    begin
      (Items[i2] as TPCJSONNameValue).Value := TPCJSONVariantValue.CreateFromJSONValue(JSONObject.Items[i]);
    end
    else
      raise EPCParametresError.Create('Invalid TJSON Data in JSONObject.' + AName + ': ' + JSONObject.Items[i]
        .ClassName);
  end;
{$ELSE}
  for i := 0 to JSONObject.Size - 1 do
  begin
    i2 := GetIndexOrCreateName(JSONObject.Get(i).JsonString.Value);
    if (JSONObject.Get(i).JSONValue is TJSONArray) then
    begin
      (Items[i2] as TPCJSONNameValue).Value := TPCJSONArray.CreateFromJSONArray
        (TJSONArray(JSONObject.Get(i).JSONValue));
    end
    else if (JSONObject.Get(i).JSONValue is TJSONObject) then
    begin
      (Items[i2] as TPCJSONNameValue).Value := TPCJSONObject.CreateFromJSONObject
        (TJSONObject(JSONObject.Get(i).JSONValue));
    end
    else if (JSONObject.Get(i).JSONValue is TJSONValue) then
    begin
      (Items[i2] as TPCJSONNameValue).Value := TPCJSONVariantValue.CreateFromJSONValue
        (TJSONValue(JSONObject.Get(i).JSONValue));
    end
    else
      raise EPCParametresError.Create('Invalid TJSON Data in JSONObject.' + JSONObject.Get(i).JsonString.Value + ': ' +
        JSONObject.Get(i).ClassName);
  end;
{$ENDIF}
end;

procedure TPCJSONObject.DeleteName(Name: string);
var
  i: Integer;
begin
  i := IndexOfName(name);
  if (i >= 0) then
  begin
    Delete(i);
  end;
end;

destructor TPCJSONObject.Destroy;
begin

  inherited;
end;

function TPCJSONObject.FindName(Name: string): TPCJSONNameValue;
var
  i: Integer;
begin
  i := IndexOfName(name);
  result := nil;
  if (i >= 0) then
    result := Items[i] as TPCJSONNameValue;
end;

function TPCJSONObject.GetAsArray(Name: string): TPCJSONArray;
var
  NV: TPCJSONNameValue;
begin
  NV := GetByName(name);
  if not(NV.Value is TPCJSONArray) then
  begin
    NV.Value := TPCJSONArray.Create;
  end;
  result := NV.Value as TPCJSONArray;
end;

function TPCJSONObject.GetAsObject(Name: string): TPCJSONObject;
var
  NV: TPCJSONNameValue;
begin
  NV := GetByName(name);
  if not(NV.Value is TPCJSONObject) then
  begin
    NV.Value := TPCJSONObject.Create;
  end;
  result := NV.Value as TPCJSONObject;
end;

function TPCJSONObject.GetAsVariant(Name: string): TPCJSONVariantValue;
var
  NV: TPCJSONNameValue;
begin
  NV := GetByName(name);
  if not(NV.Value is TPCJSONVariantValue) then
  begin
    NV.Value := TPCJSONVariantValue.Create;
  end;
  result := NV.Value as TPCJSONVariantValue;
end;

function TPCJSONObject.GetByName(Name: string): TPCJSONNameValue;
var
  i: Integer;
begin
  i := GetIndexOrCreateName(name);
  result := Items[i] as TPCJSONNameValue;
end;

function TPCJSONObject.GetIndexOrCreateName(Name: string): Integer;
var
  NV: TPCJSONNameValue;
begin
  result := IndexOfName(name);
  if (result < 0) then
  begin
    CheckValidName(name);
    NV := TPCJSONNameValue.Create(name);
    result := FList.Add(NV);
  end;
end;

function TPCJSONObject.GetNameValue(Index: Integer): TPCJSONNameValue;
begin
  result := Items[index] as TPCJSONNameValue;
end;

function TPCJSONObject.IsNull(ParamName: string): Boolean;
var
  i: Integer;
  NV: TPCJSONNameValue;
begin
  i := IndexOfName(ParamName);
  if i < 0 then
    result := true
  else
  begin
    result := False;
    NV := TPCJSONNameValue(FList.Items[i]);
    if (Assigned(NV.Value)) and (NV.Value is TPCJSONVariantValue) then
    begin
      result := TPCJSONVariantValue(NV.Value).IsNull;
    end;
  end;
end;

function TPCJSONObject.IndexOfName(Name: string): Integer;
begin
  for result := 0 to FList.Count - 1 do
  begin
    if (Assigned(FList.Items[result])) and (TObject(FList.Items[result]) is TPCJSONNameValue) then
    begin
      if TPCJSONNameValue(FList.Items[result]).Name = name then
      begin
        Exit;
      end;
    end;
  end;
  result := -1;
end;

function TPCJSONObject.LoadAsStream(ParamName: string; Stream: TStream): Integer;
var
  s: AnsiString;
begin
  s := AsString(ParamName, '');
  if (s <> '') then
  begin
    Stream.Write(s[1], length(s));
  end;
  result := length(s);
end;

function TPCJSONObject.SaveAsStream(ParamName: string; Stream: TStream): Integer;
var
  s: AnsiString;
begin
  Stream.Position := 0;
  SetLength(s, Stream.Size);
  Stream.Read(s[1], Stream.Size);
  GetAsVariant(ParamName).Value := s;
end;

procedure TPCJSONObject.SetAs(Name: string; Value: TPCJSONData);
// When assigning a object with SetAs this will not be freed automatically
var
  NV: TPCJSONNameValue;
  i: Integer;
begin
  i := GetIndexOrCreateName(name);
  NV := Items[i] as TPCJSONNameValue;
  NV.Value := Value;
  NV.FFreeValue := False;
end;

function TPCJSONObject.ToJSONFormatted(pretty: Boolean; const prefix: AnsiString): AnsiString;
var
  i: Integer;
begin
  if pretty then
    result := prefix + '{'
  else
    result := '{';
  for i := 0 to Count - 1 do
  begin
    if (i > 0) then
    begin
      result := result + ',';
      if pretty then
        result := result + #10 + prefix;
    end;
    result := result + Items[i].ToJSONFormatted(pretty, prefix + '   ');
  end;
  result := result + '}';
end;

{ TPCJSONNameValue }

constructor TPCJSONNameValue.Create(AName: string);
begin
  inherited Create;
  FName := AName;
  FValue := TPCJSONData.Create;
  FFreeValue := true;
end;

destructor TPCJSONNameValue.Destroy;
begin
  if FFreeValue then
    FValue.Free;
  inherited;
end;

procedure TPCJSONNameValue.SetValue(const Value: TPCJSONData);
var
  old: TPCJSONData;
begin
  if FValue = Value then
    Exit;
  old := FValue;
  FValue := Value;
  if FFreeValue then
    old.Free;
  FFreeValue := true;
end;

function TPCJSONNameValue.ToJSONFormatted(pretty: Boolean; const prefix: AnsiString): AnsiString;
begin
  if pretty then
    result := prefix
  else
    result := '';
  result := result + UTF8JSONEncode(name, true) + ':' + Value.ToJSONFormatted(pretty, prefix + '   ');
end;

{ TPCJSONData }

var
  _objectsCount: Integer;

procedure TPCJSONData.Assign(PCJSONData: TPCJSONData);
var
  i: Integer;
  NV: TPCJSONNameValue;
  JSOND: TPCJSONData;
  s: AnsiString;
begin
  if not Assigned(PCJSONData) then
    Abort;
  if (PCJSONData is TPCJSONObject) and (Self is TPCJSONObject) then
  begin
    for i := 0 to TPCJSONObject(PCJSONData).Count - 1 do
    begin
      NV := TPCJSONObject(PCJSONData).Items[i] as TPCJSONNameValue;
      if NV.Value is TPCJSONObject then
      begin
        TPCJSONObject(Self).GetAsObject(NV.Name).Assign(NV.Value);
      end
      else if NV.Value is TPCJSONArray then
      begin
        TPCJSONObject(Self).GetAsArray(NV.Name).Assign(NV.Value);
      end
      else if NV.Value is TPCJSONVariantValue then
      begin
        TPCJSONObject(Self).GetAsVariant(NV.Name).Assign(NV.Value);
      end
      else
        raise Exception.Create('Error in TPCJSONData.Assign decoding ' + NV.Name + ' (' + NV.Value.ClassName + ')');
    end;
  end
  else if (PCJSONData is TPCJSONArray) and (Self is TPCJSONArray) then
  begin
    for i := 0 to TPCJSONArray(PCJSONData).Count - 1 do
    begin
      JSOND := TPCJSONArray(PCJSONData).Items[i];
      s := JSOND.ToJSON(False);
      TPCJSONArray(Self).Insert(TPCJSONArray(Self).Count, TPCJSONData.ParseJSONValue(s));
    end;
  end
  else if (PCJSONData is TPCJSONVariantValue) and (Self is TPCJSONVariantValue) then
  begin
    TPCJSONVariantValue(Self).Value := TPCJSONVariantValue(PCJSONData).Value;
  end
  else
  begin
    raise Exception.Create('Error in TPCJSONData.Assign assigning a ' + PCJSONData.ClassName + ' to a ' + ClassName);
  end;

end;

constructor TPCJSONData.Create;
begin
  inc(_objectsCount);
end;

destructor TPCJSONData.Destroy;
begin
  dec(_objectsCount);
  inherited;
end;

class function TPCJSONData.ParseJSONValue(const JSONObject: TBytes): TPCJSONData;
var
  JS: TJSONValue;
{$IFDEF FPC}
  jss: TJSONStringType;
  i: Integer;
{$ENDIF}
begin
  result := nil;
  JS := nil;
{$IFDEF FPC}
  SetLength(jss, length(JSONObject));
  for i := 0 to high(JSONObject) do
    jss[i + 1] := AnsiChar(JSONObject[i]);
  try
    JS := GetJSON(jss);
  except
    on E: Exception do
    begin
     LogDebug(ClassName, 'Error processing JSON: ' + E.Message);
    end;
  end;
{$ELSE}
  try
    JS := TJSONObject.ParseJSONValue(JSONObject, 0);
  except
    on E: Exception do
    begin
      LogDebug(ClassName, 'Error processing JSON: ' + E.Message);
    end;
  end;
{$ENDIF}
  if not Assigned(JS) then
    Exit;
  try
    if JS is TJSONObject then
    begin
      result := TPCJSONObject.CreateFromJSONObject(TJSONObject(JS));
    end
    else if JS is TJSONArray then
    begin
      result := TPCJSONArray.CreateFromJSONArray(TJSONArray(JS));
    end
    else if JS is TJSONValue then
    begin
      result := TPCJSONVariantValue.CreateFromJSONValue(TJSONValue(JS));
    end
    else
      raise EPCParametresError.Create('Invalid TJSON Data type ' + JS.ClassName);
  finally
    JS.Free;
  end;
end;

procedure TPCJSONData.SaveToStream(Stream: TStream);
var
  s: AnsiString;
begin
  s := ToJSON(False);
  Stream.Write(s[1], length(s));
end;

class function TPCJSONData.ParseJSONValue(const JSONObject: string): TPCJSONData;
begin
  result := ParseJSONValue(TEncoding.ASCII.GetBytes(JSONObject));
end;

function TPCJSONData.ToJSON(pretty: Boolean): AnsiString;
begin
  result := ToJSONFormatted(pretty, '');
end;

class function TPCJSONData._GetCount: Integer;
begin
  result := _objectsCount;
end;

initialization

_objectsCount := 0;

end.
