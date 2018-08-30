unit MicroCoin.Common.Lists;

{
  This unit contains code from PascalCoin:

  Copyright (c) Albert Molina 2016 - 2018 original code from PascalCoin https://pascalcoin.org/

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

}

interface

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

uses SysUtils, classes, UCrypto, UCommon{$ifdef USE_GENERICS}, Generics.Collections, Generics.Defaults{$ENDIF};

type
  {$ifndef USE_GENERICS}
    TCardinalArray = array of cardinal;
  {$endif}

  TOrderedList = class
  private
    FOrderedList: TList{$ifdef USE_GENERICS}<Cardinal>{$endif};
    FDisabledsCount: Integer;
    FModifiedWhileDisabled: Boolean;
    FOnListChanged: TNotifyEvent;
    procedure NotifyChanged;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(Value: Cardinal): Integer;
    procedure Remove(Value: Cardinal);
    procedure Clear;
    function Get(index: Integer): Cardinal;
    function Count: Integer;
    function Find(const Value: Cardinal; var index: Integer): Boolean;
    procedure Disable;
    procedure Enable;
    property OnListChanged: TNotifyEvent read FOnListChanged write FOnListChanged;
    procedure CopyFrom(Sender: TOrderedList);
    function ToArray: {$ifdef USE_GENERICS}TArray<Cardinal>{$else}TCardinalArray{$endif};
  end;

  TOrderedRawList = class
  private
    FList: TList;
    function Find(const RawData: TRawBytes; var index: Integer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(const RawData: TRawBytes; tagValue: Integer = 0): Integer;
    function Count: Integer;
    function Get(index: Integer): TRawBytes;
    procedure Delete(index: Integer);
    procedure SetTag(const RawData: TRawBytes; newTagValue: Integer);
    function GetTag(const RawData: TRawBytes): Integer; overload;
    function GetTag(index: Integer): Integer; overload;
    function IndexOf(const RawData: TRawBytes): Integer;
  end;

implementation

uses MicroCoin.Common;

function TOrderedList.Add(Value: Cardinal): Integer;
begin
  if Find(Value, Result) then
    exit
  else
  begin
    FOrderedList.Insert(Result, {$ifndef USE_GENERICS}Pointer(Value){$else}Value{$endif});
    NotifyChanged;
  end;
end;

procedure TOrderedList.Clear;
begin
  FOrderedList.Clear;
  NotifyChanged;
end;

procedure TOrderedList.CopyFrom(Sender: TOrderedList);
var
  i: Integer;
begin
  if Self = Sender then
    exit;
  Disable;
  try
    Clear;
    for i := 0 to Sender.Count - 1 do
    begin
      Add(Sender.Get(i));
    end;
  finally
    Enable;
  end;
end;

function TOrderedList.Count: Integer;
begin
  Result := FOrderedList.Count;
end;

constructor TOrderedList.Create;
begin
  FOrderedList := TList{$ifdef USE_GENERICS}<Cardinal>{$endif}.Create;
  FDisabledsCount := 0;
  FModifiedWhileDisabled := false;
end;

destructor TOrderedList.Destroy;
begin
  FOrderedList.Free;
  inherited;
end;

procedure TOrderedList.Disable;
begin
  inc(FDisabledsCount);
end;

procedure TOrderedList.Enable;
begin
  if FDisabledsCount <= 0 then
    raise Exception.Create('Dev error. Invalid disabled counter');
  dec(FDisabledsCount);
  if (FDisabledsCount = 0) and (FModifiedWhileDisabled) then
    NotifyChanged;
end;

function TOrderedList.Find(const Value: Cardinal; var index: Integer): Boolean;
var
  L, H, i: Integer;
  C: Int64;
begin
  Result := false;
  L := 0;
  H := FOrderedList.Count - 1;
  while L <= H do
  begin
    i := (L + H) shr 1;
    C := Int64(FOrderedList[i]) - Int64(Value);
    if C < 0 then
      L := i + 1
    else
    begin
      H := i - 1;
      if C = 0 then
      begin
        Result := True;
        L := i;
      end;
    end;
  end;
  index := L;
end;

function TOrderedList.Get(index: Integer): Cardinal;
begin
{$ifdef USE_GENERICS}
  Result := FOrderedList[index];
{$else}
  Result := Cardinal(FOrderedList[index]);
{$endif}
end;

procedure TOrderedList.NotifyChanged;
begin
  if FDisabledsCount > 0 then
  begin
    FModifiedWhileDisabled := True;
    exit;
  end;
  FModifiedWhileDisabled := false;
  if Assigned(FOnListChanged) then
    FOnListChanged(Self);
end;

procedure TOrderedList.Remove(Value: Cardinal);
var
  i: Integer;
begin
  if Find(Value, i) then
  begin
    FOrderedList.Delete(i);
    NotifyChanged;
  end;
end;

function TOrderedList.ToArray: {$ifdef USE_GENERICS}TArray<Cardinal>{$else}TCardinalArray{$endif};
var
  i: Integer;
begin
  SetLength(Result, Self.Count);
  for i := 0 to Self.Count - 1 do
    Result[i] := Self.Get(i);
end;

type
  TRawListData = record

    RawData: TRawBytes;
    tag: Integer;
  end;

  PRawListData = ^TRawListData;

function TOrderedRawList.Add(const RawData: TRawBytes; tagValue: Integer = 0): Integer;
var
  P: PRawListData;
begin
  if Find(RawData, Result) then
  begin
    PRawListData(FList[Result])^.tag := tagValue;
  end
  else
  begin
    New(P);
    P^.RawData := RawData;
    P^.tag := tagValue;
    FList.Insert(Result, P);
  end;
end;

procedure TOrderedRawList.Clear;
var
  P: PRawListData;
  i: Integer;
begin
  for i := FList.Count - 1 downto 0 do
  begin
    P := FList[i];
    Dispose(P);
  end;
  FList.Clear;
end;

function TOrderedRawList.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TOrderedRawList.Create;
begin
  FList := TList.Create;
end;

procedure TOrderedRawList.Delete(index: Integer);
var
  P: PRawListData;
begin
  P := PRawListData(FList[index]);
  FList.Delete(index);
  Dispose(P);
end;

destructor TOrderedRawList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

function TOrderedRawList.Find(const RawData: TRawBytes; var index: Integer): Boolean;
var
  L, H, i: Integer;
  C: Integer;
begin
  Result := false;
  L := 0;
  H := FList.Count - 1;
  while L <= H do
  begin
    i := (L + H) shr 1;
    C := BinStrCompare(PRawListData(FList[i])^.RawData, RawData);
    if C < 0 then
      L := i + 1
    else
    begin
      H := i - 1;
      if C = 0 then
      begin
        Result := True;
        L := i;
      end;
    end;
  end;
  index := L;
end;

function TOrderedRawList.Get(index: Integer): TRawBytes;
begin
  Result := PRawListData(FList[index])^.RawData;
end;

function TOrderedRawList.GetTag(index: Integer): Integer;
begin
  Result := PRawListData(FList[index])^.tag;
end;

function TOrderedRawList.GetTag(const RawData: TRawBytes): Integer;
var
  i: Integer;
begin
  if not Find(RawData, i) then
  begin
    Result := 0;
  end
  else
  begin
    Result := PRawListData(FList[i])^.tag;
  end;
end;

function TOrderedRawList.IndexOf(const RawData: TRawBytes): Integer;
begin
  if not Find(RawData, Result) then
    Result := -1;
end;

procedure TOrderedRawList.SetTag(const RawData: TRawBytes; newTagValue: Integer);
begin
  Add(RawData, newTagValue);
end;

end.
