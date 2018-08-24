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


{$endif}

uses SysUtils, classes, UCrypto, UCommon, Generics.Collections, Generics.Defaults;

type

  TOrderedList = class
  private
    FOrderedList : TList<Cardinal>;
    FDisabledsCount : Integer;
    FModifiedWhileDisabled : Boolean;
    FOnListChanged: TNotifyEvent;
    procedure NotifyChanged;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(Value : Cardinal) : Integer;
    procedure Remove(Value : Cardinal);
    procedure Clear;
    function Get(index : Integer) : Cardinal;
    function Count : Integer;
    function Find(const Value: Cardinal; var Index: Integer): Boolean;
    procedure Disable;
    procedure Enable;
    property OnListChanged : TNotifyEvent read FOnListChanged write FOnListChanged;
    procedure CopyFrom(Sender : TOrderedList);
    function ToArray : TArray<Cardinal>;
  end;

  TOrderedRawList = Class
  private
    FList : TList;
    function Find(const RawData: TRawBytes; var Index: Integer): Boolean;
  public
    constructor Create;
    destructor Destroy; Override;
    procedure Clear;
    function Add(Const RawData : TRawBytes; tagValue : Integer = 0) : Integer;
    function Count : Integer;
    function Get(index : Integer) : TRawBytes;
    procedure Delete(index : Integer);
    procedure SetTag(Const RawData : TRawBytes; newTagValue : Integer);
    function GetTag(Const RawData : TRawBytes) : Integer; overload;
    function GetTag(index : Integer) : Integer; overload;
    function IndexOf(Const RawData : TRawBytes) : Integer;
  end;


implementation

function TOrderedList.Add(Value: Cardinal): Integer;
begin
  if Find(Value,Result) then exit
  else begin
    FOrderedList.Insert(Result,Value);
    NotifyChanged;
  end;
end;

procedure TOrderedList.Clear;
begin
  FOrderedList.Clear;
  NotifyChanged;
end;

procedure TOrderedList.CopyFrom(Sender: TOrderedList);
Var i : Integer;
begin
  if Self=Sender then exit;
  Disable;
  Try
    Clear;
    for I := 0 to Sender.Count - 1 do begin
      Add(Sender.Get(i));
    end;
  Finally
    Enable;
  End;
end;

function TOrderedList.Count: Integer;
begin
  Result := FOrderedList.Count;
end;

constructor TOrderedList.Create;
begin
  FOrderedList := TList<Cardinal>.Create;
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
  if FDisabledsCount<=0 then raise Exception.Create('Dev error. Invalid disabled counter');
  dec(FDisabledsCount);
  if (FDisabledsCount=0) And (FModifiedWhileDisabled) then NotifyChanged;
end;

function TOrderedList.Find(const Value: Cardinal; var Index: Integer): Boolean;
var L, H, I: Integer;
  C : Int64;
begin
  Result := False;
  L := 0;
  H := FOrderedList.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := Int64(FOrderedList[I]) - Int64(Value);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

function TOrderedList.Get(index: Integer): Cardinal;
begin
  Result := FOrderedList[index];
end;

procedure TOrderedList.NotifyChanged;
begin
  if FDisabledsCount>0 then begin
    FModifiedWhileDisabled := true;
    exit;
  end;
  FModifiedWhileDisabled := false;
  if Assigned(FOnListChanged) then FOnListChanged(Self);
end;

procedure TOrderedList.Remove(Value: Cardinal);
Var i : Integer;
begin
  if Find(Value,i) then begin
    FOrderedList.Delete(i);
    NotifyChanged;
  end;
end;

function TOrderedList.ToArray : TArray<Cardinal>;
var i : integer;
begin
  SetLength(Result, self.Count);
  for i := 0 to self.Count - 1 do
    Result[i] := Self.Get(i);
end;


Type TRawListData = Record
    RawData : TRawBytes;
    tag : Integer;
  End;
  PRawListData = ^TRawListData;

function TOrderedRawList.Add(const RawData: TRawBytes; tagValue : Integer = 0) : Integer;
Var P : PRawListData;
begin
  if Find(RawData,Result) then begin
    PRawListData(FList[Result])^.tag := tagValue;
  end else begin
    New(P);
    P^.RawData := RawData;
    P^.tag := tagValue;
    FList.Insert(Result,P);
  end;
end;

procedure TOrderedRawList.Clear;
Var P : PRawListData;
  i : Integer;
begin
  for i := FList.Count - 1 downto 0 do begin
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
Var P : PRawListData;
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


function TOrderedRawList.Find(const RawData: TRawBytes; var Index: Integer): Boolean;
var L, H, I: Integer;
  c : Integer;
begin
  Result := False;
  L := 0;
  H := FList.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    c := BinStrComp(PRawListData(FList[i])^.RawData,RawData);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
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
Var i : Integer;
begin
  if not Find(RawData,i) then begin
    Result := 0;
  end else begin
    Result := PRawListData(FList[i])^.tag;
  end;
end;

function TOrderedRawList.IndexOf(const RawData: TRawBytes): Integer;
begin
  if not Find(RawData,Result) then Result := -1;
end;

procedure TOrderedRawList.SetTag(const RawData: TRawBytes; newTagValue: Integer);
begin
  Add(RawData,newTagValue);
end;


end.
