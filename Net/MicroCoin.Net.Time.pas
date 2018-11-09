unit MicroCoin.Net.Time;
{
  This unit contains code from PascalCoin:

  Copyright (c) Albert Molina 2016 - 2018 original code from PascalCoin https://pascalcoin.org/

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

{$ifdef FPC}
  {$mode delphi}
{$endif}


interface

uses Sysutils, classes, UThread, SyncObjs, UTime, ULog, UConst;

type
  TNetworkAdjustedTime = class
  private
    FTimesList: TPCThreadList;
    FTimeOffset: Integer;
    FLock: TCriticalSection;
    FTotalCounter: Integer;
    function IndexOfClientIp(list: TList; const clientIp: AnsiString): Integer;
    procedure UpdateMedian(list: TList);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddNewIp(const clientIp: AnsiString; clientTimestamp: Cardinal);
    procedure RemoveIp(const clientIp: AnsiString);
    function GetAdjustedTime: Cardinal;
    property TimeOffset: Integer read FTimeOffset;
    function GetMaxAllowedTimestampForNewBlock: Cardinal;
  end;

implementation

type
  TNetworkAdjustedTimeReg = record
    clientIp: AnsiString; // Client IP allows only 1 connection per IP (not using port)
    TimeOffset: Integer;
    counter: Integer;
    // To prevent a time attack from a single IP with multiple connections, only 1 will be used for calc NAT
  end;

  PNetworkAdjustedTimeReg = ^TNetworkAdjustedTimeReg;

procedure TNetworkAdjustedTime.AddNewIp(const clientIp: AnsiString; clientTimestamp: Cardinal);
var
  l: TList;
  i: Integer;
  P: PNetworkAdjustedTimeReg;
begin
  l := FTimesList.LockList;
  try
    i := IndexOfClientIp(l, clientIp);
    if i < 0 then
    begin
      New(P);
      P^.clientIp := clientIp;
      P^.counter := 0;
      l.Add(P);
    end
    else
    begin
      P := l[i];
    end;
    P^.TimeOffset := clientTimestamp - UnivDateTimeToUnix(DateTime2UnivDateTime(now));
    Inc(P^.counter);
    Inc(FTotalCounter);
    UpdateMedian(l);
    TLog.NewLog(ltdebug, Classname, Format('AddNewIp (%s,%d) - Total:%d/%d Offset:%d', [clientIp, clientTimestamp,
      l.Count, FTotalCounter, FTimeOffset]));
  finally
    FTimesList.UnlockList;
  end;
end;

constructor TNetworkAdjustedTime.Create;
begin
  FTimesList := TPCThreadList.Create('TNetworkAdjustedTime_TimesList');
  FTimeOffset := 0;
  FTotalCounter := 0;
end;

destructor TNetworkAdjustedTime.Destroy;
var
  P: PNetworkAdjustedTimeReg;
  i: Integer;
  l: TList;
begin
  l := FTimesList.LockList;
  try
    for i := 0 to l.Count - 1 do
    begin
      P := l[i];
      P^ := Default(TNetworkAdjustedTimeReg);
      Dispose(P);
    end;
    l.Clear;
  finally
    FTimesList.UnlockList;
  end;
  FreeAndNil(FTimesList);
  inherited;
end;

function TNetworkAdjustedTime.GetAdjustedTime: Cardinal;
begin
  Result := UnivDateTimeToUnix(DateTime2UnivDateTime(now)) + FTimeOffset;
end;

function TNetworkAdjustedTime.GetMaxAllowedTimestampForNewBlock: Cardinal;
var
  l: TList;
begin
  l := FTimesList.LockList;
  try
    Result := (GetAdjustedTime + cBlockTimeStampTolerance);
  finally
    FTimesList.UnlockList;
  end;
end;

function TNetworkAdjustedTime.IndexOfClientIp(list: TList; const clientIp: AnsiString): Integer;
begin
  for Result := 0 to list.Count - 1 do
  begin
    if AnsiSameStr(PNetworkAdjustedTimeReg(list[Result])^.clientIp, clientIp) then
      exit;
  end;
  Result := -1;
end;

procedure TNetworkAdjustedTime.RemoveIp(const clientIp: AnsiString);
var
  l: TList;
  i: Integer;
  P: PNetworkAdjustedTimeReg;
begin
  l := FTimesList.LockList;
  try
    i := IndexOfClientIp(l, clientIp);
    if (i >= 0) then
    begin
      P := l[i];
      dec(P^.counter);
      if (P^.counter <= 0) then
      begin
        l.Delete(i);
        Dispose(P);
      end;
      dec(FTotalCounter);
    end;
    UpdateMedian(l);
    if (i >= 0) then
      TLog.NewLog(ltdebug, Classname, Format('RemoveIp (%s) - Total:%d/%d Offset:%d', [clientIp, l.Count, FTotalCounter,
        FTimeOffset]))
    else
      TLog.NewLog(ltError, Classname, Format('RemoveIp not found (%s) - Total:%d/%d Offset:%d',
        [clientIp, l.Count, FTotalCounter, FTimeOffset]))
  finally
    FTimesList.UnlockList;
  end;
end;

function SortPNetworkAdjustedTimeReg(P1, P2: Pointer): Integer;
begin
  Result := PNetworkAdjustedTimeReg(P1)^.TimeOffset - PNetworkAdjustedTimeReg(P2)^.TimeOffset;
end;

procedure TNetworkAdjustedTime.UpdateMedian(list: TList);
var
  last: Integer;
  i: Integer;
  s: string;
begin
  last := FTimeOffset;
  list.Sort(SortPNetworkAdjustedTimeReg);
  if list.Count < cMinimumNodeCountToCalculateNAT then
  begin
    FTimeOffset := 0;
  end
  else if ((list.Count mod 2) = 0) then
  begin
    FTimeOffset := (PNetworkAdjustedTimeReg(list[(list.Count div 2) - 1])^.TimeOffset +
      PNetworkAdjustedTimeReg(list[(list.Count div 2)])^.TimeOffset) div 2;
  end
  else
  begin
    FTimeOffset := PNetworkAdjustedTimeReg(list[list.Count div 2])^.TimeOffset;
  end;
  if (last <> FTimeOffset) then
  begin
    s := '';
    for i := 0 to list.Count - 1 do
    begin
      s := s + ',' + Inttostr(PNetworkAdjustedTimeReg(list[i])^.TimeOffset);
    end;
    TLog.NewLog(ltInfo, Classname,
      Format('Updated NAT median offset. My offset is now %d (before %d) based on %d/%d connections %s',
      [FTimeOffset, last, list.Count, FTotalCounter, s]));
  end;
end;

end.
