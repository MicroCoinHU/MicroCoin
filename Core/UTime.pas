unit UTime;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{ Copyright (c) 2017 Albert Molina -   2017 by MicroCoin Developers

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  }

interface

Uses
{$IFnDEF FPC}
  Winapi.Windows,
{$ELSE}
  {LCLIntf, LCLType, LMessages,}
{$ENDIF}
  SysUtils;

{$IFnDEF FPC}
function TzSpecificLocalTimeToSystemTime(lpTimeZoneInformation: PTimeZoneInformation; var lpLocalTime, lpUniversalTime: TSystemTime): BOOL; stdcall;
function SystemTimeToTzSpecificLocalTime(lpTimeZoneInformation: PTimeZoneInformation; var lpUniversalTime,lpLocalTime: TSystemTime): BOOL; stdcall;
{$ENDIF}

Function DateTime2UnivDateTime(d:TDateTime):TDateTime;
Function UnivDateTime2LocalDateTime(d:TDateTime):TDateTime;

function UnivDateTimeToUnix(dtDate: TDateTime): Longint;
function UnixToUnivDateTime(USec: Longint): TDateTime;

function UnixTimeToLocalElapsedTime(USec : Longint) : AnsiString;
Function DateTimeElapsedTime(dtDate : TDateTime) : AnsiString;

implementation

{$IFDEF FPC}
Uses DateUtils;
{$ELSE}
function TzSpecificLocalTimeToSystemTime; external 'kernel32.dll' name 'TzSpecificLocalTimeToSystemTime';
function SystemTimeToTzSpecificLocalTime; external kernel32 name 'SystemTimeToTzSpecificLocalTime';
{$ENDIF}

const
    UnixStartDate: TDateTime = 25569.0; // 01/01/1970

resourcestring
  rs1MinuteAgo = '1 minute ago';
  rsMinutesAgo = '%s minutes ago';
  rs1HourAgo = '1 hour ago';
  rsHoursAgo = '%s hours ago';
  rsDaysAgo = '%s days ago';
  rsSecondsAgo = '%s seconds ago';

function UnixTimeToLocalElapsedTime(USec : Longint) : AnsiString;
Var diff, positivediff : Longint;
Begin
  diff := UnivDateTimeToUnix(DateTime2UnivDateTime(now)) - Usec;
  if diff<0 then positivediff := diff * (-1)
  else positivediff := diff;
  if positivediff<60 then Result := Format(rsSecondsAgo, [inttostr(diff)])
  else if positivediff<(60*2) then Result := rs1MinuteAgo
  else if positivediff<(60*60) then Result := Format(rsMinutesAgo, [inttostr(
    diff div 60)])
  else if positivediff<(60*60*2) then Result := rs1HourAgo
  else if positivediff<(60*60*24) then Result := Format(rsHoursAgo, [inttostr(
    diff div (60*60))])
  else Result := Format(rsDaysAgo, [inttostr(diff div (60*60*24))]);
End;

Function DateTimeElapsedTime(dtDate : TDateTime) : AnsiString;
Begin
  Result := UnixTimeToLocalElapsedTime( UnivDateTimeToUnix(DateTime2UnivDateTime(dtDate)) );
End;

Function DateTime2UnivDateTime(d:TDateTime):TDateTime;
{$IFDEF FPC}
begin
  Result := LocalTimeToUniversal(d,-GetLocalTimeOffset);
end;
{$ELSE}
var
 TZI:TTimeZoneInformation;
 LocalTime, UniversalTime:TSystemTime;
begin
  GetTimeZoneInformation(tzi);
  DateTimeToSystemTime(d,LocalTime);
  TzSpecificLocalTimeToSystemTime(@tzi,LocalTime,UniversalTime);
  Result := SystemTimeToDateTime(UniversalTime);
end;
{$ENDIF}

Function UnivDateTime2LocalDateTime(d:TDateTime):TDateTime;
{$IFDEF FPC}
begin
  Result := UniversalTimeToLocal(d,-GetLocalTimeOffset);
end;

{$ELSE}
var
 TZI:TTimeZoneInformation;
 LocalTime, UniversalTime:TSystemTime;
begin
  GetTimeZoneInformation(tzi);
  DateTimeToSystemTime(d,UniversalTime);
  SystemTimeToTzSpecificLocalTime(@tzi,UniversalTime,LocalTime);
  Result := SystemTimeToDateTime(LocalTime);
end;
{$ENDIF}

function UnivDateTimeToUnix(dtDate: TDateTime): Longint;
begin
  Result := Round((dtDate - UnixStartDate) * 86400);
end;

function UnixToUnivDateTime(USec: Longint): TDateTime;
begin
  Result := (Usec / 86400) + UnixStartDate;
end;

end.
