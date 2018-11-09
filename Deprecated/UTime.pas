unit UTime;

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
{$IFNDEF FPC}
  Winapi.Windows,
{$ELSE}
  {LCLIntf, LCLType, LMessages,}
{$ENDIF}
  SysUtils;

{$IFNDEF FPC}
function TzSpecificLocalTimeToSystemTime(lpTimeZoneInformation: PTimeZoneInformation;
  var lpLocalTime, lpUniversalTime: TSystemTime): BOOL; stdcall;
function SystemTimeToTzSpecificLocalTime(lpTimeZoneInformation: PTimeZoneInformation;
  var lpUniversalTime, lpLocalTime: TSystemTime): BOOL; stdcall;
{$ENDIF}
function DateTime2UnivDateTime(d: TDateTime): TDateTime;
function UnivDateTime2LocalDateTime(d: TDateTime): TDateTime;

function UnivDateTimeToUnix(dtDate: TDateTime): Longint;
function UnixToUnivDateTime(USec: Longint): TDateTime;

function UnixTimeToLocalElapsedTime(USec: Longint): AnsiString;
function DateTimeElapsedTime(dtDate: TDateTime): AnsiString;

implementation

{$IFDEF FPC}

uses DateUtils;
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

function UnixTimeToLocalElapsedTime(USec: Longint): AnsiString;
var
  diff, positivediff: Longint;
begin
  diff := UnivDateTimeToUnix(DateTime2UnivDateTime(now)) - USec;
  if diff < 0 then
    positivediff := diff * (-1)
  else
    positivediff := diff;
  if positivediff < 60 then
    Result := Format(rsSecondsAgo, [inttostr(diff)])
  else if positivediff < (60 * 2) then
    Result := rs1MinuteAgo
  else if positivediff < (60 * 60) then
    Result := Format(rsMinutesAgo, [inttostr(diff div 60)])
  else if positivediff < (60 * 60 * 2) then
    Result := rs1HourAgo
  else if positivediff < (60 * 60 * 24) then
    Result := Format(rsHoursAgo, [inttostr(diff div (60 * 60))])
  else
    Result := Format(rsDaysAgo, [inttostr(diff div (60 * 60 * 24))]);
end;

function DateTimeElapsedTime(dtDate: TDateTime): AnsiString;
begin
  Result := UnixTimeToLocalElapsedTime(UnivDateTimeToUnix(DateTime2UnivDateTime(dtDate)));
end;

function DateTime2UnivDateTime(d: TDateTime): TDateTime;
{$IFDEF FPC}
begin
  Result := LocalTimeToUniversal(d, -GetLocalTimeOffset);
end;
{$ELSE}

var
  TZI: TTimeZoneInformation;
  LocalTime, UniversalTime: TSystemTime;
begin
  GetTimeZoneInformation(TZI);
  DateTimeToSystemTime(d, LocalTime);
  TzSpecificLocalTimeToSystemTime(@TZI, LocalTime, UniversalTime);
  Result := SystemTimeToDateTime(UniversalTime);
end;
{$ENDIF}

function UnivDateTime2LocalDateTime(d: TDateTime): TDateTime;
{$IFDEF FPC}
begin
  Result := UniversalTimeToLocal(d, -GetLocalTimeOffset);
end;

{$ELSE}

var
  TZI: TTimeZoneInformation;
  LocalTime, UniversalTime: TSystemTime;
begin
  GetTimeZoneInformation(TZI);
  DateTimeToSystemTime(d, UniversalTime);
  SystemTimeToTzSpecificLocalTime(@TZI, UniversalTime, LocalTime);
  Result := SystemTimeToDateTime(LocalTime);
end;
{$ENDIF}

function UnivDateTimeToUnix(dtDate: TDateTime): Longint;
begin
  Result := Round((dtDate - UnixStartDate) * 86400);
end;

function UnixToUnivDateTime(USec: Longint): TDateTime;
begin
  Result := (USec / 86400) + UnixStartDate;
end;

end.
