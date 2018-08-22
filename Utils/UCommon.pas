
{ 
  Copyright (c) Albert Molina 2016 - 2018 original code from PascalCoin https://pascalcoin.org/

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.   

  If you like it, consider a donation using BitCoin:
    16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  CREDITS:
  [2017-06-29] Herman Schoenfeld (herman@sphere10.com): Created unit, added IFF functions
  [2017-08-10] Herman Schoenfeld (herman@sphere10.com): Added String2Hex, BinStrComp functions
}

unit UCommon;


{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

type

  TArrayTool<T> = class
    public
      class procedure Swap(var Values : array of T; Item1Index, Item2Index : Integer);
    end;


{ Converts a string to hexidecimal format }
function String2Hex(const Buffer: AnsiString): AnsiString;

{ Binary-safe StrComp replacement. StrComp will return 0 for when str1 and str2 both start with NUL character. }
function BinStrComp(const Str1, Str2 : AnsiString): Integer;


implementation

uses
  Classes, SysUtils, Math;

function String2Hex(const Buffer: AnsiString): AnsiString;
var
  n: Integer;
begin
  Result := '';
  for n := 1 to Length(Buffer) do
    Result := LowerCase(Result + IntToHex(Ord(Buffer[n]), 2));
end;

function BinStrComp(const Str1, Str2: AnsiString): integer;
var Str1Len, Str2Len, i : Integer;
begin
   Str1Len := Length(Str1);
   Str2Len := Length(Str2);
   if (Str1Len < Str2Len) then
     Result := -1
   else if (Str1Len > Str2Len) then
     Result := 1
   else begin
     Result := 0;
     For i:= 1 to Str1Len do begin
       if Str1[i] < Str2[i] then begin
         Result := -1;
         break;
       end else if Str1[i] > Str2[i] then begin
         Result := 1;
         break;
       end
     end;
   end;
end;

class procedure TArrayTool<T>.Swap(var Values : array of T; Item1Index, Item2Index : Integer);
var temp : T; len, recSize : Integer; itemSize : Integer;
begin
  len := Length(Values);
  recSize := SizeOf(T);
  if (Item1Index < 0) OR (Item1Index > len) then Raise Exception.Create('Invalid Parameter: Item1Index out of bounds');
  if (Item2Index < 0) OR (Item2Index > len) then Raise Exception.Create('Invalid Parameter: Item2Index out of bounds');
  temp := Values[Item1Index];
  Values[Item1Index] := Values[Item2Index];
  Values[Item2Index] := temp;
end;


end.

