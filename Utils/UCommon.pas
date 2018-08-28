
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

implementation

uses
  Classes, SysUtils, Math;

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

