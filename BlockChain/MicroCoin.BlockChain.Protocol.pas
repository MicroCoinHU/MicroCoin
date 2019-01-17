unit MicroCoin.BlockChain.Protocol;

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

uses UCrypto, SysUtils, Classes, MicroCoin.Crypto.Bignum, UBaseTypes, MicroCoin.BlockChain.BlockHeader, MicroCoin.Common.Config, MicroCoin.Account.AccountKey;

type
  TMicroCoinProtocol = class
  public
    class function GetRewardForNewLine(line_index: Cardinal): UInt64;
    class function TargetToCompact(target: TRawBytes): Cardinal;
    class function TargetFromCompact(encoded: Cardinal): TRawBytes;
    class function GetNewTarget(vteorical, vreal: Cardinal; const actualTarget: TRawBytes): TRawBytes;
    class procedure CalcProofOfWork_Part1(const operationBlock: TBlockHeader; var Part1: TRawBytes);
    class procedure CalcProofOfWork_Part3(const operationBlock: TBlockHeader; var Part3: TRawBytes);
    class procedure CalcProofOfWork(const operationBlock: TBlockHeader; var PoW: TRawBytes);
  end;

implementation

class function TMicroCoinProtocol.GetNewTarget(vteorical, vreal: Cardinal; const actualTarget: TRawBytes): TRawBytes;
var
  bnact, bnaux, bn: BigInteger;
  tsTeorical, tsReal, factor1000, factor1000Min, factor1000Max: Int64;
begin
  { Given a teorical time in seconds (vteorical>0) and a real time in seconds (vreal>0)
    and an actual target, calculates a new target
    by % of difference of teorical vs real.

    Increment/decrement is adjusted to +-200% in a full CT_CalcNewTargetBlocksAverage round
    ...so each new target is a maximum +-(100% DIV (CT_CalcNewTargetBlocksAverage DIV 2)) of
    previous target. This makes target more stable.
  }
  tsTeorical := vteorical;
  tsReal := vreal;
  factor1000 := (((tsTeorical - tsReal) * 1000) div (tsTeorical)) * (-1);

  { Important: Note that a -500 is the same that divide by 2 (-100%), and
    1000 is the same that multiply by 2 (+100%), so we limit increase
    in a limit [-500..+1000] for a complete (CT_CalcNewTargetBlocksAverage DIV 2) round }
  if cDifficultyCalcBlocks > 1 then
  begin
    factor1000Min := (-500) div (cDifficultyCalcBlocks div 2);
    factor1000Max := (1000) div (cDifficultyCalcBlocks div 2);
  end
  else
  begin
    factor1000Min := (-500);
    factor1000Max := (1000);
  end;

  if factor1000 < factor1000Min then
    factor1000 := factor1000Min
  else if factor1000 > factor1000Max then
    factor1000 := factor1000Max
  else if factor1000 = 0 then
  begin
    Result := actualTarget;
    exit;
  end;

  // Calc new target by increasing factor (-500 <= x <= 1000)
  bn := factor1000;
  bnact := 0;
  bnact.RawValue := actualTarget;
  bnaux := bnact.Copy;
  bnact.Multiply(factor1000).Divide(1000).Add(bnaux);
  // Adjust to TargetCompact limitations:
  Result := TargetFromCompact(TargetToCompact(bnact.RawValue));
end;

class procedure TMicroCoinProtocol.CalcProofOfWork_Part1(const operationBlock: TBlockHeader; var Part1: TRawBytes);
var
  ms: TMemoryStream;
  s: AnsiString;
begin
  ms := TMemoryStream.Create;
  try
    // Part 1
    ms.Write(operationBlock.block, Sizeof(operationBlock.block)); // Little endian
    s := operationBlock.account_key.ToRawString;
    ms.WriteBuffer(s[1], length(s));
    ms.Write(operationBlock.reward, Sizeof(operationBlock.reward)); // Little endian
    ms.Write(operationBlock.protocol_version, Sizeof(operationBlock.protocol_version)); // Little endian
    ms.Write(operationBlock.protocol_available, Sizeof(operationBlock.protocol_available)); // Little endian
    ms.Write(operationBlock.compact_target, Sizeof(operationBlock.compact_target)); // Little endian
    SetLength(Part1, ms.Size);
    ms.Position := 0;
    ms.Read(Part1[1], ms.Size);
  finally
    ms.Free;
  end;
end;

class procedure TMicroCoinProtocol.CalcProofOfWork_Part3(const operationBlock: TBlockHeader; var Part3: TRawBytes);
var
  ms: TMemoryStream;
  s: AnsiString;
begin
  ms := TMemoryStream.Create;
  try
    ms.WriteBuffer(operationBlock.initial_safe_box_hash[1], length(operationBlock.initial_safe_box_hash));
    ms.WriteBuffer(operationBlock.transactionHash[1], length(operationBlock.transactionHash));
    // Note about fee: Fee is stored in 8 bytes, but only digest first 4 low bytes
    ms.Write(operationBlock.fee, 4);
    SetLength(Part3, ms.Size);
    ms.Position := 0;
    ms.ReadBuffer(Part3[1], ms.Size);
  finally
    ms.Free;
  end;
end;

class procedure TMicroCoinProtocol.CalcProofOfWork(const operationBlock: TBlockHeader; var PoW: TRawBytes);
var
  ms: TMemoryStream;
  s: AnsiString;
begin
  ms := TMemoryStream.Create;
  try
    // Part 1
    ms.Write(operationBlock.block, Sizeof(operationBlock.block)); // Little endian
    s := operationBlock.account_key.ToRawString;
    ms.WriteBuffer(s[1], length(s));
    ms.Write(operationBlock.reward, Sizeof(operationBlock.reward)); // Little endian
    ms.Write(operationBlock.protocol_version, Sizeof(operationBlock.protocol_version)); // Little endian
    ms.Write(operationBlock.protocol_available, Sizeof(operationBlock.protocol_available)); // Little endian
    ms.Write(operationBlock.compact_target, Sizeof(operationBlock.compact_target)); // Little endian
    // Part 2
    ms.WriteBuffer(operationBlock.block_payload[1], length(operationBlock.block_payload));
    // Part 3
    ms.WriteBuffer(operationBlock.initial_safe_box_hash[1], length(operationBlock.initial_safe_box_hash));
    ms.WriteBuffer(operationBlock.transactionHash[1], length(operationBlock.transactionHash));
    // Note about fee: Fee is stored in 8 bytes (Int64), but only digest first 4 low bytes
    ms.Write(operationBlock.fee, 4);
    ms.Write(operationBlock.timestamp, 4);
    ms.Write(operationBlock.nonce, 4);
    TCrypto.DoDoubleSha256(ms.Memory, ms.Size, PoW);
  finally
    ms.Free;
  end;
end;

class function TMicroCoinProtocol.GetRewardForNewLine(line_index: Cardinal): UInt64;
var
  n, i: Cardinal;
begin
  n := (line_index + 1) div cDecreaseReward;
  Result := cInitialBlockReward;
  for i := 1 to n do
  begin
    Result := Result div 2;
  end;
  if (Result < cMinimumReward) then
    Result := cMinimumReward;
end;

class function TMicroCoinProtocol.TargetFromCompact(encoded: Cardinal): TRawBytes;
var
  nbits, offset, i: Cardinal;
  bn: BigInteger;
  raw: TRawBytes;
begin
  {
    Compact Target is a 4 byte value that tells how many "0" must have the hash at left if presented in binay format.
    First byte indicates haw many "0 bits" are on left, so can be from 0x00 to 0xE7
    (Because 24 bits are reserved for 3 bytes, and 1 bit is implicit, max: 256-24-1=231=0xE7)
    Next 3 bytes indicates next value in XOR, stored in RAW format

    Example: If we want a hash lower than 0x0000 0000 0000 65A0 A2F4 +29 bytes
    Binary "0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0110 0101 1010 0000 1010 0010 1111 0100"
    That is 49 zeros on left before first 1. So first byte is 49 decimal = 0x31
    After we have "110 0101 1010 0000 1010 0010 1111 0100 1111 0100" but we only can accept first 3 bytes,
    also note that first "1" is implicit, so value is transformed in
    binary as "10 0101 1010 0000 1010 0010 11" that is 0x96828B
    But note that we must XOR this value, so result offset is: 0x697D74
    Compacted value is: 0x31697D74

    When translate compact target back to target: ( 0x31697D74 )
    0x31 = 49 bits at "0", then 1 bit at "1" followed by XOR 0x697D74 = 0x96828B
    49 "0" bits "0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0"
    0x96828B "1001 0110 1000 0010 1000 1011"
    Hash target = "0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0110 0101 1010 0000 1010 0010 11.. ...."
    Fill last "." with "1"
    Hash target = "0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0110 0101 1010 0000 1010 0010 1111 1111"
    Hash target = 0x00 00 00 00 00 00 65 A0 A2 FF + 29 bytes
    Note that is not exactly the same than expected due to compacted format
  }
  nbits := encoded shr 24;
  i := cMinimumDifficulty shr 24;
  if nbits < i then
    nbits := i; // min nbits
  if nbits > 231 then
    nbits := 231; // max nbits

  offset := (encoded shl 8) shr 8;
  // Make a XOR at offset and put a "1" on the left
  offset := ((offset xor $00FFFFFF) or ($01000000));

  bn := offset;
  bn.LShift(256 - nbits - 25);
  raw := bn.RawValue;
  SetLength(Result, 32);
  FillChar(Result[1], 32, 0);
  for i := 1 to length(raw) do
  begin
    Result[i + 32 - length(raw)] := raw[i];
  end;
end;

class function TMicroCoinProtocol.TargetToCompact(target: TRawBytes): Cardinal;
var
  bn, bn2: BigInteger;
  i: Int64;
  nbits: Cardinal;
  raw: TRawBytes;
  j: Integer;
begin
  { See instructions in explanation of TargetFromCompact }
  Result := 0;
  if length(target) > 32 then
  begin
    raise Exception.Create('Invalid target to compact: ' + TBaseType.ToHexaString(target) + ' (' +
      inttostr(length(target)) + ')');
  end;
  SetLength(raw, 32);
  FillChar(raw[1], 32, 0);
  for j := 1 to length(target) do
  begin
    raw[j + 32 - length(target)] := target[j];
  end;
  target := raw;

  bn := 0;
  bn2 := '8000000000000000000000000000000000000000000000000000000000000000';
  // First bit 1 followed by 0
  bn.RawValue := target;
  nbits := 0;
  while (bn.CompareTo(bn2) < 0) and (nbits < 231) do
  begin
    bn2.RShift(1);
    inc(nbits);
  end;
  i := cMinimumDifficulty shr 24;
  if (nbits < i) then
  begin
    Result := cMinimumDifficulty;
    exit;
  end;
  bn.RShift((256 - 25) - nbits);
  Result := (nbits shl 24) + ((bn.value and $00FFFFFF) xor $00FFFFFF);
end;

end.
