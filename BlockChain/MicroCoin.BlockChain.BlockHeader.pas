unit MicroCoin.BlockChain.BlockHeader;

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

uses Sysutils, classes, UCrypto, UBaseTypes, MicroCoin.Account.AccountKey;

type

  TBlockHeader = record
    block: Cardinal;
    account_key: TAccountKey;
    reward: UInt64;
    fee: UInt64;
    protocol_version: Word; // Protocol version
    protocol_available: Word; // Used to upgrade protocol
    timestamp: Cardinal; // Timestamp creation
    compact_target: Cardinal; // Target in compact form
    nonce: Cardinal; // Random value to generate a new P-o-W
    block_payload: TRawBytes; // RAW Payload that a miner can include to a blockchain
    initial_safe_box_hash: TRawBytes; // RAW Safe Box Hash value (32 bytes, it's a Sha256)
    transactionHash: TRawBytes; // RAW sha256 (32 bytes) of Operations
    proof_of_work: TRawBytes; // RAW Double Sha256

    class function Empty : TBlockHeader; static;

    function LoadFromStream(const stream: TStream): Boolean;
    procedure SaveToStream(const stream: TStream);

    function ToString : AnsiString;

    class operator Equal(ABlock1, ABlock2 : TBlockHeader) : Boolean;
    class operator NotEqual(ABlock1, ABlock2 : TBlockHeader) : Boolean;
  end;

implementation

uses MicroCoin.Common.Stream;

procedure TBlockHeader.SaveToStream(const stream: TStream);
begin
  stream.Write(block, Sizeof(block));
  stream.WriteAccountKey(account_key);
  stream.Write(reward, Sizeof(reward));
  stream.Write(fee, Sizeof(fee));
  stream.Write(protocol_version, Sizeof(protocol_version));
  stream.Write(protocol_available, Sizeof(protocol_available));
  stream.Write(timestamp, Sizeof(timestamp));
  stream.Write(compact_target, Sizeof(compact_target));
  stream.Write(nonce, Sizeof(nonce));
  stream.WriteAnsiString(block_payload);
  stream.WriteAnsiString(initial_safe_box_hash);
  stream.WriteAnsiString(transactionHash);
  stream.WriteAnsiString(proof_of_work);
end;

function TBlockHeader.ToString: AnsiString;
begin
  Result := Format('Block:%d Timestamp:%d Reward:%d Fee:%d Target:%d PoW:%s',
    [Block, timestamp, reward, Fee,
    compact_target, TBaseType.ToHexaString(proof_of_work)]);
end;

class function TBlockHeader.Empty: TBlockHeader;
begin
  Result.block := 0;
  Result.account_key := TAccountKey.Empty;
  Result.reward := 0;
  Result.fee := 0;
  Result.protocol_version := 0;
  Result.protocol_available := 0;
  Result.timestamp := 0;
  Result.compact_target := 0;
  Result.nonce := 0;
  Result.block_payload := '';
  Result.initial_safe_box_hash := '';
  Result.transactionHash := '';
  Result.proof_of_work := '';
end;

class operator TBlockHeader.Equal(ABlock1, ABlock2: TBlockHeader): Boolean;
begin
    Result := (ABlock1.Block = ABlock2.Block) and
    (ABlock1.account_key = ABlock2.account_key) and
    (ABlock1.reward = ABlock2.reward) and (ABlock1.Fee = ABlock2.Fee) and
    (ABlock1.protocol_version = ABlock2.protocol_version) and
    (ABlock1.protocol_available = ABlock2.protocol_available) and
    (ABlock1.timestamp = ABlock2.timestamp) and
    (ABlock1.compact_target = ABlock2.compact_target) and
    (ABlock1.nonce = ABlock2.nonce) and (ABlock1.block_payload = ABlock2.block_payload)
    and (ABlock1.initial_safe_box_hash = ABlock2.initial_safe_box_hash) and
    (ABlock1.transactionHash = ABlock2.transactionHash) and
    (ABlock1.proof_of_work = ABlock2.proof_of_work);
end;

function TBlockHeader.LoadFromStream(const stream: TStream): Boolean;
begin
  Result := false;
  if stream.Read(block, Sizeof(block)) < Sizeof(block)
  then exit;
  stream.ReadAccountKey(account_key);
  stream.Read(reward, Sizeof(reward));
  stream.Read(fee, Sizeof(fee));
  stream.Read(protocol_version, Sizeof(protocol_version));
  stream.Read(protocol_available, Sizeof(protocol_available));
  stream.Read(timestamp, Sizeof(timestamp));
  stream.Read(compact_target, Sizeof(compact_target));
  stream.Read(nonce, Sizeof(nonce));
  if stream.ReadAnsiString(block_payload) < 0 then exit;
  if stream.ReadAnsiString(initial_safe_box_hash) < 0 then exit;
  if stream.ReadAnsiString(transactionHash) < 0then exit;
  if stream.ReadAnsiString(proof_of_work) < 0 then exit;
  Result := true;
end;

class operator TBlockHeader.NotEqual(ABlock1, ABlock2: TBlockHeader): Boolean;
begin
  Result := not (ABlock1 = ABlock2);
end;

end.
