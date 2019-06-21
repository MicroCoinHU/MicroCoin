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
    class function LoadFromStream(const stream: TStream; var ABlockHeader: TBlockHeader): Boolean; static;
    procedure SaveToStream(const stream: TStream);
    class function Empty : TBlockHeader; static;
    function ToString : AnsiString;
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

class function TBlockHeader.LoadFromStream(const stream: TStream; var ABlockHeader: TBlockHeader): Boolean;
begin
  Result := false;
  ABlockHeader := TBlockHeader.Empty;
  if stream.Read(ABlockHeader.block, Sizeof(ABlockHeader.block)) < Sizeof(ABlockHeader.block)
  then exit;
  stream.ReadAccountKey(ABlockHeader.account_key);
  stream.Read(ABlockHeader.reward, Sizeof(ABlockHeader.reward));
  stream.Read(ABlockHeader.fee, Sizeof(ABlockHeader.fee));
  stream.Read(ABlockHeader.protocol_version, Sizeof(ABlockHeader.protocol_version));
  stream.Read(ABlockHeader.protocol_available, Sizeof(ABlockHeader.protocol_available));
  stream.Read(ABlockHeader.timestamp, Sizeof(ABlockHeader.timestamp));
  stream.Read(ABlockHeader.compact_target, Sizeof(ABlockHeader.compact_target));
  stream.Read(ABlockHeader.nonce, Sizeof(ABlockHeader.nonce));
  if stream.ReadAnsiString(ABlockHeader.block_payload) < 0
  then exit;
  if stream.ReadAnsiString(ABlockHeader.initial_safe_box_hash) < 0
  then exit;
  if stream.ReadAnsiString(ABlockHeader.transactionHash) < 0
  then exit;
  if stream.ReadAnsiString(ABlockHeader.proof_of_work) < 0
  then exit;
  Result := true;
end;

end.
