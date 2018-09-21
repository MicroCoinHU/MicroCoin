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

uses Sysutils, classes, UCrypto, MicroCoin.Account.AccountKey;

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
    procedure SaveToStream(const stream: TStream);
    class function LoadFromStream(const stream: TStream; var ABlockHeader: TBlockHeader): Boolean; static;
    class function Empty : TBlockHeader; static;
  end;


implementation

procedure TBlockHeader.SaveToStream(const stream: TStream);
begin
  stream.Write(block, Sizeof(block));
  TStreamOp.WriteAccountKey(stream, account_key);
  stream.Write(reward, Sizeof(reward));
  stream.Write(fee, Sizeof(fee));
  stream.Write(protocol_version, Sizeof(protocol_version));
  stream.Write(protocol_available, Sizeof(protocol_available));
  stream.Write(timestamp, Sizeof(timestamp));
  stream.Write(compact_target, Sizeof(compact_target));
  stream.Write(nonce, Sizeof(nonce));
  TStreamOp.WriteAnsiString(stream, block_payload);
  TStreamOp.WriteAnsiString(stream, initial_safe_box_hash);
  TStreamOp.WriteAnsiString(stream, transactionHash);
  TStreamOp.WriteAnsiString(stream, proof_of_work);
end;

class function TBlockHeader.Empty: TBlockHeader;
begin
  Result.block := 0;
  Result.account_key := CT_TECDSA_Public_Nul;
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
  TStreamOp.ReadAccountKey(stream, ABlockHeader.account_key);
  stream.Read(ABlockHeader.reward, Sizeof(ABlockHeader.reward));
  stream.Read(ABlockHeader.fee, Sizeof(ABlockHeader.fee));
  stream.Read(ABlockHeader.protocol_version, Sizeof(ABlockHeader.protocol_version));
  stream.Read(ABlockHeader.protocol_available, Sizeof(ABlockHeader.protocol_available));
  stream.Read(ABlockHeader.timestamp, Sizeof(ABlockHeader.timestamp));
  stream.Read(ABlockHeader.compact_target, Sizeof(ABlockHeader.compact_target));
  stream.Read(ABlockHeader.nonce, Sizeof(ABlockHeader.nonce));
  if TStreamOp.ReadAnsiString(stream, ABlockHeader.block_payload) < 0
  then exit;
  if TStreamOp.ReadAnsiString(stream, ABlockHeader.initial_safe_box_hash) < 0
  then exit;
  if TStreamOp.ReadAnsiString(stream, ABlockHeader.transactionHash) < 0
  then exit;
  if TStreamOp.ReadAnsiString(stream, ABlockHeader.proof_of_work) < 0
  then exit;
  Result := true;
end;

end.
