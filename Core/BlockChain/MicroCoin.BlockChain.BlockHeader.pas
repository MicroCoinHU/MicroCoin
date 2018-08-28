unit MicroCoin.BlockChain.BlockHeader;

{
  This unit contains code from PascalCoin:

  Copyright (c) Albert Molina 2016 - 2018 original code from PascalCoin https://pascalcoin.org/

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

}

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
    operations_hash: TRawBytes; // RAW sha256 (32 bytes) of Operations
    proof_of_work: TRawBytes; // RAW Double Sha256
    procedure SaveToStream(const stream: TStream);
    class function LoadFromStream(const stream: TStream; var operationBlock: TBlockHeader): Boolean; static;
  end;

const
  CT_OperationBlock_NUL: TBlockHeader = (block: 0; account_key: (EC_OpenSSL_NID: 0; x: ''; y: ''); reward: 0; fee: 0;
    protocol_version: 0; protocol_available: 0; timestamp: 0; compact_target: 0; nonce: 0; block_payload: '';
    operations_hash: ''; proof_of_work: '');

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
  TStreamOp.WriteAnsiString(stream, operations_hash);
  TStreamOp.WriteAnsiString(stream, proof_of_work);
end;

class function TBlockHeader.LoadFromStream(const stream: TStream; var operationBlock: TBlockHeader): Boolean;
begin
  Result := false;
  operationBlock := CT_OperationBlock_NUL;
  if stream.Read(operationBlock.block, Sizeof(operationBlock.block)) < Sizeof(operationBlock.block) then
    exit;
  TStreamOp.ReadAccountKey(stream, operationBlock.account_key);
  stream.Read(operationBlock.reward, Sizeof(operationBlock.reward));
  stream.Read(operationBlock.fee, Sizeof(operationBlock.fee));
  stream.Read(operationBlock.protocol_version, Sizeof(operationBlock.protocol_version));
  stream.Read(operationBlock.protocol_available, Sizeof(operationBlock.protocol_available));
  stream.Read(operationBlock.timestamp, Sizeof(operationBlock.timestamp));
  stream.Read(operationBlock.compact_target, Sizeof(operationBlock.compact_target));
  stream.Read(operationBlock.nonce, Sizeof(operationBlock.nonce));
  if TStreamOp.ReadAnsiString(stream, operationBlock.block_payload) < 0 then
    exit;
  if TStreamOp.ReadAnsiString(stream, operationBlock.initial_safe_box_hash) < 0 then
    exit;
  if TStreamOp.ReadAnsiString(stream, operationBlock.operations_hash) < 0 then
    exit;
  if TStreamOp.ReadAnsiString(stream, operationBlock.proof_of_work) < 0 then
    exit;
  Result := true;
end;

end.
