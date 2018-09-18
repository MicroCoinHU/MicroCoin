{==============================================================================|
| MicroCoin                                                                    |
| Copyright (c) 2018 MicroCoin Developers                                      |
|==============================================================================|
| Permission is hereby granted, free of charge, to any person obtaining a copy |
| of this software and associated documentation files (the "Software"), to     |
| deal in the Software without restriction, including without limitation the   |
| rights to use, copy, modify, merge, publish, distribute, sublicense, and/or  |
| sell opies of the Software, and to permit persons to whom the Software is    |
| furnished to do so, subject to the following conditions:                     |
|                                                                              |
| The above copyright notice and this permission notice shall be included in   |
| all copies or substantial portions of the Software.                          |
|------------------------------------------------------------------------------|
| THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR   |
| IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,     |
| FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE  |
| AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER       |
| LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING      |
| FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER          |
| DEALINGS IN THE SOFTWARE.                                                    |
|==============================================================================|
| This unit contains portions from PascalCoin                                  |
| Copyright (c) Albert Molina 2016 - 2018                                      |
|                                                                              |
| Distributed under the MIT software license, see the accompanying file        |
| LICENSE or visit http://www.opensource.org/licenses/mit-license.php.         |
|==============================================================================|
| File:       MicroCoin.Account.Storage.pas                                    |
| Created at: 2018-08-23                                                       |
| Purpose:    Account storage managment                                        |
| Todo:                                                                        |
|   - Move TOrderedAccountKeysList to new unit                                 |
|==============================================================================}

unit MicroCoin.Account.Storage;

{$ifdef FPC}
  {$mode delphi}
{$endif}

interface

uses SysUtils, classes, UCrypto, UThread, MicroCoin.Common.Lists,
  MicroCoin.Account.Data, MicroCoin.Account.AccountKey, MicroCoin.BlockChain.BlockHeader,
  UConst, UBaseTypes, ULog, MicroCoin.BlockChain.Protocol;

type

  EInvalidAccountException = class(Exception)

  end;

  TAccountStorage = class;

  TAccountStorageEntry = record
    BlockHeader: TBlockHeader;
    accounts: array [0 .. CT_AccountsPerBlock - 1] of TAccount;
    block_hash: AnsiString; // Calculated on every block change (on create and on accounts updated)
    accumulatedWork: UInt64; // Accumulated work (previous + target) this value can be calculated.
  end;

  TAccountStorageHeader = record
    Protocol: Word;
    startBlock, endBlock, blocksCount: Cardinal;
    AccountStorageHash: TRawBytes;
  end;

  TAccountStorage = class
  private
    FBlockAccountsList: TList;
    FListOfOrderedAccountKeysList: TList;
    FBufferBlocksHash: TRawBytes;
    FOrderedByName: TOrderedRawList;
    FTotalBalance: UInt64;
    FTotalFee: Int64;
    FAccountStorageHash: TRawBytes;
    FLock: TPCCriticalSection; // Thread safe
    FWorkSum: UInt64;
    FCurrentProtocol: Integer;
    procedure AccountKeyListAddAccounts(const AccountKey: TAccountKey; const accounts: array of Cardinal);
    procedure AccountKeyListRemoveAccount(const AccountKey: TAccountKey; const accounts: array of Cardinal);
  public
    function DoUpgradeToProtocol2: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure SetAccount(account_number: Cardinal; const newAccountInfo: TAccountInfo; const newName: TRawBytes;
      newType: Word; newBalance: UInt64; newN_operation: Cardinal);
    function AddNew(const BlockChain: TBlockHeader): TAccountStorageEntry;
    function AccountsCount: Integer;
    function blocksCount: Integer;
    procedure CopyFrom(accounts: TAccountStorage);
    class function CalcBlockHash(const block: TAccountStorageEntry; useProtocol2Method: Boolean): TRawBytes;
    class function BlockAccountToText(const block: TAccountStorageEntry): AnsiString;
    function LoadFromStream(stream: TStream; checkAll: Boolean; var LastReadBlock: TAccountStorageEntry;
      var errors: AnsiString): Boolean;
    class function LoadHeaderFromStream(stream: TStream; var sbHeader: TAccountStorageHeader): Boolean;
    class function SaveHeaderToStream(stream: TStream; Protocol: Word;
      OffsetStartBlock, OffsetEndBlock, CurrentBlocksCount: Cardinal): Boolean;
    class function MustSaved(blocksCount: Cardinal): Boolean;
    procedure SaveEntryToStream(stream: TStream; nBlock: Cardinal);
    procedure SaveToStream(stream: TStream; FromBlock, ToBlock: Cardinal);
    class function CopyChunk(Source, dest: TStream; FromBlock, ToBlock: Cardinal; var errors: AnsiString): Boolean;
    class function ConcatStream(Source1, Source2, dest: TStream; var errors: AnsiString): Boolean;
    class function AccountNameIsValid(const new_name: TRawBytes; var errors: AnsiString): Boolean;

    function IsValidNewBlockHeader(const newOperationBlock: TBlockHeader; validateHash: Boolean;
      var errors: AnsiString): Boolean;
    function GetActualTargetHash(UseProtocolV2: Boolean): TRawBytes;
    function GetActualCompactTargetHash(UseProtocolV2: Boolean): Cardinal;
    function FindAccountByName(aName: AnsiString): Integer;

    procedure Clear;
    function Account(account_number: Cardinal): TAccount;
    function block(block_number: Cardinal): TAccountStorageEntry;
    function CalculateHash: TRawBytes;
    function CalcBlockHashRateInKhs(block_number: Cardinal; Previous_blocks_average: Cardinal): Int64;
    property TotalBalance: UInt64 read FTotalBalance;
    procedure StartThreadSafe;
    procedure EndThreadSave;
    property AccountStorageHash: TRawBytes read FAccountStorageHash;
    property WorkSum: UInt64 read FWorkSum;
    property CurrentProtocol: Integer read FCurrentProtocol;
    function CanUpgradeToProtocol2: Boolean;
    procedure CheckMemory;
    property TotalFee: Int64 read FTotalFee;
    property ListOfOrderedAccountKeysList: TList read FListOfOrderedAccountKeysList;
  end;

  TOrderedAccountKeysList = class
  private
    FAutoAddAll: Boolean;
    FAccountStorage: TAccountStorage;
    FOrderedAccountKeysList: TList; // An ordered list of pointers to quickly find account keys in account list
    function Find(const AccountKey: TAccountKey; var Index: Integer): Boolean;
    function GetAccountList(Index: Integer): TOrderedList;
    function GetAccountKey(Index: Integer): TAccountKey;
  public
    procedure ClearAccounts(RemoveAccountList: Boolean);
    constructor Create(AccountStorage: TAccountStorage; AutoAddAll: Boolean);
    destructor Destroy; override;
    procedure AddAccountKey(const AccountKey: TAccountKey);
    procedure RemoveAccountKey(const AccountKey: TAccountKey);
    procedure AddAccounts(const AccountKey: TAccountKey; const accounts: array of Cardinal);
    procedure RemoveAccounts(const AccountKey: TAccountKey; const accounts: array of Cardinal);
    function IndexOfAccountKey(const AccountKey: TAccountKey): Integer;
    property AccountList[index: Integer]: TOrderedList read GetAccountList;
    property AccountKey[index: Integer]: TAccountKey read GetAccountKey;
    function Count: Integer;
    property AccountStorage: TAccountStorage read FAccountStorage write FAccountStorage;
    procedure Clear;
  end;

const
  CT_AccountChunkIdentificator = 'SafeBoxChunk';
  CT_AccountStorageHeader_NUL: TAccountStorageHeader = (Protocol: 0; startBlock: 0; endBlock: 0; blocksCount: 0;
    AccountStorageHash: '');

implementation

{$DEFINE uselowmem}

const

  CT_BlockAccount_NUL: TAccountStorageEntry = (BlockHeader: (block: 0; account_key: (EC_OpenSSL_NID: 0; x: ''; y: '');
    reward: 0; fee: 0; protocol_version: 0; protocol_available: 0; timestamp: 0; compact_target: 0; nonce: 0;
    block_payload: ''; operations_hash: ''; proof_of_work: '');
    accounts: ((AccountNumber: 0; AccountInfo: (state: as_Unknown; AccountKey: (EC_OpenSSL_NID: 0; x: ''; y: '');
    locked_until_block: 0; price: 0; account_to_pay: 0; new_publicKey: (EC_OpenSSL_NID: 0; x: ''; y: '')); balance: 0;
    updated_block: 0; n_operation: 0; name: ''; account_type: 0; previous_updated_block: 0), (AccountNumber: 0;
    AccountInfo: (state: as_Unknown; AccountKey: (EC_OpenSSL_NID: 0; x: ''; y: ''); locked_until_block: 0; price: 0;
    account_to_pay: 0; new_publicKey: (EC_OpenSSL_NID: 0; x: ''; y: '')); balance: 0; updated_block: 0; n_operation: 0;
    name: ''; account_type: 0; previous_updated_block: 0), (AccountNumber: 0; AccountInfo: (state: as_Unknown;
    AccountKey: (EC_OpenSSL_NID: 0; x: ''; y: ''); locked_until_block: 0; price: 0; account_to_pay: 0;
    new_publicKey: (EC_OpenSSL_NID: 0; x: ''; y: '')); balance: 0; updated_block: 0; n_operation: 0; name: '';
    account_type: 0; previous_updated_block: 0), (AccountNumber: 0; AccountInfo: (state: as_Unknown;
    AccountKey: (EC_OpenSSL_NID: 0; x: ''; y: ''); locked_until_block: 0; price: 0; account_to_pay: 0;
    new_publicKey: (EC_OpenSSL_NID: 0; x: ''; y: '')); balance: 0; updated_block: 0; n_operation: 0; name: '';
    account_type: 0; previous_updated_block: 0), (AccountNumber: 0; AccountInfo: (state: as_Unknown;
    AccountKey: (EC_OpenSSL_NID: 0; x: ''; y: ''); locked_until_block: 0; price: 0; account_to_pay: 0;
    new_publicKey: (EC_OpenSSL_NID: 0; x: ''; y: '')); balance: 0; updated_block: 0; n_operation: 0; name: '';
    account_type: 0; previous_updated_block: 0)); block_hash: ''; accumulatedWork: 0);

procedure ToTMemAccount(const Source: TAccount; var dest: TMemAccount);
{.$IFDEF uselowmem}
var
  raw: TRawBytes;
{.$ENDIF}
begin
{$IFDEF OLDVERSION}
  Source.AccountInfo.ToRawString(raw);
  TBaseType.To256RawBytes(raw, dest.AccountInfo);
  dest.balance := Source.balance;
  dest.updated_block := Source.updated_block;
  dest.n_operation := Source.n_operation;
  dest.name := Source.name;
  dest.account_type := Source.account_type;
  dest.previous_updated_block := Source.previous_updated_block;
  dest.Subaccounts := Source.SubAccounts;
  dest.ExtraData := Source.ExtraData;
{$ELSE}
  dest := Source;
{$ENDIF}
end;

procedure ToTAccount(const Source: TMemAccount; account_number: Cardinal; var dest: TAccount);
{$IFDEF uselowmem}
var
  raw: TRawBytes;
{$ENDIF}
begin
{$IFDEF ouselowmem}
  dest.AccountNumber := account_number;
  TBaseType.ToRawBytes(Source.AccountInfo, raw);
  TAccountInfo.FromRawString(raw, dest.AccountInfo);
  dest.balance := Source.balance;
  dest.updated_block := Source.updated_block;
  dest.n_operation := Source.n_operation;
  dest.name := Source.name;
  dest.account_type := Source.account_type;
  dest.previous_updated_block := Source.previous_updated_block;
  raw := '';
{$ELSE}
  dest := Source;
{$ENDIF}
end;

procedure ToTMemBlockAccount(const Source: TAccountStorageEntry; var dest: TMemBlockAccount);
{$IFDEF uselowmem}
var
  i: Integer;
var
  raw: TRawBytes;
{$ENDIF}
begin
{$IFDEF uselowmem}
  Source.BlockHeader.account_key.ToRawString(raw);
  TBaseType.To256RawBytes(raw, dest.blockchainInfo.account_key);
  dest.blockchainInfo.reward := Source.BlockHeader.reward;
  dest.blockchainInfo.fee := Source.BlockHeader.fee;
  dest.blockchainInfo.protocol_version := Source.BlockHeader.protocol_version;
  dest.blockchainInfo.protocol_available := Source.BlockHeader.protocol_available;
  dest.blockchainInfo.timestamp := Source.BlockHeader.timestamp;
  dest.blockchainInfo.compact_target := Source.BlockHeader.compact_target;
  dest.blockchainInfo.nonce := Source.BlockHeader.nonce;
  TBaseType.To256RawBytes(Source.BlockHeader.block_payload, dest.blockchainInfo.block_payload);
  TBaseType.To32Bytes(Source.BlockHeader.initial_safe_box_hash, dest.blockchainInfo.initial_safe_box_hash);
  TBaseType.To32Bytes(Source.BlockHeader.operations_hash, dest.blockchainInfo.operations_hash);
  TBaseType.To32Bytes(Source.BlockHeader.proof_of_work, dest.blockchainInfo.proof_of_work);

  for i := low(Source.accounts) to high(Source.accounts) do
  begin
    ToTMemAccount(Source.accounts[i], dest.accounts[i]);
  end;
  TBaseType.To32Bytes(Source.block_hash, dest.block_hash);
  dest.accumulatedWork := Source.accumulatedWork;
{$ELSE}
  dest := Source;
{$ENDIF}
end;

procedure ToTBlockAccount(const Source: TMemBlockAccount; block_number: Cardinal; var dest: TAccountStorageEntry);
{$IFDEF uselowmem}
var
  i: Integer;
  raw: TRawBytes;
{$ENDIF}
begin
{$IFDEF uselowmem}
  dest.BlockHeader.block := block_number;
  TBaseType.ToRawBytes(Source.blockchainInfo.account_key, raw);
  TAccountKey.FromRawString(raw, dest.BlockHeader.account_key);
  dest.BlockHeader.reward := Source.blockchainInfo.reward;
  dest.BlockHeader.fee := Source.blockchainInfo.fee;
  dest.BlockHeader.protocol_version := Source.blockchainInfo.protocol_version;
  dest.BlockHeader.protocol_available := Source.blockchainInfo.protocol_available;
  dest.BlockHeader.timestamp := Source.blockchainInfo.timestamp;
  dest.BlockHeader.compact_target := Source.blockchainInfo.compact_target;
  dest.BlockHeader.nonce := Source.blockchainInfo.nonce;
  TBaseType.ToRawBytes(Source.blockchainInfo.block_payload, dest.BlockHeader.block_payload);
  TBaseType.ToRawBytes(Source.blockchainInfo.initial_safe_box_hash, dest.BlockHeader.initial_safe_box_hash);
  TBaseType.ToRawBytes(Source.blockchainInfo.operations_hash, dest.BlockHeader.operations_hash);
  TBaseType.ToRawBytes(Source.blockchainInfo.proof_of_work, dest.BlockHeader.proof_of_work);

  for i := low(Source.accounts) to high(Source.accounts) do
  begin
    ToTAccount(Source.accounts[i], (block_number * CT_AccountsPerBlock) + i, dest.accounts[i]);
  end;
  TBaseType.ToRawBytes(Source.block_hash, dest.block_hash);
  dest.accumulatedWork := Source.accumulatedWork;
{$ELSE}
  dest := Source;
{$ENDIF}
end;

function TAccountStorage.Account(account_number: Cardinal): TAccount;
var
  b: Cardinal;
begin
  b := account_number div CT_AccountsPerBlock;
  if (b < 0) or (b >= FBlockAccountsList.Count) then
    raise EInvalidAccountException.Create('Invalid account: ' + inttostr(account_number));
  ToTAccount(PBlockAccount(FBlockAccountsList.Items[b])^.accounts[account_number mod CT_AccountsPerBlock],
    account_number, Result);
end;

function TAccountStorage.AddNew(const BlockChain: TBlockHeader): TAccountStorageEntry;
var
  i, base_addr: Integer;
  P: PBlockAccount;
  accs: array of Cardinal;
begin
  Result := CT_BlockAccount_NUL;
  Result.BlockHeader := BlockChain;
  if BlockChain.block <> blocksCount then
    raise Exception.Create('ERROR DEV 20170427-2');
  if BlockChain.fee <> FTotalFee then
    raise Exception.Create('ERROR DEV 20170427-3');
  base_addr := blocksCount * CT_AccountsPerBlock;
  SetLength(accs, length(Result.accounts));
  for i := low(Result.accounts) to high(Result.accounts) do
  begin
    Result.accounts[i] := TAccount.Empty;
    Result.accounts[i].AccountNumber := base_addr + i;
    Result.accounts[i].AccountInfo.state := as_Normal;
    Result.accounts[i].AccountInfo.AccountKey := BlockChain.account_key;
    Result.accounts[i].updated_block := blocksCount;
    Result.accounts[i].n_operation := 0;
    if i = low(Result.accounts) then
    begin
      // Only first account wins the reward + fee
      Result.accounts[i].balance := BlockChain.reward + BlockChain.fee;
    end
    else
    begin
    end;
    accs[i] := base_addr + i;
  end;
  inc(FWorkSum, Result.BlockHeader.compact_target);
  Result.accumulatedWork := FWorkSum;
  // Calc block hash
  Result.block_hash := CalcBlockHash(Result, FCurrentProtocol >= CT_PROTOCOL_2);

  New(P);
  ToTMemBlockAccount(Result, P^);

  FBlockAccountsList.Add(P);
  FBufferBlocksHash := FBufferBlocksHash + Result.block_hash;
  inc(FTotalBalance, BlockChain.reward + BlockChain.fee);
  Dec(FTotalFee, BlockChain.fee);
  AccountKeyListAddAccounts(BlockChain.account_key, accs);
  // Calculating new value of safebox
  FAccountStorageHash := CalculateHash;
end;

procedure TAccountStorage.AccountKeyListAddAccounts(const AccountKey: TAccountKey; const accounts: array of Cardinal);
var
  i: Integer;
begin
  for i := 0 to FListOfOrderedAccountKeysList.Count - 1 do
  begin
    TOrderedAccountKeysList(FListOfOrderedAccountKeysList[i]).AddAccounts(AccountKey, accounts);
  end;
end;

procedure TAccountStorage.AccountKeyListRemoveAccount(const AccountKey: TAccountKey; const accounts: array of Cardinal);
var
  i: Integer;
begin
  for i := 0 to FListOfOrderedAccountKeysList.Count - 1 do
  begin
    TOrderedAccountKeysList(FListOfOrderedAccountKeysList[i]).RemoveAccounts(AccountKey, accounts);
  end;
end;

function TAccountStorage.AccountsCount: Integer;
begin
  Result := blocksCount * CT_AccountsPerBlock;
end;

function TAccountStorage.block(block_number: Cardinal): TAccountStorageEntry;
begin
  if (block_number < 0) or (block_number >= FBlockAccountsList.Count) then
    raise Exception.Create('Invalid block number: ' + inttostr(block_number));
  ToTBlockAccount(PBlockAccount(FBlockAccountsList.Items[block_number])^, block_number, Result);
end;

class function TAccountStorage.BlockAccountToText(const block: TAccountStorageEntry): AnsiString;
begin
  Result := Format('Block:%d Timestamp:%d BlockHash:%s', [block.BlockHeader.block, block.BlockHeader.timestamp,
    TCrypto.ToHexaString(block.block_hash)]);
end;

function TAccountStorage.blocksCount: Integer;
begin
  Result := FBlockAccountsList.Count;
end;

class function TAccountStorage.CalcBlockHash(const block: TAccountStorageEntry; useProtocol2Method: Boolean): TRawBytes;
// Protocol v2 update:
// In order to store values to generate PoW and allow Safebox checkpointing, we
// store info about TOperationBlock on each row and use it to obtain blockchash
var
  raw: TRawBytes;
  ms: TMemoryStream;
  i: Integer;
begin
  ms := TMemoryStream.Create;
//  ms.SetSize(2048);
  try
    if (not useProtocol2Method) then
    begin
      // PROTOCOL 1 BlockHash calculation
      ms.Write(block.BlockHeader.block, 4); // Little endian
      for i := low(block.accounts) to high(block.accounts) do
      begin
        ms.Write(block.accounts[i].AccountNumber, 4); // Little endian
        raw := block.accounts[i].AccountInfo.ToRawString;
        ms.WriteBuffer(raw[1], length(raw)); // Raw bytes
        ms.Write(block.accounts[i].balance, Sizeof(UInt64)); // Little endian
        ms.Write(block.accounts[i].updated_block, 4); // Little endian
        ms.Write(block.accounts[i].n_operation, 4); // Little endian
      end;
      ms.Write(block.BlockHeader.timestamp, 4); // Little endian
    end
    else
    begin
      // PROTOCOL 2 BlockHash calculation
      ms.SetSize(1024*4);
      block.BlockHeader.SaveToStream(ms);
      for i := low(block.accounts) to high(block.accounts) do
      begin
        ms.Write(block.accounts[i].AccountNumber, 4); // Little endian
        raw := block.accounts[i].AccountInfo.ToRawString;
        ms.WriteBuffer(raw[1], length(raw)); // Raw bytes
        ms.Write(block.accounts[i].balance, Sizeof(UInt64)); // Little endian
        ms.Write(block.accounts[i].updated_block, 4); // Little endian
        ms.Write(block.accounts[i].n_operation, 4); // Little endian
        // Use new Protocol 2 fields
        if length(block.accounts[i].name) > 0 then
        begin
          ms.WriteBuffer(block.accounts[i].name[1], length(block.accounts[i].name));
        end;
        ms.Write(block.accounts[i].account_type, 2);
      end;
      ms.Write(block.accumulatedWork, Sizeof(block.accumulatedWork));
    end;
    Result := TCrypto.DoSha256(ms.Memory, ms.Position)
  finally
    ms.Free;
  end;
end;

function TAccountStorage.CalcBlockHashRateInKhs(block_number: Cardinal; Previous_blocks_average: Cardinal): Int64;
var
  c, t: Cardinal;
  t_sum: Extended;
  bn, bn_sum: TBigNum;
begin
  FLock.Acquire;
  try
    bn_sum := TBigNum.Create;
    try
      if (block_number = 0) then
      begin
        Result := 1;
        exit;
      end;
      if (block_number < 0) or (block_number >= FBlockAccountsList.Count) then
        raise Exception.Create('Invalid block number: ' + inttostr(block_number));
      if (Previous_blocks_average <= 0) then
        raise Exception.Create('Dev error 20161016-1');
      if (Previous_blocks_average > block_number) then
        Previous_blocks_average := block_number;
      //
      c := (block_number - Previous_blocks_average) + 1;
      t_sum := 0;
      while (c <= block_number) do
      begin
        bn := TBigNum.TargetToHashRate(PBlockAccount(FBlockAccountsList.Items[c])^.blockchainInfo.compact_target);
        try
          bn_sum.Add(bn);
        finally
          bn.Free;
        end;
        t_sum := t_sum + (PBlockAccount(FBlockAccountsList.Items[c])^.blockchainInfo.timestamp -
          PBlockAccount(FBlockAccountsList.Items[c - 1])^.blockchainInfo.timestamp);
        inc(c);
      end;
      bn_sum.Divide(Previous_blocks_average); // Obtain target average
      t_sum := t_sum / Previous_blocks_average; // time average
      t := Round(t_sum);
      if (t <> 0) then
      begin
        bn_sum.Divide(t);
      end;
      Result := bn_sum.Divide(1024).value; // Value in Kh/s
    finally
      bn_sum.Free;
    end;
  finally
    FLock.Release;
  end;
end;

function TAccountStorage.CalculateHash: TRawBytes;
begin
  if (FBufferBlocksHash = '') then
    Result := TCrypto.DoSha256(CT_Genesis_Magic_String_For_Old_Block_Hash)
  else
    Result := TCrypto.DoSha256(FBufferBlocksHash);
end;

function TAccountStorage.CanUpgradeToProtocol2: Boolean;
begin
  Result := (FCurrentProtocol < CT_PROTOCOL_2) and (blocksCount >= CT_Protocol_Upgrade_v2_MinBlock);
end;

procedure TAccountStorage.CheckMemory;
{ Note about Free Pascal compiler
  When compiling using Delphi it's memory manager more is efficient and does not increase, but
  When compiling using Free Pascal Compiler, is a good solution to "force" generate a new SafeBox
  in order to free memory not used. Tested with FPC 3.0 }
{$IFDEF FPC}
var
  sb: TAccountStorage;
  tc: Cardinal;
{$ENDIF}
begin
{$IFDEF FPC}
  StartThreadSafe;
  try
    tc := GetTickCount;
    sb := TAccountStorage.Create;
    try
      sb.CopyFrom(Self);
      Self.Clear;
      Self.CopyFrom(sb);
    finally
      sb.Free;
    end;
    tc := GetTickCount - tc;
    TLog.NewLog(ltDebug, Classname, 'Checked memory ' + inttostr(tc) + ' miliseonds');
  finally
    EndThreadSave;
  end;
{$ENDIF}
end;

procedure TAccountStorage.Clear;
var
  i: Integer;
  P: PBlockAccount;
begin
  StartThreadSafe;
  try
    for i := 0 to FBlockAccountsList.Count - 1 do
    begin
      P := FBlockAccountsList.Items[i];
//      setLength(TMemBlockAccount(p^).accounts[0].AccountInfo,0);
      TMemBlockAccount(p^).accounts[0] := Default(TMemAccount);
      TMemBlockAccount(p^).accounts[1] := Default(TMemAccount);
      TMemBlockAccount(p^).accounts[2] := Default(TMemAccount);
      TMemBlockAccount(p^).accounts[3] := Default(TMemAccount);
      TMemBlockAccount(p^).accounts[4] := Default(TMemAccount);
      TMemBlockAccount(P^).blockchainInfo := Default(TMemOperationBlock);
      TMemBlockAccount(P^) := Default(TMemBlockAccount);
      Dispose(P);
    end;
    FOrderedByName.Clear;
    FBlockAccountsList.Clear;
    for i := 0 to FListOfOrderedAccountKeysList.Count - 1 do
    begin
      TOrderedAccountKeysList(FListOfOrderedAccountKeysList[i]).ClearAccounts(false);
    end;
    FBufferBlocksHash := '';
    FTotalBalance := 0;
    FTotalFee := 0;
    FAccountStorageHash := CalculateHash;
    FWorkSum := 0;
    FCurrentProtocol := CT_PROTOCOL_1;
  finally
    EndThreadSave;
  end;
end;

procedure TAccountStorage.CopyFrom(accounts: TAccountStorage);
var
  i, j: Cardinal;
  P: PBlockAccount;
  BA: TAccountStorageEntry;
begin
  StartThreadSafe;
  try
    accounts.StartThreadSafe;
    try
      if accounts = Self then
        exit;
      Clear;
      if accounts.blocksCount > 0 then
      begin
        FBlockAccountsList.Capacity := accounts.blocksCount;
        for i := 0 to accounts.blocksCount - 1 do
        begin
          BA := accounts.block(i);
          New(P);
          ToTMemBlockAccount(BA, P^);
          FBlockAccountsList.Add(P);
          for j := low(BA.accounts) to high(BA.accounts) do
          begin
            if (BA.accounts[j].name <> '') then
              FOrderedByName.Add(BA.accounts[j].name, BA.accounts[j].AccountNumber);
            AccountKeyListAddAccounts(BA.accounts[j].AccountInfo.AccountKey, [BA.accounts[j].AccountNumber]);
          end;
        end;
      end;
      FTotalBalance := accounts.TotalBalance;
      FTotalFee := accounts.FTotalFee;
      FBufferBlocksHash := accounts.FBufferBlocksHash;
      FAccountStorageHash := accounts.FAccountStorageHash;
      FWorkSum := accounts.FWorkSum;
      FCurrentProtocol := accounts.FCurrentProtocol;
    finally
      accounts.EndThreadSave;
    end;
  finally
    EndThreadSave;
  end;
end;

constructor TAccountStorage.Create;
begin
  FLock := TPCCriticalSection.Create('TAccountStorage_Lock');
  FBlockAccountsList := TList.Create;
  FListOfOrderedAccountKeysList := TList.Create;
  FCurrentProtocol := CT_PROTOCOL_1;
  FOrderedByName := TOrderedRawList.Create;
  Clear;
end;

destructor TAccountStorage.Destroy;
var
  i: Integer;
  p: PBlockAccount;
begin
  Clear;
  for i := 0 to FListOfOrderedAccountKeysList.Count - 1 do
  begin
    TOrderedAccountKeysList(FListOfOrderedAccountKeysList[i]).AccountStorage := nil;
  end;
  FreeAndNil(FBlockAccountsList);
  FreeAndNil(FListOfOrderedAccountKeysList);
  FreeAndNil(FLock);
  FreeAndNil(FOrderedByName);
  inherited;
end;

function TAccountStorage.DoUpgradeToProtocol2: Boolean;
var
  block_number: Cardinal;
  aux: TRawBytes;
begin
  // Upgrade process to protocol 2
  Result := false;
  if not CanUpgradeToProtocol2 then
    exit;
  // Recalc all BlockAccounts block_hash value
  aux := CalculateHash;
  TLog.NewLog(ltInfo, Classname, 'Start Upgrade to protocol 2 - Old Hash:' + TCrypto.ToHexaString(FAccountStorageHash) +
    ' calculated: ' + TCrypto.ToHexaString(aux) + ' Blocks: ' + inttostr(blocksCount));
  FBufferBlocksHash := '';
  for block_number := 0 to blocksCount - 1 do
  begin
{$IFDEF uselowmem}
    TBaseType.To32Bytes(CalcBlockHash(block(block_number), true), PBlockAccount(FBlockAccountsList.Items[block_number])
      ^.block_hash);
    FBufferBlocksHash := FBufferBlocksHash + TBaseType.ToRawBytes(PBlockAccount(FBlockAccountsList.Items[block_number])
      ^.block_hash);
{$ELSE}
    PBlockAccount(FBlockAccountsList.Items[block_number])^.block_hash := CalcBlockHash(block(block_number), true);
    FBufferBlocksHash := FBufferBlocksHash + PBlockAccount(FBlockAccountsList.Items[block_number])^.block_hash;
{$ENDIF}
  end;
  FAccountStorageHash := CalculateHash;
  FCurrentProtocol := CT_PROTOCOL_2;
  Result := true;
  TLog.NewLog(ltInfo, Classname, 'End Upgraded to protocol 2 - New hash:' + TCrypto.ToHexaString(FAccountStorageHash));
end;

procedure TAccountStorage.EndThreadSave;
begin
  FLock.Release;
end;

function TAccountStorage.LoadFromStream(stream: TStream; checkAll: Boolean; var LastReadBlock: TAccountStorageEntry;
  var errors: AnsiString): Boolean;
var
  iblock, iacc: Cardinal;
  s: AnsiString;
  block: TAccountStorageEntry;
  P: PBlockAccount;
  i, j: Integer;
  savedSBH: TRawBytes;
  nPos, posOffsetZone: Int64;
  offsets: array of Cardinal;
  sbHeader: TAccountStorageHeader;
  xIsExtendedAccount: boolean;
  b: byte;
begin
  StartThreadSafe;
  try
    Clear;
    Result := false;
    try
      if not LoadHeaderFromStream(stream, sbHeader) then
      begin
        errors := 'Invalid stream. Invalid header/version';
        exit;
      end;
      errors := 'Invalid version or corrupted stream';
      case sbHeader.Protocol of
        CT_PROTOCOL_1:
          FCurrentProtocol := 1;
        CT_PROTOCOL_2:
          FCurrentProtocol := 2;
      else
        exit;
      end;
      if (sbHeader.blocksCount = 0) or (sbHeader.startBlock <> 0) or (sbHeader.endBlock <> (sbHeader.blocksCount - 1))
      then
      begin
        errors := Format('Stream contains blocks from %d to %d (of %d blocks). Not valid',
          [sbHeader.startBlock, sbHeader.endBlock, sbHeader.blocksCount]);
        exit;
      end;
      // Offset zone
      posOffsetZone := stream.Position;
      if checkAll then
      begin
        SetLength(offsets, sbHeader.blocksCount + 1); // Last offset = End of blocks
        stream.Read(offsets[0], 4 * (sbHeader.blocksCount + 1));
      end
      else
      begin
        nPos := stream.Position + ((sbHeader.blocksCount + 1) * 4);
        if stream.Size < nPos then
          exit;
        stream.Position := nPos;
      end;
      // Build 1.3.0 to increase reading speed:
      FBlockAccountsList.Capacity := sbHeader.blocksCount;
      SetLength(FBufferBlocksHash, sbHeader.blocksCount * 32); // Initialize for high speed reading
      errors := 'Corrupted stream';
      for iblock := 0 to sbHeader.blocksCount - 1 do
      begin
        errors := 'Corrupted stream reading block blockchain ' + inttostr(iblock + 1) + '/' +
          inttostr(sbHeader.blocksCount);
        if (checkAll) then
        begin
          if (offsets[iblock] <> stream.Position - posOffsetZone) then
          begin
            errors := errors + Format(' - offset[%d]:%d <> %d Position:%d offset:%d',
              [iblock, offsets[iblock], stream.Position - posOffsetZone, stream.Position, posOffsetZone]);
            exit;
          end;
        end;

        block := CT_BlockAccount_NUL;
        if not TBlockHeader.LoadFromStream(stream, block.BlockHeader) then
          exit;
        if block.BlockHeader.block <> iblock then
          exit;
        for iacc := low(block.accounts) to high(block.accounts) do
        begin
          errors := 'Corrupted stream reading account ' + inttostr(iacc + 1) + '/' + inttostr(length(block.accounts)) +
            ' of block ' + inttostr(iblock + 1) + '/' + inttostr(sbHeader.blocksCount);
          if stream.Read(block.accounts[iacc].AccountNumber, 4) < 4 then
            exit;
          if TStreamOp.ReadAnsiString(stream, s) < 0
          then exit;
          if s=''
          then xIsExtendedAccount := true
          else xIsExtendedAccount := false;
          if xIsExtendedAccount
          then if TStreamOp.ReadAnsiString(stream, s) < 0
               then exit;
          block.accounts[iacc].AccountInfo := TAccountInfo.FromRawString(s);
          if stream.Read(block.accounts[iacc].balance, Sizeof(UInt64)) < Sizeof(UInt64) then
            exit;
          if stream.Read(block.accounts[iacc].updated_block, 4) < 4 then
            exit;
          if stream.Read(block.accounts[iacc].n_operation, 4) < 4 then
            exit;
          if FCurrentProtocol >= CT_PROTOCOL_2 then
          begin
            if TStreamOp.ReadAnsiString(stream, block.accounts[iacc].name) < 0 then
              exit;
            if stream.Read(block.accounts[iacc].account_type, 2) < 2 then
              exit;
          end;
          //
          if stream.Read(block.accounts[iacc].previous_updated_block, 4) < 4 then
            exit;
          {$IFDEF EXTENDEDACCOUNT}
          if xIsExtendedAccount
          then begin
            if stream.Read(b,1) < 1
            then exit;
            SetLength(block.accounts[iacc].SubAccounts, b);
            if b > 0 then begin
              for I := 0 to b - 1
              do begin
                TStreamOp.ReadAccountKey(stream, block.accounts[iacc].SubAccounts[i].AccountKey);
                stream.Read(block.accounts[iacc].SubAccounts[i].DailyLimit, SizeOf(Int64));
                stream.Read(block.accounts[iacc].SubAccounts[i].TotalLimit, SizeOf(Int64));
                stream.Read(block.accounts[iacc].SubAccounts[i].Balance, SizeOf(UInt64));
                stream.Read(block.accounts[iacc].SubAccounts[i].Currency, SizeOf(Cardinal));
              end;
            end;
            Stream.Read(b, SizeOf(block.accounts[iacc].ExtraData.DataType));
            block.accounts[iacc].ExtraData.DataType := b;
            Stream.Read(block.accounts[iacc].ExtraData.ExtraType, SizeOf(block.accounts[iacc].ExtraData.ExtraType));
            TStreamOp.ReadAnsiString(stream, block.accounts[iacc].ExtraData.Data);
          end;
          {$ENDIF}
          // check valid
          if (block.accounts[iacc].name <> '') then
          begin
            if FOrderedByName.IndexOf(block.accounts[iacc].name) >= 0 then
            begin
              errors := errors + ' Duplicate name "' + block.accounts[iacc].name + '"';
              exit;
            end;
            if not TAccountStorage.AccountNameIsValid(block.accounts[iacc].name, s) then
            begin
              errors := errors + ' > Invalid name "' + block.accounts[iacc].name + '": ' + s;
              exit;
            end;
            FOrderedByName.Add(block.accounts[iacc].name, block.accounts[iacc].AccountNumber);
          end;
          if checkAll then
          begin
            if not block.accounts[iacc].AccountInfo.IsValid(s) then
            begin
              errors := errors + ' > ' + s;
              exit;
            end;
          end;
          inc(FTotalBalance, block.accounts[iacc].balance);
        end;
        errors := 'Corrupted stream reading block ' + inttostr(iblock + 1) + '/' + inttostr(sbHeader.blocksCount);
        if TStreamOp.ReadAnsiString(stream, block.block_hash) < 0 then
          exit;
        if stream.Read(block.accumulatedWork, Sizeof(block.accumulatedWork)) < Sizeof(block.accumulatedWork) then
          exit;
        if checkAll then
        begin
          // Check is valid:
          // STEP 1: Validate the block
          if not IsValidNewBlockHeader(block.BlockHeader, false, s) then
          begin
            errors := errors + ' > ' + s;
            exit;
          end;
          // STEP 2: Check if valid block hash
          if CalcBlockHash(block, FCurrentProtocol >= CT_PROTOCOL_2) <> block.block_hash then
          begin
            errors := errors + ' > Invalid block hash ' + inttostr(iblock + 1) + '/' + inttostr(sbHeader.blocksCount);
            exit;
          end;
          // STEP 3: Check accumulatedWork
          if (iblock > 0) then
          begin
            if (Self.block(iblock - 1).accumulatedWork) + block.BlockHeader.compact_target <> block.accumulatedWork then
            begin
              errors := errors + ' > Invalid accumulatedWork';
              exit;
            end;
          end;
        end;
        // Add
        New(P);
        ToTMemBlockAccount(block, P^);
        FBlockAccountsList.Add(P);
        for j := low(block.accounts) to high(block.accounts) do
        begin
          AccountKeyListAddAccounts(block.accounts[j].AccountInfo.AccountKey, [block.accounts[j].AccountNumber]);
        end;
        // BufferBlocksHash fill with data
        j := (length(P^.block_hash) * (iblock));
        for i := 1 to length(P^.block_hash) do
        begin
{$IFDEF FPC}
          FBufferBlocksHash[i + j] := AnsiChar(P^.block_hash[i - (low(FBufferBlocksHash) - low(P^.block_hash))]);
{$ELSE}
          FBufferBlocksHash[i + j] := AnsiChar(P^.block_hash[i - {$IFDEF uselowmem}1{$ELSE}0{$ENDIF}]);
{$ENDIF}
        end;
        LastReadBlock := block;
        inc(FWorkSum, block.BlockHeader.compact_target);
      end;
      if checkAll then
      begin
        if (offsets[sbHeader.blocksCount] <> 0) and (offsets[sbHeader.blocksCount] <> stream.Position - posOffsetZone)
        then
        begin
          errors := errors + Format(' - Final offset[%d]=%d <> Eof Position:%d offset:%d',
            [sbHeader.blocksCount, offsets[sbHeader.blocksCount], stream.Position - posOffsetZone, posOffsetZone]);
          exit;
        end;
      end;
      // Finally load SafeBoxHash
      if TStreamOp.ReadAnsiString(stream, savedSBH) < 0 then
      begin
        errors := 'No Hash value';
        exit;
      end;
      // Check worksum value
      if sbHeader.blocksCount > 0 then
      begin
        if (FWorkSum <> Self.block(sbHeader.blocksCount - 1).accumulatedWork) then
        begin
          errors := 'Invalid WorkSum value';
          exit;
        end;
      end;
      // Calculating safe box hash
      FAccountStorageHash := CalculateHash;
      // Checking saved SafeBoxHash
      if FAccountStorageHash <> savedSBH then
      begin
        errors := 'Invalid Hash value in stream ' + TCrypto.ToHexaString(FAccountStorageHash) + '<>' +
          TCrypto.ToHexaString(savedSBH) + ' Last block:' + inttostr(LastReadBlock.BlockHeader.block);
        exit;
      end;
      Result := true;
    finally
      if not Result then
        Clear
      else
        errors := '';
    end;
  finally
    EndThreadSave;
  end;
end;

class function TAccountStorage.LoadHeaderFromStream(stream: TStream; var sbHeader: TAccountStorageHeader): Boolean;
// This function reads SafeBox stream info and sets position at offset start zone if valid, otherwise sets position to actual position
var
  w: Word;
  s: AnsiString;
  AccountStorageVersion: Word;
  offsetPos, initialPos: Int64;
  endBlocks: Cardinal;
begin
  Result := false;
  if (stream.Size = 0) then
  begin
    Result := true;
    exit;
  end;
  sbHeader := CT_AccountStorageHeader_NUL;
  initialPos := stream.Position;
  try
    TStreamOp.ReadAnsiString(stream, s);
    if (s <> CT_MagicIdentificator) then
      exit;
    if stream.Size < 8 then
      exit;
    stream.Read(w, Sizeof(w));
    if not(w in [1, 2]) then
      exit;
    sbHeader.Protocol := w;
    stream.Read(AccountStorageVersion, 2);
    if AccountStorageVersion <> CT_AccountStorageVersion then
      exit;
    stream.Read(sbHeader.blocksCount, 4);
    stream.Read(sbHeader.startBlock, 4);
    stream.Read(sbHeader.endBlock, 4);
    if (sbHeader.blocksCount <= 0) or (sbHeader.blocksCount > (CT_NewLineSecondsAvg * 2000000)) then
      exit; // Protection for corrupted data...
    offsetPos := stream.Position;
    // Go to read SafeBoxHash
    if (stream.Size < offsetPos + (((sbHeader.endBlock - sbHeader.startBlock) + 2) * 4)) then
      exit;
    stream.Position := offsetPos + (((sbHeader.endBlock - sbHeader.startBlock) + 1) * 4);
    stream.Read(endBlocks, 4);
    // Go to end
    if (stream.Size < offsetPos + (endBlocks)) then
      exit;
    stream.Position := offsetPos + endBlocks;
    if TStreamOp.ReadAnsiString(stream, sbHeader.AccountStorageHash) < 0 then
      exit;
    // Back
    stream.Position := offsetPos;
    Result := true;
  finally
    if not Result then
      stream.Position := initialPos;
  end;
end;

class function TAccountStorage.SaveHeaderToStream(stream: TStream; Protocol: Word;
  OffsetStartBlock, OffsetEndBlock, CurrentBlocksCount: Cardinal): Boolean;
var
  c: Cardinal;
begin
  Result := false;
  // Header zone
  TStreamOp.WriteAnsiString(stream, CT_MagicIdentificator);
  stream.Write(Protocol, Sizeof(Protocol));
  stream.Write(CT_AccountStorageVersion, Sizeof(CT_AccountStorageVersion));
  c := CurrentBlocksCount;
  stream.Write(c, Sizeof(c)); // Save Total blocks of the safebox
  c := OffsetStartBlock;
  stream.Write(c, Sizeof(c)); // Save first block saved
  c := OffsetEndBlock;
  stream.Write(c, Sizeof(c)); // Save last block saved
  Result := true;
end;

class function TAccountStorage.MustSaved(blocksCount: Cardinal): Boolean;
begin
  Result := (blocksCount mod CT_BankToDiskEveryNBlocks) = 0;
end;

procedure TAccountStorage.SaveEntryToStream(stream: TStream; nBlock: Cardinal);
var
  b: TAccountStorageEntry;
  iacc: Integer;
  ws: UInt64;
  bs: byte;
  was: word;
  i: integer;
begin
  ws := FWorkSum;
  b := block(nBlock);
  b.BlockHeader.SaveToStream(stream);
  for iacc := low(b.accounts) to high(b.accounts) do
  begin
    stream.Write(b.accounts[iacc].AccountNumber, Sizeof(b.accounts[iacc].AccountNumber));
    {$ifdef EXTENDEDACCOUNT}
      was:=0;
      stream.Write(was, 2);
    {$ENDIF}
    TStreamOp.WriteAnsiString(stream, b.accounts[iacc].AccountInfo.ToRawString);
    stream.Write(b.accounts[iacc].balance, Sizeof(b.accounts[iacc].balance));
    stream.Write(b.accounts[iacc].updated_block, Sizeof(b.accounts[iacc].updated_block));
    stream.Write(b.accounts[iacc].n_operation, Sizeof(b.accounts[iacc].n_operation));
    if FCurrentProtocol >= CT_PROTOCOL_2 then
    begin
      TStreamOp.WriteAnsiString(stream, b.accounts[iacc].name);
      stream.Write(b.accounts[iacc].account_type, Sizeof(b.accounts[iacc].account_type));
    end;
    stream.Write(b.accounts[iacc].previous_updated_block, Sizeof(b.accounts[iacc].previous_updated_block));
    {$ifdef EXTENDEDACCOUNT}
      bs := Byte(Length(b.accounts[iacc].SubAccounts));
      stream.Write(bs, 1);
      for i := 0 to High(b.accounts[iacc].SubAccounts)
      do begin
        TStreamOp.WriteAccountKey(stream, b.accounts[iacc].SubAccounts[i].AccountKey);
        stream.Write(b.accounts[iacc].SubAccounts[i].DailyLimit, sizeof(b.accounts[iacc].SubAccounts[i].DailyLimit));
        stream.Write(b.accounts[iacc].SubAccounts[i].TotalLimit, sizeof(b.accounts[iacc].SubAccounts[i].TotalLimit));
        stream.Write(b.accounts[iacc].SubAccounts[i].Balance, sizeof(b.accounts[iacc].SubAccounts[i].Balance));
        stream.Write(b.accounts[iacc].SubAccounts[i].Currency, sizeof(b.accounts[iacc].SubAccounts[i].Currency));
      end;
      stream.Write(b.accounts[iacc].ExtraData.DataType, sizeof(b.accounts[iacc].ExtraData.DataType));
      stream.Write(b.accounts[iacc].ExtraData.ExtraType, sizeof(b.accounts[iacc].ExtraData.ExtraType));
      TStreamOp.WriteAnsiString(stream, b.accounts[iacc].ExtraData.Data);
    {$endif}
  end;
  TStreamOp.WriteAnsiString(stream, b.block_hash);
  stream.Write(b.accumulatedWork, Sizeof(b.accumulatedWork));
end;

procedure TAccountStorage.SaveToStream(stream: TStream; FromBlock, ToBlock: Cardinal);
var
  totalBlocks, iblock: Cardinal;
  b: TAccountStorageEntry;
  posOffsetZone, posFinal: Int64;
  offsets: {$ifdef USE_GENERICS}TArray<Cardinal>{$else}TCardinalArray{$endif};
  raw: TRawBytes;
begin
  if (FromBlock > ToBlock) or (ToBlock >= blocksCount) then
    raise Exception.Create(Format('Cannot save account storage from %d to %d (currently %d blocks)',
      [FromBlock, ToBlock, blocksCount]));
  StartThreadSafe;
  try
    // Header zone
    SaveHeaderToStream(stream, FCurrentProtocol, FromBlock, ToBlock, blocksCount);
    totalBlocks := ToBlock - FromBlock + 1;
    // Offsets zone
    posOffsetZone := stream.Position;
    SetLength(raw, (totalBlocks + 1) * 4); // Last position = end
    FillChar(raw[1], length(raw), 0);
    stream.WriteBuffer(raw[1], length(raw));
    SetLength(offsets, totalBlocks + 1); // c = total blocks  - Last position = offset to end
    // Blocks zone
    for iblock := FromBlock to ToBlock do
    begin
      offsets[iblock] := stream.Position - posOffsetZone;
      SaveEntryToStream(stream, iblock);
    end;
    offsets[high(offsets)] := stream.Position - posOffsetZone;
    // Save offsets zone with valid values
    posFinal := stream.Position;
    stream.Position := posOffsetZone;
    for iblock := FromBlock to ToBlock + 1 do
    begin
      stream.Write(offsets[iblock], Sizeof(offsets[iblock]));
    end;
    stream.Position := posFinal;
    // Final zone: Save safeboxhash for next block
    if (ToBlock + 1 < blocksCount) then
    begin
      b := block(ToBlock);
      TStreamOp.WriteAnsiString(stream, b.BlockHeader.initial_safe_box_hash);
    end
    else
    begin
      TStreamOp.WriteAnsiString(stream, FAccountStorageHash);
    end;
  finally
    EndThreadSave;
  end;
end;

class function TAccountStorage.CopyChunk(Source, dest: TStream; FromBlock, ToBlock: Cardinal;
  var errors: AnsiString): Boolean;
var
  iblock: Cardinal;
  raw: TRawBytes;
  posOffsetZoneSource, posOffsetZoneDest, posFinal, posBlocksZoneDest, posInitial: Int64;
  offsetsSource, offsetsDest: {$ifdef USE_GENERICS}TArray<Cardinal>{$else}TCardinalArray{$endif};
  destTotalBlocks: Cardinal;
  sbHeader: TAccountStorageHeader;
begin
  Result := false;
  errors := '';
  posInitial := Source.Position;
  try
    if (FromBlock > ToBlock) then
    begin
      errors := Format('Invalid CopyStream(from %d, to %d)', [FromBlock, ToBlock]);
      exit;
    end;
    if not LoadHeaderFromStream(Source, sbHeader) then
    begin
      errors := 'Invalid stream. Invalid header/version';
      exit;
    end;
    if (sbHeader.startBlock > FromBlock) or (sbHeader.endBlock < ToBlock) or
      ((sbHeader.startBlock + sbHeader.blocksCount) < ToBlock) then
    begin
      errors := Format('Stream contain blocks from %d to %d (of %d). Need between %d and %d !',
        [sbHeader.startBlock, sbHeader.endBlock, sbHeader.blocksCount, FromBlock, ToBlock]);
      exit;
    end;
    destTotalBlocks := ToBlock - FromBlock + 1;
    TLog.NewLog(ltInfo, Classname,
      Format('Copy Stream from AccountStorage with %d to %d (of %d sbh:%s) to AccountStorage with %d and %d',
      [sbHeader.startBlock, sbHeader.endBlock, sbHeader.blocksCount, TCrypto.ToHexaString(sbHeader.AccountStorageHash),
      FromBlock, ToBlock]));
    // Read Source Offset zone
    posOffsetZoneSource := Source.Position;
    SetLength(offsetsSource, (sbHeader.endBlock - sbHeader.startBlock) + 2);
    Source.Read(offsetsSource[0], 4 * length(offsetsSource));
    // DEST STREAM:
    // Init dest stream
    // Header zone
    SaveHeaderToStream(dest, sbHeader.Protocol, FromBlock, ToBlock, sbHeader.blocksCount);
    // Offsets zone
    posOffsetZoneDest := dest.Position;
    SetLength(raw, (destTotalBlocks + 1) * 4); // Cardinal = 4 bytes for each block + End position
    FillChar(raw[1], length(raw), 0);
    dest.WriteBuffer(raw[1], length(raw));
    SetLength(offsetsDest, destTotalBlocks + 1);
    // Blocks zone
    posBlocksZoneDest := dest.Position;
    TLog.NewLog(ltInfo, Classname,
      Format('Copying AccountStorage Stream from source Position %d (size:%d) to dest %d bytes - OffsetSource[%d] - OffsetSource[%d]',
      [posOffsetZoneSource + offsetsSource[FromBlock - sbHeader.startBlock], Source.Size,
      offsetsSource[ToBlock - sbHeader.startBlock + 1] - offsetsSource[FromBlock - sbHeader.startBlock],
      ToBlock - sbHeader.startBlock + 1, FromBlock - sbHeader.startBlock]));

    Source.Position := posOffsetZoneSource + offsetsSource[FromBlock - sbHeader.startBlock];
    dest.CopyFrom(Source, offsetsSource[ToBlock - sbHeader.startBlock + 1] - offsetsSource
      [FromBlock - sbHeader.startBlock]);
    // Save offsets zone with valid values
    posFinal := dest.Position;
    dest.Position := posOffsetZoneDest;
    for iblock := FromBlock to ToBlock do
    begin
      offsetsDest[iblock - FromBlock] := offsetsSource[iblock - (sbHeader.startBlock)] -
        offsetsSource[FromBlock - sbHeader.startBlock] + (posBlocksZoneDest - posOffsetZoneDest);
    end;
    offsetsDest[high(offsetsDest)] := posFinal - posOffsetZoneDest;

    dest.WriteBuffer(offsetsDest[0], length(offsetsDest) * 4);
    dest.Position := posFinal;
    Source.Position := offsetsSource[high(offsetsSource)] + posOffsetZoneSource;
    TStreamOp.ReadAnsiString(Source, raw);
    TStreamOp.WriteAnsiString(dest, raw);
    Result := true;
  finally
    Source.Position := posInitial;
  end;
end;

class function TAccountStorage.ConcatStream(Source1, Source2, dest: TStream; var errors: AnsiString): Boolean;
  function MinCardinal(v1, v2: Cardinal): Cardinal;
  begin
    if v1 < v2 then
      Result := v1
    else
      Result := v2;
  end;
  function MaxCardinal(v1, v2: Cardinal): Cardinal;
  begin
    if v1 > v2 then
      Result := v1
    else
      Result := v2;
  end;
  function ReadAccountStorageEntryFromStream(stream: TStream; offsetIndex: Cardinal; destStream: TStream): Cardinal;
  // PRE: safeBoxStream is a valid SafeBox Stream (with enough size) located at Offsets zone, and offsetIndex is >=0 and <= end block
  // Returns the size of the saved block at destStream
  var
    offsetPos, auxPos: Int64;
    c, cNext: Cardinal;
  begin
    Result := 0;
    offsetPos := stream.Position;
    try
      stream.Seek(4 * offsetIndex, soFromCurrent);
      stream.Read(c, 4);
      stream.Read(cNext, 4);
      if cNext < c then
        exit;
      Result := cNext - c; // Result is the offset difference between blocks
      if Result <= 0 then
        exit;
      auxPos := offsetPos + c;
      if stream.Size < auxPos + Result then
        exit; // Invalid
      stream.Position := auxPos;
      destStream.CopyFrom(stream, Result);
    finally
      stream.Position := offsetPos;
    end;
  end;

  procedure WriteEntryToStream(stream, targetStream: TStream; nBytes: Integer; offsetIndex, totalOffsets: Cardinal);
  // PRE: safeBoxStream is a valid SafeBox Stream located at Offsets zone, and offsetIndex=0 or offsetIndex-1 has a valid value
  var
    offsetPos: Int64;
    c, cLength: Cardinal;
  begin
    offsetPos := targetStream.Position;
    try
      if offsetIndex = 0 then
      begin
        // First
        c := ((totalOffsets + 1) * 4);
        targetStream.Write(c, 4);
      end
      else
      begin
        targetStream.Seek(4 * (offsetIndex), soFromCurrent);
        targetStream.Read(c, 4); // c is position
      end;
      cLength := c + nBytes;
      targetStream.Write(cLength, 4);
      targetStream.Position := offsetPos + c;
      targetStream.CopyFrom(stream, nBytes);
    finally
      targetStream.Position := offsetPos;
    end;
  end;

var
  destStartBlock, destEndBlock, nBlock: Cardinal;
  source1InitialPos, source2InitialPos, destOffsetPos: Int64;
  ms: TMemoryStream;
  c: Cardinal;
  destOffsets: {$ifdef USE_GENERICS}TArray<Cardinal>{$else}TCardinalArray{$endif};
  i: Integer;
  s1Header, s2Header: TAccountStorageHeader;
begin
  Result := false;
  errors := '';
  source1InitialPos := Source1.Position;
  source2InitialPos := Source2.Position;
  try
    if not LoadHeaderFromStream(Source1, s1Header) then
    begin
      errors := 'Invalid source 1 stream. Invalid header/version';
      exit;
    end;
    if not LoadHeaderFromStream(Source2, s2Header) then
    begin
      errors := 'Invalid source 2 stream. Invalid header/version';
      exit;
    end;
    // Check SBH and blockcount
    if (s1Header.AccountStorageHash <> s2Header.AccountStorageHash) or (s1Header.blocksCount <> s2Header.blocksCount) or
      (s1Header.Protocol <> s2Header.Protocol) then
    begin
      errors := Format('Source1 and Source2 have diff. Source 1 %d %s (protocol %d) Source 2 %d %s (protocol %d)',
        [s1Header.blocksCount, TCrypto.ToHexaString(s1Header.AccountStorageHash), s1Header.Protocol,
        s2Header.blocksCount, TCrypto.ToHexaString(s2Header.AccountStorageHash), s2Header.Protocol]);
      exit;
    end;
    // Save dest heaer
    destStartBlock := MinCardinal(s1Header.startBlock, s2Header.startBlock);
    destEndBlock := MaxCardinal(s1Header.endBlock, s2Header.endBlock);
    SaveHeaderToStream(dest, s1Header.Protocol, destStartBlock, destEndBlock, s1Header.blocksCount);
    // Save offsets
    destOffsetPos := dest.Position;
    SetLength(destOffsets, ((destEndBlock - destStartBlock) + 2));
    for i := low(destOffsets) to high(destOffsets) do
      destOffsets[i] := 0;
    dest.Write(destOffsets[0], ((destEndBlock - destStartBlock) + 2) * 4);
    dest.Position := destOffsetPos;
    //
    nBlock := destStartBlock;
    ms := TMemoryStream.Create;
    try
      for nBlock := destStartBlock to destEndBlock do
      begin
        ms.Clear;
        if (nBlock >= s1Header.startBlock) and (nBlock <= s1Header.endBlock) then
        begin
          c := ReadAccountStorageEntryFromStream(Source1, nBlock - s1Header.startBlock, ms);
          ms.Position := 0;
          WriteEntryToStream(ms, dest, c, nBlock - destStartBlock, destEndBlock - destStartBlock + 1);
        end
        else if (nBlock >= s2Header.startBlock) and (nBlock <= s2Header.endBlock) then
        begin
          c := ReadAccountStorageEntryFromStream(Source2, nBlock - s2Header.startBlock, ms);
          ms.Position := 0;
          WriteEntryToStream(ms, dest, c, nBlock - destStartBlock, destEndBlock - destStartBlock + 1);
        end
        else
          raise Exception.Create('ERROR DEV 20170518-1');
      end;
    finally
      ms.Free;
    end;
    // Save SafeBoxHash at the end
    dest.Seek(0, soFromEnd);
    TStreamOp.WriteAnsiString(dest, s1Header.AccountStorageHash);
    Result := true;
  finally
    Source1.Position := source1InitialPos;
    Source2.Position := source2InitialPos;
  end;
end;

class function TAccountStorage.AccountNameIsValid(const new_name: TRawBytes; var errors: AnsiString): Boolean;
{ Note:
  This function is case senstive, and only lower case chars are valid.
  Execute a LowerCase() prior to call this function!
}
const
  CT_MicroCoin_Base64_Charset: ShortString = 'abcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*()-+{}[]\_:"|<>,.?/~';
  // First char can't start with a number
  CT_MicroCoin_FirstChar_Charset: ShortString = 'abcdefghijklmnopqrstuvwxyz!@#$%^&*()-+{}[]\_:"|<>,.?/~';
  CT_MicroCoin_name_min_length = 3;
  CT_MicroCoin_name_max_length = 64;
var
  i, j: Integer;
begin
  Result := false;
  errors := '';
  if (length(new_name) < CT_MicroCoin_name_min_length) or (length(new_name) > CT_MicroCoin_name_max_length) then
  begin
    errors := 'Invalid length:' + inttostr(length(new_name)) + ' (valid from ' + inttostr(CT_MicroCoin_name_max_length)
      + ' to ' + inttostr(CT_MicroCoin_name_max_length) + ')';
    exit;
  end;
  for i := 1 to length(new_name) do
  begin
    j := 1;
    if (i = 1) then
    begin
      // First char can't start with a number
      while (j <= length(CT_MicroCoin_FirstChar_Charset)) and (new_name[i] <> CT_MicroCoin_FirstChar_Charset[j]) do
        inc(j);
      if j > length(CT_MicroCoin_FirstChar_Charset) then
      begin
        errors := 'Invalid char ' + new_name[i] + ' at first pos';
        exit; // Not found
      end;
    end
    else
    begin
      while (j <= length(CT_MicroCoin_Base64_Charset)) and (new_name[i] <> CT_MicroCoin_Base64_Charset[j]) do
        inc(j);
      if j > length(CT_MicroCoin_Base64_Charset) then
      begin
        errors := 'Invalid char ' + new_name[i] + ' at pos ' + inttostr(i);
        exit; // Not found
      end;
    end;
  end;
  Result := true;
end;

function TAccountStorage.IsValidNewBlockHeader(const newOperationBlock: TBlockHeader; validateHash: Boolean;
  var errors: AnsiString): Boolean;
{ This function will check a OperationBlock info as a valid candidate to be included in the safebox

  TOperationBlock contains the info of the new block EXCEPT the operations, including only operations_hash value (SHA256 of the Operations)
  So, cannot check operations and fee values
}
var
  target_hash, PoW: TRawBytes;
  i: Integer;
  lastBlock: TBlockHeader;
begin
  Result := false;
  errors := '';
  if blocksCount > 0 then
    lastBlock := block(blocksCount - 1).BlockHeader
  else
    lastBlock := CT_OperationBlock_NUL;
  // Check block
  if (blocksCount <> newOperationBlock.block) then
  begin
    errors := 'block (' + inttostr(newOperationBlock.block) + ') is not new position (' + inttostr(blocksCount) + ')';
    exit;
  end;
  // Check Account key
  if not newOperationBlock.account_key.IsValidAccountKey(errors) then
  begin
    exit;
  end;
  // reward
  if (newOperationBlock.reward <> TMicroCoinProtocol.GetRewardForNewLine(newOperationBlock.block)) then
  begin
    errors := 'Invalid reward';
    exit;
  end;
  // fee: Cannot be checked only with the safebox
  // protocol available is not checked
  if (newOperationBlock.block > 0) then
  begin
    // protocol
    if (newOperationBlock.protocol_version <> CurrentProtocol) then
    begin
      // Protocol must be 1 or 2. If 1 then all prior blocksmust be 1 and never 2 (invalide blockchain version scenario v1...v2...v1)
      if (lastBlock.protocol_version > newOperationBlock.protocol_version) then
      begin
        errors := 'Invalid MicroCoin protocol version: ' + inttostr(newOperationBlock.protocol_version) + ' Current: ' +
          inttostr(CurrentProtocol) + ' Previous:' + inttostr(lastBlock.protocol_version);
        exit;
      end;
      if (newOperationBlock.protocol_version = CT_PROTOCOL_2) then
      begin
        if (newOperationBlock.block < CT_Protocol_Upgrade_v2_MinBlock) then
        begin
          errors := 'Upgrade to protocol version 2 available at block: ' + inttostr(CT_Protocol_Upgrade_v2_MinBlock);
          exit;
        end;
      end
      else if (newOperationBlock.protocol_version <> CT_PROTOCOL_1) then
      begin
        errors := 'Invalid protocol version change to ' + inttostr(newOperationBlock.protocol_version);
        exit;
      end;
    end
    else if (not(newOperationBlock.protocol_version in [CT_PROTOCOL_1, CT_PROTOCOL_2])) then
    begin
      errors := 'Invalid protocol version ' + inttostr(newOperationBlock.protocol_version);
      exit;
    end;
    // timestamp
    if ((newOperationBlock.timestamp) < (lastBlock.timestamp)) then
    begin
      errors := 'Invalid timestamp (Back timestamp: New timestamp:' + inttostr(newOperationBlock.timestamp) +
        ' < last timestamp (' + inttostr(blocksCount - 1) + '):' + inttostr(lastBlock.timestamp) + ')';
      exit;
    end;
  end
  else
  begin
    if (CT_Zero_Block_Proof_of_work_in_Hexa <> '') then
    begin
      // Check if valid Zero block
      if not(AnsiSameText(TCrypto.ToHexaString(newOperationBlock.proof_of_work), CT_Zero_Block_Proof_of_work_in_Hexa))
      then
      begin
        errors := 'Zero block not valid, Proof of Work invalid: ' +
          TCrypto.ToHexaString(newOperationBlock.proof_of_work) + '<>' + CT_Zero_Block_Proof_of_work_in_Hexa;
        exit;
      end;
    end;
  end;
  // compact_target
  target_hash := GetActualTargetHash(newOperationBlock.protocol_version = CT_PROTOCOL_2);
  if (newOperationBlock.compact_target <> TMicroCoinProtocol.TargetToCompact(target_hash)) then
  begin
    errors := 'Invalid target found:' + IntToHex(newOperationBlock.compact_target, 8) + ' actual:' +
      IntToHex(TMicroCoinProtocol.TargetToCompact(target_hash), 8);
    exit;
  end;
  // nonce: Not checked
  // block_payload: Checking Miner payload size
  if length(newOperationBlock.block_payload) > CT_MaxPayloadSize then
  begin
    errors := 'Invalid Miner Payload length: ' + inttostr(length(newOperationBlock.block_payload));
    exit;
  end;
  // Checking Miner Payload valid chars
  for i := 1 to length(newOperationBlock.block_payload) do
  begin
    if not(newOperationBlock.block_payload[i] in [#32 .. #254]) then
    begin
      errors := 'Invalid Miner Payload character at pos ' + inttostr(i) + ' value:' +
        inttostr(ord(newOperationBlock.block_payload[i]));
      exit;
    end;
  end;
  // initial_safe_box_hash: Only can be checked when adding new blocks, not when restoring a safebox
  if validateHash then
  begin
    // TODO: Can use FSafeBoxHash instead of CalcSafeBoxHash ???? Quick speed if possible
    if (newOperationBlock.initial_safe_box_hash <> FAccountStorageHash) then
    begin
      errors := 'BlockChain Safe box hash invalid: ' + TCrypto.ToHexaString(newOperationBlock.initial_safe_box_hash) +
        ' var: ' + TCrypto.ToHexaString(FAccountStorageHash) + ' Calculated:' + TCrypto.ToHexaString(CalculateHash);
      exit;
    end;
  end;
  // operations_hash: NOT CHECKED WITH OPERATIONS!
  if (length(newOperationBlock.operations_hash) <> 32) then
  begin
    errors := 'Invalid Operations hash value: ' + TCrypto.ToHexaString(newOperationBlock.operations_hash) + ' length=' +
      inttostr(length(newOperationBlock.operations_hash));
    exit;
  end;
  // proof_of_work:
  TMicroCoinProtocol.CalcProofOfWork(newOperationBlock, PoW);
  if (PoW <> newOperationBlock.proof_of_work) then
  begin
    errors := 'Proof of work is bad calculated ' + TCrypto.ToHexaString(newOperationBlock.proof_of_work) + ' <> Good: '
      + TCrypto.ToHexaString(PoW);
    exit;
  end;
  if (newOperationBlock.proof_of_work > target_hash) then
  begin
    errors := 'Proof of work is higher than target ' + TCrypto.ToHexaString(newOperationBlock.proof_of_work) + ' > ' +
      TCrypto.ToHexaString(target_hash);
    exit;
  end;
  Result := true;
end;

function TAccountStorage.GetActualTargetHash(UseProtocolV2: Boolean): TRawBytes;
{ Target is calculated in each block with avg obtained in previous
  CT_CalcNewDifficulty blocks.
  If Block is lower than CT_CalcNewDifficulty then is calculated
  with all previous blocks.
}
var
  ts1, ts2, tsTeorical, tsReal, tsTeoricalStop, tsRealStop: Int64;
  CalcBack: Integer;
  lastBlock: TBlockHeader;
begin
  if (blocksCount <= 1) then
  begin
    // Important: CT_MinCompactTarget is applied for blocks 0 until ((CT_CalcNewDifficulty*2)-1)
    Result := TMicroCoinProtocol.TargetFromCompact(CT_MinCompactTarget);
  end
  else
  begin
    if blocksCount > CT_CalcNewTargetBlocksAverage then
      CalcBack := CT_CalcNewTargetBlocksAverage
    else
      CalcBack := blocksCount - 1;
    lastBlock := block(blocksCount - 1).BlockHeader;
    // Calc new target!
    ts1 := lastBlock.timestamp;
    ts2 := block(blocksCount - CalcBack - 1).BlockHeader.timestamp;
    tsTeorical := (CalcBack * CT_NewLineSecondsAvg);
    tsReal := (ts1 - ts2);
    if (not UseProtocolV2) then
    begin
      Result := TMicroCoinProtocol.GetNewTarget(tsTeorical, tsReal,
        TMicroCoinProtocol.TargetFromCompact(lastBlock.compact_target));
    end
    else
    begin
      CalcBack := CalcBack div CT_CalcNewTargetLimitChange_SPLIT;
      if CalcBack = 0 then
        CalcBack := 1;
      ts2 := block(blocksCount - CalcBack - 1).BlockHeader.timestamp;
      tsTeoricalStop := (CalcBack * CT_NewLineSecondsAvg);
      tsRealStop := (ts1 - ts2);
      { Protocol 2 change:
        Only will increase/decrease Target if (CT_CalcNewTargetBlocksAverage DIV 10) needs to increase/decrease too, othewise use
        current Target.
        This will prevent sinusoidal movement and provide more stable hashrate, computing always time from CT_CalcNewTargetBlocksAverage }
      if ((tsTeorical > tsReal) and (tsTeoricalStop > tsRealStop)) or
        ((tsTeorical < tsReal) and (tsTeoricalStop < tsRealStop)) then
      begin
        Result := TMicroCoinProtocol.GetNewTarget(tsTeorical, tsReal,
          TMicroCoinProtocol.TargetFromCompact(lastBlock.compact_target));
      end
      else
      begin
        // Nothing to do!
        Result := TMicroCoinProtocol.TargetFromCompact(lastBlock.compact_target);
      end;
    end;
  end;
end;

function TAccountStorage.GetActualCompactTargetHash(UseProtocolV2: Boolean): Cardinal;
begin
  Result := TMicroCoinProtocol.TargetToCompact(GetActualTargetHash(UseProtocolV2));
end;

function TAccountStorage.FindAccountByName(aName: AnsiString): Integer;
var
  nameLower: AnsiString;
  i: Integer;
begin
  nameLower := LowerCase(aName);
  i := FOrderedByName.IndexOf(aName);
  if i >= 0 then
    Result := FOrderedByName.GetTag(i)
  else
    Result := -1;
end;

procedure TAccountStorage.SetAccount(account_number: Cardinal; const newAccountInfo: TAccountInfo;
  const newName: TRawBytes; newType: Word; newBalance: UInt64; newN_operation: Cardinal);
var
  iblock: Cardinal;
  i, j, iAccount: Integer;
  lastbalance: UInt64;
  acc: TAccount;
  bacc: TAccountStorageEntry;
  P: PBlockAccount;
begin
  iblock := account_number div CT_AccountsPerBlock;
  iAccount := account_number mod CT_AccountsPerBlock;
  acc := Account(account_number);
  P := FBlockAccountsList.Items[iblock];

  if (not TAccountKey.EqualAccountKeys(acc.AccountInfo.AccountKey, newAccountInfo.AccountKey)) then
  begin
    AccountKeyListRemoveAccount(acc.AccountInfo.AccountKey, [account_number]);
    AccountKeyListAddAccounts(newAccountInfo.AccountKey, [account_number]);
  end;

  acc.AccountInfo := newAccountInfo;
  // Name:
  if acc.name <> newName then
  begin
    if acc.name <> '' then
    begin
      i := FOrderedByName.IndexOf(acc.name);
      if i < 0 then
        TLog.NewLog(ltError, Classname, 'ERROR DEV 20170606-1')
      else
        FOrderedByName.Delete(i);
    end;
    acc.name := newName;
    if acc.name <> '' then
    begin
      i := FOrderedByName.IndexOf(acc.name);
      if i >= 0 then
        TLog.NewLog(ltError, Classname, 'ERROR DEV 20170606-2')
      else
        FOrderedByName.Add(acc.name, account_number);
    end;
  end;
  acc.account_type := newType;
  lastbalance := acc.balance;
  acc.balance := newBalance;
  // Will update previous_updated_block only on first time/block
  if acc.updated_block <> blocksCount then
  begin
    acc.previous_updated_block := acc.updated_block;
    acc.updated_block := blocksCount;
  end;
  acc.n_operation := newN_operation;
  // Save new account values
  ToTMemAccount(acc, P^.accounts[iAccount]);
  // Update block_hash
  bacc := block(iblock);

{$IFDEF uselowmem}
  TBaseType.To32Bytes(CalcBlockHash(bacc, FCurrentProtocol >= CT_PROTOCOL_2), P^.block_hash);
{$ELSE}
  P^.block_hash := CalcBlockHash(bacc, FCurrentProtocol >= CT_PROTOCOL_2);
{$ENDIF}
  // Update buffer block hash
  j := (length(P^.block_hash) * (iblock));
  for i := 1 to length(P^.block_hash) do
  begin
{$IFDEF FPC}
    FBufferBlocksHash[i + j] := AnsiChar(P^.block_hash[i - (low(FBufferBlocksHash) - low(P^.block_hash))]);
{$ELSE}
    FBufferBlocksHash[i + j] := AnsiChar(P^.block_hash[i - {$IFDEF uselowmem}1{$ELSE}0{$ENDIF}]);
{$ENDIF}
  end;

  FTotalBalance := FTotalBalance - (Int64(lastbalance) - Int64(newBalance));
  FTotalFee := FTotalFee + (Int64(lastbalance) - Int64(newBalance));
end;

procedure TAccountStorage.StartThreadSafe;
begin
  TPCThread.ProtectEnterCriticalSection(Self, FLock);
end;

type
  TOrderedAccountKeyList = record
    rawaccountkey: TRawBytes;
    accounts_number: TOrderedList;
  end;

  POrderedAccountKeyList = ^TOrderedAccountKeyList;

function SortOrdered(Item1, Item2: Pointer): Integer;
begin
  Result := PtrInt(Item1) - PtrInt(Item2);
end;

procedure TOrderedAccountKeysList.AddAccountKey(const AccountKey: TAccountKey);
var
  P: POrderedAccountKeyList;
  i, j: Integer;
begin
  if not Find(AccountKey, i) then
  begin
    New(P);
    P^.rawaccountkey := AccountKey.ToRawString;
    P^.accounts_number := TOrderedList.Create;
    FOrderedAccountKeysList.Insert(i, P);
    // Search this key in the AccountsList and add all...
    j := 0;
    if Assigned(FAccountStorage) then
    begin
      for i := 0 to FAccountStorage.AccountsCount - 1 do
      begin
        if TAccountKey.EqualAccountKeys(FAccountStorage.Account(i).AccountInfo.AccountKey, AccountKey) then
        begin
          // Note: P^.accounts will be ascending ordered due to "for i:=0 to ..."
          P^.accounts_number.Add(i);
        end;
      end;
      TLog.NewLog(ltDebug, Classname, Format('Adding account key (%d of %d) %s', [j, FAccountStorage.AccountsCount,
        TCrypto.ToHexaString(AccountKey.ToRawString)]));
    end
    else
    begin
      TLog.NewLog(ltDebug, Classname, Format('Adding account key (no Account List) %s',
        [TCrypto.ToHexaString(AccountKey.ToRawString)]));
    end;
  end;
end;

procedure TOrderedAccountKeysList.AddAccounts(const AccountKey: TAccountKey; const accounts: array of Cardinal);
var
  P: POrderedAccountKeyList;
  i, i2: Integer;
begin
  if Find(AccountKey, i) then
  begin
    P := POrderedAccountKeyList(FOrderedAccountKeysList[i]);
  end
  else if (FAutoAddAll) then
  begin
    New(P);
    P^.rawaccountkey := AccountKey.ToRawString;
    P^.accounts_number := TOrderedList.Create;
    FOrderedAccountKeysList.Insert(i, P);
  end
  else
    exit;
  for i := low(accounts) to high(accounts) do
  begin
    P^.accounts_number.Add(accounts[i]);
  end;
end;

procedure TOrderedAccountKeysList.Clear;
begin
  ClearAccounts(true);
end;

procedure TOrderedAccountKeysList.ClearAccounts(RemoveAccountList: Boolean);
var
  P: POrderedAccountKeyList;
  i: Integer;
begin
  for i := 0 to FOrderedAccountKeysList.Count - 1 do
  begin
    P := FOrderedAccountKeysList[i];
    if RemoveAccountList then
    begin
      P^.accounts_number.Free;
      P^:=Default(TOrderedAccountKeyList);
      Dispose(P);
      P := nil;
    end
    else
    begin
      P^.accounts_number.Clear;
    end;
  end;
  if RemoveAccountList then
  begin
    FOrderedAccountKeysList.Clear;
  end;
end;

function TOrderedAccountKeysList.Count: Integer;
begin
  Result := FOrderedAccountKeysList.Count;
end;

constructor TOrderedAccountKeysList.Create(AccountStorage: TAccountStorage; AutoAddAll: Boolean);
var
  i: Integer;
begin
  TLog.NewLog(ltDebug, Classname, 'Creating an Ordered Account Keys List adding all:' + CT_TRUE_FALSE[AutoAddAll]);
  FAutoAddAll := AutoAddAll;
  FAccountStorage := AccountStorage;
  FOrderedAccountKeysList := TList.Create;
  if Assigned(AccountStorage) then
  begin
    AccountStorage.ListOfOrderedAccountKeysList.Add(Self);
    if AutoAddAll then
    begin
      for i := 0 to AccountStorage.AccountsCount - 1 do
      begin
        AddAccountKey(AccountStorage.Account(i).AccountInfo.AccountKey);
      end;
    end;
  end;
end;

destructor TOrderedAccountKeysList.Destroy;
begin
  TLog.NewLog(ltDebug, Classname, 'Destroying an Ordered Account Keys List adding all:' + CT_TRUE_FALSE[FAutoAddAll]);
  if Assigned(FAccountStorage) then
  begin
    FAccountStorage.ListOfOrderedAccountKeysList.Remove(Self);
  end;
  ClearAccounts(true);
  FreeAndNil(FOrderedAccountKeysList);
  inherited;
end;

function TOrderedAccountKeysList.Find(const AccountKey: TAccountKey; var Index: Integer): Boolean;
var
  L, H, i, c: Integer;
  rak: TRawBytes;
begin
  Result := false;
  rak := AccountKey.ToRawString;
  L := 0;
  H := FOrderedAccountKeysList.Count - 1;
  while L <= H do
  begin
    i := (L + H) shr 1;
    c := CompareStr(POrderedAccountKeyList(FOrderedAccountKeysList[i]).rawaccountkey, rak);
    if c < 0 then
      L := i + 1
    else
    begin
      H := i - 1;
      if c = 0 then
      begin
        Result := true;
        L := i;
      end;
    end;
  end;
  index := L;
end;

function TOrderedAccountKeysList.GetAccountKey(Index: Integer): TAccountKey;
var
  raw: TRawBytes;
begin
  raw := POrderedAccountKeyList(FOrderedAccountKeysList[index]).rawaccountkey;
  Result := TAccountKey.FromRawString(raw);
end;

function TOrderedAccountKeysList.GetAccountList(Index: Integer): TOrderedList;
begin
  Result := POrderedAccountKeyList(FOrderedAccountKeysList[index]).accounts_number;
end;

function TOrderedAccountKeysList.IndexOfAccountKey(const AccountKey: TAccountKey): Integer;
begin
  if not Find(AccountKey, Result) then
    Result := -1;
end;

procedure TOrderedAccountKeysList.RemoveAccounts(const AccountKey: TAccountKey; const accounts: array of Cardinal);
var
  P: POrderedAccountKeyList;
  i, j: Integer;
begin
  if not Find(AccountKey, i) then
    exit; // Nothing to do
  P := POrderedAccountKeyList(FOrderedAccountKeysList[i]);
  for j := low(accounts) to high(accounts) do
  begin
    P^.accounts_number.Remove(accounts[j]);
  end;
  if (P^.accounts_number.Count = 0) and (FAutoAddAll) then
  begin
    // Remove from list
    FOrderedAccountKeysList.Delete(i);
    // Free it
    P^.accounts_number.Free;
    P^ := Default(TOrderedAccountKeyList);
    Dispose(P);
  end;
end;

procedure TOrderedAccountKeysList.RemoveAccountKey(const AccountKey: TAccountKey);
var
  P: POrderedAccountKeyList;
  i: Integer;
begin
  if not Find(AccountKey, i) then
    exit; // Nothing to do
  P := POrderedAccountKeyList(FOrderedAccountKeysList[i]);
  // Remove from list
  FOrderedAccountKeysList.Delete(i);
  // Free it
  P^.accounts_number.Free;
  P^ := Default(TOrderedAccountKeyList);
  Dispose(P);
end;

end.
