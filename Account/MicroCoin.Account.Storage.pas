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

uses SysUtils, Classes, UCrypto, UThread, MicroCoin.Common.Lists,
  MicroCoin.Crypto.BigNum, SyncObjs,
  MicroCoin.Account.Data, MicroCoin.Account.AccountKey, MicroCoin.BlockChain.BlockHeader,
  MicroCoin.Common.Config, UBaseTypes, ULog, MicroCoin.BlockChain.Protocol;

type

  EInvalidAccountException = class(Exception)

  end;

  TAccountStorage = class;

  TAccountStorageEntry = record

    BlockHeader: TBlockHeader;
    Accounts: array [0 .. cAccountsPerBlock - 1] of TAccount;
    BlockHash: AnsiString; // Calculated on every block change (on create and on accounts updated)
    AccumulatedWork: UInt64; // Accumulated work (previous + target) this value can be calculated.

    class function Empty: TAccountStorageEntry; static;

    function ToString : AnsiString;
    function CalcBlockHash (AUseProtocol2Method: Boolean): TRawBytes;
  end;

  TAccountStorageHeader = record
  public
    Protocol: Word;
    StartBlock: Cardinal;
    EndBlock: Cardinal;
    BlocksCount: Cardinal;
    AccountStorageHash: TRawBytes;
  public
    class function LoadFromStream(AStream : TStream) : TAccountStorageHeader; static;
    class function Empty : TAccountStorageHeader; static;
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
    FLock: TCriticalSection; // Thread safe
    FWorkSum: UInt64;
    FCurrentProtocol: Integer;

    function GetAccountsCount: Integer;
    function GetBlocksCount: Integer;
    function GetAccount(AAccountNumber: Cardinal): TAccount;
    function GetBlock(ABlockNumber: Cardinal): TAccountStorageEntry;

    procedure AccountKeyListAddAccounts(const AAccountKey: TAccountKey; const AAccounts: array of Cardinal);
    procedure AccountKeyListRemoveAccount(const AAccountKey: TAccountKey; const AAccounts: array of Cardinal);
  public

    constructor Create;
    destructor Destroy; override;

    class function MustSaved(ABlocksCount: Cardinal): Boolean;

    class function SaveHeaderToStream(AStream: TStream; AProtocol: Word;
      AOffsetStartBlock, AOffsetEndBlock, ACurrentBlocksCount: Cardinal): Boolean;
    class function CopyChunk(ASource, ADestination: TStream; AFromBlock, AToBlock: Cardinal; var RErrors: AnsiString): Boolean;
    class function ConcatStream(ASource1, ASource2, ADestionation: TStream; var RErrors: AnsiString): Boolean;
    procedure CopyFrom(AAccounts: TAccountStorage);
    function LoadFromStream(AStream: TStream; ACheckAll: Boolean; var ALastReadBlock: TAccountStorageEntry;
      var errors: AnsiString): Boolean;
    procedure SaveToStream(AStream: TStream; FromBlock, AToBlock: Cardinal);
    procedure SaveEntryToStream(AStream: TStream; ANBlock: Cardinal);

    function DoUpgradeToProtocol2: Boolean;
    function CanUpgradeToProtocol2: Boolean;

    function GetActualTargetHash(AUseProtocolV2: Boolean): TRawBytes;
    function GetActualCompactTargetHash(AUseProtocolV2: Boolean): Cardinal;

    class function IsValidAccountName(const ANewName: TRawBytes; var RErrors: AnsiString): Boolean;
    function IsValidNewBlockHeader(const ANewOperationBlock: TBlockHeader; AValidateHash: Boolean;
      var RErrors: AnsiString): Boolean;

    procedure UpdateAccount(AAccountNumber: Cardinal; const ANewAccountInfo: TAccountInfo; const ANewName: TRawBytes;
      ANewType: Word; ANewBalance: UInt64; ANewNumberOfTransactions: Cardinal);
    function AddNew(const ABlockHeader: TBlockHeader): TAccountStorageEntry;

    function FindAccountByName(AName: AnsiString): Integer;

    procedure Clear;

    function CalculateHash: TRawBytes;
    function CalcBlockHashRateInKhs(ABlockNumber: Cardinal; APreviousBlocksAverage: Cardinal): Int64;

    procedure StartThreadSafe;
    procedure EndThreadSave;
    procedure CheckMemory;

    property BlocksCount : Integer read GetBlocksCount;
    property AccountsCount : Integer read GetAccountsCount;
    property TotalBalance: UInt64 read FTotalBalance;
    property AccountStorageHash: TRawBytes read FAccountStorageHash;
    property WorkSum: UInt64 read FWorkSum;
    property CurrentProtocol: Integer read FCurrentProtocol;
    property TotalFee: Int64 read FTotalFee;
    property ListOfOrderedAccountKeysList: TList read FListOfOrderedAccountKeysList;
    property Accounts[AAccountNumber : Cardinal] : TAccount read GetAccount;
    property Blocks[ABlockNumber : Cardinal] : TAccountStorageEntry read GetBlock;
  end;

  TOrderedAccountKeysList = class
  private
    FAutoAddAll: Boolean;
    FAccountStorage: TAccountStorage;
    FOrderedAccountKeysList: TList; // An ordered list of pointers to quickly find account keys in account list
    function Find(const AccountKey: TAccountKey; var Index: Integer): Boolean;
    function GetAccountList(AIndex: Integer): TOrderedList;
    function GetAccountKey(AIndex: Integer): TAccountKey;
  public
    procedure ClearAccounts(ARemoveAccountList: Boolean);
    constructor Create(AAccountStorage: TAccountStorage; AAutoAddAll: Boolean);
    destructor Destroy; override;
    procedure AddAccountKey(const AAccountKey: TAccountKey);
    procedure RemoveAccountKey(const AAccountKey: TAccountKey);
    procedure AddAccounts(const AAccountKey: TAccountKey; const AAccounts: array of Cardinal);
    procedure RemoveAccounts(const AAccountKey: TAccountKey; const AAccounts: array of Cardinal);
    function IndexOfAccountKey(const AAccountKey: TAccountKey): Integer;
    procedure Clear;
    function Count: Integer;
    property AccountList[index: Integer]: TOrderedList read GetAccountList;
    property AccountKey[index: Integer]: TAccountKey read GetAccountKey;
    property AccountStorage: TAccountStorage read FAccountStorage write FAccountStorage;
  end;

const
  CT_AccountChunkIdentificator = 'SafeBoxChunk';

implementation

uses MicroCoin.Common.Stream;

{$DEFINE uselowmem}

procedure ToTMemAccount(const Source: TAccount; var dest: TMemAccount);
begin
  dest := Source;
end;

procedure ToTAccount(const Source: TMemAccount; account_number: Cardinal; var dest: TAccount);
begin
  dest := Source;
end;

procedure ToTMemBlockAccount(const Source: TAccountStorageEntry; var dest: TMemBlockAccount);
{$IFDEF uselowmem}
var
  i: Integer;
  raw: TRawBytes;
{$ENDIF}
begin
{$IFDEF uselowmem}
  Source.BlockHeader.account_key.ToRawString(raw);
  TBaseType.To256RawBytes(raw, dest.BlockChainInfo.AccountKey);
  dest.BlockChainInfo.Reward := Source.BlockHeader.reward;
  dest.BlockChainInfo.Fee := Source.BlockHeader.fee;
  dest.BlockChainInfo.ProtocolVersion := Source.BlockHeader.protocol_version;
  dest.BlockChainInfo.ProtocolAvailable := Source.BlockHeader.protocol_available;
  dest.BlockChainInfo.Timestamp := Source.BlockHeader.timestamp;
  dest.BlockChainInfo.CompactTarget := Source.BlockHeader.compact_target;
  dest.BlockChainInfo.nonce := Source.BlockHeader.nonce;
  TBaseType.To256RawBytes(Source.BlockHeader.block_payload, dest.BlockChainInfo.BlockPayload);
  TBaseType.To32Bytes(Source.BlockHeader.initial_safe_box_hash, dest.BlockChainInfo.InitialAccountStorageHash);
  TBaseType.To32Bytes(Source.BlockHeader.transactionHash, dest.BlockChainInfo.TransactionHash);
  TBaseType.To32Bytes(Source.BlockHeader.proof_of_work, dest.BlockChainInfo.ProofOfWork);

  for i := low(Source.Accounts) to high(Source.Accounts)
  do ToTMemAccount(Source.Accounts[i], dest.Accounts[i]);

  TBaseType.To32Bytes(Source.BlockHash, dest.BlockHash);
  dest.AccumulatedWork := Source.AccumulatedWork;
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
  TBaseType.ToRawBytes(Source.BlockChainInfo.AccountKey, raw);
  TAccountKey.FromRawString(raw, dest.BlockHeader.account_key);
  dest.BlockHeader.reward := Source.BlockChainInfo.Reward;
  dest.BlockHeader.fee := Source.BlockChainInfo.Fee;
  dest.BlockHeader.protocol_version := Source.BlockChainInfo.ProtocolVersion;
  dest.BlockHeader.protocol_available := Source.BlockChainInfo.ProtocolAvailable;
  dest.BlockHeader.timestamp := Source.BlockChainInfo.Timestamp;
  dest.BlockHeader.compact_target := Source.BlockChainInfo.CompactTarget;
  dest.BlockHeader.nonce := Source.BlockChainInfo.nonce;
  TBaseType.ToRawBytes(Source.BlockChainInfo.BlockPayload, dest.BlockHeader.block_payload);
  TBaseType.ToRawBytes(Source.BlockChainInfo.InitialAccountStorageHash, dest.BlockHeader.initial_safe_box_hash);
  TBaseType.ToRawBytes(Source.BlockChainInfo.TransactionHash, dest.BlockHeader.transactionHash);
  TBaseType.ToRawBytes(Source.BlockChainInfo.ProofOfWork, dest.BlockHeader.proof_of_work);

  for i := low(Source.Accounts) to high(Source.Accounts) do
  begin
    ToTAccount(Source.Accounts[i], (block_number * cAccountsPerBlock) + i, dest.Accounts[i]);
  end;
  TBaseType.ToRawBytes(Source.BlockHash, dest.BlockHash);
  dest.AccumulatedWork := Source.AccumulatedWork;
{$ELSE}
  dest := Source;
{$ENDIF}
end;

function TAccountStorage.GetAccount(AAccountNumber: Cardinal): TAccount;
var
  b: Cardinal;
begin
  b := AAccountNumber div cAccountsPerBlock;
  if (b >= FBlockAccountsList.Count) then
    raise EInvalidAccountException.Create('Invalid account: ' + inttostr(AAccountNumber));
  ToTAccount(PBlockAccount(FBlockAccountsList.Items[b])^.Accounts[AAccountNumber mod cAccountsPerBlock],
    AAccountNumber, Result);
end;

function TAccountStorage.AddNew(const ABlockHeader: TBlockHeader): TAccountStorageEntry;
var
  i, base_addr: Integer;
  P: PBlockAccount;
  accs: array of Cardinal;
begin
  Result := TAccountStorageEntry.Empty;
  Result.BlockHeader := ABlockHeader;
  if ABlockHeader.block <> BlocksCount
  then raise Exception.Create('ERROR DEV 20170427-2');
  if ABlockHeader.fee <> FTotalFee
  then raise Exception.Create('ERROR DEV 20170427-3');
  base_addr := BlocksCount * cAccountsPerBlock;
  SetLength(accs, length(Result.Accounts));
  for i := low(Result.Accounts) to high(Result.Accounts) do
  begin
    Result.Accounts[i] := TAccount.Empty;
    Result.Accounts[i].AccountNumber := base_addr + i;
    Result.Accounts[i].AccountInfo.State := as_Normal;
    Result.Accounts[i].AccountInfo.AccountKey := ABlockHeader.account_key;
    Result.Accounts[i].UpdatedBlock := BlocksCount;
    Result.Accounts[i].NumberOfTransactions := 0;
    if i = low(Result.Accounts)
    then Result.Accounts[i].Balance := ABlockHeader.reward + ABlockHeader.fee;
    accs[i] := base_addr + i;
  end;
  inc(FWorkSum, Result.BlockHeader.compact_target);
  Result.AccumulatedWork := FWorkSum;
  // Calc block hash
  Result.BlockHash := Result.CalcBlockHash(FCurrentProtocol >= cPROTOCOL_2);

  New(P);
  ToTMemBlockAccount(Result, P^);

  FBlockAccountsList.Add(P);
  FBufferBlocksHash := FBufferBlocksHash + Result.BlockHash;
  inc(FTotalBalance, ABlockHeader.reward + ABlockHeader.fee);
  Dec(FTotalFee, ABlockHeader.fee);
  AccountKeyListAddAccounts(ABlockHeader.account_key, accs);
  // Calculating new value of safebox
  FAccountStorageHash := CalculateHash;
end;

procedure TAccountStorage.AccountKeyListAddAccounts(const AAccountKey: TAccountKey; const AAccounts: array of Cardinal);
var
  i: Integer;
begin
  for i := 0 to FListOfOrderedAccountKeysList.Count - 1
  do TOrderedAccountKeysList(FListOfOrderedAccountKeysList[i]).AddAccounts(AAccountKey, AAccounts);
end;

procedure TAccountStorage.AccountKeyListRemoveAccount(const AAccountKey: TAccountKey; const AAccounts: array of Cardinal);
var
  i: Integer;
begin
  for i := 0 to FListOfOrderedAccountKeysList.Count - 1
  do TOrderedAccountKeysList(FListOfOrderedAccountKeysList[i]).RemoveAccounts(AAccountKey, AAccounts);
end;

function TAccountStorage.GetAccountsCount: Integer;
begin
  Result := BlocksCount * cAccountsPerBlock;
end;

function TAccountStorage.GetBlock(ABlockNumber: Cardinal): TAccountStorageEntry;
begin
  if (ABlockNumber >= FBlockAccountsList.Count) then
    raise Exception.Create('Invalid block number: ' + inttostr(ABlockNumber));
  ToTBlockAccount(PBlockAccount(FBlockAccountsList.Items[ABlockNumber])^, ABlockNumber, Result);
end;

function TAccountStorage.GetBlocksCount: Integer;
begin
  Result := FBlockAccountsList.Count;
end;

function TAccountStorage.CalcBlockHashRateInKhs(ABlockNumber: Cardinal; APreviousBlocksAverage: Cardinal): Int64;
var
  c, t: Cardinal;
  t_sum: Extended;
  bn, bn_sum: BigInteger;
begin
  FLock.Acquire;
  try
    bn_sum := 0;
    if (ABlockNumber = 0) then
    begin
      Result := 1;
      exit;
    end;
    if (ABlockNumber >= FBlockAccountsList.Count) then
      raise Exception.Create('Invalid block number: ' + inttostr(ABlockNumber));
    if (APreviousBlocksAverage <= 0) then
      raise Exception.Create('Dev error 20161016-1');
    if (APreviousBlocksAverage > ABlockNumber) then
      APreviousBlocksAverage := ABlockNumber;
    //
    c := (ABlockNumber - APreviousBlocksAverage) + 1;
    t_sum := 0;
    while (c <= ABlockNumber) do
    begin
      bn := BigInteger.TargetToHashRate(PBlockAccount(FBlockAccountsList.Items[c])^.BlockChainInfo.CompactTarget);
      bn_sum.Add(bn);
      t_sum := t_sum + (PBlockAccount(FBlockAccountsList.Items[c])^.BlockChainInfo.Timestamp -
        PBlockAccount(FBlockAccountsList.Items[c - 1])^.BlockChainInfo.Timestamp);
      inc(c);
    end;
    bn_sum.Divide(APreviousBlocksAverage); // Obtain target average
    t_sum := t_sum / APreviousBlocksAverage; // time average
    t := Round(t_sum);
    if (t <> 0) then
    begin
      bn_sum.Divide(t);
    end;
    Result := bn_sum.Divide(1024); // Value in Kh/s
  finally
    FLock.Release;
  end;
end;

function TAccountStorage.CalculateHash: TRawBytes;
begin
  if (FBufferBlocksHash = '') then
    Result := TCrypto.DoSha256(cGenesisBlockMagic)
  else begin
    Result := TCrypto.DoSha256(FBufferBlocksHash);
  end;
end;

function TAccountStorage.CanUpgradeToProtocol2: Boolean;
begin
  Result := (FCurrentProtocol < cPROTOCOL_2) and (BlocksCount >= cProtocol_Upgrade_v2_MinBlock);
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
    LogDebug(Classname, 'Checked memory ' + inttostr(tc) + ' miliseonds');
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
      TMemBlockAccount(p^).Accounts[0] := Default(TMemAccount);
      TMemBlockAccount(p^).Accounts[1] := Default(TMemAccount);
      TMemBlockAccount(p^).Accounts[2] := Default(TMemAccount);
      TMemBlockAccount(p^).Accounts[3] := Default(TMemAccount);
      TMemBlockAccount(p^).Accounts[4] := Default(TMemAccount);
      TMemBlockAccount(P^).BlockChainInfo := Default(TMemTransactionBlock);
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
    FCurrentProtocol := cPROTOCOL_1;
  finally
    EndThreadSave;
  end;
end;

procedure TAccountStorage.CopyFrom(AAccounts: TAccountStorage);
var
  i, j: Cardinal;
  P: PBlockAccount;
  BA: TAccountStorageEntry;
begin
  StartThreadSafe;
  try
    AAccounts.StartThreadSafe;
    try
      if AAccounts = Self then
        exit;
      Clear;
      if AAccounts.BlocksCount > 0 then
      begin
        FBlockAccountsList.Capacity := AAccounts.BlocksCount;
        for i := 0 to AAccounts.BlocksCount - 1 do
        begin
          BA := AAccounts.GetBlock(i);
          New(P);
          ToTMemBlockAccount(BA, P^);
          FBlockAccountsList.Add(P);
          for j := low(BA.Accounts) to high(BA.Accounts) do
          begin
            if (BA.Accounts[j].Name <> '') then
              FOrderedByName.Add(BA.Accounts[j].Name, BA.Accounts[j].AccountNumber);
            AccountKeyListAddAccounts(BA.Accounts[j].AccountInfo.AccountKey, [BA.Accounts[j].AccountNumber]);
          end;
        end;
      end;
      FTotalBalance := AAccounts.TotalBalance;
      FTotalFee := AAccounts.FTotalFee;
      FBufferBlocksHash := AAccounts.FBufferBlocksHash;
      FAccountStorageHash := AAccounts.FAccountStorageHash;
      FWorkSum := AAccounts.FWorkSum;
      FCurrentProtocol := AAccounts.FCurrentProtocol;
    finally
      AAccounts.EndThreadSave;
    end;
  finally
    EndThreadSave;
  end;
end;

constructor TAccountStorage.Create;
begin
  FLock := TCriticalSection.Create();
  FBlockAccountsList := TList.Create;
  FListOfOrderedAccountKeysList := TList.Create;
  FCurrentProtocol := cPROTOCOL_1;
  FOrderedByName := TOrderedRawList.Create;
  Clear;
end;

destructor TAccountStorage.Destroy;
var
  i: Integer;
begin
  Clear;
  for i := 0 to FListOfOrderedAccountKeysList.Count - 1
  do TOrderedAccountKeysList(FListOfOrderedAccountKeysList[i]).AccountStorage := nil;
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
  TLog.NewLog(ltInfo, Classname, 'Start Upgrade to protocol 2 - Old Hash:' + TBaseType.ToHexaString(FAccountStorageHash) +
    ' calculated: ' + TBaseType.ToHexaString(aux) + ' Blocks: ' + inttostr(BlocksCount));
  FBufferBlocksHash := '';
  for block_number := 0 to BlocksCount - 1 do
  begin
{$IFDEF uselowmem}
    TBaseType.To32Bytes(blocks[block_number].CalcBlockHash(true), PBlockAccount(FBlockAccountsList.Items[block_number])
      ^.BlockHash);
    FBufferBlocksHash := FBufferBlocksHash + TBaseType.ToRawBytes(PBlockAccount(FBlockAccountsList.Items[block_number])
      ^.BlockHash);
{$ELSE}
    PBlockAccount(FBlockAccountsList.Items[block_number])^.block_hash := CalcBlockHash(block(block_number), true);
    FBufferBlocksHash := FBufferBlocksHash + PBlockAccount(FBlockAccountsList.Items[block_number])^.block_hash;
{$ENDIF}
  end;
  FAccountStorageHash := CalculateHash;
  FCurrentProtocol := cPROTOCOL_2;
  Result := true;
  TLog.NewLog(ltInfo, Classname, 'End Upgraded to protocol 2 - New hash:' + TBaseType.ToHexaString(FAccountStorageHash));
end;

procedure TAccountStorage.EndThreadSave;
begin
  FLock.Release;
end;

function TAccountStorage.LoadFromStream(AStream: TStream; ACheckAll: Boolean; var ALastReadBlock: TAccountStorageEntry;
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
begin
  StartThreadSafe;
  try
    Clear;
    Result := false;
    try
      try
       sbHeader := TAccountStorageHeader.LoadFromStream(AStream);
      except on E:Exception do begin
          errors := 'Invalid stream. Invalid header/version';
          exit;
        end;
      end;
      errors := 'Invalid version or corrupted stream';
      FCurrentProtocol := sbHeader.Protocol;
      if (sbHeader.BlocksCount = 0) or (sbHeader.StartBlock <> 0) or (sbHeader.EndBlock <> (sbHeader.BlocksCount - 1))
      then begin
        errors := Format('Stream contains blocks from %d to %d (of %d blocks). Not valid',
          [sbHeader.StartBlock, sbHeader.EndBlock, sbHeader.BlocksCount]);
        exit;
      end;
      // Offset zone
      posOffsetZone := AStream.Position;

      if ACheckAll
      then begin
        SetLength(offsets, sbHeader.BlocksCount + 1); // Last offset = End of blocks
        AStream.Read(offsets[0], 4 * (sbHeader.BlocksCount + 1));
      end else begin
        nPos := AStream.Position + ((sbHeader.BlocksCount + 1) * 4);
        if AStream.Size < nPos
        then exit;
        AStream.Position := nPos;
      end;
      // Build 1.3.0 to increase reading speed:
      FBlockAccountsList.Capacity := sbHeader.BlocksCount;
      SetLength(FBufferBlocksHash, sbHeader.BlocksCount * 32); // Initialize for high speed reading
      errors := 'Corrupted stream';
      for iblock := 0 to sbHeader.BlocksCount - 1 do
      begin
        errors := 'Corrupted stream reading block blockchain ' + inttostr(iblock + 1) + '/' + inttostr(sbHeader.BlocksCount);

        if (ACheckAll) then
        begin
          if (offsets[iblock] <> AStream.Position - posOffsetZone) then
          begin
            errors := errors + Format(' - offset[%d]:%d <> %d Position:%d offset:%d',
              [iblock, offsets[iblock], AStream.Position - posOffsetZone, AStream.Position, posOffsetZone]);
            exit;
          end;
        end;

        if not TBlockHeader.LoadFromStream(AStream, block.BlockHeader)
        then exit;

        if block.BlockHeader.block <> iblock
        then exit;

        for iacc := low(block.Accounts) to high(block.Accounts) do
        begin
          errors := 'Corrupted stream reading account ' + inttostr(iacc + 1) + '/' + inttostr(length(block.Accounts)) +
            ' of block ' + inttostr(iblock + 1) + '/' + inttostr(sbHeader.BlocksCount);
          if not TAccount.LoadFromStream(AStream, block.Accounts[iacc], FCurrentProtocol)
          then exit;
          // check valid
          if (block.Accounts[iacc].Name <> '') then
          begin
            if FOrderedByName.IndexOf(block.Accounts[iacc].Name) >= 0 then
            begin
              errors := errors + ' Duplicate name "' + block.Accounts[iacc].Name + '"';
              exit;
            end;
            if not TAccountStorage.IsValidAccountName(block.Accounts[iacc].Name, s) then
            begin
              errors := errors + ' > Invalid name "' + block.Accounts[iacc].Name + '": ' + s;
              exit;
            end;
            FOrderedByName.Add(block.Accounts[iacc].Name, block.Accounts[iacc].AccountNumber);
          end;

          if ACheckAll
          then begin
            if not block.Accounts[iacc].AccountInfo.IsValid(s) then
            begin
              errors := errors + ' > ' + s;
              exit;
            end;
          end;

          inc(FTotalBalance, block.Accounts[iacc].Balance);

        end;

        errors := 'Corrupted stream reading block ' + inttostr(iblock + 1) + '/' + inttostr(sbHeader.BlocksCount);
        if AStream.ReadAnsiString(block.BlockHash) < 0
        then exit;
        if AStream.Read(block.AccumulatedWork, Sizeof(block.AccumulatedWork)) < Sizeof(block.AccumulatedWork)
        then exit;
        if ACheckAll
        then begin
          // Check is valid:
          // STEP 1: Validate the block
          if not IsValidNewBlockHeader(block.BlockHeader, false, s)
          then begin
            errors := errors + ' > ' + s;
            exit;
          end;
          // STEP 2: Check if valid block hash
          if block.CalcBlockHash(FCurrentProtocol >= cPROTOCOL_2) <> block.BlockHash
          then begin
            errors := errors + ' > Invalid block hash ' + inttostr(iblock + 1) + '/' + inttostr(sbHeader.BlocksCount);
            exit;
          end;
          // STEP 3: Check accumulatedWork
          if (iblock > 0)
          then begin
            if (Blocks[iblock - 1].AccumulatedWork) + block.BlockHeader.compact_target <> block.AccumulatedWork
            then begin
              errors := errors + ' > Invalid accumulatedWork';
              exit;
            end;
          end;
        end;
        // Add
        New(P);
        ToTMemBlockAccount(block, P^);
        FBlockAccountsList.Add(P);
        for j := low(block.Accounts) to high(block.Accounts)
        do begin
          AccountKeyListAddAccounts(block.Accounts[j].AccountInfo.AccountKey, [block.Accounts[j].AccountNumber]);
        end;
        // BufferBlocksHash fill with data
        j := (length(P^.BlockHash) * (iblock));
        for i := 1 to length(P^.BlockHash) do
        begin
{$IFDEF FPC}
          FBufferBlocksHash[i + j] := AnsiChar(P^.BlockHash[i - (low(FBufferBlocksHash) - low(P^.BlockHash))]);
{$ELSE}
          FBufferBlocksHash[i + j] := AnsiChar(P^.BlockHash[i - {$IFDEF uselowmem}1{$ELSE}0{$ENDIF}]);
{$ENDIF}
        end;
        ALastReadBlock := block;
        inc(FWorkSum, block.BlockHeader.compact_target);
      end;
      if ACheckAll then
      begin
        if (offsets[sbHeader.BlocksCount] <> 0) and (offsets[sbHeader.BlocksCount] <> AStream.Position - posOffsetZone)
        then
        begin
          errors := errors + Format(' - Final offset[%d]=%d <> Eof Position:%d offset:%d',
            [sbHeader.BlocksCount, offsets[sbHeader.BlocksCount], AStream.Position - posOffsetZone, posOffsetZone]);
          exit;
        end;
      end;
      // Finally load SafeBoxHash
      if AStream.ReadAnsiString(savedSBH) < 0 then
      begin
        errors := 'No Hash value';
        exit;
      end;
      // Check worksum value
      if sbHeader.BlocksCount > 0 then
      begin
        if (FWorkSum <> Blocks[sbHeader.BlocksCount - 1].AccumulatedWork) then
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
        errors := 'Invalid Hash value in stream ' + TBaseType.ToHexaString(FAccountStorageHash) + '<>' +
          TBaseType.ToHexaString(savedSBH) + ' Last block:' + inttostr(ALastReadBlock.BlockHeader.block);
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
{
class function TAccountStorage.LoadHeaderFromStream(AStream: TStream; var AAccountStorageHeader: TAccountStorageHeader): Boolean;
// This function reads SafeBox stream info and sets position at offset start zone if valid, otherwise sets position to actual position
var
  w: Word;
  s: AnsiString;
  AccountStorageVersion: Word;
  offsetPos, initialPos: Int64;
  endBlocks: Cardinal;
begin
  Result := false;
  if (AStream.Size = 0) then
  begin
    Result := true;
    exit;
  end;
  AAccountStorageHeader := CT_AccountStorageHeader_NUL;
  initialPos := AStream.Position;
  try
    TStreamOp.ReadAnsiString(AStream, s);
    if (s <> cMagicID) then
      exit;
    if AStream.Size < 8 then
      exit;
    AStream.Read(w, Sizeof(w));
    if not(w in [1, 2]) then
      exit;
    AAccountStorageHeader.Protocol := w;
    AStream.Read(AccountStorageVersion, 2);
    if AccountStorageVersion <> cAccountStorageVersion then
      exit;
    AStream.Read(AAccountStorageHeader.BlocksCount, 4);
    AStream.Read(AAccountStorageHeader.StartBlock, 4);
    AStream.Read(AAccountStorageHeader.EndBlock, 4);
    if (AAccountStorageHeader.BlocksCount <= 0) or (AAccountStorageHeader.BlocksCount > (cBlockTime * 2000000)) then
      exit; // Protection for corrupted data...
    offsetPos := AStream.Position;
    // Go to read SafeBoxHash
    if (AStream.Size < offsetPos + (((AAccountStorageHeader.EndBlock - AAccountStorageHeader.StartBlock) + 2) * 4)) then
      exit;
    AStream.Position := offsetPos + (((AAccountStorageHeader.EndBlock - AAccountStorageHeader.StartBlock) + 1) * 4);
    AStream.Read(endBlocks, 4);
    // Go to end
    if (AStream.Size < offsetPos + (endBlocks)) then
      exit;
    AStream.Position := offsetPos + endBlocks;
    if TStreamOp.ReadAnsiString(AStream, AAccountStorageHeader.AccountStorageHash) < 0 then
      exit;
    // Back
    AStream.Position := offsetPos;
    Result := true;
  finally
    if not Result then
      AStream.Position := initialPos;
  end;
end;
}
class function TAccountStorage.SaveHeaderToStream(AStream: TStream; AProtocol: Word;
  AOffsetStartBlock, AOffsetEndBlock, ACurrentBlocksCount: Cardinal): Boolean;
var
  c: Cardinal;
begin
  // Header zone
  AStream.WriteAnsiString(cMagicID);
  AStream.Write(AProtocol, Sizeof(AProtocol));
  AStream.Write(cAccountStorageVersion, Sizeof(cAccountStorageVersion));
  c := ACurrentBlocksCount;
  AStream.Write(c, Sizeof(c)); // Save Total blocks of the safebox
  c := AOffsetStartBlock;
  AStream.Write(c, Sizeof(c)); // Save first block saved
  c := AOffsetEndBlock;
  AStream.Write(c, Sizeof(c)); // Save last block saved
  Result := true;
end;

class function TAccountStorage.MustSaved(ABlocksCount: Cardinal): Boolean;
begin
  Result := (ABlocksCount mod cSaveAccountStorageOnBlocks) = 0;
end;

procedure TAccountStorage.SaveEntryToStream(AStream: TStream; ANBlock: Cardinal);
var
  b: TAccountStorageEntry;
  iacc: Integer;
  ws: UInt64;
begin
  b := Blocks[ANBlock];
  b.BlockHeader.SaveToStream(AStream);
  for iacc := low(b.Accounts) to high(b.Accounts) do
  begin
    AStream.Write(b.Accounts[iacc].AccountNumber, Sizeof(b.Accounts[iacc].AccountNumber));
    AStream.WriteAnsiString(b.Accounts[iacc].AccountInfo.ToRawString);
    AStream.Write(b.Accounts[iacc].Balance, Sizeof(b.Accounts[iacc].Balance));
    AStream.Write(b.Accounts[iacc].UpdatedBlock, Sizeof(b.Accounts[iacc].UpdatedBlock));
    AStream.Write(b.Accounts[iacc].NumberOfTransactions, Sizeof(b.Accounts[iacc].NumberOfTransactions));
    if FCurrentProtocol >= cPROTOCOL_2 then
    begin
      AStream.WriteAnsiString(b.Accounts[iacc].Name);
      AStream.Write(b.Accounts[iacc].AccountType, Sizeof(b.Accounts[iacc].AccountType));
    end;
    AStream.Write(b.Accounts[iacc].PreviusUpdatedBlock, Sizeof(b.Accounts[iacc].PreviusUpdatedBlock));
  end;
  AStream.WriteAnsiString(b.BlockHash);
  AStream.Write(b.AccumulatedWork, Sizeof(b.AccumulatedWork));
end;

procedure TAccountStorage.SaveToStream(AStream: TStream; FromBlock, AToBlock: Cardinal);
var
  totalBlocks, iblock: Cardinal;
  b: TAccountStorageEntry;
  posOffsetZone, posFinal: Int64;
  offsets: {$ifdef USE_GENERICS}TArray<Cardinal>{$else}TCardinalArray{$endif};
  raw: TRawBytes;
begin
  if (FromBlock > AToBlock) or (AToBlock >= BlocksCount) then
    raise Exception.Create(Format('Cannot save account storage from %d to %d (currently %d blocks)',
      [FromBlock, AToBlock, BlocksCount]));
  StartThreadSafe;
  try
    // Header zone
    SaveHeaderToStream(AStream, FCurrentProtocol, FromBlock, AToBlock, BlocksCount);
    totalBlocks := AToBlock - FromBlock + 1;
    // Offsets zone
    posOffsetZone := AStream.Position;
    SetLength(raw, (totalBlocks + 1) * 4); // Last position = end
    FillChar(raw[1], length(raw), 0);
    AStream.WriteBuffer(raw[1], length(raw));
    SetLength(offsets, totalBlocks + 1); // c = total blocks  - Last position = offset to end
    // Blocks zone
    for iblock := FromBlock to AToBlock do
    begin
      offsets[iblock] := AStream.Position - posOffsetZone;
      SaveEntryToStream(AStream, iblock);
    end;
    offsets[high(offsets)] := AStream.Position - posOffsetZone;
    // Save offsets zone with valid values
    posFinal := AStream.Position;
    AStream.Position := posOffsetZone;
    for iblock := FromBlock to AToBlock + 1 do
    begin
      AStream.Write(offsets[iblock], Sizeof(offsets[iblock]));
    end;
    AStream.Position := posFinal;
    // Final zone: Save safeboxhash for next block
    if (AToBlock + 1 < BlocksCount) then
    begin
      b := Blocks[AToBlock];
      AStream.WriteAnsiString(b.BlockHeader.initial_safe_box_hash);
    end
    else
    begin
      AStream.WriteAnsiString(FAccountStorageHash);
    end;
  finally
    EndThreadSave;
  end;
end;

class function TAccountStorage.CopyChunk(ASource, ADestination: TStream; AFromBlock, AToBlock: Cardinal;
  var RErrors: AnsiString): Boolean;
var
  iblock: Cardinal;
  raw: TRawBytes;
  posOffsetZoneSource, posOffsetZoneDest, posFinal, posBlocksZoneDest, posInitial: Int64;
  offsetsSource, offsetsDest: {$ifdef USE_GENERICS}TArray<Cardinal>{$else}TCardinalArray{$endif};
  destTotalBlocks: Cardinal;
  sbHeader: TAccountStorageHeader;
begin
  Result := false;
  RErrors := '';
  posInitial := ASource.Position;
  try
    if (AFromBlock > AToBlock) then
    begin
      RErrors := Format('Invalid CopyStream(from %d, to %d)', [AFromBlock, AToBlock]);
      exit;
    end;
    try
      sbHeader := TAccountStorageHeader.LoadFromStream(ASource);
    except on e:exception do begin
        RErrors := 'Invalid stream. Invalid header/version';
        exit;
      end;
    end;
    if (sbHeader.StartBlock > AFromBlock) or (sbHeader.EndBlock < AToBlock) or
      ((sbHeader.StartBlock + sbHeader.BlocksCount) < AToBlock) then
    begin
      RErrors := Format('Stream contain blocks from %d to %d (of %d). Need between %d and %d !',
        [sbHeader.StartBlock, sbHeader.EndBlock, sbHeader.BlocksCount, AFromBlock, AToBlock]);
      exit;
    end;
    destTotalBlocks := AToBlock - AFromBlock + 1;
    TLog.NewLog(ltInfo, Classname,
      Format('Copy Stream from AccountStorage with %d to %d (of %d sbh:%s) to AccountStorage with %d and %d',
      [sbHeader.StartBlock, sbHeader.EndBlock, sbHeader.BlocksCount, TBaseType.ToHexaString(sbHeader.AccountStorageHash),
      AFromBlock, AToBlock]));
    // Read Source Offset zone
    posOffsetZoneSource := ASource.Position;
    SetLength(offsetsSource, (sbHeader.EndBlock - sbHeader.StartBlock) + 2);
    ASource.Read(offsetsSource[0], 4 * length(offsetsSource));
    // DEST STREAM:
    // Init dest stream
    // Header zone
    SaveHeaderToStream(ADestination, sbHeader.Protocol, AFromBlock, AToBlock, sbHeader.BlocksCount);
    // Offsets zone
    posOffsetZoneDest := ADestination.Position;
    SetLength(raw, (destTotalBlocks + 1) * 4); // Cardinal = 4 bytes for each block + End position
    FillChar(raw[1], length(raw), 0);
    ADestination.WriteBuffer(raw[1], length(raw));
    SetLength(offsetsDest, destTotalBlocks + 1);
    // Blocks zone
    posBlocksZoneDest := ADestination.Position;
    TLog.NewLog(ltInfo, Classname,
      Format('Copying AccountStorage Stream from source Position %d (size:%d) to dest %d bytes - OffsetSource[%d] - OffsetSource[%d]',
      [posOffsetZoneSource + offsetsSource[AFromBlock - sbHeader.StartBlock], ASource.Size,
      offsetsSource[AToBlock - sbHeader.StartBlock + 1] - offsetsSource[AFromBlock - sbHeader.StartBlock],
      AToBlock - sbHeader.StartBlock + 1, AFromBlock - sbHeader.StartBlock]));

    ASource.Position := posOffsetZoneSource + offsetsSource[AFromBlock - sbHeader.StartBlock];
    ADestination.CopyFrom(ASource, offsetsSource[AToBlock - sbHeader.StartBlock + 1] - offsetsSource
      [AFromBlock - sbHeader.StartBlock]);
    // Save offsets zone with valid values
    posFinal := ADestination.Position;
    ADestination.Position := posOffsetZoneDest;
    for iblock := AFromBlock to AToBlock do
    begin
      offsetsDest[iblock - AFromBlock] := offsetsSource[iblock - (sbHeader.StartBlock)] -
        offsetsSource[AFromBlock - sbHeader.StartBlock] + (posBlocksZoneDest - posOffsetZoneDest);
    end;
    offsetsDest[high(offsetsDest)] := posFinal - posOffsetZoneDest;

    ADestination.WriteBuffer(offsetsDest[0], length(offsetsDest) * 4);
    ADestination.Position := posFinal;
    ASource.Position := offsetsSource[high(offsetsSource)] + posOffsetZoneSource;
    ASource.ReadAnsiString(raw);
    ADestination.WriteAnsiString(raw);
    Result := true;
  finally
    ASource.Position := posInitial;
  end;
end;

class function TAccountStorage.ConcatStream(ASource1, ASource2, ADestionation: TStream; var RErrors: AnsiString): Boolean;
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
      stream.Seek(4 * offsetIndex, soCurrent);
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
        targetStream.Seek(4 * (offsetIndex), soCurrent);
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
  RErrors := '';
  source1InitialPos := ASource1.Position;
  source2InitialPos := ASource2.Position;
  try

    try
      s1Header := TAccountStorageHeader.LoadFromStream(ASource1);
    except on e:exception do begin
        RErrors := 'Invalid source 1 stream. Invalid header/version';
        exit;
      end;
    end;

    try
      s2Header := TAccountStorageHeader.LoadFromStream(ASource2);
    except on e:exception do begin
        RErrors := 'Invalid source 2 stream. Invalid header/version';
        exit;
      end;
    end;
    // Check SBH and blockcount
    if (s1Header.AccountStorageHash <> s2Header.AccountStorageHash) or (s1Header.BlocksCount <> s2Header.BlocksCount) or
      (s1Header.Protocol <> s2Header.Protocol) then
    begin
      RErrors := Format('Source1 and Source2 have diff. Source 1 %d %s (protocol %d) Source 2 %d %s (protocol %d)',
        [s1Header.BlocksCount, TBaseType.ToHexaString(s1Header.AccountStorageHash), s1Header.Protocol,
        s2Header.BlocksCount, TBaseType.ToHexaString(s2Header.AccountStorageHash), s2Header.Protocol]);
      exit;
    end;
    // Save dest heaer
    destStartBlock := MinCardinal(s1Header.StartBlock, s2Header.StartBlock);
    destEndBlock := MaxCardinal(s1Header.EndBlock, s2Header.EndBlock);
    SaveHeaderToStream(ADestionation, s1Header.Protocol, destStartBlock, destEndBlock, s1Header.BlocksCount);
    // Save offsets
    destOffsetPos := ADestionation.Position;
    SetLength(destOffsets, ((destEndBlock - destStartBlock) + 2));
    for i := low(destOffsets) to high(destOffsets) do
      destOffsets[i] := 0;
    ADestionation.Write(destOffsets[0], ((destEndBlock - destStartBlock) + 2) * 4);
    ADestionation.Position := destOffsetPos;
    //
    ms := TMemoryStream.Create;
    try
      for nBlock := destStartBlock to destEndBlock do
      begin
        ms.Clear;
        if (nBlock >= s1Header.StartBlock) and (nBlock <= s1Header.EndBlock) then
        begin
          c := ReadAccountStorageEntryFromStream(ASource1, nBlock - s1Header.StartBlock, ms);
          ms.Position := 0;
          WriteEntryToStream(ms, ADestionation, c, nBlock - destStartBlock, destEndBlock - destStartBlock + 1);
        end
        else if (nBlock >= s2Header.StartBlock) and (nBlock <= s2Header.EndBlock) then
        begin
          c := ReadAccountStorageEntryFromStream(ASource2, nBlock - s2Header.StartBlock, ms);
          ms.Position := 0;
          WriteEntryToStream(ms, ADestionation, c, nBlock - destStartBlock, destEndBlock - destStartBlock + 1);
        end
        else
          raise Exception.Create('ERROR DEV 20170518-1');
      end;
    finally
      ms.Free;
    end;
    // Save SafeBoxHash at the end
    ADestionation.Seek(0, soFromEnd);
    ADestionation.WriteAnsiString(s1Header.AccountStorageHash);
    Result := true;
  finally
    ASource1.Position := source1InitialPos;
    ASource2.Position := source2InitialPos;
  end;
end;

class function TAccountStorage.IsValidAccountName(const ANewName: TRawBytes; var RErrors: AnsiString): Boolean;
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
  RErrors := '';
  if (length(ANewName) < CT_MicroCoin_name_min_length) or (length(ANewName) > CT_MicroCoin_name_max_length) then
  begin
    RErrors := 'Invalid length:' + inttostr(length(ANewName)) + ' (valid from ' + inttostr(CT_MicroCoin_name_max_length)
      + ' to ' + inttostr(CT_MicroCoin_name_max_length) + ')';
    exit;
  end;
  for i := 1 to length(ANewName) do
  begin
    j := 1;
    if (i = 1) then
    begin
      // First char can't start with a number
      while (j <= length(CT_MicroCoin_FirstChar_Charset)) and (ANewName[i] <> CT_MicroCoin_FirstChar_Charset[j]) do
        inc(j);
      if j > length(CT_MicroCoin_FirstChar_Charset) then
      begin
        RErrors := 'Invalid char ' + ANewName[i] + ' at first pos';
        exit; // Not found
      end;
    end
    else
    begin
      while (j <= length(CT_MicroCoin_Base64_Charset)) and (ANewName[i] <> CT_MicroCoin_Base64_Charset[j]) do
        inc(j);
      if j > length(CT_MicroCoin_Base64_Charset) then
      begin
        RErrors := 'Invalid char ' + ANewName[i] + ' at pos ' + inttostr(i);
        exit; // Not found
      end;
    end;
  end;
  Result := true;
end;

function TAccountStorage.IsValidNewBlockHeader(const ANewOperationBlock: TBlockHeader; AValidateHash: Boolean;
  var RErrors: AnsiString): Boolean;
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
  RErrors := '';
  if BlocksCount > 0
  then lastBlock := Blocks[BlocksCount - 1].BlockHeader
  else lastBlock := TBlockHeader.Empty;
  // Check block
  if (BlocksCount <> ANewOperationBlock.block) then
  begin
    RErrors := 'block (' + inttostr(ANewOperationBlock.block) + ') is not new position (' + inttostr(BlocksCount) + ')';
    exit;
  end;
  // Check Account key
  if not ANewOperationBlock.account_key.IsValidAccountKey(RErrors) then
  begin
    exit;
  end;
  // reward
  if (ANewOperationBlock.reward <> TMicroCoinProtocol.GetRewardForNewLine(ANewOperationBlock.block)) then
  begin
    RErrors := 'Invalid reward';
    exit;
  end;
  // fee: Cannot be checked only with the safebox
  // protocol available is not checked
  if (ANewOperationBlock.block > 0) then
  begin
    // protocol
    if (ANewOperationBlock.protocol_version <> CurrentProtocol) then
    begin
      // Protocol must be 1 or 2. If 1 then all prior blocksmust be 1 and never 2 (invalide blockchain version scenario v1...v2...v1)
      if (lastBlock.protocol_version > ANewOperationBlock.protocol_version) then
      begin
        RErrors := 'Invalid MicroCoin protocol version: ' + inttostr(ANewOperationBlock.protocol_version) + ' Current: ' +
          inttostr(CurrentProtocol) + ' Previous:' + inttostr(lastBlock.protocol_version);
        exit;
      end;
      if (ANewOperationBlock.protocol_version = cPROTOCOL_2) then
      begin
        if (ANewOperationBlock.block < cProtocol_Upgrade_v2_MinBlock) then
        begin
          RErrors := 'Upgrade to protocol version 2 available at block: ' + inttostr(cProtocol_Upgrade_v2_MinBlock);
          exit;
        end;
      end
      else if (ANewOperationBlock.protocol_version <> cPROTOCOL_1) then
      begin
        RErrors := 'Invalid protocol version change to ' + inttostr(ANewOperationBlock.protocol_version);
        exit;
      end;
    end
    else if (not(ANewOperationBlock.protocol_version in [cPROTOCOL_1, cPROTOCOL_2])) then
    begin
      RErrors := 'Invalid protocol version ' + inttostr(ANewOperationBlock.protocol_version);
      exit;
    end;
    // timestamp
    if ((ANewOperationBlock.timestamp) < (lastBlock.timestamp)) then
    begin
      RErrors := 'Invalid timestamp (Back timestamp: New timestamp:' + inttostr(ANewOperationBlock.timestamp) +
        ' < last timestamp (' + inttostr(BlocksCount - 1) + '):' + inttostr(lastBlock.timestamp) + ')';
      exit;
    end;
  end
  else
  begin
    if (cGenesisBlockPoW <> '') then
    begin
      // Check if valid Zero block
      if not(AnsiSameText(TBaseType.ToHexaString(ANewOperationBlock.proof_of_work), cGenesisBlockPoW))
      then
      begin
        RErrors := 'Zero block not valid, Proof of Work invalid: ' +
          TBaseType.ToHexaString(ANewOperationBlock.proof_of_work) + '<>' + cGenesisBlockPoW;
        exit;
      end;
    end;
  end;
  // compact_target
  target_hash := GetActualTargetHash(ANewOperationBlock.protocol_version = cPROTOCOL_2);
  if (ANewOperationBlock.compact_target <> TMicroCoinProtocol.TargetToCompact(target_hash)) then
  begin
    RErrors := 'Invalid target found:' + IntToHex(ANewOperationBlock.compact_target, 8) + ' actual:' +
      IntToHex(TMicroCoinProtocol.TargetToCompact(target_hash), 8);
    exit;
  end;
  // nonce: Not checked
  // block_payload: Checking Miner payload size
  if length(ANewOperationBlock.block_payload) > cMaxPayloadSize then
  begin
    RErrors := 'Invalid Miner Payload length: ' + inttostr(length(ANewOperationBlock.block_payload));
    exit;
  end;
  // Checking Miner Payload valid chars
  for i := 1 to length(ANewOperationBlock.block_payload) do
  begin
    if not(ANewOperationBlock.block_payload[i] in [#32 .. #254]) then
    begin
      RErrors := 'Invalid Miner Payload character at pos ' + inttostr(i) + ' value:' +
        inttostr(ord(ANewOperationBlock.block_payload[i]));
      exit;
    end;
  end;
  // initial_safe_box_hash: Only can be checked when adding new blocks, not when restoring a safebox
  if AValidateHash then
  begin
    // TODO: Can use FSafeBoxHash instead of CalcSafeBoxHash ???? Quick speed if possible
    if (ANewOperationBlock.initial_safe_box_hash <> FAccountStorageHash) then
    begin
      RErrors := 'BlockChain Safe box hash invalid: ' + TBaseType.ToHexaString(ANewOperationBlock.initial_safe_box_hash) +
        ' var: ' + TBaseType.ToHexaString(FAccountStorageHash) + ' Calculated:' + TBaseType.ToHexaString(CalculateHash);
      exit;
    end;
  end;
  // operations_hash: NOT CHECKED WITH OPERATIONS!
  if (length(ANewOperationBlock.transactionHash) <> 32) then
  begin
    RErrors := 'Invalid Operations hash value: ' + TBaseType.ToHexaString(ANewOperationBlock.transactionHash) + ' length=' +
      inttostr(length(ANewOperationBlock.transactionHash));
    exit;
  end;
  // proof_of_work:
  TMicroCoinProtocol.CalcProofOfWork(ANewOperationBlock, PoW);
  if (PoW <> ANewOperationBlock.proof_of_work) then
  begin
//  ANewOperationBlock := Pow;
    RErrors := 'Proof of work is bad calculated ' + TBaseType.ToHexaString(ANewOperationBlock.proof_of_work) + ' <> Good: '
      + TBaseType.ToHexaString(PoW);
    exit;
  end;
  if (ANewOperationBlock.proof_of_work > target_hash) then
  begin
    RErrors := 'Proof of work is higher than target ' + TBaseType.ToHexaString(ANewOperationBlock.proof_of_work) + ' > ' +
      TBaseType.ToHexaString(target_hash);
    exit;
  end;
  Result := true;
end;

function TAccountStorage.GetActualTargetHash(AUseProtocolV2: Boolean): TRawBytes;
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
  if (BlocksCount <= 1) then
  begin
    // Important: CT_MinCompactTarget is applied for blocks 0 until ((CT_CalcNewDifficulty*2)-1)
    Result := TMicroCoinProtocol.TargetFromCompact(cMinimumDifficulty);
  end
  else
  begin
    if BlocksCount > cDifficultyCalcBlocks then
      CalcBack := cDifficultyCalcBlocks
    else
      CalcBack := BlocksCount - 1;
    lastBlock := Blocks[BlocksCount - 1].BlockHeader;
    // Calc new target!
    ts1 := lastBlock.timestamp;
    ts2 := Blocks[BlocksCount - CalcBack - 1].BlockHeader.timestamp;
    tsTeorical := (CalcBack * cBlockTime);
    tsReal := (ts1 - ts2);
    if (not AUseProtocolV2) then
    begin
      Result := TMicroCoinProtocol.GetNewTarget(tsTeorical, tsReal,
        TMicroCoinProtocol.TargetFromCompact(lastBlock.compact_target));
    end
    else
    begin
      CalcBack := CalcBack div cDifficultyCalcBlocks_SPLIT;
      if CalcBack = 0 then
        CalcBack := 1;
      ts2 := Blocks[BlocksCount - CalcBack - 1].BlockHeader.timestamp;
      tsTeoricalStop := (CalcBack * cBlockTime);
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

function TAccountStorage.GetActualCompactTargetHash(AUseProtocolV2: Boolean): Cardinal;
begin
  Result := TMicroCoinProtocol.TargetToCompact(GetActualTargetHash(AUseProtocolV2));
end;

function TAccountStorage.FindAccountByName(AName: AnsiString): Integer;
var
  nameLower: AnsiString;
  i: Integer;
begin
  nameLower := LowerCase(AName);
  i := FOrderedByName.IndexOf(AName);
  if i >= 0 then
    Result := FOrderedByName.GetTag(i)
  else
    Result := -1;
end;

procedure TAccountStorage.UpdateAccount(AAccountNumber: Cardinal; const ANewAccountInfo: TAccountInfo;
  const ANewName: TRawBytes; ANewType: Word; ANewBalance: UInt64; ANewNumberOfTransactions: Cardinal);
var
  iblock: Cardinal;
  i, j, iAccount: Integer;
  lastbalance: UInt64;
  acc: TAccount;
  bacc: TAccountStorageEntry;
  P: PBlockAccount;
begin
  iblock := AAccountNumber div cAccountsPerBlock;
  iAccount := AAccountNumber mod cAccountsPerBlock;
  acc := Accounts[AAccountNumber];
  P := FBlockAccountsList.Items[iblock];

  if (acc.AccountInfo.AccountKey <> ANewAccountInfo.AccountKey) then
  begin
    AccountKeyListRemoveAccount(acc.AccountInfo.AccountKey, [AAccountNumber]);
    AccountKeyListAddAccounts(ANewAccountInfo.AccountKey, [AAccountNumber]);
  end;

  acc.AccountInfo := ANewAccountInfo;
  // Name:
  if acc.Name <> ANewName then
  begin
    if acc.Name <> '' then
    begin
      i := FOrderedByName.IndexOf(acc.Name);
      if i < 0 then
        TLog.NewLog(ltError, Classname, 'ERROR DEV 20170606-1')
      else
        FOrderedByName.Delete(i);
    end;
    acc.Name := ANewName;
    if acc.Name <> '' then
    begin
      i := FOrderedByName.IndexOf(acc.Name);
      if i >= 0 then
        TLog.NewLog(ltError, Classname, 'ERROR DEV 20170606-2')
      else
        FOrderedByName.Add(acc.Name, AAccountNumber);
    end;
  end;
  acc.AccountType := ANewType;
  lastbalance := acc.Balance;
  acc.Balance := ANewBalance;
  // Will update previous_updated_block only on first time/block
  if acc.UpdatedBlock <> BlocksCount then
  begin
    acc.PreviusUpdatedBlock := acc.UpdatedBlock;
    acc.UpdatedBlock := BlocksCount;
  end;
  acc.NumberOfTransactions := ANewNumberOfTransactions;
  ToTMemAccount(acc, P^.Accounts[iAccount]);
  bacc := Blocks[iblock];

{$IFDEF uselowmem}
  TBaseType.To32Bytes(bacc.CalcBlockHash(FCurrentProtocol >= cPROTOCOL_2), P^.BlockHash);
{$ELSE}
  P^.block_hash := CalcBlockHash(bacc, FCurrentProtocol >= CT_PROTOCOL_2);
{$ENDIF}
  // Update buffer block hash
  j := (length(P^.BlockHash) * (iblock));
  for i := 1 to length(P^.BlockHash) do
  begin
{$IFDEF FPC}
    FBufferBlocksHash[i + j] := AnsiChar(P^.BlockHash[i - (low(FBufferBlocksHash) - low(P^.BlockHash))]);
{$ELSE}
    FBufferBlocksHash[i + j] := AnsiChar(P^.BlockHash[i - {$IFDEF uselowmem}1{$ELSE}0{$ENDIF}]);
{$ENDIF}
  end;

  FTotalBalance := FTotalBalance - (Int64(lastbalance) - Int64(ANewBalance));
  FTotalFee := FTotalFee + (Int64(lastbalance) - Int64(ANewBalance));
end;

procedure TAccountStorage.StartThreadSafe;
begin
  TPCThread.ProtectEnterCriticalSection(Self, FLock);
end;

type
  TOrderedAccountKeyList = record
    RawAccountKey: TRawBytes;
    AccountNumbers: TOrderedList;
  end;

  POrderedAccountKeyList = ^TOrderedAccountKeyList;

function SortOrdered(AItem1, AItem2: Pointer): Integer;
begin
  Result := PtrInt(AItem1) - PtrInt(AItem2);
end;

procedure TOrderedAccountKeysList.AddAccountKey(const AAccountKey: TAccountKey);
var
  P: POrderedAccountKeyList;
  i, j: Integer;
begin
  if not Find(AAccountKey, i) then
  begin
    New(P);
    P^.RawAccountKey := AAccountKey.ToRawString;
    P^.AccountNumbers := TOrderedList.Create;
    FOrderedAccountKeysList.Insert(i, P);
    // Search this key in the AccountsList and add all...
    j := 0;
    if Assigned(FAccountStorage) then
    begin
      for i := 0 to FAccountStorage.AccountsCount - 1 do
      begin
        if FAccountStorage.Accounts[i].AccountInfo.AccountKey = AAccountKey then
        begin
          // Note: P^.accounts will be ascending ordered due to "for i:=0 to ..."
          P^.AccountNumbers.Add(i);
        end;
      end;
      LogDebug(Classname, 'Adding account key (%d of %d) %s', [j, FAccountStorage.AccountsCount,
        TBaseType.ToHexaString(AAccountKey.ToRawString)]);
    end
    else
    begin
      LogDebug(Classname, Format('Adding account key (no Account List) %s',
        [TBaseType.ToHexaString(AAccountKey.ToRawString)]));
    end;
  end;
end;

procedure TOrderedAccountKeysList.AddAccounts(const AAccountKey: TAccountKey; const AAccounts: array of Cardinal);
var
  P: POrderedAccountKeyList;
  i: Integer;
begin
  if Find(AAccountKey, i)
  then P := POrderedAccountKeyList(FOrderedAccountKeysList[i])
  else if (FAutoAddAll)
       then begin
         New(P);
         P^.RawAccountKey := AAccountKey.ToRawString;
         P^.AccountNumbers := TOrderedList.Create;
         FOrderedAccountKeysList.Insert(i, P);
       end
       else exit;
  for i := low(AAccounts) to high(AAccounts)
  do P^.AccountNumbers.Add(AAccounts[i]);
end;

procedure TOrderedAccountKeysList.Clear;
begin
  ClearAccounts(true);
end;

procedure TOrderedAccountKeysList.ClearAccounts(ARemoveAccountList: Boolean);
var
  P: POrderedAccountKeyList;
  i: Integer;
begin
  for i := 0 to FOrderedAccountKeysList.Count - 1 do
  begin
    P := FOrderedAccountKeysList[i];
    if ARemoveAccountList then
    begin
      P^.AccountNumbers.Free;
      P^:=Default(TOrderedAccountKeyList);
      Dispose(P);
      P := nil;
    end
    else
    begin
      P^.AccountNumbers.Clear;
    end;
  end;
  if ARemoveAccountList then
  begin
    FOrderedAccountKeysList.Clear;
  end;
end;

function TOrderedAccountKeysList.Count: Integer;
begin
  Result := FOrderedAccountKeysList.Count;
end;

constructor TOrderedAccountKeysList.Create(AAccountStorage: TAccountStorage; AAutoAddAll: Boolean);
var
  i: Integer;
begin
  LogDebug(Classname, 'Creating an Ordered Account Keys List adding all:' + BooltoStr(AAutoAddAll, true));
  FAutoAddAll := AAutoAddAll;
  FAccountStorage := AAccountStorage;
  FOrderedAccountKeysList := TList.Create;
  if Assigned(AAccountStorage) then
  begin
    AAccountStorage.ListOfOrderedAccountKeysList.Add(Self);
    if AAutoAddAll then
    begin
      for i := 0 to AAccountStorage.AccountsCount - 1 do
      begin
        AddAccountKey(AAccountStorage.Accounts[i].AccountInfo.AccountKey);
      end;
    end;
  end;
end;

destructor TOrderedAccountKeysList.Destroy;
begin
  LogDebug(Classname, 'Destroying an Ordered Account Keys List adding all:' + BoolToStr(FAutoAddAll));
  if Assigned(FAccountStorage)
  then FAccountStorage.ListOfOrderedAccountKeysList.Remove(Self);
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
    c := CompareStr(POrderedAccountKeyList(FOrderedAccountKeysList[i]).RawAccountKey, rak);
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

function TOrderedAccountKeysList.GetAccountKey(AIndex: Integer): TAccountKey;
var
  raw: TRawBytes;
begin
  raw := POrderedAccountKeyList(FOrderedAccountKeysList[AIndex]).RawAccountKey;
  Result := TAccountKey.FromRawString(raw);
end;

function TOrderedAccountKeysList.GetAccountList(AIndex: Integer): TOrderedList;
begin
  Result := POrderedAccountKeyList(FOrderedAccountKeysList[AIndex]).AccountNumbers;
end;

function TOrderedAccountKeysList.IndexOfAccountKey(const AAccountKey: TAccountKey): Integer;
begin
  if not Find(AAccountKey, Result) then
    Result := -1;
end;

procedure TOrderedAccountKeysList.RemoveAccounts(const AAccountKey: TAccountKey; const AAccounts: array of Cardinal);
var
  P: POrderedAccountKeyList;
  i, j: Integer;
begin
  if not Find(AAccountKey, i) then
    exit; // Nothing to do
  P := POrderedAccountKeyList(FOrderedAccountKeysList[i]);
  for j := low(AAccounts) to high(AAccounts) do
  begin
    P^.AccountNumbers.Remove(AAccounts[j]);
  end;
  if (P^.AccountNumbers.Count = 0) and (FAutoAddAll) then
  begin
    // Remove from list
    FOrderedAccountKeysList.Delete(i);
    // Free it
    P^.AccountNumbers.Free;
    P^ := Default(TOrderedAccountKeyList);
    Dispose(P);
  end;
end;

procedure TOrderedAccountKeysList.RemoveAccountKey(const AAccountKey: TAccountKey);
var
  P: POrderedAccountKeyList;
  i: Integer;
begin
  if not Find(AAccountKey, i) then
    exit; // Nothing to do
  P := POrderedAccountKeyList(FOrderedAccountKeysList[i]);
  // Remove from list
  FOrderedAccountKeysList.Delete(i);
  // Free it
  P^.AccountNumbers.Free;
  P^ := Default(TOrderedAccountKeyList);
  Dispose(P);
end;

{ TAccountStorageEntry }

function TAccountStorageEntry.CalcBlockHash(
  AUseProtocol2Method: Boolean): TRawBytes;
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
    if (not AUseProtocol2Method) then
    begin
      // PROTOCOL 1 BlockHash calculation
      ms.Write(BlockHeader.block, 4); // Little endian
      for i := low(Accounts) to high(Accounts) do
      begin
        ms.Write(Accounts[i].AccountNumber, 4); // Little endian
        raw := Accounts[i].AccountInfo.ToRawString;
        ms.WriteBuffer(raw[1], length(raw)); // Raw bytes
        ms.Write(Accounts[i].Balance, Sizeof(UInt64)); // Little endian
        ms.Write(Accounts[i].UpdatedBlock, 4); // Little endian
        ms.Write(Accounts[i].NumberOfTransactions, 4); // Little endian
      end;
      ms.Write(BlockHeader.timestamp, 4); // Little endian
    end
    else
    begin
      // PROTOCOL 2 BlockHash calculation
      ms.SetSize(1024*4);
      BlockHeader.SaveToStream(ms);
      for i := low(Accounts) to high(Accounts) do
      begin
        ms.Write(Accounts[i].AccountNumber, 4); // Little endian
        raw := Accounts[i].AccountInfo.ToRawString;
        ms.WriteBuffer(raw[1], length(raw)); // Raw bytes
        ms.Write(Accounts[i].Balance, Sizeof(UInt64)); // Little endian
        ms.Write(Accounts[i].UpdatedBlock, 4); // Little endian
        ms.Write(Accounts[i].NumberOfTransactions, 4); // Little endian
        // Use new Protocol 2 fields
        if length(Accounts[i].Name) > 0 then
        begin
          ms.WriteBuffer(Accounts[i].Name[1], length(Accounts[i].Name));
        end;
        ms.Write(Accounts[i].AccountType, 2);
      end;
      ms.Write(AccumulatedWork, Sizeof(AccumulatedWork));
    end;
    Result := TCrypto.DoSha256(ms.Memory, ms.Position)
  finally
    ms.Free;
  end;
end;

class function TAccountStorageEntry.Empty: TAccountStorageEntry;
var
  i : integer;
begin
  Result.BlockHeader := TBlockHeader.Empty;
  Result.BlockHash := '';
  Result.AccumulatedWork := 0;
  for i := Low(Result.Accounts) to High(Result.Accounts) do
    Result.Accounts[i] := TAccount.Empty;
end;

function TAccountStorageEntry.ToString: AnsiString;
begin
  Result := Format('Block:%d Timestamp:%d BlockHash:%s', [BlockHeader.block, BlockHeader.timestamp,
    TBaseType.ToHexaString(BlockHash)]);
end;

{ TAccountStorageHeader }

class function TAccountStorageHeader.Empty: TAccountStorageHeader;
begin
  Result.Protocol := 0;
  Result.StartBlock := 0;
  Result.EndBlock := 0;
  Result.BlocksCount := 0;
  Result.AccountStorageHash := '';
end;

class function TAccountStorageHeader.LoadFromStream(AStream: TStream): TAccountStorageHeader;
var
  w: Word;
  s: AnsiString;
  AccountStorageVersion: Word;
  offsetPos, initialPos: Int64;
  endBlocks: Cardinal;
  xResult : boolean;
begin
  Result := TAccountStorageHeader.Empty;
  if (AStream.Size = 0) then exit;
  xResult := false;
  initialPos := AStream.Position;
  try
    AStream.ReadAnsiString(s);
    if (s <> cMagicID) then exit;
    if AStream.Size < 8 then exit;
    AStream.Read(w, Sizeof(w));
    if not(w in [1, 2]) then exit;
    Result.Protocol := w;
    AStream.Read(AccountStorageVersion, 2);
    if AccountStorageVersion <> cAccountStorageVersion then exit;
    AStream.Read(Result.BlocksCount, 4);
    AStream.Read(Result.StartBlock, 4);
    AStream.Read(Result.EndBlock, 4);
    if (Result.BlocksCount <= 0) or (Result.BlocksCount > (cBlockTime * 2000000))
    then exit; // Protection for corrupted data...
    offsetPos := AStream.Position;
    // Go to read SafeBoxHash
    if (AStream.Size < offsetPos + (((Result.EndBlock - Result.StartBlock) + 2) * 4))
    then exit;
    AStream.Position := offsetPos + (((Result.EndBlock - Result.StartBlock) + 1) * 4);
    AStream.Read(endBlocks, 4);
    // Go to end
    if (AStream.Size < offsetPos + (endBlocks))
    then exit;
    AStream.Position := offsetPos + endBlocks;
    if AStream.ReadAnsiString(Result.AccountStorageHash) < 0
    then exit;
    // Back
    AStream.Position := offsetPos;
    xResult := true;
  finally
    if not xResult then begin
      AStream.Position := initialPos;
      raise Exception.Create('Invalid Account storage header stream');
    end;
  end;
end;

end.
