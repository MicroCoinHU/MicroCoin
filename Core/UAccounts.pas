unit UAccounts;

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
  Classes, UConst, UCrypto, SyncObjs, UThread, UBaseTypes, UCommon,
  MicroCoin.Common, MicroCoin.Common.Lists, MicroCoin.Account.AccountKey,
  MicroCoin.Account, MicroCoin.Account.Storage, MicroCoin.BlockChain.Blockheader;

{$I config.inc}

type


  { TMicroCoinProtocol }
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

  {
    Protocol 2:
    Introducing OperationBlock info on the safebox, this will allow checkpointing a safebox because
    each row of the safebox (TBlockAccount) will have data about how to calculate
    its PoW, so next row will use row-1 info to check it's good generated thanks to PoW

    This solution does not include operations, but include operations_hash value,
    that is a SHA256 of operations.

    If someone wants to change the safebox and spam, will need to find values
    to alter safebox accounts of last checkpoint and also find new blocks prior
    to honest nodes, that will be only accepted by nodes that does not have
    last blocks (only fresh nodes). This is a very hard job and not efficient
    because usually there will be few new fresh nodes per period, so only
    can spam new fresh nodes because old nodes does not need to download
    a checkpointing.
    This solution was created by Herman Schoenfeld (Thanks!)
  }


  { Estimated TAccount size:
    4 + 200 (max aprox) + 8 + 4 + 4 = 220 max aprox
    Estimated TBlockAccount size:
    4 + (5 * 220) + 4 + 32 = 1140 max aprox
  }


  // This is a class to quickly find accountkeys and their respective account number/s
  TOrderedAccountKeysList = class
  private
    FAutoAddAll: Boolean;
    FAccountStorage: TAccountStorage;
    FOrderedAccountKeysList: TList; // An ordered list of pointers to quickly find account keys in account list
    function Find(const AccountKey: TAccountKey; var Index: Integer): Boolean;
    function GetAccountKeyList(Index: Integer): TOrderedList;
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
    property AccountKeyList[index: Integer]: TOrderedList read GetAccountKeyList;
    property AccountKey[index: Integer]: TAccountKey read GetAccountKey;
    function Count: Integer;
    property AccountStorage: TAccountStorage read FAccountStorage write FAccountStorage;
    procedure Clear;
  end;

  // Maintains a TRawBytes (AnsiString) list ordered to quick search withoud duplicates

  // SafeBox is a box that only can be updated using SafeBoxTransaction, and this
  // happens only when a new BlockChain is included. After this, a new "SafeBoxHash"
  // is created, so each SafeBox has a unique SafeBoxHash

  { TPCSafeBox }


  TOrderedAccountList = class
  private
    FList: TList;
    function Find(const account_number: Cardinal; var Index: Integer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(const Account: TAccount): Integer;
    function Count: Integer;
    function Get(Index: Integer): TAccount;
  end;

  { TAccountTransaction }

  TAccountTransaction = class
  private
    FOrderedList: TOrderedAccountList;
    FFreezedAccounts: TAccountStorage;
    FTotalBalance: Int64;
    FTotalFee: Int64;
    FPreviusHash: TRawBytes;
    FAccountNames_Deleted: TOrderedRawList;
    FAccountNames_Added: TOrderedRawList;
    function GetInternalAccount(account_number: Cardinal): PAccount;
  protected
  public
    constructor Create(AccountStorage: TAccountStorage);
    destructor Destroy; override;
    function TransferAmount(sender, target: Cardinal; n_operation: Cardinal; amount, fee: UInt64; var errors: AnsiString): Boolean;
    function UpdateAccountInfo(signer_account, signer_n_operation, target_account: Cardinal; AccountInfo: TAccountInfo; newName: TRawBytes; newType: Word; fee: UInt64; var errors: AnsiString)
      : Boolean;
    function BuyAccount(buyer, account_to_buy, seller: Cardinal; n_operation: Cardinal; amount, account_price, fee: UInt64; const new_account_key: TAccountKey; var errors: AnsiString): Boolean;
    function Commit(const operationBlock: TBlockHeader; var errors: AnsiString): Boolean;
    function Account(account_number: Cardinal): TAccount;
    procedure Rollback;
    function CheckIntegrity: Boolean;
    property FreezedAccountStorage: TAccountStorage read FFreezedAccounts;
    property TotalFee: Int64 read FTotalFee;
    property TotalBalance: Int64 read FTotalBalance;
    procedure CopyFrom(transaction: TAccountTransaction);
    procedure CleanTransaction;
    function ModifiedCount: Integer;
    function Modified(Index: Integer): TAccount;
    property FreezedAccounts : TAccountStorage read FFreezedAccounts;
  end;

const

  CT_AccountChunkIdentificator = 'SafeBoxChunk';
  CT_AccountStorageHeader_NUL: TAccountStorageHeader = (protocol: 0; startBlock: 0; endBlock: 0; blocksCount: 0; AccountStorageHash: '');

implementation

uses
  SysUtils, ULog, UOpenSSLdef, UOpenSSL;

{ TMicroCoinProtocol }

class function TMicroCoinProtocol.GetNewTarget(vteorical, vreal: Cardinal; const actualTarget: TRawBytes): TRawBytes;
var
  bnact, bnaux, bnmindiff, bnremainder, bn: TBigNum;
  ts1, ts2: Cardinal;
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
  if CT_CalcNewTargetBlocksAverage > 1 then
  begin
    factor1000Min := (-500) div (CT_CalcNewTargetBlocksAverage div 2);
    factor1000Max := (1000) div (CT_CalcNewTargetBlocksAverage div 2);
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
  bn := TBigNum.Create(factor1000);
  bnact := TBigNum.Create(0);
  try
    bnact.RawValue := actualTarget;
    bnaux := bnact.Copy;
    try
      bnact.Multiply(factor1000).Divide(1000).Add(bnaux);
    finally
      bnaux.Free;
    end;
    // Adjust to TargetCompact limitations:
    Result := TargetFromCompact(TargetToCompact(bnact.RawValue));
  finally
    bn.Free;
    bnact.Free;
  end;
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
    ms.WriteBuffer(operationBlock.operations_hash[1], length(operationBlock.operations_hash));
    // Note about fee: Fee is stored in 8 bytes, but only digest first 4 low bytes
    ms.Write(operationBlock.fee, 4);
    SetLength(Part3, ms.Size);
    ms.Position := 0;
    ms.ReadBuffer(Part3[1], ms.Size);
    s := TCrypto.ToHexaString(operationBlock.operations_hash);
    s := '';
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
    ms.WriteBuffer(operationBlock.operations_hash[1], length(operationBlock.operations_hash));
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
  n := (line_index + 1) div CT_NewLineRewardDecrease;
  Result := CT_FirstReward;
  for i := 1 to n do
  begin
    Result := Result div 2;
  end;
  if (Result < CT_MinReward) then
    Result := CT_MinReward;
end;

class function TMicroCoinProtocol.TargetFromCompact(encoded: Cardinal): TRawBytes;
var
  nbits, high, offset, i: Cardinal;
  bn: TBigNum;
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
  i := CT_MinCompactTarget shr 24;
  if nbits < i then
    nbits := i; // min nbits
  if nbits > 231 then
    nbits := 231; // max nbits

  offset := (encoded shl 8) shr 8;
  // Make a XOR at offset and put a "1" on the left
  offset := ((offset xor $00FFFFFF) or ($01000000));

  bn := TBigNum.Create(offset);
  try
    bn.LShift(256 - nbits - 25);
    raw := bn.RawValue;
    SetLength(Result, 32);
    FillChar(Result[1], 32, 0);
    for i := 1 to length(raw) do
    begin
      Result[i + 32 - length(raw)] := raw[i];
    end;
  finally
    bn.Free;
  end;
end;

class function TMicroCoinProtocol.TargetToCompact(target: TRawBytes): Cardinal;
var
  bn, bn2: TBigNum;
  i: Int64;
  nbits: Cardinal;
  c: AnsiChar;
  raw: TRawBytes;
  j: Integer;
begin
  { See instructions in explanation of TargetFromCompact }
  Result := 0;
  if length(target) > 32 then
  begin
    raise Exception.Create('Invalid target to compact: ' + TCrypto.ToHexaString(target) + ' (' + inttostr(length(target)) + ')');
  end;
  SetLength(raw, 32);
  FillChar(raw[1], 32, 0);
  for j := 1 to length(target) do
  begin
    raw[j + 32 - length(target)] := target[j];
  end;
  target := raw;

  bn := TBigNum.Create(0);
  bn2 := TBigNum.Create('8000000000000000000000000000000000000000000000000000000000000000'); // First bit 1 followed by 0
  try
    bn.RawValue := target;
    nbits := 0;
    while (bn.CompareTo(bn2) < 0) and (nbits < 231) do
    begin
      bn2.RShift(1);
      inc(nbits);
    end;
    i := CT_MinCompactTarget shr 24;
    if (nbits < i) then
    begin
      Result := CT_MinCompactTarget;
      exit;
    end;
    bn.RShift((256 - 25) - nbits);
    Result := (nbits shl 24) + ((bn.value and $00FFFFFF) xor $00FFFFFF);
  finally
    bn.Free;
    bn2.Free;
  end;
end;

{ TPCSafeBox }

// New on version 2: To reduce mem usage
{$DEFINE uselowmem}

{$IFDEF uselowmem}


  { In order to store less memory on RAM, those types will be used
    to store in RAM memory (better than to use original ones)
    This will reduce 10-15% of memory usage.
    For future versions, will be a good solution to use those instead
    of originals, but }


{$ELSE}


type
  PBlockAccount = ^TBlockAccount;
  TMemAccount = TAccount;
  TMemBlockAccount = TBlockAccount;
{$ENDIF}

{ TPCSafeBoxTransaction }

function TAccountTransaction.Account(account_number: Cardinal): TAccount;
var
  i: Integer;
begin
  if FOrderedList.Find(account_number, i) then
    Result := PAccount(FOrderedList.FList[i])^
  else
  begin
    Result := FreezedAccountStorage.Account(account_number);
  end;
end;

function TAccountTransaction.BuyAccount(buyer, account_to_buy,
  seller: Cardinal; n_operation: Cardinal; amount, account_price, fee: UInt64;
  const new_account_key: TAccountKey; var errors: AnsiString): Boolean;
var
  PaccBuyer, PaccAccountToBuy, PaccSeller: PAccount;
begin
  Result := false;
  errors := '';
  if not CheckIntegrity then
  begin
    errors := 'Invalid integrity in accounts transaction';
    exit;
  end;
  if (buyer < 0) or (buyer >= (FFreezedAccounts.blocksCount * CT_AccountsPerBlock)) or
    (account_to_buy < 0) or (account_to_buy >= (FFreezedAccounts.blocksCount * CT_AccountsPerBlock)) or
    (seller < 0) or (seller >= (FFreezedAccounts.blocksCount * CT_AccountsPerBlock)) then
  begin
    errors := 'Invalid account number on buy';
    exit;
  end;
  if TAccount.IsAccountBlockedByProtocol(buyer, FFreezedAccounts.blocksCount) then
  begin
    errors := 'Buyer account is blocked for protocol';
    exit;
  end;
  if TAccount.IsAccountBlockedByProtocol(account_to_buy, FFreezedAccounts.blocksCount) then
  begin
    errors := 'Account to buy is blocked for protocol';
    exit;
  end;
  if TAccount.IsAccountBlockedByProtocol(seller, FFreezedAccounts.blocksCount) then
  begin
    errors := 'Seller account is blocked for protocol';
    exit;
  end;
  PaccBuyer := GetInternalAccount(buyer);
  PaccAccountToBuy := GetInternalAccount(account_to_buy);
  PaccSeller := GetInternalAccount(seller);
  if (PaccBuyer^.n_operation + 1 <> n_operation) then
  begin
    errors := 'Incorrect n_operation';
    exit;
  end;
  if (PaccBuyer^.balance < (amount + fee)) then
  begin
    errors := 'Insuficient founds';
    exit;
  end;
  if (fee > CT_MaxTransactionFee) then
  begin
    errors := 'Max fee';
    exit;
  end;
  if (PaccBuyer^.AccountInfo.IsLocked(FFreezedAccounts.blocksCount)) then
  begin
    errors := 'Buyer account is locked until block ' + inttostr(PaccBuyer^.AccountInfo.locked_until_block);
    exit;
  end;
  if not(PaccAccountToBuy^.AccountInfo.IsAccountForSale) then
  begin
    errors := 'Account is not for sale';
    exit;
  end;
  if (PaccAccountToBuy^.AccountInfo.new_publicKey.EC_OpenSSL_NID <> CT_TECDSA_Public_Nul.EC_OpenSSL_NID) and
    (not TAccountKey.EqualAccountKeys(PaccAccountToBuy^.AccountInfo.new_publicKey, new_account_key)) then
  begin
    errors := 'New public key is not equal to allowed new public key for account';
    exit;
  end;
  // Buy an account applies when account_to_buy.amount + operation amount >= price
  // Then, account_to_buy.amount will be (account_to_buy.amount + amount - price)
  // and buyer.amount will be buyer.amount + price
  if (PaccAccountToBuy^.AccountInfo.price > (PaccAccountToBuy^.balance + amount)) then
  begin
    errors := 'Account price ' + TCurrencyUtils.FormatMoney(PaccAccountToBuy^.AccountInfo.price) + ' < balance ' +
      TCurrencyUtils.FormatMoney(PaccAccountToBuy^.balance) + ' + amount ' + TCurrencyUtils.FormatMoney(amount);
    exit;
  end;

  if PaccBuyer^.updated_block <> FFreezedAccounts.blocksCount then
  begin
    PaccBuyer^.previous_updated_block := PaccBuyer^.updated_block;
    PaccBuyer^.updated_block := FFreezedAccounts.blocksCount;
  end;

  if PaccAccountToBuy^.updated_block <> FFreezedAccounts.blocksCount then
  begin
    PaccAccountToBuy^.previous_updated_block := PaccAccountToBuy^.updated_block;
    PaccAccountToBuy^.updated_block := FFreezedAccounts.blocksCount;
  end;

  if PaccSeller^.updated_block <> FFreezedAccounts.blocksCount then
  begin
    PaccSeller^.previous_updated_block := PaccSeller^.updated_block;
    PaccSeller^.updated_block := FFreezedAccounts.blocksCount;
  end;

  // Inc buyer n_operation
  PaccBuyer^.n_operation := n_operation;
  // Set new balance values
  PaccBuyer^.balance := PaccBuyer^.balance - (amount + fee);
  PaccAccountToBuy^.balance := PaccAccountToBuy^.balance + amount - PaccAccountToBuy^.AccountInfo.price;
  PaccSeller^.balance := PaccSeller^.balance + PaccAccountToBuy^.AccountInfo.price;

  // After buy, account will be unlocked and set to normal state and new account public key changed
  PaccAccountToBuy^.AccountInfo := CT_AccountInfo_NUL;
  PaccAccountToBuy^.AccountInfo.state := as_Normal;
  PaccAccountToBuy^.AccountInfo.AccountKey := new_account_key;

  Dec(FTotalBalance, fee);
  inc(FTotalFee, fee);
  Result := true;
end;

function TAccountTransaction.CheckIntegrity: Boolean;
begin
  Result := FPreviusHash = FFreezedAccounts.AccountStorageHash;
end;

procedure TAccountTransaction.CleanTransaction;
begin
  FOrderedList.Clear;
  FPreviusHash := FFreezedAccounts.AccountStorageHash;
  FTotalBalance := FFreezedAccounts.TotalBalance;
  FTotalFee := 0;
  FAccountNames_Added.Clear;
  FAccountNames_Deleted.Clear;
end;

function TAccountTransaction.Commit(const operationBlock: TBlockHeader;
  var errors: AnsiString): Boolean;
var
  i, j: Integer;
  b: TAccountStorageEntry;
  Pa: PAccount;
begin
  Result := false;
  errors := '';
  FFreezedAccounts.StartThreadSafe;
  try
    if not CheckIntegrity then
    begin
      errors := 'Invalid integrity in accounts transaction on commit';
      exit;
    end;
    for i := 0 to FOrderedList.FList.Count - 1 do
    begin
      Pa := PAccount(FOrderedList.FList[i]);
      FreezedAccounts.SetAccount(Pa^.Account,
        Pa^.AccountInfo,
        Pa^.name,
        Pa^.account_type,
        Pa^.balance,
        Pa^.n_operation);
    end;
    //
    if (FFreezedAccounts.TotalBalance <> FTotalBalance) then
    begin
      TLog.NewLog(ltError, Classname, Format('Invalid integrity balance! StrongBox:%d Transaction:%d', [FFreezedAccounts.TotalBalance, FTotalBalance]));
    end;
    if (FreezedAccounts.TotalFee <> TotalFee) then
    begin
      TLog.NewLog(ltError, Classname, Format('Invalid integrity fee! StrongBox:%d Transaction:%d', [FFreezedAccounts.TotalFee, FTotalFee]));
    end;
    b := FFreezedAccounts.AddNew(operationBlock);
    if (b.accounts[0].balance <> (operationBlock.reward + FTotalFee)) then
    begin
      TLog.NewLog(ltError, Classname, Format('Invalid integrity reward! Account:%d Balance:%d  Reward:%d Fee:%d (Reward+Fee:%d)',
        [b.accounts[0].Account, b.accounts[0].balance, operationBlock.reward, FTotalFee, operationBlock.reward + FTotalFee]));
    end;
    CleanTransaction;
    //
    if (FFreezedAccounts.CurrentProtocol < CT_PROTOCOL_2) and (operationBlock.protocol_version = CT_PROTOCOL_2) then
    begin
      // First block with new protocol!
      if FFreezedAccounts.CanUpgradeToProtocol2 then
      begin
        TLog.NewLog(ltInfo, Classname, 'Protocol upgrade to v2');
        if not FFreezedAccounts.DoUpgradeToProtocol2 then
        begin
          raise Exception.Create('Cannot upgrade to protocol v2 !');
        end;
      end;
    end;
    Result := true;
  finally
    FFreezedAccounts.EndThreadSave;
  end;
end;

procedure TAccountTransaction.CopyFrom(transaction: TAccountTransaction);
var
  i: Integer;
  P: PAccount;
begin
  if transaction = Self then
    exit;
  if transaction.FFreezedAccounts <> FFreezedAccounts then
    raise Exception.Create('Invalid Freezed accounts to copy');
  CleanTransaction;
  for i := 0 to transaction.FOrderedList.FList.Count - 1 do
  begin
    P := PAccount(transaction.FOrderedList.FList[i]);
    FOrderedList.Add(P^);
  end;
  FPreviusHash := transaction.FPreviusHash;
  FTotalBalance := transaction.FTotalBalance;
  FTotalFee := transaction.FTotalFee;
end;

constructor TAccountTransaction.Create(AccountStorage: TAccountStorage);
begin
  FOrderedList := TOrderedAccountList.Create;
  FFreezedAccounts := AccountStorage;
  FPreviusHash := AccountStorage.AccountStorageHash;
  FTotalBalance := FFreezedAccounts.TotalBalance;
  FTotalFee := 0;
  FAccountNames_Added := TOrderedRawList.Create;
  FAccountNames_Deleted := TOrderedRawList.Create;
end;

destructor TAccountTransaction.Destroy;
begin
  CleanTransaction;
  FreeAndNil(FOrderedList);
  FreeAndNil(FAccountNames_Added);
  FreeAndNil(FAccountNames_Deleted);
  inherited;
end;

function TAccountTransaction.GetInternalAccount(account_number: Cardinal): PAccount;
var
  i: Integer;
  P: PAccount;
begin
  if FOrderedList.Find(account_number, i) then
    Result := PAccount(FOrderedList.FList[i])
  else
  begin
    i := FOrderedList.Add(FreezedAccountStorage.Account(account_number));
    Result := PAccount(FOrderedList.FList[i]);
  end;
end;

function TAccountTransaction.Modified(Index: Integer): TAccount;
begin
  Result := FOrderedList.Get(index);
end;

function TAccountTransaction.ModifiedCount: Integer;
begin
  Result := FOrderedList.Count;
end;

procedure TAccountTransaction.Rollback;
begin
  CleanTransaction;
end;

function TAccountTransaction.TransferAmount(sender, target: Cardinal;
  n_operation: Cardinal; amount, fee: UInt64; var errors: AnsiString): Boolean;
var
  intSender, intTarget: Integer;
  PaccSender, PaccTarget: PAccount;
begin
  Result := false;
  errors := '';
  if not CheckIntegrity then
  begin
    errors := 'Invalid integrity in accounts transaction';
    exit;
  end;
  if (sender < 0) or (sender >= (FFreezedAccounts.blocksCount * CT_AccountsPerBlock)) or
    (target < 0) or (target >= (FFreezedAccounts.blocksCount * CT_AccountsPerBlock)) then
  begin
    errors := 'Invalid sender or target on transfer';
    exit;
  end;
  if TAccount.IsAccountBlockedByProtocol(sender, FFreezedAccounts.blocksCount) then
  begin
    errors := 'Sender account is blocked for protocol';
    exit;
  end;
  if TAccount.IsAccountBlockedByProtocol(target, FFreezedAccounts.blocksCount) then
  begin
    errors := 'Target account is blocked for protocol';
    exit;
  end;
  PaccSender := GetInternalAccount(sender);
  PaccTarget := GetInternalAccount(target);
  if (PaccSender^.n_operation + 1 <> n_operation) then
  begin
    errors := 'Incorrect n_operation';
    exit;
  end;
  if (PaccSender^.balance < (amount + fee)) then
  begin
    errors := 'Insuficient founds';
    exit;
  end;
  if ((PaccTarget^.balance + amount) > CT_MaxWalletAmount) then
  begin
    errors := 'Max account balance';
    exit;
  end;
  if (fee > CT_MaxTransactionFee) then
  begin
    errors := 'Max fee';
    exit;
  end;
  if (PaccSender^.AccountInfo.IsLocked(FFreezedAccounts.blocksCount)) then
  begin
    errors := 'Sender account is locked until block ' + inttostr(PaccSender^.AccountInfo.locked_until_block);
    exit;
  end;

  if PaccSender^.updated_block <> FFreezedAccounts.blocksCount then
  begin
    PaccSender^.previous_updated_block := PaccSender^.updated_block;
    PaccSender^.updated_block := FFreezedAccounts.blocksCount;
  end;

  if PaccTarget^.updated_block <> FFreezedAccounts.blocksCount then
  begin
    PaccTarget^.previous_updated_block := PaccTarget.updated_block;
    PaccTarget^.updated_block := FFreezedAccounts.blocksCount;
  end;

  PaccSender^.n_operation := n_operation;
  PaccSender^.balance := PaccSender^.balance - (amount + fee);
  PaccTarget^.balance := PaccTarget^.balance + (amount);

  Dec(FTotalBalance, fee);
  inc(FTotalFee, fee);
  Result := true;
end;

function TAccountTransaction.UpdateAccountInfo(signer_account, signer_n_operation, target_account: Cardinal;
  AccountInfo: TAccountInfo; newName: TRawBytes; newType: Word; fee: UInt64; var errors: AnsiString): Boolean;
var
  i: Integer;
  P_signer, P_target: PAccount;
begin
  Result := false;
  errors := '';
  if (signer_account < 0) or (signer_account >= (FFreezedAccounts.blocksCount * CT_AccountsPerBlock)) or
    (target_account < 0) or (target_account >= (FFreezedAccounts.blocksCount * CT_AccountsPerBlock)) then
  begin
    errors := 'Invalid account';
    exit;
  end;
  if (TAccount.IsAccountBlockedByProtocol(signer_account, FFreezedAccounts.blocksCount)) or
    (TAccount.IsAccountBlockedByProtocol(target_account, FFreezedAccounts.blocksCount)) then
  begin
    errors := 'account is blocked for protocol';
    exit;
  end;
  P_signer := GetInternalAccount(signer_account);
  P_target := GetInternalAccount(target_account);
  if (P_signer^.n_operation + 1 <> signer_n_operation) then
  begin
    errors := 'Incorrect n_operation';
    exit;
  end;
  if (P_signer^.balance < fee) then
  begin
    errors := 'Insuficient founds';
    exit;
  end;
  if (P_signer^.AccountInfo.IsLocked(FFreezedAccounts.blocksCount)) then
  begin
    errors := 'Signer account is locked until block ' + inttostr(P_signer^.AccountInfo.locked_until_block);
    exit;
  end;
  if (P_target^.AccountInfo.IsLocked(FFreezedAccounts.blocksCount)) then
  begin
    errors := 'Target account is locked until block ' + inttostr(P_target^.AccountInfo.locked_until_block);
    exit;
  end;
  if P_signer^.updated_block <> FFreezedAccounts.blocksCount then
  begin
    P_signer^.previous_updated_block := P_signer^.updated_block;
    P_signer^.updated_block := FFreezedAccounts.blocksCount;
  end;
  if (signer_account <> target_account) then
  begin
    if P_target^.updated_block <> FFreezedAccounts.blocksCount then
    begin
      P_target^.previous_updated_block := P_target^.updated_block;
      P_target^.updated_block := FFreezedAccounts.blocksCount;
    end;
  end;
  if not TAccountKey.EqualAccountKeys(P_signer^.AccountInfo.AccountKey, P_target^.AccountInfo.AccountKey) then
  begin
    errors := 'Signer and target have diff key';
    exit;
  end;
  if (newName <> P_target^.name) then
  begin
    // NEW NAME CHANGE CHECK:
    if (newName <> '') then
    begin
      if not FFreezedAccounts.AccountNameIsValid(newName, errors) then
      begin
        errors := 'Invalid account name "' + newName + '" length:' + inttostr(length(newName)) + ': ' + errors;
        exit;
      end;
      i := FFreezedAccounts.FindAccountByName(newName);
      if (i >= 0) then
      begin
        // This account name is in the safebox... check if deleted:
        i := FAccountNames_Deleted.IndexOf(newName);
        if i < 0 then
        begin
          errors := 'Account name "' + newName + '" is in current use';
          exit;
        end;
      end;
      i := FAccountNames_Added.IndexOf(newName);
      if (i >= 0) then
      begin
        // This account name is added in this transaction! (perhaps deleted also, but does not allow to "double add same name in same block")
        errors := 'Account name "' + newName + '" is in same transaction';
        exit;
      end;
    end;
    // Ok, include
    if (P_target^.name <> '') then
    begin
      // In use in the safebox, mark as deleted
      FAccountNames_Deleted.Add(P_target^.name, target_account);
    end;
    if (newName <> '') then
    begin
      FAccountNames_Added.Add(newName, target_account);
    end;
  end;

  P_signer^.n_operation := signer_n_operation;
  P_target^.AccountInfo := AccountInfo;
  P_target^.name := newName;
  P_target^.account_type := newType;
  Dec(P_signer^.balance, fee); // Signer is who pays the fee
  Dec(FTotalBalance, fee);
  inc(FTotalFee, fee);
  Result := true;
end;

{ TOrderedAccountList }

function TOrderedAccountList.Add(const Account: TAccount): Integer;
var
  P: PAccount;
begin
  if Find(Account.Account, Result) then
  begin
    PAccount(FList[Result])^ := Account;
  end
  else
  begin
    New(P);
    P^ := Account;
    FList.Insert(Result, P);
  end;
end;

procedure TOrderedAccountList.Clear;
var
  i: Integer;
  P: PAccount;
begin
  for i := 0 to FList.Count - 1 do
  begin
    P := FList[i];
    Dispose(P);
  end;
  FList.Clear;
end;

function TOrderedAccountList.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TOrderedAccountList.Create;
begin
  FList := TList.Create;
end;

destructor TOrderedAccountList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

function TOrderedAccountList.Find(const account_number: Cardinal; var Index: Integer): Boolean;
var
  L, H, i: Integer;
  c: Int64;
begin
  Result := false;
  L := 0;
  H := FList.Count - 1;
  while L <= H do
  begin
    i := (L + H) shr 1;
    c := Int64(PAccount(FList[i]).Account) - Int64(account_number);
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

function TOrderedAccountList.Get(Index: Integer): TAccount;
begin
  Result := PAccount(FList.Items[index])^;
end;

{ TOrderedAccountKeysList }
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
      TLog.NewLog(ltDebug, Classname, Format('Adding account key (%d of %d) %s', [j, FAccountStorage.AccountsCount, TCrypto.ToHexaString(AccountKey.ToRawString)]));
    end
    else
    begin
      TLog.NewLog(ltDebug, Classname, Format('Adding account key (no Account List) %s', [TCrypto.ToHexaString(AccountKey.ToRawString)]));
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
      Dispose(P);
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

function TOrderedAccountKeysList.GetAccountKeyList(Index: Integer): TOrderedList;
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
    Dispose(P);
  end;
end;

procedure TOrderedAccountKeysList.RemoveAccountKey(const AccountKey: TAccountKey);
var
  P: POrderedAccountKeyList;
  i, j: Integer;
begin
  if not Find(AccountKey, i) then
    exit; // Nothing to do
  P := POrderedAccountKeyList(FOrderedAccountKeysList[i]);
  // Remove from list
  FOrderedAccountKeysList.Delete(i);
  // Free it
  P^.accounts_number.Free;
  Dispose(P);
end;

end.
