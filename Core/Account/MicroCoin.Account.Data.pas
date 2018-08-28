unit MicroCoin.Account.Data;

{
  This unit contains code from PascalCoin:

  Copyright (c) Albert Molina 2016 - 2018 original code from PascalCoin https://pascalcoin.org/

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

}

interface

uses Classes, Sysutils, MicroCoin.Account.AccountKey, UCrypto, UConst, UBaseTypes;

type

  TAccountState = (as_Unknown, as_Normal, as_ForSale);

  TAccountInfo = record
    state: TAccountState;
    AccountKey: TAccountKey;
    // Trade info, only when state=as_ForSale
    locked_until_block: Cardinal; // 0 = Not locked
    price: UInt64; // 0 = invalid price
    account_to_pay: Cardinal; // <> itself
    new_publicKey: TAccountKey;
    function IsValid(var errors: AnsiString): Boolean;
    function IsAccountForSale: Boolean;
    function IsAccountForSaleAcceptingTransactions: Boolean;
    function ToRawString: TRawBytes; overload;
    function IsLocked(blocks_count: Cardinal): Boolean;
    procedure ToRawString(var dest: TRawBytes); overload;
    class function FromRawString(const rawaccstr: TRawBytes): TAccountInfo; overload; static;
    class procedure FromRawString(const rawaccstr: TRawBytes; var dest: TAccountInfo); overload; static;
    class function EqualAccountInfos(const accountInfo1, accountInfo2: TAccountInfo): Boolean; static;
    class operator Equal(const accountInfo1, accountInfo2: TAccountInfo): Boolean;
    class operator NotEqual(const accountInfo1, accountInfo2: TAccountInfo): Boolean;
  end;

  PAccount = ^TAccount;

  TAccount = record
    Account: Cardinal; // FIXED value. Account number
    AccountInfo: TAccountInfo;
    balance: UInt64; // Balance, always >= 0
    updated_block: Cardinal; // Number of block where was updated
    n_operation: Cardinal; // count number of owner operations (when receive, this is not updated)
    name: TRawBytes; // Protocol 2. Unique name
    account_type: Word; // Protocol 2. Layer 2 use case
    previous_updated_block: Cardinal;
    // New Build 1.0.8 -> Only used to store this info to storage. It helps App to search when an account was updated. NOT USED FOR HASH CALCULATIONS!
    class function IsAccountBlockedByProtocol(account_number, blocks_count: Cardinal): Boolean; static;
    class function AccountNumberToAccountTxtNumber(account_number: Cardinal): AnsiString; static;
    class function AccountTxtNumberToAccountNumber(const account_txt_number: AnsiString; var account_number: Cardinal)
      : Boolean; static;
    class function AccountBlock(const account_number: Cardinal): Cardinal; static;
  end;

  TOrderedAccountList = class
  private
    FList: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(const Account: TAccount): Integer;
    function Count: Integer;
    function Get(Index: Integer): TAccount; overload;
    function GetPointer(Index: Integer): PAccount; overload;
    function Find(const account_number: Cardinal; var Index: Integer): Boolean;
  end;

  TMemAccount = record // TAccount with less memory usage
    // account number is discarded (-4 bytes)
    AccountInfo: TDynRawBytes;
    balance: UInt64;
    updated_block: Cardinal;
    n_operation: Cardinal;
    name: TRawBytes;
    account_type: Word;
    previous_updated_block: Cardinal;
  end;

  TMemOperationBlock = record // TOperationBlock with less memory usage
    // block number is discarded (-4 bytes)
    account_key: TDynRawBytes;
    reward: UInt64;
    fee: UInt64;
    protocol_version: Word;
    protocol_available: Word;
    timestamp: Cardinal;
    compact_target: Cardinal;
    nonce: Cardinal;
    block_payload: TDynRawBytes;
    initial_safe_box_hash: T32Bytes; // 32 direct bytes instead of use an AnsiString (-8 bytes)
    operations_hash: T32Bytes; // 32 direct bytes instead of use an AnsiString (-8 bytes)
    proof_of_work: T32Bytes; // 32 direct bytes instead of use an AnsiString (-8 bytes)
  end;

  TMemBlockAccount = record // TBlockAccount with less memory usage
    blockchainInfo: TMemOperationBlock;
    accounts: array [0 .. CT_AccountsPerBlock - 1] of TMemAccount;
    block_hash: T32Bytes; // 32 direct bytes instead of use an AnsiString (-8 bytes)
    accumulatedWork: UInt64;
  end;

type
  PBlockAccount = ^TMemBlockAccount;

const
  CT_AccountInfo_NUL: TAccountInfo = (state: as_Unknown; AccountKey: (EC_OpenSSL_NID: 0; x: ''; y: '');
    locked_until_block: 0; price: 0; account_to_pay: 0; new_publicKey: (EC_OpenSSL_NID: 0; x: ''; y: ''));
  CT_Account_NUL: TAccount = (Account: 0; AccountInfo: (state: as_Unknown; AccountKey: (EC_OpenSSL_NID: 0; x: '';
    y: ''); locked_until_block: 0; price: 0; account_to_pay: 0; new_publicKey: (EC_OpenSSL_NID: 0; x: ''; y: ''));
    balance: 0; updated_block: 0; n_operation: 0; name: ''; account_type: 0; previous_updated_block: 0);

implementation

function TAccountInfo.IsValid(var errors: AnsiString): Boolean;
var
  s: AnsiString;
begin
  errors := '';
  case state of
    as_Unknown:
      begin
        errors := 'Account state is unknown';
        Result := false;
      end;
    as_Normal:
      begin
        Result := AccountKey.IsValidAccountKey(errors);
      end;
    as_ForSale:
      begin
        if not AccountKey.IsValidAccountKey(s) then
          errors := errors + ' ' + s;
        Result := errors = '';
      end;
  else
    raise Exception.Create('DEVELOP ERROR 20170214-3');
  end;
end;

class operator TAccountInfo.NotEqual(const accountInfo1, accountInfo2: TAccountInfo): Boolean;
begin
  Result := not TAccountInfo.EqualAccountInfos(accountInfo1, accountInfo2);
end;

function TAccountInfo.ToRawString: TRawBytes;
begin
  ToRawString(Result);
end;

procedure TAccountInfo.ToRawString(var dest: TRawBytes);
var
  ms: TMemoryStream;
  w: Word;
begin
  case state of
    as_Normal:
      AccountKey.ToRawString(dest);
    as_ForSale:
      begin
        ms := TMemoryStream.Create;
        try
          w := CT_AccountInfo_ForSale;
          ms.Write(w, Sizeof(w));
          //
          TStreamOp.WriteAccountKey(ms, AccountKey);
          ms.Write(locked_until_block, Sizeof(locked_until_block));
          ms.Write(price, Sizeof(price));
          ms.Write(account_to_pay, Sizeof(account_to_pay));
          TStreamOp.WriteAccountKey(ms, new_publicKey);
          SetLength(dest, ms.Size);
          ms.Position := 0;
          ms.Read(dest[1], ms.Size);
        finally
          ms.Free;
        end;
      end;
  else
    raise Exception.Create('DEVELOP ERROR 20170214-1');
  end;
end;

class operator TAccountInfo.Equal(const accountInfo1, accountInfo2: TAccountInfo): Boolean;
begin
  Result := TAccountInfo.EqualAccountInfos(accountInfo1, accountInfo2);
end;

class function TAccountInfo.EqualAccountInfos(const accountInfo1, accountInfo2: TAccountInfo): Boolean;
begin
  Result := (accountInfo1.state = accountInfo2.state) and
    (TAccountKey.EqualAccountKeys(accountInfo1.AccountKey, accountInfo2.AccountKey)) and
    (accountInfo1.locked_until_block = accountInfo2.locked_until_block) and (accountInfo1.price = accountInfo2.price)
    and (accountInfo1.account_to_pay = accountInfo2.account_to_pay) and
    (TAccountKey.EqualAccountKeys(accountInfo1.new_publicKey, accountInfo2.new_publicKey));
end;

function TAccountInfo.IsLocked(blocks_count: Cardinal): Boolean;
begin
  Result := (state = as_ForSale) and ((locked_until_block) >= blocks_count);
end;

class function TAccountInfo.FromRawString(const rawaccstr: TRawBytes): TAccountInfo;
begin
  FromRawString(rawaccstr, Result);
end;

class procedure TAccountInfo.FromRawString(const rawaccstr: TRawBytes; var dest: TAccountInfo);
var
  ms: TMemoryStream;
  w: Word;
begin
  if length(rawaccstr) = 0 then
  begin
    dest := CT_AccountInfo_NUL;
    exit;
  end;
  ms := TMemoryStream.Create;
  try
    ms.WriteBuffer(rawaccstr[1], length(rawaccstr));
    ms.Position := 0;
    if ms.Read(w, Sizeof(w)) <> Sizeof(w) then
      exit;
    case w of
      CT_NID_secp256k1, CT_NID_secp384r1, CT_NID_sect283k1, CT_NID_secp521r1:
        begin
          dest.state := as_Normal;
          TAccountKey.FromRawString(rawaccstr, dest.AccountKey);
          dest.locked_until_block := CT_AccountInfo_NUL.locked_until_block;
          dest.price := CT_AccountInfo_NUL.price;
          dest.account_to_pay := CT_AccountInfo_NUL.account_to_pay;
          dest.new_publicKey := CT_AccountInfo_NUL.new_publicKey;
        end;
      CT_AccountInfo_ForSale:
        begin
          TStreamOp.ReadAccountKey(ms, dest.AccountKey);
          ms.Read(dest.locked_until_block, Sizeof(dest.locked_until_block));
          ms.Read(dest.price, Sizeof(dest.price));
          ms.Read(dest.account_to_pay, Sizeof(dest.account_to_pay));
          TStreamOp.ReadAccountKey(ms, dest.new_publicKey);
          dest.state := as_ForSale;
        end;
    else
      raise Exception.Create('DEVELOP ERROR 20170214-2');
    end;
  finally
    ms.Free;
  end;
end;

function TAccountInfo.IsAccountForSale: Boolean;
begin
  Result := (state = as_ForSale);
end;

function TAccountInfo.IsAccountForSaleAcceptingTransactions: Boolean;
var
  errors: AnsiString;
begin
  Result := (state = as_ForSale) and (new_publicKey.IsValidAccountKey(errors));
end;

class function TAccount.AccountBlock(const account_number: Cardinal): Cardinal;
begin
  Result := account_number div CT_AccountsPerBlock;
end;

class function TAccount.AccountNumberToAccountTxtNumber(account_number: Cardinal): AnsiString;
var
  an: Int64;
begin
  an := account_number; // Converting to int64 to prevent overflow when *101
  an := ((an * 101) mod 89) + 10;
  Result := inttostr(account_number) + '-' + inttostr(an);
end;

class function TAccount.AccountTxtNumberToAccountNumber(const account_txt_number: AnsiString;
  var account_number: Cardinal): Boolean;
var
  i: Integer;
  char1: AnsiChar;
  char2: AnsiChar;
  an, rn, anaux: Int64;
begin
  Result := false;
  if length(trim(account_txt_number)) = 0 then
    exit;
  an := 0;
  i := 1;
  while (i <= length(account_txt_number)) do
  begin
    if account_txt_number[i] in ['0' .. '9'] then
    begin
      an := (an * 10) + ord(account_txt_number[i]) - ord('0');
    end
    else
    begin
      break;
    end;
    inc(i);
  end;
  account_number := an;
  if (i > length(account_txt_number)) then
  begin
    Result := true;
    exit;
  end;
  if (account_txt_number[i] in ['-', '.', ' ']) then
    inc(i);
  if length(account_txt_number) - 1 <> i then
    exit;
  rn := StrToIntDef(Copy(account_txt_number, i, length(account_txt_number)), 0);
  anaux := ((an * 101) mod 89) + 10;
  Result := rn = anaux;
end;

class function TAccount.IsAccountBlockedByProtocol(account_number, blocks_count: Cardinal): Boolean;
var
  waitBlocks: Integer;
begin
  // Update protocol
  if blocks_count >= CT_V2BlockNumber then
    waitBlocks := CT_WaitNewBlocksBeforeTransactionV2
  else
    waitBlocks := CT_WaitNewBlocksBeforeTransaction;
  if blocks_count < waitBlocks then
    Result := true
  else
  begin
    Result := ((blocks_count - waitBlocks) * CT_AccountsPerBlock) <= account_number;
  end;
end;

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

function TOrderedAccountList.GetPointer(Index: Integer): PAccount;
begin
  Result := PAccount(FList.Items[index]);
end;

function TOrderedAccountList.Get(Index: Integer): TAccount;
begin
  Result := GetPointer(index)^;
end;

end.
