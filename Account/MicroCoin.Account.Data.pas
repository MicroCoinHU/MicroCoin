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
| File:       MicroCoin.Account.Data.pas                                       |
| Created at: 2018-08-27                                                       |
| Purpose:    Base account structures and definitions                          |
|==============================================================================}

unit MicroCoin.Account.Data;


{$ifdef FPC}
  {$mode delphi}
{$endif}

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

  TSubAccount = record
    AccountKey: TAccountKey;
    DailyLimit: Int64;
    TotalLimit: Int64;
    Balance: UInt64;
    Currency: Cardinal;
  end;

  TExtraData = record
    DataType: byte;
    ExtraType: byte;
    Data: TRawBytes;
  end;

  PAccount = ^TAccount;

  TAccount = record
  private
    FHasExtraData: boolean;
  public
    AccountNumber: Cardinal; // FIXED value. Account number
    AccountInfo: TAccountInfo;
    balance: UInt64; // Balance, always >= 0
    updated_block: Cardinal; // Number of block where was updated
    numberOfTransactions: Cardinal; // count number of owner operations (when receive, this is not updated)
    name: TRawBytes; // Protocol 2. Unique name
    account_type: Word; // Protocol 2. Layer 2 use case
    previous_updated_block: Cardinal;
    {$IFDEF EXTENDEDACCOUNT}
    SubAccounts: array of TSubAccount;
    ExtraData: TExtraData;
    {$ENDIF}
    class function Empty: TAccount; static;
    class function IsAccountBlockedByProtocol(account_number, blocks_count: Cardinal): Boolean; static;
    class function AccountNumberToAccountTxtNumber(account_number: Cardinal): AnsiString; static;
    class function AccountTxtNumberToAccountNumber(const account_txt_number: AnsiString; var account_number: Cardinal) : Boolean; static;
    class function AccountBlock(const account_number: Cardinal): Cardinal; static;
    class function LoadFromStream(AStream : TStream; var RAccount : TAccount; ACurrentProtocol: shortint) : boolean; static;
    property HasExtraData: boolean read FHasExtraData write FHasExtraData;
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

  TMemAccount = TAccount;
  { record // TAccount with less memory usage
    // account number is discarded (-4 bytes)
    AccountInfo: TDynRawBytes;
    balance: UInt64;
    updated_block: Cardinal;
    n_operation: Cardinal;
    name: TRawBytes;
    account_type: Word;
    previous_updated_block: Cardinal;
    Subaccounts : array of TSubAccount;
    ExtraData: TExtraData;
  end; }

  TMemTransactionBlock = record // TOperationBlock with less memory usage
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
    transactionHash: T32Bytes; // 32 direct bytes instead of use an AnsiString (-8 bytes)
    proof_of_work: T32Bytes; // 32 direct bytes instead of use an AnsiString (-8 bytes)
  end;

  TMemBlockAccount = record // TBlockAccount with less memory usage
    blockchainInfo: TMemTransactionBlock;
    accounts: array [0 .. CT_AccountsPerBlock - 1] of TMemAccount;
    block_hash: T32Bytes; // 32 direct bytes instead of use an AnsiString (-8 bytes)
    accumulatedWork: UInt64;
  end;

type
  PBlockAccount = ^TMemBlockAccount;

const
  CT_AccountInfo_NUL: TAccountInfo = (state: as_Unknown; AccountKey: (EC_OpenSSL_NID: 0; x: ''; y: '');
    locked_until_block: 0; price: 0; account_to_pay: 0; new_publicKey: (EC_OpenSSL_NID: 0; x: ''; y: ''));

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

class function TAccount.Empty: TAccount;
begin
  Result.AccountNumber := 0;
  Result.AccountInfo := CT_AccountInfo_NUL;
  Result.balance := 0;
  Result.updated_block := 0;
  Result.numberOfTransactions := 0;
  Result.name := '';
  Result.account_type := 0;
  Result.previous_updated_block := 0;
  Result.HasExtraData := false;
  {$IFDEF EXTENDEDACCOUNT}
    SetLength(Result.SubAccounts, 0);
    Result.ExtraData.DataType := 0;
    Result.ExtraData.ExtraType := 0;
    Result.ExtraData.Data := '';
  {$ENDIF}
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

class function TAccount.LoadFromStream(AStream: TStream;
  var RAccount: TAccount; ACurrentProtocol: shortint): boolean;
var
  s: AnsiString;
  xIsExtendedAccount : boolean;
  b: byte;
  i: integer;
begin
  Result := false;

  if AStream.Read(RAccount.AccountNumber, 4) < 4
  then exit;

  if TStreamOp.ReadAnsiString(AStream, s) < 0
  then exit;

  if s=''
  then xIsExtendedAccount := true
  else xIsExtendedAccount := false;

  if xIsExtendedAccount
  then if TStreamOp.ReadAnsiString(AStream, s) < 0
       then exit;

  RAccount.AccountInfo := TAccountInfo.FromRawString(s);

  if AStream.Read(RAccount.balance, Sizeof(UInt64)) < Sizeof(UInt64)
  then exit;
  if AStream.Read(RAccount.updated_block, 4) < 4
  then exit;
  if AStream.Read(RAccount.numberOfTransactions, 4) < 4
  then exit;
  if ACurrentProtocol >= CT_PROTOCOL_2 then
  begin
    if TStreamOp.ReadAnsiString(AStream, RAccount.name) < 0 then
      exit;
    if AStream.Read(RAccount.account_type, 2) < 2 then
      exit;
  end;
  //
  if AStream.Read(RAccount.previous_updated_block, 4) < 4 then
    exit;
  {$IFDEF EXTENDEDACCOUNT}
  if xIsExtendedAccount
  then begin
    RAccount.HasExtraData := true;
    if AStream.Read(b,1) < 1
    then exit;
    SetLength(RAccount.SubAccounts, b);
    if b > 0 then begin
      for i := 0 to b - 1
      do begin
        TStreamOp.ReadAccountKey(AStream, RAccount.SubAccounts[i].AccountKey);
        AStream.Read(RAccount.SubAccounts[i].DailyLimit, SizeOf(Int64));
        AStream.Read(RAccount.SubAccounts[i].TotalLimit, SizeOf(Int64));
        AStream.Read(RAccount.SubAccounts[i].Balance, SizeOf(UInt64));
        AStream.Read(RAccount.SubAccounts[i].Currency, SizeOf(Cardinal));
      end;
    end;
    AStream.Read(b, SizeOf(RAccount.ExtraData.DataType));
    RAccount.ExtraData.DataType := b;
    AStream.Read(RAccount.ExtraData.ExtraType, SizeOf(RAccount.ExtraData.ExtraType));
    TStreamOp.ReadAnsiString(AStream, RAccount.ExtraData.Data);
  end;
  {$ENDIF}
  Result := true;
end;

function TOrderedAccountList.Add(const Account: TAccount): Integer;
var
  P: PAccount;
begin
  if Find(Account.AccountNumber, Result) then
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
    P^.AccountInfo := Default(TAccountInfo);
    P^:=Default(TAccount);
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
    c := Int64(PAccount(FList[i]).AccountNumber) - Int64(account_number);
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
