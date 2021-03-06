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

uses Classes, Sysutils, MicroCoin.Account.AccountKey, UCrypto, MicroCoin.Common.Config, UBasetypes;

type

  TAccountState = (as_Unknown, as_Normal, as_ForSale);

  TAccountInfo = record
    State: TAccountState;
    AccountKey: TAccountKey;
    LockedUntilBlock: Cardinal; // 0 = Not locked
    Price: UInt64; // 0 = invalid price
    AccountToPay: Cardinal; // <> itself
    NewPublicKey: TAccountKey;

    function IsValid(var RErrors: AnsiString): Boolean;
    function IsAccountForSale: Boolean;
    function IsAccountForSaleAcceptingTransactions: Boolean;
    function ToRawString: TRawBytes; overload;
    function IsLocked(ABlockCount: Cardinal): Boolean;
    procedure ToRawString(var RDestination: TRawBytes); overload;

    class function FromRawString(const ARawAccountString: TRawBytes): TAccountInfo; overload; static;
    class procedure FromRawString(const ARawAccountString: TRawBytes; var dest: TAccountInfo); overload; static;
    class function EqualAccountInfos(const AAccountInfo1, AAccountInfo2: TAccountInfo): Boolean; static;
    class operator Equal(const AAccountInfo1, AAccountInfo2: TAccountInfo): Boolean;
    class operator NotEqual(const AAccountInfo1, AAccountInfo2: TAccountInfo): Boolean;
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
  public
    AccountNumber: Cardinal; // FIXED value. Account number
    AccountInfo: TAccountInfo;
    Balance: UInt64; // Balance, always >= 0
    UpdatedBlock: Cardinal; // Number of block where was updated
    NumberOfTransactions: Cardinal; // count number of owner operations (when receive, this is not updated)
    Name: TRawBytes; // Protocol 2. Unique name
    AccountType: Word; // Protocol 2. Layer 2 use case
    PreviusUpdatedBlock: Cardinal;
    class function AccountBlock(const AAccountNumber: Cardinal): Cardinal; static;
    class function Empty: TAccount; static;
    class function IsAccountBlockedByProtocol(AAccountNumber, ABlockCount: Cardinal): Boolean; static;
    class function AccountNumberToString(AAccountNumber: Cardinal): AnsiString;  static;
    class function ParseAccountNumber(const AStringValue: AnsiString; var RAccountNumber: Cardinal) : Boolean; overload; static;
    class function LoadFromStream(AStream : TStream; var RAccount : TAccount; ACurrentProtocol: shortint) : boolean; static;
  end;

  TOrderedAccountList = class
  private
    FList: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(const AAccount: TAccount): Integer;
    function Count: Integer;
    function Get(Index: Integer): TAccount; overload;
    function GetPointer(AIndex: Integer): PAccount; overload;
    function Find(const AAccountNumber: Cardinal; var RIndex: Integer): Boolean;
  end;

  TMemAccount = TAccount;

  TMemTransactionBlock = record // TOperationBlock with less memory usage
    // block number is discarded (-4 bytes)
    AccountKey: TDynRawBytes;
    Reward: UInt64;
    Fee: UInt64;
    ProtocolVersion: Word;
    ProtocolAvailable: Word;
    Timestamp: Cardinal;
    CompactTarget: Cardinal;
    nonce: Cardinal;
    BlockPayload: TDynRawBytes;
    InitialAccountStorageHash: T32Bytes; // 32 direct bytes instead of use an AnsiString (-8 bytes)
    TransactionHash: T32Bytes; // 32 direct bytes instead of use an AnsiString (-8 bytes)
    ProofOfWork: T32Bytes; // 32 direct bytes instead of use an AnsiString (-8 bytes)
  end;

  TMemBlockAccount = record // TBlockAccount with less memory usage
    BlockChainInfo: TMemTransactionBlock;
    Accounts: array [0 .. cAccountsPerBlock - 1] of TMemAccount;
    BlockHash: T32Bytes; // 32 direct bytes instead of use an AnsiString (-8 bytes)
    AccumulatedWork: UInt64;
  end;

type
  PBlockAccount = ^TMemBlockAccount;

const
  CT_AccountInfo_NUL: TAccountInfo = (State: as_Unknown; AccountKey: (EC_OpenSSL_NID: 0; x: ''; y: '');
    LockedUntilBlock: 0; Price: 0; AccountToPay: 0; NewPublicKey: (EC_OpenSSL_NID: 0; x: ''; y: ''));

implementation

uses MicroCoin.Common.Stream;

function TAccountInfo.IsValid(var RErrors: AnsiString): Boolean;
var
  s: AnsiString;
begin
  RErrors := '';
  case State of
    as_Unknown:
      begin
        RErrors := 'Account state is unknown';
        Result := false;
      end;
    as_Normal:
      begin
        Result := AccountKey.IsValidAccountKey(RErrors);
      end;
    as_ForSale:
      begin
        if not AccountKey.IsValidAccountKey(s) then
          RErrors := RErrors + ' ' + s;
        Result := RErrors = '';
      end;
  else
    raise Exception.Create('DEVELOP ERROR 20170214-3');
  end;
end;

class operator TAccountInfo.NotEqual(const AAccountInfo1, AAccountInfo2: TAccountInfo): Boolean;
begin
  Result := not TAccountInfo.EqualAccountInfos(AAccountInfo1, AAccountInfo2);
end;

function TAccountInfo.ToRawString: TRawBytes;
begin
  ToRawString(Result);
end;

procedure TAccountInfo.ToRawString(var RDestination: TRawBytes);
var
  ms: TMemoryStream;
  w: Word;
begin
  case State of
    as_Normal:
      AccountKey.ToRawString(RDestination);
    as_ForSale:
      begin
        ms := TMemoryStream.Create;
        try
          w := cAccountInfo_ForSale;
          ms.Write(w, Sizeof(w));
          //
          ms.WriteAccountKey(AccountKey);
          ms.Write(LockedUntilBlock, Sizeof(LockedUntilBlock));
          ms.Write(Price, Sizeof(Price));
          ms.Write(AccountToPay, Sizeof(AccountToPay));
          ms.WriteAccountKey(NewPublicKey);
          SetLength(RDestination, ms.Size);
          ms.Position := 0;
          ms.Read(RDestination[1], ms.Size);
        finally
          ms.Free;
        end;
      end;
  else
    raise Exception.Create('DEVELOP ERROR 20170214-1');
  end;
end;

class operator TAccountInfo.Equal(const AAccountInfo1, AAccountInfo2: TAccountInfo): Boolean;
begin
  Result := TAccountInfo.EqualAccountInfos(AAccountInfo1, AAccountInfo2);
end;

class function TAccountInfo.EqualAccountInfos(const AAccountInfo1, AAccountInfo2: TAccountInfo): Boolean;
begin
  Result := (AAccountInfo1.State = AAccountInfo2.State) and
    (AAccountInfo1.AccountKey = AAccountInfo2.AccountKey) and
    (AAccountInfo1.LockedUntilBlock = AAccountInfo2.LockedUntilBlock) and (AAccountInfo1.Price = AAccountInfo2.Price)
    and (AAccountInfo1.AccountToPay = AAccountInfo2.AccountToPay) and
    (AAccountInfo1.NewPublicKey = AAccountInfo2.NewPublicKey);
end;

function TAccountInfo.IsLocked(ABlockCount: Cardinal): Boolean;
begin
  Result := (State = as_ForSale) and ((LockedUntilBlock) >= ABlockCount);
end;

class function TAccountInfo.FromRawString(const ARawAccountString: TRawBytes): TAccountInfo;
begin
  FromRawString(ARawAccountString, Result);
end;

class procedure TAccountInfo.FromRawString(const ARawAccountString: TRawBytes; var dest: TAccountInfo);
var
  ms: TMemoryStream;
  w: Word;
begin
  if length(ARawAccountString) = 0 then
  begin
    dest := CT_AccountInfo_NUL;
    exit;
  end;
  ms := TMemoryStream.Create;
  try
    ms.WriteBuffer(ARawAccountString[1], length(ARawAccountString));
    ms.Position := 0;
    if ms.Read(w, Sizeof(w)) <> Sizeof(w)
    then exit;
    case w of
      cNID_secp256k1, cNID_secp384r1, cNID_sect283k1, cNID_secp521r1:
        begin
          dest.State := as_Normal;
          TAccountKey.FromRawString(ARawAccountString, dest.AccountKey);
          dest.LockedUntilBlock := CT_AccountInfo_NUL.LockedUntilBlock;
          dest.Price := CT_AccountInfo_NUL.Price;
          dest.AccountToPay := CT_AccountInfo_NUL.AccountToPay;
          dest.NewPublicKey := CT_AccountInfo_NUL.NewPublicKey;
        end;
      cAccountInfo_ForSale:
        begin
          ms.ReadAccountKey(dest.AccountKey);
          ms.Read(dest.LockedUntilBlock, Sizeof(dest.LockedUntilBlock));
          ms.Read(dest.Price, Sizeof(dest.Price));
          ms.Read(dest.AccountToPay, Sizeof(dest.AccountToPay));
          ms.ReadAccountKey(dest.NewPublicKey);
          dest.State := as_ForSale;
        end;
    else raise Exception.Create('DEVELOP ERROR 20170214-2');
    end;
  finally
    ms.Free;
  end;
end;

function TAccountInfo.IsAccountForSale: Boolean;
begin
  Result := (State = as_ForSale);
end;

function TAccountInfo.IsAccountForSaleAcceptingTransactions: Boolean;
var
  errors: AnsiString;
begin
  Result := (State = as_ForSale) and (NewPublicKey.IsValidAccountKey(errors));
end;

class function TAccount.AccountBlock(const AAccountNumber: Cardinal): Cardinal;
begin
  Result := AAccountNumber div cAccountsPerBlock;
end;

class function TAccount.AccountNumberToString(AAccountNumber: Cardinal): AnsiString;
var
  an: Int64;
begin
  an := AAccountNumber; // Converting to int64 to prevent overflow when *101
  an := ((an * 101) mod 89) + 10;
  Result := inttostr(AAccountNumber) + '-' + inttostr(an);
end;

class function TAccount.ParseAccountNumber(const AStringValue: AnsiString;
  var RAccountNumber: Cardinal): Boolean;
var
  i: Integer;
  an, rn, anaux: Int64;
begin
  Result := false;

  if length(trim(AStringValue)) = 0
  then Exit;

  an := 0;
  i := 1;

  while (i <= length(AStringValue)) do
  begin
    if AStringValue[i] in ['0' .. '9']
    then an := (an * 10) + ord(AStringValue[i]) - ord('0')
    else break;
    inc(i);
  end;

  RAccountNumber := an;

  if (i > length(AStringValue))
  then begin
    Result := true;
    Exit;
  end;

  if (AStringValue[i] in ['-', '.', ' '])
  then inc(i);

  if length(AStringValue) - 1 < i
  then Exit;

  rn := StrToIntDef(Copy(AStringValue, i, length(AStringValue)), 0);

  anaux := ((an * 101) mod 89) + 10;

  Result := rn = anaux;

end;

class function TAccount.Empty: TAccount;
begin
  Result.AccountNumber := 0;
  Result.AccountInfo := CT_AccountInfo_NUL;
  Result.Balance := 0;
  Result.UpdatedBlock := 0;
  Result.NumberOfTransactions := 0;
  Result.Name := '';
  Result.AccountType := 0;
  Result.PreviusUpdatedBlock := 0;
end;

class function TAccount.IsAccountBlockedByProtocol(AAccountNumber, ABlockCount: Cardinal): Boolean;
var
  waitBlocks: Integer;
begin
  if ABlockCount >= cV2EnableAtBlock
  then waitBlocks := cMinimumBlocksToLiveAccountV2
  else waitBlocks := cMinimumBlocksToLiveAccount;
  if ABlockCount < waitBlocks
  then Result := true
  else Result := ((ABlockCount - waitBlocks) * cAccountsPerBlock) <= AAccountNumber;
end;

class function TAccount.LoadFromStream(AStream: TStream;
  var RAccount: TAccount; ACurrentProtocol: shortint): boolean;
var
  s: AnsiString;
begin
  Result := false;

  if AStream.Read(RAccount.AccountNumber, 4) < 4
  then exit;

  if AStream.ReadAnsiString(s) < 0
  then exit;

  RAccount.AccountInfo := TAccountInfo.FromRawString(s);

  if AStream.Read(RAccount.Balance, Sizeof(UInt64)) < Sizeof(UInt64)
  then Exit;
  if AStream.Read(RAccount.UpdatedBlock, 4) < 4
  then Exit;
  if AStream.Read(RAccount.NumberOfTransactions, 4) < 4
  then Exit;
  if ACurrentProtocol >= cPROTOCOL_2
  then begin
    if AStream.ReadAnsiString(RAccount.Name) < 0
    then Exit;
    if AStream.Read(RAccount.AccountType, 2) < 2
    then Exit;
  end;
  //
  if AStream.Read(RAccount.PreviusUpdatedBlock, 4) < 4
  then exit;
  Result := true;
end;

function TOrderedAccountList.Add(const AAccount: TAccount): Integer;
var
  P: PAccount;
begin
  if Find(AAccount.AccountNumber, Result)
  then PAccount(FList[Result])^ := AAccount
  else begin
    New(P);
    P^ := AAccount;
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

function TOrderedAccountList.Find(const AAccountNumber: Cardinal; var RIndex: Integer): Boolean;
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
    c := Int64(PAccount(FList[i]).AccountNumber) - Int64(AAccountNumber);
    if c < 0
    then L := i + 1
    else begin
      H := i - 1;
      if c = 0
      then begin
        Result := true;
        L := i;
      end;
    end;
  end;
  RIndex := L;
end;

function TOrderedAccountList.GetPointer(AIndex: Integer): PAccount;
begin
  Result := PAccount(FList.Items[AIndex]);
end;

function TOrderedAccountList.Get(Index: Integer): TAccount;
begin
  Result := GetPointer(index)^;
end;

end.
