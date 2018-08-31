unit UWalletKeys;

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

uses Classes, MicroCoin.BlockChain.BlockManager, MicroCoin.Account.AccountKey, UCrypto,
  MicroCoin.Account.Storage, MicroCoin.BlockChain.BlockHeader;

type
  TWalletKey = record
    Name: AnsiString;
    AccountKey: TAccountKey;
    CryptedKey: TRawBytes;
    PrivateKey: TECPrivateKey;
    SearchableAccountKey: TRawBytes;
  end;

  TWalletKeys = class(TComponent)
  strict private
    FSearchableKeys: TList;
    FFileName: AnsiString;
    FWalletPassword: AnsiString;
    FWalletFileStream: TFileStream;
    FIsValidPassword: Boolean;
    FWalletFileName: AnsiString;
    FIsReadingStream: Boolean;
    FOnChanged: TNotifyEvent;
    function GetKey(index: Integer): TWalletKey;
    procedure SetWalletPassword(const Value: AnsiString);
    procedure GeneratePrivateKeysFromPassword;
    procedure SetWalletFileName(const Value: AnsiString);
    function Find(const AccountKey: TAccountKey; var index: Integer): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor destroy; override;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure Delete(index: Integer); virtual;
    procedure Clear; virtual;
    procedure SetName(index: Integer; const newName: AnsiString);
    function AddPrivateKey(const Name: AnsiString; ECPrivateKey: TECPrivateKey): Integer; virtual;
    function AddPublicKey(const Name: AnsiString; ECDSA_Public: TECDSA_Public): Integer; virtual;
    function IndexOfAccountKey(AccountKey: TAccountKey): Integer;
    function Count: Integer;
    function LockWallet: Boolean;

    property IsValidPassword: Boolean read FIsValidPassword;
    property WalletPassword: AnsiString read FWalletPassword write SetWalletPassword;
    property WalletFileName: AnsiString read FWalletFileName write SetWalletFileName;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property Key[index: Integer]: TWalletKey read GetKey; default;
  end;

  TWalletKeysExt = class(TWalletKeys)
  strict private
    FOrderedAccountKeysList: TOrderedAccountKeysList;
    procedure SetAccountStorage(const Value: TAccountStorage);
    function GetAccountStorage: TAccountStorage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor destroy; override;
    function AddPrivateKey(const Name: AnsiString; ECPrivateKey: TECPrivateKey): Integer; override;
    function AddPublicKey(const Name: AnsiString; ECDSA_Public: TECDSA_Public): Integer; override;
    procedure Delete(index: Integer); override;
    procedure Clear; override;
    //
    property AccountsKeyList: TOrderedAccountKeysList read FOrderedAccountKeysList;
    property AccountStorage: TAccountStorage read GetAccountStorage write SetAccountStorage;
  end;

const
  CT_TWalletKey_NUL: TWalletKey = (name: ''; AccountKey: (EC_OpenSSL_NID: 0; x: ''; y: ''); CryptedKey: '';
    PrivateKey: nil; SearchableAccountKey: '');

implementation

uses SysUtils, UConst, ULog, UAES;

const
  CT_PrivateKeyFile_Magic = 'TWalletKeys';
  CT_PrivateKeyFile_Version = 100;

  { TWalletKeys }

type
  PWalletKey = ^TWalletKey;

function TWalletKeys.AddPrivateKey(const Name: AnsiString; ECPrivateKey: TECPrivateKey): Integer;
var
  P: PWalletKey;
  s: AnsiString;
begin
  if not Find(ECPrivateKey.PublicKey, Result) then
  begin
    // Result is new position
    New(P);
    P^ := CT_TWalletKey_NUL;
    P^.Name := name;
    P^.AccountKey := ECPrivateKey.PublicKey;
    P^.CryptedKey := TAESComp.EVP_Encrypt_AES256(TCrypto.PrivateKey2Hexa(ECPrivateKey), WalletPassword);
    P^.PrivateKey := TECPrivateKey.Create;
    P^.PrivateKey.SetPrivateKeyFromHexa(ECPrivateKey.EC_OpenSSL_NID, TCrypto.PrivateKey2Hexa(ECPrivateKey));
    P^.SearchableAccountKey := ECPrivateKey.PublicKey.ToRawString;
    FSearchableKeys.Insert(Result, P);
  end
  else
  begin
    P := FSearchableKeys[Result];
    P^.Name := name;
  end;
  if not FIsReadingStream then
    SaveToStream(FWalletFileStream);
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TWalletKeys.AddPublicKey(const Name: AnsiString; ECDSA_Public: TECDSA_Public): Integer;
var
  P: PWalletKey;
begin
  if not Find(ECDSA_Public, Result) then
  begin
    // Result is new position
    New(P);
    P^ := CT_TWalletKey_NUL;
    P^.Name := name;
    P^.AccountKey := ECDSA_Public;
    P^.PrivateKey := nil;
    P^.SearchableAccountKey := ECDSA_Public.ToRawString;
    FSearchableKeys.Insert(Result, P);
  end
  else
  begin
    P := FSearchableKeys[Result];
    P^.Name := name;
  end;
  if not FIsReadingStream then
    SaveToStream(FWalletFileStream);
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TWalletKeys.Clear;
var
  P: PWalletKey;
  i: Integer;
begin
  for i := FSearchableKeys.Count - 1 downto 0 do
  begin
    P := FSearchableKeys[i];
    FreeAndNil(P^.PrivateKey);
    Dispose(P);
  end;
  FSearchableKeys.Clear;
  FIsValidPassword := true;
end;

function TWalletKeys.Count: Integer;
begin
  Result := FSearchableKeys.Count;
end;

constructor TWalletKeys.Create(AOwner: TComponent);
begin
  inherited;
  FIsValidPassword := false;
  FWalletFileStream := nil;
  FWalletPassword := '';
  FSearchableKeys := TList.Create;
  FIsReadingStream := false;
  FOnChanged := nil;
end;

procedure TWalletKeys.Delete(index: Integer);
var
  P: PWalletKey;
begin
  P := FSearchableKeys[index];
  FreeAndNil(P^.PrivateKey);
  Dispose(P);
  FSearchableKeys.Delete(index);
  SaveToStream(FWalletFileStream);
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

destructor TWalletKeys.destroy;
begin
  FOnChanged := nil;
  FreeAndNil(FWalletFileStream);
  Clear;
  FreeAndNil(FSearchableKeys);
  inherited;
end;

function TWalletKeys.Find(const AccountKey: TAccountKey; var index: Integer): Boolean;
var
  L, H, i, C: Integer;
  rak: TRawBytes;
begin
  Result := false;
  rak := AccountKey.ToRawString;
  L := 0;
  H := FSearchableKeys.Count - 1;
  while L <= H do
  begin
    i := (L + H) shr 1;
    C := CompareStr(PWalletKey(FSearchableKeys[i]).SearchableAccountKey, rak);
    if C < 0 then
      L := i + 1
    else
    begin
      H := i - 1;
      if C = 0 then
      begin
        Result := true;
        L := i;
      end;
    end;
  end;
  index := L;
end;

procedure TWalletKeys.GeneratePrivateKeysFromPassword;
var
  i: Integer;
  P: PWalletKey;
  s: TRawBytes;
  isOk: Boolean;
begin
  FIsValidPassword := false;
  isOk := true;
  for i := 0 to FSearchableKeys.Count - 1 do
  begin
    P := FSearchableKeys[i];
    FreeAndNil(P^.PrivateKey);
  end;
  // try to unencrypt
  for i := 0 to FSearchableKeys.Count - 1 do
  begin
    P := FSearchableKeys[i];
    if P^.CryptedKey <> '' then
    begin
      isOk := TAESComp.EVP_Decrypt_AES256(P^.CryptedKey, FWalletPassword, s);
      if isOk then
      begin
        P^.PrivateKey := TECPrivateKey.Create;
        try
          P^.PrivateKey.SetPrivateKeyFromHexa(P^.AccountKey.EC_OpenSSL_NID, s);
        except
          on E: Exception do
          begin
            P^.PrivateKey.Free;
            P^.PrivateKey := nil;
            isOk := false;
            TLog.NewLog(lterror, ClassName, Format('Fatal error when generating EC private key %d/%d: %s',
              [i + 1, FSearchableKeys.Count, E.Message]));
            // disabled exit... continue: exit;
          end;
        end;
      end;
    end;
  end;
  FIsValidPassword := isOk;
end;

function TWalletKeys.GetKey(index: Integer): TWalletKey;
begin
  Result := PWalletKey(FSearchableKeys[index])^;
end;

function TWalletKeys.IndexOfAccountKey(AccountKey: TAccountKey): Integer;
begin
  if not Find(AccountKey, Result) then
    Result := -1;
end;

procedure TWalletKeys.LoadFromStream(Stream: TStream);
var
  fileversion, i, L, j: Integer;
  s: AnsiString;
  P: PWalletKey;
  wk: TWalletKey;
begin
  Clear;
  FIsValidPassword := false;
  FIsReadingStream := true;
  try
    if Stream.Size - Stream.Position > 0 then
    begin
      TStreamOp.ReadAnsiString(Stream, s);
      if not AnsiSameStr(s, CT_PrivateKeyFile_Magic) then
        raise Exception.Create('Invalid ' + ClassName + ' stream');
      // Read version:
      Stream.Read(fileversion, 4);
      if (fileversion <> CT_PrivateKeyFile_Version) then
      begin
        // Old version
        Stream.Position := Stream.Position - 4;
        TLog.NewLog(lterror, ClassName, 'Invalid PrivateKeys file version: ' + Inttostr(fileversion));
      end;
      Stream.Read(L, 4);
      for i := 0 to L - 1 do
      begin
        wk := CT_TWalletKey_NUL;
        TStreamOp.ReadAnsiString(Stream, wk.Name);
        Stream.Read(wk.AccountKey.EC_OpenSSL_NID, sizeof(wk.AccountKey.EC_OpenSSL_NID));
        TStreamOp.ReadAnsiString(Stream, wk.AccountKey.x);
        TStreamOp.ReadAnsiString(Stream, wk.AccountKey.y);
        TStreamOp.ReadAnsiString(Stream, wk.CryptedKey);
        wk.PrivateKey := nil;
        j := AddPublicKey(wk.Name, wk.AccountKey);
        P := PWalletKey(FSearchableKeys[j]);
        P^.CryptedKey := wk.CryptedKey; // Adding encrypted data
      end;
    end;
    GeneratePrivateKeysFromPassword;
  finally
    FIsReadingStream := false;
  end;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TWalletKeys.LockWallet: Boolean;
begin
  // Return true when wallet has a password, locking it. False if there password is empty string
  FWalletPassword := '';
  GeneratePrivateKeysFromPassword;
  Result := not IsValidPassword;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TWalletKeys.SaveToStream(Stream: TStream);
var
  i: Integer;
  P: PWalletKey;
begin
  if FIsReadingStream then
    exit;
  if not Assigned(Stream) then
    exit;
  Stream.Size := 0;
  Stream.Position := 0;
  TStreamOp.WriteAnsiString(Stream, CT_PrivateKeyFile_Magic);
  i := CT_PrivateKeyFile_Version;
  Stream.Write(i, 4);
  i := FSearchableKeys.Count;
  Stream.Write(i, 4);
  for i := 0 to FSearchableKeys.Count - 1 do
  begin
    P := FSearchableKeys[i];
    TStreamOp.WriteAnsiString(Stream, P^.Name);
    Stream.Write(P^.AccountKey.EC_OpenSSL_NID, sizeof(P^.AccountKey.EC_OpenSSL_NID));
    TStreamOp.WriteAnsiString(Stream, P^.AccountKey.x);
    TStreamOp.WriteAnsiString(Stream, P^.AccountKey.y);
    TStreamOp.WriteAnsiString(Stream, P^.CryptedKey);
  end;
end;

procedure TWalletKeys.SetName(index: Integer; const newName: AnsiString);
begin
  if PWalletKey(FSearchableKeys[index])^.Name = newName then
    exit;
  PWalletKey(FSearchableKeys[index])^.Name := newName;
  SaveToStream(FWalletFileStream);
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TWalletKeys.SetWalletFileName(const Value: AnsiString);
var
  fm: Word;
begin
  if FWalletFileName = Value then
    exit;
  FWalletFileName := Value;
  if Assigned(FWalletFileStream) then
    FWalletFileStream.Free;
  FWalletFileStream := nil;
  if Value <> '' then
  begin
    if FileExists(Value) then
      fm := fmOpenReadWrite
    else
      fm := fmCreate;
    FWalletFileStream := TFileStream.Create(WalletFileName, fm + fmShareDenyWrite);
    FWalletFileStream.Position := 0;
    LoadFromStream(FWalletFileStream);
  end;
end;

procedure TWalletKeys.SetWalletPassword(const Value: AnsiString);
var
  i: Integer;
  P: PWalletKey;
begin
  if FWalletPassword = Value then
    exit;
  FWalletPassword := Value;
  for i := 0 to FSearchableKeys.Count - 1 do
  begin
    P := FSearchableKeys[i];
    if Assigned(P^.PrivateKey) then
    begin
      P^.CryptedKey := TAESComp.EVP_Encrypt_AES256(TCrypto.PrivateKey2Hexa(P^.PrivateKey), FWalletPassword);
    end
    else
    begin
      if FIsValidPassword then
      begin
        TLog.NewLog(lterror, ClassName, Format('Fatal error: Private key not found %d/%d',
          [i + 1, FSearchableKeys.Count]));
      end;
      FIsValidPassword := false;
    end;
  end;
  // Try if password is Ok
  GeneratePrivateKeysFromPassword;
  if FIsValidPassword then
    SaveToStream(FWalletFileStream);
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

{ TWalletKeysExt }

function TWalletKeysExt.AddPrivateKey(const Name: AnsiString; ECPrivateKey: TECPrivateKey): Integer;
begin
  Result := inherited AddPrivateKey(name, ECPrivateKey);
  if Assigned(FOrderedAccountKeysList) then
  begin
    FOrderedAccountKeysList.AddAccountKey(ECPrivateKey.PublicKey);
  end;
end;

function TWalletKeysExt.AddPublicKey(const Name: AnsiString; ECDSA_Public: TECDSA_Public): Integer;
begin
  Result := inherited AddPublicKey(name, ECDSA_Public);
  if Assigned(FOrderedAccountKeysList) then
  begin
    FOrderedAccountKeysList.AddAccountKey(ECDSA_Public);
  end;
end;

procedure TWalletKeysExt.Clear;
begin
  inherited;
  if Assigned(FOrderedAccountKeysList) then
  begin
    FOrderedAccountKeysList.Clear;
  end;
end;

constructor TWalletKeysExt.Create(AOwner: TComponent);
begin
  inherited;
  FOrderedAccountKeysList := nil;
end;

procedure TWalletKeysExt.Delete(index: Integer);
begin
  if Assigned(FOrderedAccountKeysList) then
  begin
    FOrderedAccountKeysList.RemoveAccountKey(Key[index].AccountKey);
  end;
  inherited;
end;

destructor TWalletKeysExt.destroy;
begin
  FreeAndNil(FOrderedAccountKeysList);
  inherited;
end;

function TWalletKeysExt.GetAccountStorage: TAccountStorage;
begin
  Result := nil;
  if Assigned(FOrderedAccountKeysList) then
  begin
    Result := FOrderedAccountKeysList.AccountStorage;
  end;
end;

procedure TWalletKeysExt.SetAccountStorage(const Value: TAccountStorage);
var
  i: Integer;
begin
  if Assigned(FOrderedAccountKeysList) then
  begin
    if FOrderedAccountKeysList.AccountStorage <> Value then
      FreeAndNil(FOrderedAccountKeysList)
    else
      exit;
  end;
  if Assigned(Value) then
  begin
    // Initialize
    FOrderedAccountKeysList := TOrderedAccountKeysList.Create(Value, false);
    for i := 0 to Count - 1 do
    begin
      FOrderedAccountKeysList.AddAccountKey(Key[i].AccountKey);
    end;
  end;
end;

end.
