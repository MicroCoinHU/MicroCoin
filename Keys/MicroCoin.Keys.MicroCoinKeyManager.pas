unit MicroCoin.Keys.MicroCoinKeyManager;

interface

uses MicroCoin.Account.AccountKey, UCrypto, DBModule, MicroCoin.Account.Storage;

type

  PWalletKey = ^TWalletKey;
  TWalletKey = record
    id : integer;
    Name: AnsiString;
    AccountKey: TAccountKey;
    CryptedKey: TRawBytes;
    PrivateKey: TECPrivateKey;
    SearchableAccountKey: TRawBytes;
  end;

  TMicroCoinKeyManager = class(TObject)
    strict private
      FOrderedAccountKeysList: TOrderedAccountKeysList;
      FIsValidPassword: boolean;
      FWalletPassword: string;
      function GetCount: integer;
    public
      procedure SetName(Id: integer; ANewName : AnsiString);
      procedure Add(AKey : TWalletKey);
      function Get(id : integer) : TWalletKey;
      function IndexOfAccountKey(AccountKey: TAccountKey): Integer;
      function AddPrivateKey(const AName: AnsiString; ECPrivateKey: TECPrivateKey): Integer;
      function LockWallet : boolean;
      procedure Delete(id:integer);
      function AddPublicKey(const AName: AnsiString; ECDSA_Public: TECDSA_Public): Integer;
      constructor Create;
      property Key[id : integer] : TWalletKey read Get; default;
    published
      property IsValidPassword : boolean read FIsValidPassword write FIsValidPassword default true;
      property Count : integer read GetCount;
      property AccountsKeyList: TOrderedAccountKeysList read FOrderedAccountKeysList;
      property WalletPassword : string read FWalletPassword write FWalletPassword;
  end;

const
  CT_TWalletKey_NUL: TWalletKey = (name: ''; AccountKey: (EC_OpenSSL_NID: 0; x: ''; y: ''); CryptedKey: '';
    PrivateKey: nil; SearchableAccountKey: '');



implementation

uses ZDataset;

{ TMicroCoinKeyManager }

procedure TMicroCoinKeyManager.Add(AKey: TWalletKey);
begin
  DBModule.MicroCoinData.KeysTable.Append;
  with DBModule.MicroCoinData.KeysTable do begin
    FieldByName('key_name').Value := AKey.Name;
    FieldByName('x').Value := TCrypto.ToHexaString(AKey.AccountKey.x);
    FieldByName('y').Value := TCrypto.ToHexaString(AKey.AccountKey.y);
    FieldByName('key_type').Value := AKey.AccountKey.EC_OpenSSL_NID;
    FieldByName('d').Value := TCrypto.PrivateKey2Hexa(AKey.PrivateKey);
    Post;
    DBModule.MicroCoinData.KeysTable.Refresh;
  end;
end;

function TMicroCoinKeyManager.AddPrivateKey(const AName: AnsiString;
  ECPrivateKey: TECPrivateKey): Integer;
begin
  DBModule.MicroCoinData.KeysTable.Append;
  with DBModule.MicroCoinData.KeysTable do begin
    FieldByName('x').Value := TCrypto.ToHexaString( ECPrivateKey.PublicKey.x );
    FieldByName('y').Value := TCrypto.ToHexaString( ECPrivateKey.PublicKey.y );
    FieldByName('key_type').Value := ECPrivateKey.PublicKey.EC_OpenSSL_NID;
    FieldByName('d').Value := TCrypto.PrivateKey2Hexa(ECPrivateKey);
    Fields.FindField('key_name').Value := AName;
    Post;
    DBModule.MicroCoinData.KeysTable.Refresh;
  end;
end;

function TMicroCoinKeyManager.AddPublicKey(const AName: AnsiString;
  ECDSA_Public: TECDSA_Public): Integer;
begin
  DBModule.MicroCoinData.KeysTable.Append;
  with DBModule.MicroCoinData.KeysTable do begin
    FieldByName('key_name').Value := AName;
    FieldByName('x').Value := TCrypto.ToHexaString( ECDSA_Public.x );
    FieldByName('y').Value := TCrypto.ToHexaString( ECDSA_Public.y );
    FieldByName('key_type').Value := ECDSA_Public.EC_OpenSSL_NID;
    Post;
    DBModule.MicroCoinData.KeysTable.Refresh;
  end;
end;

constructor TMicroCoinKeyManager.Create;
begin
  FIsValidPassword := true;
end;

procedure TMicroCoinKeyManager.Delete(id: integer);
begin
  DBModule.MicroCoinData.KeysTable.Locate('id', id,[]);
  DBModule.MicroCoinData.KeysTable.Delete;
  DBModule.MicroCoinData.KeysTable.Refresh;
end;

function TMicroCoinKeyManager.Get(id: integer): TWalletKey;
begin
  Result.AccountKey.EC_OpenSSL_NID := 0;
  if DBModule.MicroCoinData.KeysTable.Locate('id', id, [])
  then begin
    with DBModule.MicroCoinData.KeysTable do begin
      Result.id := FieldByName('id').AsInteger;
      Result.Name := FieldByName('key_name').AsAnsiString;
      Result.AccountKey.EC_OpenSSL_NID := FieldByName('key_type').AsInteger;
      Result.AccountKey.x := TCrypto.HexaToRaw(FieldByName('x').AsAnsiString);
      Result.AccountKey.y := TCrypto.HexaToRaw(FieldByName('y').AsAnsiString);
      Result.PrivateKey := TECPrivateKey.Create;
      Result.PrivateKey.SetPrivateKeyFromHexa(Result.AccountKey.EC_OpenSSL_NID, FieldByName('d').AsAnsiString);
      Result.SearchableAccountKey := Result.PrivateKey.PublicKey.ToRawString;
      Result.CryptedKey := Result.PrivateKey.ExportToRaw;
    end;
  end;
end;

function TMicroCoinKeyManager.GetCount: integer;
begin
  Result := DBModule.MicroCoinData.KeysTable.RecordCount;
end;

function TMicroCoinKeyManager.IndexOfAccountKey(
  AccountKey: TAccountKey): Integer;
var
  xQuery : TZQuery;
begin
  xQuery := TZQuery.Create(nil);
  xQuery.Connection := DBModule.MicroCoinData.KeysConnection;
  xQuery.SQL.Text := 'select id from keys where x = :x and y = :y and key_type = :key_type';
  xQuery.ParamByName('x').Value := TCrypto.ToHexaString(AccountKey.x);
  xQuery.ParamByName('y').Value := TCrypto.ToHexaString(AccountKey.y);
  xQuery.ParamByName('key_type').Value := AccountKey.EC_OpenSSL_NID;
  xQuery.Open;
  xQuery.First;
  Result := xQuery.FieldByName('id').AsInteger;
  xQuery.Close;
  xQuery.Free;
  if Result = 0
  then Result := -1;
end;

function TMicroCoinKeyManager.LockWallet: boolean;
begin
  Result := true;
end;

procedure TMicroCoinKeyManager.SetName(Id: integer; ANewName: AnsiString);
begin
  DBModule.MicroCoinData.KeysTable.Locate('id', id,[]);
  DBModule.MicroCoinData.KeysTable.Edit;
  DBModule.MicroCoinData.KeysTable.FieldByName('key_name').Value := ANewName;
  DBModule.MicroCoinData.KeysTable.Post;
  DBModule.MicroCoinData.KeysTable.Refresh;
end;

end.
