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
| File:       MicroCoin.Keys.KeyManager.pas                                    |
| Created at: 2018-09-04                                                       |
| Purpose:    Private and public key managment                                 |
|==============================================================================}

{$ifdef FPC}
 {$mode delphi}
{$endif}

unit MicroCoin.Keys.KeyManager;

interface

uses SysUtils, Classes, MicroCoin.Crypto.Keys, MicroCoin.BlockChain.BlockManager, MicroCoin.Account.AccountKey, UCrypto,
  MicroCoin.Account.Storage, MicroCoin.BlockChain.BlockHeader, UWalletKeys;

type
  TKeyManager = class(TWalletKeys)
  strict private
    FOrderedAccountKeysList: TOrderedAccountKeysList;
    procedure SetAccountStorage(const Value: TAccountStorage);
    function GetAccountStorage: TAccountStorage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddPrivateKey(const Name: AnsiString; ECPrivateKey: TECKeyPair): Integer; override;
    function AddPublicKey(const Name: AnsiString; ECDSA_Public: TECPublicKey): Integer; override;
    procedure Delete(index: Integer); override;
    procedure Clear; override;
    property AccountsKeyList: TOrderedAccountKeysList read FOrderedAccountKeysList;
    property AccountStorage: TAccountStorage read GetAccountStorage write SetAccountStorage;
  end;

implementation

{ TWalletKeysExt }

function TKeyManager.AddPrivateKey(const Name: AnsiString; ECPrivateKey: TECKeyPair): Integer;
begin
  Result := inherited AddPrivateKey(name, ECPrivateKey);
  if Assigned(FOrderedAccountKeysList) then
  begin
    FOrderedAccountKeysList.AddAccountKey(ECPrivateKey.PublicKey);
  end;
end;

function TKeyManager.AddPublicKey(const Name: AnsiString; ECDSA_Public: TECPublicKey): Integer;
begin
  Result := inherited AddPublicKey(name, ECDSA_Public);
  if Assigned(FOrderedAccountKeysList) then
  begin
    FOrderedAccountKeysList.AddAccountKey(ECDSA_Public);
  end;
end;

procedure TKeyManager.Clear;
begin
  inherited;
  if Assigned(FOrderedAccountKeysList) then
  begin
    FOrderedAccountKeysList.Clear;
  end;
end;

constructor TKeyManager.Create(AOwner: TComponent);
begin
  inherited;
  FOrderedAccountKeysList := nil;
end;

procedure TKeyManager.Delete(index: Integer);
begin
  if Assigned(FOrderedAccountKeysList) then
  begin
    FOrderedAccountKeysList.RemoveAccountKey(Key[index].AccountKey);
  end;
  inherited;
end;

destructor TKeyManager.destroy;
begin
  FreeAndNil(FOrderedAccountKeysList);
  inherited;
end;

function TKeyManager.GetAccountStorage: TAccountStorage;
begin
  Result := nil;
  if Assigned(FOrderedAccountKeysList) then
  begin
    Result := FOrderedAccountKeysList.AccountStorage;
  end;
end;

procedure TKeyManager.SetAccountStorage(const Value: TAccountStorage);
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
