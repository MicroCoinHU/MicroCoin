{==============================================================================|
| MicroCoin                                                                    |
| Copyright (c) 2017-2018 MicroCoin Developers                                 |
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
| File:       MicroCoin.Account.Editors.pas
| Created at: 2018-09-04
| Purpose:    Custom editors for Account Numbers
|==============================================================================}

unit MicroCoin.Account.Editors;

interface

uses
  System.SysUtils, System.Classes,
  Vcl.ExtCtrls, Forms, Vcl.Buttons, Graphics,
  MicroCoin.Forms.AccountSelectDialog,
  MicroCoin.Account.Data, Vcl.Imaging.pngimage, PngSpeedButton,
  Vcl.Controls, Vcl.StdCtrls, MicroCoin.Node.Node;

type
  TAccountEdit = class(TEdit)
  private
    FAccount: TAccount;
    procedure SetAccount(const Value: TAccount);
    { Private declarations }
    property Width;
    property Height;
  protected
    procedure KeyPress(var Key: Char); override;
    procedure Change; override;
    { Protected declarations }
  public
    constructor Create(AOwner : TComponent); override;
    { Public declarations }
  published
    property Account : TAccount read FAccount write SetAccount;
  end;

  TAccountEditor = class(TCustomPanel)
  private
    FAccountEdit: TAccountEdit;
    FButton : TPngSpeedButton;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  private
    FNode: TNode;
    FJustMyAccounts: Boolean;
    function GetAccounNumber: AnsiString;
    function GetAccount: TAccount;
    procedure SetAccountNumber(const Value: AnsiString);
    function GetTextHint: string;
    procedure SetTextHint(const Value: string);
    function GetGlyph: TPngImage;
    procedure SetGlyph(const Value: TPngImage);
    property Caption;
    property ShowCaption;
    property BorderStyle;
    property BevelKind;
    procedure OnButtonClick(Sender: TObject);
    property AccountEdit : TAccountEdit read FAccountEdit;
  published
    property Account : TAccount read GetAccount;
    property AccountNumber : AnsiString read GetAccounNumber write SetAccountNumber;
    property TextHint : string read GetTextHint write SetTextHint;
    property Width;
    property Height;
    property Align;
    property Glyph: TPngImage read GetGlyph write SetGlyph;
    property JustMyAccounts : Boolean read FJustMyAccounts write FJustMyAccounts default false;
  end;

  TEncryptionMode = (emNone, emTarget, emSource, emPassword);
  TEncryptedMemo = class(TCustomMemo)
  private
    FEncryptionMode: TEncryptionMode;
    FPassword: AnsiString;
    FAccount: TAccount;
    function GetEncryptedMessage: AnsiString;
  published
    property EncryptionMode : TEncryptionMode read FEncryptionMode write FEncryptionMode;
    property EncryptedMessage : AnsiString read GetEncryptedMessage;
    property Account : TAccount read FAccount write FAccount;
    property Password : AnsiString read FPassword write FPassword;
  end;

procedure Register;

implementation

uses UAES, UECIES;

procedure Register;
begin
  RegisterComponents('MicroCoin', [TAccountEdit]);
  RegisterComponents('MicroCoin', [TAccountEditor]);
  RegisterComponents('MicroCoin', [TEncryptedMemo]);
end;

{ TAccountEditor }

constructor TAccountEditor.Create(AOwner: TComponent);
begin
  inherited;
  Height := 21;
  Width := 100;
  BorderStyle := bsNone;
  BevelKind := bkNone;
  BevelEdges := [];
  BevelOuter := bvNone;
  BevelInner := bvNone;
  ShowCaption := false;
  Caption := '';
  FButton := TPngSpeedButton.Create(self);
  FButton.Caption := '';
  FButton.Name := 'SearchButton';
  FButton.Align := alRight;
  FAccountEdit := TAccountEdit.Create(self);
  FAccountEdit.Alignment := taRightJustify;
  FAccountEdit.Width := self.Width-40;
  FAccountEdit.Height := self.Height;
  FAccountEdit.Align := alClient;
  FButton.Parent := self;
  FAccountEdit.Parent := self;
  FAccountEdit.Name := 'Editor';
  FAccountEdit.Text := '';
  FButton.OnClick := OnButtonClick;
end;

destructor TAccountEditor.Destroy;
begin
  FreeAndNil(FButton);
  FreeAndNil(FAccountEdit);
  inherited;
end;

function TAccountEditor.GetAccounNumber: AnsiString;
begin
  Result := FAccountEdit.Text;
end;

function TAccountEditor.GetAccount: TAccount;
begin
 Result := FAccountEdit.Account;
end;

function TAccountEditor.GetGlyph: TPngImage;
begin
  Result:=FButton.PngImage;
end;

function TAccountEditor.GetTextHint: string;
begin
 Result := FAccountEdit.TextHint;
end;

procedure TAccountEditor.OnButtonClick(Sender: TObject);
var
  dialog : TAccountSelectDialog;
begin
  dialog := TAccountSelectDialog.Create(self.Owner);
  dialog.JustMyAccounts := FJustMyAccounts;
  if dialog.ShowModal = mrOk then begin
    FAccountEdit.Account := dialog.SelectedAccount;
  end;
  dialog.Free;
end;

procedure TAccountEditor.SetAccountNumber(const Value: AnsiString);
begin
  FAccountEdit.Text := Value;
end;

procedure TAccountEditor.SetGlyph(const Value: TPngImage);
begin
 if Value<>nil then FButton.Caption := ''
 else FButton.Caption := '...';
 FButton.PngImage := Value;
end;

procedure TAccountEditor.SetTextHint(const Value: string);
begin
  FAccountEdit.TextHint := Value;
end;

{ TAccountEdit }

procedure TAccountEdit.Change;
var
  accNumber : Cardinal;
begin
  inherited;
  if TAccount.AccountTxtNumberToAccountNumber(Text, accNumber)
  then begin
    FAccount := TNode.Node.Operations.BlockManager.AccountStorage.Account(accNumber);
  end;
end;

constructor TAccountEdit.Create(AOwner: TComponent);
begin
  inherited;
  StyleElements := [seClient, seBorder, seFont];
end;

procedure TAccountEdit.KeyPress(var Key: Char);
begin
  inherited;
  if not (Key in ['0'..'9', '-', #8, #9, #13, #10])
  then Key := #0;
  if (Key = '-') and (Pos('-', Text)>0)
  then Key := #0;
end;

procedure TAccountEdit.SetAccount(const Value: TAccount);
begin
  FAccount := Value;
  Text := TAccount.AccountNumberToAccountTxtNumber( Value.AccountNumber)
end;

{ TEncryptedMemo }

function TEncryptedMemo.GetEncryptedMessage: AnsiString;
begin
  if csDesigning in ComponentState
  then Exit('');

  if Trim(Text)<>'' then begin
    case FEncryptionMode of
      emNone:    Result := Text;
      emTarget:  Result := ECIESEncrypt(account.AccountInfo.AccountKey, Text);
      emSource:  Result := ECIESEncrypt(account.AccountInfo.AccountKey, Text);
      emPassword: Result := TAESComp.EVP_Encrypt_AES256(Text, FPassword);
    end;
  end else Result := '';
end;

end.
