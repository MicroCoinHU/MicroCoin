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
| File:       MicroCoin.Transaction.Transaction.pas                            |
| Created at: 2018-09-04                                                       |
| Purpose:    Base class for transactions                                      |
|==============================================================================}

unit MicroCoin.Transaction.Transaction;


{$ifdef FPC}
  {$mode delphi}
{$endif}


interface

uses Sysutils, classes, UCrypto, UbaseTypes, MicroCoin.Transaction.ITransaction, MicroCoin.Transaction.Base, MicroCoin.Common.Config, MicroCoin.Account.Transaction;

type
  TTransaction = class(TInterfacedObject, ITransaction)
  private
    Ftag: Integer;
    function Get_HasValidSignature: Boolean;
    function Get_Previous_Destination_updated_block: Cardinal;
    function Get_Previous_Seller_updated_block: Cardinal;
    function Get_Previous_Signer_updated_block: Cardinal;
    procedure Set_Previous_Destination_updated_block(const Value: Cardinal);
    procedure Set_Previous_Seller_updated_block(const Value: Cardinal);
    procedure Set_Previous_Signer_updated_block(const Value: Cardinal);
  protected
    FPrevious_Signer_updated_block: Cardinal;
    FPrevious_Destination_updated_block: Cardinal;
    FPrevious_Seller_updated_block: Cardinal;
    FHasValidSignature: Boolean;
    FBufferedSha256: TRawBytes;
    function GetAmount: Int64; virtual; abstract;
    function GetFee: UInt64; virtual; abstract;
    function GetPayload: TRawBytes; virtual; abstract;
    function GetSignerAccount: Cardinal; virtual; abstract;
    function GetDestinationAccount: Int64; virtual;
    function GetSellerAccount: Int64; virtual;
    function GetNumberOfTransactions: Cardinal; virtual; abstract;
    function GetTransactionType: Byte; virtual; abstract;
    function GetTag: Integer;
    procedure SetTag(Value: Integer);

  public
    constructor Create; virtual;
    procedure InitializeData; virtual;
    function GetBuffer(UseProtocolV2: Boolean): TRawBytes; virtual;
    function ApplyTransaction(AccountTransaction: TAccountTransaction; var errors: AnsiString): Boolean; virtual; abstract;
    procedure AffectedAccounts(list: TList); virtual; abstract;
    function GetTransactionData(Block: Cardinal; Affected_account_number: Cardinal;
      var TransactionData: TTransactionData): Boolean; virtual; abstract;

    function SaveToStream(Stream: TStream; SaveExtendedData: Boolean): Boolean; virtual; abstract;
    function LoadFromStream(Stream: TStream; LoadExtendedData: Boolean): Boolean; virtual; abstract;
    function SaveToNettransfer(Stream: TStream): Boolean;
    function LoadFromNettransfer(Stream: TStream): Boolean;
    function SaveToStorage(Stream: TStream): Boolean;
    function LoadFromStorage(Stream: TStream; LoadProtocolV2: Boolean): Boolean;

    function TransactionHash_OLD(Block: Cardinal): TRawBytes;
    function TransactionHash(Block: Cardinal): TRawBytes;
    class function DecodeOperationHash(const OperationHash: TRawBytes;
      var Block, Account, N_Operation: Cardinal): Boolean;
    class function FinalOperationHashAsHexa(const OperationHash: TRawBytes): AnsiString;

    function Sha256: TRawBytes;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    property Previous_Signer_updated_block: Cardinal read Get_Previous_Signer_updated_block
      write Set_Previous_Signer_updated_block;
    property Previous_Destination_updated_block: Cardinal read Get_Previous_Destination_updated_block
      write Set_Previous_Destination_updated_block;
    property Previous_Seller_updated_block: Cardinal read Get_Previous_Seller_updated_block
      write Set_Previous_Seller_updated_block;
    property HasValidSignature: Boolean read Get_HasValidSignature;

    property Fee: UInt64 read GetFee;
    property Amount: Int64 read GetAmount;
    property Payload: TRawBytes read GetPayload;
    property SignerAccount: Cardinal read GetSignerAccount;
    property DestinationAccount: Int64 read GetDestinationAccount;
    property SellerAccount: Int64 read GetSellerAccount;
    property NumberOfTransactions: Cardinal read GetNumberOfTransactions;
    property Tag: Integer read Ftag write Ftag;
    property TransactionType: Byte read GetTransactionType;
  end;

implementation

constructor TTransaction.Create;
begin
  FBufferedSha256 := '';
  InitializeData;
end;

function TTransaction.GetBuffer(UseProtocolV2: Boolean): TRawBytes;
var
  ms: TMemoryStream;
begin
  if (UseProtocolV2) then
  begin
    ms := TMemoryStream.Create;
    try
      SaveToStream(ms, false);
      ms.Position := 0;
      SetLength(Result, ms.Size);
      ms.ReadBuffer(Result[1], ms.Size);
    finally
      ms.Free;
    end;
  end
  else
    raise Exception.Create('ERROR DEV 20170426-1');
end;

class function TTransaction.DecodeOperationHash(const OperationHash: TRawBytes;
  var Block, Account, N_Operation: Cardinal): Boolean;
var
  ms: TMemoryStream;
begin
  Result := false;
  Account := 0;
  N_Operation := 0;
  if length(OperationHash) <> 32
  then exit;
  ms := TMemoryStream.Create;
  try
    ms.Write(OperationHash[1], length(OperationHash));
    ms.Position := 0;
    ms.Read(Block, 4);
    ms.Read(Account, 4);
    ms.Read(N_Operation, 4);
    Result := true;
  finally
    ms.Free;
  end;
end;

class function TTransaction.FinalOperationHashAsHexa(const OperationHash: TRawBytes): AnsiString;
begin
  Result := TBaseType.ToHexaString(Copy(OperationHash, 5, 28));
end;

procedure TTransaction.InitializeData;
begin
  Ftag := 0;
  FPrevious_Signer_updated_block := 0;
  FPrevious_Destination_updated_block := 0;
  FPrevious_Seller_updated_block := 0;
  FHasValidSignature := false;
  FBufferedSha256 := '';
end;

function TTransaction.LoadFromNettransfer(Stream: TStream): Boolean;
begin
  Result := LoadFromStream(Stream, false);
end;

function TTransaction.LoadFromStorage(Stream: TStream; LoadProtocolV2: Boolean): Boolean;
begin
  Result := false;
  if LoadFromStream(Stream, LoadProtocolV2) then
  begin
    if Stream.Size - Stream.Position < 8 then
      exit;
    Stream.Read(FPrevious_Signer_updated_block, Sizeof(FPrevious_Signer_updated_block));
    Stream.Read(FPrevious_Destination_updated_block, Sizeof(FPrevious_Destination_updated_block));
    if (LoadProtocolV2) then
    begin
      Stream.Read(FPrevious_Seller_updated_block, Sizeof(FPrevious_Seller_updated_block));
    end;
    Result := true;
  end;
end;

function TTransaction.TransactionHash_OLD(Block: Cardinal): TRawBytes;
{ OperationHash is a 32 bytes value.
  First 4 bytes (0..3) are Block in little endian
  Next 4 bytes (4..7) are Account in little endian
  Next 4 bytes (8..11) are N_Operation in little endian
  Next 20 bytes (12..31) are a RipeMD160 of the operation buffer to hash
  //
  This format is easy to undecode because include account and n_operation
}
var
  ms: TMemoryStream;
  _a, _o: Cardinal;
begin
  ms := TMemoryStream.Create;
  try
    ms.Write(Block, 4);
    _a := GetSignerAccount;
    _o := GetNumberOfTransactions;
    ms.Write(_a, 4);
    ms.Write(_o, 4);
    ms.WriteBuffer(TCrypto.DoRipeMD160_HEXASTRING(GetBuffer(false))[1], 20);
    SetLength(Result, ms.Size);
    ms.Position := 0;
    ms.Read(Result[1], ms.Size);
  finally
    ms.Free;
  end;
end;

function TTransaction._AddRef: Integer;
begin
  inherited;
end;

function TTransaction._Release: Integer;
begin
  inherited;
end;

function TTransaction.TransactionHash(Block: Cardinal): TRawBytes;
{ OperationHash is a 32 bytes value.
  First 4 bytes (0..3) are Block in little endian
  Next 4 bytes (4..7) are Account in little endian
  Next 4 bytes (8..11) are N_Operation in little endian
  Next 20 bytes (12..31) are a RipeMD160 of the SAME data used to calc Sha256
  //
  This format is easy to undecode because include account and n_operation
}
var
  ms: TMemoryStream;
  _a, _o: Cardinal;
begin
  ms := TMemoryStream.Create;
  try
    ms.Write(Block, 4); // Save block (4 bytes)
    _a := GetSignerAccount;
    _o := GetNumberOfTransactions;
    ms.Write(_a, 4); // Save Account (4 bytes)
    ms.Write(_o, 4); // Save N_Operation (4 bytes)
    ms.WriteBuffer(TCrypto.DoRipeMD160AsRaw(GetBuffer(true))[1], 20);
    // Calling GetBufferForOpHash(TRUE) is the same than data used for Sha256
    SetLength(Result, ms.Size);
    ms.Position := 0;
    ms.Read(Result[1], ms.Size);
  finally
    ms.Free;
  end;
end;

function TTransaction.GetDestinationAccount: Int64;
begin
  Result := -1;
end;

function TTransaction.GetSellerAccount: Int64;
begin
  Result := -1;
end;

function TTransaction.GetTag: Integer;
begin
  Result := Ftag;
end;

function TTransaction.Get_HasValidSignature: Boolean;
begin
  Result := FHasValidSignature;
end;

function TTransaction.Get_Previous_Destination_updated_block: Cardinal;
begin
  Result := FPrevious_Destination_updated_block;
end;

function TTransaction.Get_Previous_Seller_updated_block: Cardinal;
begin
  Result := FPrevious_Seller_updated_block;
end;

function TTransaction.Get_Previous_Signer_updated_block: Cardinal;
begin
  Result := FPrevious_Signer_updated_block;
end;

function TTransaction.SaveToNettransfer(Stream: TStream): Boolean;
begin
  Result := SaveToStream(Stream, false);
end;

function TTransaction.SaveToStorage(Stream: TStream): Boolean;
begin
  Result := SaveToStream(Stream, true);
  Stream.Write(FPrevious_Signer_updated_block, Sizeof(FPrevious_Signer_updated_block));
  Stream.Write(FPrevious_Destination_updated_block, Sizeof(FPrevious_Destination_updated_block));
  Stream.Write(FPrevious_Seller_updated_block, Sizeof(FPrevious_Seller_updated_block));
  Result := true;
end;

procedure TTransaction.SetTag(Value: Integer);
begin
  Ftag := Value;
end;

procedure TTransaction.Set_Previous_Destination_updated_block(const Value: Cardinal);
begin
  FPrevious_Destination_updated_block := Value;
end;

procedure TTransaction.Set_Previous_Seller_updated_block(const Value: Cardinal);
begin
  FPrevious_Seller_updated_block := Value;
end;

procedure TTransaction.Set_Previous_Signer_updated_block(const Value: Cardinal);
begin
  FPrevious_Signer_updated_block := Value;
end;

function TTransaction.Sha256: TRawBytes;
begin
  if FBufferedSha256 = '' then
  begin
    FBufferedSha256 := TCrypto.DoSha256(GetBuffer(true));
  end;
  Result := FBufferedSha256;
end;

end.
