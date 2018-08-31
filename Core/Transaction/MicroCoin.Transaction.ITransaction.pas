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
| File:       MicroCoin.Transaction.ITransaction.pas                           |
| Created at: 2018-08-31                                                       |
| Purpose:    Base interface definition for transactions                       |
| Todo:                                                                        |
|   - Fix naming violations                                                    |
|==============================================================================}

unit MicroCoin.Transaction.ITransaction;

interface

uses Sysutils, classes, MicroCoin.Transaction.Base, MicroCoin.Account.Transaction, UCrypto;

type
  ITransaction = interface
    ['{54C0B36D-912C-4CC9-B73E-4B8BACB2E053}']
    function SaveToStream(Stream: TStream; SaveExtendedData: Boolean): Boolean;
    function LoadFromStream(Stream: TStream; LoadExtendedData: Boolean): Boolean;
    function SaveToStorage(Stream: TStream): Boolean;
    function LoadFromStorage(Stream: TStream; LoadProtocolV2: Boolean): Boolean;
    function SaveToNettransfer(Stream: TStream): Boolean;
    function LoadFromNettransfer(Stream: TStream): Boolean;

    function GetTransactionData(Block: Cardinal; Affected_account_number: Cardinal;
      var TransactionData: TTransactionData): Boolean;

    function ApplyTransaction(AccountTransaction: TAccountTransaction; var errors: AnsiString): Boolean;

    function GetBufferForOpHash(UseProtocolV2: Boolean): TRawBytes;

    function GetAmount: Int64;
    function GetFee: UInt64;
    function GetPayload: TRawBytes;
    function GetSignerAccount: Cardinal;
    function GetDestinationAccount: Int64;
    function GetSellerAccount: Int64;
    function GetNumberOfTransactions: Cardinal;
    function GetOpType: byte;

    function Get_Previous_Signer_updated_block: Cardinal;
    function Get_Previous_Destination_updated_block: Cardinal;
    function Get_Previous_Seller_updated_block: Cardinal;

    procedure Set_Previous_Destination_updated_block(const Value: Cardinal);
    procedure Set_Previous_Seller_updated_block(const Value: Cardinal);
    procedure Set_Previous_Signer_updated_block(const Value: Cardinal);

    function Get_HasValidSignature: Boolean;
    function GetTag: Integer;
    procedure SetTag(Value: Integer);

    function Sha256: TRawBytes;

    function TransactionHash(Block: Cardinal): TRawBytes;
    function TransactionHash_OLD(Block: Cardinal): TRawBytes;

    procedure AffectedAccounts(list: TList);
    procedure InitializeData;

    function ToString: string;

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
    property OpType: byte read GetOpType;
    property Tag: Integer read GetTag write SetTag;

  end;

implementation

end.
