unit MicroCoin.Transaction.Base;

{
  This unit contains code from PascalCoin:

  Copyright (c) Albert Molina 2016 - 2018 original code from PascalCoin https://pascalcoin.org/

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

}

{$ifdef FPC}
  {$mode delphi}
{$endif}

interface

uses SysUtils, Classes, UCrypto, UConst, MicroCoin.Account.AccountKey,
  MicroCoin.Account.Transaction;

type

  TTransactionData = record
    valid: Boolean;
    Block: Cardinal;
    NOpInsideBlock: Integer;
    OpType: Word;
    OpSubtype: Word;
    time: Cardinal;
    AffectedAccount: Cardinal;
    SignerAccount: Int64; // Is the account that executes this operation
    DestAccount: Int64; //
    SellerAccount: Int64; // Protocol 2 - only used when is a pay to transaction
    newKey: TAccountKey;
    OperationTxt: AnsiString;
    Amount: Int64;
    Fee: Int64;
    Balance: Int64;
    OriginalPayload: TRawBytes;
    PrintablePayload: AnsiString;
    OperationHash: TRawBytes;
    OperationHash_OLD: TRawBytes; // Will include old oeration hash value
    errors: AnsiString;
    class function Empty: TTransactionData; static;
  end;

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

class function TTransactionData.Empty: TTransactionData;
begin
  Result.valid := false;
  Result.NOpInsideBlock := -1;
  Result.OpType := 0;
  Result.Block := 0;
  Result.SignerAccount := -1;
  Result.DestAccount := -1;
  Result.SellerAccount := -1;
  Result.AffectedAccount := 0;
  Result.newKey.EC_OpenSSL_NID := 0;
  Result.newKey.x := '';
  Result.newKey.y := '';
  Result.time := 0;
  Result.OpSubtype := 0;
  Result.AffectedAccount := 0;
  Result.OperationTxt := '';
  Result.Amount := 0;
  Result.Fee := 0;
  Result.OriginalPayload := '';
  Result.PrintablePayload := '';
  Result.OperationHash := '';
  Result.OperationHash_OLD := '';
  Result.errors := '';
end;

end.
