unit MicroCoin.Transaction.Base;

interface

uses
  SysUtils, Classes, UAccounts, UCrypto, UConst;

type

  TTransactionData = Record
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
    class function Empty : TTransactionData; static;
  end;


  ITransaction = interface
    function SaveToStream(Stream: TStream; SaveExtendedData: Boolean): Boolean;
    function LoadFromStream(Stream: TStream; LoadExtendedData: Boolean) : Boolean;
    function SaveToStorage(Stream: TStream): Boolean;
    function LoadFromStorage(Stream: TStream; LoadProtocolV2: Boolean): Boolean;
    function SaveToNettransfer(Stream: TStream): Boolean;
    function LoadFromNettransfer(Stream: TStream): Boolean;

    function GetTransactionData(Block: Cardinal; Affected_account_number: Cardinal; var TransactionData: TTransactionData): Boolean;

    function DoOperation(AccountTransaction: TPCSafeBoxTransaction; var errors: AnsiString): Boolean;

    function GetBufferForOpHash(UseProtocolV2: Boolean): TRawBytes;

    function GetOperationAmount: Int64;
    function GetOperationFee: UInt64;
    function GetOperationPayload: TRawBytes;
    function GetSignerAccount: Cardinal;
    function GetDestinationAccount: Int64;
    function GetSellerAccount: Int64;
    function GetNumberOfOperations: Cardinal;
    function GetOpType : byte;

    function Get_Previous_Signer_updated_block : Cardinal;
    function Get_Previous_Destination_updated_block : Cardinal;
    function Get_Previous_Seller_updated_block : Cardinal;
    function Get_HasValidSignature : Boolean;
    function GetTag : integer;

    function Sha256: TRawBytes;

    function ToString : string;

    procedure Free;

    property Previous_Signer_updated_block: Cardinal read Get_Previous_Signer_updated_block;
    property Previous_Destination_updated_block: Cardinal read Get_Previous_Destination_updated_block;
    property Previous_Seller_updated_block: Cardinal read Get_Previous_Seller_updated_block;
    property HasValidSignature: Boolean read Get_HasValidSignature;

    property OperationFee : UInt64 read GetOperationFee;
    property OperationAmount : Int64 read GetOperationAmount;
    property OperationPayload : TRawBytes read GetOperationPayload;
    property SignerAccount : Cardinal read GetSignerAccount;
    property DestinationAccount : Int64 read GetDestinationAccount;
    property SellerAccount : Int64 read GetSellerAccount;
    property NumberOfOperations: Cardinal read GetNumberOfOperations;
    property OpType : byte read GetOpType;
    property Tag : integer read GetTag;

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
  Result.newKey.EC_OpenSSL_NID := 0;
  Result.newKey.x := '';
  Result.newKey.y := '';
  Result.time := 0;
  Result.OpSubtype := 0;
  Result.AffectedAccount := 0;
  Result.OperationTxt := '';
  Result.Amount:=0;
  Result.Fee := 0;
  Result.OriginalPayload := '';
  Result.PrintablePayload := '';
  Result.OperationHash := '';
  Result.OperationHash_OLD := '';
  Result.errors := '';
end;


end.
