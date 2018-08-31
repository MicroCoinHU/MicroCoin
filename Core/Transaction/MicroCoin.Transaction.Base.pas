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

uses SysUtils, Classes, UCrypto, UConst, MicroCoin.Account.AccountKey;

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
