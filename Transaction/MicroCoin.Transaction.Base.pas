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
| File:       MicroCoin.Transaction.Base.pas
| Created at: 2018-09-04
| Purpose:    Transaction base data structure
|==============================================================================}

unit MicroCoin.Transaction.Base;

{$ifdef FPC}
  {$mode delphi}
{$endif}

interface

uses SysUtils, Classes, UCrypto, UBaseTypes, MicroCoin.Common.Config, MicroCoin.Account.AccountKey;

const
  CT_Op_Transaction = $01;
  CT_Op_Changekey = $02;
  CT_Op_Recover = $03;
  CT_Op_ListAccountForSale = $04;
  CT_Op_DelistAccount = $05;
  CT_Op_BuyAccount = $06;
  CT_Op_ChangeKeySigned = $07;
  CT_Op_ChangeAccountInfo = $08;
  CT_Op_CreateSubAccount = $09;
  CT_Op_Transaction_Extended = $0A;
  CT_Op_Data = $0B;

  CT_OpSubtype_TransactionSender          = 11;
  CT_OpSubtype_TransactionReceiver        = 12;
  CT_OpSubtype_BuyTransactionBuyer        = 13;
  CT_OpSubtype_BuyTransactionTarget       = 14;
  CT_OpSubtype_BuyTransactionSeller       = 15;
  CT_OpSubtype_ChangeKey                  = 21;
  CT_OpSubtype_Recover                    = 31;
  CT_OpSubtype_ListAccountForPublicSale   = 41;
  CT_OpSubtype_ListAccountForPrivateSale  = 42;
  CT_OpSubtype_DelistAccount              = 51;
  CT_OpSubtype_BuyAccountBuyer            = 61;
  CT_OpSubtype_BuyAccountTarget           = 62;
  CT_OpSubtype_BuyAccountSeller           = 63;
  CT_OpSubtype_ChangeKeySigned            = 71;
  CT_OpSubtype_ChangeAccountInfo          = 81;
  CT_OpSubtype_CreateSubAccount           = 91;

type

  TTransactionData = record
    valid: Boolean;
    Block: Cardinal;
    NOpInsideBlock: Integer;
    transactionType: Word;
    transactionSubtype: Word;
    time: Cardinal;
    AffectedAccount: Cardinal;
    SignerAccount: Int64; // Is the account that executes this operation
    DestAccount: Int64; //
    SellerAccount: Int64; // Protocol 2 - only used when is a pay to transaction
    newKey: TAccountKey;
    TransactionAsString: AnsiString;
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
  Result.transactionType := 0;
  Result.Block := 0;
  Result.SignerAccount := -1;
  Result.DestAccount := -1;
  Result.SellerAccount := -1;
  Result.AffectedAccount := 0;
  Result.newKey.EC_OpenSSL_NID := 0;
  Result.newKey.x := '';
  Result.newKey.y := '';
  Result.time := 0;
  Result.transactionSubtype := 0;
  Result.AffectedAccount := 0;
  Result.TransactionAsString := '';
  Result.Amount := 0;
  Result.Fee := 0;
  Result.OriginalPayload := '';
  Result.PrintablePayload := '';
  Result.OperationHash := '';
  Result.OperationHash_OLD := '';
  Result.errors := '';
end;

end.
