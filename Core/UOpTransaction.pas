unit UOpTransaction;

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

Uses
  UCrypto, UBlockChain, Classes,
  UAccounts, MicroCoin.Transaction.Transaction,
  MicroCoin.Transaction.Base;

  // NEW OPERATIONS PROTOCOL 2
Procedure RegisterOperationsClass;

implementation

uses
  SysUtils, UConst, ULog;

Procedure RegisterOperationsClass;
Begin
End;

initialization

RegisterOperationsClass;
end.
