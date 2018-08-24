unit UAccounts;

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

uses
  Classes, UConst, UCrypto, SyncObjs, UThread, UBaseTypes, UCommon,
  MicroCoin.Common, MicroCoin.Common.Lists, MicroCoin.Account.AccountKey,
  MicroCoin.Account, MicroCoin.Account.Storage, MicroCoin.BlockChain.Blockheader,
  MicroCoin.Transaction.TransactionList;

{$I config.inc}



  { TMicroCoinProtocol }

  {
    Protocol 2:
    Introducing OperationBlock info on the safebox, this will allow checkpointing a safebox because
    each row of the safebox (TBlockAccount) will have data about how to calculate
    its PoW, so next row will use row-1 info to check it's good generated thanks to PoW

    This solution does not include operations, but include operations_hash value,
    that is a SHA256 of operations.

    If someone wants to change the safebox and spam, will need to find values
    to alter safebox accounts of last checkpoint and also find new blocks prior
    to honest nodes, that will be only accepted by nodes that does not have
    last blocks (only fresh nodes). This is a very hard job and not efficient
    because usually there will be few new fresh nodes per period, so only
    can spam new fresh nodes because old nodes does not need to download
    a checkpointing.
    This solution was created by Herman Schoenfeld (Thanks!)
  }


  { Estimated TAccount size:
    4 + 200 (max aprox) + 8 + 4 + 4 = 220 max aprox
    Estimated TBlockAccount size:
    4 + (5 * 220) + 4 + 32 = 1140 max aprox
  }



  // Maintains a TRawBytes (AnsiString) list ordered to quick search withoud duplicates

  // SafeBox is a box that only can be updated using SafeBoxTransaction, and this
  // happens only when a new BlockChain is included. After this, a new "SafeBoxHash"
  // is created, so each SafeBox has a unique SafeBoxHash

  { TPCSafeBox }



  { TAccountTransaction }


implementation

uses
  SysUtils, ULog, UOpenSSLdef, UOpenSSL;

{ TMicroCoinProtocol }


{ TPCSafeBox }

// New on version 2: To reduce mem usage
{$DEFINE uselowmem}

{$IFDEF uselowmem}


  { In order to store less memory on RAM, those types will be used
    to store in RAM memory (better than to use original ones)
    This will reduce 10-15% of memory usage.
    For future versions, will be a good solution to use those instead
    of originals, but }


{$ELSE}


type
  PBlockAccount = ^TBlockAccount;
  TMemAccount = TAccount;
  TMemBlockAccount = TBlockAccount;
{$ENDIF}


end.
