unit UConst;

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

{$I config.inc}

{$IFNDEF FPC}

type
  PtrInt = integer;
  PtrUInt = cardinal;
{$ENDIF}

Const
  CT_Genesis_Magic_String_For_Old_Block_Hash :
    AnsiString = '(c) Peter Nemeth - Okes rendben okes';
  CT_Zero_Block_Proof_of_work_in_Hexa =
    {$IFDEF PRODUCTION}'0000007A35BA642D75EBDA692D25DA09AB8EABE802BC970C01DE08F60DC3F93F'{$ELSE}{$IFDEF TESTNET}''{$ELSE}{$ENDIF}{$ENDIF};


  CT_NetServer_Port = {$IFDEF PRODUCTION}4004{$ELSE}{$IFDEF TESTNET}4104{$ELSE}{$ENDIF}{$ENDIF};
  CT_JSONRPCMinerServer_Port = {$IFDEF PRODUCTION}4009{$ELSE}{$IFDEF TESTNET}4109{$ELSE}{$ENDIF}{$ENDIF};
  CT_JSONRPC_Port = {$IFDEF PRODUCTION}4003{$ELSE}{$IFDEF TESTNET}4103{$ELSE}{$ENDIF}{$ENDIF};
  CT_AccountsPerBlock = 5;

  CT_NewLineSecondsAvg: Cardinal = {$IFDEF PRODUCTION}300{$ELSE}{$IFDEF TESTNET}30{$ELSE}{$ENDIF}{$ENDIF};

  CT_FirstReward: UInt64 = 1000000; // 4 decimals... First reward = 100,0000
  CT_MinReward: UInt64 = 10000; // 4 decimals... Min reward = 1,0000
  CT_NewLineRewardDecrease: Cardinal = 420480; // Avg 4 year

  CT_WaitNewBlocksBeforeTransaction = 10;
  CT_WaitNewBlocksBeforeTransactionV2 = 100;
  CT_V2BlockNumber = 200000;

  CT_RecoverFoundsWaitInactiveCount = 420480;  // After 4 years... if an account has no operations, money will be a reward for a miner!
  CT_MaxFutureBlocksLockedAccount = 105120; // Maximum future blocks an account can be locked

  CT_MaxTransactionAmount = 1000000000000;
  CT_MaxTransactionFee = 100000000;
  CT_MaxWalletAmount = 10000000000000;

  CT_MinCompactTarget: Cardinal = {$IFDEF PRODUCTION}$19000000{$ELSE}{$IFDEF TESTNET}$19000000{$ELSE}{$ENDIF}{$ENDIF}; // First compact target of block 0

  CT_CalcNewTargetBlocksAverage: Cardinal = 100;
  CT_CalcNewTargetLimitChange_SPLIT = 10;

  CT_MaxAccount : Cardinal = $FFFFFFFF;
  CT_MaxBlock : Cardinal = $FFFFFFFF;

  CT_MaxPayloadSize = 255; // Max payload size in bytes
  CT_MaxFutureBlockTimestampOffset = 15;
  CT_MinNodesToCalcNAT = 4;

  CT_MinServersConnected = 3;
  CT_MaxServersConnected = 5;

  CT_MaxClientsConnected = 100;

  cSaveAccountStageOnBlocks = {$IFDEF PRODUCTION}100{$ELSE}10{$ENDIF};

  CT_NID_secp256k1 = 714;
  CT_NID_secp384r1 = 715;
  CT_NID_sect283k1 = 729;
  CT_NID_secp521r1 = 716;

  CT_Default_EC_OpenSSL_NID = CT_NID_secp256k1;

  CT_AccountInfo_ForSale = 1000;

  CT_PROTOCOL_1 = 1;
  CT_PROTOCOL_2 = 2;
  CT_BlockChain_Protocol_Available: Word = $0002; // Protocol 2 flag
  CT_Protocol_Upgrade_v2_MinBlock = {$IFDEF PRODUCTION}100{$ELSE}10{$ENDIF};


  CT_MagicNetIdentification = {$IFDEF PRODUCTION}$0A043580{$ELSE}$0A04FFFF{$ENDIF};

  CT_NetProtocol_Version: Word = $0006;
  // IMPORTANT NOTE!!!
  // NetProtocol_Available MUST BE always >= NetProtocol_version
  CT_NetProtocol_Available: Word = $0006;  // Remember, >= NetProtocol_version !!!

  CT_MaxAccountOperationsPerBlockWithoutFee = 1;

  CT_AccountStorageVersion : Word = 3; // Protocol 2 upgraded safebox version from 2 to 3

  CT_MagicIdentificator: AnsiString = {$IFDEF PRODUCTION}'MicroCoin'{$ELSE}'MicroCoinTESTNET'{$ENDIF}; //

  CT_Op_Transaction = $01;
  CT_Op_Changekey = $02;
  CT_Op_Recover = $03;
  CT_Op_ListAccountForSale = $04;
  CT_Op_DelistAccount = $05;
  CT_Op_BuyAccount = $06;
  CT_Op_ChangeKeySigned = $07;
  CT_Op_ChangeAccountInfo = $08;
  CT_Op_CreateSubAccount = $09;

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

  CT_ClientAppVersion : AnsiString = {$IFDEF PRODUCTION}'1.1.3'{$ELSE}{$IFDEF TESTNET}'TESTNET 1.1.3'{$ELSE}{$ENDIF}{$ENDIF};
  {$IFDEF PRODUCTION}
  CT_Discover_IPs =  '194.182.64.181;185.28.101.93;80.211.211.48;94.177.237.196;185.33.146.44;80.211.200.121;194.182.64.181';
  {$ENDIF}
  {$IFDEF TESTNET}
  CT_Discover_IPs =  '194.182.64.181;185.33.146.44';
  {$ENDIF}
  CT_TRUE_FALSE : array[Boolean] Of AnsiString = ('FALSE','TRUE');

  CT_MAX_0_fee_operations_per_block_by_miner = {$IFDEF PRODUCTION}2000{$ELSE}{$IFDEF TESTNET}2{$ELSE}{$ENDIF}{$ENDIF};
  CT_MAX_Operations_per_block_by_miner =  {$IFDEF PRODUCTION}10000{$ELSE}{$IFDEF TESTNET}50{$ELSE}{$ENDIF}{$ENDIF};

  CT_BLOCK_EXTENDED_ACCOUNT_DATA = 429;

implementation

end.


