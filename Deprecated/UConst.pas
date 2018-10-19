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
{$IFDEF WINDOWS}
uses Windows;
{$ENDIF}

{$I config.inc}

{$IFNDEF FPC}

type
  PtrInt = integer;
  PtrUInt = cardinal;
{$ENDIF}


var
  ClientAppVersion : AnsiString = {$IFDEF PRODUCTION}'1.3.2'{$ELSE}{$IFDEF TESTNET}'TESTNET 1.3.2'{$ELSE}'DEVNET 1.3.2'{$ENDIF}{$ENDIF};

Const


  cGenesisBlockMagic : AnsiString = '(c) Peter Nemeth - Okes rendben okes';

  cGenesisBlockPoW =
    {$IFDEF PRODUCTION}
      '0000007A35BA642D75EBDA692D25DA09AB8EABE802BC970C01DE08F60DC3F93F'
      {$ELSE}
      {$IFDEF TESTNET}''{$ELSE}
      {$IFDEF DEVNET}''{$ENDIF}
      {$ENDIF}
    {$ENDIF};


  cNetServerPort = {$IFDEF PRODUCTION}4004{$ELSE}{$IFDEF TESTNET}4104{$ELSE}{$IFDEF DEVNET}4204{$ENDIF}{$ENDIF}{$ENDIF};
  cMinerServerPort = {$IFDEF PRODUCTION}4009{$ELSE}{$IFDEF TESTNET}4109{$ELSE}{$IFDEF DEVNET}4209{$ENDIF}{$ENDIF}{$ENDIF};
  cJsonRPCPort = {$IFDEF PRODUCTION}4003{$ELSE}{$IFDEF TESTNET}4103{$ELSE}{$IFDEF DEVNET}4203{$ENDIF}{$ENDIF}{$ENDIF};
  cAccountsPerBlock = 5;

  cBlockTime: Cardinal = {$IFDEF PRODUCTION}300{$ELSE}{$IFDEF TESTNET}30{$ELSE}{$IFDEF DEVNET}10{$ENDIF}{$ENDIF}{$ENDIF};

  cInitialBlockReward: UInt64 = 1000000; // 4 decimals... First reward = 100,0000
  cMinimumReward: UInt64 = 10000; // 4 decimals... Min reward = 1,0000
  cDecreaseReward: Cardinal = 420480; // Avg 4 year

  cMinimumBlocksToLiveAccount = 10;
  cMinimumBlocksToLiveAccountV2 = 100;
  cV2EnableAtBlock = 200000;

  cRecoverAfterInactive = 420480;  // After 4 years... if an account has no operations, money will be a reward for a miner!
  cMaxLockAccountTime = 105120; // Maximum future blocks an account can be locked

  cMaxTransactionAmount = 1000000000000;
  cMaxTransactionFee = 100000000;
  cMaxWalletAmount = 10000000000000;

  cMinimumDifficulty: Cardinal = {$IFDEF PRODUCTION}$19000000{$ELSE}{$IFDEF TESTNET}$19000000{$ELSE}
  {$IFDEF DEVNET}$19000000{$ENDIF}
  {$ENDIF}{$ENDIF}; // First compact target of block 0

  cDifficultyCalcBlocks: Cardinal = 100;
  cDifficultyCalcBlocks_SPLIT = 10;

  cMaxAccountNumber : Cardinal = $FFFFFFFF;
  cMaxBlocks : Cardinal = $FFFFFFFF;

  cMaxPayloadSize = 255; // Max payload size in bytes
  cBlockTimeStampTolerance = 15;
  cMinimumNodeCountToCalculateNAT = 4;

  cMinimumServersNeeded = 3;
  cMaximumNumberOfServers = 6;

  cMaximumClients = 100;

  cSaveAccountStorageOnBlocks = {$IFDEF PRODUCTION}100{$ELSE}10{$ENDIF};

  cNID_secp256k1 = 714;
  cNID_secp384r1 = 715;
  cNID_sect283k1 = 729;
  cNID_secp521r1 = 716;

  cDefault_EC_OpenSSL_NID = cNID_secp256k1;

  cAccountInfo_ForSale = 1000;

  cPROTOCOL_1 = 1;
  cPROTOCOL_2 = 2;
  cBlockChain_Protocol_Available: Word = $0002; // Protocol 2 flag
  cProtocol_Upgrade_v2_MinBlock = {$IFDEF PRODUCTION}100{$ELSE}10{$ENDIF};


  cMagicNetIdentification = {$IFDEF PRODUCTION}$0A043580{$ELSE}
  {$IFDEF TESTNET}
  $0A04FFFF
  {$ELSE}
    {$IFDEF DEVNET}
    $0A0A0A0A
    {$ENDIF}
  {$ENDIF}
  {$ENDIF};

  cNetProtocol_Version: Word = $0006;
  // IMPORTANT NOTE!!!
  // NetProtocol_Available MUST BE always >= NetProtocol_version
  cNetProtocol_Available: Word = $0006;  // Remember, >= NetProtocol_version !!!

  cMaxAccountOperationsPerBlockWithoutFee = 1;

  cAccountStorageVersion : Word = 3; // Protocol 2 upgraded safebox version from 2 to 3

  cMagicID: AnsiString = {$IFDEF PRODUCTION}'MicroCoin'
                         {$ELSE}{$IFDEF TESTNET}'MicroCoinTESTNET'{$ELSE}
                         {$IFDEF DEVNET}'MicroCoinDEV'{$ENDIF}
                         {$ENDIF}
                         {$ENDIF}; //

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

  {$IFDEF PRODUCTION}
  CT_Discover_IPs =  '195.181.240.58;194.182.64.181;185.28.101.93;80.211.211.48;94.177.237.196;185.33.146.44;80.211.200.121;194.182.64.181';
  {$ENDIF}
  {$IFDEF TESTNET}
  CT_Discover_IPs =  '194.182.64.181;185.33.146.44';
  {$ENDIF}
  {$IFDEF DEVNET}
  CT_Discover_IPs =  '194.182.64.181';
  {$ENDIF}
  CT_TRUE_FALSE : array[Boolean] Of AnsiString = ('FALSE','TRUE');

  CT_MAX_0_fee_operations_per_block_by_miner = {$IFDEF PRODUCTION}2000{$ELSE}{$IFDEF TESTNET}2{$ELSE}2{$ENDIF}{$ENDIF};
  CT_MAX_Operations_per_block_by_miner =  {$IFDEF PRODUCTION}10000{$ELSE}{$IFDEF TESTNET}100000{$ELSE}50{$ENDIF}{$ENDIF};
  {$IFDEF DEVNET}
  CT_BLOCK_EXTENDED_ACCOUNT_DATA = 19;
  {$ELSE}
  CT_BLOCK_EXTENDED_ACCOUNT_DATA = $FFFFFFFF;
  {$ENDIF}

implementation

{$IFDEF WINDOWS}
uses SysUtils;

procedure GetBuildInfo(var V1, V2, V3, V4: word);
var
  VerInfoSize, VerValueSize, Dummy: DWORD;
  VerInfo: Pointer;
  VerValue: PVSFixedFileInfo;
begin
  VerInfoSize := GetFileVersionInfoSize(PChar(ParamStr(0)), Dummy);
  if VerInfoSize > 0 then
  begin
      GetMem(VerInfo, VerInfoSize);
      try
        if GetFileVersionInfo(PChar(ParamStr(0)), 0, VerInfoSize, VerInfo) then
        begin
          VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
          with VerValue^ do
          begin
            V1 := dwFileVersionMS shr 16;
            V2 := dwFileVersionMS and $FFFF;
            V3 := dwFileVersionLS shr 16;
            V4 := dwFileVersionLS and $FFFF;
          end;
        end;
      finally
        FreeMem(VerInfo, VerInfoSize);
      end;
  end;
end;

function GetBuildInfoAsString: string;
var
  V1, V2, V3, V4: word;
begin
  GetBuildInfo(V1, V2, V3, V4);
  Result := IntToStr(V1) + '.' + IntToStr(V2) + '.' +
    IntToStr(V3) + '.' + IntToStr(V4);
end;

initialization
  ClientAppVersion := GetBuildInfoAsString;
{$ENDIF}
end.

