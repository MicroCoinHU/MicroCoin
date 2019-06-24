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
| File:       MicroCoin.Common.Config.pas                                      |
| Created at: 2019-01-15                                                       |
| Purpose:    Common constansts and configuration routines                     |
|==============================================================================}

unit MicroCoin.Common.Config;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

{$IFNDEF LINUX}
uses Windows, {$IFNDEF FPC}{$ENDIF}ShlObj;
{$ENDIF}

{$I config.inc}

{$IFNDEF FPC}
type
  PtrInt = integer;
  PtrUInt = cardinal;
{$ENDIF}


var
  ClientAppVersion : AnsiString = {$IFDEF PRODUCTION}'1.3.2'{$ELSE}{$IFDEF TESTNET}'TESTNET 1.3.2'{$ELSE}'DEVNET 1.3.2'{$ENDIF}{$ENDIF};

const


  cCoinName = 'MicroCoin';

  cPathPrefix = {$IFDEF TESTNET}'_TESTNET'{$ENDIF}
                {$IFDEF DEVNET}'_DEVNET'{$ENDIF}
                {$IFDEF PRODUCTION}''{$ENDIF};

  cDataFolder = cCoinName+cPathPrefix;

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
  cV2EnableAtBlock = 2000000;

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

  cMaxPayloadSize = 255;
  cBlockTimeStampTolerance = 15;
  cMinimumNodeCountToCalculateNAT = 4;

  cMinimumServersNeeded = 3;
  cMaximumNumberOfServers = 20;

  cMaximumClients = 100;

  cSaveAccountStorageOnBlocks = {$IFDEF PRODUCTION}100{$ELSE}10{$ENDIF};

  cAccountSorageBackupCount = 10;

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

  {$IFDEF PRODUCTION}
//     cDiscover_IPs =  '80.211.200.122';
     cDiscover_IPs =  '80.211.200.121;195.181.240.58;194.182.64.181;185.28.101.93;80.211.211.48;94.177.237.196;185.33.146.44;161.129.65.13';
  {$ENDIF}
  {$IFDEF TESTNET}
     cDiscover_IPs =  '194.182.64.181;185.33.146.44';
  {$ENDIF}
  {$IFDEF DEVNET}
    cDiscover_IPs =  '194.182.64.181';
  {$ENDIF}

  cMAX_Zero_Fee_operations_per_block_by_miner = {$IFDEF PRODUCTION}2000{$ELSE}{$IFDEF TESTNET}2{$ELSE}2{$ENDIF}{$ENDIF};
  cMAX_Operations_per_block_by_miner =  {$IFDEF PRODUCTION}10000{$ELSE}{$IFDEF TESTNET}100000{$ELSE}50{$ENDIF}{$ENDIF};
  {$IFDEF DEVNET}
  cBLOCK_EXTENDED_ACCOUNT_DATA = 19;
  {$ELSE}
  cBLOCK_EXTENDED_ACCOUNT_DATA = $FFFFFFFF;
  {$ENDIF}

function MicroCoinDataFolder: string;

implementation

uses SysUtils;

function MicroCoinDataFolder: string;
var
  FolderPath: array[0 .. MAX_PATH] of Char;
  xPath : string;
begin
  {$IFDEF FPC}
    {$IFDEF LINUX}
      xPath := GetEnvironmentVariable('HOME');
    {$ELSE}
      xPath := GetEnvironmentVariable('APPDATA');
    {$ENDIF}
  {$ELSE}
      SHGetFolderPath(0, CSIDL_APPDATA, 0, 0, @FolderPath);
      xPath := FolderPath;
  {$ENDIF}
  Result := xPath + PathDelim + cDataFolder;
  xPath := '';
end;


{$IFNDEF FPC}
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

