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
| File:       MicroCoin.Console.Application.pas                                |
| Created at: 2018-08-29                                                       |
| Purpose:    Base classes for application                                     |
|==============================================================================}

unit MicroCoin.Console.Application;

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses Classes, SysUtils, {$IFDEF MSWINDOWS}Windows, {$ifndef fpc}Threading,{$endif} {$ELSE}{$ENDIF}
  SyncObjs, OpenSSL, UCrypto, MicroCoin.Node.Node, MicroCoin.BlockChain.FileStorage,
  UFolderHelper, UWalletKeys, UConst, ULog, MicroCoin.Net.Protocol,
  IniFiles, MicroCoin.Account.AccountKey, MicroCoin.Net.ConnectionManager,
  MicroCoin.RPC.Server, MicroCoin.Mining.Server, MicroCoin.Account.Data,
  MicroCoin.Common.AppSettings, MicroCoin.Common.InifileSettings,
  MicroCoin.Keys.KeyManager;

type

  TMicroCoin = class
  strict private
    FStarted: boolean;
    FOnLog: TNewLogEvent;
    FSettings : TMicroCoinAppSettings;
    FRPC: TRPCServer;
    FWalletKeys: TKeyManager;
    FMinerServer: TMiningServer;
    FLog: TLog;
    {$ifndef fpc}
    FInitTask : ITask;
    {$endif}
    procedure InitRPCMinerServer;
    procedure InitRPCServer;
    procedure InitNode;
    procedure InitCrypto;
    procedure InitLog;
    procedure InitKeys;
    procedure SetOnLog(const Value: TNewLogEvent);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start;
  published
    property Settings : TMicroCoinAppsettings read FSettings;
    property RPCServer : TRPCServer read FRPC;
    property WalletKeys : TKeyManager read FWalletKeys;
    property MinerServer : TMiningServer read FMinerServer;
    property Log : TLog read FLog;
    property OnLog : TNewLogEvent read FOnLog write SetOnLog;
    property Started : boolean read FStarted default false;
  end;

  TMicroCoinApplication = class(TThread)
  strict protected
    procedure Execute; override;
    procedure OnMicroCoinInThreadLog(logtype: TLogType; Time: TDateTime; AThreadID: Cardinal; const sender, logtext: AnsiString);
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  MicroCoin : TMicroCoin;

implementation

procedure TMicroCoinApplication.Execute;
begin
  MicroCoin.OnLog := OnMicroCoinInThreadLog;
  MicroCoin.Start;
  repeat
    Sleep(100);
  until Terminated;
end;

constructor TMicroCoinApplication.Create;
begin
  MicroCoin := TMicroCoin.Create;
  inherited Create(false);
  WriteLn('');
  WriteLn(formatDateTime('dd/mm/yyyy hh:nn:ss.zzz', now) + ' Starting MicroCoin server');
end;

destructor TMicroCoinApplication.Destroy;
begin
   FreeAndNil(MicroCoin);
  inherited Destroy;
end;

procedure TMicroCoinApplication.OnMicroCoinInThreadLog(logtype: TLogType; Time: TDateTime; AThreadID: Cardinal;
  const sender, logtext: AnsiString);
var
  s: AnsiString;
{$IFDEF MSWINDOWS}
  color: word;
{$ENDIF}
begin
  if AThreadID = MainThreadID then
    s := ' MAIN:'
  else
    s := ' TID:';
{$IFDEF MSWINDOWS}
  case logtype of
    lterror:
      color := FOREGROUND_INTENSITY or FOREGROUND_RED;
    ltinfo:
      color := FOREGROUND_BLUE or FOREGROUND_INTENSITY;
    ltdebug:
      color := FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE;
    ltupdate:
      color := FOREGROUND_INTENSITY or FOREGROUND_GREEN;
  end;
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), color);
{$ENDIF}
  WriteLn(formatDateTime('dd/mm/yyyy hh:nn:ss.zzz', Time) + s + IntToHex(AThreadID, 8) + ' [' + CT_LogType[logtype] +
    '] <' + sender + '> ' + logtext);
end;

{ TMicroCoin }

constructor TMicroCoin.Create;
begin
  FSettings := TMicroCoinIniFileSettings.Create(ExtractFileDir(ParamStr(0)) + PathDelim + 'microcoin.ini');
  InitLog;
  InitCrypto;
end;

destructor TMicroCoin.Destroy;
begin
  if not FStarted then exit;
  {$ifndef fpc}
  if (FInitTask.Status = TTaskStatus.Running) or
     (FInitTask.Status = TTaskStatus.WaitingToRun) or
     (FInitTask.Status = TTaskStatus.WaitingForChildren)
  then FInitTask.Cancel;
  FInitTask.Wait();
  {$endif}
//  TNode.Node.BlockManager.Stopped := true;
//  TNode.Node.NetServer.Active := false;
  FSettings.Free;
  FRPC.Free;
  FMinerServer.Free;
  TNode.Node.Free;
  FWalletKeys.Free;
  TConnectionManager.ReleaseInstance;
  inherited;
end;


procedure TMicroCoin.InitCrypto;
begin

 if not LoadSSLCrypt then
  begin
    WriteLn('Cannot load ' + SSL_C_LIB);
    WriteLn('To use this software make sure this file is available on you system or reinstall the application');
    raise Exception.Create('Cannot load ' + SSL_C_LIB + #10 +
      'To use this software make sure this file is available on you system or reinstall the application');
  end;
  TCrypto.InitCrypto;
end;

procedure TMicroCoin.InitKeys;
begin
  FWalletKeys := TKeyManager.Create(nil);
  FWalletKeys.WalletFileName := TFolderHelper.GetMicroCoinDataFolder + PathDelim + 'WalletKeys.dat';
end;

procedure TMicroCoin.InitLog;
begin
  FLog := TLog.Create(nil);
  if FSettings.SaveLogs then
  begin
    FLog.SaveTypes := CT_TLogTypes_ALL;
    FLog.FileName := TFolderHelper.GetMicroCoinDataFolder + PathDelim + 'microcoin_' +
      formatDateTime('yyyymmddhhnn', now) + '.log';
  end;
end;

procedure TMicroCoin.InitNode;
begin
  TNode.Node.BlockManager.StorageClass := TFileStorage;
  TFileStorage(TNode.Node.BlockManager.Storage).DatabaseFolder := TFolderHelper.GetMicroCoinDataFolder + PathDelim + 'Data';

  TNode.Node.BlockManager.DiskRestoreFromTransactions(CT_MaxBlock);

  FWalletKeys.AccountStorage := TNode.Node.BlockManager.AccountStorage;

  TNode.Node.NetServer.Port := FSettings.Port;
  TNode.Node.NetServer.MaxConnections := FSettings.MaxConnections;
  TNode.Node.AutoDiscoverNodes(CT_Discover_IPs);
  TNode.Node.NetServer.Active := true;

  FStarted := True;

end;

procedure TMicroCoin.InitRPCMinerServer;
var
  i, Port, MaxConnections: Integer;
  s: string;
  pubkey: TAccountKey;
  errors: AnsiString;
  ECPK: TECPrivateKey;
begin
  i := FSettings.MinerServerPort;
  if (i < 0) then
    i := CT_JSONRPCMinerServer_Port;
  if (i > 0) then
  begin
    Port := i;
    pubkey := CT_TECDSA_Public_Nul;
    s := Trim(FSettings.PublicKey);
    if (s = '') or (not TAccountKey.AccountKeyFromImport(s, pubkey, errors)) then
    begin
      if s <> '' then
        TLog.NewLog(lterror, ClassName, 'Invalid INI file public key: ' + errors);
      i := 0;
      while (i < FWalletKeys.Count) and (pubkey.EC_OpenSSL_NID = CT_TECDSA_Public_Nul.EC_OpenSSL_NID) do
      begin
        if (FWalletKeys.Key[i].CryptedKey <> '') then
          pubkey := FWalletKeys[i].AccountKey
        else
          inc(i);
      end;
      if (pubkey.EC_OpenSSL_NID = CT_TECDSA_Public_Nul.EC_OpenSSL_NID) then
      begin
        // New key
        ECPK := TECPrivateKey.Create;
        try
          ECPK.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
          FWalletKeys.AddPrivateKey('RANDOM NEW BY DAEMON ' + formatDateTime('yyyy-mm-dd hh:nn:dd', now), ECPK);
          pubkey := ECPK.PublicKey;
          FSettings.PublicKey := pubkey.AccountPublicKeyExport();
          TLog.NewLog(ltinfo, ClassName, 'Generated new pubkey for miner: ' + pubkey.AccountPublicKeyExport());
        finally
          ECPK.Free;
        end;
      end;
    end
    else
    begin
      // pubkey is mine?
      if (FWalletKeys.IndexOfAccountKey(pubkey) < 0) then
      begin
        TLog.NewLog(lterror, ClassName, 'WARNING: Using a public key without private key in wallet! ' +
          pubkey.AccountPublicKeyExport());
      end;
    end;
    i := FWalletKeys.IndexOfAccountKey(pubkey);
    s := Trim(FSettings.MinerName);
    if (SameText(s, 'TIME')) then
    begin
      s := formatDateTime('yyyy-mm-dd hh:nn', now);
      TLog.NewLog(ltinfo, ClassName, 'Generated new miner name: ' + s);
    end;
    MaxConnections := FSettings.MinerServerMaxConnections;
    TLog.NewLog(ltinfo, ClassName,
      Format('Activating RPC Miner Server on port %d, name "%s", max conections %d and public key %s',
      [Port, s, MaxConnections, pubkey.AccountPublicKeyExport()]));
    FMinerServer := TMiningServer.Create;
    FMinerServer.UpdateAccountAndPayload(pubkey, s);
    FMinerServer.Port := Port;
    FMinerServer.Active := true;
    FMinerServer.MaxConnections := MaxConnections;
    FMinerServer.MaxOperationsPerBlock := FSettings.MaxTransactionPerBlock;
    FMinerServer.MaxZeroFeeOperationsPerBlock := FSettings.MaxZeroFeeTransactionPerBlock;
  end
  else
  begin
    TLog.NewLog(ltinfo, ClassName, 'RPC Miner Server NOT ACTIVE');
  end;
end;

procedure TMicroCoin.InitRPCServer;
var
  Port: Integer;
begin
  Port := FSettings.RPCPort;
  if (Port <= 0) then
  begin
    Port := CT_JSONRPC_Port;
  end;
  FRPC := TRPCServer.Create;
  FRPC.WalletKeys := FWalletKeys;
  FRPC.Port := Port;
  FRPC.Active := true;
  FRPC.ValidIPs := FSettings.RPCWhiteList;
  TLog.NewLog(ltinfo, ClassName, 'RPC server is active on port ' + IntToStr(Port));
  if FSettings.SaveRPCLogs then
  begin
    FRPC.LogFileName := TFolderHelper.GetMicroCoinDataFolder + PathDelim + 'microcoin_rpc.log';
    TLog.NewLog(ltinfo, ClassName, 'Activating RPC logs on file ' + FRPC.LogFileName);
  end
  else
  begin
    TLog.NewLog(ltinfo, ClassName, 'RPC logs not enabled');
  end;
end;


procedure TMicroCoin.SetOnLog(const Value: TNewLogEvent);
begin
  FOnLog := Value;
  FLog.OnInThreadNewLog := OnLog;
end;

procedure TMicroCoin.Start;
begin
  InitCrypto;
  InitKeys;
  InitRPCServer;
  {$ifndef fpc}
  FInitTask := TTask.Run(InitNode);
  {$else}
  InitNode;
  {$endif}
  InitRPCMinerServer;
end;

initialization
//  MicroCoin := TMicroCoin.Create;

finalization
//  FreeAndNil(MicroCoin);

end.
