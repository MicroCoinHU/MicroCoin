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
| File:       MicroCoin.Common.IniFileSettings.pas                             |
| Created at: 2018-08-31                                                       |
| Purpose:    Application setting in ini file                                  |
|==============================================================================}

unit MicroCoin.Common.IniFileSettings;

interface

uses MicroCoin.Common.AppSettings, IniFiles, Sysutils, MicroCoin.Common.Config;

type
  TMicroCoinIniFileSettings = class(TMicroCoinAppSettings)
  strict private
  const
    GENERAL = 'General';
    RPC = 'RPC';
    MINER = 'Miner';
  strict private
    FIniFile : TIniFile;
  strict protected
    procedure SetPublicKey(const Value: string); override;
    function GetMaxConnections: integer; override;
    function GetMaxTransactionPerBlock: integer; override;
    function GetMaxZeroFeeTransactionPerBlock: integer; override;
    function GetMinerName: string; override;
    function GetMinerServerMaxConnections: integer; override;
    function GetMinerServerPort: integer; override;
    function GetPort: integer; override;
    function GetPublicKey: string; override;
    function GetRPCPort: integer; override;
    function GetRPCWhiteList: string; override;
    function GetSaveLog: boolean; override;
    function GetSaveRPCLogs: boolean; override;
  public
    constructor Create(AIniFileName : TFilename);
    destructor  Destory;
  published
    property SaveLogs;
    property Port;
    property MaxConnections;
    property RPCPort;
    property RPCWhiteList;
    property SaveRPCLogs;
    property MinerServerPort;
    property PublicKey;
    property MinerName;
    property MinerServerMaxConnections;
    property MaxTransactionPerBlock;
    property MaxZeroFeeTransactionPerBlock;
  end;

implementation

constructor TMicroCoinIniFileSettings.Create(AIniFileName: TFilename);
begin
  FIniFile := TIniFile.Create(AIniFileName);
end;

destructor TMicroCoinIniFileSettings.Destory;
begin
  FreeAndNil(FIniFile);
  inherited;
end;

function TMicroCoinIniFileSettings.GetMaxConnections: integer;
begin
  Result := FIniFile.ReadInteger(GENERAL, 'MaxConnections', cMaximumClients);
  if not FIniFile.ValueExists(GENERAL, 'MaxConnections')
  then FIniFile.WriteInteger(GENERAL, 'MaxConnections', Result);
end;

function TMicroCoinIniFileSettings.GetMaxTransactionPerBlock: integer;
begin
  Result := FIniFile.ReadInteger(MINER, 'MaxTransactionPerBlock', cMAX_Operations_per_block_by_miner);
  if not FIniFile.ValueExists(MINER, 'MaxTransactionPerBlock')
  then FIniFile.WriteInteger(MINER, 'MaxTransactionPerBlock', Result);
end;

function TMicroCoinIniFileSettings.GetMaxZeroFeeTransactionPerBlock: integer;
begin
  Result := FIniFile.ReadInteger(MINER, 'ZeroFeeTransactionPerBlock', cMAX_Zero_Fee_operations_per_block_by_miner);
  if not FIniFile.ValueExists(MINER, 'ZeroFeeTransactionPerBlock')
  then FIniFile.WriteInteger(MINER, 'ZeroFeeTransactionPerBlock', Result);
end;

function TMicroCoinIniFileSettings.GetMinerName: string;
begin
  Result := FIniFile.ReadString(MINER, 'MinerName', '');
  if not FIniFile.ValueExists(MINER, 'MinerName')
  then FIniFile.WriteString(MINER, 'MinerName', Result);
end;

function TMicroCoinIniFileSettings.GetMinerServerMaxConnections: integer;
begin
  Result := FIniFile.ReadInteger(MINER, 'MinerServerMaxConnections', 1000);
  if not FIniFile.ValueExists(MINER, 'MinerServerMaxConnections')
  then FIniFile.WriteInteger(MINER, 'MinerServerMaxConnections', Result);
end;

function TMicroCoinIniFileSettings.GetMinerServerPort: integer;
begin
  Result := FIniFile.ReadInteger(MINER, 'MinerServerPort', cMinerServerPort);
  if not FIniFile.ValueExists(MINER, 'MinerServerPort')
  then FIniFile.WriteInteger(MINER, 'MinerServerPort', Result);
end;

function TMicroCoinIniFileSettings.GetPort: integer;
begin
  Result := FIniFile.ReadInteger(GENERAL, 'Port', cNetServerPort);
  if not FIniFile.ValueExists(GENERAL, 'Port')
  then FIniFile.WriteInteger(GENERAL, 'Port', Result);
end;

function TMicroCoinIniFileSettings.GetPublicKey: string;
begin
  Result := FIniFile.ReadString(GENERAL, 'PublicKey', '');
  if not FIniFile.ValueExists(GENERAL, 'PublicKey')
  then FIniFile.WriteString(GENERAL, 'PublicKey', Result);
end;

function TMicroCoinIniFileSettings.GetRPCPort: integer;
begin
  Result := FIniFile.ReadInteger(RPC, 'RPCPort', cJsonRPCPort);
  if not FIniFile.ValueExists(RPC, 'RPCPort')
  then FIniFile.WriteInteger(RPC, 'RPCPort', Result);
end;

function TMicroCoinIniFileSettings.GetRPCWhiteList: string;
begin
  Result := FIniFile.ReadString(RPC, 'WhiteList', '127.0.0.1');
  if not FIniFile.ValueExists(RPC, 'WhiteList')
  then FIniFile.WriteString(RPC, 'WhiteList', Result);
end;

function TMicroCoinIniFileSettings.GetSaveLog: boolean;
begin
  Result := FIniFile.ReadBool(GENERAL, 'SaveLog', true);
  if not FIniFile.ValueExists(GENERAL, 'SaveLog')
  then FIniFile.WriteBool(GENERAL, 'SaveLog', Result);
end;

function TMicroCoinIniFileSettings.GetSaveRPCLogs: boolean;
begin
  Result := FIniFile.ReadBool(RPC, 'SaveLog', true);
  if not FIniFile.ValueExists(RPC, 'SaveLog')
  then FIniFile.WriteBool(RPC, 'SaveLog', Result);
end;

procedure TMicroCoinIniFileSettings.SetPublicKey(const Value: string);
begin
  FIniFile.WriteString(GENERAL, 'PublicKey', Value);
end;


end.
