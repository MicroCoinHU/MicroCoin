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
| File:       MicroCoin.Common.AppSettings.pas                                 |
| Created at: 2018-08-31                                                       |
| Purpose:    Base class for application settings                              |
|==============================================================================}

unit MicroCoin.Common.AppSettings;

interface

type
  TMicroCoinAppSettings = class
  strict private
    FPublicKey: string;
    FMaxConnections : integer;
    FMaxTransactionPerBlock: integer;
    FMaxZeroFeeTransactionPerBlock: integer;
    FMinerName: string;
    FMinerServerMaxConnections: integer;
    FMinerServerPort: integer;
    FPort: integer;
    FRPCPort: integer;
    FRPCWhiteList: string;
    FSaveLog: boolean;
    FSaveRPCLogs: boolean;
    procedure SetMinerServerMaxConnections(const Value: integer);
    procedure SetMaxConnections(const Value: integer);
    procedure SetMaxTransactionPerBlock(const Value: integer);
    procedure SetMaxZeroFeeTransactionPerBlock(const Value: integer);
    procedure SetPort(const Value: integer);
    procedure SetRPCPort(const Value: integer);
    procedure SetRPCWhiteList(const Value: string);
    procedure SetSaveLog(const Value: boolean);
    procedure SetSaveRPCLogs(const Value: boolean);
    procedure SetMinerServerPort(const Value: integer);
    procedure SetMinerName(const Value: string);
  strict protected
    procedure SetPublicKey(const Value: string); virtual;
    function GetPublicKey: string; virtual;
    function GetMaxConnections: integer; virtual;
    function GetMaxTransactionPerBlock: integer; virtual;
    function GetMaxZeroFeeTransactionPerBlock: integer; virtual;
    function GetMinerName: string; virtual;
    function GetMinerServerMaxConnections: integer; virtual;
    function GetMinerServerPort: integer; virtual;
    function GetPort: integer; virtual;
    function GetRPCPort: integer; virtual;
    function GetRPCWhiteList: string; virtual;
    function GetSaveLog: boolean; virtual;
    function GetSaveRPCLogs: boolean; virtual;
  public
    constructor Create;
  published
    property SaveLogs  : boolean read GetSaveLog write SetSaveLog default true;
    property Port : integer read GetPort write SetPort default 4004;
    property MaxConnections : integer read GetMaxConnections write SetMaxConnections default 100;
    property RPCPort : integer read GetRPCPort write SetRPCPort default 4003;
    property RPCWhiteList : string read GetRPCWhiteList write SetRPCWhiteList;
    property SaveRPCLogs : boolean read GetSaveRPCLogs write SetSaveRPCLogs default true;
    property MinerServerPort : integer read GetMinerServerPort write SetMinerServerPort default 4009;
    property PublicKey : string read GetPublicKey write SetPublicKey;
    property MinerName : string read GetMinerName write SetMinerName;
    property MinerServerMaxConnections : integer read GetMinerServerMaxConnections write SetMinerServerMaxConnections default 1000;
    property MaxTransactionPerBlock : integer read GetMaxTransactionPerBlock write SetMaxTransactionPerBlock default 10000;
    property MaxZeroFeeTransactionPerBlock : integer read GetMaxZeroFeeTransactionPerBlock write SetMaxZeroFeeTransactionPerBlock default 2000;
  end;

implementation

constructor TMicroCoinAppSettings.Create;
begin
  FMinerName := '';
  FPublicKey := '';
  FRPCWhiteList := '127.0.0.1';
end;

function TMicroCoinAppSettings.GetMaxConnections: integer;
begin
  Result := FMaxConnections;
end;

function TMicroCoinAppSettings.GetMaxTransactionPerBlock: integer;
begin
  Result := FMaxTransactionPerBlock;
end;

function TMicroCoinAppSettings.GetMaxZeroFeeTransactionPerBlock: integer;
begin
  Result := FMaxZeroFeeTransactionPerBlock;
end;

function TMicroCoinAppSettings.GetMinerName: string;
begin
  Result := FMinerName;
end;

function TMicroCoinAppSettings.GetMinerServerMaxConnections: integer;
begin
  Result := FMinerServerMaxConnections;
end;

function TMicroCoinAppSettings.GetMinerServerPort: integer;
begin
  Result := FMinerServerPort;
end;

function TMicroCoinAppSettings.GetPort: integer;
begin
   Result := FPort;
end;

function TMicroCoinAppSettings.GetPublicKey: string;
begin
  Result := FPublicKey;
end;

function TMicroCoinAppSettings.GetRPCPort: integer;
begin
  Result := FRPCPort;
end;

function TMicroCoinAppSettings.GetRPCWhiteList: string;
begin
  Result := FRPCWhiteList;
end;

function TMicroCoinAppSettings.GetSaveLog: boolean;
begin
  Result := FSaveLog;
end;

function TMicroCoinAppSettings.GetSaveRPCLogs: boolean;
begin
  Result := FSaveRPCLogs;
end;

procedure TMicroCoinAppSettings.SetMaxConnections(const Value: integer);
begin
  FMaxConnections := Value;
end;

procedure TMicroCoinAppSettings.SetMaxTransactionPerBlock(const Value: integer);
begin
  FMaxTransactionPerBlock := Value;
end;

procedure TMicroCoinAppSettings.SetMaxZeroFeeTransactionPerBlock(
  const Value: integer);
begin
  FMaxZeroFeeTransactionPerBlock := Value;
end;

procedure TMicroCoinAppSettings.SetMinerName(const Value: string);
begin
  FMinerName := Value;
end;

procedure TMicroCoinAppSettings.SetMinerServerMaxConnections(
  const Value: integer);
begin
  FMinerServerMaxConnections := Value;
end;

procedure TMicroCoinAppSettings.SetMinerServerPort(const Value: integer);
begin

end;

procedure TMicroCoinAppSettings.SetPort(const Value: integer);
begin
  FPort := Value;
end;

procedure TMicroCoinAppSettings.SetPublicKey(const Value: string);
begin
  FPublicKey := Value;
end;

procedure TMicroCoinAppSettings.SetRPCPort(const Value: integer);
begin
  FRPCPort := Value;
end;

procedure TMicroCoinAppSettings.SetRPCWhiteList(const Value: string);
begin
  FRPCWhiteList := Value;
end;

procedure TMicroCoinAppSettings.SetSaveLog(const Value: boolean);
begin
  FSaveLog := Value;
end;

procedure TMicroCoinAppSettings.SetSaveRPCLogs(const Value: boolean);
begin
  FSaveRPCLogs := Value;
end;

end.
