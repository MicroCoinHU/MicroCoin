unit MicroCoin.RPC.Server;

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

uses Sysutils, Classes, httpsend, UWalletkeys, Ulog, Inifiles,
{$IFDEF fpc} fpjson, {$ELSE}System.Json, {$ENDIF} UJSONFunctions, synautil,
  MicroCoin.Common.Config, UThread, blcksock, synsock, MicroCoin.RPC.Handler, MicroCoin.Node.Events,
  MicroCoin.Transaction.Itransaction, MicroCoin.Keys.KeyManager, UbaseTypes,
  UCrypto, MicroCoin.Common, MicroCoin.Transaction.Base, MicroCoin.Account.Data;

type

  TRPCServerThread = class(TPCThread)
  private
    FServerSocket: TTCPBlockSocket;
    FPort: Word;
  protected
    procedure BCExecute; override;
  public
    constructor Create(Port: Word);
    destructor Destroy; override;
  end;

  TRPCServer = class
  private
    FRPCServerThread: TRPCServerThread;
    FActive: Boolean;
    FWalletKeys: TKeyManager;
    FPort: Word;
    FJSON20Strict: Boolean;
    FIniFileName: AnsiString;
    FIniFile: TIniFile;
    FRPCLog: TLog;
    FCallsCounter: Int64;
    FValidIPs: AnsiString;
    FNodeNotifyEvents: TNodeNotifyEvents;

    class var FInstance: TRPCServer;

    class function GetInstance: TRPCServer; static;
    procedure SetActive(AValue: Boolean);
    procedure SetIniFileName(const Value: AnsiString);
    procedure SetLogFileName(const Value: AnsiString);
    function GetLogFileName: AnsiString;
    procedure SetValidIPs(const Value: AnsiString);
    function GetWalletKeys: TKeyManager;
    procedure SetWalletKeys(const Value: TKeyManager);
  strict private
    procedure OnNodeNewOperation(Sender: TObject);
    constructor Create;
  public
    destructor Destroy; override;

    function IsValidClientIP(const clientIp: string; clientPort: Word): Boolean;
    function GetNewCallCounter: Int64;
    procedure AddRPCLog(const Sender: string; const Message: string);

    class property Instance: TRPCServer read GetInstance;

    property Port: Word read FPort write FPort;
    property Active: Boolean read FActive write SetActive;
    property WalletKeys: TKeyManager read GetWalletKeys write SetWalletKeys;
    //
    property JSON20Strict: Boolean read FJSON20Strict write FJSON20Strict;
    property IniFileName: AnsiString read FIniFileName write SetIniFileName;
    property LogFileName: AnsiString read GetLogFileName write SetLogFileName;
    property ValidIPs: AnsiString read FValidIPs write SetValidIPs;
  end;

implementation

procedure TRPCServer.AddRPCLog(const Sender: string; const Message: string);
begin
  if not Assigned(FRPCLog) then
    exit;
  FRPCLog.NotifyNewLog(ltinfo, Sender + ' ' + Inttostr(FCallsCounter), message);
end;

class function TRPCServer.GetInstance: TRPCServer;
begin
  if not Assigned(FInstance) then
    FInstance := TRPCServer.Create;
  Result := FInstance;
end;

function TRPCServer.GetLogFileName: AnsiString;
begin
  if Assigned(FRPCLog) then
    Result := FRPCLog.FileName
  else
    Result := '';
end;

function TRPCServer.GetNewCallCounter: Int64;
begin
  inc(FCallsCounter);
  Result := FCallsCounter;
end;

function TRPCServer.GetWalletKeys: TKeyManager;
begin
  Result := FWalletKeys;
end;

procedure TRPCServer.OnNodeNewOperation(Sender: TObject);
var
  i, j: Integer;
  xTransaction: ITransaction;
  xTransactionData: TTransactionData;
  an: cardinal;
  xToAccount: string;
  xAmount: string;
  xFromAccount: string;
  xPayload: string;
  xHTTP: THTTPSend;
  xDecrypted: string;
  xWalletKey: TWalletKey;
  xResult: Boolean;
  xfrom: string;
  xini: TIniFile;
  xProxy: string;
  xStream: TMemoryStream;
  a: string;
  sp: pchar;
  xFee: string;
  xTransactionHash: string;
  xBalance: string;
begin

  xini := TIniFile.Create(ExtractFileDir(ParamStr(0)) + PathDelim + 'microcoin.ini');

  xProxy := xini.ReadString('Events', 'Transaction', '');
  FreeAndNil(xini);
  if xProxy = '' then
    exit;
  { Script := TPSScript.Create(nil); }
  for i := 0 to TNodeNotifyEvents(Sender).Node.TransactionStorage.TransactionCount - 1 do
  begin
    {
      Script.AddRegisteredVariable('FromAccount', 'btString');
      Script.AddRegisteredVariable('ToAccount', 'btString');
      Script.AddRegisteredVariable('Amount', 'btString');
      Script.AddRegisteredVariable('Fee', 'btString');
      Script.AddRegisteredVariable('Balance', 'btString');
      Script.AddRegisteredVariable('OpHash', 'btString');
      Script.AddRegisteredVariable('Payload', 'btString');
    }
    xTransaction := TNodeNotifyEvents(Sender).Node.TransactionStorage.Transaction[i];
    xTransaction := TNodeNotifyEvents(Sender).Node.TransactionStorage.TransactionHashTree.GetTransaction(i);
    xTransaction.GetTransactionData(0, xTransaction.SignerAccount, xTransactionData);
    an := xTransactionData.DestAccount;
    xFromAccount := TAccount.AccountNumberToString(xTransactionData.AffectedAccount);
    xToAccount := TAccount.AccountNumberToString(xTransactionData.DestAccount);
    xAmount := TCurrencyUtils.CurrencyToString(xTransactionData.Amount * -1);
    xFee := TCurrencyUtils.CurrencyToString(xTransactionData.fee);
    xBalance := TCurrencyUtils.CurrencyToString(xTransactionData.balance);
    xTransactionHash := TBaseType.ToHexaString(xTransactionData.OperationHash);
    if TCrypto.IsHumanReadable(xTransactionData.OriginalPayload) then
      xPayload := xTransactionData.OriginalPayload
    else
      xPayload := '';
    {
      Script.AddRegisteredVariable('FromAccount', 'btString');
      Script.AddRegisteredVariable('ToAccount', 'btString');
      Script.AddRegisteredVariable('Amount', 'btString');
      Script.AddRegisteredVariable('Fee', 'btString');
      Script.AddRegisteredVariable('Balance', 'btString');
      Script.AddRegisteredVariable('OpHash', 'btString');
      Script.AddRegisteredVariable('Payload', 'btString');
      Script.Script.LoadFromFile(FromAccount);
      Script.Compile;
      Script.Execute;
    }
    try
      xStream := TMemoryStream.Create;
      xHTTP := THTTPSend.Create;
      try
        with TPCJSONObject.Create do
        begin
          GetAsVariant('from').Value := xFromAccount;
          GetAsVariant('to').Value := xToAccount;
          GetAsVariant('amount').Value := xAmount;
          GetAsVariant('fee').Value := xFee;
          GetAsVariant('balance').Value := xBalance;
          GetAsVariant('ophash').Value := xTransactionHash;
          GetAsVariant('payload').Value := xPayload;
          a := ToJSON(false);
          Free;
        end;
        WriteStrToStream(xHTTP.Document, a);
        xHTTP.MimeType := 'application/json';
        xHTTP.Protocol := '1.1';
        xResult := xHTTP.HTTPMethod('POST', xProxy);
        if xResult then
        begin
          xStream.CopyFrom(xHTTP.Document, 0);
          xStream.Position := 0;
          sp := StrAlloc(xStream.size);
          xStream.Read(sp^, xStream.size);
          a := StrPas(sp);
        end;
      finally
        xHTTP.Free;
      end;
    finally
      xStream.Free;
    end;
  end;
end;

procedure TRPCServer.SetActive(AValue: Boolean);
begin
  if FActive = AValue then
    exit;
  FActive := AValue;
  if (FActive) then
  begin
    FRPCServerThread := TRPCServerThread.Create(FPort);
  end
  else
  begin
    FRPCServerThread.Terminate;
    FRPCServerThread.WaitFor;
    FreeAndNil(FRPCServerThread);
  end;
  TLog.NewLog(ltupdate, Classname, 'Updated RPC Server to Active=' + BoolToStr(FActive, true));
end;

procedure TRPCServer.SetIniFileName(const Value: AnsiString);
begin
  if FIniFileName = Value then
    exit;
  FreeAndNil(FIniFile);
  FIniFileName := Value;
  if (FIniFileName <> '') and (FileExists(FIniFileName)) then
  begin
    FIniFile := TIniFile.Create(FIniFileName);
  end;
  if Assigned(FIniFile) then
  begin
    FJSON20Strict := FIniFile.ReadBool('general', 'json20strict', true)
  end;
end;

procedure TRPCServer.SetLogFileName(const Value: AnsiString);
begin
  if (not Assigned(FRPCLog)) and (Trim(Value) <> '') then
  begin
    FRPCLog := TLog.Create(nil);
    FRPCLog.ProcessGlobalLogs := false;
    FRPCLog.SaveTypes := CT_TLogTypes_ALL;
  end;
  if (Trim(Value) <> '') then
  begin
    FRPCLog.FileName := Value;
  end
  else
    FreeAndNil(FRPCLog);
end;

procedure TRPCServer.SetValidIPs(const Value: AnsiString);
begin
  if FValidIPs = Value then
    exit;
  FValidIPs := Value;
  if FValidIPs = '' then
    TLog.NewLog(ltupdate, Classname, 'Updated RPC Server valid IPs to ALL')
  else
    TLog.NewLog(ltupdate, Classname, 'Updated RPC Server valid IPs to: ' + FValidIPs)
end;

procedure TRPCServer.SetWalletKeys(const Value: TKeyManager);
begin
  FWalletKeys := Value;
end;

function TRPCServer.IsValidClientIP(const clientIp: string; clientPort: Word): Boolean;
begin
  if FValidIPs = '' then
    Result := true
  else
  begin
    Result := pos(clientIp, FValidIPs) > 0;
  end;
end;

constructor TRPCServer.Create;
begin
  inherited;
  if Assigned(FInstance)
  then raise Exception.Create('Multiple instance');
  FActive := false;
  FRPCLog := nil;
  FIniFile := nil;
  FIniFileName := '';
  FJSON20Strict := true;
  FWalletKeys := nil;
  FRPCServerThread := nil;
  FPort := cJsonRPCPort;
  FCallsCounter := 0;
  FValidIPs := '127.0.0.1;localhost'; // New Build 1.5 - By default, only localhost can access to RPC
  FNodeNotifyEvents := TNodeNotifyEvents.Create(nil);
  FNodeNotifyEvents.OnTransactionsChanged := OnNodeNewOperation;
  FInstance := self;
end;

destructor TRPCServer.Destroy;
begin
  FreeAndNil(FRPCLog);
  Active := false;
  FNodeNotifyEvents.Free;
  inherited Destroy;
end;

procedure TRPCServerThread.BCExecute;
var
  ClientSock: TSocket;
begin
  with FServerSocket do
  begin
    CreateSocket;
    setLinger(true, 10000);
    bind('0.0.0.0', Inttostr(FPort));
    listen;
    repeat
      if terminated then
        break;
      try
        if canread(1000) then
        begin
          ClientSock := accept;
          if lasterror = 0 then
          begin
            TRPCHandler.Create(ClientSock);
          end;
        end;
      except
        on E: Exception do
        begin
          TLog.NewLog(lterror, Classname, 'Error ' + E.Classname + ':' + E.Message);
        end;
      end;
      sleep(1);
    until false;
  end;
end;

constructor TRPCServerThread.Create(Port: Word);
begin
  TLog.NewLog(ltinfo, Classname, 'Activating RPC-JSON Server on port ' + Inttostr(Port));
  FServerSocket := TTCPBlockSocket.Create;
  FPort := Port;
  inherited Create(false);
end;

destructor TRPCServerThread.Destroy;
begin
  TLog.NewLog(ltinfo, Classname, 'Stoping RPC-JSON Server');
  FreeAndNil(FServerSocket);
  inherited Destroy;
end;

end.
