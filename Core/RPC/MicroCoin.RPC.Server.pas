unit MicroCoin.RPC.Server;

{
  This unit contains code from PascalCoin:

  Copyright (c) Albert Molina 2016 - 2018 original code from PascalCoin https://pascalcoin.org/

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

}

interface

uses Sysutils, Classes, httpsend, UWalletkeys, Ulog, Inifiles,
{$IFDEF fpc} fpjson, {$ELSE}System.Json, {$ENDIF} UJSONFunctions, synautil,
  UConst, UThread, blcksock, synsock, MicroCoin.RPC.Handler, MicroCoin.Node.Events,
  UCrypto, MicroCoin.Common, MicroCoin.Transaction.Base, MicroCoin.Account.Data;

type

  TRPCServerThread = class(TPCThread)
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
    FWalletKeys: TWalletKeysExt;
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
    property WalletKeys: TWalletKeysExt read FWalletKeys write FWalletKeys;
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

procedure TRPCServer.OnNodeNewOperation(Sender: TObject);
var
  i, j: Integer;
  Op: ITransaction;
  OPR: TTransactionData;
  an: cardinal;
  ToAccount: string;
  Amount: string;
  FromAccount: string;
  Payload: string;
  HTTP: THTTPSend;
  Decrypted: string;
  WalletKey: TWalletKey;
  Result: Boolean;
  from: string;
  ini: TIniFile;
  Proxy: string;
  stream: TMemoryStream;
  a: string;
  sp: pchar;
  fee: string;
  ophash: string;
  balance: string;
begin

  ini := TIniFile.Create('microcoin.ini');

  Proxy := ini.ReadString('Events', 'Transaction', '');
  FreeAndNil(ini);
  if Proxy = '' then
    exit;
  { Script := TPSScript.Create(nil); }
  for i := 0 to TNodeNotifyEvents(Sender).Node.Operations.Count - 1 do
  begin
    Op := TNodeNotifyEvents(Sender).Node.Operations.Operation[i];
    Op := TNodeNotifyEvents(Sender).Node.Operations.OperationsHashTree.GetOperation(i);
    {
      Script.AddRegisteredVariable('FromAccount', 'btString');
      Script.AddRegisteredVariable('ToAccount', 'btString');
      Script.AddRegisteredVariable('Amount', 'btString');
      Script.AddRegisteredVariable('Fee', 'btString');
      Script.AddRegisteredVariable('Balance', 'btString');
      Script.AddRegisteredVariable('OpHash', 'btString');
      Script.AddRegisteredVariable('Payload', 'btString');
    }
    Op.GetTransactionData(0, Op.SignerAccount, OPR);
    an := OPR.DestAccount;
    FromAccount := TAccount.AccountNumberToAccountTxtNumber(OPR.AffectedAccount);
    ToAccount := TAccount.AccountNumberToAccountTxtNumber(OPR.DestAccount);
    Amount := TCurrencyUtils.FormatMoney(OPR.Amount * -1);
    fee := TCurrencyUtils.FormatMoney(OPR.fee);
    balance := TCurrencyUtils.FormatMoney(OPR.balance);
    ophash := TCrypto.ToHexaString(OPR.OperationHash);
    if TCrypto.IsHumanReadable(OPR.OriginalPayload) then
      Payload := OPR.OriginalPayload
    else
      Payload := '';
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
      stream := TMemoryStream.Create;
      HTTP := THTTPSend.Create;
      try
        with TPCJSONObject.Create do
        begin
          GetAsVariant('from').Value := from;
          GetAsVariant('to').Value := ToAccount;
          GetAsVariant('amount').Value := Amount;
          GetAsVariant('fee').Value := fee;
          GetAsVariant('balance').Value := balance;
          GetAsVariant('ophash').Value := ophash;
          GetAsVariant('payload').Value := Payload;
          a := ToJSON(false);
          Free;
        end;
        WriteStrToStream(HTTP.Document, a);
        HTTP.MimeType := 'application/json';
        HTTP.Protocol := '1.1';
        Result := HTTP.HTTPMethod('POST', Proxy);
        if Result then
        begin
          stream.CopyFrom(HTTP.Document, 0);
          stream.Position := 0;
          sp := StrAlloc(stream.size);
          stream.Read(sp^, stream.size);
          a := StrPas(sp);
        end;
      finally
        HTTP.Free;
      end;
    finally
      stream.Free;
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
  TLog.NewLog(ltupdate, Classname, 'Updated RPC Server to Active=' + CT_TRUE_FALSE[FActive]);
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
  FActive := false;
  FRPCLog := nil;
  FIniFile := nil;
  FIniFileName := '';
  FJSON20Strict := true;
  FWalletKeys := nil;
  FRPCServerThread := nil;
  FPort := CT_JSONRPC_Port;
  FCallsCounter := 0;
  FValidIPs := '127.0.0.1;localhost'; // New Build 1.5 - By default, only localhost can access to RPC
  FNodeNotifyEvents := TNodeNotifyEvents.Create(nil);
  FNodeNotifyEvents.OnOperationsChanged := OnNodeNewOperation;
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
