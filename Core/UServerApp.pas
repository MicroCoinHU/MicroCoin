unit UServerApp;

{
  Copyright (c) Albert Molina 2016 - 2018 original code from PascalCoin https://pascalcoin.org/

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

}

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}
{$IFDEF MSWINDOWS}
{$DEFINE OS_MSWIN}
{$ENDIF}

interface

uses
{$IFDEF LCL}
  interfaces,
{$ENDIF}
{$IFDEF OS_MSWIN}
  Windows,
  Messages,
{$ENDIF}
  SyncObjs,
  MicroCoin.Node.Node,
  UOpenSSL, UCrypto, MicroCoin.BlockChain.FileStorage, UFolderHelper, UWalletKeys, UConst, ULog,
  MicroCoin.RPC.Server;

type
  TMicroCoinServerLogType = (sltDebug, sltInfo, sltError, sltWarning);

  TMicroCoinServerAppLogEvent = procedure(LogType: TMicroCoinServerLogType; Msg: string; Level: Integer) of object;

  { TMicroCoinServerApp }

  TMicroCoinServerApp = class
  private
    FLock: TCriticalSection;
    FOnLog: TMicroCoinServerAppLogEvent;
    FTerminated: Boolean;
{$IFDEF OS_MSWIN}
    hStdIn: THandle;
{$ENDIF}
    FNode: TNode;
    FWalletKeys: TWalletKeysExt;
    FRPC: TRPCServer;
    FLog: TLog;

    procedure Lock;
    procedure Unlock;

    procedure Log(const LogType: TMicroCoinServerLogType; const Msg: string; const Level: Integer = 0); overload;
    procedure Log(const LogType: TMicroCoinServerLogType; const Msg: string; const Params: array of const; const Level: Integer = 0); overload;
    procedure OnMicroCoinLog(LogType: TLogType; Time: TDateTime; ThreadID: Cardinal; const sender, logtext: AnsiString);

    function GetTerminated: Boolean;
    procedure SetTerminated;

    function ProcessOSMessage(out Terminate: Boolean): Boolean;
    function QuitKeyPressed: Boolean;
    function ProcessApplication: Boolean;
    function Process: Boolean;
    procedure ProcessOrWait;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Init;
    procedure Run;
    procedure Stop;

    property Terminated: Boolean read GetTerminated;
    property OnLog: TMicroCoinServerAppLogEvent read FOnLog write FOnLog;
  end;

var
  ServerApp: TMicroCoinServerApp = nil;

implementation

uses
  SysUtils, Classes, MicroCoin.Net.ConnectionManager;

{ TMicroCoinServerApp }

constructor TMicroCoinServerApp.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FLog := TLog.Create(nil);
  FLog.OnInThreadNewLog := OnMicroCoinLog;
{$IFDEF OS_MSWIN}
  // get the console input handle
  hStdIn := GetStdHandle(STD_INPUT_HANDLE);
{$ENDIF}
  FWalletKeys := TWalletKeysExt.Create(nil);
end;

destructor TMicroCoinServerApp.Destroy;
begin
  FreeAndNil(FWalletKeys);
  FLog.OnNewLog := nil;
  FreeAndNil(FLog);
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TMicroCoinServerApp.Lock;
begin
  FLock.Acquire;
end;

procedure TMicroCoinServerApp.Unlock;
begin
  FLock.Release;
end;

procedure TMicroCoinServerApp.Log(const LogType: TMicroCoinServerLogType; const Msg: string; const Level: Integer);
begin
  if Assigned(FOnLog) then
    FOnLog(LogType, Msg, Level);
end;

procedure TMicroCoinServerApp.Log(const LogType: TMicroCoinServerLogType; const Msg: string; const Params: array of const; const Level: Integer);
begin
  Log(LogType, Format(Msg, Params), Level);
end;

procedure TMicroCoinServerApp.OnMicroCoinLog(LogType: TLogType; Time: TDateTime; ThreadID: Cardinal; const sender, logtext: AnsiString);
var
  s: AnsiString;
begin
  if (LogType = ltdebug) then
    exit;
  if ThreadID = MainThreadID then
    s := ' MAIN:'
  else
    s := ' TID:';
  Log(sltInfo, s + IntToHex(ThreadID, 8) + ' [' + CT_LogType[LogType] + '] <' + sender + '> ' + logtext);
end;

function TMicroCoinServerApp.GetTerminated: Boolean;
begin
  Lock;
  try
    Result := FTerminated;
  finally
    Unlock;
  end;
end;

procedure TMicroCoinServerApp.SetTerminated;
begin
  Lock;
  try
    FTerminated := True;
  finally
    Unlock;
  end;
end;

// Returns True if OS message processed
// Terminate is returned True if application terminated
function TMicroCoinServerApp.ProcessOSMessage(out Terminate: Boolean): Boolean;
{$IFDEF OS_MSWIN}
var
  Msg: TMsg;
{$ENDIF}
begin
  Terminate := False;
{$IFDEF OS_MSWIN}
  Result := PeekMessageA(Msg, 0, 0, 0, PM_REMOVE);
  if Result then
    if Msg.Message = WM_QUIT then
      Terminate := True
    else
    begin
      TranslateMessage(Msg);
      DispatchMessageA(Msg);
    end
{$ELSE}
  Result := False;
{$ENDIF}
end;

function TMicroCoinServerApp.QuitKeyPressed: Boolean;
{$IFDEF OS_MSWIN}
const
  MaxConsoleEvents = 64;
var
  NumberOfEvents: DWORD;
  ConsoleEvents: array [0 .. MaxConsoleEvents - 1] of TInputRecord;
  EvtP: PInputRecord;
  NumberOfEventsRead: DWORD;
  I: Integer;
  QuitKeyDown: Boolean;
begin
  QuitKeyDown := False;
  // get the number of events
  NumberOfEvents := 0;
  GetNumberOfConsoleInputEvents(hStdIn, NumberOfEvents);
  if NumberOfEvents <> 0 then
  begin
    // retrieve the event
    NumberOfEventsRead := 0;
    PeekConsoleInput(hStdIn, ConsoleEvents[0], MaxConsoleEvents, NumberOfEventsRead);
    for I := 0 to NumberOfEventsRead - 1 do
    begin
      EvtP := @ConsoleEvents[I];
      if EvtP^.EventType = KEY_EVENT then
        if EvtP^.Event.KeyEvent.bKeyDown and ((EvtP^.Event.KeyEvent.UnicodeChar = 'q') or (EvtP^.Event.KeyEvent.UnicodeChar = 'Q')) then
        begin
          QuitKeyDown := True;
          break;
        end;
    end;
    // flush the buffer
    FlushConsoleInputBuffer(hStdIn);
  end;
  Result := QuitKeyDown;
end;
{$ELSE}

var
  C: Char;
begin
  Result := False;
  read(C);
  if (C = 'q') or (C = 'Q') then
    Result := True;
end;
{$ENDIF}

function TMicroCoinServerApp.ProcessApplication: Boolean;
begin
  Result := False;
end;

// Returns True if state processed
// Returns False if idle
function TMicroCoinServerApp.Process: Boolean;
var
  Busy: Boolean;
  DoTerminate: Boolean;
begin
  Busy := True;
  DoTerminate := False;
  if QuitKeyPressed then
    DoTerminate := True
  else if not ProcessOSMessage(DoTerminate) then
    if not ProcessApplication then
      Busy := False;
  if DoTerminate then
    SetTerminated;
  Result := Busy;
end;

procedure TMicroCoinServerApp.ProcessOrWait;
begin
  if not Process then
    Sleep(1);
end;

procedure TMicroCoinServerApp.Init;
begin
  TLog.NewLog(ltinfo, Classname, 'MicroCoin Server');
  // Load Node
  // Check OpenSSL dll
  if not LoadSSLCrypt then
    raise Exception.Create('Cannot load ' + SSL_C_LIB + #10 + 'To use this software make sure this file is available on you system or reinstall the application');
  TCrypto.InitCrypto;
  FWalletKeys.WalletFileName := TFolderHelper.GetMicroCoinDataFolder + PathDelim + 'WalletKeys.dat';
  // Creating Node:
  FNode := TNode.Node;
  // RPC Server
  Log(sltInfo, 'Activating RPC server');
  FRPC := TRPCServer.Create;
  FRPC.WalletKeys := FWalletKeys;
  FRPC.Active := True;
  // Check Database
  FNode.Bank.StorageClass := TFileStorage;
  TFileStorage(FNode.Bank.Storage).DatabaseFolder := TFolderHelper.GetMicroCoinDataFolder + PathDelim + 'Data';
  // Reading database
  Log(sltInfo, 'Reading database and constructing MicroCoin accounts');
  FNode.Node.Bank.DiskRestoreFromOperations(CT_MaxBlock);
  FWalletKeys.SafeBox := FNode.Node.Bank.AccountStorage;
  Log(sltInfo, 'Start discovering nodes');
  FNode.Node.AutoDiscoverNodes(CT_Discover_IPs);
  FNode.Node.NetServer.Active := True;
end;

procedure TMicroCoinServerApp.Run;
begin
  Log(sltInfo, 'Start');
  Log(sltInfo, 'Running (press Q to stop)');
  while not GetTerminated do
    ProcessOrWait;
end;

procedure TMicroCoinServerApp.Stop;
begin
  Log(sltInfo, 'Stop');
  FreeAndNil(FRPC);
  FNode.NetServer.Active := False;
  TConnectionManager.NetData.Free;
  FreeAndNil(FNode);
  Log(sltInfo, 'Finalized');
end;

end.
