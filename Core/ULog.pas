unit ULog;

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

uses Classes, UThread, SyncObjs;

type
  TLogType = (ltinfo, ltupdate, lterror, ltdebug);
  TLogTypes = set of TLogType;

  TNewLogEvent = procedure(logtype: TLogType; Time: TDateTime; ThreadID: Cardinal; const sender, logtext: AnsiString)
    of object;

  TLog = class;

  { TThreadSafeLogEvent }

  TThreadSafeLogEvent = class(TPCThread)
    FLog: TLog;
    procedure SynchronizedProcess;
  protected
    procedure BCExecute; override;
  public
    constructor Create(Suspended: Boolean);
  end;

  TLogData = record
    logtype: TLogType;
    Time: TDateTime;
    ThreadID: Cardinal;
    sender, logtext: AnsiString;
  end;

  ILog = interface
    procedure Log(ALogType : TLogType; ASender, ALogText : AnsiString);
    procedure Debug(ASender, ALogText : AnsiString);
    procedure Info(ASender, ALogText : AnsiString);
    procedure Warning(ASender, ALogText : AnsiString);
    procedure Error(ASender, ALogText : AnsiString);
  end;

  TLog = class(TComponent)
  private
    FLogDataList: TThreadList;
    FOnNewLog: TNewLogEvent;
    FOnInThreadNewLog: TNewLogEvent;
    FFileStream: TFileStream;
    FFileName: AnsiString;
    FSaveTypes: TLogTypes;
    FThreadSafeLogEvent: TThreadSafeLogEvent;
    FProcessGlobalLogs: Boolean;
    FLock: TCriticalSection;
    procedure SetFileName(const Value: AnsiString);
  protected
    procedure DoLog(logtype: TLogType; sender, logtext: AnsiString); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class procedure NewLog(logtype: TLogType; const sender, logtext: string);
    property OnInThreadNewLog: TNewLogEvent read FOnInThreadNewLog write FOnInThreadNewLog;
    property OnNewLog: TNewLogEvent read FOnNewLog write FOnNewLog;
    property FileName: AnsiString read FFileName write SetFileName;
    property SaveTypes: TLogTypes read FSaveTypes write FSaveTypes;
    property ProcessGlobalLogs: Boolean read FProcessGlobalLogs write FProcessGlobalLogs;
    procedure NotifyNewLog(logtype: TLogType; const sender, logtext: string);
  end;

const
  CT_LogType: array [TLogType] of AnsiString = ('Info', 'Update', 'Error', 'Debug');
  CT_TLogTypes_ALL: TLogTypes = [ltinfo, ltupdate, lterror, ltdebug];
  CT_TLogTypes_DEFAULT: TLogTypes = [ltinfo, ltupdate, lterror];

implementation

uses SysUtils;

var
  _logs: TList;

type
  PLogData = ^TLogData;

  { TLog }

constructor TLog.Create(AOwner: TComponent);
begin
  FLock := TCriticalSection.Create;
  FProcessGlobalLogs := true;
  FLogDataList := TThreadList.Create;
  FFileStream := nil;
  FFileName := '';
  FSaveTypes := CT_TLogTypes_DEFAULT;
  FOnInThreadNewLog := nil;
  FOnNewLog := nil;
  if (not assigned(_logs)) then
    _logs := TList.Create;
  _logs.Add(self);
  FThreadSafeLogEvent := TThreadSafeLogEvent.Create(true);
  FThreadSafeLogEvent.FLog := self;
  FThreadSafeLogEvent.Suspended := false;
  inherited;
end;

destructor TLog.Destroy;
var
  l: TList;
  i: Integer;
  P: PLogData;
begin
  FOnNewLog := nil;
  FOnInThreadNewLog := nil;
  FThreadSafeLogEvent.Terminate;
  FThreadSafeLogEvent.WaitFor;
  FreeAndNil(FThreadSafeLogEvent);
  _logs.Remove(self);
  FreeAndNil(FFileStream);
  l := FLogDataList.LockList;
  try
    for i := 0 to l.Count - 1 do
    begin
      P := PLogData(l[i]);
      Dispose(P);
    end;
    l.Clear;
  finally
    FLogDataList.UnlockList;
  end;
  FreeAndNil(FLogDataList);
  FreeAndNil(FLock);
  inherited;
end;

procedure TLog.DoLog(logtype: TLogType; sender, logtext: AnsiString);
begin
  //
end;

class procedure TLog.NewLog(logtype: TLogType; const sender, logtext: string);
var
  i: Integer;
begin
  if (not assigned(_logs)) then
    exit;
  for i := 0 to _logs.Count - 1 do
  begin
    if (TLog(_logs[i]).FProcessGlobalLogs) then
    begin
      TLog(_logs[i]).NotifyNewLog(logtype, sender, logtext);
    end;
  end;
end;

procedure TLog.NotifyNewLog(logtype: TLogType; const sender, logtext: string);
var
  s, tid: AnsiString;
  P: PLogData;
begin
  FLock.Acquire;
  try
    if assigned(FFileStream) and (logtype in FSaveTypes) then
    begin
      if TThread.CurrentThread.ThreadID = MainThreadID then
        tid := ' MAIN:'
      else
        tid := ' TID:';
      s := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', now) + tid + IntToHex(TThread.CurrentThread.ThreadID, 8) + ' [' +
        CT_LogType[logtype] + '] <' + sender + '> ' + logtext + #13#10;
      FFileStream.Write(s[1], length(s));
    end;
    if assigned(FOnInThreadNewLog) then
    begin
      FOnInThreadNewLog(logtype, now, TThread.CurrentThread.ThreadID, sender, logtext);
    end;
    if assigned(FOnNewLog) then
    begin
      // Add to a thread safe list
      New(P);
      P^.logtype := logtype;
      P^.Time := now;
      P^.ThreadID := TThread.CurrentThread.ThreadID;
      P^.sender := sender;
      P^.logtext := logtext;
      FLogDataList.Add(P);
    end;
  finally
    FLock.Release;
  end;
  DoLog(logtype, sender, logtext);
end;

procedure TLog.SetFileName(const Value: AnsiString);
var
  fm: Word;
begin
  if FFileName = Value then
    exit;
  if assigned(FFileStream) then
  begin
    FreeAndNil(FFileStream);
  end;
  FFileName := Value;
  if (FFileName <> '') then
  begin
    if not ForceDirectories(ExtractFileDir(FFileName)) then
      exit;
    if FileExists(FFileName) then
      fm := fmOpenWrite + fmShareDenyWrite
    else
      fm := fmCreate + fmShareDenyWrite;
    FFileStream := TFileStream.Create(FFileName, fm);
    FFileStream.Position := FFileStream.size; // To the end!
    NotifyNewLog(ltinfo, Classname, 'Log file start: ' + FFileName);
  end;
end;

{ TThreadSafeLogEvent }

procedure TThreadSafeLogEvent.BCExecute;
begin
  while (not Terminated) do
  begin
    sleep(100);
    if (not Terminated) and (assigned(FLog.OnNewLog)) then
    begin
      Synchronize(SynchronizedProcess);
    end;
  end;
end;

constructor TThreadSafeLogEvent.Create(Suspended: Boolean);
begin
  inherited Create(Suspended);
end;

procedure TThreadSafeLogEvent.SynchronizedProcess;
var
  l: TList;
  i: Integer;
  P: PLogData;
begin
  if not assigned(FLog) then
    exit;
  if not assigned(FLog.FOnNewLog) then
    exit;
  // This event is thread safe and will do OnNewLog on main thread
  l := FLog.FLogDataList.LockList;
  try
    try
      for i := 0 to l.Count - 1 do
      begin
        P := PLogData(l[i]);
        if assigned(FLog.FOnNewLog) then
        begin
          FLog.OnNewLog(P^.logtype, P^.Time, P^.ThreadID, P^.sender, P^.logtext);
        end;
        Dispose(P);
      end;
    finally
      // Protection for possible raise
      l.Clear;
    end;
  finally
    FLog.FLogDataList.UnlockList;
  end;
end;

initialization

_logs := nil;

finalization

FreeAndNil(_logs);

end.
