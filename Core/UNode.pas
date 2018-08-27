unit UNode;

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

{ UNode contains the basic structure to operate
  - An app can only contains 1 node.
  - A node contains:
  - 1 Bank
  - 1 NetServer  (Accepting incoming connections)
  - 1 Operations (Operations has actual BlockChain with Operations and SafeBankTransaction to operate with the Bank)
  - 0..x NetClients
  - 0..x Miners
}

interface

uses
  Classes, MicroCoin.BlockChain.BlockManager, UNetProtocol, UCrypto,
  UThread, SyncObjs, ULog, MicroCoin.Transaction.Base,
  MicroCoin.Common.Lists,
  MicroCoin.Account.Data,
  MicroCoin.BlockChain.Block,
  MicroCoin.Transaction.TransactionList, MicroCoin.Transaction.HashTree,
  MicroCoin.Account.Storage, MicroCoin.BlockChain.BlockHeader;

{$I config.inc}


type

  { TNode }

  TNode = class(TComponent)
  private
    FNodeLog: TLog;
    FLockNodeOperations: TPCCriticalSection;
    FOperationSequenceLock: TPCCriticalSection;
    FNotifyList: TList;
    FBank: TBlockManager;
    FOperations: TBlock;
    FNetServer: TNetServer;
    FBCBankNotify: TBlockManagerNotify;
    FPeerCache: AnsiString;
    FDisabledsNewBlocksCount: Integer;
    FSentOperations: TOrderedRawList;
{$IFDEF BufferOfFutureOperations}
    FBufferAuxWaitingOperations: TOperationsHashTree;
{$ENDIF}
    procedure OnBankNewBlock(Sender: TObject);
    procedure SetNodeLogFilename(const Value: AnsiString);
    function GetNodeLogFilename: AnsiString;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    class function Node: TNode;
    class procedure DecodeIpStringToNodeServerAddressArray(const Ips: AnsiString; var NodeServerAddressArray: TNodeServerAddressArray);
    class function EncodeNodeServerAddressArrayToIpString(const NodeServerAddressArray: TNodeServerAddressArray): AnsiString;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Bank: TBlockManager read FBank;
    function NetServer: TNetServer;
    procedure NotifyNetClientMessage(Sender: TNetConnection; const TheMessage: AnsiString);
    //
    property Operations: TBlock read FOperations;
    //
    function AddNewBlockChain(SenderConnection: TNetConnection; NewBlockOperations: TBlock; var newBlockAccount: TAccountStorageEntry; var errors: AnsiString): Boolean;
    function AddOperations(SenderConnection: TNetConnection; Operations: TTransactionHashTree; OperationsResult: TTransactionList; var errors: AnsiString): Integer;
    function AddOperation(SenderConnection: TNetConnection; Operation: ITransaction; var errors: AnsiString): Boolean;
    function SendNodeMessage(Target: TNetConnection; TheMessage: AnsiString; var errors: AnsiString): Boolean;
    //
    procedure NotifyBlocksChanged;
    //
    procedure GetStoredOperationsFromAccount(const OperationsResume: TTransactionList; account_number: Cardinal; MaxDepth, StartOperation, EndOperation: Integer);
    function FindOperation(const OperationComp: TBlock; const OperationHash: TRawBytes; var block: Cardinal; var operation_block_index: Integer): Boolean;
    //
    procedure AutoDiscoverNodes(const Ips: AnsiString);
    function IsBlockChainValid(var WhyNot: AnsiString): Boolean;
    function IsReady(var CurrentProcess: AnsiString): Boolean;
    property PeerCache: AnsiString read FPeerCache write FPeerCache;
    procedure DisableNewBlocks;
    procedure EnableNewBlocks;
    property NodeLogFilename: AnsiString read GetNodeLogFilename write SetNodeLogFilename;
    property OperationSequenceLock: TPCCriticalSection read FOperationSequenceLock;
  end;

  TNodeNotifyEvents = class;

  TThreadSafeNodeNotifyEvent = class(TPCThread)
    FNodeNotifyEvents: TNodeNotifyEvents;
    FNotifyBlocksChanged: Boolean;
    FNotifyOperationsChanged: Boolean;
    procedure SynchronizedProcess;
  protected
    procedure BCExecute; override;
    constructor Create(ANodeNotifyEvents: TNodeNotifyEvents);
  end;

  TNodeMessageEvent = procedure(NetConnection: TNetConnection; MessageData: TRawBytes) of object;

  { TNodeNotifyEvents is ThreadSafe and will only notify in the main thread }
  TNodeNotifyEvents = class(TComponent)
  private
    FNode: TNode;
    FPendingNotificationsList: TPCThreadList;
    FThreadSafeNodeNotifyEvent: TThreadSafeNodeNotifyEvent;
    FOnBlocksChanged: TNotifyEvent;
    FOnOperationsChanged: TNotifyEvent;
    FMessages: TStringList;
    FOnNodeMessageEvent: TNodeMessageEvent;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetNode(const Value: TNode);
    procedure NotifyBlocksChanged;
    procedure NotifyOperationsChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Node: TNode read FNode write SetNode;
    property OnBlocksChanged: TNotifyEvent read FOnBlocksChanged write FOnBlocksChanged;
    property OnOperationsChanged: TNotifyEvent read FOnOperationsChanged write FOnOperationsChanged;
    property OnNodeMessageEvent: TNodeMessageEvent read FOnNodeMessageEvent write FOnNodeMessageEvent;
  end;

  TThreadNodeNotifyNewBlock = class(TPCThread)
    FNetConnection: TNetConnection;
    FSanitizedOperationsHashTree: TTransactionHashTree;
    FNewBlockOperations: TBlock;
  protected
    procedure BCExecute; override;
    constructor Create(NetConnection: TNetConnection; MakeACopyOfNewBlockOperations: TBlock; MakeACopyOfSanitizedOperationsHashTree: TTransactionHashTree);
    destructor Destroy; override;
  end;

  TThreadNodeNotifyOperations = class(TPCThread)
    FNetConnection: TNetConnection;
  protected
    procedure BCExecute; override;
    constructor Create(NetConnection: TNetConnection; MakeACopyOfOperationsHashTree: TTransactionHashTree);
    destructor Destroy; override;
  end;

implementation

uses SysUtils, UConst, UTime, MicroCoin.Transaction.Transaction;

var
  _Node: TNode;

resourcestring
  rsAccountSZero = 'Account %s zero fee operations per block limit:%d';
  rsBlockchainRe = 'Blockchain reward';

  { TNode }

function TNode.AddNewBlockChain(SenderConnection: TNetConnection; NewBlockOperations: TBlock;
  var newBlockAccount: TAccountStorageEntry; var errors: AnsiString): Boolean;
var
  i, j: Integer;
  nc: TNetConnection;
  ms: TMemoryStream;
  s: string;
  errors2: AnsiString;
  OpBlock: TBlockHeader;
  opsht: TTransactionHashTree;
begin
  Result := false;
  errors := '';
  if FDisabledsNewBlocksCount > 0 then
  begin
    TLog.NewLog(lterror, Classname, Format('Cannot Add new BlockChain due is adding disabled - Connection:%s NewBlock:%s', [
      Inttohex(PtrInt(SenderConnection), 8), TBlock.OperationBlockToText(NewBlockOperations.OperationBlock)]));
    errors := 'Adding blocks is disabled';
    exit;
  end;
  if NewBlockOperations.OperationBlock.block <> Bank.BlocksCount then
  begin
    errors := 'New block number (' + IntToStr(NewBlockOperations.OperationBlock.block) + ') not valid! (Expected ' + IntToStr(Bank.BlocksCount) + ')';
    exit;
  end;
  OpBlock := NewBlockOperations.OperationBlock;
  TLog.NewLog(ltdebug, Classname, Format('AddNewBlockChain Connection:%s NewBlock:%s', [
    Inttohex(PtrInt(SenderConnection), 8), TBlock.OperationBlockToText(OpBlock)]));
  if not TPCThread.TryProtectEnterCriticalSection(Self, 2000, FLockNodeOperations) then
  begin
    if NewBlockOperations.OperationBlock.block <> Bank.BlocksCount then
      exit;
    s := 'Cannot AddNewBlockChain due blocking lock operations node';
    TLog.NewLog(lterror, Classname, s);
    if TThread.CurrentThread.ThreadID = MainThreadID then
      raise Exception.Create(s)
    else
      exit;
  end;
  try
    // Check block number:
    if TBlock.EqualsOperationBlock(Bank.LastOperationBlock, NewBlockOperations.OperationBlock) then
    begin
      errors := 'Duplicated block';
      exit;
    end;
    ms := TMemoryStream.Create;
    try
      FOperations.SaveBlockToStream(false, ms);
      Result := Bank.AddNewBlockChainBlock(NewBlockOperations, TNetData.NetData.NetworkAdjustedTime.GetMaxAllowedTimestampForNewBlock, newBlockAccount, errors);
      if Result then
      begin
        if Assigned(SenderConnection) then
        begin
          FNodeLog.NotifyNewLog(ltupdate, SenderConnection.Classname, Format(';%d;%s;%s;;%d;%d;%d;%s', [OpBlock.block, SenderConnection.ClientRemoteAddr, OpBlock.block_payload,
            OpBlock.timestamp, UnivDateTimeToUnix(DateTime2UnivDateTime(Now)), UnivDateTimeToUnix(DateTime2UnivDateTime(Now)) - OpBlock.timestamp, Inttohex(OpBlock.compact_target, 8)]));
        end
        else
        begin
          FNodeLog.NotifyNewLog(ltupdate, Classname, Format(';%d;%s;%s;;%d;%d;%d;%s', [OpBlock.block, 'NIL', OpBlock.block_payload,
            OpBlock.timestamp, UnivDateTimeToUnix(DateTime2UnivDateTime(Now)), UnivDateTimeToUnix(DateTime2UnivDateTime(Now)) - OpBlock.timestamp, Inttohex(OpBlock.compact_target, 8)]));
        end;
      end
      else
      begin
        if Assigned(SenderConnection) then
        begin
          FNodeLog.NotifyNewLog(lterror, SenderConnection.Classname, Format(';%d;%s;%s;%s;%d;%d;%d;%s', [OpBlock.block, SenderConnection.ClientRemoteAddr, OpBlock.block_payload, errors,
            OpBlock.timestamp, UnivDateTimeToUnix(DateTime2UnivDateTime(Now)), UnivDateTimeToUnix(DateTime2UnivDateTime(Now)) - OpBlock.timestamp, Inttohex(OpBlock.compact_target, 8)]));
        end
        else
        begin
          FNodeLog.NotifyNewLog(lterror, Classname, Format(';%d;%s;%s;%s;%d;%d;%d;%s', [OpBlock.block, 'NIL', OpBlock.block_payload, errors,
            OpBlock.timestamp, UnivDateTimeToUnix(DateTime2UnivDateTime(Now)), UnivDateTimeToUnix(DateTime2UnivDateTime(Now)) - OpBlock.timestamp, Inttohex(OpBlock.compact_target, 8)]));
        end;
      end;
      FOperations.Clear(true);
      ms.Position := 0;
      if not FOperations.LoadBlockFromStream(ms, errors2) then
      begin
        TLog.NewLog(lterror, Classname, 'Error recovering operations to sanitize: ' + errors2);
        if Result then
          errors := errors2
        else
          errors := errors + ' - ' + errors2;
      end;
    finally
      ms.Free;
    end;
    FOperations.SanitizeOperations;
    if Result then
    begin
      opsht := TTransactionHashTree.Create;
      try
        for i := 0 to FOperations.Count - 1 do
        begin
          opsht.AddTransactionToHashTree(FOperations.Operation[i]);
          // Add to sent operations
          FSentOperations.Add(FOperations.Operation[i].Sha256, Bank.LastBlockFound.OperationBlock.block);
        end;
        if opsht.OperationsCount > 0 then
        begin
          TLog.NewLog(ltinfo, Classname, 'Resending ' + IntToStr(opsht.OperationsCount) + ' operations for new block');
          for i := 0 to opsht.OperationsCount - 1 do
          begin
            TLog.NewLog(ltinfo, Classname, 'Resending (' + IntToStr(i + 1) + '/' + IntToStr(opsht.OperationsCount) + '): ' + opsht.GetOperation(i).ToString);
          end;
        end;
        // Clean sent operations buffer
        j := 0;
        for i := FSentOperations.Count - 1 downto 0 do
        begin
          if (FSentOperations.GetTag(i) < Bank.LastBlockFound.OperationBlock.block - 2) then
          begin
            FSentOperations.Delete(i);
            inc(j);
          end;
        end;
        if j > 0 then
        begin
          TLog.NewLog(ltdebug, Classname, 'Buffer Sent operations: Deleted ' + IntToStr(j) + ' old operations');
        end;
        TLog.NewLog(ltdebug, Classname, 'Buffer Sent operations: ' + IntToStr(FSentOperations.Count));
        // Notify to clients
        j := TNetData.NetData.ConnectionsCountAll;
        for i := 0 to j - 1 do
        begin
          if (TNetData.NetData.GetConnection(i, nc)) then
          begin
            if (nc <> SenderConnection) and (nc.Connected) then
            begin
              TThreadNodeNotifyNewBlock.Create(nc, Bank.LastBlockFound, opsht);
            end;
          end;
        end;
      finally
        opsht.Free;
      end;
    end;
  finally
    FLockNodeOperations.Release;
    TLog.NewLog(ltdebug, Classname, Format('Finalizing AddNewBlockChain Connection:%s NewBlock:%s', [
      Inttohex(PtrInt(SenderConnection), 8), TBlock.OperationBlockToText(OpBlock)]));
  end;
  if Result then
  begin
    // Notify it!
    NotifyBlocksChanged;
  end;
end;

function TNode.AddOperation(SenderConnection: TNetConnection; Operation: ITransaction; var errors: AnsiString): Boolean;
var
  ops: TTransactionHashTree;
begin
  ops := TTransactionHashTree.Create;
  try
    ops.AddTransactionToHashTree(Operation);
    Result := AddOperations(SenderConnection, ops, nil, errors) = 1;
  finally
    ops.Free;
  end;
end;

function TNode.AddOperations(SenderConnection: TNetConnection; Operations: TTransactionHashTree; OperationsResult: TTransactionList; var errors: AnsiString): Integer;
{$IFDEF BufferOfFutureOperations}
  procedure Process_BufferOfFutureOperations(valids_operations: TOperationsHashTree);
  var
    i, j, nAdded, nDeleted: Integer;
    sAcc: TAccount;
    ActOp: TPCOperation;
    e: AnsiString;
  begin
    // Prior to add new operations, will try to add waiting ones
    nAdded := 0;
    nDeleted := 0;
    for j := 0 to 3 do
    begin
      i := 0;
      while (i < FBufferAuxWaitingOperations.OperationsCount) do
      begin
        ActOp := FBufferAuxWaitingOperations.GetOperation(i);
        if FOperations.AddOperation(true, ActOp, e) then
        begin
          TLog.NewLog(ltinfo, Classname, Format('AddOperation FromBufferWaitingOperations %d/%d: %s', [i + 1, FBufferAuxWaitingOperations.OperationsCount, ActOp.ToString]));
          inc(nAdded);
          valids_operations.AddOperationToHashTree(ActOp);
          FBufferAuxWaitingOperations.Delete(i);
        end
        else
        begin
          sAcc := FOperations.SafeBoxTransaction.Account(ActOp.SignerAccount);
          if (sAcc.n_operation > ActOp.n_operation) or
            ((sAcc.n_operation = ActOp.n_operation) and (sAcc.balance > 0)) then
          begin
            FBufferAuxWaitingOperations.Delete(i);
            inc(nDeleted);
          end
          else
            inc(i);
        end;
      end;
    end;
    if (nAdded > 0) or (nDeleted > 0) or (FBufferAuxWaitingOperations.OperationsCount > 0) then
    begin
      TLog.NewLog(ltinfo, Classname, Format('FromBufferWaitingOperations status - Added:%d Deleted:%d Buffer:%d', [nAdded, nDeleted, FBufferAuxWaitingOperations.OperationsCount]));
    end;
  end;
{$ENDIF}

var
  i, j: Integer;
  operationscomp: TBlock;
  valids_operations: TTransactionHashTree;
  nc: TNetConnection;
  e: AnsiString;
  mtl: TList;
  s: string;
  OPR: TTransactionData;
  ActOp: ITransaction;
  sAcc: TAccount;
begin
  Result := -1;
  if Assigned(OperationsResult) then
    OperationsResult.Clear;
  if FDisabledsNewBlocksCount > 0 then
  begin
    errors := Format('Cannot Add Operations due is adding disabled - OpCount:%d', [Operations.OperationsCount]);
    TLog.NewLog(ltinfo, Classname, errors);
    exit;
  end;
  Result := 0;
  errors := '';
  valids_operations := TTransactionHashTree.Create;
  try
    TLog.NewLog(ltdebug, Classname, Format('AddOperations Connection:%s Operations:%d', [
      Inttohex(PtrInt(SenderConnection), 8), Operations.OperationsCount]));
    if not TPCThread.TryProtectEnterCriticalSection(Self, 4000, FLockNodeOperations) then
    begin
      s := 'Cannot AddOperations due blocking lock operations node';
      TLog.NewLog(lterror, Classname, s);
      if TThread.CurrentThread.ThreadID = MainThreadID then
        raise Exception.Create(s)
      else
        exit;
    end;
    try
{$IFDEF BufferOfFutureOperations}
      Process_BufferOfFutureOperations(valids_operations);
{$ENDIF}
      for j := 0 to Operations.OperationsCount - 1 do
      begin
        ActOp := Operations.GetOperation(j);
        if (FOperations.OperationsHashTree.IndexOf(ActOp) < 0) and (FSentOperations.GetTag(ActOp.Sha256) = 0) then
        begin
          // Protocol 2 limitation: In order to prevent spam of operations without Fee, will protect it
          if (ActOp.Fee = 0) and (Bank.AccountStorage.CurrentProtocol >= CT_PROTOCOL_2) and
            (FOperations.OperationsHashTree.TransactionCountsWithoutFeeBySameSigner(ActOp.SignerAccount) >= CT_MaxAccountOperationsPerBlockWithoutFee) then
          begin
            e := Format(rsAccountSZero, [
              TAccount.AccountNumberToAccountTxtNumber(ActOp.SignerAccount
              ), CT_MaxAccountOperationsPerBlockWithoutFee]);
            if (errors <> '') then
              errors := errors + ' ';
            errors := errors + 'Op ' + IntToStr(j + 1) + '/' + IntToStr(Operations.OperationsCount) + ':' + e;
            TLog.NewLog(ltdebug, Classname, Format('AddOperation invalid/duplicated %d/%d: %s  - Error:%s',
              [(j + 1), Operations.OperationsCount, ActOp.ToString, e]));
            if Assigned(OperationsResult) then
            begin
              ActOp.GetTransactionData(0, ActOp.SignerAccount, OPR);
              OPR.valid := false;
              OPR.NOpInsideBlock := -1;
              OPR.OperationHash := '';
              OPR.errors := e;
              OperationsResult.Add(OPR);
            end;
          end
          else
          begin
            // Buffer to prevent cyclic sending new on 1.5.4
            FSentOperations.Add(ActOp.Sha256, FOperations.OperationBlock.block);
            if (FOperations.AddOperation(true, ActOp, e)) then
            begin
              inc(Result);
              valids_operations.AddTransactionToHashTree(ActOp);
              TLog.NewLog(ltdebug, Classname, Format('AddOperation %d/%d: %s', [(j + 1), Operations.OperationsCount, ActOp.ToString]));
              if Assigned(OperationsResult) then
              begin
                ActOp.GetTransactionData(0, ActOp.SignerAccount, OPR);
                OPR.NOpInsideBlock := FOperations.Count - 1;
                OPR.balance := FOperations.SafeBoxTransaction.Account(ActOp.SignerAccount).balance;
                OperationsResult.Add(OPR);
              end;
            end
            else
            begin
              if (errors <> '') then
                errors := errors + ' ';
              errors := errors + 'Op ' + IntToStr(j + 1) + '/' + IntToStr(Operations.OperationsCount) + ':' + e;
              TLog.NewLog(ltdebug, Classname, Format('AddOperation invalid/duplicated %d/%d: %s  - Error:%s',
                [(j + 1), Operations.OperationsCount, ActOp.ToString, e]));
              if Assigned(OperationsResult) then
              begin
                ActOp.GetTransactionData(0, ActOp.SignerAccount, OPR);
                OPR.valid := false;
                OPR.NOpInsideBlock := -1;
                OPR.OperationHash := '';
                OPR.errors := e;
                OperationsResult.Add(OPR);
              end;
{$IFDEF BufferOfFutureOperations}
              // Used to solve 2.0.0 "invalid order of operations" bug
              if (Assigned(SenderConnection)) then
              begin
                sAcc := FOperations.SafeBoxTransaction.Account(ActOp.SignerAccount);
                if (sAcc.n_operation < ActOp.n_operation) or
                  ((sAcc.n_operation = ActOp.n_operation) and (sAcc.balance = 0) and (ActOp.OperationFee > 0) and (ActOp.OpType = CT_Op_Changekey)) then
                begin
                  if FBufferAuxWaitingOperations.IndexOfOperation(ActOp) < 0 then
                  begin
                    FBufferAuxWaitingOperations.AddOperationToHashTree(ActOp);
                    TLog.NewLog(ltinfo, Classname, Format('New FromBufferWaitingOperations %d/%d (new buffer size:%d): %s',
                      [j + 1, Operations.OperationsCount, FBufferAuxWaitingOperations.OperationsCount, ActOp.ToString]));
                  end;
                end;
              end;
{$ENDIF}
            end;
          end;
        end
        else
        begin
          errors := errors + 'Unable to add operation as it has already been added.';
{$IFDEF HIGHLOG}TLog.NewLog(ltdebug, Classname, Format('AddOperation made before %d/%d: %s', [(j + 1), Operations.OperationsCount, ActOp.ToString])); {$ENDIF}
        end;
      end;
    finally
      FLockNodeOperations.Release;
      if Result <> 0 then
      begin
        TLog.NewLog(ltdebug, Classname, Format('Finalizing AddOperations Connection:%s Operations:%d valids:%d', [
          Inttohex(PtrInt(SenderConnection), 8), Operations.OperationsCount, Result]));
      end;
    end;
    if Result = 0 then
      exit;
    // Send to other nodes
    j := TNetData.NetData.ConnectionsCountAll;
    for i := 0 to j - 1 do
    begin
      if TNetData.NetData.GetConnection(i, nc) then
      begin
        if (nc <> SenderConnection) and (nc.Connected) then
          TThreadNodeNotifyOperations.Create(nc, valids_operations);
      end;
    end;
  finally
    valids_operations.Free;
  end;
  // Notify it!
  for i := 0 to FNotifyList.Count - 1 do
  begin
    TNodeNotifyEvents(FNotifyList[i]).NotifyOperationsChanged;
  end;
end;

procedure TNode.AutoDiscoverNodes(const Ips: AnsiString);
var
  i, j: Integer;
  nsarr: TNodeServerAddressArray;
begin
  DecodeIpStringToNodeServerAddressArray(Ips + ';' + PeerCache, nsarr);
  for i := low(nsarr) to high(nsarr) do
  begin
    TNetData.NetData.AddServer(nsarr[i]);
  end;
  j := (CT_MaxServersConnected - TNetData.NetData.ConnectionsCount(true));
  if j <= 0 then
    exit;
  TNetData.NetData.DiscoverServers;
end;

constructor TNode.Create(AOwner: TComponent);
begin
  FSentOperations := TOrderedRawList.Create;
  FNodeLog := TLog.Create(Self);
  FNodeLog.ProcessGlobalLogs := false;
  // RegisterOperationsClass;
  if Assigned(_Node) then
    raise Exception.Create('Duplicate nodes protection');
  TLog.NewLog(ltinfo, Classname, 'TNode.Create');
  inherited;
  FDisabledsNewBlocksCount := 0;
  FLockNodeOperations := TPCCriticalSection.Create('TNode_LockNodeOperations');
  FOperationSequenceLock := TPCCriticalSection.Create('TNode_OperationSequenceLock');
  FBank := TBlockManager.Create(Self);
  FBCBankNotify := TBlockManagerNotify.Create(Self);
  FBCBankNotify.BlockManager := FBank;
  FBCBankNotify.OnNewBlock := OnBankNewBlock;
  FNetServer := TNetServer.Create;
  FOperations := TBlock.Create(Self);
  FOperations.Bank := FBank;
  FNotifyList := TList.Create;
{$IFDEF BufferOfFutureOperations}
  FBufferAuxWaitingOperations := TOperationsHashTree.Create;
{$ENDIF}
  if not Assigned(_Node) then
    _Node := Self;
end;

class procedure TNode.DecodeIpStringToNodeServerAddressArray(
  const Ips: AnsiString; var NodeServerAddressArray: TNodeServerAddressArray);
  function GetIp(var ips_string: AnsiString; var nsa: TNodeServerAddress): Boolean;
  const
    CT_IP_CHARS = ['a' .. 'z', 'A' .. 'Z', '0' .. '9', '.', '-', '_'];
  var
    i: Integer;
    port: AnsiString;
  begin
    nsa := CT_TNodeServerAddress_NUL;
    Result := false;
    if length(trim(ips_string)) = 0 then
    begin
      ips_string := '';
      exit;
    end;
    i := 1;
    while (i < length(ips_string)) and (not(ips_string[i] in CT_IP_CHARS)) do
      inc(i);
    if (i > 1) then
      ips_string := copy(ips_string, i, length(ips_string));
    //
    i := 1;
    while (i <= length(ips_string)) and (ips_string[i] in CT_IP_CHARS) do
      inc(i);
    nsa.ip := copy(ips_string, 1, i - 1);
    if (i <= length(ips_string)) and (ips_string[i] = ':') then
    begin
      inc(i);
      port := '';
      while (i <= length(ips_string)) and (ips_string[i] in ['0' .. '9']) do
      begin
        port := port + ips_string[i];
        inc(i);
      end;
      nsa.port := StrToIntDef(port, 0);
    end;
    ips_string := copy(ips_string, i + 1, length(ips_string));
    if nsa.port = 0 then
      nsa.port := CT_NetServer_Port;
    Result := (trim(nsa.ip) <> '');
  end;

var
  i, j: Integer;
  ips_string: AnsiString;
  nsa: TNodeServerAddress;
begin
  SetLength(NodeServerAddressArray, 0);
  ips_string := Ips;
  repeat
    if GetIp(ips_string, nsa) then
    begin
      SetLength(NodeServerAddressArray, length(NodeServerAddressArray) + 1);
      NodeServerAddressArray[high(NodeServerAddressArray)] := nsa;
    end;
  until (ips_string = '');
end;

destructor TNode.Destroy;
var
  step: string;
begin
  TLog.NewLog(ltinfo, Classname, 'TNode.Destroy START');
  try
    step := 'Deleting critical section';
    FreeAndNil(FLockNodeOperations);
    FreeAndNil(FOperationSequenceLock);

    step := 'Desactivating server';
    FNetServer.Active := false;

    step := 'Destroying NetServer';
    FreeAndNil(FNetServer);

    step := 'Destroying NotifyList';
    FreeAndNil(FNotifyList);
    step := 'Destroying Operations';
    FreeAndNil(FOperations);
    step := 'Assigning NIL to node var';
    if _Node = Self then
      _Node := nil;
    step := 'Destroying SentOperations list';
    FreeAndNil(FSentOperations);

    step := 'Destroying Bank';
    FreeAndNil(FBCBankNotify);
    FreeAndNil(FBank);
{$IFDEF BufferOfFutureOperations}
    FreeAndNil(FBufferAuxWaitingOperations);
{$ENDIF}
    step := 'inherited';
    FreeAndNil(FNodeLog);
    inherited;
  except
    on e: Exception do
    begin
      TLog.NewLog(lterror, Classname, 'Error destroying Node step: ' + step + ' Errors (' + e.Classname + '): ' + e.Message);
      raise;
    end;
  end;
  TLog.NewLog(ltinfo, Classname, 'TNode.Destroy END');
end;

procedure TNode.DisableNewBlocks;
begin
  inc(FDisabledsNewBlocksCount);
end;

procedure TNode.EnableNewBlocks;
begin
  if FDisabledsNewBlocksCount = 0 then
    raise Exception.Create('Dev error 20160924-1');
  dec(FDisabledsNewBlocksCount);
end;

class function TNode.EncodeNodeServerAddressArrayToIpString(
  const NodeServerAddressArray: TNodeServerAddressArray): AnsiString;
var
  i: Integer;
begin
  Result := '';
  for i := low(NodeServerAddressArray) to high(NodeServerAddressArray) do
  begin
    if (Result <> '') then
      Result := Result + ';';
    Result := Result + NodeServerAddressArray[i].ip;
    if NodeServerAddressArray[i].port > 0 then
    begin
      Result := Result + ':' + IntToStr(NodeServerAddressArray[i].port);
    end;
  end;
end;

function TNode.GetNodeLogFilename: AnsiString;
begin
  Result := FNodeLog.FileName;
end;

function TNode.IsBlockChainValid(var WhyNot: AnsiString): Boolean;
var
  unixtimediff: Integer;
begin
  Result := false;
  if (TNetData.NetData.NetStatistics.ActiveConnections <= 0) then
  begin
    WhyNot := 'No connection to check blockchain';
    exit;
  end;
  if (Bank.LastOperationBlock.block <= 0) then
  begin
    WhyNot := 'No blockchain';
    exit;
  end;
  unixtimediff := UnivDateTimeToUnix(DateTime2UnivDateTime(Now)) - Bank.LastOperationBlock.timestamp;
  if (unixtimediff < 0) then
  begin
    WhyNot := 'Invalid Last Block Time';
    exit;
  end;
  if (unixtimediff > (CT_NewLineSecondsAvg * 10)) then
  begin
    WhyNot := 'Last block has a long time ago... ' + IntToStr(unixtimediff);
    exit;
  end;
  Result := true;
end;

function TNode.IsReady(var CurrentProcess: AnsiString): Boolean;
begin
  Result := false;
  CurrentProcess := '';
  if FBank.IsReady(CurrentProcess) then
  begin
    if FNetServer.Active then
    begin
      if TNetData.NetData.IsGettingNewBlockChainFromClient then
      begin
        CurrentProcess := 'Obtaining valid BlockChain - Found block ' + IntToStr(TNetData.NetData.MaxRemoteOperationBlock.block);
      end
      else
      begin
        if TNetData.NetData.MaxRemoteOperationBlock.block > FOperations.OperationBlock.block then
        begin
          CurrentProcess := 'Found block ' + IntToStr(TNetData.NetData.MaxRemoteOperationBlock.block) + ' (Wait until downloaded)';
        end
        else
        begin
          Result := true;
        end;
      end;
    end
    else
    begin
      CurrentProcess := 'Server not active';
    end;
  end;
end;

function TNode.NetServer: TNetServer;
begin
  Result := FNetServer;
end;

class function TNode.Node: TNode;
begin
  if not Assigned(_Node) then
    _Node := TNode.Create(nil);
  Result := _Node;
end;

procedure TNode.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
end;

procedure TNode.NotifyBlocksChanged;
var
  i: Integer;
begin
  for i := 0 to FNotifyList.Count - 1 do
  begin
    TNodeNotifyEvents(FNotifyList[i]).NotifyBlocksChanged;
  end;
end;

procedure TNode.GetStoredOperationsFromAccount(const OperationsResume: TTransactionList; account_number: Cardinal; MaxDepth, StartOperation, EndOperation: Integer);
// Optimization:
// For better performance, will only include at "OperationsResume" values betweeen "startOperation" and "endOperation"
  procedure DoGetFromBlock(block_number: Integer; last_balance: Int64; act_depth: Integer; nOpsCounter: Integer);
  var
    opc: TBlock;
    op: ITransaction;
    OPR: TTransactionData;
    l: TList;
    i: Integer;
    last_block_number, next_block_number: Integer;
  begin
    if (act_depth <= 0) then
      exit;
    opc := TBlock.Create(nil);
    try
      l := TList.Create;
      try
        last_block_number := block_number + 1;
        while (last_block_number > block_number) and (act_depth > 0)
          and (block_number >= (account_number div CT_AccountsPerBlock))
          and (nOpsCounter <= EndOperation) do
        begin
          last_block_number := block_number;
          next_block_number := block_number;
          l.Clear;
          if not Bank.Storage.LoadBlockChainBlock(opc, block_number) then
          begin
            TLog.NewLog(ltdebug, Classname, 'Block ' + IntToStr(block_number) + ' not found. Cannot read operations');
            exit;
          end;
          opc.OperationsHashTree.GetTransactionsAffectingAccount(account_number, l);
          for i := l.Count - 1 downto 0 do
          begin
            op := opc.Operation[PtrInt(l.Items[i])];
            if (i = 0) then
            begin
              if op.SignerAccount = account_number then
                next_block_number := op.Previous_Signer_Updated_Block
              else if (op.DestinationAccount = account_number) then
                next_block_number := op.Previous_Destination_updated_block
              else if (op.SellerAccount = account_number) then
                next_block_number := op.Previous_Seller_updated_block;
            end;
            if op.GetTransactionData(block_number, account_number, OPR) then
            begin
              OPR.NOpInsideBlock := op.tag; // Note: Used Op.tag to include operation index inside a list
              OPR.time := opc.OperationBlock.timestamp;
              OPR.block := block_number;
              OPR.balance := last_balance;
              last_balance := last_balance - (OPR.Amount + OPR.Fee);
              if (nOpsCounter >= StartOperation) and (nOpsCounter <= EndOperation) then
              begin
                OperationsResume.Add(OPR);
              end;
              inc(nOpsCounter);
            end;
          end;
          // Is a new block operation?
          if (TAccount.AccountBlock(account_number) = block_number) and ((account_number mod CT_AccountsPerBlock) = 0) then
          begin
            OPR := TTransactionData.Empty;
            OPR.valid := true;
            OPR.block := block_number;
            OPR.time := opc.OperationBlock.timestamp;
            OPR.AffectedAccount := account_number;
            OPR.Amount := opc.OperationBlock.reward;
            OPR.Fee := opc.OperationBlock.Fee;
            OPR.balance := last_balance;
            OPR.OperationTxt := rsBlockchainRe;
            if (nOpsCounter >= StartOperation) and (nOpsCounter <= EndOperation) then
            begin
              OperationsResume.Add(OPR);
            end;
            inc(nOpsCounter);
          end;
          //
          opc.Clear(true);
          dec(act_depth);
          block_number := next_block_number;
        end;
      finally
        l.Free;
      end;
    finally
      opc.Free;
    end;
  end;

var
  acc: TAccount;
begin
  if MaxDepth < 0 then
    exit;
  if account_number >= Bank.AccountStorage.AccountsCount then
    exit;
  if StartOperation > EndOperation then
    exit;
  acc := Bank.AccountStorage.Account(account_number);
  if (acc.updated_block > 0) or (acc.Account = 0) then
    DoGetFromBlock(acc.updated_block, acc.balance, MaxDepth, 0);
end;

function TNode.FindOperation(const OperationComp: TBlock;
  const OperationHash: TRawBytes; var block: Cardinal;
  var operation_block_index: Integer): Boolean;
{ With a OperationHash, search it }
var
  Account, n_operation: Cardinal;
  i: Integer;
  op: ITransaction;
  initial_block, aux_block: Cardinal;
  opHashValid, opHash_OLD: TRawBytes;
begin
  Result := false;
  // Decode OperationHash
  if not TTransaction.DecodeOperationHash(OperationHash, block, Account, n_operation) then
    exit;
  initial_block := block;
  //
  if (Account >= Bank.AccountsCount) then
    exit; // Invalid account number
  // If block=0 then we must search in pending operations first
  if (block = 0) then
  begin
    FOperations.Lock;
    try
      for i := 0 to FOperations.Count - 1 do
      begin
        op := FOperations.Operation[i];
        if (op.SignerAccount = Account) then
        begin
          opHashValid := op.TransactionHash(0);
          opHash_OLD := op.TransactionHash_OLD(0);
          if (opHashValid = OperationHash) or
            ((FBank.BlocksCount < CT_Protocol_Upgrade_v2_MinBlock) and (opHash_OLD = OperationHash)) then
          begin
            operation_block_index := i;
            OperationComp.CopyFrom(FOperations);
            Result := true;
            exit;
          end;
        end;
      end;
    finally
      FOperations.Unlock;
    end;
    // block=0 and not found... start searching at block updated by account updated_block
    block := Bank.AccountStorage.Account(Account).updated_block;
    if Bank.AccountStorage.Account(Account).n_operation < n_operation then
      exit; // n_operation is greater than found in safebox
  end;
  if (block = 0) or (block >= Bank.BlocksCount) then
    exit;
  // Search in previous blocks
  while (not Result) and (block > 0) do
  begin
    aux_block := block;
    if not Bank.LoadOperations(OperationComp, block) then
      exit;
    for i := OperationComp.Count - 1 downto 0 do
    begin
      op := OperationComp.Operation[i];
      if (op.SignerAccount = Account) then
      begin
        if (op.NumberOfTransactions < n_operation) then
          exit; // n_operation is greaten than found
        if (op.NumberOfTransactions = n_operation) then
        begin
          // Possible candidate or dead
          opHashValid := op.TransactionHash(initial_block);
          if (opHashValid = OperationHash) then
          begin
            operation_block_index := i;
            Result := true;
            exit;
          end
          else if (block < CT_Protocol_Upgrade_v2_MinBlock) then
          begin
            opHash_OLD := op.TransactionHash_OLD(initial_block);
            if (opHash_OLD = OperationHash) then
            begin
              operation_block_index := i;
              Result := true;
              exit;
            end
            else
              exit; // Not found!
          end
          else
            exit; // Not found!
        end;
        if op.Previous_Signer_Updated_Block > block then
          exit;
        block := op.Previous_Signer_Updated_Block;
      end
      else if op.DestinationAccount = Account then
      begin
        if op.Previous_Destination_updated_block > block then
          exit;
        block := op.Previous_Destination_updated_block;
      end
      else if op.SellerAccount = Account then
      begin
        if op.Previous_Seller_updated_block > block then
          exit;
        block := op.Previous_Seller_updated_block;
      end;
    end;
    if (block >= aux_block) then
      exit; // Error... not found a valid block positioning
    if (initial_block <> 0) then
      exit; // If not found in specified block, no valid hash
  end;
end;

procedure TNode.NotifyNetClientMessage(Sender: TNetConnection; const TheMessage: AnsiString);
var
  i: Integer;
  s: AnsiString;
begin
  for i := 0 to FNotifyList.Count - 1 do
  begin
    if Assigned(TNodeNotifyEvents(FNotifyList[i]).OnNodeMessageEvent) then
    begin
      TNodeNotifyEvents(FNotifyList[i]).FMessages.AddObject(TheMessage, Sender);
    end;
  end;
end;

procedure TNode.OnBankNewBlock(Sender: TObject);
begin
  FOperations.SanitizeOperations;
end;

function TNode.SendNodeMessage(Target: TNetConnection; TheMessage: AnsiString; var errors: AnsiString): Boolean;
var
  i, j: Integer;
  nc: TNetConnection;
  s: string;
begin
  Result := false;
  if not TPCThread.TryProtectEnterCriticalSection(Self, 4000, FLockNodeOperations) then
  begin
    s := 'Cannot Send node message due blocking lock operations node';
    TLog.NewLog(lterror, Classname, s);
    if TThread.CurrentThread.ThreadID = MainThreadID then
      raise Exception.Create(s)
    else
      exit;
  end;
  try
    errors := '';
    if Assigned(Target) then
    begin
      Target.Send_Message(TheMessage);
    end
    else
    begin
      j := TNetData.NetData.ConnectionsCountAll;
      for i := 0 to j - 1 do
      begin
        if TNetData.NetData.GetConnection(i, nc) then
        begin
          if TNetData.NetData.ConnectionLock(Self, nc, 500) then
          begin
            try
              nc.Send_Message(TheMessage);
            finally
              TNetData.NetData.ConnectionUnlock(nc)
            end;
          end;
        end;
      end;
    end;
    Result := true;
  finally
    FLockNodeOperations.Release;
  end;
end;

procedure TNode.SetNodeLogFilename(const Value: AnsiString);
begin
  FNodeLog.FileName := Value;
end;

{ TNodeNotifyEvents }

constructor TNodeNotifyEvents.Create(AOwner: TComponent);
begin
  inherited;
  FOnOperationsChanged := nil;
  FOnBlocksChanged := nil;
  FOnNodeMessageEvent := nil;
  FMessages := TStringList.Create;
  FPendingNotificationsList := TPCThreadList.Create('TNodeNotifyEvents_PendingNotificationsList');
  FThreadSafeNodeNotifyEvent := TThreadSafeNodeNotifyEvent.Create(Self);
  FThreadSafeNodeNotifyEvent.FreeOnTerminate := true; // This is to prevent locking when freeing component
  Node := _Node;
end;

destructor TNodeNotifyEvents.Destroy;
begin
  if Assigned(FNode) then
    FNode.FNotifyList.Remove(Self);
  FThreadSafeNodeNotifyEvent.FNodeNotifyEvents := nil;
  FThreadSafeNodeNotifyEvent.Terminate;
  FreeAndNil(FPendingNotificationsList);
  FreeAndNil(FMessages);
  inherited;
end;

procedure TNodeNotifyEvents.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opremove) then
  begin
    if AComponent = FNode then
      FNode := nil;
  end;
end;

procedure TNodeNotifyEvents.NotifyBlocksChanged;
begin
  if Assigned(FThreadSafeNodeNotifyEvent) then
    FThreadSafeNodeNotifyEvent.FNotifyBlocksChanged := true;
end;

procedure TNodeNotifyEvents.NotifyOperationsChanged;
begin
  if Assigned(FThreadSafeNodeNotifyEvent) then
    FThreadSafeNodeNotifyEvent.FNotifyOperationsChanged := true;
end;

procedure TNodeNotifyEvents.SetNode(const Value: TNode);
begin
  if FNode = Value then
    exit;
  if Assigned(FNode) then
  begin
    FNode.RemoveFreeNotification(Self);
    FNode.FNotifyList.Remove(Self);
  end;
  FNode := Value;
  if Assigned(FNode) then
  begin
    FNode.FreeNotification(Self);
    FNode.FNotifyList.Add(Self);
  end;
end;

{ TThreadSafeNodeNotifyEvent }

procedure TThreadSafeNodeNotifyEvent.BCExecute;
begin
  while (not Terminated) and (Assigned(FNodeNotifyEvents)) do
  begin
    if (FNotifyOperationsChanged) or (FNotifyBlocksChanged) or (FNodeNotifyEvents.FMessages.Count > 0) then
      Synchronize(SynchronizedProcess);
    Sleep(100);
  end;
end;

constructor TThreadSafeNodeNotifyEvent.Create(ANodeNotifyEvents: TNodeNotifyEvents);
begin
  FNodeNotifyEvents := ANodeNotifyEvents;
  inherited Create(false);
end;

procedure TThreadSafeNodeNotifyEvent.SynchronizedProcess;
var
  i: Integer;
begin
  try
    if (Terminated) or (not Assigned(FNodeNotifyEvents)) then
      exit;
    if FNotifyBlocksChanged then
    begin
      FNotifyBlocksChanged := false;
      DebugStep := 'Notify OnBlocksChanged';
      if Assigned(FNodeNotifyEvents) and (Assigned(FNodeNotifyEvents.FOnBlocksChanged)) then
        FNodeNotifyEvents.FOnBlocksChanged(FNodeNotifyEvents);
    end;
    if FNotifyOperationsChanged then
    begin
      FNotifyOperationsChanged := false;
      DebugStep := 'Notify OnOperationsChanged';
      if Assigned(FNodeNotifyEvents) and (Assigned(FNodeNotifyEvents.FOnOperationsChanged)) then
        FNodeNotifyEvents.FOnOperationsChanged(FNodeNotifyEvents);
    end;
    if FNodeNotifyEvents.FMessages.Count > 0 then
    begin
      DebugStep := 'Notify OnNodeMessageEvent';
      if Assigned(FNodeNotifyEvents) and (Assigned(FNodeNotifyEvents.FOnNodeMessageEvent)) then
      begin
        for i := 0 to FNodeNotifyEvents.FMessages.Count - 1 do
        begin
          DebugStep := 'Notify OnNodeMessageEvent ' + IntToStr(i + 1) + '/' + IntToStr(FNodeNotifyEvents.FMessages.Count);
          FNodeNotifyEvents.FOnNodeMessageEvent(TNetConnection(FNodeNotifyEvents.FMessages.Objects[i]), FNodeNotifyEvents.FMessages.Strings[i]);
        end;
      end;
      FNodeNotifyEvents.FMessages.Clear;
    end;
  except
    on e: Exception do
    begin
      TLog.NewLog(lterror, Classname, 'Exception inside a Synchronized process: ' + e.Classname + ':' + e.Message + ' Step:' + DebugStep);
    end;
  end;
end;

{ TThreadNodeNotifyNewBlock }

procedure TThreadNodeNotifyNewBlock.BCExecute;
begin
  DebugStep := 'Locking';
  if TNetData.NetData.ConnectionLock(Self, FNetConnection, 500) then
  begin
    try
      DebugStep := 'Checking connected';
      if not FNetConnection.Connected then
        exit;
      TLog.NewLog(ltdebug, Classname, 'Sending new block found to ' + FNetConnection.Client.ClientRemoteAddr);
      DebugStep := 'Sending';
      FNetConnection.Send_NewBlockFound(FNewBlockOperations);
      DebugStep := 'Checking connected again';
      if not FNetConnection.Connected then
        exit;
      DebugStep := 'Need send opreations?';
      if FSanitizedOperationsHashTree.OperationsCount > 0 then
      begin
        DebugStep := 'Sending ' + IntToStr(FSanitizedOperationsHashTree.OperationsCount) + ' sanitized operations';
        TLog.NewLog(ltdebug, Classname, 'Sending ' + IntToStr(FSanitizedOperationsHashTree.OperationsCount) + ' sanitized operations to ' + FNetConnection.ClientRemoteAddr);
        TThreadNodeNotifyOperations.Create(FNetConnection, FSanitizedOperationsHashTree);
      end;
      DebugStep := 'Unlocking';
    finally
      TNetData.NetData.ConnectionUnlock(FNetConnection);
    end;
  end;
  DebugStep := 'Finalizing';
end;

constructor TThreadNodeNotifyNewBlock.Create(NetConnection: TNetConnection; MakeACopyOfNewBlockOperations: TBlock; MakeACopyOfSanitizedOperationsHashTree: TTransactionHashTree);
begin
  FNetConnection := NetConnection;
  FSanitizedOperationsHashTree := TTransactionHashTree.Create;
  FSanitizedOperationsHashTree.CopyFromHashTree(MakeACopyOfSanitizedOperationsHashTree);
  FNewBlockOperations := TBlock.Create(nil);
  FNewBlockOperations.CopyFrom(MakeACopyOfNewBlockOperations);
  inherited Create(false);
  FreeOnTerminate := true;
end;

destructor TThreadNodeNotifyNewBlock.Destroy;
begin
  FreeAndNil(FSanitizedOperationsHashTree);
  FreeAndNil(FNewBlockOperations);
  inherited;
end;

{ TThreadNodeNotifyOperations }

procedure TThreadNodeNotifyOperations.BCExecute;
begin
  Sleep(Random(5000)); // Delay 0..5 seconds to allow receive data and don't send if not necessary
  if TNetData.NetData.ConnectionLock(Self, FNetConnection, 500) then
  begin
    try
      if not FNetConnection.Connected then
        exit;
      FNetConnection.Send_AddOperations(nil);
    finally
      TNetData.NetData.ConnectionUnlock(FNetConnection);
    end;
  end;
end;

constructor TThreadNodeNotifyOperations.Create(NetConnection: TNetConnection; MakeACopyOfOperationsHashTree: TTransactionHashTree);
begin
  FNetConnection := NetConnection;
  FNetConnection.AddOperationsToBufferForSend(MakeACopyOfOperationsHashTree);
  inherited Create(false);
  FreeOnTerminate := true;
end;

destructor TThreadNodeNotifyOperations.Destroy;
begin
  inherited;
end;

initialization

_Node := nil;

finalization

FreeAndNil(_Node);

end.
