unit MicroCoin.Node.Node;
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

uses Classes, MicroCoin.BlockChain.BlockManager, UCrypto,
  UThread, SyncObjs, ULog, MicroCoin.Transaction.Base,
  MicroCoin.Common.Lists, MicroCoin.Account.Data,
  MicroCoin.Transaction.Itransaction, MicroCoin.Keys.KeyManager,
  MicroCoin.Transaction.Transaction, MicroCoin.BlockChain.Events,
  MicroCoin.Net.Connection, MicroCoin.Net.ConnectionManager, UBaseTypes,
  MicroCoin.Transaction.Events, MicroCoin.Net.Server, MicroCoin.Net.NodeServer,
  MicroCoin.BlockChain.Block, MicroCoin.Transaction.TransactionList,
  MicroCoin.Transaction.HashTree, MicroCoin.Account.Storage,
  MicroCoin.BlockChain.BlockHeader, Sysutils, MicroCoin.Common.Config, UTime;

type

  TNode = class(TComponent)
  strict private
    FNodeLog: TLog;
    FLockNodeOperations: TPCCriticalSection;
    FOperationSequenceLock: TPCCriticalSection;
    FNotifyList: TList;
    FBlockManager: TBlockManager;
    FTransactionStorage: TBlock;
    FNetServer: TNetServer;
    FBlockManagerNotify: TBlockManagerNotify;
    FPeerCache: AnsiString;
    FDisabledsNewBlocksCount: Integer;
    FSentOperations: TOrderedRawList;
    FKeyManager: TKeyManager;
{$IFDEF BufferOfFutureOperations}
    FBufferAuxWaitingOperations: TOperationsHashTree;
{$ENDIF}
    class var _Node: TNode;
    class var CriticalSection : TCriticalSection;
    procedure OnNewBlock(Sender: TObject);
    procedure SetNodeLogFilename(const Value: TFilename);
    function GetNodeLogFilename: TFilename;
    procedure SetKeyManager(const Value: TKeyManager);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    class function Node: TNode;
    class procedure FreeNode; static;
    class procedure DecodeIpStringToNodeServerAddressArray(const Ips: AnsiString;
      var NodeServerAddressArray: TNodeServerAddressArray);
    class function EncodeNodeServerAddressArrayToIpString(const NodeServerAddressArray: TNodeServerAddressArray)
      : AnsiString;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class constructor CreateClass;
    class destructor DestroyClass;
    class function HasInstance : boolean;

    function NetServer: TNetServer;
    procedure NotifyNetClientMessage(Sender: TNetConnection; const TheMessage: AnsiString);
    //
    property TransactionStorage: TBlock read FTransactionStorage;
    //
    function AddNewBlockChain(SenderConnection: TNetConnection; NewBlockOperations: TBlock;
      var newBlockAccount: TAccountStorageEntry; var errors: AnsiString): Boolean;
    function AddOperations(SenderConnection: TNetConnection; Operations: TTransactionHashTree;
      OperationsResult: TTransactionList; var errors: AnsiString): Integer;
    function AddTransaction(SenderConnection: TNetConnection; Operation: ITransaction; var errors: AnsiString): Boolean;
    function SendNodeMessage(Target: TNetConnection; TheMessage: AnsiString; var errors: AnsiString): Boolean;
    //
    procedure NotifyBlocksChanged;
    //
    procedure GetStoredTransactionsFromAccount(const ATransactionList: TTransactionList; AAccountNumber: Cardinal;
      AMaxDepth, AStartTransaction, AEndTransaction: Integer);
    function FindTransaction(const ABlock: TBlock; const AHash: TRawBytes; var RBlock: Cardinal;
      var ABlockIndex: Integer): Boolean;
    //
    procedure AutoDiscoverNodes(const Ips: AnsiString);
    function IsBlockChainValid(var WhyNot: AnsiString): Boolean;
    function IsReady(var CurrentProcess: AnsiString): Boolean;

    procedure DisableNewBlocks;
    procedure EnableNewBlocks;

    property PeerCache: AnsiString read FPeerCache write FPeerCache;
    property SequenceLock: TPCCriticalSection read FOperationSequenceLock;
    property NotifyList: TList read FNotifyList;
    property BlockManager: TBlockManager read FBlockManager;
  published
    property NodeLogFilename: TFilename read GetNodeLogFilename write SetNodeLogFilename;
    property KeyManager : TKeyManager read FKeyManager write SetKeyManager;
  end;



implementation

uses MicroCoin.Node.Events;


resourcestring
  rsAccountSZero = 'Account %s zero fee operations per block limit:%d';
  rsBlockchainRe = 'Blockchain reward';

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
    TLog.NewLog(lterror, Classname,
      Format('Cannot Add new BlockChain due is adding disabled - Connection:%s NewBlock:%s',
      [Inttohex(PtrInt(SenderConnection), 8), NewBlockOperations.BlockHeader.ToString()]));
    errors := 'Adding blocks is disabled';
    exit;
  end;
  if NewBlockOperations.BlockHeader.Block <> BlockManager.BlocksCount then
  begin
    errors := 'New block number (' + IntToStr(NewBlockOperations.BlockHeader.Block) + ') not valid! (Expected ' +
      IntToStr(BlockManager.BlocksCount) + ')';
    exit;
  end;
  OpBlock := NewBlockOperations.BlockHeader;
  LogDebug( Classname, Format('AddNewBlockChain Connection:%s NewBlock:%s',
    [Inttohex(PtrInt(SenderConnection), 8), OpBlock.ToString()]));
  if not TPCThread.TryProtectEnterCriticalSection(Self, 2000, FLockNodeOperations) then
  begin
    if NewBlockOperations.BlockHeader.Block <> BlockManager.BlocksCount then
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
    if TBlock.Equals(BlockManager.LastBlock, NewBlockOperations.BlockHeader) then
    begin
      errors := 'Duplicated block';
      exit;
    end;
    ms := TMemoryStream.Create;
    try
      FTransactionStorage.SaveBlockToStream(false, ms);
      Result := BlockManager.AddNewBlockToBlockChain(NewBlockOperations,
        TConnectionManager.Instance.NetworkAdjustedTime.GetMaxAllowedTimestampForNewBlock, newBlockAccount, errors);
      if Result then
      begin
        if Assigned(SenderConnection) then
        begin
          FNodeLog.NotifyNewLog(ltupdate, SenderConnection.Classname,
            Format(';%d;%s;%s;;%d;%d;%d;%s', [OpBlock.Block, SenderConnection.ClientRemoteAddr, OpBlock.block_payload,
            OpBlock.timestamp, UnivDateTimeToUnix(DateTime2UnivDateTime(Now)),
            UnivDateTimeToUnix(DateTime2UnivDateTime(Now)) - OpBlock.timestamp, Inttohex(OpBlock.compact_target, 8)]));
        end
        else
        begin
          FNodeLog.NotifyNewLog(ltupdate, Classname, Format(';%d;%s;%s;;%d;%d;%d;%s',
            [OpBlock.Block, 'NIL', OpBlock.block_payload, OpBlock.timestamp,
            UnivDateTimeToUnix(DateTime2UnivDateTime(Now)), UnivDateTimeToUnix(DateTime2UnivDateTime(Now)) -
            OpBlock.timestamp, Inttohex(OpBlock.compact_target, 8)]));
        end;
      end
      else
      begin
        if Assigned(SenderConnection) then
        begin
          FNodeLog.NotifyNewLog(lterror, SenderConnection.Classname,
            Format(';%d;%s;%s;%s;%d;%d;%d;%s', [OpBlock.Block, SenderConnection.ClientRemoteAddr, OpBlock.block_payload,
            errors, OpBlock.timestamp, UnivDateTimeToUnix(DateTime2UnivDateTime(Now)),
            UnivDateTimeToUnix(DateTime2UnivDateTime(Now)) - OpBlock.timestamp, Inttohex(OpBlock.compact_target, 8)]));
        end
        else
        begin
          FNodeLog.NotifyNewLog(lterror, Classname, Format(';%d;%s;%s;%s;%d;%d;%d;%s',
            [OpBlock.Block, 'NIL', OpBlock.block_payload, errors, OpBlock.timestamp,
            UnivDateTimeToUnix(DateTime2UnivDateTime(Now)), UnivDateTimeToUnix(DateTime2UnivDateTime(Now)) -
            OpBlock.timestamp, Inttohex(OpBlock.compact_target, 8)]));
        end;
      end;
      FTransactionStorage.Clear(true);
      ms.Position := 0;
      if not FTransactionStorage.LoadBlockFromStream(ms, errors2) then
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
    FTransactionStorage.Sanitize;
    if Result then
    begin
      opsht := TTransactionHashTree.Create;
      try
        for i := 0 to FTransactionStorage.TransactionCount - 1 do
        begin
          opsht.AddTransactionToHashTree(FTransactionStorage.Transaction[i]);
          // Add to sent operations
          FSentOperations.Add(FTransactionStorage.Transaction[i].Sha256, BlockManager.LastBlockFound.BlockHeader.Block);
        end;
        if opsht.TransactionCount > 0 then
        begin
          TLog.NewLog(ltinfo, Classname, 'Resending ' + IntToStr(opsht.TransactionCount) + ' operations for new block');
          for i := 0 to opsht.TransactionCount - 1 do
          begin
            TLog.NewLog(ltinfo, Classname, 'Resending (' + IntToStr(i + 1) + '/' + IntToStr(opsht.TransactionCount) +
              '): ' + opsht.GetTransaction(i).ToString);
          end;
        end;
        // Clean sent operations buffer
        j := 0;
        for i := FSentOperations.Count - 1 downto 0 do
        begin
          if (FSentOperations.GetTag(i) < BlockManager.LastBlockFound.BlockHeader.Block - 2) then
          begin
            FSentOperations.Delete(i);
            inc(j);
          end;
        end;
        if j > 0 then
        begin
          LogDebug( Classname, 'Buffer Sent operations: Deleted ' + IntToStr(j) + ' old operations');
        end;
        LogDebug( Classname, 'Buffer Sent operations: ' + IntToStr(FSentOperations.Count));
        // Notify to clients
        j := TConnectionManager.Instance.ConnectionsCountAll;
        for i := 0 to j - 1 do
        begin
          if (TConnectionManager.Instance.GetConnection(i, nc)) then
          begin
            if (nc <> SenderConnection) and (nc.Connected) then
            begin
              TNotifyNewBlockThread.Create(nc, BlockManager.LastBlockFound, opsht);
            end;
          end;
        end;
      finally
        opsht.Free;
      end;
    end;
  finally
    FLockNodeOperations.Release;
    LogDebug( Classname, Format('Finalizing AddNewBlockChain Connection:%s NewBlock:%s',
      [Inttohex(PtrInt(SenderConnection), 8), OpBlock.ToString()]));
  end;
  if Result then
  begin
    // Notify it!
    NotifyBlocksChanged;
  end;
end;

function TNode.AddTransaction(SenderConnection: TNetConnection; Operation: ITransaction; var errors: AnsiString): Boolean;
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

function TNode.AddOperations(SenderConnection: TNetConnection; Operations: TTransactionHashTree;
  OperationsResult: TTransactionList; var errors: AnsiString): Integer;
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
          TLog.NewLog(ltinfo, Classname, Format('AddOperation FromBufferWaitingOperations %d/%d: %s',
            [i + 1, FBufferAuxWaitingOperations.OperationsCount, ActOp.ToString]));
          inc(nAdded);
          valids_operations.AddOperationToHashTree(ActOp);
          FBufferAuxWaitingOperations.Delete(i);
        end
        else
        begin
          sAcc := FOperations.SafeBoxTransaction.Account(ActOp.SignerAccount);
          if (sAcc.n_operation > ActOp.n_operation) or ((sAcc.n_operation = ActOp.n_operation) and (sAcc.balance > 0))
          then
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
      TLog.NewLog(ltinfo, Classname, Format('FromBufferWaitingOperations status - Added:%d Deleted:%d Buffer:%d',
        [nAdded, nDeleted, FBufferAuxWaitingOperations.OperationsCount]));
    end;
  end;
{$ENDIF}

var
  i, j: Integer;
  xBlock: TBlock;
  xValidTransactions: TTransactionHashTree;
  xNetConnection: TNetConnection;
  xError: AnsiString;
  xList: TList;
  s: string;
  xTransactionData: TTransactionData;
  xTransaction: ITransaction;
  xAccount: TAccount;
begin
  Result := -1;
  if Assigned(OperationsResult) then
    OperationsResult.Clear;
  if FDisabledsNewBlocksCount > 0 then
  begin
    errors := Format('Cannot Add Operations due is adding disabled - OpCount:%d', [Operations.TransactionCount]);
    TLog.NewLog(ltinfo, Classname, errors);
    exit;
  end;
  Result := 0;
  errors := '';
  xValidTransactions := TTransactionHashTree.Create;
  try
    LogDebug(Classname, Format('AddOperations Connection:%s Operations:%d',
      [Inttohex(PtrInt(SenderConnection), 8), Operations.TransactionCount]));
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
      for j := 0 to Operations.TransactionCount - 1 do
      begin
        xTransaction := Operations.GetTransaction(j);
        if (FTransactionStorage.TransactionHashTree.IndexOf(xTransaction) < 0) and (FSentOperations.GetTag(xTransaction.Sha256) = 0) then
        begin
          // Protocol 2 limitation: In order to prevent spam of operations without Fee, will protect it
          if (xTransaction.Fee = 0) and (BlockManager.AccountStorage.CurrentProtocol >= cPROTOCOL_2) and
            (FTransactionStorage.TransactionHashTree.TransactionCountsWithoutFeeBySameSigner(xTransaction.SignerAccount) >=
            cMaxAccountOperationsPerBlockWithoutFee) then
          begin
            xError := Format(rsAccountSZero, [TAccount.AccountNumberToString(xTransaction.SignerAccount),
              cMaxAccountOperationsPerBlockWithoutFee]);
            if (errors <> '') then
              errors := errors + ' ';
            errors := errors + 'Op ' + IntToStr(j + 1) + '/' + IntToStr(Operations.TransactionCount) + ':' + xError;
            LogDebug(Classname, Format('AddOperation invalid/duplicated %d/%d: %s  - Error:%s',
              [(j + 1), Operations.TransactionCount, xTransaction.ToString, xError]));
            if Assigned(OperationsResult) then
            begin
              xTransaction.GetTransactionData(0, xTransaction.SignerAccount, xTransactionData);
              xTransactionData.valid := false;
              xTransactionData.NOpInsideBlock := -1;
              xTransactionData.OperationHash := '';
              xTransactionData.errors := xError;
              OperationsResult.Add(xTransactionData);
            end;
          end
          else
          begin
            // Buffer to prevent cyclic sending new on 1.5.4
            FSentOperations.Add(xTransaction.Sha256, FTransactionStorage.BlockHeader.Block);
            if (FTransactionStorage.AddTransaction(true, xTransaction, xError)) then
            begin
              inc(Result);
              xValidTransactions.AddTransactionToHashTree(xTransaction);
              LogDebug(Classname, Format('AddOperation %d/%d: %s', [(j + 1), Operations.TransactionCount,
                xTransaction.ToString]));
              if Assigned(OperationsResult) then
              begin
                xTransaction.GetTransactionData(0, xTransaction.SignerAccount, xTransactionData);
                xTransactionData.NOpInsideBlock := FTransactionStorage.TransactionCount - 1;
                xTransactionData.balance := FTransactionStorage.AccountTransaction.Account(xTransaction.SignerAccount).Balance;
                OperationsResult.Add(xTransactionData);
              end;
            end
            else
            begin
              if (errors <> '') then
                errors := errors + ' ';
              errors := errors + 'Op ' + IntToStr(j + 1) + '/' + IntToStr(Operations.TransactionCount) + ':' + xError;
              LogDebug( Classname, Format('AddOperation invalid/duplicated %d/%d: %s  - Error:%s',
                [(j + 1), Operations.TransactionCount, xTransaction.ToString, xError]));
              if Assigned(OperationsResult) then
              begin
                xTransaction.GetTransactionData(0, xTransaction.SignerAccount, xTransactionData);
                xTransactionData.valid := false;
                xTransactionData.NOpInsideBlock := -1;
                xTransactionData.OperationHash := '';
                xTransactionData.errors := xError;
                OperationsResult.Add(xTransactionData);
              end;
{$IFDEF BufferOfFutureOperations}
              // Used to solve 2.0.0 "invalid order of operations" bug
              if (Assigned(SenderConnection)) then
              begin
                sAcc := FOperations.SafeBoxTransaction.Account(ActOp.SignerAccount);
                if (sAcc.n_operation < ActOp.n_operation) or
                  ((sAcc.n_operation = ActOp.n_operation) and (sAcc.balance = 0) and (ActOp.OperationFee > 0) and
                  (ActOp.OpType = CT_Op_Changekey)) then
                begin
                  if FBufferAuxWaitingOperations.IndexOfOperation(ActOp) < 0 then
                  begin
                    FBufferAuxWaitingOperations.AddOperationToHashTree(ActOp);
                    TLog.NewLog(ltinfo, Classname,
                      Format('New FromBufferWaitingOperations %d/%d (new buffer size:%d): %s',
                      [j + 1, Operations.OperationsCount, FBufferAuxWaitingOperations.OperationsCount,
                      ActOp.ToString]));
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
{$IFDEF HIGHLOG}LogDebug( Classname, Format('AddOperation made before %d/%d: %s', [(j + 1), Operations.OperationsCount, ActOp.ToString])); {$ENDIF}
        end;
      end;
    finally
      FLockNodeOperations.Release;
      if Result <> 0 then
      begin
        LogDebug(Classname, Format('Finalizing AddOperations Connection:%s Operations:%d valids:%d',
          [Inttohex(PtrInt(SenderConnection), 8), Operations.TransactionCount, Result]));
      end;
    end;
    if Result = 0 then
      exit;
    // Send to other nodes
    j := TConnectionManager.Instance.ConnectionsCountAll;
    for i := 0 to j - 1 do
    begin
      if TConnectionManager.Instance.GetConnection(i, xNetConnection) then
      begin
        if (xNetConnection <> SenderConnection) and (xNetConnection.Connected) then
          TNotifyTransactionThread.Create(xNetConnection, xValidTransactions);
      end;
    end;
  finally
    xValidTransactions.Free;
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
    TConnectionManager.Instance.AddServer(nsarr[i]);
  end;
  j := (cMaximumNumberOfServers - TConnectionManager.Instance.ConnectionsCount(true));
  if j <= 0 then
    exit;
  TConnectionManager.Instance.DiscoverServers;
end;

class constructor TNode.CreateClass;
begin
  CriticalSection := TCriticalSection.Create;
end;

constructor TNode.Create(AOwner: TComponent);
begin
  inherited;
  if csDesigning in ComponentState then exit;
  FSentOperations := TOrderedRawList.Create;
  FNodeLog := TLog.Create(Self);
  FNodeLog.ProcessGlobalLogs := false;
  // RegisterOperationsClass;
  if Assigned(_Node) then
    raise Exception.Create('Duplicate nodes protection');
  TLog.NewLog(ltinfo, Classname, 'TNode.Create');
  FDisabledsNewBlocksCount := 0;
  FLockNodeOperations := TPCCriticalSection.Create('TNode_LockNodeOperations');
  FOperationSequenceLock := TPCCriticalSection.Create('TNode_OperationSequenceLock');
  FBlockManager := TBlockManager.Create(Self);
  FBlockManagerNotify := TBlockManagerNotify.Create(Self);
  FBlockManagerNotify.BlockManager := FBlockManager;
  FBlockManagerNotify.OnNewBlock := OnNewBlock;
  FNetServer := TNetServer.Create;
  FTransactionStorage := TBlock.Create(Self);
  FTransactionStorage.BlockManager := FBlockManager;
  FNotifyList := TList.Create;
{$IFDEF BufferOfFutureOperations}
  FBufferAuxWaitingOperations := TOperationsHashTree.Create;
{$ENDIF}
  if not Assigned(_Node) then
    _Node := Self;
end;

class procedure TNode.DecodeIpStringToNodeServerAddressArray(const Ips: AnsiString;
  var NodeServerAddressArray: TNodeServerAddressArray);

  function GetIp(var ips_string: AnsiString; var nsa: TNodeServer): Boolean;
  const
    CT_IP_CHARS = ['a' .. 'z', 'A' .. 'Z', '0' .. '9', '.', '-', '_'];
  var
    i: Integer;
    port: AnsiString;
  begin
    nsa := TNodeServer.Empty;
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
      nsa.port := cNetServerPort;
    Result := (trim(nsa.ip) <> '');
  end;

var
  i, j: Integer;
  ips_string: AnsiString;
  nsa: TNodeServer;
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

class destructor TNode.DestroyClass;
begin
  FreeAndNil(CriticalSection);
end;

destructor TNode.Destroy;
var
  step: string;
begin

  FSentOperations.Free;
  FNodeLog.Free;
  FLockNodeOperations.Free;
  FOperationSequenceLock.Free;
  FBlockManager.Free;
  FBlockManagerNotify.Free;
  FNetServer.Free;
  FTransactionStorage.BlockManager.Free;
  FTransactionStorage.Free;
  FNotifyList.Free;
  inherited;
  exit;

  TLog.NewLog(ltinfo, Classname, 'TNode.Destroy START');
  try
    step := 'Deleting critical section';
    FreeAndNil(FLockNodeOperations);
    FreeAndNil(FOperationSequenceLock);

    step := 'Desactivating server';

    step := 'Destroying NetServer';
    FreeAndNil(FNetServer);

    step := 'Destroying NotifyList';
    FreeAndNil(FNotifyList);
    step := 'Destroying Operations';
    FreeAndNil(FTransactionStorage);
    step := 'Assigning NIL to node var';
    if _Node = Self
    then _Node := nil;
    step := 'Destroying SentOperations list';
    FreeAndNil(FSentOperations);
    step := 'Destroying Bank';
    FreeAndNil(FBlockManagerNotify);
    FreeAndNil(FBlockManager);
{$IFDEF BufferOfFutureOperations}
    FreeAndNil(FBufferAuxWaitingOperations);
{$ENDIF}
    step := 'inherited';
    FreeAndNil(FNodeLog);
    FreeAndNil(FKeyManager);
    inherited;
  except
    on e: Exception do
    begin
      TLog.NewLog(lterror, Classname, 'Error destroying Node step: ' + step + ' Errors (' + e.Classname + '): ' +
        e.Message);
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

class function TNode.EncodeNodeServerAddressArrayToIpString(const NodeServerAddressArray: TNodeServerAddressArray)
  : AnsiString;
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

function TNode.GetNodeLogFilename: TFilename;
begin
  Result := FNodeLog.FileName;
end;

function TNode.IsBlockChainValid(var WhyNot: AnsiString): Boolean;
var
  unixtimediff: Integer;
begin
  Result := false;
  if (TConnectionManager.Instance.NetStatistics.ActiveConnections <= 0) then
  begin
    WhyNot := 'No connection to check blockchain';
    exit;
  end;
  if (BlockManager.LastBlock.Block <= 0) then
  begin
    WhyNot := 'No blockchain';
    exit;
  end;
  unixtimediff := UnivDateTimeToUnix(DateTime2UnivDateTime(Now)) - BlockManager.LastBlock.timestamp;
  if (unixtimediff < 0) then
  begin
    WhyNot := 'Invalid Last Block Time';
    exit;
  end;
  if (unixtimediff > (cBlockTime * 10)) then
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
  if FBlockManager.IsReady(CurrentProcess) then
  begin
    if FNetServer.Active then
    begin
      if TConnectionManager.Instance.IsGettingNewBlockChainFromClient then
      begin
        CurrentProcess := 'Obtaining valid BlockChain - Found block ' +
          IntToStr(TConnectionManager.Instance.MaxRemoteOperationBlock.Block);
      end
      else
      begin
        if TConnectionManager.Instance.MaxRemoteOperationBlock.Block > FTransactionStorage.BlockHeader.Block then
        begin
          CurrentProcess := 'Found block ' + IntToStr(TConnectionManager.Instance.MaxRemoteOperationBlock.Block) +
            ' (Wait until downloaded)';
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
  if not Assigned(CriticalSection)
  then exit;
  CriticalSection.Acquire;
  if not Assigned(_Node) then
    _Node := TNode.Create(nil);
  Result := _Node;
  CriticalSection.Release;
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

procedure TNode.GetStoredTransactionsFromAccount(const ATransactionList: TTransactionList; AAccountNumber: Cardinal;
  AMaxDepth, AStartTransaction, AEndTransaction: Integer);
// Optimization:
// For better performance, will only include at "OperationsResume" values betweeen "startOperation" and "endOperation"
  procedure DoGetFromBlock(block_number: Integer; last_balance: Int64; act_depth: Integer; nOpsCounter: Integer);
  var
    xBlock: TBlock;
    xTransaction: ITransaction;
    xTransactionData: TTransactionData;
    xList: TList;
    i: Integer;
    xLastBlockNumber, xNextBlockNumber: Integer;
  begin
    if (act_depth <= 0) then
      exit;
    xBlock := TBlock.Create(nil);
    try
      xList := TList.Create;
      try
        xLastBlockNumber := block_number + 1;
        while (xLastBlockNumber > block_number) and (act_depth > 0) and
          (block_number >= (AAccountNumber div cAccountsPerBlock)) and (nOpsCounter <= AEndTransaction) do
        begin
          xLastBlockNumber := block_number;
          xNextBlockNumber := block_number;
          xList.Clear;
          if not BlockManager.Storage.LoadBlockChainBlock(xBlock, block_number) then
          begin
            LogDebug( Classname, 'Block ' + IntToStr(block_number) + ' not found. Cannot read operations');
            exit;
          end;
          xBlock.TransactionHashTree.GetTransactionsAffectingAccount(AAccountNumber, xList);
          for i := xList.Count - 1 downto 0 do
          begin
            xTransaction := xBlock.Transaction[PtrInt(xList.Items[i])];
            if (i = 0) then
            begin
              if xTransaction.SignerAccount = AAccountNumber then
                xNextBlockNumber := xTransaction.Previous_Signer_Updated_Block
              else if (xTransaction.DestinationAccount = AAccountNumber) then
                xNextBlockNumber := xTransaction.Previous_Destination_updated_block
              else if (xTransaction.SellerAccount = AAccountNumber) then
                xNextBlockNumber := xTransaction.Previous_Seller_updated_block;
            end;
            if xTransaction.GetTransactionData(block_number, AAccountNumber, xTransactionData) then
            begin
              xTransactionData.NOpInsideBlock := xTransaction.tag; // Note: Used Op.tag to include operation index inside a list
              xTransactionData.time := xBlock.BlockHeader.timestamp;
              xTransactionData.Block := block_number;
              xTransactionData.balance := last_balance;
              last_balance := last_balance - (xTransactionData.Amount + xTransactionData.Fee);
              if (nOpsCounter >= AStartTransaction) and (nOpsCounter <= AEndTransaction) then
              begin
                ATransactionList.Add(xTransactionData);
              end;
              inc(nOpsCounter);
            end;
          end;
          // Is a new block operation?
          if (TAccount.AccountBlock(AAccountNumber) = block_number) and ((AAccountNumber mod cAccountsPerBlock) = 0)
          then
          begin
            xTransactionData := TTransactionData.Empty;
            xTransactionData.valid := true;
            xTransactionData.Block := block_number;
            xTransactionData.time := xBlock.BlockHeader.timestamp;
            xTransactionData.AffectedAccount := AAccountNumber;
            xTransactionData.Amount := xBlock.BlockHeader.reward;
            xTransactionData.Fee := xBlock.BlockHeader.Fee;
            xTransactionData.balance := last_balance;
            xTransactionData.TransactionAsString := rsBlockchainRe;
            if (nOpsCounter >= AStartTransaction) and (nOpsCounter <= AEndTransaction) then
            begin
              ATransactionList.Add(xTransactionData);
            end;
            inc(nOpsCounter);
          end;
          //
          xBlock.Clear(true);
          dec(act_depth);
          block_number := xNextBlockNumber;
        end;
      finally
        xList.Free;
      end;
    finally
      xBlock.Free;
    end;
  end;

var
  acc: TAccount;
begin
  if AMaxDepth < 0 then
    exit;
  if AAccountNumber >= BlockManager.AccountStorage.AccountsCount then
    exit;
  if AStartTransaction > AEndTransaction then
    exit;
  acc := BlockManager.AccountStorage.Accounts[AAccountNumber];
  if (acc.UpdatedBlock > 0) or (acc.AccountNumber = 0) then
    DoGetFromBlock(acc.UpdatedBlock, acc.Balance, AMaxDepth, 0);
end;

class function TNode.HasInstance: boolean;
begin
  Result := Assigned(_Node);
end;

function TNode.FindTransaction(const ABlock: TBlock; const AHash: TRawBytes; var RBlock: Cardinal;
  var ABlockIndex: Integer): Boolean;
{ With a OperationHash, search it }
var
  Account, n_operation: Cardinal;
  i: Integer;
  xTransaction: ITransaction;
  xInitialBlock, xAuxBlock: Cardinal;
  xHashValid, xOldHash: TRawBytes;
begin
  Result := false;
  // Decode OperationHash
  if not TTransaction.DecodeOperationHash(AHash, RBlock, Account, n_operation) then
    exit;
  xInitialBlock := RBlock;
  //
  if (Account >= BlockManager.AccountsCount) then
    exit; // Invalid account number
  // If block=0 then we must search in pending operations first
  if (RBlock = 0) then
  begin
    FTransactionStorage.Lock;
    try
      for i := 0 to FTransactionStorage.TransactionCount - 1 do
      begin
        xTransaction := FTransactionStorage.Transaction[i];
        if (xTransaction.SignerAccount = Account) then
        begin
          xHashValid := xTransaction.TransactionHash(0);
          xOldHash := xTransaction.TransactionHash_OLD(0);
          if (xHashValid = AHash) or ((FBlockManager.BlocksCount < cProtocol_Upgrade_v2_MinBlock) and
            (xOldHash = AHash)) then
          begin
            ABlockIndex := i;
            ABlock.CopyFrom(FTransactionStorage);
            Result := true;
            exit;
          end;
        end;
      end;
    finally
      FTransactionStorage.Unlock;
    end;
    // block=0 and not found... start searching at block updated by account updated_block
    RBlock := BlockManager.AccountStorage.Accounts[Account].UpdatedBlock;
    if BlockManager.AccountStorage.Accounts[Account].NumberOfTransactions < n_operation then
      exit; // n_operation is greater than found in safebox
  end;
  if (RBlock = 0) or (RBlock >= BlockManager.BlocksCount) then
    exit;
  // Search in previous blocks
  while (not Result) and (RBlock > 0) do
  begin
    xAuxBlock := RBlock;
    if not BlockManager.LoadTransactions(ABlock, RBlock) then
      exit;
    for i := ABlock.TransactionCount - 1 downto 0 do
    begin
      xTransaction := ABlock.Transaction[i];
      if (xTransaction.SignerAccount = Account) then
      begin
        if (xTransaction.NumberOfTransactions < n_operation) then
          exit; // n_operation is greaten than found
        if (xTransaction.NumberOfTransactions = n_operation) then
        begin
          // Possible candidate or dead
          xHashValid := xTransaction.TransactionHash(xInitialBlock);
          if (xHashValid = AHash) then
          begin
            ABlockIndex := i;
            Result := true;
            exit;
          end
          else if (RBlock < cProtocol_Upgrade_v2_MinBlock) then
          begin
            xOldHash := xTransaction.TransactionHash_OLD(xInitialBlock);
            if (xOldHash = AHash) then
            begin
              ABlockIndex := i;
              Result := true;
              exit;
            end
            else
              exit; // Not found!
          end
          else
            exit; // Not found!
        end;
        if xTransaction.Previous_Signer_Updated_Block > RBlock then
          exit;
        RBlock := xTransaction.Previous_Signer_Updated_Block;
      end
      else if xTransaction.DestinationAccount = Account then
      begin
        if xTransaction.Previous_Destination_updated_block > RBlock then
          exit;
        RBlock := xTransaction.Previous_Destination_updated_block;
      end
      else if xTransaction.SellerAccount = Account then
      begin
        if xTransaction.Previous_Seller_updated_block > RBlock then
          exit;
        RBlock := xTransaction.Previous_Seller_updated_block;
      end;
    end;
    if (RBlock >= xAuxBlock) then
      exit; // Error... not found a valid block positioning
    if (xInitialBlock <> 0) then
      exit; // If not found in specified block, no valid hash
  end;
end;

class procedure TNode.FreeNode;
begin
  if Assigned(_Node) then
    FreeAndNil(_Node);
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
      TNodeNotifyEvents(FNotifyList[i]).Messages.AddObject(TheMessage, Sender);
    end;
  end;
end;

procedure TNode.OnNewBlock(Sender: TObject);
begin
  FTransactionStorage.Sanitize;
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
      j := TConnectionManager.Instance.ConnectionsCountAll;
      for i := 0 to j - 1 do
      begin
        if TConnectionManager.Instance.GetConnection(i, nc) then
        begin
          if TConnectionManager.Instance.ConnectionLock(Self, nc, 500) then
          begin
            try
              nc.Send_Message(TheMessage);
            finally
              TConnectionManager.Instance.ConnectionUnlock(nc)
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

procedure TNode.SetKeyManager(const Value: TKeyManager);
begin
  FKeyManager := Value;
  FKeyManager.AccountStorage := self.BlockManager.AccountStorage;
end;

procedure TNode.SetNodeLogFilename(const Value: TFilename);
begin
  FNodeLog.FileName := Value;
end;

initialization
//CriticalSection := TCriticalSection.Create;

finalization
// FreeAndNil(CriticalSection);

end.
