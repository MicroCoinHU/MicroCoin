unit MicroCoin.Mining.Server;
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

uses UTCPIP, UJsonFunctions, MicroCoin.Account.AccountKey, UThread,
  MicroCoin.BlockChain.Block,
  UCrypto, MicroCoin.Node.Events, SysUtils, Classes, ULog, MicroCoin.RPC.Client,
  MicroCoin.BlockChain.BlockHeader, MicroCoin.Common.Config, MicroCoin.Net.ConnectionManager,
  Variants,
  MicroCoin.Node.Node, MicroCoin.Transaction.HashTree,
  MicroCoin.Transaction.Base,
  MicroCoin.Transaction.Itransaction,
  MicroCoin.Account.Storage,{$IFDEF MSWINDOWS} Windows,{$ENDIF} MicroCoin.BlockChain.Protocol,
  MicroCoin.Mining.Common;

type

  TMiningServer = class;

  TPoolMiningServerThread = class(TPCThread)
  private
    FPoolMiningServer: TMiningServer;
  protected
    procedure BCExecute; override;
  public
    constructor Create(APoolMiningServer: TMiningServer);
    destructor Destroy; override;
  end;

  TMiningServer = class(TNetTcpIpServer)
  private
    FIncomingsCounter: Integer;
    FNodeNotifyEvents: TNodeNotifyEvents;
    FMinerAccountKey: TAccountKey;
    FMinerPayload: TRawBytes;
    FClientsWins: Integer;
    FClientsCount: Integer;
    FOnMiningServerNewBlockFound: TNotifyEvent;
    FPoolJobs: TPCThreadList;
    FPoolThread: TPoolMiningServerThread;
    FMinerOperations: TBlock;
    FMaxOperationsPerBlock: Integer;
    FMax0FeeOperationsPerBlock: Integer;
    procedure DoProcessJSON(json: TPCJSONObject; ResponseMethod: string; Client: TJSONRPCTcpIpClient);
    procedure OnNodeNewBlock(Sender: TObject);
    procedure OnNodeOperationsChanged(Sender: TObject);
    function MinerSubmit(Client: TJSONRPCTcpIpClient; params: TPCJSONObject; const id: Variant): Boolean;
    procedure SetMinerAccountKey(const Value: TAccountKey);
    procedure SetMinerPayload(const Value: TRawBytes);
    procedure ClearPoolJobs;
    procedure CaptureNewJobAndSendToMiners;
    procedure SendJobToMiner(Operations: TBlock; Client: TJSONRPCTcpIpClient; IsResponse: Boolean; idResponse: Variant);
    procedure FillMinerOperations;
    procedure SetMax0FeeOperationsPerBlock(const Value: Integer);
    procedure SetMaxOperationsPerBlock(const Value: Integer);
  protected
    procedure OnNewIncommingConnection(Sender: TObject; Client: TNetTcpIpClient); override;
    procedure SetActive(const Value: Boolean); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property MinerAccountKey: TAccountKey read FMinerAccountKey write SetMinerAccountKey;
    property MinerPayload: TRawBytes read FMinerPayload write SetMinerPayload;
    procedure UpdateAccountAndPayload(AMinerAccountKey: TAccountKey; AMinerPayload: TRawBytes);
    property ClientsCount: Integer read FClientsCount;
    property ClientsWins: Integer read FClientsWins;
    property OnMiningServerNewBlockFound: TNotifyEvent read FOnMiningServerNewBlockFound
      write FOnMiningServerNewBlockFound;
    property MaxZeroFeeOperationsPerBlock: Integer read FMax0FeeOperationsPerBlock write SetMax0FeeOperationsPerBlock;
    property MaxOperationsPerBlock: Integer read FMaxOperationsPerBlock write SetMaxOperationsPerBlock;
  end;

  TPoolJob = record
    OperationsComp: TBlock;
    SentDateTime: TDateTime;
    SentMinTimestamp: Cardinal;
  end;

  PPoolJob = ^TPoolJob;

implementation

const
  CT_WAIT_SECONDS_BEFORE_SEND_NEW_JOB = 10;
  CT_MAX_SECONDS_BETWEEN_JOBS = 30;
  // 1.5.3 Will send new job to miner (with updated timestamp)
  CT_MAX_BUFFER_JOBS = 10; // 1.5.3 Will buffer last 10 jobs sent to miner

procedure TMiningServer.CaptureNewJobAndSendToMiners;
var
  P, PToDelete: PPoolJob;
  i: Integer;
  l: TList;
  doAdd: Boolean;
  params: TPCJSONObject;
  OpB: TBlockHeader;
begin
  if not Active then
    exit;
  if FClientsCount <= 0 then
    exit;
  doAdd := false;
  P := nil;
  l := FPoolJobs.LockList;
  try
    if l.count = 0 then
      doAdd := true
    else
    begin
      P := l[l.count - 1];
      if (FNodeNotifyEvents.Node.TransactionStorage.TransactionHashTree.HashTree <> P^.OperationsComp.TransactionHashTree.HashTree)
      then
      begin
        doAdd := (P^.SentDateTime + EncodeTime(0, 0, CT_WAIT_SECONDS_BEFORE_SEND_NEW_JOB, 0)) < Now;
      end
      else
      begin
        // No new operations waiting to be sent, but to prevent "old time mining", we will send new job with time:
        doAdd := ((P^.SentDateTime + EncodeTime(0, 0, CT_MAX_SECONDS_BETWEEN_JOBS, 0)) < Now);
      end;
    end;
    if doAdd then
    begin
      New(P);
      P^.SentDateTime := Now;
      P^.SentMinTimestamp := TConnectionManager.Instance.NetworkAdjustedTime.GetAdjustedTime;
      if (P^.SentMinTimestamp < FNodeNotifyEvents.Node.BlockManager.LastBlockFound.BlockHeader.timestamp) then
      begin
        P^.SentMinTimestamp := FNodeNotifyEvents.Node.BlockManager.LastBlockFound.BlockHeader.timestamp;
      end;
      FillMinerOperations;
      P^.OperationsComp := TBlock.Create(nil);
      P^.OperationsComp.CopyFrom(FMinerOperations);
      P^.OperationsComp.AccountKey := FMinerAccountKey;
      P^.OperationsComp.BlockPayload := FMinerPayload;
      P^.OperationsComp.timestamp := P^.SentMinTimestamp;
      // Best practices 1.5.3
      OpB := P^.OperationsComp.BlockHeader;
      if (OpB.Block <> 0) and (OpB.Block <> (FNodeNotifyEvents.Node.BlockManager.LastBlockFound.BlockHeader.Block + 1)) then
      begin
        // A new block is generated meanwhile ... do not include
        TLog.NewLog(ltDebug, ClassName, 'Generated a new block meanwhile ... ' + OpB.ToString() + '<>'
          + FNodeNotifyEvents.Node.BlockManager.LastBlockFound.BlockHeader.ToString());
        P^.OperationsComp.Free;
        Dispose(P);
        P := nil;
      end
      else
      begin
        i := l.Add(P);
        TLog.NewLog(ltDebug, ClassName, 'Added new job ' + IntToStr(i + 1) + '/' + IntToStr(l.count));
      end;
    end;
    // Clean buffer jobs
    while (l.count > CT_MAX_BUFFER_JOBS) do
    begin
      PToDelete := l[0]; // Index 0 is oldest sent job
      l.Delete(0);
      PToDelete^.OperationsComp.Free;
      Dispose(PToDelete);
      TLog.NewLog(ltDebug, ClassName, 'Deleted Job 1 from buffer, now count:' + IntToStr(l.count));
    end;
  finally
    FPoolJobs.UnlockList;
  end;
  if (doAdd) and (Assigned(P)) then
  begin
    params := TPCJSONObject.Create;
    try
      l := NetTcpIpClientsLock;
      try
        for i := 0 to l.count - 1 do
        begin
          if not Active then
            exit;
          SendJobToMiner(P^.OperationsComp, l[i], false, null);
        end;
        TLog.NewLog(ltDebug, ClassName, 'Sending job to miners: ' + P^.OperationsComp.BlockHeader.ToString
          () + ' Cache blocks:' + IntToStr(l.count));
      finally
        NetTcpIpClientsUnlock;
      end;
    finally
      params.Free;
    end;
  end;
end;

procedure TMiningServer.ClearPoolJobs;
var
  P: PPoolJob;
  i: Integer;
  l: TList;
begin
  l := FPoolJobs.LockList;
  try
    for i := l.count - 1 downto 0 do
    begin
      P := l[i];
      l.Delete(i);
      P^.OperationsComp.Free;
      Dispose(P);
    end;
    l.Clear;
  finally
    FPoolJobs.UnlockList;
  end;
end;

constructor TMiningServer.Create;
begin
  inherited;
  FOnMiningServerNewBlockFound := nil;
  FIncomingsCounter := 0;
  FClientsWins := 0;
  FClientsCount := 0;
  MaxConnections := 1000;
  NetTcpIpClientClass := TJSONRPCTcpIpClient;
  FNodeNotifyEvents := TNodeNotifyEvents.Create(nil);
  FNodeNotifyEvents.OnBlocksChanged := OnNodeNewBlock;
  FNodeNotifyEvents.OnTransactionsChanged := OnNodeOperationsChanged;
  FNodeNotifyEvents.Node := TNode.Node;
  FMinerOperations := TBlock.Create(FNodeNotifyEvents.Node.BlockManager);
  FMinerAccountKey := CT_TECDSA_Public_Nul;
  FMinerPayload := '';
  FPoolJobs := TPCThreadList.Create('TPoolMiningServer_PoolJobs');
  FPoolThread := TPoolMiningServerThread.Create(Self);
  FMax0FeeOperationsPerBlock := cMAX_Zero_Fee_operations_per_block_by_miner;
  FMaxOperationsPerBlock := cMAX_Operations_per_block_by_miner;
end;

destructor TMiningServer.Destroy;
begin
  Active := False;
  FPoolThread.Terminate;
  FPoolThread.WaitFor;
  FreeAndNil(FPoolThread);
  FNodeNotifyEvents.Node := nil;
  FNodeNotifyEvents.OnBlocksChanged := nil;
  FNodeNotifyEvents.OnTransactionsChanged := nil;
  FreeAndNil(FMinerOperations);
  FreeAndNil(FNodeNotifyEvents);
  ClearPoolJobs;
  FreeAndNil(FPoolJobs);
  inherited;
end;

procedure TMiningServer.DoProcessJSON(json: TPCJSONObject; ResponseMethod: string; Client: TJSONRPCTcpIpClient);
var
  method: string;
  params: TPCJSONArray;
  id_value: Variant;
  i: Integer;
  response_result: TPCJSONObject;
begin
  if ResponseMethod <> '' then
  begin
    method := ResponseMethod;
    params := json.GetAsArray('result');
  end
  else
  begin
    method := json.AsString('method', '');
    params := json.GetAsArray('params');
  end;
  i := json.IndexOfName('id');
  if i < 0 then
  begin
    id_value := null;
  end
  else
  begin
    id_value := json.GetAsVariant('id').Value;
  end;
  if method = CT_PoolMining_Method_STATUS then
  begin
    response_result := TPCJSONObject.Create;
    try
      response_result.GetAsVariant('block').Value := FNodeNotifyEvents.Node.BlockManager.LastBlockFound.BlockHeader.Block;
      response_result.GetAsVariant('account_key').Value :=
        TCrypto.ToHexaString(FNodeNotifyEvents.Node.BlockManager.LastBlockFound.BlockHeader.account_key.ToRawString);
      response_result.GetAsVariant('reward').Value := FNodeNotifyEvents.Node.BlockManager.LastBlockFound.BlockHeader.reward;
      response_result.GetAsVariant('fee').Value := FNodeNotifyEvents.Node.BlockManager.LastBlockFound.BlockHeader.fee;
      response_result.GetAsVariant('p_version').Value := FNodeNotifyEvents.Node.BlockManager.LastBlockFound.BlockHeader.
        protocol_version;
      response_result.GetAsVariant('p_available').Value := FNodeNotifyEvents.Node.BlockManager.LastBlockFound.BlockHeader.
        protocol_available;
      response_result.GetAsVariant('timestamp').Value :=
        FNodeNotifyEvents.Node.BlockManager.LastBlockFound.BlockHeader.timestamp;
      response_result.GetAsVariant('target').Value := FNodeNotifyEvents.Node.BlockManager.LastBlockFound.BlockHeader.
        compact_target;
      response_result.GetAsVariant('nonce').Value := FNodeNotifyEvents.Node.BlockManager.LastBlockFound.BlockHeader.nonce;
      response_result.GetAsVariant('payload').Value :=
        TCrypto.ToHexaString(FNodeNotifyEvents.Node.BlockManager.LastBlockFound.BlockHeader.block_payload);
      response_result.GetAsVariant('initial_sbh').Value :=
        TCrypto.ToHexaString(FNodeNotifyEvents.Node.BlockManager.LastBlockFound.BlockHeader.initial_safe_box_hash);
      response_result.GetAsVariant('operations_hash').Value :=
        TCrypto.ToHexaString(FNodeNotifyEvents.Node.BlockManager.LastBlockFound.BlockHeader.transactionHash);
      response_result.GetAsVariant('pow').Value :=
        TCrypto.ToHexaString(FNodeNotifyEvents.Node.BlockManager.LastBlockFound.BlockHeader.proof_of_work);
      Client.SendJSONRPCResponse(response_result, id_value);
    finally
      response_result.Free;
    end;
  end
  else if method = CT_PoolMining_Method_MINER_NOTIFY then
  begin
    SendJobToMiner(nil, Client, true, id_value);
  end
  else if method = CT_PoolMining_Method_MINER_SUBMIT then
  begin
    // Try to submit a PoW
    if params.count = 1 then
      MinerSubmit(Client, params.GetAsObject(0), id_value)
    else
      TLog.NewLog(lterror, ClassName, 'Invalid params array of method ' + method);
  end
  else
  begin
    // Invalid command
    if (not VarIsNull(id_value)) then
    begin
      Client.SendJSONRPCErrorResponse(id_value, 'method not found: ' + method);
    end;
  end;
end;

procedure TMiningServer.FillMinerOperations;
var
  tree: TTransactionHashTree;
  procedure doAdd(const Op: ITransaction; checkDuplicate: Boolean);
  begin
    if checkDuplicate then
    begin
      if tree.IndexOf(Op) >= 0 then
        exit;
    end;
    tree.AddTransactionToHashTree(Op);
  end;

var
  i, j: Integer;
  xMaster: TBlock;
  xBlock: ITransaction;
var
  errors: AnsiString;
begin
  xMaster := FNodeNotifyEvents.Node.TransactionStorage;
  xMaster.Lock;
  try
    FMinerOperations.Lock;
    try
      tree := TTransactionHashTree.Create;
      try
        if (not(TBlock.Equals(FMinerOperations.BlockHeader, xMaster.BlockHeader))) then
        begin
          FMinerOperations.Clear(true);
          if xMaster.TransactionCount > 0 then
          begin
            // First round: Select with fee > 0
            i := 0;
            while (tree.TransactionCount < MaxOperationsPerBlock) and
              (i < xMaster.TransactionHashTree.TransactionCount) do
            begin
              xBlock := xMaster.TransactionHashTree.GetTransaction(i);
              if xBlock.fee > 0 then
              begin
                doAdd(xBlock, false);
              end;
              inc(i);
            end;
            // Second round: Allow fee = 0
            j := 0;
            i := 0;
            while (tree.TransactionCount < MaxOperationsPerBlock) and (i < xMaster.TransactionHashTree.TransactionCount)
              and (j < MaxZeroFeeOperationsPerBlock) do
            begin
              xBlock := xMaster.TransactionHashTree.GetTransaction(i);
              if xBlock.fee = 0 then
              begin
                doAdd(xBlock, true);
                inc(j);
              end;
              inc(i);
            end;
            // Add operations:
            i := FMinerOperations.AddTransactions(tree, errors);
            if (i <> tree.TransactionCount) or (i <> xMaster.TransactionHashTree.TransactionCount) then
            begin
              TLog.NewLog(ltDebug, ClassName,
                Format('Cannot add all operations! Master:%d Selected:%d Added:%d - Errors: %s',
                [xMaster.TransactionHashTree.TransactionCount, tree.TransactionCount, i, errors]));
            end;
          end
          else
          begin
            FMinerOperations.CopyFrom(xMaster);
          end;
          //
          TLog.NewLog(ltInfo, ClassName, Format('New miner operations:%d Hash:%s %s',
            [FMinerOperations.TransactionHashTree.TransactionCount,
            TCrypto.ToHexaString(FMinerOperations.TransactionHashTree.HashTree),
            TCrypto.ToHexaString(FMinerOperations.BlockHeader.transactionHash)]));
        end
        else
        begin
          TLog.NewLog(ltDebug, ClassName, Format('No need to change Miner buffer. Operations:%d',
            [FMinerOperations.TransactionHashTree.TransactionCount]));
        end;
      finally
        tree.Free;
      end;
    finally
      FMinerOperations.Unlock;
    end;
  finally
    xMaster.Unlock;
  end;
end;

function TMiningServer.MinerSubmit(Client: TJSONRPCTcpIpClient; params: TPCJSONObject; const id: Variant): Boolean;
var
  s: string;
  nbOperations: TBlock;
  errors, sJobInfo: AnsiString;
  nba: TAccountStorageEntry;
  json: TPCJSONObject;
  p1, p2, p3: TRawBytes;
  P: PPoolJob;
  i: Integer;
  l: TList;
  _payloadHexa, _payload: AnsiString;
  _timestamp, _nOnce: Cardinal;
  _targetPoW: TRawBytes;
  PowNotOK: Boolean;
begin
  { Miner params must submit:
    - "payload" as an Hexadecimal
    - "timestamp" as an unsigned integer 32 bits
    - "nonce" as an unsigned integer 32 bits
    If payload length is < Node payload then error
    If Node payload is not included in first bytes of payload then error
    If timestamp is not valid then error
    If calculated PoW does not match valid PoW then error
    If all ok... congrats!!! }
  Result := false;
  sJobInfo := '';
  PowNotOK := false;
  // Must chek on previous sent jobs
  nbOperations := nil;
  try
    _payloadHexa := params.AsString('payload', '');
    _payload := TCrypto.HexaToRaw(_payloadHexa);
    if FMinerPayload <> '' then
    begin
      if (copy(_payload, 1, length(FMinerPayload)) <> FMinerPayload) then
      begin
        if _payload = '' then
          _payload := _payloadHexa;
        Client.SendJSONRPCErrorResponse(id, 'Invalid payload (' + _payload + '). Need start with: ' + FMinerPayload);
        exit;
      end;
    end;
    _timestamp := params.AsCardinal('timestamp', 0);
    _nOnce := params.AsCardinal('nonce', 0);
    l := FPoolJobs.LockList;
    try
      i := l.count - 1;
      while (i >= 0) and (not Assigned(nbOperations)) do
      begin
        P := l[i];
        P^.OperationsComp.BlockPayload := _payload;
        // Best practices: Only will accept a solution if timestamp >= sent timestamp for this job (1.5.3)
        if (P^.SentMinTimestamp <= _timestamp) then
        begin
          P^.OperationsComp.timestamp := _timestamp;
          P^.OperationsComp.nonce := _nOnce;
          _targetPoW := FNodeNotifyEvents.Node.BlockManager.AccountStorage.GetActualTargetHash
            (P^.OperationsComp.BlockHeader.protocol_version = cPROTOCOL_2);
          if (P^.OperationsComp.BlockHeader.proof_of_work <= _targetPoW) then
          begin
            // Candidate!
            nbOperations := TBlock.Create(nil);
            nbOperations.BlockManager := FNodeNotifyEvents.Node.BlockManager;
            nbOperations.CopyFrom(P^.OperationsComp);
            nbOperations.AccountKey := MinerAccountKey;
            sJobInfo := 'Miner job ' + IntToStr(i + 1) + '/' + IntToStr(l.count);
            TLog.NewLog(ltInfo, ClassName, sJobInfo + ' - Found a solution for block ' +
              IntToStr(nbOperations.BlockHeader.Block));
            PowNotOK := false;
          end
          else
            PowNotOK := true;
        end;
        dec(i);
      end;
    finally
      FPoolJobs.UnlockList;
    end;
    if Assigned(nbOperations) then
    begin
      if FNodeNotifyEvents.Node.AddNewBlockChain(nil, nbOperations, nba, errors) then
      begin
        // CONGRATS !!!
        json := TPCJSONObject.Create;
        try
          json.GetAsVariant('block').Value := FNodeNotifyEvents.Node.BlockManager.LastBlock.Block;
          json.GetAsVariant('pow').Value :=
            TCrypto.ToHexaString(FNodeNotifyEvents.Node.BlockManager.LastBlock.proof_of_work);
          json.GetAsVariant('payload').Value := nbOperations.BlockPayload;
          json.GetAsVariant('timestamp').Value := nbOperations.timestamp;
          json.GetAsVariant('nonce').Value := nbOperations.nonce;
          inc(FClientsWins);
          Client.SendJSONRPCResponse(json, id);
        finally
          json.Free;
        end;
        if Assigned(FOnMiningServerNewBlockFound) then
          FOnMiningServerNewBlockFound(Self);
      end
      else
      begin
        Client.SendJSONRPCErrorResponse(id, 'Error: ' + errors + ' executing ' + sJobInfo + ' payload:' +
          nbOperations.BlockPayload + ' timestamp:' + IntToStr(nbOperations.timestamp) + ' nonce:' +
          IntToStr(nbOperations.nonce));
      end;
    end
    else
    begin
      if PowNotOK then
        Client.SendJSONRPCErrorResponse(id, 'Error: Pow NOT OK! payload:' + _payload + ' timestamp:' +
          IntToStr(_timestamp) + ' nonce:' + IntToStr(_nOnce))
      else
        Client.SendJSONRPCErrorResponse(id,
          'Error: No valid job found with these values! (Perhaps prior block job or old job) payload:' + _payload +
          ' timestamp:' + IntToStr(_timestamp) + ' nonce:' + IntToStr(_nOnce));
    end;
  finally
    if Assigned(nbOperations) then
      nbOperations.Free;
  end;
end;

procedure TMiningServer.OnNewIncommingConnection(Sender: TObject; Client: TNetTcpIpClient);
var
  bClient: TJSONRPCTcpIpClient;
  jsonobj: TPCJSONObject;
  doDelete: Boolean;
  rmethod: string;
begin
  inherited;
  inc(FClientsCount);
  try
    TLog.NewLog(ltInfo, ClassName, 'New Mining Pool Connection: ' + Client.ClientRemoteAddr);
    bClient := TJSONRPCTcpIpClient(Client);
    inc(FIncomingsCounter);
    SendJobToMiner(nil, bClient, false, null);
    while (Active) and (Client.Connected) do
    begin
      doDelete := bClient.LastReadTC + 1000 < GetTickCount;
      // TODO: Protect GetTickCount overflow
      jsonobj := TPCJSONObject.Create;
      try
        if bClient.DoProcessBuffer(nil, 1000, doDelete, rmethod, jsonobj) then
        begin
          DoProcessJSON(jsonobj, rmethod, bClient);
        end;
      finally
        jsonobj.Free;
      end;
      sleep(10);
    end;
  finally
    dec(FClientsCount);
    TLog.NewLog(ltInfo, ClassName, 'Finalizing Mining Pool Connection: ' + Client.ClientRemoteAddr);
  end;
end;

procedure TMiningServer.OnNodeNewBlock(Sender: TObject);
begin
  // Delete Jobs cache, because a new block was found prior to us
  ClearPoolJobs;
  CaptureNewJobAndSendToMiners;
end;

procedure TMiningServer.OnNodeOperationsChanged(Sender: TObject);
begin
  CaptureNewJobAndSendToMiners;
end;

procedure TMiningServer.SendJobToMiner(Operations: TBlock; Client: TJSONRPCTcpIpClient; IsResponse: Boolean;
  idResponse: Variant);
var
  params: TPCJSONObject;
  ts: Cardinal;
var
  P: PPoolJob;
  i, nJobs: Integer;
  l: TList;
begin
  if FClientsCount <= 0 then
    exit;
  if (not Assigned(Operations)) then
  begin
    P := nil;
    l := FPoolJobs.LockList;
    try
      if l.count > 0 then
        P := l[l.count - 1] // The last
      else
      begin
        TLog.NewLog(ltInfo, ClassName, 'Creating new job for miner');
        New(P);
        P^.SentDateTime := Now;
        P^.SentMinTimestamp := TConnectionManager.Instance.NetworkAdjustedTime.GetAdjustedTime;
        if (P^.SentMinTimestamp < FNodeNotifyEvents.Node.BlockManager.LastBlockFound.BlockHeader.timestamp) then
        begin
          P^.SentMinTimestamp := FNodeNotifyEvents.Node.BlockManager.LastBlockFound.BlockHeader.timestamp;
        end;
        FillMinerOperations;
        P^.OperationsComp := TBlock.Create(nil);
        P^.OperationsComp.CopyFrom(FMinerOperations);
        P^.OperationsComp.AccountKey := FMinerAccountKey;
        P^.OperationsComp.BlockPayload := FMinerPayload;
        P^.OperationsComp.timestamp := P^.SentMinTimestamp;
        // Best practices 1.5.3
        if (P^.OperationsComp.BlockHeader.Block <> 0) and
          (P^.OperationsComp.BlockHeader.Block <> (FNodeNotifyEvents.Node.BlockManager.LastBlockFound.BlockHeader.Block
          + 1)) then
        begin
          TLog.NewLog(lterror, ClassName, 'ERROR DEV 20170228-2 ' + P^.OperationsComp.BlockHeader.ToString
            () + '<>' + FNodeNotifyEvents.Node.BlockManager.LastBlockFound.BlockHeader.ToString
            ());
          P^.OperationsComp.Free;
          Dispose(P);
          raise Exception.Create('ERROR DEV 20170228-2');
        end;
        l.Add(P);
      end;
    finally
      FPoolJobs.UnlockList;
    end;
    Operations := P^.OperationsComp;
  end;
  l := FPoolJobs.LockList;
  try
    nJobs := l.count;
  finally
    FPoolJobs.UnlockList;
  end;

  params := TPCJSONObject.Create;
  try
    if not Active then
      exit;
    params.GetAsVariant('block').Value := Operations.BlockHeader.Block;
    params.GetAsVariant('version').Value := Operations.BlockHeader.protocol_version;
    params.GetAsVariant('part1').Value := TCrypto.ToHexaString(Operations.PoW_Digest_Part1);
    params.GetAsVariant('payload_start').Value := TCrypto.ToHexaString(Operations.BlockHeader.block_payload);
    params.GetAsVariant('part3').Value := TCrypto.ToHexaString(Operations.PoW_Digest_Part3);
    params.GetAsVariant('target').Value := Operations.BlockHeader.compact_target;
    params.GetAsVariant('target_pow').Value :=
      TCrypto.ToHexaString(TMicroCoinProtocol.TargetFromCompact(Operations.BlockHeader.compact_target));

    ts := TConnectionManager.Instance.NetworkAdjustedTime.GetAdjustedTime;
    if (ts < FNodeNotifyEvents.Node.BlockManager.LastBlockFound.BlockHeader.timestamp) then
    begin
      ts := FNodeNotifyEvents.Node.BlockManager.LastBlockFound.BlockHeader.timestamp;
    end;
    params.GetAsVariant('timestamp').Value := ts;

    if IsResponse then
    begin
      Client.SendJSONRPCResponse(params, idResponse);
    end
    else
    begin
      Client.SendJSONRPCMethod(CT_PoolMining_Method_MINER_NOTIFY, params, null);
    end;

    TLog.NewLog(ltInfo, ClassName, Format('Sending job %d to miner - Block:%d Ops:%d Target:%s PayloadStart:%s',
      [nJobs, Operations.BlockHeader.Block, Operations.TransactionCount, IntToHex(Operations.BlockHeader.compact_target, 8),
      Operations.BlockHeader.block_payload]));
  finally
    params.Free;
  end;
end;

procedure TMiningServer.SetActive(const Value: Boolean);
begin
  inherited;
  if (not Value) then
  begin
    WaitUntilNetTcpIpClientsFinalized;
  end;
end;

procedure TMiningServer.SetMax0FeeOperationsPerBlock(const Value: Integer);
begin
  if FMax0FeeOperationsPerBlock = Value then
    exit;
  if (Value < (cMAX_Zero_Fee_operations_per_block_by_miner div 5)) or (Value < 1) then
  begin
    FMax0FeeOperationsPerBlock := (cMAX_Zero_Fee_operations_per_block_by_miner div 5);
    // To prevent no 0 fee...
    if FMax0FeeOperationsPerBlock < 1 then
      FMax0FeeOperationsPerBlock := 1; // For Testnet or low constant values...
    TLog.NewLog(lterror, ClassName, Format('Invalid max zero fee operations per block value %d, set to %d',
      [Value, FMax0FeeOperationsPerBlock]));
  end
  else
    FMax0FeeOperationsPerBlock := Value;
  TLog.NewLog(ltInfo, ClassName, Format('Updated max zero fee operations per block to %d',
    [FMax0FeeOperationsPerBlock]));
  CaptureNewJobAndSendToMiners;
end;

procedure TMiningServer.SetMaxOperationsPerBlock(const Value: Integer);
begin
  if FMaxOperationsPerBlock = Value then
    exit;
  if (Value < (cMAX_Operations_per_block_by_miner div 5)) or (Value < 1) then
  begin
    FMaxOperationsPerBlock := (cMAX_Operations_per_block_by_miner div 5);
    // To prevent very small blocks...
    if FMaxOperationsPerBlock < 1 then
      FMaxOperationsPerBlock := 1; // For Testnet or low constant values...
    TLog.NewLog(lterror, ClassName, Format('Invalid max operations per block value %d, set to %d',
      [Value, FMaxOperationsPerBlock]));
  end
  else
    FMaxOperationsPerBlock := Value;
  TLog.NewLog(ltInfo, ClassName, Format('Updated max operations per block to %d', [FMaxOperationsPerBlock]));
  CaptureNewJobAndSendToMiners;
end;

procedure TMiningServer.SetMinerAccountKey(const Value: TAccountKey);
begin
  if TAccountKey.EqualAccountKeys(FMinerAccountKey, Value) then
    exit;
  FMinerAccountKey := Value;
  TLog.NewLog(ltDebug, ClassName, 'Assigning Miner account key to: ' + TCrypto.ToHexaString(Value.ToRawString));
  CaptureNewJobAndSendToMiners;
end;

procedure TMiningServer.SetMinerPayload(const Value: TRawBytes);
begin
  FMinerPayload := Value;
  TLog.NewLog(ltDebug, ClassName, 'Assigning Miner new Payload: ' + TCrypto.ToHexaString(Value));
  CaptureNewJobAndSendToMiners;
end;

procedure TMiningServer.UpdateAccountAndPayload(AMinerAccountKey: TAccountKey; AMinerPayload: TRawBytes);
begin
  FMinerAccountKey := AMinerAccountKey;
  TLog.NewLog(ltDebug, ClassName, 'Assigning Miner account key to: ' +
    TCrypto.ToHexaString(AMinerAccountKey.ToRawString));
  FMinerPayload := AMinerPayload;
  TLog.NewLog(ltDebug, ClassName, 'Assigning Miner new Payload: ' + TCrypto.ToHexaString(AMinerPayload));
  CaptureNewJobAndSendToMiners;
end;

procedure TPoolMiningServerThread.BCExecute;
var
  i: Integer;
begin
  i := 0;
  repeat
    sleep(100);
    inc(i);
    if (not terminated) and ((i mod 10) = 0) then
    begin
      try
        FPoolMiningServer.CaptureNewJobAndSendToMiners;
      except
        on E: Exception do
        begin
          TLog.NewLog(lterror, ClassName, 'Error (' + E.ClassName + ') Capturing job for miners: ' + E.Message);
        end;
      end;
    end;
  until terminated;
end;

constructor TPoolMiningServerThread.Create(APoolMiningServer: TMiningServer);
begin
  FPoolMiningServer := APoolMiningServer;
  inherited Create(false);
end;

destructor TPoolMiningServerThread.Destroy;
begin

  inherited;
end;

end.
