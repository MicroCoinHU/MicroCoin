unit MicroCoin.Net.Utils;
{
  This unit contains code from PascalCoin:

  Copyright (c) Albert Molina 2016 - 2018 original code from PascalCoin https://pascalcoin.org/

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

interface

uses UThread;

type
  TThreadGetNewBlockChainFromClient = class(TPCThread)
  protected
    procedure BCExecute; override;
  end;

implementation

uses MicroCoin.Net.Connection,
  MicroCoin.Node.Node,
  MicroCoin.BlockChain.BlockHeader, MicroCoin.Net.NodeServer, ULog, MicroCoin.Net.ConnectionManager, Classes, Sysutils;

procedure TThreadGetNewBlockChainFromClient.BCExecute;
var
  i, j, iMax: Integer;
  maxWork: UInt64;
  nsa: TNodeServer;
  candidates: TList;
  lop: TBlockHeader;
  nc: TNetConnection;
begin
  // Search better candidates:
  candidates := TList.Create;
  try
    lop := CT_OperationBlock_NUL;
    TConnectionManager.NetData.MaxRemoteOperationBlock := CT_OperationBlock_NUL;
    // First round: Find by most work
    iMax := 0;
    maxWork := 0;
    j := TConnectionManager.NetData.ConnectionsCountAll;
    for i := 0 to j - 1 do
    begin
      if TConnectionManager.NetData.GetConnection(i, nc) then
      begin
        if (nc.RemoteAccumulatedWork > maxWork) and (nc.RemoteAccumulatedWork > TNode.Node.Bank.AccountStorage.WorkSum)
        then
        begin
          maxWork := nc.RemoteAccumulatedWork;
          iMax := i;
        end;
        // Preventing downloading
        if nc.IsDownloadingBlocks then
          exit;
      end;
    end;
    if (maxWork > 0) then
    begin
      for i := 0 to j - 1 do
      begin
        if TConnectionManager.NetData.GetConnection(i, nc) then
        begin
          if (nc.RemoteAccumulatedWork >= maxWork) then
          begin
            candidates.Add(nc);
            lop := nc.RemoteOperationBlock;
          end;
        end;
      end;
    end;
    // Second round: Find by most height
    if candidates.Count = 0 then
    begin
      for i := 0 to j - 1 do
      begin
        if (TConnectionManager.NetData.GetConnection(i, nc)) then
        begin
          if (nc.RemoteOperationBlock.Block >= TNode.Node.Bank.BlocksCount) and
            (nc.RemoteOperationBlock.Block >= lop.Block) then
          begin
            lop := nc.RemoteOperationBlock;
          end;
        end;
      end;
      if (lop.Block > 0) then
      begin
        for i := 0 to j - 1 do
        begin
          if (TConnectionManager.NetData.GetConnection(i, nc)) then
          begin
            if (nc.RemoteOperationBlock.Block >= lop.Block) then
            begin
              candidates.Add(nc);
            end;
          end;
        end;
      end;
    end;
    TConnectionManager.NetData.MaxRemoteOperationBlock := lop;
    if (candidates.Count > 0) then
    begin
      // Random a candidate
      i := 0;
      if (candidates.Count > 1) then
        i := Random(candidates.Count); // i = 0..count-1
      nc := TNetConnection(candidates[i]);
      TConnectionManager.NetData.GetNewBlockChainFromClient(nc, Format('Candidate block: %d sum: %d',
        [nc.RemoteOperationBlock.Block, nc.RemoteAccumulatedWork]));
    end;
  finally
    candidates.Free;
  end;
end;

end.
