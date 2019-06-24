unit MicroCoin.Net.Utils;
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
  i, j, xAccount: Integer;
  xMaxWork: UInt64;
  xNodeServer: TNodeServer;
  xCandidateList: TList;
  xBlockHeader: TBlockHeader;
  xConnection: TNetConnection;
begin
  // Search better candidates:
  xCandidateList := TList.Create;
  try
    xBlockHeader := TBlockHeader.Empty;
//    TConnectionManager.Instance.MaxRemoteOperationBlock := CT_OperationBlock_NUL;
    // First round: Find by most work
    xAccount := 0;
    xMaxWork := 0;
    j := TConnectionManager.Instance.ConnectionsCountAll;
    for i := 0 to j - 1 do
    begin
      if TConnectionManager.Instance.GetConnection(i, xConnection) then
      begin
        if (xConnection.RemoteAccumulatedWork > xMaxWork) and (xConnection.RemoteAccumulatedWork > TNode.Node.BlockManager.AccountStorage.WorkSum)
        then
        begin
          xMaxWork := xConnection.RemoteAccumulatedWork;
          xAccount := i;
        end;
        // Preventing downloading
        if xConnection.IsDownloadingBlocks
        then exit;
      end;
    end;
    if (xMaxWork > 0) then
    begin
      for i := 0 to j - 1 do
      begin
        if TConnectionManager.Instance.GetConnection(i, xConnection) then
        begin
          if (xConnection.RemoteAccumulatedWork >= xMaxWork) then
          begin
            xCandidateList.Add(xConnection);
            xBlockHeader := xConnection.RemoteOperationBlock;
          end;
        end;
      end;
    end;
    // Second round: Find by most height
    if xCandidateList.Count = 0 then
    begin
      for i := 0 to j - 1 do
      begin
        if (TConnectionManager.Instance.GetConnection(i, xConnection)) then
        begin
          if (xConnection.RemoteOperationBlock.Block >= TNode.Node.BlockManager.BlocksCount) and
            (xConnection.RemoteOperationBlock.Block >= xBlockHeader.Block) then
          begin
            xBlockHeader := xConnection.RemoteOperationBlock;
          end;
        end;
      end;
      if (xBlockHeader.Block > 0) then
      begin
        for i := 0 to j - 1 do
        begin
          if (TConnectionManager.Instance.GetConnection(i, xConnection)) then
          begin
            if (xConnection.RemoteOperationBlock.Block >= xBlockHeader.Block) then
            begin
              xCandidateList.Add(xConnection);
            end;
          end;
        end;
      end;
    end;
    TConnectionManager.Instance.MaxRemoteOperationBlock := xBlockHeader;
    if (xCandidateList.Count > 0) then
    begin
      // Random a candidate
      i := 0;
      if (xCandidateList.Count > 1) then
        i := Random(xCandidateList.Count); // i = 0..count-1
      xConnection := TNetConnection(xCandidateList[i]);
      TConnectionManager.Instance.GetNewBlockChainFromClient(xConnection, Format('Candidate block: %d sum: %d',
        [xConnection.RemoteOperationBlock.Block, xConnection.RemoteAccumulatedWork]));
    end;
  finally
    xCandidateList.Free;
  end;
end;

end.
