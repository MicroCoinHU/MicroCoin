unit MicroCoin.Net.Statistics;
{
  This unit contains code from PascalCoin:

  Copyright (c) Albert Molina 2016 - 2018 original code from PascalCoin https://pascalcoin.org/

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

interface

type

  TNetStatistics = record
    ActiveConnections: Integer; // All connections wiht "connected" state
    ClientsConnections: Integer; // All clients connected to me like a server with "connected" state
    ServersConnections: Integer; // All servers where I'm connected
    ServersConnectionsWithResponse: Integer; // All servers where I'm connected and I've received data
    TotalConnections: Integer;
    TotalClientsConnections: Integer;
    TotalServersConnections: Integer;
    BytesReceived: Int64;
    BytesSend: Int64;
    NodeServersListCount: Integer;
    NodeServersDeleted: Integer;
  end;

const
    CT_TNetStatistics_NUL: TNetStatistics = (ActiveConnections: 0; ClientsConnections: 0; ServersConnections: 0; ServersConnectionsWithResponse: 0; TotalConnections: 0; TotalClientsConnections: 0;
    TotalServersConnections: 0; BytesReceived: 0; BytesSend: 0; NodeServersListCount: 0; NodeServersDeleted: 0);

implementation

end.
