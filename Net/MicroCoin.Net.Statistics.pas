{==============================================================================|
| MicroCoin                                                                    |
| Copyright (c) 2018 MicroCoin Developers                                      |
|==============================================================================|
| Permission is hereby granted, free of charge, to any person obtaining a copy |
| of this software and associated documentation files (the "Software"), to     |
| deal in the Software without restriction, including without limitation the   |
| rights to use, copy, modify, merge, publish, distribute, sublicense, and/or  |
| sell opies of the Software, and to permit persons to whom the Software is    |
| furnished to do so, subject to the following conditions:                     |
|                                                                              |
| The above copyright notice and this permission notice shall be included in   |
| all copies or substantial portions of the Software.                          |
|------------------------------------------------------------------------------|
| THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR   |
| IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,     |
| FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE  |
| AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER       |
| LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING      |
| FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER          |
| DEALINGS IN THE SOFTWARE.                                                    |
|==============================================================================|
| File:       MicroCoin.Net.Statistics.pas                                     |
| Created at: 2018-08-28                                                       |
| Purpose:    Base structure for network stats                                 |
|==============================================================================}

unit MicroCoin.Net.Statistics;

{$ifdef FPC}
  {$mode delphi}
{$endif}


interface

type

  TNetStatistics = record
  private
    FBytesReceived: Int64;
    FBytesSend: Int64;
    procedure SetBytesReceived(const Value: Int64);
    procedure SetBytesSend(const Value: Int64);
    function GetBytesReceived: Int64;
    function GetBytesSend: Int64;
  public
    ActiveConnections: Integer;
    ClientsConnections: Integer;
    ServersConnections: Integer;
    ServersConnectionsWithResponse: Integer;
    TotalConnections: Integer;
    TotalClientsConnections: Integer;
    TotalServersConnections: Integer;
    NodeServersListCount: Integer;
    NodeServersDeleted: Integer;

    class function Empty : TNetStatistics; static;

    property BytesSend: Int64 read GetBytesSend write SetBytesSend;
    property BytesReceived: Int64 read GetBytesReceived write SetBytesReceived;

  end;

implementation

{ TNetStatistics }

class function TNetStatistics.Empty: TNetStatistics;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

function TNetStatistics.GetBytesReceived: Int64;
begin
  Result := FBytesReceived;
end;

function TNetStatistics.GetBytesSend: Int64;
begin
 Result := FBytesSend;
end;

procedure TNetStatistics.SetBytesReceived(const Value: Int64);
begin
  FBytesReceived := Value;
end;

procedure TNetStatistics.SetBytesSend(const Value: Int64);
begin
  FBytesSend := Value;
end;

end.
