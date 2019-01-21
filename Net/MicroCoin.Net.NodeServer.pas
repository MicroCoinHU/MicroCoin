unit MicroCoin.Net.NodeServer;
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

uses classes, MicroCoin.Common.Stream;

type
  TNodeServer = record
    ip: AnsiString;
    port: Word;
    last_connection: Cardinal;
    last_connection_by_server: Cardinal;
    netConnection: TObject;
    its_myself: Boolean;
    last_attempt_to_connect: TDateTime;
    total_failed_attemps_to_connect: Integer;
    is_blacklisted: Boolean; // Build 1.4.4
    BlackListText: string;
    class function LoadFromStream(AStream : TStream) : TNodeServer; static;
    class function Empty : TNodeServer; static;
  end;

  TNodeServerAddressArray = array of TNodeServer;

  PNodeServerAddress = ^TNodeServer;

implementation

{ TNodeServer }

class function TNodeServer.Empty: TNodeServer;
begin
  Result.ip := '';
  Result.port := 0;
  Result.last_connection := 0;
  Result.last_connection_by_server := 0;
  Result.netConnection := nil;
  Result.its_myself := false;
  Result.last_attempt_to_connect := 0;
  Result.total_failed_attemps_to_connect := 0;
  Result.is_blacklisted := false;
  Result.BlackListText := '';
end;

class function TNodeServer.LoadFromStream(AStream: TStream): TNodeServer;
begin
  Result := Empty;
  AStream.ReadAnsiString(Result.ip);
  AStream.Read(Result.port, 2);
  AStream.Read(Result.last_connection_by_server, 4);
end;

end.
