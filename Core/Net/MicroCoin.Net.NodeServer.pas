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
  end;

  TNodeServerAddressArray = array of TNodeServer;

  PNodeServerAddress = ^TNodeServer;

const
  CT_TNodeServerAddress_NUL: TNodeServer = (ip: ''; port: 0; last_connection: 0; last_connection_by_server: 0;
    { netConnection: nil; } its_myself: false; last_attempt_to_connect: 0; total_failed_attemps_to_connect: 0;
    is_blacklisted: false; BlackListText: '');

implementation

end.
