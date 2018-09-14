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
| File:       MicroCoin.Net.INetNotificationSource.pas                         |
| Created at: 2018-08-31                                                       |
| Purpose:    Interface declaration for network notification sources           |
|==============================================================================}

unit MicroCoin.Net.INetNotificationSource;

interface

uses Classes;

type
  INetNotificationSource = interface
    ['{9CC4B8C1-2D7A-45B9-978B-DE92356A5818}']
    function GetOnNetConnectionsUpdated: TNotifyEvent;
    function GetOnNodeServersUpdated: TNotifyEvent;
    function GetOnBlackListUpdated: TNotifyEvent;
    function GetOnReceivedHelloMessage: TNotifyEvent;
    function GetOnStatisticsChanged: TNotifyEvent;

    property OnNetConnectionsUpdated: TNotifyEvent read GetOnNetConnectionsUpdated;
    property OnNodeServersUpdated: TNotifyEvent read GetOnNodeServersUpdated;
    property OnBlackListUpdated: TNotifyEvent read GetOnBlackListUpdated;
    property OnReceivedHelloMessage: TNotifyEvent read GetOnReceivedHelloMessage;
    property OnStatisticsChanged: TNotifyEvent read GetOnStatisticsChanged;
  end;


implementation

end.
