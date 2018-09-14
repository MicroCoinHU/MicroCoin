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
| File:       MicroCoin.BlockChain.Base.pas
| Created at: 2018-09-11
| Purpose:    Base blockchain manager class
|==============================================================================}

unit MicroCoin.BlockChain.Base;


{$ifdef FPC}
  {$mode delphi}
{$endif}

interface

uses Classes, MicroCoin.Account.Storage, MicroCoin.BlockChain.BlockHeader;

type
  TBlockManagerBase = class(TComponent)
  protected
    function GetBlocksCount: Cardinal; virtual; abstract;
    function GetAccountStorage: TAccountStorage; virtual; abstract;
    function GetLastOperationBlock: TBlockHeader; virtual; abstract;
  public
    function LoadAccountsFromStream(Stream: TStream; useSecureLoad: Boolean; var errors: AnsiString): Boolean; virtual; abstract;
    property AccountStorage: TAccountStorage read GetAccountStorage;
    property LastOperationBlock: TBlockHeader read GetLastOperationBlock;
    property BlocksCount: Cardinal read GetBlocksCount;
  end;

implementation

{ TBank }

end.
