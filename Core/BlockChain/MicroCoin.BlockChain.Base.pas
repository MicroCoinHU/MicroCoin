unit MicroCoin.BlockChain.Base;

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
