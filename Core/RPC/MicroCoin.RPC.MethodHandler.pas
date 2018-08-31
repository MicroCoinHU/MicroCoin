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
| File:       MicroCoin.RPC.MethodHandler.pas                                  |
| Created at: 2018-08-31                                                       |
| Purpose:    Types and defintions for RPC Method handlers                     |
|==============================================================================}

unit MicroCoin.RPC.MethodHandler;

interface

uses UJsonFunctions, MicroCoin.RPC.Result;

type

  THandler = function(AParams: TPCJSONObject): TRPCResult of object;

  {$ifndef USE_GENERICS}
    IHandlerWrapper = interface
     ['{C5C7E236-D8D3-4AEA-9FD1-13133F34D41E}']
     function GetHandler : THandler;
     property Handler : THandler read GetHandler;
    end;

    THandlerWrapper = class(TInterfacedObject, IHandlerWrapper)
    private
     FHandler : THandler;
    public
     function GetHandler : THandler;
     constructor Create(AHandler : THandler);
     property Handler : THandler read GetHandler write FHandler;
    end;
  {$endif}


implementation

constructor THandlerWrapper.Create(AHandler: THandler);
begin
  Handler := AHandler;
end;

function THandlerWrapper.GetHandler: THandler;
begin
  Result := FHandler;
end;

end.
