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
| File:       MicroCoin.RPC.Result.pas                                         |
| Created at: 2018-08-31                                                       |
| Purpose:    Result types for RPC methods                                     |
|==============================================================================}
{$ifdef fpc}
 {$mode delphi}
{$endif}

unit MicroCoin.RPC.Result;

interface

uses UJSONFunctions, JclSysUtils;

type

  TRPCResult = record
  private
    FResponse: TPCJSONObject;
    FGuard: ISafeGuard;
    function GetResponse: TPCJSONObject;
  public
    Success: Boolean;
    ErrorCode: integer;
    ErrorMessage: string;
    property Response: TPCJSONObject read GetResponse;
  end;

implementation

{ TRPCResult }

function TRPCResult.GetResponse: TPCJSONObject;
begin
  if FResponse = nil
  then FResponse := Guard(TPCJSONObject.Create, FGuard) as TPCJSONObject;
  Result := FResponse;
end;

end.
