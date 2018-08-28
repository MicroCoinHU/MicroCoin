unit MicroCoin.RPC.PluginManager;

{
  Copyright 2018 MicroCoin Developers

  Permission is hereby granted, free of charge, to any person obtaining a copy of this
  software and associated documentation files (the "Software"), to deal in the Software
  without restriction, including without limitation the rights to use, copy, modify, merge,
  publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons
  to whom the Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all copies or
  substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
  INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
  PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
  FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
  ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
}

interface

uses UCrypto, sysutils, Ulog, Math, rtti, UJSONFunctions, Generics.Collections;

type

  RPCMethodAttribute = class(TCustomAttribute)
  strict private
    FRPCMethod: string;
  public
    constructor Create(AMethod: string);

    property RPCMethod: string read FRPCMethod;
  end;

  TRPCResult = record
  private
    FResponse: TPCJSONObject;
    function GetResponse: TPCJSONObject;
  public
    Success: Boolean;
    ErrorCode: integer;
    ErrorMessage: string;
    property Response: TPCJSONObject read GetResponse;
  end;

  THandler = function(AParams: TPCJSONObject): TRPCResult of object;

  TRPCManager = class
  strict private
    class var FHandlers: TDictionary<string, THandler>;
  public
    class constructor Create;
    class procedure RegisterPlugin(AHandler: TObject);
    class procedure UnRegisterPlugin(AHandler: TObject);
    class function GetHandler(AMethod: string): THandler;
  end;

  TRPCPlugin = class
  strict protected
    function ToJSONCurrency(microCoins: Int64): Real;
    function ToMicroCoins(jsonCurr: Real): Int64;
  end;

implementation

constructor RPCMethodAttribute.Create(AMethod: string);
begin
  inherited Create;
  FRPCMethod := AMethod;
end;

function TRPCPlugin.ToJSONCurrency(microCoins: Int64): Real;
begin
  Result := RoundTo(microCoins / 10000, -4);
end;

function TRPCPlugin.ToMicroCoins(jsonCurr: Real): Int64;
begin
  Result := Round(jsonCurr * 10000);
end;

class constructor TRPCManager.Create;
begin
  FHandlers := TDictionary<string, THandler>.Create();
end;

class function TRPCManager.GetHandler(AMethod: string): THandler;
begin
  Result := nil;
  FHandlers.TryGetValue(AMethod, Result)
end;

class procedure TRPCManager.RegisterPlugin(AHandler: TObject);
var
  rttiContext: TRttiContext;
  method: TRTTIMethod;
  attr: TCustomAttribute;
  rpchandler: THandler;
begin
  TLog.NewLog(ltInfo, ClassName, Format('Registering RPC Plugin %s', [AHandler.ClassName]));
  rttiContext := TRttiContext.Create;
  for method in rttiContext.GetType(AHandler.ClassType).GetMethods do
  begin
    for attr in method.GetAttributes do
    begin
      if attr is RPCMethodAttribute then
      begin
        TMethod(rpchandler).Code := method.CodeAddress;
        TMethod(rpchandler).Data := AHandler;
        FHandlers.AddOrSetValue(RPCMethodAttribute(attr).RPCMethod, rpchandler);
        TLog.NewLog(ltdebug, ClassName, Format('Registered RPC handler for %s -> %s.%s',
          [RPCMethodAttribute(attr).RPCMethod, AHandler.ClassName, method.Name]));
      end;
    end;
  end;
end;

class procedure TRPCManager.UnRegisterPlugin(AHandler: TObject);
var
  rttiContext: TRttiContext;
  method: TRTTIMethod;
  attr: TCustomAttribute;
  rpchandler: THandler;
begin
  TLog.NewLog(ltInfo, ClassName, Format('UnRegistering RPC Plugin %s', [AHandler.ClassName]));
  rttiContext := TRttiContext.Create;
  for method in rttiContext.GetType(AHandler.ClassType).GetMethods do
  begin
    for attr in method.GetAttributes do
    begin
      if attr is RPCMethodAttribute then
      begin
        TMethod(rpchandler).Code := method.CodeAddress;
        TMethod(rpchandler).Data := AHandler;
        FHandlers.Remove(RPCMethodAttribute(attr).RPCMethod);
        TLog.NewLog(ltdebug, ClassName, Format('Removed RPC handler %s -> %s.%s', [RPCMethodAttribute(attr).RPCMethod,
          AHandler.ClassName, method.Name]));
      end;
    end;
  end;
  FreeAndNil(rttiContext);
end;

{ TRPCResult }

function TRPCResult.GetResponse: TPCJSONObject;
begin
  if FResponse = nil then
    FResponse := TPCJSONObject.Create;
  Result := FResponse;
end;

end.
