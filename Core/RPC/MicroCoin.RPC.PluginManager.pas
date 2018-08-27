unit MicroCoin.RPC.PluginManager;

{

  Copyright (c) MicroCoin Developers 2018

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

}

interface

uses UCrypto, sysutils, Ulog, Math, rtti, UJSONFunctions, Generics.Collections;

type

  RPCMethodAttribute = class(TCustomAttribute)
  strict private
    FRPCMethod : string;
  public
    constructor Create(AMethod : string);
    property RPCMethod : string read FRPCMethod;
  end;

  TRPCResult = record
  private
    FResponse : TPCJSONObject;
    function GetResponse : TPCJSONObject;
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
  rttiContext : TRttiContext;
  method : TRTTIMethod;
  attr : TCustomAttribute;
  rpchandler : THandler;
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
  rttiContext : TRttiContext;
  method : TRTTIMethod;
  attr : TCustomAttribute;
  rpchandler : THandler;
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
        TLog.NewLog(ltdebug, ClassName, Format('Removed RPC handler %s -> %s.%s',
          [RPCMethodAttribute(attr).RPCMethod, AHandler.ClassName, method.Name]));
      end;
    end;
  end;
  FreeAndNil(rttiContext);
end;

{ TRPCResult }

function TRPCResult.GetResponse: TPCJSONObject;
begin
  if FResponse = nil then FResponse := TPCJSONObject.Create;
  Result := FResponse;
end;

end.
