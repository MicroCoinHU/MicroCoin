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
| File:       MicroCoin.RPC.PluginManager.pas                                  |
| Created at: 2018-08-27                                                       |
| Purpose:    RPC plugin managment                                             |
|==============================================================================}

unit MicroCoin.RPC.PluginManager;

{$ifdef FPC}
  {$mode delphi}
{$endif}

interface

uses UCrypto, sysutils, Ulog, MicroCoin.RPC.MethodHandler,
MicroCoin.RPC.Plugin,
{$IFDEF USE_RTTI}{$IFDEF FPC}TypInfo{$else} RTTI, MicroCoin.RPC.RPCMethodAttribute {$endif},{$ENDIF} UJSONFunctions
  {$ifdef USE_GENERICS}, Generics.Collections{$else}, classes{$ENDIF};

type

  TRPCPluginManager = class
  strict private
    class var FHandlers: {$IFDEF USE_GENERICS}TDictionary<string, THandler>{$ELSE}TStringList{$endif};
    class var FWrappers : TInterfaceList;
  public
    class constructor Create;
    class destructor Destroy;
    class procedure RegisterPlugin(AHandler: IRPCPlugin);
    class procedure UnRegisterPlugin(AHandler: IRPCPlugin);
    class function GetHandler(AMethod: string): THandler;
    class procedure RegisterHandler(AMethod : string; AHandler : THandler);
  end;

implementation

class constructor TRPCPluginManager.Create;
begin
  {$ifdef USE_GENERICS}
  FHandlers := TDictionary<string, THandler>.Create();
  {$else}
  FHandlers := TStringList.Create;
  {$endif}
  FWrappers := TInterfaceList.Create;
end;

class destructor TRPCPluginManager.Destroy;
begin
  FWrappers.Clear;
  FWrappers.Free;
  FWrappers := nil;
  FHandlers.Clear;
  FreeAndNil(FHandlers);
end;

class function TRPCPluginManager.GetHandler(AMethod: string): THandler;
var
  xIndex : integer;
  xHandlerWrapper : IHandlerWrapper;
begin
  Result := nil;
  {$IFDEF USE_GENERICS}
    FHandlers.TryGetValue(AMethod, Result);
  {$ELSE}
    xIndex := FHandlers.IndexOf(AMethod);
    if xIndex >-1
    then begin
     if Supports(FHandlers.Objects[xIndex], IHandlerWrapper, xHandlerWrapper)
     then Result := xHandlerWrapper.Handler;
    end;
  {$endif}
end;

class procedure TRPCPluginManager.RegisterHandler(AMethod: string; AHandler: THandler);
var
  xHandlerWrapper : IHandlerWrapper;
begin
  {$IFDEF USE_GENERICS}
    FHandlers.AddOrSetValue(LowerCase(AMethod), AHandler);
  {$ELSE}
    xHandlerWrapper := THandlerWrapper.Create(AHandler);
    FHandlers.AddObject(LowerCase(AMethod), xHandlerWrapper as TObject);
    FWrappers.Add(xHandlerWrapper);
  {$ENDIF}
end;

class procedure TRPCPluginManager.RegisterPlugin(AHandler: IRPCPlugin);
{$IFDEF USE_RTTI}
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
        RegisterHandler(RPCMethodAttribute(attr).RPCMethod, rpchandler);
        TLog.NewLog(ltdebug, ClassName, Format('Registered RPC handler for %s -> %s.%s',
          [RPCMethodAttribute(attr).RPCMethod, AHandler.ClassName, method.Name]));
      end;
    end;
  end;
{$ELSE}
begin
  AHandler.RegisterHandlers();
{$ENDIF}
end;

class procedure TRPCPluginManager.UnRegisterPlugin(AHandler: IRPCPlugin);
{$IFDEF USE_RTTI}
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
        {$ifdef USE_GENERICS}
        FHandlers.Remove(RPCMethodAttribute(attr).RPCMethod);
        {$else}
        FHandlers.Delete(FHandlers.IndexOf(RPCMethodAttribute(attr).RPCMethod));
        {$endif}
        TLog.NewLog(ltdebug, ClassName, Format('Removed RPC handler %s -> %s.%s', [RPCMethodAttribute(attr).RPCMethod,
          AHandler.ClassName, method.Name]));
      end;
    end;
  end;
{$ELSE}
begin
{$ENDIF}
end;

end.
