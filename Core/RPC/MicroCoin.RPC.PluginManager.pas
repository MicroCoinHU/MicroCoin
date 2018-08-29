unit MicroCoin.RPC.PluginManager;

{==============================================================================|
| This unit part of MicroCoin                                                  |
| Copyright (c) 2018 MicroCoin Developers                                      |
|==============================================================================|
| Permission is hereby granted, free of charge, to any person obtaining a copy |
| of this software and associated documentation files (the "Software"), to     |
| deal in the Software without restriction, including without limitation the   |
| rights to use, copy, modify, merge, publish, distribute, sublicense, and/or  |
| sell opies of the Software, and to permit persons to whom the Software is    |
| furnished to do so, subject to the following conditions:                     |
|------------------------------------------------------------------------------|
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
| File: MicroCoin.RPC.PluginManager.pas                                        |
| Purpose: Classes and utilites for managing the JSON-RPC plugins              |
|==============================================================================}

{$ifdef FPC}
  {$mode delphi}
{$endif}

interface

uses UCrypto, sysutils, Ulog,
{$IFDEF USE_RTTI}{$IFDEF FPC}TypInfo{$else} RTTI {$endif},{$ENDIF} UJSONFunctions
  {$ifdef USE_GENERICS}, Generics.Collections{$else}, classes{$ENDIF};

type
  {$ifdef FPC}
    TCustomAttribute = class
    end;
  {$endif}

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

  {$ifndef USE_GENERICS}
    THandlerWrapper = class
    public
      Handler : THandler;
      constructor Create(AHandler : THandler);
    end;
  {$endif}

  IRPCPlugin = interface
    procedure RegisterHandlers;
  end;

  TRPCManager = class
  strict private
    class var FHandlers: {$IFDEF USE_GENERICS}TDictionary<string, THandler>{$ELSE}TStringList{$endif};
  public
    class constructor Create;
    class procedure RegisterPlugin(AHandler: IRPCPlugin);
    class procedure UnRegisterPlugin(AHandler: IRPCPlugin);
    class function GetHandler(AMethod: string): THandler;
    class procedure RegisterHandler(AMethod : string; AHandler : THandler);
  end;

  TRPCPlugin = class(TInterfacedObject, IRPCPlugin)
  strict protected
    procedure RegisterHandlers; virtual; abstract;
  end;

implementation

constructor RPCMethodAttribute.Create(AMethod: string);
begin
  inherited Create;
  FRPCMethod := AMethod;
end;

class constructor TRPCManager.Create;
begin
  {$ifdef USE_GENERICS}
  FHandlers := TDictionary<string, THandler>.Create();
  {$else}
  FHandlers := TStringList.Create;
  {$endif}
end;

class function TRPCManager.GetHandler(AMethod: string): THandler;
var
  index : integer;
begin
  Result := nil;
  {$IFDEF USE_GENERICS}
    FHandlers.TryGetValue(AMethod, Result);
  {$ELSE}
    index := FHandlers.IndexOf(AMethod);
    if index >-1
    then Result := THandlerWrapper(FHandlers.Objects[index]).Handler;
  {$endif}
end;

class procedure TRPCManager.RegisterHandler(AMethod: string; AHandler: THandler);
begin
  {$IFDEF USE_GENERICS}
    FHandlers.AddOrSetValue(LowerCase(AMethod), AHandler);
  {$ELSE}
    FHandlers.AddObject(LowerCase(AMethod), THandlerWrapper.Create(AHandler));
  {$ENDIF}
end;

class procedure TRPCManager.RegisterPlugin(AHandler: IRPCPlugin);
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

class procedure TRPCManager.UnRegisterPlugin(AHandler: IRPCPlugin);
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

{ TRPCResult }

function TRPCResult.GetResponse: TPCJSONObject;
begin
  if FResponse = nil then
    FResponse := TPCJSONObject.Create;
  Result := FResponse;
end;

{ THandlerWrapper }

constructor THandlerWrapper.Create(AHandler: THandler);
begin
  Handler := AHandler;
end;

end.
