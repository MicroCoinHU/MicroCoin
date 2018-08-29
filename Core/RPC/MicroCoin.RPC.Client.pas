unit MicroCoin.RPC.Client;

interface

uses UJsonFunctions, UThread, SysUtils, Variants, UTCPIP, Classes, ULog, Windows, MicroCoin.Common;

type

  TProcessJSONObjectEvent = Procedure (json : TPCJSONObject; method : String) of object;

  TJSONRPCTcpIpClient = Class(TBufferedNetTcpIpClient)
  private
    FLastId : Cardinal;
    FLockProcessBuffer : TPCCriticalSection;
    FReceivedBuffer : TBytes;
    FLockReceivedBuffer : TPCCriticalSection;
    FPendingResponseMessages : TPCThreadList;
  protected
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure SendJSONRPCErrorResponse(const id : Variant; const error : String);
    Procedure SendJSONRPCResponse(result : TPCJSONObject; const id : Variant);
    Procedure SendJSONRPCMethod(const method : String; params : TPCJSONList; const id : Variant);
    Function SendJSONRPCMethodAndWait(const method : String; params : TPCJSONList; MaxWaitMiliseconds : Cardinal; resultObject : TPCJSONObject; processEventOnInvalid : TProcessJSONObjectEvent = Nil) : Boolean;
    Function DoProcessBuffer(SenderThread : TPCThread; MaxWaitMiliseconds : Cardinal; DeleteBufferOnExit : Boolean; var ResponseMethod : String; var jsonObject : TPCJSONObject) : Boolean;
    Function GetNewId : Cardinal;
  End;

  PPendingResponseMessage = ^TPendingResponseMessage;
  TPendingResponseMessage = Record
       sendDateTime : TDateTime;
       maxDateTime : TDateTime;
       id : Integer;
       method : String;
     end;

implementation

 constructor TJSONRPCTcpIpClient.Create(AOwner: TComponent);
begin
  inherited;
  FLastId := 1;
  SetLength(FReceivedBuffer,0);
  FLockProcessBuffer := TPCCriticalSection.Create('TJSONRPCTcpIpClient_LockProcessBuffer');
  FLockReceivedBuffer := TPCCriticalSection.Create('TJSONRPCTcpIpClient_LockReceivedBuffer');
  FPendingResponseMessages := TPCThreadList.Create('TJSONRPCTcpIpClient_PendingResponseMessages');
end;

destructor TJSONRPCTcpIpClient.Destroy;
var P : PPendingResponseMessage;
  l : TList;
  i : Integer;
begin
  l := FPendingResponseMessages.LockList;
  try
    for i:=0 to l.count-1 do begin
      P:=l[i];
      Dispose(P);
    end;
    l.clear;
  finally
    FPendingResponseMessages.UnlockList;
  end;
  FreeAndNil(FLockReceivedBuffer);
  FreeAndNil(FLockProcessBuffer);
  SetLength(FReceivedBuffer,0);
  FreeAndNil(FPendingResponseMessages);
  inherited;
end;

function TJSONRPCTcpIpClient.DoProcessBuffer(SenderThread : TPCThread; MaxWaitMiliseconds : Cardinal; DeleteBufferOnExit : Boolean; var ResponseMethod : String; var jsonObject : TPCJSONObject) : Boolean;
var last_bytes_read : Integer;
  jsonData : TPCJSONData;
  tc : Cardinal;
  ms : TMemoryStream;
  i,lasti : Integer;
  continue : Boolean;
  procedure FlushBufferPendingMessages(doSearchId : Boolean; idValue : Integer);
  var l : TList;
    i : Integer;
    P : PPendingResponseMessage;
  Begin
    l := FPendingResponseMessages.LockList;
    Try
      for i := l.count-1 downto 0 do begin
        P := l[i];
        if (doSearchId) And (idValue=P^.id) then begin
          ResponseMethod:=P^.method;
          Dispose(P);
          l.Delete(i);
        end else if (P^.maxDateTime<now) then begin
          TLog.NewLog(lterror,Classname,'Deleting a Pending response message id:'+inttostr(P^.id)+' method:'+P^.method);
          Dispose(P);
          l.Delete(i);
        end;
      end;
    finally
      FPendingResponseMessages.UnlockList;
    end;
  end;
var PartialBuffer : TBytes;
  Function ProcessPartialBuffer : Boolean;
  Var i,istart : Integer;
    aux : TBytes;
  begin
    result := false;
    i := 0; istart :=0;
    while (i<=high(FReceivedBuffer)) do begin
      if FReceivedBuffer[i]<32 then begin
        if i=istart then inc(istart)
        else break;
      end else begin
      end;
      inc(i);
    end;
    if (i>0) And (i>istart) And (i<=High(FReceivedBuffer)) then begin
      SetLength(PartialBuffer,i-istart);
      move(FReceivedBuffer[istart],PartialBuffer[0],i-istart);
      // Inc i until valid char
      while (i<=High(FReceivedBuffer)) And (FReceivedBuffer[i]<32) do inc(i);
      // i is the first valid pos for next buffer
      if i<=High(FReceivedBuffer) then begin
        setlength(aux,length(FReceivedBuffer)-i);
        move(FReceivedBuffer[i],aux[0],length(aux));
        SetLength(FReceivedBuffer,length(aux));
        move(aux[0],FReceivedBuffer[0],length(aux));
      end else begin
        // empty next buffer
        SetLength(FReceivedBuffer,0);
      end;
      Result := true;
    end;
  end;
var islocked : Boolean;
  ps : AnsiString;
  isa: integer;
begin
  Result := false;
  ResponseMethod := '';
  tc := GetTickCount;
  Repeat
    islocked := FLockProcessBuffer.TryEnter;
  until (islocked) Or ((GetTickCount>(tc+MaxWaitMiliseconds)) And (MaxWaitMiliseconds<>0));
  If Not islocked then exit;
  try
    if Assigned(SenderThread) then continue := Not SenderThread.Terminated
    else continue := true;
    while (Connected) And ((GetTickCount<=(tc+MaxWaitMiliseconds)) Or (MaxWaitMiliseconds=0)) And (continue) do begin
      last_bytes_read := 0;
      ms := ReadBufferLock;
      try
        if (ms.Size)>0 then begin
          lasti := length(FReceivedBuffer);
          setLength(FReceivedBuffer,length(FReceivedBuffer)+ms.Size);
          CopyMemory(@FReceivedBuffer[lasti],ms.Memory,ms.Size);
          last_bytes_read := ms.Size;
          ms.Size := 0;
        end;
      finally
        ReadBufferUnlock;
      end;
//      ps := '{"id":null,"method":"miner-notify","params":[{"block":1610,"version":2,"part1":"4A060000CA0220001D75D79906F4B238AE40BDD54D26C2290708FA24E7F54E40669FDAAF8FEEA19520001C091DAABADC4C639704ADD37B164818DEFF0EB781C86BBE21CBDA5DF93F525340420F000000000002000200A370AE1D","payload_start":"506574696B6520766167796F6B","part3":"1C9EC89F0EE1435D1012514AF42618A3963DE58EBEBA15991E86C6654373B1A7E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B85500000000","target":497971363,"target_pow":"00000005463D7000000000000000000000000000000000000000000000000000","timestamp":1515221255}]}';
//      setLength(PartialBuffer,Length(ps));
//      for isa:=1 to Length(ps) do
//          PartialBuffer[isa] := Byte(ps[isa]);
      If ProcessPartialBuffer then begin
        // Decode
        jsonData := TPCJSONData.ParseJSONValue(PartialBuffer);
        if Assigned(jsonData) then begin
          Try
            if jsonData is TPCJSONObject then begin
              jsonObject.Assign(jsonData);
              If (Not jsonObject.IsNull('id')) And (jsonObject.IndexOfName('method')<0) then begin
                // Is a Response!
                FlushBufferPendingMessages(true,jsonObject.AsInteger('id',0));
              end;
              Result := true;
              exit;
            end else begin
              TLog.NewLog(lterror,ClassName,'Invalid JSON class: '+jsonData.ClassName+' json: '+TBytesToString(PartialBuffer));
            End;
          Finally
            jsonData.Free; // Memory leak on 1.5.0
          end;
        end else begin
          TLog.NewLog(lterror,ClassName,Format('Read %d bytes but no valid JSON inside: %s',[last_bytes_read,TBytesToString(PartialBuffer)]));
        end;
      end;
      sleep(1);
      if Assigned(SenderThread) then continue := Not SenderThread.Terminated
      else continue := true;
    end;
    if (length(FReceivedBuffer)>0) And (DeleteBufferOnExit) then begin
      TLog.NewLog(lterror,ClassName,AnsiString( Format('Deleting %d bytes from buffer after waiting %d milis: %s',[length(FReceivedBuffer),MaxWaitMiliseconds,TBytesToString(FReceivedBuffer)])));
      SetLength(FReceivedBuffer,0);
    end;
  finally
    FlushBufferPendingMessages(false,0);
    FLockProcessBuffer.Release;
  end;
end;

function TJSONRPCTcpIpClient.GetNewId: Cardinal;
begin
  FLockReceivedBuffer.Acquire;
  try
    inc(FLastId);
    Result := FLastId;
  finally
    FLockReceivedBuffer.Release;
  end;
end;

procedure TJSONRPCTcpIpClient.SendJSONRPCErrorResponse(const id: Variant; const error: String);
Var response : TPCJSONObject;
  stream : TMemoryStream;
  b : Byte;
begin
  TLog.NewLog(lterror,ClassName,'Sending Error JSON RPC id ('+VarToStr(id)+') : '+error);
  response := TPCJSONObject.Create;
  Try
    response.GetAsVariant('result').Value := Null;
    response.GetAsVariant('error').Value := error;
    response.GetAsVariant('id').Value := id;
    stream := TMemoryStream.Create;
    try
      response.SaveToStream(stream);
      b := 13;
      stream.Write(b,1);
      b := 10;
      stream.Write(b,1);
      b := 0;
      stream.Write(b,1);
      stream.Position := 0;
      WriteBufferToSend(stream);
    finally
      stream.Free;
    end;
  Finally
    response.Free;
  End;
end;

procedure TJSONRPCTcpIpClient.SendJSONRPCMethod(const method: String; params: TPCJSONList; const id: Variant);
Var json : TPCJSONObject;
  stream : TMemoryStream;
  b : Byte;
  P : PPendingResponseMessage;
  l : TList;
begin
  json := TPCJSONObject.Create;
  Try
    json.GetAsVariant('id').Value := id;
    json.GetAsVariant('method').Value := method;
    if Assigned(params) then begin
      If params is TPCJSONObject then begin
        json.GetAsArray('params').GetAsObject(0).Assign(params);
      end else if params is TPCJSONArray then begin
        json.GetAsArray('params').Assign(params);
      end;
    end;
    if (Not VarIsNull(id)) then begin
      new(P);
      P^.id:=id;
      P^.sendDateTime:=Now;
      P^.maxDateTime:=Now + encodetime(0,0,30,0);
      P^.method:=method;
      FPendingResponseMessages.Add(P);
    end;
    TLog.NewLog(ltInfo,Classname,'Sending JSON: '+json.ToJSON(false));
    stream := TMemoryStream.Create;
    try
      json.SaveToStream(stream);
      b := 13;
      stream.Write(b,1);
      b := 10;
      stream.Write(b,1);
      stream.Position := 0;
      WriteBufferToSend(stream);
    finally
      stream.Free;
    end;
  Finally
    json.Free;
  End;
end;

function TJSONRPCTcpIpClient.SendJSONRPCMethodAndWait(const method: String; params: TPCJSONList; MaxWaitMiliseconds: Cardinal; resultObject : TPCJSONObject; processEventOnInvalid : TProcessJSONObjectEvent = Nil) : Boolean;
Var nId : Cardinal;
  tc,maxw : Cardinal;
  json : TPCJSONObject;
  rm : String;
begin
  Result := false;
  FLockProcessBuffer.Acquire;
  try
    nId := GetNewId;
    SendJSONRPCMethod(method,params,nId);
    tc := GetTickCount;
    json := TPCJSONObject.Create;
    Try
      repeat
        maxw := MaxWaitMiliseconds - (GetTickCount - tc);
        if maxw<1 then maxw := 1
        else if maxw>10000 then maxw := 10000;
        If DoProcessBuffer(nil,maxw,true,rm,json) then begin
          If json.AsCardinal('id',0)=nId then begin
            resultObject.Assign(json);
            Result := true;
          end else begin
            TLog.NewLog(ltdebug,classname,'Received a unexpected JSON while waiting for response Id:'+inttostr(nId)+' Received:'+json.ToJSON(false));
            If Assigned(processEventOnInvalid) then begin
              TLog.NewLog(ltdebug,classname,'Sending to process unexpected JSON:'+json.ToJSON(false));
              processEventOnInvalid(json,rm);
            end else TLog.NewLog(lterror,Classname,'Lost JSON message! '+json.ToJSON(false));
          end;
        end;
      until (Result) Or (GetTickCount > (tc+MaxWaitMiliseconds));
    finally
      json.free;
    end;
    if (Not Result) then begin
      TLog.NewLog(lterror,classname,'Not received a JSON response Id:'+inttostr(nId)+' for method:'+method);
    end;
  finally
    FLockProcessBuffer.Release;
  end;
end;

procedure TJSONRPCTcpIpClient.SendJSONRPCResponse(result: TPCJSONObject; const id: Variant);
Var response : TPCJSONObject;
  stream : TMemoryStream;
  b : Byte;
begin
  response := TPCJSONObject.Create;
  Try
    If Assigned(Result) then response.GetAsObject('result').Assign(result)
    else response.GetAsVariant('result').Value:=null;
    response.GetAsVariant('error').Value := Null;
    response.GetAsVariant('id').Value := id;
    stream := TMemoryStream.Create;
    try
      response.SaveToStream(stream);
      b := 13;
      stream.Write(b,1);
      b := 10;
      stream.Write(b,1);
      stream.Position := 0;
      WriteBufferToSend(stream);
    finally
      stream.Free;
    end;
  Finally
    response.Free;
  End;
end;

end.
