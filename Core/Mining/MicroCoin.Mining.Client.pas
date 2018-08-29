unit MicroCoin.Mining.Client;

interface

uses MicroCoin.RPC.Client, UCrypto, SysUtils, Classes, UJsonFunctions, ULog, Variants,
  MicroCoin.Mining.Common, MicroCoin.BlockChain.Protocol, UConst;

type

  TPoolType = (ptNone, ptIdentify);

  TMinerClient = class(TJSONRPCTcpIpClient)
  private
    FMinerValuesForWork: TMinerValuesForWork;
    FOnMinerMustChangeValues: TNotifyEvent;
    FPassword: string;
    FFinalMinerName: string;
    FPoolType: TPoolType;
    FStratum_Target_PoW: TRawBytes;
    FUserName: string;
    procedure SetMinerValuesForWork(const Value: TMinerValuesForWork);
  protected
    procedure DoOnConnect; override;
  public
    constructor Create(AOwner: TComponent); override;
    property OnMinerMustChangeValues: TNotifyEvent read FOnMinerMustChangeValues write FOnMinerMustChangeValues;
    property MinerValuesForWork: TMinerValuesForWork read FMinerValuesForWork write SetMinerValuesForWork;
    procedure SubmitBlockFound(const MinerValuesToGenerateBlock: TMinerValuesForWork; const Payload: TRawBytes;
      Timestamp, NOnce: Cardinal);
    procedure DoProcessJSONObject(json: TPCJSONObject; ResponseMethod: string);
    property PoolType: TPoolType read FPoolType write FPoolType;
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property FinalMinerName: string read FFinalMinerName;
    property Stratum_Target_PoW: TRawBytes read FStratum_Target_PoW;
  end;

implementation

constructor TMinerClient.Create(AOwner: TComponent);
begin
  FMinerValuesForWork := CT_TMinerValuesForWork_NULL;
  FPoolType := ptNone;
  FUserName := '';
  FPassword := '';
  FFinalMinerName := '';
  FStratum_Target_PoW := '';
  inherited;
end;

procedure TMinerClient.DoOnConnect;
var
  params: TPCJSONArray;
  resultObject: TPCJSONObject;
  s: string;
  raws: TRawBytes;
  i: Integer;
begin
  inherited DoOnConnect;
  if FPoolType = ptIdentify then
  begin
    // Pool initialization
    params := TPCJSONArray.Create;
    resultObject := TPCJSONObject.Create;
    try
      params.GetAsVariant(0).Value := UserName;
      params.GetAsVariant(1).Value := Password;
      if SendJSONRPCMethodAndWait(CT_PoolMining_Method_STRATUM_MINING_AUTHORIZE, params, 1000, resultObject, nil) then
      begin
        TLog.NewLog(ltInfo, Classname, CT_PoolMining_Method_STRATUM_MINING_AUTHORIZE + ' response: ' +
          resultObject.ToJSON(false));
        // Now subscribe
        params.Clear;
        resultObject.Clear;
        if SendJSONRPCMethodAndWait(CT_PoolMining_Method_STRATUM_MINING_SUBSCRIBE, params, 1000, resultObject, nil) then
        begin
          //
          TLog.NewLog(ltInfo, Classname, CT_PoolMining_Method_STRATUM_MINING_SUBSCRIBE + ' response: ' +
            resultObject.ToJSON(false));
          // Decode response
          if (resultObject.IsNull('error')) then
          begin
            s := resultObject.GetAsArray('result').GetAsArray(0).GetAsArray(0).GetAsVariant(0).AsString('');
            if (s <> 'mining.nonce') then
              raise Exception.Create('Not a mining.nonce');
            s := resultObject.GetAsArray('result').GetAsVariant(1).AsString('');
            raws := TCrypto.HexaToRaw(s);
            if (length(s) > 0) and (length(raws) = 0) then
            begin
              TLog.NewLog(lterror, Classname, 'Invalid value to assign as a Miner name. Not hexadecimal ' + s);
              FFinalMinerName := '';
            end
            else
            begin
              FFinalMinerName := raws;
              for i := 1 to length(raws) do
              begin
                if not(raws[i] in [#32 .. #254]) then
                begin
                  TLog.NewLog(lterror, Classname, 'Invalid proposed miner name. Value at pos ' + inttostr(i) +
                    ' is not #24..#254: ' + inttostr(Integer(raws[i])));
                  FFinalMinerName := '';
                  break;
                end;
              end;
            end;
            TLog.NewLog(ltInfo, Classname, 'Final miner name: "' + FFinalMinerName + '" (Length ' +
              inttostr(length(FFinalMinerName)));
          end;
        end
        else
          raise Exception.Create('Not response to "' + CT_PoolMining_Method_STRATUM_MINING_SUBSCRIBE +
            '" method for user "' + UserName + '"');
      end
      else
        raise Exception.Create('Not response to "' + CT_PoolMining_Method_STRATUM_MINING_AUTHORIZE +
          '" method for user "' + UserName + '"');
    finally
      resultObject.free;
      params.free;
    end;
  end;
end;

procedure TMinerClient.DoProcessJSONObject(json: TPCJSONObject; ResponseMethod: string);
var
  method: string;
  id_value: Variant;
  i: Integer;
  params_as_object, pobject: TPCJSONObject;
  params_as_array: TPCJSONArray;
  params: TPCJSONData;
  mvfw: TMinerValuesForWork;
  prev_pow, proposed_pow: TRawBytes;
begin
  TLog.NewLog(ltInfo, Classname, 'Received JSON: ' + json.ToJSON(false));
  params := nil;
  params_as_object := nil;
  params_as_array := nil;
  if (ResponseMethod <> '') then
  begin
    method := ResponseMethod;
    i := json.IndexOfName('result');
    if (i >= 0) then
    begin
      params := json.Items[i];
    end;
    TLog.NewLog(ltInfo, Classname, 'Received response method:' + ResponseMethod + ' JSON:' + json.ToJSON(false));
  end
  else
  begin
    method := json.AsString('method', '');
    i := json.IndexOfName('params');
    if (i >= 0) then
    begin
      params := json.Items[i];
    end;
  end;
  if Assigned(params) then
  begin
    if (params is TPCJSONNameValue) then
    begin
      if (TPCJSONNameValue(params).Value is TPCJSONObject) then
        params_as_object := TPCJSONObject(TPCJSONNameValue(params).Value)
      else if (TPCJSONNameValue(params).Value is TPCJSONArray) then
        params_as_array := TPCJSONArray(TPCJSONNameValue(params).Value);
    end;
  end;
  i := json.IndexOfName('id');
  if i < 0 then
  begin
    id_value := Null;
  end
  else
  begin
    id_value := json.GetAsVariant('id').Value;
  end;
  if method = CT_PoolMining_Method_MINER_NOTIFY then
  begin
    if Assigned(params_as_array) then
      pobject := params_as_array.GetAsObject(0)
    else
      pobject := params_as_object;
    if Assigned(pobject) then
    begin
      mvfw := CT_TMinerValuesForWork_NULL;
      mvfw.block := pobject.AsInteger('block', 0);
      mvfw.version := pobject.AsInteger('version', 0);
      mvfw.part1 := TCrypto.HexaToRaw(pobject.AsString('part1', ''));
      mvfw.payload_start := TCrypto.HexaToRaw(pobject.AsString('payload_start', ''));
      mvfw.part3 := TCrypto.HexaToRaw(pobject.AsString('part3', ''));
      mvfw.target := pobject.AsInteger('target', 0);
      mvfw.Timestamp := pobject.AsInteger('timestamp', 0);
      mvfw.part1 := TCrypto.HexaToRaw(pobject.AsString('part1', ''));
      mvfw.target_pow := TCrypto.HexaToRaw(pobject.AsString('target_pow', ''));
      if FPoolType = ptIdentify then
      begin
        mvfw.jobid := pobject.AsString('jobid', '');
      end;
      if (not VarIsNull(id_value)) and (ResponseMethod = '') then
      begin
        SendJSONRPCResponse(pobject, id_value);
      end;
      MinerValuesForWork := mvfw;
    end
    else
      TLog.NewLog(lterror, Classname, 'method ' + method + ' without JSON object ' + params.ToJSON(false));
  end;
end;

procedure TMinerClient.SetMinerValuesForWork(const Value: TMinerValuesForWork);
var
  _t: Cardinal;
  _t_pow: TRawBytes;
begin
  FMinerValuesForWork := Value;
  if FStratum_Target_PoW <> '' then
  begin
    FMinerValuesForWork.target := TMicroCoinProtocol.TargetToCompact(FStratum_Target_PoW);
    FMinerValuesForWork.target_pow := TMicroCoinProtocol.TargetFromCompact(FMinerValuesForWork.target);
  end
  else
  begin
    // Check that target and target_pow are equal!
    _t_pow := TMicroCoinProtocol.TargetFromCompact(FMinerValuesForWork.target);
    if (length(FMinerValuesForWork.target_pow) = 32) then
    begin
      _t := TMicroCoinProtocol.TargetToCompact(FMinerValuesForWork.target_pow);
      if (FMinerValuesForWork.target < CT_MinCompactTarget) then
      begin
        // target has no valid value... assigning compact_target!
        FMinerValuesForWork.target := TMicroCoinProtocol.TargetToCompact(_t_pow);
      end
      else if (_t_pow <> FMinerValuesForWork.target_pow) or (_t <> FMinerValuesForWork.target) then
      begin
        TLog.NewLog(lterror, Classname, 'Received bad values for target and target_pow!');
        if (FMinerValuesForWork.target < CT_MinCompactTarget) then
        begin
          FMinerValuesForWork.target_pow := TMicroCoinProtocol.TargetFromCompact(FMinerValuesForWork.target);
        end
        else
        begin
          FMinerValuesForWork.target := TMicroCoinProtocol.TargetToCompact(_t_pow);
        end;
      end;
    end
    else
    begin
      if (FMinerValuesForWork.target < CT_MinCompactTarget) then
      begin
        // target_pow has no value... assigning target!
        FMinerValuesForWork.target_pow := TMicroCoinProtocol.TargetFromCompact(FMinerValuesForWork.target);
      end
      else
      begin
        // Invalid target and compact_target
        FMinerValuesForWork.target := CT_TMinerValuesForWork_NULL.target;
        FMinerValuesForWork.target_pow := CT_TMinerValuesForWork_NULL.target_pow;
      end;
    end;
  end;
  if (FPoolType = ptIdentify) and (FFinalMinerName <> '') then
    FMinerValuesForWork.payload_start := FFinalMinerName;
  if Assigned(FOnMinerMustChangeValues) then
    FOnMinerMustChangeValues(Self);
end;

procedure TMinerClient.SubmitBlockFound(const MinerValuesToGenerateBlock: TMinerValuesForWork; const Payload: TRawBytes;
  Timestamp, NOnce: Cardinal);
var
  json, resultJSON: TPCJSONObject;
  nOnceAsSignedInt: Int32;
begin
  json := TPCJSONObject.Create;
  try
    nOnceAsSignedInt := NOnce;
    if FPoolType = ptIdentify then
    begin
      json.GetAsVariant('jobid').Value := MinerValuesToGenerateBlock.jobid;
    end;
    json.GetAsVariant('payload').Value := TCrypto.ToHexaString(Payload);
    json.GetAsVariant('timestamp').Value := Timestamp;
    json.GetAsVariant('nonce').Value := nOnceAsSignedInt;
    resultJSON := TPCJSONObject.Create;
    try
      SendJSONRPCMethod(CT_PoolMining_Method_MINER_SUBMIT, json, GetNewId);
    finally
      resultJSON.free;
    end;
  finally
    json.free;
  end;
end;

end.
