unit MicroCoin.Transaction.ChangeKey;

{
  This unit contains code from PascalCoin:

  Copyright (c) Albert Molina 2016 - 2018 original code from PascalCoin https://pascalcoin.org/

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

}

interface

uses MicroCoin.Transaction.Base, MicroCoin.Transaction.Transaction,
MicroCoin.Account.AccountKey,
Sysutils, classes, UAccounts, UCrypto, ULog, UConst, MicroCoin.Transaction.Manager;

 type

  TOpChangeKeyData = Record
    account_signer,
    account_target: Cardinal;
    n_operation : Cardinal;
    fee: UInt64;
    payload: TRawBytes;
    public_key: TECDSA_Public;
    new_accountkey: TAccountKey;
    sign: TECDSA_SIG;
  End;

  TChangeKeyTransaction = Class(TTransaction)
  private
    FData : TOpChangeKeyData;
  protected
    procedure InitializeData; override;
    function SaveToStream(Stream: TStream; SaveExtendedData : Boolean): Boolean; override;
    function LoadFromStream(Stream: TStream; LoadExtendedData : Boolean): Boolean; override;
  public
    Class Function GetOperationHashToSign(const op : TOpChangeKeyData) : TRawBytes;
    Class Function DoSignOperation(key : TECPrivateKey; var op : TOpChangeKeyData) : Boolean;
    function GetOpType : Byte; override;

    function GetBufferForOpHash(UseProtocolV2 : Boolean): TRawBytes; override;
    function ApplyTransaction(AccountTransaction : TPCSafeBoxTransaction; var errors : AnsiString) : Boolean; override;
    function GetAmount : Int64; override;
    function GetFee : UInt64; override;
    function GetPayload : TRawBytes; override;
    function GetSignerAccount : Cardinal; override;
    function GetDestinationAccount : Int64; override;
    function GetNumberOfTransactions : Cardinal; override;
    procedure AffectedAccounts(list : TList); override;
    Constructor Create(account_signer, n_operation, account_target: Cardinal; key:TECPrivateKey; new_account_key : TAccountKey; fee: UInt64; payload: TRawBytes);
    function GetTransactionData(Block: Cardinal; Affected_account_number: Cardinal; var TransactionData: TTransactionData): Boolean; override;
    Property Data : TOpChangeKeyData read FData;
    Function toString : String; Override;
  End;

  TChangeKeySignedTransaction = Class(TChangeKeyTransaction)
  public
    function GetOpType : Byte; override;
    function GetTransactionData(Block: Cardinal;
      Affected_account_number: Cardinal;
      var TransactionData: TTransactionData): Boolean; override;
  end;

const
  CT_TOpChangeKeyData_NUL : TOpChangeKeyData = (account_signer:0;account_target:0;n_operation:0;fee:0;payload:'';public_key:(EC_OpenSSL_NID:0;x:'';y:'');new_accountkey:(EC_OpenSSL_NID:0;x:'';y:'');sign:(r:'';s:''));

implementation

procedure TChangeKeyTransaction.AffectedAccounts(list: TList);
begin
  list.Add(TObject(FData.account_signer));
  if (FData.account_target<>FData.account_signer) then list.Add(TObject(FData.account_target));
end;

constructor TChangeKeyTransaction.Create(account_signer, n_operation, account_target: Cardinal; key:TECPrivateKey; new_account_key : TAccountKey; fee: UInt64; payload: TRawBytes);
begin
  inherited Create;
  FData.account_signer := account_signer;
  FData.account_target := account_target;
  If (OpType=CT_Op_Changekey) then begin
    If (account_signer<>account_target) then Raise Exception.Create('ERROR DEV 20170530-4');
  end else if (OpType=CT_Op_ChangeKeySigned) then begin
    // Allowed signer<>target
  end else Raise Exception.Create('ERROR DEV 20170530-5');
  FData.n_operation := n_operation;
  FData.fee := fee;
  FData.payload := payload;
  // V2: No need to store public key because it's at safebox. Saving at least 64 bytes!
  // FData.public_key := key.PublicKey;
  FData.new_accountkey := new_account_key;
  If Not DoSignOperation(key,FData) then begin
    TLog.NewLog(lterror,Classname,'Error signing a new Change key');
    FHasValidSignature := false;
  end else FHasValidSignature := true;
end;

function TChangeKeyTransaction.ApplyTransaction(AccountTransaction : TPCSafeBoxTransaction; var errors: AnsiString): Boolean;
Var account_signer, account_target : TAccount;
begin
  Result := false;
  if (FData.account_signer>=AccountTransaction.FreezedSafeBox.AccountsCount) then begin
    errors := 'Invalid account number';
    Exit;
  end;
  if TAccountComp.IsAccountBlockedByProtocol(FData.account_signer, AccountTransaction.FreezedSafeBox.BlocksCount) then begin
    errors := 'account is blocked for protocol';
    Exit;
  end;
  If (FData.account_signer<>FData.account_target) then begin
    if (FData.account_target>=AccountTransaction.FreezedSafeBox.AccountsCount) then begin
      errors := 'Invalid account target number';
      Exit;
    end;
    if TAccountComp.IsAccountBlockedByProtocol(FData.account_target, AccountTransaction.FreezedSafeBox.BlocksCount) then begin
      errors := 'Target account is blocked for protocol';
      Exit;
    end;
  end;
  if (FData.fee<0) Or (FData.fee>CT_MaxTransactionFee) then begin
    errors := 'Invalid fee: '+Inttostr(FData.fee);
    exit;
  end;
  account_signer := AccountTransaction.Account(FData.account_signer);
  account_target := AccountTransaction.Account(FData.account_target);
  if ((account_signer.n_operation+1)<>FData.n_operation) then begin
    errors := 'Invalid n_operation';
    Exit;
  end;
  if (account_signer.balance<FData.fee) then begin
    errors := 'Insuficient founds';
    exit;
  end;
  if (length(FData.payload)>CT_MaxPayloadSize) then begin
    errors := 'Invalid Payload size:'+inttostr(length(FData.payload))+' (Max: '+inttostr(CT_MaxPayloadSize)+')';
    If (AccountTransaction.FreezedSafeBox.CurrentProtocol>=CT_PROTOCOL_2) then begin
      Exit; // BUG from protocol 1
    end;
  end;
  // Is locked? Protocol 2 check
  if (TAccountComp.IsAccountLocked(account_signer.accountInfo,AccountTransaction.FreezedSafeBox.BlocksCount)) then begin
    errors := 'Account signer is currently locked';
    exit;
  end;
  If Not FData.new_accountkey.IsValidAccountKey( errors ) then begin
    exit;
  end;
  // NEW v2 protocol protection: Does not allow to change key for same key
  if (AccountTransaction.FreezedSafeBox.CurrentProtocol>=CT_PROTOCOL_2) then begin
    if (TAccountKey.EqualAccountKeys(account_target.accountInfo.accountKey,FData.new_accountkey)) then begin
      errors := 'New public key is the same public key';
      exit;
    end;
  end;
  // Build 1.4
  If (FData.public_key.EC_OpenSSL_NID<>CT_TECDSA_Public_Nul.EC_OpenSSL_NID) And (Not TAccountKey.EqualAccountKeys(FData.public_key,account_signer.accountInfo.accountkey)) then begin
    errors := Format('Invalid public key for account %d. Distinct from SafeBox public key! %s <> %s',[
      FData.account_signer,
      TCrypto.ToHexaString(FData.public_key.ToRawString),
      TCrypto.ToHexaString(account_signer.accountInfo.accountkey.ToRawString)]);
    exit;
  end;
  If (FData.account_signer<>FData.account_target) then begin
    if (TAccountComp.IsAccountLocked(account_target.accountInfo,AccountTransaction.FreezedSafeBox.BlocksCount)) then begin
      errors := 'Account target is currently locked';
      exit;
    end;
    // Check have same public key
    If Not TAccountKey.EqualAccountKeys(account_signer.accountInfo.accountKey,account_target.accountInfo.accountKey) then begin
      errors := 'Signer and target accounts have different public key';
      exit;
    end;
    if (AccountTransaction.FreezedSafeBox.CurrentProtocol<CT_PROTOCOL_2) then begin
      errors := 'NOT ALLOWED ON PROTOCOL 1';
      exit;
    end;
  end;

  If Not TCrypto.ECDSAVerify(account_signer.accountInfo.accountkey,GetOperationHashToSign(FData),FData.sign) then begin
    errors := 'Invalid sign';
    FHasValidSignature := false;
    exit;
  end else FHasValidSignature := true;
  FPrevious_Signer_updated_block := account_signer.updated_block;
  FPrevious_Destination_updated_block := account_target.updated_block;
  account_target.accountInfo.accountKey := FData.new_accountkey;
  // Set to normal:
  account_target.accountInfo.state := as_Normal;
  account_target.accountInfo.locked_until_block := 0;
  account_target.accountInfo.price := 0;
  account_target.accountInfo.account_to_pay := 0;
  account_target.accountInfo.new_publicKey := CT_TECDSA_Public_Nul;
  Result := AccountTransaction.UpdateAccountInfo(FData.account_signer,FData.n_operation,FData.account_target,
         account_target.accountInfo,
         account_target.name,
         account_target.account_type,
         FData.fee,errors);
end;

class function TChangeKeyTransaction.DoSignOperation(key: TECPrivateKey; var op: TOpChangeKeyData): Boolean;
var s : AnsiString;
  _sign : TECDSA_SIG;
begin
  s := GetOperationHashToSign(op);
  Try
    _sign := TCrypto.ECDSASign(key,s);
    op.sign := _sign;
    Result := true;
  Except
    On E:Exception do begin
      Result := false;
      TLog.NewLog(lterror,ClassName,'Error signing ChangeKey operation: '+E.Message);
    end;
  End;
end;

function TChangeKeyTransaction.GetBufferForOpHash(UseProtocolV2 : Boolean): TRawBytes;
var ms : TMemoryStream;
  s : AnsiString;
begin
  If UseProtocolV2 then Result := inherited GetBufferForOpHash(UseProtocolV2)
  else begin
    ms := TMemoryStream.Create;
    try
      ms.Write(FData.account_signer,Sizeof(FData.account_signer)); //Protocol 1 does not allow signer/target. signer=target always
      ms.Write(FData.n_operation,Sizeof(FData.n_operation));
      ms.Write(FData.fee,Sizeof(FData.fee));
      if length(FData.payload)>0 then
        ms.WriteBuffer(FData.payload[1],length(FData.payload));
      ms.Write(FData.public_key.EC_OpenSSL_NID,Sizeof(FData.public_key.EC_OpenSSL_NID));
      if length(FData.public_key.x)>0 then
        ms.WriteBuffer(FData.public_key.x[1],length(FData.public_key.x));
      if length(FData.public_key.y)>0 then
        ms.WriteBuffer(FData.public_key.y[1],length(FData.public_key.y));
      s := FData.new_accountkey.ToRawString;
      if length(s)>0 then
        ms.WriteBuffer(s[1],length(s));
      if length(FData.sign.r)>0 then
        ms.WriteBuffer(FData.sign.r[1],length(FData.sign.r));
      if length(FData.sign.s)>0 then
        ms.WriteBuffer(FData.sign.s[1],length(FData.sign.s));
      ms.Position := 0;
      setlength(Result,ms.Size);
      ms.ReadBuffer(Result[1],ms.Size);
    finally
      ms.Free;
    end;
  end;
end;

class function TChangeKeyTransaction.GetOperationHashToSign(const op: TOpChangeKeyData): TRawBytes;
var ms : TMemoryStream;
  s : AnsiString;
begin
  ms := TMemoryStream.Create;
  try
    ms.Write(op.account_signer,Sizeof(op.account_signer));
    if (op.account_signer<>op.account_target) then ms.Write(op.account_target,Sizeof(op.account_target));
    ms.Write(op.n_operation,Sizeof(op.n_operation));
    ms.Write(op.fee,Sizeof(op.fee));
    if length(op.payload)>0 then
      ms.WriteBuffer(op.payload[1],length(op.payload));
    ms.Write(op.public_key.EC_OpenSSL_NID,Sizeof(op.public_key.EC_OpenSSL_NID));
    if length(op.public_key.x)>0 then
      ms.WriteBuffer(op.public_key.x[1],length(op.public_key.x));
    if length(op.public_key.y)>0 then
      ms.WriteBuffer(op.public_key.y[1],length(op.public_key.y));
    s := op.new_accountkey.ToRawString;
    if length(s)>0 then
      ms.WriteBuffer(s[1],length(s));
    ms.Position := 0;
    setlength(Result,ms.Size);
    ms.ReadBuffer(Result[1],ms.Size);
  finally
    ms.Free;
  end;
end;


procedure TChangeKeyTransaction.InitializeData;
begin
  inherited;
  FData := CT_TOpChangeKeyData_NUL;
end;

function TChangeKeyTransaction.LoadFromStream(Stream: TStream; LoadExtendedData : Boolean): Boolean;
var s : AnsiString;
begin
  Result := false;
  if Stream.Size-Stream.Position < 16  then exit; // Invalid stream
  Stream.Read(FData.account_signer,Sizeof(FData.account_signer));
  If (OpType=CT_Op_ChangeKey) then begin
    FData.account_target:=FData.account_signer;
  end else if (OpType=CT_Op_ChangeKeySigned) then begin
    Stream.Read(FData.account_target,Sizeof(FData.account_target));
  end else Raise Exception.Create('ERROR DEV 20170530-1');
  Stream.Read(FData.n_operation,Sizeof(FData.n_operation));
  Stream.Read(FData.fee,Sizeof(FData.fee));
  if TStreamOp.ReadAnsiString(Stream,FData.payload)<0 then exit;
  if Stream.Read(FData.public_key.EC_OpenSSL_NID,Sizeof(FData.public_key.EC_OpenSSL_NID))<0 then exit;
  if TStreamOp.ReadAnsiString(Stream,FData.public_key.x)<0 then exit;
  if TStreamOp.ReadAnsiString(Stream,FData.public_key.y)<0 then exit;
  if TStreamOp.ReadAnsiString(Stream,s)<0 then exit;
  FData.new_accountkey := TAccountKey.FromRawString(s);
  if TStreamOp.ReadAnsiString(Stream,FData.sign.r)<0 then exit;
  if TStreamOp.ReadAnsiString(Stream,FData.sign.s)<0 then exit;
  Result := true;
end;

function TChangeKeyTransaction.GetAmount: Int64;
begin
  Result := 0;
end;

function TChangeKeyTransaction.GetFee: UInt64;
begin
  Result := FData.fee;
end;

function TChangeKeyTransaction.GetPayload: TRawBytes;
begin
  Result := FData.payload;
end;

function TChangeKeyTransaction.GetOpType: Byte;
begin
  Result := CT_Op_Changekey;
end;

function TChangeKeyTransaction.SaveToStream(Stream: TStream; SaveExtendedData : Boolean): Boolean;
begin
  Stream.Write(FData.account_signer,Sizeof(FData.account_signer));
  If (OpType=CT_Op_ChangeKey) then begin
    If FData.account_target<>FData.account_signer then Raise Exception.Create('ERROR DEV 20170530-2');
  end else if (OpType=CT_Op_ChangeKeySigned) then begin
    Stream.Write(FData.account_target,Sizeof(FData.account_target));
  end else Raise Exception.Create('ERROR DEV 20170530-3');
  Stream.Write(FData.n_operation,Sizeof(FData.n_operation));
  Stream.Write(FData.fee,Sizeof(FData.fee));
  TStreamOp.WriteAnsiString(Stream,FData.payload);
  Stream.Write(FData.public_key.EC_OpenSSL_NID,Sizeof(FData.public_key.EC_OpenSSL_NID));
  TStreamOp.WriteAnsiString(Stream,FData.public_key.x);
  TStreamOp.WriteAnsiString(Stream,FData.public_key.y);
  TStreamOp.WriteAnsiString(Stream,FData.new_accountkey.ToRawString);
  TStreamOp.WriteAnsiString(Stream,FData.sign.r);
  TStreamOp.WriteAnsiString(Stream,FData.sign.s);
  Result := true;
end;

function TChangeKeyTransaction.GetSignerAccount: Cardinal;
begin
  Result := FData.account_signer;
end;

function TChangeKeyTransaction.GetTransactionData(Block,
  Affected_account_number: Cardinal;
  var TransactionData: TTransactionData): Boolean;
begin
  TransactionData.OpSubtype := CT_OpSubtype_ChangeKey;
  TransactionData.newKey := Data.new_accountkey;
  TransactionData.DestAccount := Data.account_target;
  TransactionData.OperationTxt := 'Change Key to ' +
  TAccountComp.GetECInfoTxt(TransactionData.newKey.EC_OpenSSL_NID);
  Result := true;
  TransactionData.OriginalPayload := GetPayload;
  If TCrypto.IsHumanReadable(TransactionData.OriginalPayload) then
    TransactionData.PrintablePayload := TransactionData.OriginalPayload
  else
    TransactionData.PrintablePayload := TCrypto.ToHexaString(TransactionData.OriginalPayload);
  TransactionData.OperationHash := TransactionHash(Block);
  if (Block < CT_Protocol_Upgrade_v2_MinBlock) then
  begin
    TransactionData.OperationHash_OLD := TransactionHash_OLD(Block);
  end;
  TransactionData.valid := true;
end;

function TChangeKeyTransaction.GetDestinationAccount: Int64;
begin
  Result := FData.account_target;
end;

function TChangeKeyTransaction.GetNumberOfTransactions: Cardinal;
begin
  Result := FData.n_operation;
end;

function TChangeKeyTransaction.toString: String;
begin
  Result := Format('Change key of %s to new key: %s fee:%s (n_op:%d) payload size:%d',[
    TAccountComp.AccountNumberToAccountTxtNumber(FData.account_target),
    TAccountComp.GetECInfoTxt(FData.new_accountkey.EC_OpenSSL_NID),
    TAccountComp.FormatMoney(FData.fee),FData.n_operation,Length(FData.payload)]);
end;


function TChangeKeySignedTransaction.GetOpType: Byte;
begin
  Result:=CT_Op_ChangeKeySigned;
end;

function TChangeKeySignedTransaction.GetTransactionData(Block,
  Affected_account_number: Cardinal;
  var TransactionData: TTransactionData): Boolean;
begin
  TransactionData.OpSubtype := CT_OpSubtype_ChangeKeySigned;
  TransactionData.newKey := Data.new_accountkey;
  TransactionData.DestAccount := Data.account_target;
  TransactionData.OperationTxt := 'Change ' +
  TAccountComp.AccountNumberToAccountTxtNumber(TransactionData.DestAccount) + ' account key to ' +
  TAccountComp.GetECInfoTxt(TransactionData.newKey.EC_OpenSSL_NID);
  Result := true;
  TransactionData.OriginalPayload := GetPayload;
  If TCrypto.IsHumanReadable(TransactionData.OriginalPayload) then
    TransactionData.PrintablePayload := TransactionData.OriginalPayload
  else
    TransactionData.PrintablePayload := TCrypto.ToHexaString(TransactionData.OriginalPayload);
  TransactionData.OperationHash := TransactionHash(Block);
  if (Block < CT_Protocol_Upgrade_v2_MinBlock) then
  begin
    TransactionData.OperationHash_OLD := TransactionHash_OLD(Block);
  end;
  TransactionData.valid := true;
end;

initialization
{TODO 'Remove ublockchain dependency'}
  TTransactionManager.RegisterOperationClass(TChangeKeyTransaction, CT_Op_Changekey);
  TTransactionManager.RegisterOperationClass(TChangeKeySignedTransaction, CT_Op_ChangeKeySigned);
end.
