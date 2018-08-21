unit UOpTransaction;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{
  Copyright (c) Albert Molina 2016 - 2018 original code from PascalCoin https://pascalcoin.org/

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  If you like it, consider a donation using BitCoin:
    16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  }

interface

Uses UCrypto, UBlockChain, Classes, UAccounts, MicroCoin.Transaction.Transaction,
MicroCoin.Transaction.Base;


Type

  // NEW OPERATIONS PROTOCOL 2
  TOpListAccountOperationType = (lat_Unknown, lat_ListForSale, lat_DelistAccount);

  TOpListAccountData = Record
    account_signer,
    account_target: Cardinal;
    operation_type : TOpListAccountOperationType;
    n_operation : Cardinal;
    account_price: UInt64;
    account_to_pay : Cardinal;
    fee: UInt64;
    payload: TRawBytes;
    public_key: TAccountKey;
    new_public_key: TAccountKey;   // If EC_OpenSSL_NID=0 then is OPEN, otherwise is for only 1 public key
    locked_until_block : Cardinal; //
    sign: TECDSA_SIG;
  End;

  TOpChangeAccountInfoType = (public_key,account_name,account_type);
  TOpChangeAccountInfoTypes = Set of TOpChangeAccountInfoType;

  TOpChangeAccountInfoData = Record
    account_signer,
    account_target: Cardinal;
    n_operation : Cardinal;
    fee: UInt64;
    payload: TRawBytes;
    public_key: TECDSA_Public;
    changes_type : TOpChangeAccountInfoTypes; // bits mask. $0001 = New account key , $0002 = New name , $0004 = New type
    new_accountkey: TAccountKey;  // If (changes_mask and $0001)=$0001 then change account key
    new_name: TRawBytes;          // If (changes_mask and $0002)=$0002 then change name
    new_type: Word;               // If (changes_mask and $0004)=$0004 then change type
    sign: TECDSA_SIG;
  End;


Const
  CT_TOpListAccountData_NUL : TOpListAccountData = (account_signer:0;account_target:0;operation_type:lat_Unknown;n_operation:0;account_price:0;account_to_pay:0;fee:0;payload:'';public_key:(EC_OpenSSL_NID:0;x:'';y:'');new_public_key:(EC_OpenSSL_NID:0;x:'';y:'');locked_until_block:0;sign:(r:'';s:''));
  CT_TOpChangeAccountInfoData_NUL : TOpChangeAccountInfoData = (account_signer:0;account_target:0;n_operation:0;fee:0;payload:'';public_key:(EC_OpenSSL_NID:0;x:'';y:'');changes_type:[];
    new_accountkey:(EC_OpenSSL_NID:0;x:'';y:'');new_name:'';new_type:0;sign:(r:'';s:''));

Type

  { TOpListAccount }
  TOpListAccount = Class(TTransaction)
  private
    FData : TOpListAccountData;
  protected
    procedure InitializeData; override;
    function SaveToStream(Stream: TStream; SaveExtendedData : Boolean): Boolean; override;
    function LoadFromStream(Stream: TStream; LoadExtendedData : Boolean): Boolean; override;
  public
    Class Function GetOperationHashToSign(const operation : TOpListAccountData) : TRawBytes;
    Class Function DoSignOperation(key : TECPrivateKey; var operation : TOpListAccountData) : Boolean;

    Function IsPrivateSale : Boolean;
    Function IsDelist : Boolean; virtual; abstract;

    function GetBufferForOpHash(UseProtocolV2 : Boolean): TRawBytes; override;
    function DoOperation(AccountTransaction : TPCSafeBoxTransaction; var errors : AnsiString) : Boolean; override;
    function GetOperationAmount : Int64; override;
    function GetOperationFee : UInt64; override;
    function GetOperationPayload : TRawBytes; override;
    function GetSignerAccount : Cardinal; override;
    function GetDestinationAccount : Int64; override;
    function GetSellerAccount : Int64; override;
    function GetNumberOfOperations : Cardinal; override;
    procedure AffectedAccounts(list : TList); override;
    Property Data : TOpListAccountData read FData;
    Function toString : String; Override;
  End;

  TOpListAccountForSale = Class(TOpListAccount)
  public
    function GetOpType : Byte; override;
    Constructor CreateListAccountForSale(account_signer, n_operation, account_target: Cardinal; account_price, fee : UInt64; account_to_pay:Cardinal; new_public_key:TAccountKey; locked_until_block : Cardinal; key:TECPrivateKey; payload: TRawBytes);
    Function IsDelist : Boolean; override;
    function GetTransactionData(Block: Cardinal;
      Affected_account_number: Cardinal;
      var TransactionData: TTransactionData): Boolean; override;


  End;

  TOpDelistAccountForSale = Class(TOpListAccount)
  public
    function GetOpType : Byte; override;
    Constructor CreateDelistAccountForSale(account_signer, n_operation, account_target: Cardinal; fee: UInt64; key: TECPrivateKey; payload: TRawBytes);
    Function IsDelist : Boolean; override;
    function GetTransactionData(Block: Cardinal;
      Affected_account_number: Cardinal;
      var TransactionData: TTransactionData): Boolean; override;
  end;

  { TOpChangeAccountInfo }

  TOpChangeAccountInfo = Class(TTransaction)
  private
    FData : TOpChangeAccountInfoData;
  protected
    procedure InitializeData; override;
    function SaveToStream(Stream: TStream; SaveExtendedData : Boolean): Boolean; override;
    function LoadFromStream(Stream: TStream; LoadExtendedData : Boolean): Boolean; override;
  public
    Class Function GetOperationHashToSign(const op : TOpChangeAccountInfoData) : TRawBytes;
    Class Function DoSignOperation(key : TECPrivateKey; var op : TOpChangeAccountInfoData) : Boolean;
    function GetOpType : Byte; override;

    function GetBufferForOpHash(UseProtocolV2 : Boolean): TRawBytes; override;
    function DoOperation(AccountTransaction : TPCSafeBoxTransaction; var errors : AnsiString) : Boolean; override;
    function GetOperationAmount : Int64; override;
    function GetOperationFee : UInt64; override;
    function GetOperationPayload : TRawBytes; override;
    function GetSignerAccount : Cardinal; override;
    function GetDestinationAccount : Int64; override;
    function GetNumberOfOperations : Cardinal; override;
    procedure AffectedAccounts(list : TList); override;
    Constructor CreateChangeAccountInfo(account_signer, n_operation, account_target: Cardinal; key:TECPrivateKey;
      change_key : Boolean; const new_account_key : TAccountKey;
      change_name: Boolean; const new_name : TRawBytes;
      change_type: Boolean; const new_type : Word;
      fee: UInt64; payload: TRawBytes);
    function GetTransactionData(Block: Cardinal;
      Affected_account_number: Cardinal;
      var TransactionData: TTransactionData): Boolean; override;
    Property Data : TOpChangeAccountInfoData read FData;
    Function toString : String; Override;

  End;


Procedure RegisterOperationsClass;

implementation

uses
  SysUtils, UConst, ULog;

Procedure RegisterOperationsClass;
Begin
  TPCOperationsComp.RegisterOperationClass(TOpListAccountForSale, CT_Op_ListAccountForSale);
  TPCOperationsComp.RegisterOperationClass(TOpDelistAccountForSale, CT_Op_DelistAccount);
  TPCOperationsComp.RegisterOperationClass(TOpChangeAccountInfo, CT_Op_ChangeAccountInfo);
End;

{ TOpChangeAccountInfo }

procedure TOpChangeAccountInfo.InitializeData;
begin
  inherited InitializeData;
  FData := CT_TOpChangeAccountInfoData_NUL;
end;

function TOpChangeAccountInfo.SaveToStream(Stream: TStream; SaveExtendedData: Boolean): Boolean;
var b : byte;
begin
  Stream.Write(FData.account_signer,Sizeof(FData.account_signer));
  Stream.Write(FData.account_target,Sizeof(FData.account_target));
  Stream.Write(FData.n_operation,Sizeof(FData.n_operation));
  Stream.Write(FData.fee,Sizeof(FData.fee));
  TStreamOp.WriteAnsiString(Stream,FData.payload);
  TStreamOp.WriteAccountKey(Stream,FData.public_key);
  b := 0;
  if (public_key in FData.changes_type) then b:=b OR $01;
  if (account_name in FData.changes_type) then b:=b OR $02;
  if (account_type in FData.changes_type) then b:=b OR $04;
  Stream.Write(b,Sizeof(b));
  TStreamOp.WriteAccountKey(Stream,FData.new_accountkey);
  TStreamOp.WriteAnsiString(Stream,FData.new_name);
  Stream.Write(FData.new_type,Sizeof(FData.new_type));
  TStreamOp.WriteAnsiString(Stream,FData.sign.r);
  TStreamOp.WriteAnsiString(Stream,FData.sign.s);
  Result := true;
end;

function TOpChangeAccountInfo.LoadFromStream(Stream: TStream; LoadExtendedData: Boolean): Boolean;
var b : Byte;
begin
  Result := False;
  If Stream.Size - Stream.Position < 20 then exit;
  Stream.Read(FData.account_signer,Sizeof(FData.account_signer));
  Stream.Read(FData.account_target,Sizeof(FData.account_target));
  Stream.Read(FData.n_operation,Sizeof(FData.n_operation));
  Stream.Read(FData.fee,Sizeof(FData.fee));
  if TStreamOp.ReadAnsiString(Stream,FData.payload)<0 then Exit;
  if TStreamOp.ReadAccountKey(Stream,FData.public_key)<0 then Exit;
  Stream.Read(b,SizeOf(b));
  FData.changes_type:=[];
  if (b AND $01)=$01 then FData.changes_type:=FData.changes_type + [public_key];
  if (b AND $02)=$02 then FData.changes_type:=FData.changes_type + [account_name];
  if (b AND $04)=$04 then FData.changes_type:=FData.changes_type + [account_type];
  // Check
  if (b AND $F8)<>0 then Exit;
  if TStreamOp.ReadAccountKey(Stream,FData.new_accountkey)<0 then Exit;
  if TStreamOp.ReadAnsiString(Stream,FData.new_name)<0 then Exit;
  Stream.Read(FData.new_type,Sizeof(FData.new_type));
  if TStreamOp.ReadAnsiString(Stream,FData.sign.r)<0 then Exit;
  if TStreamOp.ReadAnsiString(Stream,FData.sign.s)<0 then Exit;
  Result := true;
end;

class function TOpChangeAccountInfo.GetOperationHashToSign(const op: TOpChangeAccountInfoData): TRawBytes;
var Stream : TMemoryStream;
  b : Byte;
begin
  Stream := TMemoryStream.Create;
  try
    Stream.Write(op.account_signer,Sizeof(op.account_signer));
    Stream.Write(op.account_target,Sizeof(op.account_target));
    Stream.Write(op.n_operation,Sizeof(op.n_operation));
    Stream.Write(op.fee,Sizeof(op.fee));
    TStreamOp.WriteAnsiString(Stream,op.payload);
    TStreamOp.WriteAccountKey(Stream,op.public_key);
    b := 0;
    if (public_key in op.changes_type) then b:=b OR $01;
    if (account_name in op.changes_type) then b:=b OR $02;
    if (account_type in op.changes_type) then b:=b OR $04;
    Stream.Write(b,Sizeof(b));
    TStreamOp.WriteAccountKey(Stream,op.new_accountkey);
    TStreamOp.WriteAnsiString(Stream,op.new_name);
    Stream.Write(op.new_type,Sizeof(op.new_type));
    Stream.Position := 0;
    setlength(Result,Stream.Size);
    Stream.ReadBuffer(Result[1],Stream.Size);
  finally
    Stream.Free;
  end;
end;

class function TOpChangeAccountInfo.DoSignOperation(key: TECPrivateKey; var op: TOpChangeAccountInfoData): Boolean;
var raw : TRawBytes;
  _sign : TECDSA_SIG;
begin
  If Not Assigned(key.PrivateKey) then begin
    Result := false;
    op.sign.r:='';
    op.sign.s:='';
    exit;
  end;
  raw := GetOperationHashToSign(op);
  Try
    _sign := TCrypto.ECDSASign(key.PrivateKey,raw);
    op.sign := _sign;
    Result := true;
  Except
    op.sign.r:='';
    op.sign.s:='';
    Result := false;
  End;
  SetLength(raw,0);
end;

function TOpChangeAccountInfo.GetOpType: Byte;
begin
  Result := CT_Op_ChangeAccountInfo;
end;

function TOpChangeAccountInfo.GetBufferForOpHash(UseProtocolV2: Boolean): TRawBytes;
begin
  Result:=inherited GetBufferForOpHash(True);
end;

function TOpChangeAccountInfo.DoOperation(AccountTransaction: TPCSafeBoxTransaction; var errors: AnsiString): Boolean;
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
  if (AccountTransaction.FreezedSafeBox.CurrentProtocol<CT_PROTOCOL_2) then begin
    errors := 'NOT ALLOWED ON PROTOCOL 1';
    exit;
  end;
  If (public_key in FData.changes_type) then begin
    If Not TAccountComp.IsValidAccountKey( FData.new_accountkey, errors ) then begin
      exit;
    end;
  end;
  If (account_name in FData.changes_type) then begin
    If (FData.new_name<>'') then begin
      If Not TPCSafeBox.ValidAccountName(FData.new_name,errors) then Exit;
    end;
  end else begin
    If (FData.new_name<>'') then begin
      errors := 'Invalid data in new_name field';
      Exit;
    end;
  end;
  If (FData.changes_type=[]) then begin
    errors := 'No change';
    Exit;
  end;
  If (FData.public_key.EC_OpenSSL_NID<>CT_TECDSA_Public_Nul.EC_OpenSSL_NID) And (Not TAccountComp.EqualAccountKeys(FData.public_key,account_signer.accountInfo.accountkey)) then begin
    errors := Format('Invalid public key for account %d. Distinct from SafeBox public key! %s <> %s',[
      FData.account_signer,
      TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(FData.public_key)),
      TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(account_signer.accountInfo.accountkey))]);
    exit;
  end;
  If (FData.account_signer<>FData.account_target) then begin
    if (TAccountComp.IsAccountLocked(account_target.accountInfo,AccountTransaction.FreezedSafeBox.BlocksCount)) then begin
      errors := 'Account target is currently locked';
      exit;
    end;
    // Check have same public key
    If Not TAccountComp.EqualAccountKeys(account_signer.accountInfo.accountKey,account_target.accountInfo.accountKey) then begin
      errors := 'Signer and target accounts have different public key';
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
  If (public_key in FData.changes_type) then begin
    account_target.accountInfo.accountKey := FData.new_accountkey;
  end;
  If (account_name in FData.changes_type) then begin
    account_target.name := FData.new_name;
  end;
  If (account_type in FData.changes_type) then begin
    account_target.account_type := FData.new_type;
  end;
  Result := AccountTransaction.UpdateAccountInfo(FData.account_signer,FData.n_operation,FData.account_target,
         account_target.accountInfo,
         account_target.name,
         account_target.account_type,
         FData.fee,errors);
end;

function TOpChangeAccountInfo.GetOperationAmount: Int64;
begin
  Result := 0;
end;

function TOpChangeAccountInfo.GetOperationFee: UInt64;
begin
  Result := FData.fee;
end;

function TOpChangeAccountInfo.GetOperationPayload: TRawBytes;
begin
  Result := FData.payload;
end;

function TOpChangeAccountInfo.GetSignerAccount: Cardinal;
begin
  Result := FData.account_signer;
end;

function TOpChangeAccountInfo.GetTransactionData(Block,
  Affected_account_number: Cardinal;
  var TransactionData: TTransactionData): Boolean;

var s : string;

begin
  TransactionData.DestAccount := GetDestinationAccount;
  s := '';
  if (public_key in Data.changes_type)
  then
  begin
    s := 'key';
  end;
  if (account_name in Data.changes_type)
  then
  begin
    if s <> '' then
      s := s + ',';
    s := s + 'name';
  end;
  if (account_type in Data.changes_type)
  then
  begin
    if s <> '' then
      s := s + ',';
    s := s + 'type';
  end;
  TransactionData.OperationTxt := 'Changed ' + s + ' of account ' +
    TAccountComp.AccountNumberToAccountTxtNumber
    (GetDestinationAccount);
  TransactionData.OpSubtype := CT_OpSubtype_ChangeAccountInfo;
  Result := true;
  TransactionData.OriginalPayload := GetOperationPayload;
  If TCrypto.IsHumanReadable(TransactionData.OriginalPayload) then
    TransactionData.PrintablePayload := TransactionData.OriginalPayload
  else
    TransactionData.PrintablePayload := TCrypto.ToHexaString(TransactionData.OriginalPayload);
  TransactionData.OperationHash := OperationHashValid(self, Block);
  if (Block < CT_Protocol_Upgrade_v2_MinBlock) then
  begin
    TransactionData.OperationHash_OLD := TTransaction.OperationHash_OLD(self, Block);
  end;
  TransactionData.valid := true;
end;

function TOpChangeAccountInfo.GetDestinationAccount: Int64;
begin
  Result := FData.account_target;
end;

function TOpChangeAccountInfo.GetNumberOfOperations: Cardinal;
begin
  Result := FData.n_operation;
end;

procedure TOpChangeAccountInfo.AffectedAccounts(list: TList);
begin
  list.Add(TObject(FData.account_signer));
  if (FData.account_target<>FData.account_signer) then list.Add(TObject(FData.account_target));
end;

constructor TOpChangeAccountInfo.CreateChangeAccountInfo(account_signer, n_operation,
  account_target: Cardinal; key: TECPrivateKey; change_key: Boolean;
  const new_account_key: TAccountKey; change_name: Boolean;
  const new_name: TRawBytes; change_type: Boolean; const new_type: Word;
  fee: UInt64; payload: TRawBytes);
begin
  inherited Create;
  FData.account_signer:=account_signer;
  FData.account_target:=account_target;
  FData.n_operation:=n_operation;
  FData.fee:=fee;
  FData.payload:=payload;
  // V2: No need to store public key because it's at safebox. Saving at least 64 bytes!
  // FData.public_key:=key.PublicKey;
  FData.changes_type:=[];
  If change_key then begin
    FData.changes_type:=FData.changes_type + [public_key];
    FData.new_accountkey:=new_account_key;
  end;
  If change_name then begin
    FData.changes_type:=FData.changes_type + [account_name];
    FData.new_name:=new_name;
  end;
  If change_type then begin
    FData.changes_type:=FData.changes_type + [account_type];
    FData.new_type:=new_type;
  end;
  If Not DoSignOperation(key,FData) then begin
    TLog.NewLog(lterror,Classname,'Error signing a new Change Info operation');
    FHasValidSignature := false;
  end else FHasValidSignature := true;
end;

function TOpChangeAccountInfo.toString: String;
var s : String;
begin
  s := '';
  If (public_key IN FData.changes_type) then s := 'new public key '+TAccountComp.GetECInfoTxt(FData.new_accountkey.EC_OpenSSL_NID);
  If (account_name IN FData.changes_type)  then begin
    if s<>'' then s:=s+', ';
    s := s + 'new name to "'+FData.new_name+'"';
  end;
  If (account_type IN FData.changes_type)  then begin
    if s<>'' then s:=s+', ';
    s := s + 'new type to '+IntToStr(FData.new_type);
  end;
  Result := Format('Change account %s info: %s fee:%s (n_op:%d) payload size:%d',[
     TAccountComp.AccountNumberToAccountTxtNumber(FData.account_target),
     s,
     TAccountComp.FormatMoney(FData.fee),FData.n_operation,Length(FData.payload)]);
end;

{ TOpListAccount }

procedure TOpListAccount.AffectedAccounts(list: TList);
begin
  list.Add(TObject(FData.account_signer));
  if FData.account_signer<>FData.account_target then
    list.Add(TObject(FData.account_target));
end;

function TOpListAccount.DoOperation(AccountTransaction: TPCSafeBoxTransaction; var errors: AnsiString): Boolean;
Var account_signer, account_target : TAccount;
begin
  Result := false;
  if (AccountTransaction.FreezedSafeBox.CurrentProtocol<CT_PROTOCOL_2) then begin
    errors := 'List/Delist Account is not allowed on Protocol 1';
    exit;
  end;
  if (FData.account_signer>=AccountTransaction.FreezedSafeBox.AccountsCount) then begin
    errors := 'Invalid signer account number';
    Exit;
  end;
  if (FData.account_target>=AccountTransaction.FreezedSafeBox.AccountsCount) then begin
    errors := 'Invalid target account number';
    Exit;
  end;
  if TAccountComp.IsAccountBlockedByProtocol(FData.account_signer, AccountTransaction.FreezedSafeBox.BlocksCount) then begin
    errors := 'Signer account is blocked for protocol';
    Exit;
  end;
  if TAccountComp.IsAccountBlockedByProtocol(FData.account_target, AccountTransaction.FreezedSafeBox.BlocksCount) then begin
    errors := 'Target account is blocked for protocol';
    Exit;
  end;
  if Not IsDelist then begin
    if (FData.account_to_pay>=AccountTransaction.FreezedSafeBox.AccountsCount) then begin
      errors := 'Invalid account to pay number';
      Exit;
    end;
    if (FData.account_target = FData.account_to_pay) then begin
      errors := 'Account to pay is itself';
      Exit;
    end;
    if TAccountComp.IsAccountBlockedByProtocol(FData.account_to_pay, AccountTransaction.FreezedSafeBox.BlocksCount) then begin
      errors := 'Account to pay is blocked for protocol';
      Exit;
    end;
    if (FData.account_price<=0) then begin
      errors := 'Account for sale price must be > 0';
      exit;
    end;
    if (FData.locked_until_block > (AccountTransaction.FreezedSafeBox.BlocksCount + CT_MaxFutureBlocksLockedAccount)) then begin
      errors := 'Invalid locked block: Current block '+Inttostr(AccountTransaction.FreezedSafeBox.BlocksCount)+' cannot lock to block '+IntToStr(FData.locked_until_block);
      exit;
    end;
    if IsPrivateSale then begin
      If Not TAccountComp.IsValidAccountKey( FData.new_public_key, errors ) then begin
        errors := 'Invalid new public key: '+errors;
        exit;
      end;
    end;
  end;
  if (FData.fee<0) Or (FData.fee>CT_MaxTransactionFee) then begin
    errors := 'Invalid fee: '+Inttostr(FData.fee);
    exit;
  end;
  account_signer := AccountTransaction.Account(FData.account_signer);
  account_target := AccountTransaction.Account(FData.account_target);
  if (FData.account_signer<>FData.account_target) then begin
    // Both accounts must have same PUBLIC KEY!
    if Not TAccountComp.EqualAccountKeys(account_signer.accountInfo.accountKey,account_target.accountInfo.accountKey) then begin
      errors := 'Signer and affected accounts have different public key';
      Exit;
    end;
  end;

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
    Exit;
  end;
  // Is locked?
  if (TAccountComp.IsAccountLocked(account_signer.accountInfo,AccountTransaction.FreezedSafeBox.BlocksCount)) then begin
    errors := 'Signer account is currently locked';
    exit;
  end;
  if (TAccountComp.IsAccountLocked(account_target.accountInfo,AccountTransaction.FreezedSafeBox.BlocksCount)) then begin
    errors := 'Target account is currently locked';
    exit;
  end;
  if (IsPrivateSale) then begin
    if TAccountComp.EqualAccountKeys(account_target.accountInfo.accountKey,FData.new_public_key) then begin
      errors := 'New public key for private sale is the same public key';
      Exit;
    end;
  end;

  //
  // Build 1.4
  If (FData.public_key.EC_OpenSSL_NID<>CT_TECDSA_Public_Nul.EC_OpenSSL_NID) And (Not TAccountComp.EqualAccountKeys(FData.public_key,account_signer.accountInfo.accountkey)) then begin
    errors := Format('Invalid public key for account %d. Distinct from SafeBox public key! %s <> %s',[
      FData.account_signer,
      TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(FData.public_key)),
      TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(account_signer.accountInfo.accountkey))]);
    exit;
  end;

  If Not TCrypto.ECDSAVerify(account_signer.accountInfo.accountkey,GetOperationHashToSign(FData),FData.sign) then begin
    errors := 'Invalid sign';
    FHasValidSignature := false;
    exit;
  end else FHasValidSignature := true;

  FPrevious_Signer_updated_block := account_signer.updated_block;
  FPrevious_Destination_updated_block := account_target.updated_block;
  if IsDelist then begin
    account_target.accountInfo.state := as_Normal;
    account_target.accountInfo.locked_until_block := CT_AccountInfo_NUL.locked_until_block;
    account_target.accountInfo.price := CT_AccountInfo_NUL.price;
    account_target.accountInfo.account_to_pay := CT_AccountInfo_NUL.account_to_pay;
    account_target.accountInfo.new_publicKey := CT_AccountInfo_NUL.new_publicKey;
  end else begin
    account_target.accountInfo.state := as_ForSale;
    account_target.accountInfo.locked_until_block := FData.locked_until_block;
    account_target.accountInfo.price := FData.account_price;
    account_target.accountInfo.account_to_pay := FData.account_to_pay;
    account_target.accountInfo.new_publicKey := FData.new_public_key;
  end;
  Result := AccountTransaction.UpdateAccountInfo(FData.account_signer,FData.n_operation,FData.account_target,
         account_target.accountInfo,
         account_target.name,
         account_target.account_type,
         FData.fee,errors);
end;

class function TOpListAccount.DoSignOperation(key: TECPrivateKey; var operation: TOpListAccountData): Boolean;
var s : AnsiString;
  _sign : TECDSA_SIG;
begin
  s := GetOperationHashToSign(operation);
  Try
    _sign := TCrypto.ECDSASign(key.PrivateKey,s);
    operation.sign := _sign;
    Result := true;
  Except
    On E:Exception do begin
      Result := false;
      TLog.NewLog(lterror,ClassName,'Error signing Account for sale operation: '+E.Message);
    end;
  End;
end;

function TOpListAccount.GetBufferForOpHash(UseProtocolV2 : Boolean): TRawBytes;
begin
  // This Operation is new from protocol V2, so we cannot hash it as a previous protocol!
  Result := inherited GetBufferForOpHash(true);
end;

class function TOpListAccount.GetOperationHashToSign(const operation: TOpListAccountData): TRawBytes;
var ms : TMemoryStream;
  s : AnsiString;
begin
  ms := TMemoryStream.Create;
  try
    ms.Write(operation.account_signer,Sizeof(operation.account_signer));
    ms.Write(operation.account_target,Sizeof(operation.account_target));
    ms.Write(operation.n_operation,Sizeof(operation.n_operation));
    ms.Write(operation.account_price,Sizeof(operation.account_price));
    ms.Write(operation.account_to_pay,Sizeof(operation.account_to_pay));
    ms.Write(operation.fee,Sizeof(operation.fee));
    if length(operation.payload)>0 then
      ms.WriteBuffer(operation.payload[1],length(operation.payload));
    ms.Write(operation.public_key.EC_OpenSSL_NID,Sizeof(operation.public_key.EC_OpenSSL_NID));
    if length(operation.public_key.x)>0 then
      ms.WriteBuffer(operation.public_key.x[1],length(operation.public_key.x));
    if length(operation.public_key.y)>0 then
      ms.WriteBuffer(operation.public_key.y[1],length(operation.public_key.y));
    s := TAccountComp.AccountKey2RawString(operation.new_public_key);
    if length(s)>0 then
      ms.WriteBuffer(s[1],length(s));
    ms.Write(operation.locked_until_block,Sizeof(operation.locked_until_block));
    ms.Position := 0;
    setlength(Result,ms.Size);
    ms.ReadBuffer(Result[1],ms.Size);
  finally
    ms.Free;
  end;
end;

procedure TOpListAccount.InitializeData;
begin
  inherited;
  FData := CT_TOpListAccountData_NUL;
end;

function TOpListAccount.IsPrivateSale: Boolean;
begin
  Result := (Not IsDelist) And (FData.new_public_key.EC_OpenSSL_NID<>0);
end;

function TOpListAccount.LoadFromStream(Stream: TStream; LoadExtendedData : Boolean): Boolean;
var s : AnsiString;
  w : Word;
begin
  Result := false;
  if Stream.Size-Stream.Position < 14  then exit; // Invalid stream
  Stream.Read(FData.account_signer,Sizeof(FData.account_signer));
  Stream.Read(FData.account_target,Sizeof(FData.account_target));
  Stream.Read(w,2);
  case w of
    CT_Op_ListAccountForSale : FData.operation_type := lat_ListForSale;
    CT_Op_DelistAccount : FData.operation_type := lat_DelistAccount;
  else exit; // Invalid data info
  end;
  Stream.Read(FData.n_operation,Sizeof(FData.n_operation));
  if (FData.operation_type = lat_ListForSale) then begin
    Stream.Read(FData.account_price,Sizeof(FData.account_price));
    Stream.Read(FData.account_to_pay,Sizeof(FData.account_to_pay));
    if Stream.Read(FData.public_key.EC_OpenSSL_NID,Sizeof(FData.public_key.EC_OpenSSL_NID))<0 then exit;
    if TStreamOp.ReadAnsiString(Stream,FData.public_key.x)<0 then exit;
    if TStreamOp.ReadAnsiString(Stream,FData.public_key.y)<0 then exit;
    if TStreamOp.ReadAnsiString(Stream,s)<0 then exit;
    FData.new_public_key := TAccountComp.RawString2Accountkey(s);
    Stream.Read(FData.locked_until_block,Sizeof(FData.locked_until_block));
  end;
  Stream.Read(FData.fee,Sizeof(FData.fee));
  if TStreamOp.ReadAnsiString(Stream,FData.payload)<0 then exit;
  if TStreamOp.ReadAnsiString(Stream,FData.sign.r)<0 then exit;
  if TStreamOp.ReadAnsiString(Stream,FData.sign.s)<0 then exit;
  Result := true;
end;

function TOpListAccount.GetNumberOfOperations: Cardinal;
begin
  Result := FData.n_operation;
end;

function TOpListAccount.GetOperationAmount: Int64;
begin
  Result := 0;
end;

function TOpListAccount.GetOperationFee: UInt64;
begin
  Result := FData.fee;
end;

function TOpListAccount.GetOperationPayload: TRawBytes;
begin
  Result := FData.payload;
end;

function TOpListAccount.SaveToStream(Stream: TStream; SaveExtendedData : Boolean): Boolean;
Var w : Word;
begin
  Stream.Write(FData.account_signer,Sizeof(FData.account_signer));
  Stream.Write(FData.account_target,Sizeof(FData.account_target));
  case FData.operation_type of
    lat_ListForSale : w := CT_Op_ListAccountForSale;
    lat_DelistAccount : w := CT_Op_DelistAccount;
  else raise Exception.Create('ERROR DEV 20170412-1');
  end;
  Stream.Write(w,2);
  Stream.Write(FData.n_operation,Sizeof(FData.n_operation));
  if FData.operation_type=lat_ListForSale then begin
    Stream.Write(FData.account_price,Sizeof(FData.account_price));
    Stream.Write(FData.account_to_pay,Sizeof(FData.account_to_pay));
    Stream.Write(FData.public_key.EC_OpenSSL_NID,Sizeof(FData.public_key.EC_OpenSSL_NID));
    TStreamOp.WriteAnsiString(Stream,FData.public_key.x);
    TStreamOp.WriteAnsiString(Stream,FData.public_key.y);
    TStreamOp.WriteAnsiString(Stream,TAccountComp.AccountKey2RawString(FData.new_public_key));
    Stream.Write(FData.locked_until_block,Sizeof(FData.locked_until_block));
  end;
  Stream.Write(FData.fee,Sizeof(FData.fee));
  TStreamOp.WriteAnsiString(Stream,FData.payload);
  TStreamOp.WriteAnsiString(Stream,FData.sign.r);
  TStreamOp.WriteAnsiString(Stream,FData.sign.s);
  Result := true;
end;

function TOpListAccount.GetSignerAccount: Cardinal;
begin
  Result := FData.account_signer;
end;

function TOpListAccount.GetDestinationAccount: Int64;
begin
  Result := FData.account_target;
end;

function TOpListAccount.GetSellerAccount: Int64;
begin
  Case FData.operation_type of
    lat_ListForSale : Result := FData.account_to_pay;
  else Result:=inherited GetSellerAccount;
  end;
end;

function TOpListAccount.toString: String;
begin
  case FData.operation_type of
    lat_ListForSale : begin
      if (FData.new_public_key.EC_OpenSSL_NID=CT_TECDSA_Public_Nul.EC_OpenSSL_NID) then begin
        Result := Format('List account %s for sale price %s locked until block:%d fee:%s (n_op:%d) payload size:%d',[
          TAccountComp.AccountNumberToAccountTxtNumber(FData.account_target), TAccountComp.FormatMoney(FData.account_price),
          FData.locked_until_block, TAccountComp.FormatMoney(FData.fee),
          FData.n_operation, Length(FData.payload)])
      end else begin
        Result := Format('List account %s for private sale price %s reserved for %s locked until block:%d fee:%s (n_op:%d) payload size:%d',[
          TAccountComp.AccountNumberToAccountTxtNumber(FData.account_target), TAccountComp.FormatMoney(FData.account_price),
          TAccountComp.GetECInfoTxt(FData.new_public_key.EC_OpenSSL_NID),
          FData.locked_until_block, TAccountComp.FormatMoney(FData.fee),
          FData.n_operation, Length(FData.payload)])
      end;
    end;
    lat_DelistAccount : begin
      Result := Format('Delist account %s for sale fee:%s (n_op:%d) payload size:%d',[
        TAccountComp.AccountNumberToAccountTxtNumber(FData.account_target), TAccountComp.FormatMoney(FData.fee),
          FData.n_operation, Length(FData.payload)])
    end;
  else Result := 'ERROR DEV 20170414-2';
  end;
end;

{ TOpListAccountForSale }

constructor TOpListAccountForSale.CreateListAccountForSale(account_signer, n_operation, account_target: Cardinal;
  account_price, fee: UInt64; account_to_pay: Cardinal;
  new_public_key: TAccountKey; locked_until_block: Cardinal; key: TECPrivateKey;
  payload: TRawBytes);
begin
  inherited Create;
  FData.account_signer := account_signer;
  FData.account_target := account_target;
  FData.operation_type := lat_ListForSale;
  FData.n_operation := n_operation;
  FData.account_price := account_price;
  FData.account_to_pay := account_to_pay;
  FData.fee := fee;
  FData.payload := payload;
  // V2: No need to store public key because it's at safebox. Saving at least 64 bytes!
  // FData.public_key := key.PublicKey;
  FData.new_public_key := new_public_key;
  FData.locked_until_block := locked_until_block;
  If Not DoSignOperation(key,FData) then begin
    TLog.NewLog(lterror,Classname,'Error signing a new list account for sale operation');
    FHasValidSignature := false;
  end else FHasValidSignature := true;
end;

function TOpListAccountForSale.IsDelist: Boolean;
begin
  Result := False;
end;

function TOpListAccountForSale.GetOpType: Byte;
begin
  Result := CT_Op_ListAccountForSale;
end;

function TOpListAccountForSale.GetTransactionData(Block,
  Affected_account_number: Cardinal;
  var TransactionData: TTransactionData): Boolean;
begin
  If IsPrivateSale then
  begin
    TransactionData.OpSubtype := CT_OpSubtype_ListAccountForPrivateSale;
    TransactionData.OperationTxt := 'List account ' +
      TAccountComp.AccountNumberToAccountTxtNumber
      (Data.account_target) +
      ' for private sale price ' + TAccountComp.FormatMoney
      (Data.account_price) + ' MCC pay to ' +
      TAccountComp.AccountNumberToAccountTxtNumber(Data.account_to_pay);
  end
  else
  begin
    TransactionData.OpSubtype := CT_OpSubtype_ListAccountForPublicSale;
    TransactionData.OperationTxt := 'List account ' +
      TAccountComp.AccountNumberToAccountTxtNumber(Data.account_target) + ' for sale price '
      + TAccountComp.FormatMoney(Data.account_price) + ' MCC pay to ' +
      TAccountComp.AccountNumberToAccountTxtNumber(
      Data.account_to_pay);
  end;
  TransactionData.newKey := Data.new_public_key;
  TransactionData.SellerAccount := GetSellerAccount;
  Result := true;
end;

{ TOpDelistAccountForSale }

constructor TOpDelistAccountForSale.CreateDelistAccountForSale(account_signer, n_operation, account_target: Cardinal; fee: UInt64; key: TECPrivateKey; payload: TRawBytes);
begin
  inherited Create;
  FData.account_signer := account_signer;
  FData.account_target := account_target;
  FData.operation_type := lat_DelistAccount;
  FData.n_operation := n_operation;
  FData.fee := fee;
  FData.payload := payload;
  If Not DoSignOperation(key,FData) then begin
    TLog.NewLog(lterror,Classname,'Error signing a delist account operation');
    FHasValidSignature := false;
  end else FHasValidSignature := true;
end;

function TOpDelistAccountForSale.IsDelist: Boolean;
begin
  Result := True;
end;

function TOpDelistAccountForSale.GetOpType: Byte;
begin
  Result := CT_Op_DelistAccount;
end;

function TOpDelistAccountForSale.GetTransactionData(Block,
  Affected_account_number: Cardinal;
  var TransactionData: TTransactionData): Boolean;
begin
  TransactionData.OpSubtype := CT_OpSubtype_DelistAccount;
  TransactionData.OperationTxt := 'Delist account ' +
    TAccountComp.AccountNumberToAccountTxtNumber(Data.account_target) +
    ' for sale';
  Result := true;
  TransactionData.OriginalPayload := GetOperationPayload;
  If TCrypto.IsHumanReadable(TransactionData.OriginalPayload) then
    TransactionData.PrintablePayload := TransactionData.OriginalPayload
  else
    TransactionData.PrintablePayload := TCrypto.ToHexaString(TransactionData.OriginalPayload);
  TransactionData.OperationHash := OperationHashValid(self, Block);
  if (Block < CT_Protocol_Upgrade_v2_MinBlock) then
  begin
    TransactionData.OperationHash_OLD := TTransaction.OperationHash_OLD(self, Block);
  end;
  TransactionData.valid := true;
end;

{ TOpBuyAccount }

initialization

RegisterOperationsClass;
end.
