unit DBModule;

interface

uses
  System.SysUtils, System.Classes, Data.DB, ZAbstractRODataset,
  ZAbstractDataset, ZAbstractTable, ZDataset, ZAbstractConnection, ZConnection,
  ZSqlUpdate;

type
  TMicroCoinData = class(TDataModule)
    KeysConnection: TZConnection;
    KeysTable: TZTable;
    KeysTableid: TLargeintField;
    KeysTablekey_type: TIntegerField;
    KeysTablex: TWideStringField;
    KeysTabley: TWideStringField;
    KeysTabled: TWideStringField;
    KeysTableencryption_mode: TIntegerField;
    KeysTablekey_name: TWideStringField;
    ZUpdateSQL1: TZUpdateSQL;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MicroCoinData: TMicroCoinData;

implementation

uses UFolderHelper;

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TMicroCoinData.DataModuleCreate(Sender: TObject);
begin
  KeysConnection.Database := TFolderHelper.GetMicroCoinDataFolder+'\microcoin.keys';
  KeysConnection.Connect;
  if not KeysTable.Exists then
  KeysConnection.ExecuteDirect(
'create table keys('+
  'id integer not null,'+
  'key_type unsigned smallint not null,'+
  'key_name varchar(255) null,'+
 	'x varchar(255),'+
	'y varchar(255),'+
	'd varchar(255) null,'+
  'encryption_mode unsigned smallint default 0,'+
'primary key(id))');
  KeysTable.Active := true;
end;

end.
