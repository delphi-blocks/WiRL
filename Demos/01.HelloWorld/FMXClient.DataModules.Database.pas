unit FMXClient.DataModules.Database;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  WiRL.Client.CustomResource, WiRL.Client.Resource, WiRL.Client.Application,
  WiRL.http.Client, WiRL.http.Accept.MediaType;

type
  TDatabaseModule = class(TDataModule)
    WiRLClient1: TWiRLClient;
    WiRLClientApplication1: TWiRLClientApplication;
    HelloWorldResource: TWiRLClientResource;
    EchoStringResource: TWiRLClientResource;
    ReverseStringResource: TWiRLClientResource;
    PostStreamResource: TWiRLClientResource;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    procedure GetDBData(ADataSet: TDataSet);
  end;

var
  DatabaseModule: TDatabaseModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

procedure TDatabaseModule.DataModuleCreate(Sender: TObject);
begin
  WiRLClientApplication1.SetReaders('*.*');
  WiRLClientApplication1.SetWriters('*.*');
end;

procedure TDatabaseModule.GetDBData(ADataSet: TDataSet);
begin
  WiRLClientApplication1
    .Resource('database/db')
    .Accept(TMediaType.APPLICATION_JSON)
    .Get(ADataSet);
end;

end.
