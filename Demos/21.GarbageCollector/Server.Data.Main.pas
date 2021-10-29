{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2021 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Data.Main;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.FB,
  FireDAC.Phys.FBDef, Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Param,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Comp.DataSet,
  FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.VCLUI.Wait,

  WiRL.Core.GarbageCollector;

type
  TDataMain = class(TDataModule)
    qryTestSQL: TFDQuery;
    FDConnection1: TFDConnection;
    DataBaseConnection: TFDConnection;
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    function GetCustomers1: TFDQuery;
    function GetCustomers2(AGC: TWiRLGarbageCollector): TFDQuery;
    function GetCustomers3(AGC: TWiRLGarbageCollector): TFDQuery;
  end;

var
  DataMain: TDataMain;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TDataMain.DataModuleCreate(Sender: TObject);
var
  LParams: TStrings;
begin
  LParams := TStringList.Create;
  try
    LParams.Add('Database=C:\Users\Public\Documents\Embarcadero\Studio\19.0\Samples\data\FDDemo.sdb');
    LParams.Add('Pooled=True');
    FDManager.AddConnectionDef('Demo_Pooled', 'SQLite', LParams);
    //FDManager.ConnectionDefs.ConnectionDefByName().Params.Database
  finally
    LParams.Free;
  end;

  DatabaseConnection.ConnectionDefName := 'Demo_Pooled';
  DatabaseConnection.Connected := True;

  FDManager.Active := True;
end;

procedure TDataMain.DataModuleDestroy(Sender: TObject);
begin
  FDManager.Active := False;
end;

function TDataMain.GetCustomers1: TFDQuery;
var
  LConn: TFDConnection;
begin
  // In this scenario, I create the FDQuery first and I assign the query as Owner
  // for other components.
  // This way when the query is destructed the other components will too.

  Result := TFDQuery.Create(nil);
  LConn  := TFDConnection.Create(Result);
  try
    Result.Connection := LConn;
    LConn.ConnectionDefName := 'Demo_Pooled';

    Result.SQL.Text := 'select * from Customers';
    Result.Open;
  except
    LConn.Free;
    Result.Free;
    raise;
  end;
end;

function TDataMain.GetCustomers2(AGC: TWiRLGarbageCollector): TFDQuery;
var
  LConn: TFDConnection;
begin
  // In this scenario I use the GarbageCollector passed as parameter
  // to add the components I want destroyed at the end of the request.

  // ! Remember the result will always be destroyed automatically

  Result := TFDQuery.Create(nil);
  LConn  := TFDConnection.Create(nil);
  try
    AGC.AddGarbage(LConn);
    Result.Connection := LConn;
    LConn.ConnectionDefName := 'Demo_Pooled';

    Result.SQL.Text := 'select * from Customers';
    Result.Open;
  except
    LConn.Free;
    Result.Free;
    raise;
  end;
end;

function TDataMain.GetCustomers3(AGC: TWiRLGarbageCollector): TFDQuery;
var
  LConn: TFDConnection;
begin
  // In this scenario I use the GarbageCollector passed as parameter
  // as the Owner to all of the components I create. Then when the GC gets
  // destroyed (at the end of the request) all the components are freed too.

  // ! Remember the result will always be destroyed automatically

  Result := TFDQuery.Create(nil);
  LConn  := TFDConnection.Create(AGC);
  try
    Result.Connection := LConn;
    LConn.ConnectionDefName := 'Demo_Pooled';

    Result.SQL.Text := 'select * from Customers';
    Result.Open;
  except
    LConn.Free;
    Result.Free;
    raise;
  end;
end;

end.
