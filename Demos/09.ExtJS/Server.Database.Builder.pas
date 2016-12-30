{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Database.Builder;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.FB,
  FireDAC.Phys.FBDef, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, FireDAC.VCLUI.Wait, FireDAC.Comp.UI, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs, FireDAC.Comp.ScriptCommands,
  FireDAC.Stan.Util, FireDAC.Comp.Script;

type
  TDatabaseBuilder = class(TDataModule)
    FDConnection: TFDConnection;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDScript1: TFDScript;
  private
    FCreateDatabase :Boolean;
    procedure DoInitialize;
  public
    class procedure Initialize;
    constructor Create(AOwner: TComponent); override;
  end;

var
  DatabaseBuilder: TDatabaseBuilder;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

{ TTDatabaseBuilder }

constructor TDatabaseBuilder.Create(AOwner: TComponent);
const
  DatabaseName = 'data.db';
begin
  inherited;
  FCreateDatabase := not FileExists(DatabaseName);
  FDConnection.DriverName := 'SQLite';
  FDConnection.Params.Add('Database=' + DatabaseName);
  FDConnection.Params.Add('SQLiteAdvanced=page_size=4096');
  FDConnection.Connected := True;
end;

procedure TDatabaseBuilder.DoInitialize;
begin
  if FCreateDatabase then
    FDScript1.ExecuteAll;
end;

class procedure TDatabaseBuilder.Initialize;
var
  DatabaseBuilder :TDatabaseBuilder;
begin
  DatabaseBuilder := TDatabaseBuilder.Create(nil);
  try
    DatabaseBuilder.DoInitialize;
  finally
    DatabaseBuilder.Free;
  end;
end;

end.
