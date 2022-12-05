{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.MainData;

interface

uses
  System.SysUtils, System.Classes,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, Data.DB, FireDAC.Comp.Client, FireDAC.Phys.FB,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, FireDAC.Comp.DataSet, FireDAC.VCLUI.Wait, FireDAC.Comp.UI,
  FireDAC.Phys.SQLite, FireDAC.Stan.ExprFuncs,

  WiRL.Data.FireDAC.DataModule,
  WiRL.Core.Attributes,
  WiRL.http.URL,
  WiRL.Core.Auth.Context, FireDAC.Phys.SQLiteDef,
  FireDAC.Phys.SQLiteWrapper.Stat;

type
  [Path('/maindata')]
  TMainDataResource = class(TWiRLFDDataModuleResource)
    FDConnection1: TFDConnection;
    employee: TFDQuery;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    customers: TFDQuery;
  private
  public
    [GET, Path('/array')]
    function DataSetArray: TArray<TDataSet>;

    [GET, Path('/employee')]
    function EmployeeDataSet: TDataSet;
  end;

implementation

{$R *.dfm}

uses
  WiRL.Core.Registry;

{ TMainDataResource }

function TMainDataResource.EmployeeDataSet: TDataSet;
begin
  Result := employee;
end;

function TMainDataResource.DataSetArray: TArray<TDataSet>;
begin
  Result := [employee, customers];
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TMainDataResource>(
    function: TObject
    begin
      Result := TMainDataResource.Create(nil);
    end
  );

end.
