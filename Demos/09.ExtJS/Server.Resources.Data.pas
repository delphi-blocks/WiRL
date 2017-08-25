{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Resources.Data;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.DateUtils, Data.DB,
  Variants, Windows, Datasnap.DBClient, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.SQLite,
  FireDAC.Stan.ExprFuncs, FireDAC.Stan.Param,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.VCLUI.Wait,
  FireDAC.Comp.UI, FireDAC.Comp.DataSet, FireDAC.Comp.Client,

  WiRL.Core.JSON,
  WiRL.Core.Registry,
  WiRL.Core.Attributes,
  WiRL.http.Accept.MediaType,
  WiRL.http.URL,
  WiRL.http.Request,
  WiRL.Core.MessageBody.Default,
  WiRL.Data.MessageBody.Default,
  WiRL.Core.Exceptions,
  WiRL.Data.Resolver, FireDAC.Phys.SQLiteDef;

  // In Delphi versions earlier than 10 Seattle please remove FireDAC.Phys.SQLiteDef


type
  [Path('/main')]
  TMainModule = class(TDataModule)
    FDConnection: TFDConnection;
    qryEmployee: TFDQuery;
    qryEmployeeEMP_NO: TSmallintField;
    qryEmployeeFIRST_NAME: TStringField;
    qryEmployeeLAST_NAME: TStringField;
    qryEmployeePHONE_EXT: TStringField;
    qryEmployeeHIRE_DATE: TSQLTimeStampField;
    qryEmployeeDEPT_NO: TStringField;
    qryEmployeeJOB_CODE: TStringField;
    qryEmployeeJOB_GRADE: TSmallintField;
    qryEmployeeJOB_COUNTRY: TStringField;
    qryEmployeeSALARY: TBCDField;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    qryEmpNoGen: TFDQuery;
    procedure DataModuleCreate(Sender: TObject);
  private
//    [Context] FRequest: TWiRLRequest;
  public
    [GET, Path('/employee/')]
    function Employee(): TDataSet;

    [POST, Path('/employee/')]
    function InsertEmployee([BodyParam] Json: TJSONValue): TJSONObject;

    [PUT, Path('/employee/{Id}')]
    function UpdateEmployee([BodyParam] Json: TJSONValue): TJSONObject;

    [DELETE, Path('/employee/{Id}')]
    function DeleteEmployee([PathParam] Id: Integer; [BodyParam] Json: TJSONValue): TJSONObject;
  end;

var
  MainModule: TMainModule;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

{ TMainModule }

function TMainModule.Employee: TDataSet;
begin
  Result := qryEmployee;
end;

function TMainModule.InsertEmployee(Json: TJSONValue): TJSONObject;
begin
  raise EWiRLNotImplementedException.Create('Not yet implemented');
end;

procedure TMainModule.DataModuleCreate(Sender: TObject);
const
  DatabaseName = 'data.db';
begin
  inherited;
  FDConnection.DriverName := 'SQLite';
  FDConnection.Params.Add('Database=' + DatabaseName);
  FDConnection.Params.Add('SQLiteAdvanced=page_size=4096');
  FDConnection.Connected := True;
end;

function TMainModule.DeleteEmployee(Id: Integer; Json: TJSONValue): TJSONObject;
begin
  TWiRLResolver.DeleteDataSet(qryEmployee, Id);
  Result := TJSONObject.Create(TJSONPair.Create('success', TJSONTrue.Create));
end;

function TMainModule.UpdateEmployee([BodyParam] Json: TJSONValue): TJSONObject;
begin
  TWiRLResolver.UpdateDataSet(qryEmployee, Json);
  Result := TJSONObject.Create(TJSONPair.Create('success', TJSONTrue.Create));
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TMainModule>(
    function: TObject
    begin
      Result := TMainModule.Create(nil);
    end
  );

end.
