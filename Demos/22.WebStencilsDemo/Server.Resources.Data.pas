{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
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
  System.IOUtils, System.Rtti,

  WiRL.Core.JSON,
  WiRL.Rtti.Utils,
  WiRL.Core.Registry,
  WiRL.Core.Attributes,
  WiRL.http.Accept.MediaType,
  WiRL.http.URL,
  WiRL.http.Request,
  WiRL.Core.MessageBody.Default,
  WiRL.Data.MessageBody.Default,
  WiRL.Core.Exceptions,
  WiRL.Data.Resolver, FireDAC.Phys.SQLiteDef, FireDAC.Phys.SQLiteWrapper.Stat,
  Web.HTTPApp, Web.Stencils;

  // In Delphi versions earlier than 10 Seattle please remove FireDAC.Phys.SQLiteDef


type
  TPerson = class
  private
    FName: string;
    FAge: Integer;
  public
    property Name: string read FName write FName;
    property Age: Integer read FAge write FAge;

    constructor Create(const AName: string; AAge: Integer);
  end;

  [Path('/employee')]
  TEmployeeModule = class(TDataModule)
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
    //[Context] FRequest: TWiRLRequest;
  public
    [GET]
    [Produces(TMediaType.APPLICATION_JSON)]
    [Produces(TMediaType.TEXT_HTML)]
    function Employee(): TDataSet;

    [GET, Path('{Id}')]
    [Produces(TMediaType.TEXT_HTML)]
    [Produces(TMediaType.APPLICATION_JSON)]
    function EmployeeById([PathParam('Id')] AId: Integer): TDataSet;

    [POST]
    function InsertEmployee([BodyParam] Json: TJSONValue): TJSONObject;

    [PUT, Path('{Id}')]
    function UpdateEmployee([BodyParam] Json: TJSONValue): TJSONObject;

    [DELETE, Path('{Id}')]
    function DeleteEmployee([PathParam] Id: Integer; [BodyParam] Json: TJSONValue): TJSONObject;
  end;

  [Path('/person')]
  TPersonResource = class(TObject)
  public
    [GET]
    [Produces(TMediaType.TEXT_HTML)]
    [Produces(TMediaType.APPLICATION_JSON)]
    function GetPerson(): TPerson;
  end;

var
  EmployeeModule: TEmployeeModule;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

{ TMainModule }

function TEmployeeModule.Employee: TDataSet;
begin
  Result := qryEmployee;
end;

function TEmployeeModule.EmployeeById(AId: Integer): TDataSet;
begin
  qryEmployee.MacroByName('filter').Value := ' WHERE EMP_NO = ' + AId.ToString;
  Result := qryEmployee;
end;

function TEmployeeModule.InsertEmployee(Json: TJSONValue): TJSONObject;
begin
  raise EWiRLNotImplementedException.Create('Not yet implemented');
end;

procedure TEmployeeModule.DataModuleCreate(Sender: TObject);
const
  DatabaseName = 'data.db';
begin
  inherited;
  FDConnection.DriverName := 'SQLite';
  FDConnection.Params.Add('Database=' + DatabaseName);
  FDConnection.Params.Add('SQLiteAdvanced=page_size=4096');
  FDConnection.Connected := True;
end;

function TEmployeeModule.DeleteEmployee(Id: Integer; Json: TJSONValue): TJSONObject;
begin
  TWiRLResolver.DeleteDataSet(qryEmployee, Id);
  Result := TJSONObject.Create(TJSONPair.Create('success', TJSONTrue.Create));
end;

function TEmployeeModule.UpdateEmployee([BodyParam] Json: TJSONValue): TJSONObject;
begin
  TWiRLResolver.UpdateDataSet(qryEmployee, Json);
  Result := TJSONObject.Create(TJSONPair.Create('success', TJSONTrue.Create));
end;

{ TPerson }

constructor TPerson.Create(const AName: string; AAge: Integer);
begin
  inherited Create;
  FName := AName;
  FAge := AAge;
end;

{ TPersonResource }

function TPersonResource.GetPerson: TPerson;
begin
  Result := TPerson.Create('Luca', 42);
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TEmployeeModule>(
    function: TObject
    begin
      Result := TEmployeeModule.Create(nil);
    end
  );

  TWiRLResourceRegistry.Instance.RegisterResource<TPersonResource>();

end.
