unit Server.Resources.Data;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.DateUtils, Data.DB,
  Variants, Windows, Datasnap.DBClient, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs, FireDAC.Stan.Param,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.VCLUI.Wait,
  FireDAC.Comp.UI, FireDAC.Comp.DataSet, FireDAC.Comp.Client

  , MARS.Core.JSON
  , MARS.Core.Request
  , MARS.Core.Response

  , MARS.Core.Registry
  , MARS.Core.Attributes
  , MARS.Core.MediaType
  , MARS.Core.URL
  , MARS.Core.MessageBodyWriters
  , MARS.Core.Token
  , MARS.Core.Token.Resource
  , MARS.Data.Resolver
  ;


type

// http://localhost:8080/rest/default/main/employee

  [Path('/main')]
  TMainModule = class(TDataModule)
    FBConnection: TFDConnection;
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
  public
    [GET, Path('/employee/'){, RolesAllowed('standard')}]
    function Employee(): TDataSet;

    [POST, Path('/employee/'), Produces(TMediaType.APPLICATION_JSON)]
    function InsertEmployee([BodyParam] Json: TJSONValue): TJSONObject;

    [PUT, Path('/employee/{Id}'), Produces(TMediaType.APPLICATION_JSON)]
    function UpdateEmployee(Id :Integer; [BodyParam] Json: TJSONValue): TJSONObject;

    [DELETE, Path('/employee/{Id}'), Produces(TMediaType.APPLICATION_JSON)]
    function DeleteEmployee([PathParam] Id :Integer; [BodyParam] Json: TJSONValue): TJSONObject;
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
  raise Exception.Create('Not yet implemented');
end;

function TMainModule.DeleteEmployee(Id: Integer; Json: TJSONValue): TJSONObject;
begin
  TMARSResolver.DeleteDataSet(qryEmployee, Id);
  Result := TJSONObject.Create(TJSONPair.Create('success', TJSONBool.Create(True)));
end;

function TMainModule.UpdateEmployee(Id :Integer; Json: TJSONValue): TJSONObject;
begin
  TMARSResolver.UpdateDataSet(qryEmployee, Json);
  Result := TJSONObject.Create(TJSONPair.Create('success', TJSONBool.Create(True)));
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<TMainModule>(
    function: TObject
    begin
      Result := TMainModule.Create(nil);
    end
  );

end.
