{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2025 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit PetStore.Forms.Main;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.ExtCtrls, System.Actions, Winapi.Windows, Winapi.ShellAPI, System.JSON,

  WiRL.Configuration.Core,
  WiRL.Configuration.Neon,
  WiRL.Configuration.CORS,
  WiRL.Configuration.Errors,
  WiRL.Configuration.OpenAPI,
  WiRL.Core.Application,
  WiRL.Engine.REST,
  WiRL.http.Server,
  WiRL.http.Server.Indy,
  Neon.Core.Types,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,
  OpenAPI.Model.Classes,

  PetStore.Resources.OpenAPI;

type
  TMainForm = class(TForm)
    TopPanel: TPanel;
    StartButton: TButton;
    StopButton: TButton;
    MainActionList: TActionList;
    actStartServer: TAction;
    actStopServer: TAction;
    PortNumberEdit: TEdit;
    Label1: TLabel;
    btnDocumentation: TButton;
    actShowDocumentation: TAction;
    procedure actShowDocumentationExecute(Sender: TObject);
    procedure actShowDocumentationUpdate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actStartServerExecute(Sender: TObject);
    procedure actStartServerUpdate(Sender: TObject);
    procedure actStopServerExecute(Sender: TObject);
    procedure actStopServerUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private const
    ENG_PATH = 'api';
    APP_PATH = 'v3';
    API_PATH = 'openapi';
  private
    FEngine: TWiRLRESTEngine;
    FRESTServer: TWiRLServer;
    function ConfigureOpenAPIDocument: TOpenAPIDocument;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  PetStore.Entities,
  WiRL.Core.Exceptions,
  WiRL.http.Accept.MediaType;

{$R *.dfm}

function TMainForm.ConfigureOpenAPIDocument: TOpenAPIDocument;
begin
  Result := TOpenAPIDocument.Create(TOpenAPIVersion.v303);

  Result.Info.TermsOfService := 'https://swagger.io/terms/';
  Result.Info.Title := 'Swagger Petstore - OpenAPI 3.0';
  Result.Info.Version := '1.0.27';
  Result.Info.Description :=
    'This is a sample Pet Store Server based on the OpenAPI 3.0 specification. ' +
    'You can find out more about Swagger at [https://swagger.io](https://swagger.io). ' +
    'In the third iteration of the pet store, we''ve switched to the design first approach!' + sLineBreak +
    'You can now help us improve the API whether it''s by making changes to the definition itself or to the code.' + sLineBreak +
    'That way, with time, we can improve the API in general, and expose some of the new features in OAS3.';
  Result.Info.Contact.Email := 'apiteam@swagger.io';
  Result.Info.License.Name := 'Apache 2.0';
  Result.Info.License.Url :=  'https://www.apache.org/licenses/LICENSE-2.0.html';

  Result.SetExternalDocs('https://swagger.io', 'Find out more about Swagger');

  Result.AddServer('/api/v3', '');
  Result.AddServer('https://api.example.com/rest/app', 'Production Server');
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  LDocument: TOpenAPIDocument;
begin
  FRESTServer := TWiRLServer.Create(Self);

  LDocument := ConfigureOpenAPIDocument;

  FEngine :=
    FRESTServer.AddEngine<TWiRLRESTEngine>(ENG_PATH)
      .SetEngineName('RESTEngine');


  FEngine.AddApplication(APP_PATH)
    .SetAppName('PetStore')
    .SetResources('*')
    .SetFilters('*')

    .Plugin.Configure<IWiRLConfigurationErrors>
      .SetErrorClass(EWiRLWebApplicationException)
      .SetErrorCase(TNeonCase.CamelCase)
      .SetErrorMediaType(TMediaType.APPLICATION_JSON)
    .ApplyConfig

    .Plugin.Configure<IWiRLConfigurationNeon>
      .SetUseUTCDate(True)
      .SetMemberCase(TNeonCase.CamelCase)
    .ApplyConfig

    .Plugin.Configure<IWiRLConfigurationCORS>
      .SetOrigin('*')
      .SetMethods('GET,POST,PUT,DELETE')
      .SetHeaders('Content-Type,Authorization')
    .ApplyConfig

    .Plugin.Configure<IWiRLConfigurationOpenAPI>
      // Set the OpenAPI resource (to skip it in the documentation generation)
      .SetOpenAPIResource(TDocumentationResource)

      // Set the Delphi XML documentation output directory (Project -> Options -> Compiler)
      .SetXMLDocFolder('{AppPath}\..\dist\XML')

      // Set the Delphi JSON (OAS format) documentation output directory
      .SetOASDocFolder('{AppPath}\..\dist\OAS')

      // Set the folder to the html UI assets location
      .SetGUIDocFolder('{AppPath}\..\dist\UI')
      //.SetGUIDocFolder('{AppPath}\..\dist\ReDoc')

      // Set the (optional) API logo
      .SetAPILogo('api-logo.png')

      // Set the OpenAPI document for the OpenAPI engine to fill
      .SetAPIDocument(LDocument)

    .ApplyConfig
  ;

  actStartServer.Execute;
end;

procedure TMainForm.actShowDocumentationExecute(Sender: TObject);
var
  LUrl: string;
begin
  LUrl := Format('http://localhost:%d/%s/%s/%s/', [FRESTServer.Port, ENG_PATH, APP_PATH, API_PATH]);
  ShellExecute(Handle, 'open', PChar(LUrl), '', '', SW_NORMAL);
end;

procedure TMainForm.actShowDocumentationUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FRESTServer) and FRESTServer.Active;
end;

procedure TMainForm.actStartServerExecute(Sender: TObject);
begin
  FRESTServer.Port := StrToIntDef(PortNumberEdit.Text, 8080);
  if not FRESTServer.Active then
    FRESTServer.Active := True;
end;

procedure TMainForm.actStartServerUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := not Assigned(FRESTServer) or not FRESTServer.Active;
end;

procedure TMainForm.actStopServerExecute(Sender: TObject);
begin
  FRESTServer.Active := False;
end;

procedure TMainForm.actStopServerUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FRESTServer) and FRESTServer.Active;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FRESTServer.Free;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  actStopServer.Execute;
end;

end.
