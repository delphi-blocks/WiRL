{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2025 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Forms.Main;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.ExtCtrls, System.Actions, Winapi.Windows, Winapi.ShellAPI, System.JSON,

  WiRL.Configuration.Core,
  WiRL.Configuration.Neon,
  WiRL.Configuration.CORS,
  WiRL.Configuration.OpenAPI,
  WiRL.Core.Exceptions,
  Neon.Core.Types,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON.Schema,
  WiRL.Core.Application,
  WiRL.Engine.REST,
  WiRL.Engine.FileSystem,
  WiRL.http.Server,
  WiRL.http.Server.Indy,
  OpenAPI.Model.Classes,
  Server.Resources.OpenAPI;

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
    ENG_PATH = 'rest';
    APP_PATH = 'app';
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
  System.TypInfo,
  WiRL.Core.Utils,
  WiRL.http.Accept.MediaType,
  WiRL.Core.Metadata.XMLDoc,
  WiRL.Core.OpenAPI.Resource;

{$R *.dfm}

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

function TMainForm.ConfigureOpenAPIDocument: TOpenAPIDocument;
var
  LExtensionObject: TJSONObject;
begin
  Result := TOpenAPIDocument.Create(TOpenAPIVersion.v303);

  Result.Info.TermsOfService := 'http://api.example.com/terms/';
  Result.Info.Title := 'WiRL OpenAPI Integration Demo';
  Result.Info.Version := '1.0.2';
  Result.Info.Description := 'This is a **demo API** to test [WiRL](https://github.com/delphi-blocks/WiRL) OpenAPI documentation features';
  Result.Info.Contact.Name := 'Paolo Rossi';
  Result.Info.Contact.Email := 'paolo@mail.it';
  Result.Info.License.Name := 'Apache 2.0';
  Result.Info.License.Url :=  'http://www.apache.org/licenses/LICENSE-2.0.html';
  Result.AddServer('http://localhost:8080/rest/app', 'Testing Server');
  Result.AddServer('https://api.example.com/rest/app', 'Production Server');

  // Shows how to use Extensions (for ReDoc UI)
  LExtensionObject := TJSONObject.Create;
  LExtensionObject.AddPair('url', 'http://localhost:8080/rest/app/openapi/api-logo.png');
  LExtensionObject.AddPair('backgroundColor', '#FFFFFF');
  LExtensionObject.AddPair('altText', 'API Logo');
  Result.Info.Extensions.Add('x-logo', LExtensionObject);


  //var res := Result.Components.AddResponse('BadRequest', 'Bad Request');
  //var mt := res.AddMediaType(TMediaType.APPLICATION_JSON);
  //mt.Schema.SetSchemaReference()
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FRESTServer.Free;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  actStopServer.Execute;
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
      .SetAppName('demo')
      .SetResources('Server.Resources.Demo.*')
      .SetResources('Server.Resources.Customer.*')
      .SetResources('Server.Resources.OpenAPI')
      .SetFilters('*')

      .Plugin.Configure<IWiRLConfigurationNeon>
        .SetUseUTCDate(True)
        .SetMemberCase(TNeonCase.Unchanged)
      .ApplyConfig

      .Plugin.Configure<IWiRLConfigurationCORS>
        .SetOrigin('*')
        .SetMethods('GET,POST,PUT,DELETE')
        .SetHeaders('Content-Type,Authorization')
      .ApplyConfig

      .Plugin.Configure<IWiRLConfigurationOpenAPI>
        .SetNeonConfiguration(
          TNeonConfiguration.Camel
            .Rules.ForClass<Exception>
              .SetIgnoreMembers([
                'BaseException',
                'HelpContext',
                'InnerException',
                'StackTrace',
                'StackInfo'])
          .ApplyConfig
        )

        // Set the OpenAPI resource (to skip it in the documentation generation)
        .SetOpenAPIResource(TDocumentationResource)

        // Set the Delphi XML documentation output directory (Project -> Options -> Compiler)
        .SetXMLDocFolder('{AppPath}\..\..\Docs')

        // Set the folder to the html UI assets location
        .SetGUIDocFolder('{AppPath}\..\..\UI')
        //.SetGUIDocFolder('{AppPath}\..\..\ReDoc')

        // Set the (optional) API logo
        .SetAPILogo('api-logo.png')

        // Set the OpenAPI document for the OpenAPI engine to fill
        .SetAPIDocument(LDocument)

        .SetAPIDocumentCallback(
          procedure(ADocument: TOpenAPIDocument)
          begin
            ADocument.Info.Title := 'WiRL OpenAPI Integration Demo (Changed)';

            ADocument.AddTag('callback', 'User added content');

            var res := ADocument.AddPath('/callback');
            res.Description := 'This is a test for the callback';
            res.Summary := 'Sample';

            var op := res.AddOperation(TOperationType.Get);
            op.Summary := 'Get Sample Data';
            op.Description := 'Sample Data description';
          end
        )

      .ApplyConfig
  ;

  actStartServer.Execute;
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

end.
