{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2021 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Forms.Main;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.ExtCtrls, System.Diagnostics, System.Actions, IdContext,

  WiRL.Configuration.Core,
  WiRL.Configuration.Neon,
  WiRL.Configuration.CORS,
  WiRL.Configuration.OpenAPI,
  Neon.Core.Types,
  WiRL.Core.Application,
  WiRL.Core.Engine,
  WiRL.http.FileSystemEngine,
  WiRL.http.Server,
  WiRL.http.Server.Indy,

  Server.Resources.Swagger, Xml.xmldom, Xml.XMLIntf, Xml.XMLDoc;

type
  TMainForm = class(TForm)
    TopPanel: TPanel;
    StartButton: TButton;
    StopButton: TButton;
    MainActionList: TActionList;
    StartServerAction: TAction;
    StopServerAction: TAction;
    PortNumberEdit: TEdit;
    Label1: TLabel;
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    XMLDocument1: TXMLDocument;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure StartServerActionExecute(Sender: TObject);
    procedure StartServerActionUpdate(Sender: TObject);
    procedure StopServerActionExecute(Sender: TObject);
    procedure StopServerActionUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FEng: TWiRLEngine;
    FRESTServer: TWiRLServer;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  WiRL.Core.Utils,
  WiRL.Core.Metadata.XMLDoc,
  WiRL.Core.OpenAPI.Resource;

{$R *.dfm}

procedure TMainForm.Button1Click(Sender: TObject);
var
  LContext: TWiRLXMLDocContext;
  LApp: TWiRLApplication;
begin
  LApp := FEng.GetApplicationByName('demo');
  LContext.Proxy := LApp.Proxy;
  LContext.XMLDocFolder := TWiRLTemplatePaths.Render('{AppPath}\..\..\Docs');
  TWiRLProxyEngineXMLDoc.Process(LContext);
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  LXMLDoc: TWiRLProxyEngineXMLDoc;
  LContext: TWiRLXMLDocContext;
  LApp: TWiRLApplication;
var
  LDoc: IXMLDocument;
  //LDevNotes, LNodeClass: IXMLNode;
begin
  LApp := FEng.GetApplicationByName('demo');
  LContext.Proxy := LApp.Proxy;
  LContext.XMLDocFolder := TWiRLTemplatePaths.Render('{AppPath}\..\..\Docs');

  LXMLDoc := TWiRLProxyEngineXMLDoc.Create(LContext);
  try
    LDoc := LXMLDoc.LoadXMLUnit('Server.Resources.Demo');
    //LNodeClass :=
    //Memo1.Lines.Text := LXMLDoc.FindClassWithAttribute(LDoc.DocumentElement, 'TParametersResource');
  finally
    LXMLDoc.Free;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FRESTServer.Free;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StopServerAction.Execute;
end;

/// <summary>
/// Bla *bla* bla
/// </summary>
procedure TMainForm.FormCreate(Sender: TObject);
begin
  FRESTServer := TWiRLServer.Create(Self);

  /// <summary>
  /// subsubsub
  /// </summary>
  FEng :=
  FRESTServer.AddEngine<TWiRLEngine>('/rest')
    .SetEngineName('RESTEngine');

  FEng.AddApplication('/app')
      .SetAppName('demo')
      // Test for namespaces
      .SetResources('Server.Resources.Demo.*')
      .SetResources('Server.Resources.Customer.*')
      //.SetResources('Server.Resources.Swagger.*')
      .SetFilters('*')

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
        .SetUseSwaggerUI()
        .SetOpenAPIResource(TDocumentationResource)
        .SetXMLDocFolder('{AppPath}\..\..\Docs')
        .SetSwaggerUIFolder('{AppPath}\..\..\UI')
        .SetDocumentationFolder('{AppPath}\..\..\Documentation')
        // Properties for the Swagger document
        .SetAPILogo('api-logo.png')
        .SetAPITitle('WiRL Swagger Integration Demo')
        .SetAPIVersion('1.0.0')
        .SetAPIDescription('This is a **demo API** to test [WiRL](https://github.com/delphi-blocks/WiRL) OpenAPI documentation features')
        .AddAPIServer('http://localhost:8080/rest/app', 'Testing Server')
        .AddAPIServer('https://api.example.com/rest/app', 'Production Server')
      .ApplyConfig
  ;

  StartServerAction.Execute;
end;

procedure TMainForm.StartServerActionExecute(Sender: TObject);
begin
  FRESTServer.Port := StrToIntDef(PortNumberEdit.Text, 8080);
  if not FRESTServer.Active then
    FRESTServer.Active := True;
end;

procedure TMainForm.StartServerActionUpdate(Sender: TObject);
begin
  StartServerAction.Enabled := not Assigned(FRESTServer) or not FRESTServer.Active;
end;

procedure TMainForm.StopServerActionExecute(Sender: TObject);
begin
  FRESTServer.Active := False;
end;

procedure TMainForm.StopServerActionUpdate(Sender: TObject);
begin
  StopServerAction.Enabled := Assigned(FRESTServer) and FRESTServer.Active;
end;

end.
