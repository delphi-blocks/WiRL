{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Forms.Main;

interface

uses
  System.Classes, System.SysUtils,
  Winapi.Messages, Winapi.ShellApi, Winapi.Windows,
  Vcl.Forms, Vcl.ActnList, Vcl.ComCtrls,
  Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls, System.Diagnostics, System.Actions,

  WiRL.Engine.REST,
  WiRL.Engine.WebServer,
  WiRL.Core.Application,
  WiRL.http.Server,
  WiRL.http.Server.Indy;

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
    ButtonWebDemo: TButton;
    Button1: TButton;
    procedure StartServerActionExecute(Sender: TObject);
    procedure StartServerActionUpdate(Sender: TObject);
    procedure StopServerActionExecute(Sender: TObject);
    procedure StopServerActionUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonWebDemoClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FServer: TWiRLServer;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  WiRL.Core.JSON,
  WiRL.Rtti.Utils,
  WiRL.Configuration.CORS;


procedure TMainForm.Button1Click(Sender: TObject);
begin
  ShellExecute(Handle, 'open', 'DemoChunksClient.exe', '', '', SW_NORMAL);
end;

procedure TMainForm.ButtonWebDemoClick(Sender: TObject);
const
  LTemplateURL = 'http://localhost:%d/';
begin
  ShellExecute(Handle, 'open', PChar(Format(LTemplateURL, [FServer.Port])), '', '', SW_NORMAL);
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StopServerAction.Execute;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  StartServerAction.Execute;
end;

procedure TMainForm.StartServerActionExecute(Sender: TObject);
begin
  // Create http server
  FServer := TWiRLServer.Create(nil);

  // Server configuration
  FServer
    .SetPort(StrToIntDef(PortNumberEdit.Text, 8080))

    // ReST engine configuration
    .AddEngine<TWiRLRESTEngine>('/rest')
      .SetEngineName('WiRL ContentType Demo')

      // Application configuration
      .AddApplication('/app')
        .SetAppName('Content App')
        .SetWriters('*')
        .SetReaders('*')
        .SetResources('*')

      .Plugin.Configure<IWiRLConfigurationCORS>
        .SetOrigin('*')
        .SetMethods('HEAD, GET, PUT, POST, DELETE, OPTIONS')
        .SetHeaders('Accept, Content-Type, Content-Encoding, Authorization');

  // File server engine configuration
  FServer
    .AddEngine<TWiRLWebServerEngine>('/')
      .SetEngineName('WiRL WebServer Engine')
      .SetRootFolder('{AppPath}\www');

  if not FServer.Active then
    FServer.Active := True;
end;

procedure TMainForm.StartServerActionUpdate(Sender: TObject);
begin
  StartServerAction.Enabled := (FServer = nil) or (FServer.Active = False);
end;

procedure TMainForm.StopServerActionExecute(Sender: TObject);
begin
  FServer.Active := False;
  FServer.Free;
end;

procedure TMainForm.StopServerActionUpdate(Sender: TObject);
begin
  StopServerAction.Enabled := Assigned(FServer) and (FServer.Active = True);
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
