{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2024 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Forms.Main;

interface

uses
  System.Classes, System.SysUtils, Winapi.Windows, Vcl.Forms, Vcl.ActnList,
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls,
  System.Diagnostics, System.Actions, Winapi.ShellAPI,

  WiRL.Engine.REST,
  WiRL.Engine.WebServer,
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
    Button1: TButton;
    TestAction: TAction;
    procedure StartServerActionExecute(Sender: TObject);
    procedure StartServerActionUpdate(Sender: TObject);
    procedure StopServerActionExecute(Sender: TObject);
    procedure StopServerActionUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TestActionExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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
  WiRL.Rtti.Utils;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Create http server
  FServer := TWiRLServer.Create(nil);

  // Server configuration
  FServer
    .SetPort(StrToIntDef(PortNumberEdit.Text, 8080))
    // Engine configuration
    .AddEngine<TWiRLWebServerEngine>('/')
      .SetEngineName('WiRL WebServer Engine')
      .SetRootFolder('{AppPath}\www')
      .AddAlias('/images', '{AppPath}\www\img')
  ;

  StartServerAction.Execute;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  StopServerAction.Execute;
  FServer.Free;
end;

procedure TMainForm.StartServerActionExecute(Sender: TObject);
begin
  if not FServer.Active then
  begin
    FServer.Port := StrToIntDef(PortNumberEdit.Text, 8080);
    FServer.Active := True;
  end;
end;

procedure TMainForm.StartServerActionUpdate(Sender: TObject);
begin
  StartServerAction.Enabled := (FServer = nil) or (FServer.Active = False);
end;

procedure TMainForm.StopServerActionExecute(Sender: TObject);
begin
  if FServer.Active then
    FServer.Active := False;
end;

procedure TMainForm.StopServerActionUpdate(Sender: TObject);
begin
  StopServerAction.Enabled := Assigned(FServer) and (FServer.Active = True);
end;

procedure TMainForm.TestActionExecute(Sender: TObject);
const
  LTemplateURL = 'http://localhost:%d/';
begin
  ShellExecute(Handle, 'open', PChar(Format(LTemplateURL, [FServer.Port])), '', '', SW_NORMAL);
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
