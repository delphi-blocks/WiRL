{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
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

  WiRL.Core.Engine,
  WiRL.http.Server.Indy,
  WiRL.Core.Application;

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
    FServer: TWiRLhttpServerIndy;
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
  StartServerAction.Execute;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  StopServerAction.Execute;
end;

procedure TMainForm.StartServerActionExecute(Sender: TObject);
begin
  // Create http server
  FServer := TWiRLhttpServerIndy.Create;

  // Configure the engine
  FServer
    .SetPort(StrToIntDef(PortNumberEdit.Text, 8080))
    .SetThreadPoolSize(5)
    .ConfigureEngine('/rest')
    .SetName('WiRL Template Demo')

    // Add and configure an application
    .AddApplication('/web')
      .SetName('Default')
      .SetResources('Server.Resources.TStaticWebResource');

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

procedure TMainForm.TestActionExecute(Sender: TObject);
const
  LTemplateURL = 'http://localhost:%d/rest/web/home/';
begin
  ShellExecute(Handle, 'open', PChar(Format(LTemplateURL, [FServer.Port])), '', '', SW_NORMAL);
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
