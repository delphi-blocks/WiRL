(*
  Copyright 2015-2016, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit Server.Forms.Main;

interface

uses
  System.Classes, System.SysUtils, Winapi.Windows, Vcl.Forms, Vcl.ActnList,
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls,
  System.Diagnostics, System.Actions, Winapi.ShellAPI,

  MARS.Core.Engine,
  MARS.http.Server.Indy,
  MARS.Core.Application;

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
    FServer: TMARShttpServerIndy;
    FEngine: TMARSEngine;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  MARS.Core.JSON,
  MARS.Rtti.Utils,
  MARS.Core.MessageBodyWriter,
  MARS.Core.MessageBodyWriters;


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
  FServer := TMARShttpServerIndy.Create;

  // Configure the engine
  FServer.ConfigureEngine('/rest')
    .SetName('MARS Template Demo')
    .SetPort(StrToIntDef(PortNumberEdit.Text, 8080))
    .SetThreadPoolSize(5)

    // Add and configure an application
    .AddApplication('/default')
      .SetName('Default')
      .SetResources([ 'Server.Resources.THelloWorldResource']);

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
  LTemplateURL = 'http://localhost:%d/rest/default/helloworld/';
begin
  ShellExecute(Handle, 'open', PChar(Format(LTemplateURL, [FEngine.Port])), '', '', SW_NORMAL);
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
