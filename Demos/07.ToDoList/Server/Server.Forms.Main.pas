(*
  Copyright 2015-2016, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit Server.Forms.Main;

interface

uses
  System.Classes, Winapi.Windows, System.SysUtils, Vcl.Forms, Vcl.ActnList,
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls,
  System.Diagnostics, System.Actions,

  MARS.Core.Engine,
  MARS.http.Server.Indy,
  MARS.Core.Application,
  MARS.Diagnostics.Manager,
  MARS.Diagnostics.Resources;

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
    procedure StartServerActionExecute(Sender: TObject);
    procedure StartServerActionUpdate(Sender: TObject);
    procedure StopServerActionExecute(Sender: TObject);
    procedure StopServerActionUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FServer: TMARShttpServerIndy;
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
  FServer := TMARShttpServerIndy.Create;

  // Engine configuration
  FServer.ConfigureEngine('/rest')
    .SetPort(StrToIntDef(PortNumberEdit.Text, 8080))
    .SetName('MARS ToDo List')
    .SetThreadPoolSize(5);

  // Add and configure an application
  FServer.Engine
    .AddApplication('/todo')
      .SetName('ToDoList')
      .SetResources(['Server.Resources*']);

  // Add and configure the diagnostic application
  FServer.Engine
    .AddApplication('/diagnostics')
    .SetName('Diagnostics')
    .SetResources(['*']);

  TMARSDiagnosticsManager.FEngine := FServer.Engine; // TODO: REMOVE!!!
  TMARSDiagnosticsManager.Instance;

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
  FreeAndNil(FServer);
end;

procedure TMainForm.StopServerActionUpdate(Sender: TObject);
begin
  StopServerAction.Enabled := Assigned(FServer) and (FServer.Active = True);
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
