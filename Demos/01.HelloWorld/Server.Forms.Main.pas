(*
  Copyright 2015-2016, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit Server.Forms.Main;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.ExtCtrls, System.Diagnostics, System.Actions, IdContext,

  MARS.Core.Engine,
  MARS.Core.Application,
  MARS.http.Server.Indy;

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
    Edit1: TEdit;
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
  MARS.Core.MessageBodyWriter,
  MARS.Core.MessageBodyWriters,
  MARS.Core.URL;

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

  FServer.ConfigureEngine('/rest')
    .SetName('MARS HelloWorld')
    .SetPort(StrToIntDef(PortNumberEdit.Text, 8080))
    .SetThreadPoolSize(10)

    // Adds and configures an application
    .AddApplication('/app')
      .SetResources([
        'Server.Resources.THelloWorldResource',
        'Server.Resources.TEntityResource'
      ])
      .SetSecret(
        function (): TBytes
        begin
          Result := TEncoding.UTF8.GetBytes(Edit1.Text);
        end
      )
  ;

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

end.
