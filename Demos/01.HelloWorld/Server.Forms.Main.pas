(*
  Copyright 2015-2016, WiRL - REST Library

  Home: https://github.com/WiRL-library

*)
unit Server.Forms.Main;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.ExtCtrls, System.Diagnostics, System.Actions, IdContext,

  WiRL.http.Accept.MediaType,
  WiRL.Core.Engine,
  WiRL.Core.Application,
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
    edtSecret: TEdit;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure StartServerActionExecute(Sender: TObject);
    procedure StartServerActionUpdate(Sender: TObject);
    procedure StopServerActionExecute(Sender: TObject);
    procedure StopServerActionUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FServer: TWiRLhttpServerIndy;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  WiRL.http.Accept.Parser,
  WiRL.Core.MessageBodyWriter,
  WiRL.Core.MessageBodyWriters,
  WiRL.Core.URL;

procedure TMainForm.Button1Click(Sender: TObject);
var
  LMediaTypes: TMediaTypeList;
begin
  LMediaTypes := TMediaTypeList.Create;
  TAcceptHeaderParser<TMediaType>.Parse(edtSecret.Text, LMediaTypes);
  edtSecret.Text := LMediaTypes[0].AcceptItemOnly;
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
  FServer := TWiRLhttpServerIndy.Create;

  FServer.ConfigureEngine('/rest')
    .SetName('WiRL HelloWorld')
    .SetPort(StrToIntDef(PortNumberEdit.Text, 8080))
    .SetThreadPoolSize(10)

    // Adds and configures an application
    .AddApplication('/app')
      .SetResources([
        'Server.Resources.THelloWorldResource',
        'Server.Resources.TEntityResource'
      ])
      .SetSecret(TEncoding.UTF8.GetBytes(edtSecret.Text))
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
