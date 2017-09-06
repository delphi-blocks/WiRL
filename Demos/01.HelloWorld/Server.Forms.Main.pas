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
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.ExtCtrls, System.Diagnostics, System.Actions, IdContext,

  WiRL.Core.Engine, WiRL.http.Server, WiRL.http.Server.Indy;

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
    WiRLhttpServer1: TWiRLhttpServer;
    WiRLEngine1: TWiRLEngine;
    procedure StartServerActionExecute(Sender: TObject);
    procedure StartServerActionUpdate(Sender: TObject);
    procedure StopServerActionExecute(Sender: TObject);
    procedure StopServerActionUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure SetupWiRLServer;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StopServerAction.Execute;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  SetupWiRLServer;
  StartServerAction.Execute;
end;

procedure TMainForm.SetupWiRLServer;
begin
  WiRLEngine1
    // Adds and configures an application
    .AddApplication('/app')
    {$IF CompilerVersion >=28} //XE7
      .SetResources([
        'Server.Resources.THelloWorldResource',
        'Server.Resources.TEntityResource'
      ]);
    {$ELSE}
      .SetResources(
        'Server.Resources.THelloWorldResource,'+
        'Server.Resources.TEntityResource'
      );
    {$IFEND}
  ;
end;

procedure TMainForm.StartServerActionExecute(Sender: TObject);
begin
  WiRLhttpServer1.Port := StrToIntDef(PortNumberEdit.Text, 8080);
  if not WiRLhttpServer1.Active then
    WiRLhttpServer1.Active := True;
end;

procedure TMainForm.StartServerActionUpdate(Sender: TObject);
begin
  StartServerAction.Enabled := (WiRLhttpServer1 = nil) or (WiRLhttpServer1.Active = False);
end;

procedure TMainForm.StopServerActionExecute(Sender: TObject);
begin
  WiRLhttpServer1.Active := False;
end;

procedure TMainForm.StopServerActionUpdate(Sender: TObject);
begin
  StopServerAction.Enabled := Assigned(WiRLhttpServer1) and (WiRLhttpServer1.Active = True);
end;

end.
