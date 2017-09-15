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

  WiRL.Core.Engine, WiRL.http.Server, WiRL.http.Server.Indy,
  WiRL.Core.MessageBodyReader, WiRL.Core.MessageBodyWriter, WiRL.http.Filters,
  WiRL.Core.Registry, WiRL.Core.Application;

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
    WiRLServer1: TWiRLServer;
    WiRLEngine1: TWiRLEngine;
    WiRLApplication1: TWiRLApplication;
    procedure StartServerActionExecute(Sender: TObject);
    procedure StartServerActionUpdate(Sender: TObject);
    procedure StopServerActionExecute(Sender: TObject);
    procedure StopServerActionUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
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
  StartServerAction.Execute;
end;

procedure TMainForm.StartServerActionExecute(Sender: TObject);
begin
  WiRLServer1.Port := StrToIntDef(PortNumberEdit.Text, 8080);
  if not WiRLServer1.Active then
    WiRLServer1.Active := True;
end;

procedure TMainForm.StartServerActionUpdate(Sender: TObject);
begin
  StartServerAction.Enabled := (WiRLServer1 = nil) or (WiRLServer1.Active = False);
end;

procedure TMainForm.StopServerActionExecute(Sender: TObject);
begin
  WiRLServer1.Active := False;
end;

procedure TMainForm.StopServerActionUpdate(Sender: TObject);
begin
  StopServerAction.Enabled := Assigned(WiRLServer1) and (WiRLServer1.Active = True);
end;

end.
