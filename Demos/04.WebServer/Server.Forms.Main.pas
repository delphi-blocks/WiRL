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

  WiRL.http.FileSystemEngine,
  WiRL.Core.Engine,
  WiRL.Core.Context,
  WiRL.http.Server,
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
    WiRLServer1: TWiRLServer;
    WiRLFileSystemEngine1: TWiRLFileSystemEngine;
    procedure StartServerActionExecute(Sender: TObject);
    procedure StartServerActionUpdate(Sender: TObject);
    procedure StopServerActionExecute(Sender: TObject);
    procedure StopServerActionUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TestActionExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
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
  if not WiRLServer1.Active then
  begin
    WiRLServer1.Port := StrToIntDef(PortNumberEdit.Text, 8080);
    WiRLServer1.Active := True;
  end;
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

procedure TMainForm.TestActionExecute(Sender: TObject);
const
  LTemplateURL = 'http://localhost:%d/';
begin
  ShellExecute(Handle, 'open', PChar(Format(LTemplateURL, [WiRLServer1.Port])), '', '', SW_NORMAL);
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
