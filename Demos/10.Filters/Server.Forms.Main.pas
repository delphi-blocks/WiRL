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
  System.Classes, System.SysUtils, Vcl.Forms, Vcl.ActnList, Vcl.ComCtrls,
  Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls, System.Diagnostics, System.Actions,
  WiRL.Core.Engine,
  WiRL.http.Server,
  WiRL.http.Server.Indy,
  WiRL.Core.Application,
  WiRL.http.Filters, WiRL.Core.MessageBodyReader, WiRL.Core.MessageBodyWriter,
  WiRL.Core.Registry;

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
    lstLog: TListBox;
    WiRLhttpServer1: TWiRLhttpServer;
    WiRLEngine1: TWiRLEngine;
    WiRLApplication1: TWiRLApplication;
    procedure StartServerActionExecute(Sender: TObject);
    procedure StartServerActionUpdate(Sender: TObject);
    procedure StopServerActionExecute(Sender: TObject);
    procedure StopServerActionUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    // FServer: TWiRLhttpServer;
  public
    procedure Log(const AMsg :string);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  WiRL.Core.JSON,
  WiRL.Rtti.Utils,
  WiRL.Core.MessageBody.Default,
  WiRL.Data.MessageBody.Default;


procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StopServerAction.Execute;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  StartServerAction.Execute;
end;

procedure TMainForm.Log(const AMsg: string);
begin
  TThread.Synchronize(nil,
    procedure ()
    begin
      lstLog.Items.Add(AMsg);
    end
  );
end;

procedure TMainForm.StartServerActionExecute(Sender: TObject);
begin
  if not WiRLhttpServer1.Active then
  begin
    WiRLhttpServer1.Port := StrToIntDef(PortNumberEdit.Text, 8080);
    WiRLhttpServer1.Active := True;
  end;
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

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
