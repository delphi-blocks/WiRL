{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Forms.Main;

interface

uses
  System.Classes, System.SysUtils, Vcl.Forms, Vcl.ActnList, Vcl.ComCtrls,
  Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls, System.Diagnostics, System.Actions,

  WiRL.Engine.REST,
  WiRL.Core.Application,
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
    procedure StartServerActionExecute(Sender: TObject);
    procedure StartServerActionUpdate(Sender: TObject);
    procedure StopServerActionExecute(Sender: TObject);
    procedure StopServerActionUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FServer: TWiRLServer;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  {$IFDEF DJSON}
  DJSON.Params,
  WiRL.MessageBody.DJSON,
  {$ENDIF}

  {$IFDEF OXML}
  WiRL.MessageBody.OXML,
  {$ENDIF}

  WiRL.Core.JSON,
  WiRL.Rtti.Utils,
  WiRL.Core.MessageBodyWriter,
  WiRL.Data.MessageBody.Default;


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
  FServer := TWiRLServer.Create(nil);

  // Engine configuration
  FServer
    .SetPort(StrToIntDef(PortNumberEdit.Text, 8080))
    .SetThreadPoolSize(5)
    .AddEngine<TWiRLRESTEngine>('/rest')
    .SetEngineName('WiRL MessageBody* Demo')

    // Application configuration
    .AddApplication('/app')
      .SetAppName('Content App')
      .SetResources('Server.Resources.TMessageBodyResource')
  ;

  {$IFDEF DJSON}
  TMessageBodyDJSON.Params :=
    procedure (AParams: IdjParams)
    begin
      AParams.Engine := eDelphiStream;
      AParams.SerializationMode := smJavaScript;
      AParams.SerializationType := stFields;
      AParams.TypeAnnotations := False;
    end
  ;
  {$ENDIF}

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

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
