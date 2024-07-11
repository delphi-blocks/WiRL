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
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.ExtCtrls, System.Diagnostics, System.Actions, IdContext,

  Neon.Core.Types,
  WiRL.Configuration.Neon,
  WiRL.Core.Application,
  WiRL.Engine.REST,
  WiRL.http.Server,
  WiRL.http.Server.Indy;

type

  TExceptionListener = class(TInterfacedObject, IWiRLHandleExceptionListener)
    procedure HandleException(const ASender: TWiRLRESTEngine; const AApplication: TWiRLApplication; E: Exception);
  end;

  TMainForm = class(TForm)
    TopPanel: TPanel;
    StartButton: TButton;
    StopButton: TButton;
    MainActionList: TActionList;
    StartServerAction: TAction;
    StopServerAction: TAction;
    PortNumberEdit: TEdit;
    Label1: TLabel;
    memoLog: TMemo;
    procedure FormDestroy(Sender: TObject);
    procedure StartServerActionExecute(Sender: TObject);
    procedure StartServerActionUpdate(Sender: TObject);
    procedure StopServerActionExecute(Sender: TObject);
    procedure StopServerActionUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FRESTServer: TWiRLServer;
    FListener: IWiRLHandleExceptionListener;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FListener := nil;
  FRESTServer.Free;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StopServerAction.Execute;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FRESTServer := TWiRLServer.Create(Self);
  FListener := TExceptionListener.Create;

  FRESTServer.AddEngine<TWiRLRESTEngine>('/rest')
    .SetEngineName('RESTEngine')
    .AddSubscriber(FListener)
    .AddApplication('/app')
      .SetResources('*')
      .SetFilters('*')
      .Plugin.Configure<IWiRLConfigurationNeon>
        .SetUseUTCDate(True)
        .SetMemberCase(TNeonCase.SnakeCase);

  StartServerAction.Execute;
end;

procedure TMainForm.StartServerActionExecute(Sender: TObject);
begin
  FRESTServer.Port := StrToIntDef(PortNumberEdit.Text, 8080);
  if not FRESTServer.Active then
    FRESTServer.Active := True;
end;

procedure TMainForm.StartServerActionUpdate(Sender: TObject);
begin
  StartServerAction.Enabled := not Assigned(FRESTServer) or not FRESTServer.Active;
end;

procedure TMainForm.StopServerActionExecute(Sender: TObject);
begin
  FRESTServer.Active := False;
end;

procedure TMainForm.StopServerActionUpdate(Sender: TObject);
begin
  StopServerAction.Enabled := Assigned(FRESTServer) and FRESTServer.Active;
end;

{ TExceptionListener }

procedure TExceptionListener.HandleException(const ASender: TWiRLRESTEngine;
  const AApplication: TWiRLApplication; E: Exception);
begin
  MainForm.memoLog.Lines.Add(E.Message);
end;

end.
