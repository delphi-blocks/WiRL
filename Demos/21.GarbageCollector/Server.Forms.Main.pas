{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2021 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Forms.Main;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.ExtCtrls, System.Diagnostics, System.Actions, System.TypInfo,

  Neon.Core.Types,
  WiRL.Configuration.Neon,
  WiRL.Configuration.Converter,
  WiRL.Core.Converter,
  WiRL.Engine.REST,
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
    RESTServer: TWiRLServer;
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
  RESTServer := TWiRLServer.Create(Self);

  RESTServer.AddEngine<TWiRLRESTEngine>('/rest')
    .SetEngineName('RESTEngine')
    .AddApplication('/app')
      .SetResources('*')
      .SetFilters('*')

      .Plugin.Configure<IWiRLFormatSetting>
        .AddFormat(TypeInfo(TDateTime), TWiRLFormatSetting.ISODATE_UTF)
        .ApplyConfig

      .Plugin.Configure<IWiRLConfigurationNeon>
        .SetUseUTCDate(True)
        .SetVisibility([mvPublic, mvPublished])
        .SetMemberCase(TNeonCase.PascalCase);

  StartServerAction.Execute;
end;

procedure TMainForm.StartServerActionExecute(Sender: TObject);
begin
  RESTServer.Port := StrToIntDef(PortNumberEdit.Text, 8080);
  if not RESTServer.Active then
    RESTServer.Active := True;
end;

procedure TMainForm.StartServerActionUpdate(Sender: TObject);
begin
  StartServerAction.Enabled := not Assigned(RESTServer) or not RESTServer.Active;
end;

procedure TMainForm.StopServerActionExecute(Sender: TObject);
begin
  RESTServer.Active := False;
end;

procedure TMainForm.StopServerActionUpdate(Sender: TObject);
begin
  StopServerAction.Enabled := Assigned(RESTServer) and RESTServer.Active;
end;

end.
