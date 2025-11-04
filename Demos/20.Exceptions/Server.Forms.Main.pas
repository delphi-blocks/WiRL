{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2025 WiRL Team                                      }
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
  WiRL.http.Server,
  WiRL.http.Server.Indy,
  WiRL.Core.Exceptions,
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
    lstLog: TListBox;
    procedure StartServerActionExecute(Sender: TObject);
    procedure StartServerActionUpdate(Sender: TObject);
    procedure StopServerActionExecute(Sender: TObject);
    procedure StopServerActionUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FServer: TWiRLServer;
  public
    procedure Log(const AMsg :string);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Neon.Core.Types,
  WiRL.Configuration.Neon,
  WiRL.Configuration.Errors,
  WiRL.Core.JSON,
  WiRL.Rtti.Utils;


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
  // Create http server
  FServer := TWiRLServer.Create(nil);

  // Engine configuration
  FServer
    .SetPort(StrToIntDef(PortNumberEdit.Text, 8080))
    .AddEngine<TWiRLRESTEngine>('/rest')
    .SetEngineName('WiRL custom Exceptions')

    // Application configuration
    .AddApplication('/app')
      .SetAppName('Default App')
      .SetResources('*')
      .SetFilters('*')

      .Plugin.Configure<IWiRLConfigurationNeon>
        .SetUseUTCDate(True)
        .SetMemberCase(TNeonCase.SnakeCase)
        .ApplyConfig

      .Plugin.Configure<IWiRLConfigurationErrors>
        .SetErrorClass(EWiRLWebApplicationException)
        .SetErrorCase(TNeonCase.CamelCase)
        .SetErrorDebugInfo(True)
        //.SetErrorMediaType()
        .ApplyConfig

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
  FServer.Free;
end;

procedure TMainForm.StopServerActionUpdate(Sender: TObject);
begin
  StopServerAction.Enabled := Assigned(FServer) and (FServer.Active = True);
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
