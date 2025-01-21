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

  Server.Filters,
  Server.Listener;

type
  TMainForm = class(TForm, ILogger)
    TopPanel: TPanel;
    StartButton: TButton;
    StopButton: TButton;
    MainActionList: TActionList;
    StartServerAction: TAction;
    StopServerAction: TAction;
    PortNumberEdit: TEdit;
    Label1: TLabel;
    lstLog: TListBox;
    TitlePanel: TPanel;
    procedure StartServerActionExecute(Sender: TObject);
    procedure StartServerActionUpdate(Sender: TObject);
    procedure StopServerActionExecute(Sender: TObject);
    procedure StopServerActionUpdate(Sender: TObject);
    procedure PortNumberEditChange(Sender: TObject);
  private
    FListener: TListener;
  public
    procedure Log(const AMsg :string);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  FListener := TListener.Create;
  StartServerAction.Execute;
  RegisterLogger(Self);

  Caption := FListener.Name;
  TitlePanel.Caption := FListener.DisplayName;
end;

destructor TMainForm.Destroy;
begin
  UnregisterLogger(Self);

  FListener.Free;
  inherited;
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

procedure TMainForm.PortNumberEditChange(Sender: TObject);
begin
  FListener.Port := StrToIntDef(PortNumberEdit.Text, 8081);
end;

procedure TMainForm.StartServerActionExecute(Sender: TObject);
begin
  FListener.Start;
end;

procedure TMainForm.StartServerActionUpdate(Sender: TObject);
begin
  PortNumberEdit.Enabled := not FListener.Active;
  StartServerAction.Enabled := not FListener.Active;
end;

procedure TMainForm.StopServerActionExecute(Sender: TObject);
begin
  FListener.Stop;
end;

procedure TMainForm.StopServerActionUpdate(Sender: TObject);
begin
  StopServerAction.Enabled := FListener.Active;
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
