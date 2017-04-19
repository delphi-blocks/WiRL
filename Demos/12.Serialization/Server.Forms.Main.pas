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
  WiRL.Core.Application,
  WiRL.http.Server.Indy,
  WiRL.Core.Serialization,

  Server.Resources;

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
    btnSerialize: TButton;
    memoSerialize: TMemo;
    Button2: TButton;
    btnDeserialize: TButton;
    memoDeserialize: TMemo;
    procedure btnSerializeClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btnDeserializeClick(Sender: TObject);
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
  System.Rtti, REST.Json,
  WiRL.Core.JSON,
  WiRL.Rtti.Utils,
  WiRL.Core.MessageBodyWriter,
  WiRL.Core.MessageBodyWriters,
  WiRL.Data.MessageBodyWriters,

  Server.Entities;


procedure TMainForm.btnSerializeClick(Sender: TObject);
var
  LPerson: TPerson;
  LJSON: TJSONObject;
begin
  LPerson := TPerson.Create;
  try
    LPerson.Name := 'Paolo';
    LPerson.Surname := 'Rossi';
    LPerson.AddAddress('Piacenza', 'Italy');
    LPerson.AddAddress('Parma', 'Italy');
    LPerson.Note.Date := Now;
    LPerson.Note.Text := 'Note Text';

    LJSON := TWiRLJSONMapper.ObjectToJSON(LPerson);
    try
      memoSerialize.Lines.Text := TJson.Format(LJSON);
    finally
      LJSON.Free;
    end;
  finally
    LPerson.Free;
  end;
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  LRec: TMyRecord;
begin
  LRec.Uno := 'Test Test Test';
  LRec.Due := 42;

  //memoSerialize.Lines.Text := TWiRLJSONMapper.RecordToJSON(TValue.From<TMyRecord>(LRec)).ToJSON;
end;

procedure TMainForm.btnDeserializeClick(Sender: TObject);
var
  LPerson: TPerson;
  LJSON: TJSONObject;
begin
  LPerson := TPerson.Create;
  try
    LJSON := TJSONObject.ParseJSONValue(memoSerialize.Lines.Text) as TJSONObject;
    try
      TWiRLJSONMapper.JSONToObject(LPerson, LJSON);
    finally
      LJSON.Free;
    end;

    LJSON := TWiRLJSONMapper.ObjectToJSON(LPerson);
    try
      memoDeserialize.Lines.Text := TJson.Format(LJSON);
    finally
      LJSON.Free;
    end;
  finally
    LPerson.Free;
  end;
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

  // Engine configuration
  FServer.ConfigureEngine('/rest')
    .SetPort(StrToIntDef(PortNumberEdit.Text, 8080))
    .SetName('WiRL ContentType Demo')
    .SetThreadPoolSize(5)

    // Application configuration
    .AddApplication('/app')
      .SetName('Content App')
      .SetResources('Server.Resources.TSampleResource')
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
