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

  Server.Resources, Vcl.Imaging.pngimage, System.JSON;

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
    btnSerComplexObject: TButton;
    memoSerialize: TMemo;
    btnSimpleTypes: TButton;
    btnDesComplexObject: TButton;
    memoDeserialize: TMemo;
    btnGenericList: TButton;
    btnGenericObjectList: TButton;
    imgSample: TImage;
    btnImage: TButton;
    procedure btnSerComplexObjectClick(Sender: TObject);
    procedure btnSimpleTypesClick(Sender: TObject);
    procedure btnDesComplexObjectClick(Sender: TObject);
    procedure btnGenericListClick(Sender: TObject);
    procedure btnGenericObjectListClick(Sender: TObject);
    procedure btnImageClick(Sender: TObject);
    procedure StartServerActionExecute(Sender: TObject);
    procedure StartServerActionUpdate(Sender: TObject);
    procedure StopServerActionExecute(Sender: TObject);
    procedure StopServerActionUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FServer: TWiRLhttpServerIndy;
    procedure Log(const ATitle, ALog: string);
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  System.Rtti, REST.Json,
  System.Generics.Collections,
  WiRL.Core.JSON,
  WiRL.Rtti.Utils,
  WiRL.Core.MessageBodyWriter,
  WiRL.Core.MessageBodyWriters,
  WiRL.Data.MessageBodyWriters,

  Server.Entities;


procedure TMainForm.btnSerComplexObjectClick(Sender: TObject);
var
  LPerson: TPerson;
  LJSON: TJSONValue;
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
      memoSerialize.Lines.Text := TJSONHelper.PrettyPrint(LJSON);
    finally
      LJSON.Free;
    end;
  finally
    LPerson.Free;
  end;
end;

procedure TMainForm.btnSimpleTypesClick(Sender: TObject);
var
  LRec: TMyRecord;
  LArr: TIntArray;


  function GetStringFromValue(const AValue: TValue): string;
  var
    LJSON: TJSONValue;
  begin
    LJSON := TWiRLJSONMapper.TValueToJSON(AValue);
    try
      Result := LJSON.ToJSON;
    finally
      LJSON.Free;
    end;
  end;
begin
  Log('Integer', GetStringFromValue(TValue.From<Integer>(42)));

  LRec.Uno := 'Test Test Test';
  LRec.Due := 42;
  Log('Record', GetStringFromValue(TValue.From<TMyRecord>(LRec)));

  LArr := [12, 34, 797, 5252636];
  Log('Array of Integer', GetStringFromValue(TValue.From<TIntArray>(LArr)));
end;

procedure TMainForm.btnDesComplexObjectClick(Sender: TObject);
var
  LPerson: TPerson;
  LJSON: TJSONValue;
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
      memoDeserialize.Lines.Text := TJSONHelper.PrettyPrint(LJSON);
    finally
      LJSON.Free;
    end;
  finally
    LPerson.Free;
  end;
end;

procedure TMainForm.btnGenericListClick(Sender: TObject);
var
  LList: TList<Double>;
  LJSON: TJSONValue;
begin
  LList := TList<Double>.Create;
  try
    LList.Add(34.9);
    LList.Add(10.0);

    LJSON := TWiRLJSONMapper.ObjectToJSON(LList);
    try
      Log('List', TJSONHelper.PrettyPrint(LJSON));
    finally
      LJSON.Free;
    end;
  finally
    LList.Free;
  end;
end;

procedure TMainForm.btnGenericObjectListClick(Sender: TObject);
var
  LBook: TAddressBook;
  LJSON: TJSONValue;
begin
  LBook := TAddressBook.Create;
  try
    LBook.Add('Verona', 'Italy');
    LBook.Add('Napoli', 'Italy');
    LBook.NoteList.Add('Note 1');
    LBook.NoteList.Add('Note 2');
    LBook.NoteList.Add('Note 3');
    LJSON := TWiRLJSONMapper.ObjectToJSON(LBook);
    try
      Log('List', TJSONHelper.PrettyPrint(LJSON));
    finally
      LJSON.Free;
    end;
  finally
    LBook.Free;
  end;
end;

procedure TMainForm.btnImageClick(Sender: TObject);
var
  LJSON: TJSONValue;
begin
  LJSON := TWiRLJSONMapper.ObjectToJSON(imgSample);
  try
    Log('Image', TJSONHelper.PrettyPrint(LJSON));
  finally
    LJSON.Free;
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

procedure TMainForm.Log(const ATitle, ALog: string);
begin
  memoSerialize.Lines.Add('');
  memoSerialize.Lines.Add(ATitle + ':');
  memoSerialize.Lines.Add(ALog);
  memoSerialize.Lines.Add('-----------------------');
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
