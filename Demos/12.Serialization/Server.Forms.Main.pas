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
  System.Classes, System.SysUtils, Vcl.Forms, Vcl.ActnList, Vcl.ComCtrls, System.Rtti,
  Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls, System.Diagnostics, System.Actions,

  WiRL.Core.Engine,
  WiRL.Core.Application,
  WiRL.http.Server.Indy,

  WiRL.Persistence.Core,
  WiRL.Persistence.JSON,

  Server.Resources, Vcl.Imaging.pngimage, System.JSON, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, FireDAC.Stan.StorageBin, Vcl.Grids, Vcl.DBGrids;

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
    dsPersons: TFDMemTable;
    dsPersonsName: TStringField;
    dsPersonsSurname: TStringField;
    dsPersonsAge: TIntegerField;
    btnDataSet: TButton;
    Button1: TButton;
    Button2: TButton;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    procedure btnDataSetClick(Sender: TObject);
    procedure btnSerComplexObjectClick(Sender: TObject);
    procedure btnSimpleTypesClick(Sender: TObject);
    procedure btnDesComplexObjectClick(Sender: TObject);
    procedure btnGenericListClick(Sender: TObject);
    procedure btnGenericObjectListClick(Sender: TObject);
    procedure btnImageClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure StartServerActionExecute(Sender: TObject);
    procedure StartServerActionUpdate(Sender: TObject);
    procedure StopServerActionExecute(Sender: TObject);
    procedure StopServerActionUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FServer: TWiRLhttpServerIndy;
    procedure Log(const ATitle, ALog: string);
    function GetStringFromValue(const AValue: TValue): string;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  REST.Json,
  System.Generics.Collections,
  WiRL.Core.JSON,
  WiRL.Rtti.Utils,
  WiRL.Core.MessageBodyWriter,
  WiRL.Core.MessageBodyWriters,
  WiRL.Data.MessageBodyWriters,

  Server.Entities;


procedure TMainForm.btnDataSetClick(Sender: TObject);
var
  LJSON: TJSONValue;
begin
  LJSON := TNeonMapperJSON.ObjectToJSON(dsPersons, TNeonConfiguration.Default);
  try
    Log('DataSet', TJSONHelper.PrettyPrint(LJSON));
  finally
    LJSON.Free;
  end;
end;

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

    LJSON := TNeonMapperJSON.ObjectToJSON(LPerson, TNeonConfiguration.Default);
    try
      memoSerialize.Lines.Text := TJSONHelper.PrettyPrint(LJSON);
    finally
      LJSON.Free;
    end;

  finally
    LPerson.Free;
  end;
end;

function TMainForm.GetStringFromValue(const AValue: TValue): string;
var
  LJSON: TJSONValue;
begin
  LJSON := TNeonMapperJSON.ValueToJSON(AValue, TNeonConfiguration.Default);
  try
    Result :=  TJSONHelper.ToJSON(LJSON);
  finally
    LJSON.Free;
  end;
end;

procedure TMainForm.btnSimpleTypesClick(Sender: TObject);
var
  LRec: TMyRecord;
  LArr: TIntArray;
begin
  Log('Integer', GetStringFromValue(TValue.From<Integer>(42)));

  LRec.Uno := 'Test Test Test';
  LRec.Due := 42;
  Log('Record', GetStringFromValue(TValue.From<TMyRecord>(LRec)));

  SetLength(LArr, 4);
  LArr[0] := 12;
  LArr[1] := 34;
  LArr[2] := 797;
  LArr[3] := 5236;
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
      TNeonMapperJSON.JSONToObject(LPerson, LJSON);
    finally
      LJSON.Free;
    end;

    LJSON := TNeonMapperJSON.ObjectToJSON(LPerson, TNeonConfiguration.Default);
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

    LJSON := TNeonMapperJSON.ObjectToJSON(LList, TNeonConfiguration.Default);
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
    LJSON := TNeonMapperJSON.ObjectToJSON(LBook, TNeonConfiguration.Default);
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
  LJSON := TNeonMapperJSON.ObjectToJSON(imgSample, TNeonConfiguration.Default);
  try
    Log('Image', TJSONHelper.PrettyPrint(LJSON));
  finally
    LJSON.Free;
  end;
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  LArr: TIntArray;
  LJString: string;
  LDes: TNeonDeserializerJSON;
  LJSON: TJSONValue;
  LInt: Integer;
  LRec: TMyRecord;
  LValue: TValue;
begin
  // Integer
  LInt := 42;
  LJString := GetStringFromValue(TValue.From<Integer>(LInt));

  LDes := TNeonDeserializerJSON.Create(TNeonConfiguration.Default);
  try
    LJSON := TJSONObject.ParseJSONValue(LJString);
    try
      LInt := LDes.JSONToTValue(LJSON, TRttiHelper.Context.GetType(TypeInfo(Integer))).AsInteger;
    finally
      LJSON.Free;
    end;
  finally
    LDes.Free;
  end;
  memoDeserialize.Lines.Add('Integer: ' + LInt.ToString);

  // Record
  LRec.Uno := 'Test Test Test';
  LRec.Due := 42;
  LJString := GetStringFromValue(TValue.From<TMyRecord>(LRec));
  Log('Record', LJString);

  LDes := TNeonDeserializerJSON.Create(TNeonConfiguration.Default);
  try
    LJSON := TJSONObject.ParseJSONValue(LJString);
    try
      LValue := LDes.JSONToTValue(LJSON, TRttiHelper.Context.GetType(TypeInfo(TMyRecord)), TValue.From<TMyRecord>(LRec));
      if LValue.IsArray then
        LRec := LValue.AsType<TMyRecord>;
      memoDeserialize.Lines.Add('Record: ' + LRec.ToString);
    finally
      LJSON.Free;
    end;
  finally
    LDes.Free;
  end;

  // Dynamic Array
  SetLength(LArr, 4);
  LArr[0] := 12;
  LArr[1] := 34;
  LArr[2] := 797;
  LArr[3] := 5236;

  LJString := GetStringFromValue(TValue.From<TIntArray>(LArr));
  Log('Array', LJString);

  SetLength(LArr, 1);
  LArr[0] := 100;

  LDes := TNeonDeserializerJSON.Create(TNeonConfiguration.Default);
  try
    LJSON := TJSONObject.ParseJSONValue(LJString);
    try
      LValue := LDes.JSONToTValue(LJSON, TRttiHelper.Context.GetType(TypeInfo(TIntArray)), TValue.From<TIntArray>(LArr));
      if LValue.IsArray then
        LArr := LValue.AsType<TIntArray>;

      LJString := '[';
      for LInt in LArr do
        LJString := LJString + LInt.ToString + ',';
      LJString := LJString.TrimRight([',']) + ']';

      memoDeserialize.Lines.Add('Array: ' + LJString);
    finally
      LJSON.Free;
    end;
  finally
    LDes.Free;
  end;
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  LJString: string;
  LJSON: TJSONValue;
  LDes: TNeonDeserializerJSON;
  LValue: TValue;
begin
  // DataSet
  //LJString := GetStringFromValue(dsPersons);
  //Log('DataSet', LJString);
  dsPersons.EmptyDataSet;
  LJString := memoSerialize.Lines.Text;
  LDes := TNeonDeserializerJSON.Create(TNeonConfiguration.Default);
  try
    LJSON := TJSONObject.ParseJSONValue(LJString);
    try
      LValue := LDes.JSONToTValue(LJSON, TRttiHelper.Context.GetType(dsPersons.ClassType), dsPersons);
    finally
      LJSON.Free;
    end;
  finally
    LDes.Free;
  end;
  memoDeserialize.Lines.Add('DataSet: ' + dsPersons.Fields[0].AsString);
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
