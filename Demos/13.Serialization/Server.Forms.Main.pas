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
  System.TypInfo, Vcl.Dialogs, System.UITypes, Vcl.Imaging.pngimage, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, FireDAC.Stan.StorageBin, Vcl.Grids, Vcl.DBGrids,
  Vcl.ToolWin, System.Contnrs, System.JSON,

  WiRL.Core.Engine,
  WiRL.Core.Application,
  WiRL.http.Server.Indy,

  WiRL.Persistence.Core,
  WiRL.Persistence.JSON,

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
    btnSerComplexObject: TButton;
    memoSerialize: TMemo;
    btnSerSimpleTypes: TButton;
    btnDesComplexObject: TButton;
    memoDeserialize: TMemo;
    btnSerGenericList: TButton;
    btnSerGenericObjectList: TButton;
    imgSample: TImage;
    dsPersons: TFDMemTable;
    dsPersonsName: TStringField;
    dsPersonsSurname: TStringField;
    dsPersonsAge: TIntegerField;
    btnSerDataSet: TButton;
    btnDesSimpleTypes: TButton;
    btnDesDataSet: TButton;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    btnSerStreamable: TButton;
    btnDesStreamable: TButton;
    btnDesStreamableProp: TButton;
    btnStreamableProp: TButton;
    Panel2: TPanel;
    Panel3: TPanel;
    btnDesGenericList: TButton;
    btnDesGenericObjectList: TButton;
    btnDesSimpleObject: TButton;
    btnSerSimpleObject: TButton;
    Panel1: TPanel;
    grpType: TGroupBox;
    rbMemberStandard: TRadioButton;
    rbMemberFields: TRadioButton;
    rbMemberProperties: TRadioButton;
    grpCase: TGroupBox;
    rbCasePascal: TRadioButton;
    rbCaseCamel: TRadioButton;
    rbCaseSnake: TRadioButton;
    rbCaseLower: TRadioButton;
    rbCaseUpper: TRadioButton;
    rbCaseCustom: TRadioButton;
    grpPrefix: TGroupBox;
    chkVisibilityPrivate: TCheckBox;
    grpVisibility: TGroupBox;
    chkIgnorePrefix: TCheckBox;
    chkVisibilityProtected: TCheckBox;
    chkVisibilityPublic: TCheckBox;
    chkVisibilityPublished: TCheckBox;
    procedure btnSerDataSetClick(Sender: TObject);
    procedure btnSerComplexObjectClick(Sender: TObject);
    procedure btnSerSimpleTypesClick(Sender: TObject);
    procedure btnDesComplexObjectClick(Sender: TObject);
    procedure btnSerGenericListClick(Sender: TObject);
    procedure btnSerGenericObjectListClick(Sender: TObject);
    procedure btnSerImageClick(Sender: TObject);
    procedure btnDesSimpleTypesClick(Sender: TObject);
    procedure btnDesDataSetClick(Sender: TObject);
    procedure btnDesGenericListClick(Sender: TObject);
    procedure btnDesGenericObjectListClick(Sender: TObject);
    procedure btnSerStreamableClick(Sender: TObject);
    procedure btnDesStreamableClick(Sender: TObject);
    procedure btnDesStreamablePropClick(Sender: TObject);
    procedure btnStreamablePropClick(Sender: TObject);
    procedure btnDesSimpleObjectClick(Sender: TObject);
    procedure btnSerSimpleObjectClick(Sender: TObject);
    procedure StartServerActionExecute(Sender: TObject);
    procedure StartServerActionUpdate(Sender: TObject);
    procedure StopServerActionExecute(Sender: TObject);
    procedure StopServerActionUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FServer: TWiRLhttpServerIndy;

    FCustomCaseAlgo: TCaseFunc;
    procedure Log(const ALog: string); overload;
    procedure Log(const ATitle, ALog: string); overload;
    function GetStringFromValue(const AValue: TValue): string;
    function BuildSerializerConfig: INeonConfiguration;
  protected
    //Various tests
    procedure TestSerializeRecord;
    procedure TestRecordPointers;
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
  Server.Entities;

{ TMainForm }

procedure TMainForm.btnSerDataSetClick(Sender: TObject);
var
  LJSON: TJSONValue;
begin
  LJSON := TNeonMapperJSON.ObjectToJSON(dsPersons, BuildSerializerConfig);
  try
    Log(TJSONHelper.PrettyPrint(LJSON));
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

    LJSON := TNeonMapperJSON.ObjectToJSON(LPerson, BuildSerializerConfig);
    try
      Log(TJSONHelper.PrettyPrint(LJSON));
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
  LJSON := TNeonMapperJSON.ValueToJSON(AValue, BuildSerializerConfig);
  try
    Result := TJSONHelper.ToJSON(LJSON);
  finally
    LJSON.Free;
  end;
end;

procedure TMainForm.Log(const ALog: string);
begin
  memoSerialize.Lines.Text := ALog;
end;

procedure TMainForm.btnSerSimpleTypesClick(Sender: TObject);
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
      TNeonMapperJSON.JSONToObject(LPerson, LJSON, BuildSerializerConfig);
    finally
      LJSON.Free;
    end;

    LJSON := TNeonMapperJSON.ObjectToJSON(LPerson, BuildSerializerConfig);
    try
      memoDeserialize.Lines.Text := TJSONHelper.PrettyPrint(LJSON);
    finally
      LJSON.Free;
    end;
  finally
    LPerson.Free;
  end;
end;

procedure TMainForm.btnSerGenericListClick(Sender: TObject);
var
  LList: TList<Double>;
  LJSON: TJSONValue;
begin
  LList := TList<Double>.Create;
  try
    LList.Add(34.9);
    LList.Add(10.0);

    LJSON := TNeonMapperJSON.ObjectToJSON(LList, BuildSerializerConfig);
    try
      Log(TJSONHelper.PrettyPrint(LJSON));
    finally
      LJSON.Free;
    end;
  finally
    LList.Free;
  end;
end;

procedure TMainForm.btnSerGenericObjectListClick(Sender: TObject);
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
    LJSON := TNeonMapperJSON.ObjectToJSON(LBook, BuildSerializerConfig);
    try
      Log(TJSONHelper.PrettyPrint(LJSON));
    finally
      LJSON.Free;
    end;
  finally
    LBook.Free;
  end;
end;

procedure TMainForm.btnSerImageClick(Sender: TObject);
var
  LJSON: TJSONValue;
begin
  LJSON := TNeonMapperJSON.ObjectToJSON(imgSample, BuildSerializerConfig);
  try
    Log('Image', TJSONHelper.PrettyPrint(LJSON));
  finally
    LJSON.Free;
  end;
end;

procedure TMainForm.btnDesSimpleTypesClick(Sender: TObject);
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

  LDes := TNeonDeserializerJSON.Create(BuildSerializerConfig);
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

  LDes := TNeonDeserializerJSON.Create(BuildSerializerConfig);
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

  LDes := TNeonDeserializerJSON.Create(BuildSerializerConfig);
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

procedure TMainForm.btnDesDataSetClick(Sender: TObject);
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
  LDes := TNeonDeserializerJSON.Create(BuildSerializerConfig);
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

procedure TMainForm.btnDesGenericListClick(Sender: TObject);
var
  LList: TList<Double>;
  LJSON: TJSONValue;
begin
  LList := TList<Double>.Create;
  try
    LJSON := TJSONObject.ParseJSONValue(memoSerialize.Lines.Text);
    try
      TNeonMapperJSON.JSONToObject(LList, LJSON, BuildSerializerConfig);
    finally
      LJSON.Free;
    end;

    LJSON := TNeonMapperJSON.ObjectToJSON(LList, BuildSerializerConfig);
    try
      memoDeserialize.Lines.Text := TJSONHelper.PrettyPrint(LJSON);
    finally
      LJSON.Free;
    end;
  finally
    LList.Free;
  end;
end;

procedure TMainForm.btnDesGenericObjectListClick(Sender: TObject);
var
  LList: TAddressBook;
  LJSON: TJSONValue;
begin
  LList := TAddressBook.Create;
  try
    LJSON := TJSONObject.ParseJSONValue(memoSerialize.Lines.Text);
    try
      TNeonMapperJSON.JSONToObject(LList, LJSON, BuildSerializerConfig);
    finally
      LJSON.Free;
    end;

    LJSON := TNeonMapperJSON.ObjectToJSON(LList, BuildSerializerConfig);
    try
      memoDeserialize.Lines.Text := TJSONHelper.PrettyPrint(LJSON);
    finally
      LJSON.Free;
    end;
  finally
    LList.Free;
  end;
end;

procedure TMainForm.btnSerStreamableClick(Sender: TObject);
var
  LStreamable: TStreamableSample;
  LJSON: TJSONValue;
begin
  LStreamable := TStreamableSample.Create;
  LStreamable.AsString := 'Paolo';

  LJSON := TNeonMapperJSON.ObjectToJSON(LStreamable, BuildSerializerConfig);
  try
    Log(TJSONHelper.PrettyPrint(LJSON));
  finally
    LJSON.Free;
  end;

  LStreamable.Free;
end;

procedure TMainForm.btnDesStreamableClick(Sender: TObject);
var
  LStreamable: TStreamableSample;
  LJSON: TJSONValue;
begin
  LStreamable := TStreamableSample.Create;

  LJSON := TJSONObject.ParseJSONValue(memoSerialize.Lines.Text);
  try
    TNeonMapperJSON.JSONToObject(LStreamable, LJSON, BuildSerializerConfig);
  finally
    LJSON.Free;
  end;

  LJSON := TNeonMapperJSON.ObjectToJSON(LStreamable, BuildSerializerConfig);
  try
    memoDeserialize.Lines.Text := TJSONHelper.PrettyPrint(LJSON);
  finally
    LJSON.Free;
  end;

  LStreamable.Free;
end;

procedure TMainForm.btnDesStreamablePropClick(Sender: TObject);
var
  LStreamable: TStreamableComposition;
  LJSON: TJSONValue;
begin
  LStreamable := TStreamableComposition.Create;

  LJSON := TJSONObject.ParseJSONValue(memoSerialize.Lines.Text);
  try
    TNeonMapperJSON.JSONToObject(LStreamable, LJSON, BuildSerializerConfig);
  finally
    LJSON.Free;
  end;

  LJSON := TNeonMapperJSON.ObjectToJSON(LStreamable, BuildSerializerConfig);
  try
    memoDeserialize.Lines.Text := TJSONHelper.PrettyPrint(LJSON);
  finally
    LJSON.Free;
  end;

  LStreamable.Free;

end;

procedure TMainForm.btnStreamablePropClick(Sender: TObject);
var
  LStreamable: TStreamableComposition;
  LJSON: TJSONValue;
begin
  LStreamable := TStreamableComposition.Create;
  LStreamable.InValue := 233;
  LStreamable.Stream.AsString := 'Paolo';

  LJSON := TNeonMapperJSON.ObjectToJSON(LStreamable, BuildSerializerConfig);
  try
    Log(TJSONHelper.PrettyPrint(LJSON));
  finally
    LJSON.Free;
  end;

  LStreamable.Free;
end;

procedure TMainForm.btnDesSimpleObjectClick(Sender: TObject);
var
  LSimple: TCaseClass;
  LJSON: TJSONValue;
begin
  LSimple := TCaseClass.Create;
  try
    LJSON := TJSONObject.ParseJSONValue(memoSerialize.Lines.Text) as TJSONObject;
    try
      TNeonMapperJSON.JSONToObject(LSimple, LJSON, BuildSerializerConfig);
    finally
      LJSON.Free;
    end;

    LJSON := TNeonMapperJSON.ObjectToJSON(LSimple, BuildSerializerConfig);
    try
      memoDeserialize.Lines.Text := TJSONHelper.PrettyPrint(LJSON);
    finally
      LJSON.Free;
    end;

  finally
    LSimple.Free;
  end;
end;

procedure TMainForm.btnSerSimpleObjectClick(Sender: TObject);
var
  LJSON: TJSONValue;
  LSimple: TCaseClass;
begin
  LSimple := TCaseClass.DefaultValues;
  try
    LJSON := TNeonMapperJSON.ObjectToJSON(LSimple, BuildSerializerConfig);
    try
      Log(TJSONHelper.PrettyPrint(LJSON));
    finally
      LJSON.Free;
    end;
  finally
    LSimple.Free;
  end;
end;

function TMainForm.BuildSerializerConfig: INeonConfiguration;
var
  LVis: TNeonVisibility;
begin
  LVis := [];
  Result := TNeonConfiguration.Default;

  // Case settings
  Result.SetMemberCustomCase(nil);
  if rbCaseCamel.Checked then
    Result.SetMemberCase(TNeonCase.CamelCase);
  if rbCaseSnake.Checked then
    Result.SetMemberCase(TNeonCase.SnakeCase);
  if rbCaseLower.Checked then
    Result.SetMemberCase(TNeonCase.LowerCase);
  if rbCaseUpper.Checked then
    Result.SetMemberCase(TNeonCase.UpperCase);
  if rbCaseCustom.Checked then
    Result
      .SetMemberCase(TNeonCase.CustomCase)
      .SetMemberCustomCase(FCustomCaseAlgo);

  // Member type settings
  if rbMemberFields.Checked then
    Result.SetMembersType(TNeonMembersType.Fields);
  if rbMemberProperties.Checked then
    Result.SetMembersType(TNeonMembersType.Properties);

  // F Prefix setting
  if chkIgnorePrefix.Checked then
    Result.SetIgnoreFieldPrefix(True);

  // Visibility settings
  if chkVisibilityPrivate.Checked then
    LVis := LVis + [mvPrivate];
  if chkVisibilityProtected.Checked then
    LVis := LVis + [mvProtected];
  if chkVisibilityPublic.Checked then
    LVis := LVis + [mvPublic];
  if chkVisibilityPublished.Checked then
    LVis := LVis + [mvPublished];
  Result.SetVisibility(LVis);
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StopServerAction.Execute;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FCustomCaseAlgo :=
    function(const AString: string): string
    begin
      Result := AString + 'X';
    end
  ;

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
      .SetResources('Server.Resources.TEntityResource')
      .ConfigureSerializer
        .SetMembersType(TNeonMembersType.Standard)
        .SetVisibility([mvPublic, mvPublished])
        .SetMemberCase(TNeonCase.SnakeCase)
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

procedure TMainForm.TestRecordPointers;
var
  LValue: TValue;
  LP1: Pointer;
  LP2: Pointer;
  LR: TMyRecord;
begin
  LR.Uno := 'Paolo';
  LR.Due := 20;

  LValue := TValue.From<TMyRecord>(LR);

  LP1 := LValue.GetReferenceToRawData;

  LP2 := Pointer(btnSerGenericList);

  if LP1 = LP2 then
    memoSerialize.Lines.Add('equal');
end;

procedure TMainForm.TestSerializeRecord;
var
  LJSON: TJSONValue;
  LRec: TMyRecord;
begin
  LRec.Uno := 'Paolo';
  LRec.Due := 47;

  LJSON := TNeonMapperJSON.ValueToJSON(TValue.From<TMyRecord>(LRec), TNeonConfiguration.Snake);
  try
    Log(TJSONHelper.PrettyPrint(LJSON));
  finally
    LJSON.Free;
  end;
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
