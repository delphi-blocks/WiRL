{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit FMXClient.Forms.Main;

{$I '..\Core\WiRL.inc'}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl,
  FMX.StdCtrls, FMX.Layouts, FMX.ListBox, FMX.MultiView, FMX.Memo,
  FMX.Controls.Presentation, FMX.Edit, FMX.ScrollBox,
  Generics.Collections, System.Rtti, FMX.Grid.Style, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, FMX.Grid, FireDAC.Stan.StorageJSON, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Fmx.Bind.Grid, System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.Components, Data.Bind.Grid, Data.Bind.DBScope, FMX.Memo.Types,
  FMX.Objects, FMXClient.DataModules.Database;

type
  TMainForm = class(TForm)
    ToolBar1: TToolBar;
    MainTabControl: TTabControl;
    HelloWorldTabItem: TTabItem;
    btnExecute: TButton;
    Memo1: TMemo;
    Layout1: TLayout;
    btnEcho: TButton;
    Image1: TImage;
    btnReverse: TButton;
    btnPost: TButton;
    BtnGenericGET: TButton;
    BtnGenericPOST: TButton;
    BtnException: TButton;
    Button1: TButton;
    BtnGetImage: TButton;
    Button2: TButton;
    BtnGetStringAndStream: TButton;
    BtnPostStream: TButton;
    BtnGetWiRLResponse: TButton;
    EditInput: TEdit;
    Label1: TLabel;
    BtnPersonAndHeader: TButton;
    BtnPersorOrError: TButton;
    EditPersonID: TEdit;
    Label2: TLabel;
    TabDatabase: TTabItem;
    StringGrid1: TStringGrid;
    btnLoad: TButton;
    dsUsers: TFDMemTable;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource;
    dsUsersid: TIntegerField;
    dsUsersvalue: TStringField;
    TabParams: TTabItem;
    Layout2: TLayout;
    btnString: TButton;
    Image2: TImage;
    btnInt: TButton;
    btnFloat: TButton;
    Memo2: TMemo;
    btnParamObject: TButton;
    btnBool: TButton;
    btnDate: TButton;
    btnTime: TButton;
    btnEnum: TButton;
    btnDateTime: TButton;
    btnMultiPart: TButton;
    btnParamArray: TButton;
    procedure btnExecuteClick(Sender: TObject);
    procedure btnEchoClick(Sender: TObject);
    procedure btnReverseClick(Sender: TObject);
    procedure btnPostClick(Sender: TObject);
    procedure BtnGenericGETClick(Sender: TObject);
    procedure BtnGenericPOSTClick(Sender: TObject);
    procedure BtnExceptionClick(Sender: TObject);
    procedure BtnGetImageClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure BtnGetStringAndStreamClick(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
    procedure BtnPostStreamClick(Sender: TObject);
    procedure BtnGetWiRLResponseClick(Sender: TObject);
    procedure BtnPersonAndHeaderClick(Sender: TObject);
    procedure BtnPersorOrErrorClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnStringClick(Sender: TObject);
    procedure btnIntClick(Sender: TObject);
    procedure btnFloatClick(Sender: TObject);
    procedure btnParamObjectClick(Sender: TObject);
    procedure btnBoolClick(Sender: TObject);
    procedure btnEnumClick(Sender: TObject);
    procedure btnTimeClick(Sender: TObject);
    procedure btnDateClick(Sender: TObject);
    procedure btnDateTimeClick(Sender: TObject);
    procedure btnMultiPartClick(Sender: TObject);
    procedure btnParamArrayClick(Sender: TObject);
  private
    procedure Log(const ATag, AMessage: string);
    procedure ParamLog(const ATag, AMessage: string);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  System.IOUtils,
  FMXClient.DataModules.Main,
  WiRL.Core.Utils,
  WiRL.Rtti.Utils,
  WiRL.Client.Utils,
  WiRL.Core.JSON, Demo.Entities, FMXClient.DataModules.Params;

procedure TMainForm.btnBoolClick(Sender: TObject);
var
  LResult: Boolean;
begin
  LResult := ParamsModule.GetBoolean(True);
  ParamLog('GetBoolean', BoolToStr(LResult, True));
end;

procedure TMainForm.btnDateClick(Sender: TObject);
var
  LResult: TDate;
begin
  LResult := ParamsModule.GetDate(Date);
  ParamLog('GetDate', DateToStr(LResult));
end;

procedure TMainForm.btnDateTimeClick(Sender: TObject);
var
  LResult: TDateTime;
begin
  LResult := ParamsModule.GetDateTime(Now);
  ParamLog('GetDateTime', DateTimeToStr(LResult));
end;

procedure TMainForm.btnEchoClick(Sender: TObject);
var
  LResult: string;
begin
  LResult := MainDataModule.EchoString(EditInput.Text);

  Log('Echo', LResult);
end;

procedure TMainForm.btnEnumClick(Sender: TObject);
var
  LResult: TMyEnum;
begin
  LResult := ParamsModule.GetEnum(TMyEnum.Second);
  ParamLog('GetEnum', TRttiEnumerationType.GetName(LResult));
end;

procedure TMainForm.btnReverseClick(Sender: TObject);
var
  LResult: string;
begin
  LResult := MainDataModule.ReverseString(EditInput.Text);

  Log('Reverse', LResult);
end;

procedure TMainForm.btnStringClick(Sender: TObject);
var
  LResult: string;
begin
  LResult := ParamsModule.GetString('test');
  ParamLog('GetString', LResult);
end;

procedure TMainForm.btnTimeClick(Sender: TObject);
var
  LResult: TTime;
begin
  LResult := ParamsModule.GetTime(Now);
  ParamLog('GetTime', TimeToStr(LResult));
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  LOrderProposal: TOrderProposal;
  LOrder: TOrder;
begin
  LOrderProposal := TOrderProposal.Create;
  try
    LOrderProposal.Article := 'WiRL';
    LOrderProposal.Description := 'Delphi RESTful Library';
    LOrderProposal.DueDate := Now;
    LOrderProposal.Quantity := 42;

    LOrder := TOrder.Create;
    try
      MainDataModule.FillOrder(LOrderProposal, LOrder);
      Log(
        'FillOrder',
        'Id: ' + LOrder.ID.ToString + sLineBreak +
        'Article: ' + LOrder.Article + sLineBreak +
        'Description: ' + LOrder.Description + sLineBreak +
        'DueDate: ' + DateTimeToStr(LOrder.DueDate)
      );
    finally
      LOrder.Free;
    end;
  finally
    LOrderProposal.Free;
  end;

end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  LImageStream: TStream;
begin
  LImageStream := TMemoryStream.Create;
  try
    MainDataModule.FillImageStreamResource(LImageStream);
    LImageStream.Position := 0;
    Image1.MultiResBitmap.Add.Bitmap.LoadFromStream(LImageStream);
    Log(
      'FillImageStreamResource',
      'Size: ' + LImageStream.Size.ToString
    );
  finally
    LImageStream.Free;
  end;

end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  MainTabControl.ActiveTab := HelloWorldTabItem;
end;

procedure TMainForm.BtnPersonAndHeaderClick(Sender: TObject);
var
  LResponse: TPair<TPerson, string>;
begin
  LResponse := MainDataModule.GetPersonAndHeader(EditPersonID.Text.ToInteger);
  try
    Log('GetReverseAndHeader', LResponse.Key.Name + sLineBreak + LResponse.Value);
  finally
    LResponse.Key.Free;
  end;
end;

procedure TMainForm.BtnPersorOrErrorClick(Sender: TObject);
var
  LPerson: TPerson;
begin
  LPerson := MainDataModule.GetPersonOrError(EditPersonID.Text.ToInteger);
  try
    Log(
      'GetPerson',
      'Name: ' + LPerson.Name + sLineBreak +
      'Age: ' + LPerson.Age.ToString + sLineBreak +
      'Detail: ' + LPerson.Detail
    );
  finally
    LPerson.Free;
  end;

end;

procedure TMainForm.BtnGetStringAndStreamClick(Sender: TObject);
var
  LImageStream: TStream;
  LString: string;
begin
  LImageStream := TMemoryStream.Create;
  try
    LString := MainDataModule.GetStringAndStream(LImageStream);
    LImageStream.Position := 0;
    Log(
      'GetStringAndStream',
      'String: ' + LString + ' - StreamSize: ' + LImageStream.Size.ToString
    );
  finally
    LImageStream.Free;
  end;

end;

procedure TMainForm.Image1DblClick(Sender: TObject);
begin
  Image1.MultiResBitmap.Clear;
end;

procedure TMainForm.Log(const ATag, AMessage: string);
begin
  Memo1.Lines.Add('---------- ' + DateTimeToStr(Now) + ' ' + ATag + ' ----------');
  Memo1.Lines.Add(AMessage);
end;

procedure TMainForm.ParamLog(const ATag, AMessage: string);
begin
  Memo2.Lines.Add('---------- ' + DateTimeToStr(Now) + ' ' + ATag + ' ----------');
  Memo2.Lines.Add(AMessage);
end;

procedure TMainForm.BtnGenericGETClick(Sender: TObject);
var
  LPerson: TPerson;
begin
  LPerson := MainDataModule.GetPerson(EditPersonID.Text.ToInteger);
  try
    Log(
      'GetPerson',
      'Name: ' + LPerson.Name + sLineBreak +
      'Age: ' + LPerson.Age.ToString + sLineBreak +
      'Detail: ' + LPerson.Detail
    );
  finally
    LPerson.Free;
  end;
end;

procedure TMainForm.BtnGenericPOSTClick(Sender: TObject);
var
  LOrderProposal: TOrderProposal;
  LOrder: TOrder;
begin
  LOrderProposal := TOrderProposal.Create;
  try
    LOrderProposal.Article := 'WiRL';
    LOrderProposal.Description := 'Delphi RESTful Library';
    LOrderProposal.DueDate := Now;
    LOrderProposal.Quantity := 42;

    LOrder := MainDataModule.PostOrder(LOrderProposal);
    try
      Log(
        'PortOrder',
        'Id: ' + LOrder.ID.ToString + sLineBreak +
        'Article: ' + LOrder.Article + sLineBreak +
        'Description: ' + LOrder.Description + sLineBreak +
        'DueDate: ' + DateTimeToStr(LOrder.DueDate)
      );
    finally
      LOrder.Free;
    end;
  finally
    LOrderProposal.Free;
  end;
end;

procedure TMainForm.BtnGetImageClick(Sender: TObject);
var
  LImageStream: TStream;
begin
  LImageStream := MainDataModule.GetImageStreamResource;
  try
    Image1.MultiResBitmap.Add.Bitmap.LoadFromStream(LImageStream);
    Log(
      'GetImageStreamResource',
      'Size: ' + LImageStream.Size.ToString
    );
  finally
    LImageStream.Free;
  end;
end;

procedure TMainForm.BtnGetWiRLResponseClick(Sender: TObject);
var
  LResponse: string;
begin
  LResponse := MainDataModule.GetRawResponse;

  Log('GetRawResponse', LResponse);

end;

procedure TMainForm.btnIntClick(Sender: TObject);
var
  LResult: Integer;
begin
  LResult := ParamsModule.GetInteger(42);
  ParamLog('GetInteger', LResult.ToString);
end;

procedure TMainForm.btnLoadClick(Sender: TObject);
begin
  DatabaseModule.GetDBData(dsUsers);
end;

procedure TMainForm.btnMultiPartClick(Sender: TObject);
var
  LResult: string;
  LObject: TSimpleParam;
begin
  LObject := TSimpleParam.Create('Test data');
  try
    LResult := MainDataModule.PostMultiPart(ParamStr(0), LObject, 'AString');
    ParamLog('GetMultiPart', TJSONHelper.PrettyPrint(LResult));
  finally
    LObject.Free;
  end;
end;

procedure TMainForm.btnParamArrayClick(Sender: TObject);
var
  LResult: TArrayInt;
begin
  LResult := ParamsModule.GetArray([1,2,3]);
  {$IFDEF HAS_ARRAY_TO_STRING}
  ParamLog('GetArray', TArray.ToString(LResult));
  {$ELSE}
  ParamLog('GetArray', 'Array size: ' + Length(LResult).ToString);
  {$ENDIF}
end;

procedure TMainForm.btnParamObjectClick(Sender: TObject);
var
  LResult: string;
begin
  LResult := ParamsModule.GetStringFromObject('testobject');
  ParamLog('GetStringFromObject', LResult);
end;

procedure TMainForm.btnPostClick(Sender: TObject);
var
  LResponse: string;
begin
  LResponse := MainDataModule.PostStreamResource.POST<string, string>(EditInput.Text);

  Log('POST', LResponse);
end;

procedure TMainForm.BtnPostStreamClick(Sender: TObject);
var
  LStream: TStream;
  LResponse: string;
begin
  LStream := TStringStream.Create(EditInput.Text, TEncoding.UTF8);
  try
    LResponse := MainDataModule.PostStream(LStream);

    Log('PostStream', LResponse);
  finally
    LStream.Free;
  end;
end;

procedure TMainForm.BtnExceptionClick(Sender: TObject);
var
  LResponse: string;
begin
  LResponse := MainDataModule.TestException;

  Log('TestException', LResponse);
end;

procedure TMainForm.btnExecuteClick(Sender: TObject);
var
  LResponse: string;
begin
  LResponse := MainDataModule.ExecuteHelloWorld;

  Log('ExecuteHelloWorld', LResponse);
end;

procedure TMainForm.btnFloatClick(Sender: TObject);
var
  LResult: Double;
begin
  LResult := ParamsModule.GetFloat(3.14);
  ParamLog('GetFloat', LResult.ToString);
end;

end.
