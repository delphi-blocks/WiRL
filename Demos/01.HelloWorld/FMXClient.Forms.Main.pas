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
  FMX.Objects;

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
  private
    procedure Log(const ATag, AMessage: string);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  FMXClient.DataModules.Main,
  WiRL.Core.Utils,
  WiRL.Rtti.Utils,
  WiRL.Client.Utils,
  WiRL.Core.JSON, Demo.Entities;

procedure TMainForm.btnEchoClick(Sender: TObject);
var
  LResult: string;
begin
  LResult := MainDataModule.EchoString(EditInput.Text);

  Log('Echo', LResult);
end;

procedure TMainForm.btnReverseClick(Sender: TObject);
var
  LResult: string;
begin
  LResult := MainDataModule.ReverseString(EditInput.Text);

  Log('Reverse', LResult);
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

procedure TMainForm.BtnPersonAndHeaderClick(Sender: TObject);
var
  LResponse: TPair<TPerson, string>;
begin
  LResponse := MainDataModule.GetPersonAndHeader(12);
  try
    Log('GetReverseAndHeader', LResponse.Key.Name + sLineBreak + LResponse.Value);
  finally
    LResponse.Key.Free;
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

procedure TMainForm.BtnGenericGETClick(Sender: TObject);
var
  LPerson: TPerson;
begin
  LPerson := MainDataModule.GetPerson(12);
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

end.
