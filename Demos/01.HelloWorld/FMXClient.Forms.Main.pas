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
  Data.Bind.Components, Data.Bind.Grid, Data.Bind.DBScope;

type
  TMainForm = class(TForm)
    ToolBar1: TToolBar;
    MainTabControl: TTabControl;
    HelloWorldTabItem: TTabItem;
    StringDemosTabItem: TTabItem;
    btnExecute: TButton;
    Memo1: TMemo;
    Layout1: TLayout;
    Layout2: TLayout;
    Edit1: TEdit;
    Label1: TLabel;
    btnEcho: TButton;
    Edit2: TEdit;
    Label2: TLabel;
    Layout3: TLayout;
    Edit3: TEdit;
    Label3: TLabel;
    btnReverse: TButton;
    Edit4: TEdit;
    Label4: TLabel;
    btnPost: TButton;
    BtnGenericGET: TButton;
    BtnGenericPOST: TButton;
    procedure btnExecuteClick(Sender: TObject);
    procedure btnEchoClick(Sender: TObject);
    procedure btnReverseClick(Sender: TObject);
    procedure btnPostClick(Sender: TObject);
    procedure BtnGenericGETClick(Sender: TObject);
    procedure BtnGenericPOSTClick(Sender: TObject);
  private
    { Private declarations }
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
begin
  Edit2.Text := MainDataModule.EchoString(Edit1.Text);
end;

procedure TMainForm.btnReverseClick(Sender: TObject);
begin
  Edit4.Text := MainDataModule.ReverseString(Edit3.Text);
end;

procedure TMainForm.BtnGenericGETClick(Sender: TObject);
var
  LPerson: TPerson;
begin
  LPerson := MainDataModule.GetPerson(12);
  try
    ShowMessage(
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
      ShowMessage(
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

procedure TMainForm.btnPostClick(Sender: TObject);
var
  LResponse: string;
begin
  LResponse := MainDataModule.PostStreamResource.POST<string, string>('Hello, World!');
  ShowMessage(LResponse);
end;

procedure TMainForm.btnExecuteClick(Sender: TObject);
begin
  Memo1.Lines.Add(MainDataModule.ExecuteHelloWorld);
end;

end.
