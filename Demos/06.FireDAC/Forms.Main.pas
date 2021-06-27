{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Forms.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  WiRL.Client.CustomResource, WiRL.Client.Resource,
  WiRL.Client.Application, WiRL.http.Client, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, System.Rtti, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Fmx.Bind.Grid, System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.Components, Data.Bind.Grid, Data.Bind.DBScope, FMX.Layouts,
  FMX.Grid, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  FMX.Controls.Presentation, FMX.StdCtrls, Data.Bind.Controls,
  Fmx.Bind.Navigator, FMX.ListView.Types, FMX.ListView, FMX.Grid.Style,
  FMX.ScrollBox, FMX.Memo, WiRL.http.Client.Indy, System.Net.HttpClient.Win;

type
  TForm1 = class(TForm)
    WiRLClient1: TWiRLClient;
    WiRLClientApplication1: TWiRLClientApplication;
    StringGrid1: TStringGrid;
    BindingsList1: TBindingsList;
    btnPUT: TButton;
    BindNavigator1: TBindNavigator;
    Layout1: TLayout;
    employee1: TFDMemTable;
    BindSourceDB2: TBindSourceDB;
    LinkGridToDataSourceBindSourceDB2: TLinkGridToDataSource;
    btnGET: TButton;
    btnCloseDataSet: TButton;
    btnOpenDataSet: TButton;
    memoLog: TMemo;
    WiRLClientResource1: TWiRLClientResource;
    procedure btnGETClick(Sender: TObject);
    procedure btnPUTClick(Sender: TObject);
    procedure btnCloseDataSetClick(Sender: TObject);
    procedure btnOpenDataSetClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.btnGETClick(Sender: TObject);
begin
  // Todo
  //WiRLFDResource1.GET();
end;

procedure TForm1.btnPUTClick(Sender: TObject);
begin
  // Todo
  //WiRLFDResource1.PUT();

  //memoLog.Lines.Add(WiRLFDResource1.JSONResponse.ToJSON);
end;

procedure TForm1.btnCloseDataSetClick(Sender: TObject);
begin
  employee1.Close;
end;

procedure TForm1.btnOpenDataSetClick(Sender: TObject);
begin
  employee1.open;
end;

end.
