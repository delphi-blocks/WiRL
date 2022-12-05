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
  FMX.ScrollBox, FMX.Memo, WiRL.http.Client.Indy, System.Net.HttpClient.Win,
  FMX.Memo.Types, WiRL.Core.MessageBody.Default, WiRL.Data.MessageBody.Default{,
  WiRL.Data.FireDAC.MessageBody.Default};

type
  TForm1 = class(TForm)
    WiRLClient1: TWiRLClient;
    WiRLClientApplication1: TWiRLClientApplication;
    StringGrid1: TStringGrid;
    BindingsList1: TBindingsList;
    btnPUT: TButton;
    BindNavigator1: TBindNavigator;
    Layout1: TLayout;
    employee: TFDMemTable;
    BindSourceDB2: TBindSourceDB;
    LinkGridToDataSourceBindSourceDB2: TLinkGridToDataSource;
    btnGETALL: TButton;
    memoLog: TMemo;
    WiRLClientResource1: TWiRLClientResource;
    customers: TFDMemTable;
    WiRLClientResource2: TWiRLClientResource;
    employeeEmployeeID: TIntegerField;
    employeeFirstName: TStringField;
    employeeLastName: TStringField;
    StringGrid2: TStringGrid;
    BindSourceDB1: TBindSourceDB;
    LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource;
    customersCompanyName: TStringField;
    customersCustomerID: TStringField;
    BtnGET: TButton;
    procedure btnGETALLClick(Sender: TObject);
    procedure btnPUTClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtnGETClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.btnGETALLClick(Sender: TObject);
begin
  // Close and clear the datasets
  employee.Close;
  customers.Close;

  WiRLClientResource2.Get<TArray<TDataSet>>([employee,customers]);
end;

procedure TForm1.BtnGETClick(Sender: TObject);
begin
  // Close and clear the datasets
  employee.Close;
  customers.Close;

  // DataSet should be open
  employee.Open;
  WiRLClientResource1.Get(employee);

end;

procedure TForm1.btnPUTClick(Sender: TObject);
begin
  // Todo
  raise Exception.Create('Not yet implemented');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  WiRLClientApplication1.SetReaders('*.*');
  WiRLClientApplication1.SetWriters('*.*');
end;

end.
