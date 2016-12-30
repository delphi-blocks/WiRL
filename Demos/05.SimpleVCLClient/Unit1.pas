{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, Data.DB, Vcl.Grids, Vcl.DBGrids, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, WiRL.Client.CustomResource, WiRL.Client.Resource,
  WiRL.Client.FireDAC, WiRL.Client.Application, WiRL.Client.Client, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    WiRLClient1: TWiRLClient;
    WiRLClientApplication1: TWiRLClientApplication;
    WiRLDatamoduleResource: TWiRLFDResource;
    employee1: TFDMemTable;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    SendToServerButton: TButton;
    FilterEdit: TEdit;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure SendToServerButtonClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  WiRLDatamoduleResource.QueryParams.Values['filter'] := FilterEdit.Text;
  WiRLDatamoduleResource.GET();
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  WiRLDatamoduleResource.GET();
end;

procedure TForm1.SendToServerButtonClick(Sender: TObject);
begin
  WiRLDatamoduleResource.POST();
end;

end.
