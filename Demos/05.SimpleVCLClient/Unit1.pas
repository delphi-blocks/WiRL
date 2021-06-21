{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
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
  WiRL.Client.Application, WiRL.http.Client, Vcl.StdCtrls,
  System.Net.HttpClient.Win, WiRL.Core.MessageBody.Default,
  WiRL.Data.FireDAC.MessageBody.Default, WiRL.http.Accept.MediaType,
  WiRL.http.Client.Indy;

type
  TForm1 = class(TForm)
    WiRLClient1: TWiRLClient;
    WiRLClientApplication1: TWiRLClientApplication;
    employee1: TFDMemTable;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    SendToServerButton: TButton;
    FilterEdit: TEdit;
    Button1: TButton;
    DBResource: TWiRLClientResource;
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
  DBResource.Headers.Accept := TMediaType.APPLICATION_JSON;
  DBResource.QueryParam('filter', FilterEdit.Text);
  DBResource.Get(employee1);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  DBResource.Headers.Accept := TMediaType.APPLICATION_JSON;
  DBResource.Get(employee1);
end;

procedure TForm1.SendToServerButtonClick(Sender: TObject);
var
  LRowChanged: Integer;
begin
  DBResource.Headers.Accept := TMediaType.TEXT_PLAIN;
  LRowChanged := DBResource.Post<TFDMemTable, Integer>(employee1);
  ShowMessage(Format('Changed %d rows', [LRowChanged]));
end;

end.
