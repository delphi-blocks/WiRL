{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2026 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Demo.Form.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Actions,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMX.Objects, FMX.ActnList,


  WiRL.http.Client,
  WiRL.http.Client.Interfaces,
  WiRL.http.Client.NetHttp,
  WiRL.Client.Application,
  WiRL.Client.Resource,

  WiRL.Engine.REST,
  WiRL.http.Server,
  WiRL.http.Server.Indy;

type
  TfrmMain = class(TForm)
    memoLog: TMemo;
    btnServer: TButton;
    StyleBook1: TStyleBook;
    listMain: TActionList;
    actServerStart: TAction;
    actServerStop: TAction;
    btnHello: TButton;
    btnTime: TButton;
    btnPostOrder: TButton;
    btnException: TButton;
    procedure actServerStartExecute(Sender: TObject);
    procedure actServerStartUpdate(Sender: TObject);
    procedure actServerStopExecute(Sender: TObject);
    procedure actServerStopUpdate(Sender: TObject);
    procedure btnExceptionClick(Sender: TObject);
    procedure btnHelloClick(Sender: TObject);
    procedure btnPostOrderClick(Sender: TObject);
    procedure btnServerClick(Sender: TObject);
    procedure btnTimeClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FServer: TWiRLServer;
    FClient: TWiRLClient;
    FCLientApp: TWiRLClientApplication;
    procedure SetControl(Sender: TButton; AActive: Boolean);

    procedure SetupClient;
    procedure SetupServer;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  Neon.Core.Persistence.JSON,
  Demo.Entities,
  Demo.Resources;

{$R *.fmx}

procedure TfrmMain.actServerStartExecute(Sender: TObject);
begin
  FServer.Active := not FServer.Active;
end;

procedure TfrmMain.actServerStartUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := not FServer.Active;
end;

procedure TfrmMain.actServerStopExecute(Sender: TObject);
begin
  FServer.Active := False;
end;

procedure TfrmMain.actServerStopUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := FServer.Active;
end;

procedure TfrmMain.btnExceptionClick(Sender: TObject);
begin
  try
    FCLientApp.Resource('demo/exception').Get<string>;
  except
    on E: EWiRLClientProtocolException do
    begin
      memoLog.Lines.Add(TNeon.Print(E.ResponseJson, True));
    end;
  end;
end;

procedure TfrmMain.btnHelloClick(Sender: TObject);
begin
  var hello := FCLientApp.Resource('demo')
    .Get<THelloWorld>;

  memoLog.Lines.Add('Message is: ' + hello.Message);

  // Didn't use try-finally to not clutter the code
  hello.Free;
end;

procedure TfrmMain.btnPostOrderClick(Sender: TObject);
begin
  var o := TOrderProposal.Create;
  o.Article := 'AABBCC';
  o.Description := 'Article aabbcc';
  o.Quantity := 50;
  o.DueDate := Now;

  var order := FCLientApp.Resource('demo/order')
    .Post<TOrderProposal, TOrder>(o);

  memoLog.Lines.Add(Format('ID: %d', [order.ID]));
  memoLog.Lines.Add(Format('Code: %s', [order.Article]));

  // Didn't use try-finally to not clutter the code
  o.Free;
  order.Free;
end;

procedure TfrmMain.btnServerClick(Sender: TObject);
begin
  FServer.Active := not FServer.Active;

  SetControl(Sender as TButton, FServer.Active);
end;

procedure TfrmMain.btnTimeClick(Sender: TObject);
begin
  var time := FCLientApp.Resource('demo/time')
    .Accept('text/plain')
    .Get<TDateTime>;

  memoLog.Lines.Add('Now it''s: ' + TimeToStr(time));
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FServer.Active := False;
  FServer.Free;

  FClientApp.Free;
  FClient.Free;
end;

procedure TfrmMain.SetControl(Sender: TButton; AActive: Boolean);
begin
  var obj := Sender.FindStyleResource('underline');
  var rec := obj as TRectangle;

  if AActive then
  begin
    rec.Fill.Color := TAlphaColorRec.Green;
    Sender.Text := 'Stop Server';
  end
  else
  begin
    rec.Fill.Color := TAlphaColorRec.Red;
    Sender.Text := 'Start Server';
  end;
end;

procedure TfrmMain.SetupClient;
begin
  FClient := TWiRLClient.Create(nil);
  FClient.WiRLEngineURL := 'http://localhost:8080/rest';

  FClientApp := TWiRLClientApplication.Create(nil);
  FClientApp.Client := FClient;
  FCLientApp.SetAppName('app')
end;

procedure TfrmMain.SetupServer;
begin
  FServer := TWiRLServer.Create(nil);
  TDemoServer.ConfigureServer(FServer);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  SetupServer;
  SetupClient;
end;

end.
