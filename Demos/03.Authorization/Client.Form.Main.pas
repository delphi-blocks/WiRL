{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2021 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Client.Form.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.TypInfo,
  System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,

  WiRL.http.Client.NetHttp,
  WiRL.http.Client,
  WiRL.Client.Application,

  Client.Filters;

type
  TfrmClientMain = class(TForm)
    edtHost: TEdit;
    pgcAuthApp: TPageControl;
    tsPublicResource: TTabSheet;
    tsLogin: TTabSheet;
    tsPrivateResource: TTabSheet;
    btnPublicResource: TButton;
    edtResourcePublic: TEdit;
    Label2: TLabel;
    memoLog: TMemo;
    btnLoginBasic: TButton;
    edtResourceLogin: TEdit;
    btnPrivateResource: TButton;
    edtResourcePrivate: TEdit;
    Label1: TLabel;
    Label5: TLabel;
    edtToken: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    edtUsername: TEdit;
    edtPassword: TEdit;
    Label14: TLabel;
    Label15: TLabel;
    memoResponse: TMemo;
    Label13: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnLoginBasicClick(Sender: TObject);
    procedure btnPrivateResourceClick(Sender: TObject);
    procedure btnPublicResourceClick(Sender: TObject);
    procedure edtHostChange(Sender: TObject);
  private
    FClient: TWiRLClient;
    FApp: TWiRLClientApplication;
    procedure WiRLInit;
  public
    procedure ShowResponse(const ABody: string);
  end;

var
  frmClientMain: TfrmClientMain;

implementation

uses
  System.JSON,

  Neon.Core.Types,
  WiRL.Core.Classes,
  WiRL.Core.JSON,
  WiRL.Core.Auth.Resource,
  WiRL.http.Accept.MediaType,
  WiRL.Configuration.Neon,
  WiRL.Core.MessageBody.Default,

  Common.Entities;

{$R *.dfm}

procedure TfrmClientMain.FormCreate(Sender: TObject);
begin
  pgcAuthApp.ActivePageIndex := 0;

  FClient := TWiRLClient.Create(nil);
  FApp := TWiRLClientApplication.Create(nil);
  WiRLInit;
end;

procedure TfrmClientMain.ShowResponse(const ABody: string);
begin
  memoResponse.Text := ABody;
end;

procedure TfrmClientMain.WiRLInit;
begin
  // In this demo the configuration of the client is not needed

  // FApp
  //   .SetReaders('*.*')
  //   .SetWriters('*.*')
  //   .SetFilters('*.*')
  //   .Plugin.Configure<IWiRLConfigurationNeon>
  //     .SetUseUTCDate(True)
  //     .SetVisibility([mvPublic, mvPublished])
  //     .SetMemberCase(TNeonCase.CamelCase)
  //     .ApplyConfig;

  FApp.AppName := 'app';
  FApp.Client := FClient;
end;

procedure TfrmClientMain.btnPublicResourceClick(Sender: TObject);
var
  LUserInfo: TUserInfo;
begin
  memoLog.Lines.Add('Getting Public Resource (no auth required)');

  LUserInfo := FApp
    .Resource(edtResourcePublic.Text)
    .Accept(TMediaType.APPLICATION_JSON)
    .Get<TUserInfo>;

  try
    ShowMessage('FullName: ' + LUserInfo.FullName);
  finally
    LUserInfo.Free;
  end;

  memoLog.Lines.Add('Done.');
end;

procedure TfrmClientMain.btnLoginBasicClick(Sender: TObject);
var
  LLoginResponse: TWiRLLoginResponse;
begin
  memoLog.Lines.Add('Basic Auth Login');

  LLoginResponse := FApp
    .Resource(edtResourceLogin.Text)
    .Authorization(TBasicAuth.Create(edtUsername.Text, edtPassword.Text))
    .Accept(TMediaType.APPLICATION_JSON)
    .Post<string, TWiRLLoginResponse>('');

  try
    edtToken.Text := LLoginResponse.AccessToken;
    ShowMessage('Token: ' + LLoginResponse.AccessToken);
  finally
    LLoginResponse.Free;
  end;
  memoLog.Lines.Add('Login done.');
end;

procedure TfrmClientMain.btnPrivateResourceClick(Sender: TObject);
var
  LJSON: TJSONValue;
begin
  memoLog.Lines.Add('Getting Private Resource (auth required)');

  LJSON := FApp
    .Resource(edtResourcePrivate.Text)
    .Authorization(TBearerAuth.Create(edtToken.Text))
    .Accept(TMediaType.APPLICATION_JSON)
    .Get<TJSONObject>();
  try
    ShowMessage('Message: ' + LJSON.GetValue<string>('Message'));
  finally
    LJSON.Free;
  end;
  memoLog.Lines.Add('Done.');
end;

procedure TfrmClientMain.edtHostChange(Sender: TObject);
begin
  FClient.WiRLEngineURL := edtHost.Text;
end;

end.
