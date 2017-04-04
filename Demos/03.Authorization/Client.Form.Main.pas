unit Client.Form.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent;

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
    Label3: TLabel;
    memoHeadersPublic: TMemo;
    memoResponsePublic: TMemo;
    Label4: TLabel;
    httpRestClient: TNetHTTPClient;
    httpRequest: TNetHTTPRequest;
    memoLog: TMemo;
    btnLoginBasic: TButton;
    edtResourceLogin: TEdit;
    btnPrivateResource: TButton;
    edtResourcePrivate: TEdit;
    Label1: TLabel;
    Label6: TLabel;
    memoHeadersLogin: TMemo;
    Label7: TLabel;
    memoResponseLogin: TMemo;
    Label5: TLabel;
    edtToken: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    memoHeadersPrivate: TMemo;
    Label13: TLabel;
    memoResponsePrivate: TMemo;
    btnCompileLogin: TButton;
    edtUsername: TEdit;
    edtPassword: TEdit;
    Label14: TLabel;
    Label15: TLabel;
    btnCompilePrivate: TButton;
    btnCompilePublic: TButton;
    procedure btnCompileLoginClick(Sender: TObject);
    procedure btnCompilePrivateClick(Sender: TObject);
    procedure btnLoginBasicClick(Sender: TObject);
    procedure btnPrivateResourceClick(Sender: TObject);
    procedure btnPublicResourceClick(Sender: TObject);
  private
    function DoHttpRequest(const AMethod, AResource: string; AHeaders: TStrings;
        ARequestBody: TStream = nil): string;
  public
    { Public declarations }
  end;

var
  frmClientMain: TfrmClientMain;

implementation

uses
  System.IOUtils, System.NetEncoding, REST.Json, System.JSON;

{$R *.dfm}

procedure TfrmClientMain.btnCompileLoginClick(Sender: TObject);
var
  LBasicAuth: string;
begin
  LBasicAuth := 'Authorization=Basic ' +
    TNetEncoding.Base64.Encode(edtUsername.Text + ':' + edtPassword.Text);

  memoHeadersLogin.Lines.Add(LBasicAuth);
end;

procedure TfrmClientMain.btnCompilePrivateClick(Sender: TObject);
var
  LBearerAuth: string;
begin
  LBearerAuth := 'Authorization=Bearer ' + edtToken.Text;

  memoHeadersPrivate.Lines.Add(LBearerAuth);
end;

procedure TfrmClientMain.btnPublicResourceClick(Sender: TObject);
var
  LJSON: TJSONValue;
  LRes: string;
begin
  memoLog.Lines.Add('Getting Public Resource (no auth required)');
  LRes := DoHttpRequest(sHTTPMethodGet, edtResourcePublic.Text, memoHeadersPublic.Lines);

  LJSON := TJSONObject.ParseJSONValue(LRes);
  try
    memoResponsePublic.Lines.Text := TJson.Format(LJSON);
  finally
    LJSON.Free;
  end;
end;

procedure TfrmClientMain.btnLoginBasicClick(Sender: TObject);
var
  LJSON: TJSONValue;
  LRes: string;
begin
  memoLog.Lines.Add('Basic Auth Login');
  LRes := DoHttpRequest(sHTTPMethodPost, edtResourceLogin.Text, memoHeadersLogin.Lines);

  LJSON := TJSONObject.ParseJSONValue(LRes);
  try
    edtToken.Text := LJSON.GetValue<string>('access_token');
    memoResponseLogin.Lines.Text := TJson.Format(LJSON);
  finally
    LJSON.Free;
  end;
end;

procedure TfrmClientMain.btnPrivateResourceClick(Sender: TObject);
var
  LJSON: TJSONValue;
  LRes: string;
begin
  memoLog.Lines.Add('Getting Private Resource (auth required)');
  LRes := DoHttpRequest(sHTTPMethodGet, edtResourcePrivate.Text, memoHeadersPrivate.Lines);

  LJSON := TJSONObject.ParseJSONValue(LRes);
  try
    memoResponsePrivate.Lines.Text := TJson.Format(LJSON);
  finally
    LJSON.Free;
  end;
end;

function TfrmClientMain.DoHttpRequest(const AMethod, AResource: string;
    AHeaders: TStrings; ARequestBody: TStream = nil): string;
var
  LURL, LName: string;
  LHeader: TNetHeader;
  LHeaders: TNetHeaders;
  LResponse: IHTTPResponse;
  LIndex: Integer;
begin
  LURL := TPath.Combine(edtHost.Text, AResource);

  memoLog.Lines.Add('URL: ' + LURL);

  for LIndex := 0 to AHeaders.Count - 1 do
  begin
    LName := AHeaders.Names[LIndex];
    LHeader := TNetHeader.Create(LName, AHeaders.Values[LName]);
    LHeaders := LHeaders + [LHeader];
  end;

  if AMethod = sHTTPMethodGet then
    LResponse := httpRequest.Get(LURL, nil, LHeaders)
  else if AMethod = sHTTPMethodPost then
    LResponse := httpRequest.Post(LURL, ARequestBody, nil, LHeaders);
  Result := LResponse.ContentAsString();

  memoLog.Lines.Add('Operation done......');
end;

end.
