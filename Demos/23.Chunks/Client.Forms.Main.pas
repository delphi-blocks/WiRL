unit Client.Forms.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, IdBaseComponent,
  IdComponent, IdTCPConnection, IdTCPClient, IdHTTP, IdGlobal,
  System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent;

type
  TForm29 = class(TForm)
    ButtonIndySync: TButton;
    IdHTTPSync: TIdHTTP;
    MemoLog: TMemo;
    IdHTTPAsync: TIdHTTP;
    ButtonIndyAsync: TButton;
    NetHTTPClientSync: TNetHTTPClient;
    ButtonHttpSync: TButton;
    NetHTTPClientAsync: TNetHTTPClient;
    ButtonHttpAsync: TButton;
    procedure ButtonIndySyncClick(Sender: TObject);
    procedure ButtonIndyAsyncClick(Sender: TObject);
    procedure ButtonHttpAsyncClick(Sender: TObject);
    procedure IdHTTPAsyncChunkReceived(Sender: TObject; var Chunk: TIdBytes);
    procedure NetHTTPClientAsyncReceiveDataEx(const Sender: TObject;
      AContentLength, AReadCount: Int64; AChunk: Pointer;
      AChunkLength: Cardinal; var AAbort: Boolean);
    procedure ButtonHttpSyncClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form29: TForm29;

implementation

{$R *.dfm}

const
  ServiceUrl = 'http://localhost:8080/rest/app/streaming/chunks';

procedure TForm29.ButtonHttpAsyncClick(Sender: TObject);
begin
  MemoLog.Lines.Clear;
  MemoLog.Lines.Add('Loading...');
  NetHTTPClientAsync.Get(ServiceUrl);
  MemoLog.Lines.Add('Done');
end;

procedure TForm29.ButtonHttpSyncClick(Sender: TObject);
var
  LResponse: IHTTPResponse;
begin
  MemoLog.Lines.Clear;
  Application.ProcessMessages;
  MemoLog.Lines.Add('Loading...');
  LResponse := NetHTTPClientSync.Get(ServiceUrl);
  MemoLog.Lines.Add(LResponse.ContentAsString);
  MemoLog.Lines.Add('Done');
end;

procedure TForm29.ButtonIndyAsyncClick(Sender: TObject);
begin
  MemoLog.Lines.Clear;
  MemoLog.Lines.Add('Loading...');
  IdHTTPAsync.Get(ServiceUrl);
  MemoLog.Lines.Add('Done');
end;

procedure TForm29.ButtonIndySyncClick(Sender: TObject);
var
  LResult: string;
begin
  MemoLog.Lines.Clear;
  Application.ProcessMessages;
  MemoLog.Lines.Add('Loading...');
  LResult := IdHTTPSync.Get(ServiceUrl);
  MemoLog.Lines.Add(LResult);
  MemoLog.Lines.Add('Done');
end;

procedure TForm29.IdHTTPAsyncChunkReceived(Sender: TObject;
  var Chunk: TIdBytes);
begin
  MemoLog.Lines.Text :=
    MemoLog.Lines.Text +
    IndyTextEncoding_UTF8.GetString(Chunk);

  Application.ProcessMessages;
end;

procedure TForm29.NetHTTPClientAsyncReceiveDataEx(const Sender: TObject;
  AContentLength, AReadCount: Int64; AChunk: Pointer; AChunkLength: Cardinal;
  var AAbort: Boolean);
var
  LBytes: TBytes;
  LChunkStr: string;
begin
  if AChunkLength > 0 then
  begin
    SetLength(LBytes, AChunkLength);
    Move(AChunk^, LBytes[0], AChunkLength);
    LChunkStr := TEncoding.UTF8.GetString(LBytes);
    MemoLog.Lines.Text := MemoLog.Lines.Text + LChunkStr;
  end;

  Application.ProcessMessages;
end;

end.
