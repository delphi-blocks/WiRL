{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.Server.Indy;

interface

uses
  System.Classes, System.SysUtils, System.SyncObjs,
  IdContext, IdCookie, IdCustomHTTPServer, IdHTTPServer, IdException, IdTCPServer, IdIOHandlerSocket,
  IdSchedulerOfThreadPool, idGlobal, IdGlobalProtocols, IdURI, IdResourceStringsProtocols,

  WiRL.Core.Classes,
  WiRL.http.Core,
  WiRL.http.Headers,
  WiRL.http.Cookie,
  WiRL.http.Server.Interfaces,
  WiRL.http.Engines,
  WiRL.http.Response,
  WiRL.http.Request,
  WiRL.Core.Auth.Context;

type
  TWiRLhttpServerIndy = class(TInterfacedObject, IWiRLServer)
  private
    FHttpServer: TIdHTTPServer;
    FListener: IWiRLListener;
    FThreadPoolSize: Integer;
  protected
    procedure ParseAuthorizationHeader(AContext: TIdContext; const AAuthType,
        AAuthData: string; var VUsername, VPassword: string; var VHandled: Boolean);

    { IWiRLServer }
    procedure Startup;
    procedure Shutdown;
    function GetPort: Word;
    procedure SetPort(AValue: Word);
    function GetThreadPoolSize: Integer;
    procedure SetThreadPoolSize(AValue: Integer);
    function GetListener: IWiRLListener;
    procedure SetListener(AValue: IWiRLListener);
    function GetServerImplementation: TObject;

    procedure DoCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure DoCommandOther(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);

    procedure SetupThreadPooling(const APoolSize: Integer = 25);

  public
    constructor Create;
    destructor Destroy; override;
  end;

  TWiRLHttpResponseIndy = class(TWiRLResponse)
  private
    FContext: TIdContext;
    FResponseInfo: TIdHTTPResponseInfo;
    FHeaders: IWiRLHeaders;
    procedure SendCookies;
  protected
    function GetContent: string; override;
    function GetContentStream: TStream; override;
    procedure SetContent(const Value: string); override;
    procedure SetContentStream(const Value: TStream); override;
    function GetStatusCode: Integer; override;
    procedure SetStatusCode(const Value: Integer); override;
    function GetReasonString: string; override;
    procedure SetReasonString(const Value: string); override;
    function IsUnknownResponseCode: Boolean; override;
    function GetHeaders: IWiRLHeaders; override;
  public
    procedure SendHeaders; override;
    constructor Create(AContext: TIdContext; AResponseInfo: TIdHTTPResponseInfo);
    destructor Destroy; override;
  end;

  TWiRLHttpRequestIndy = class(TWiRLRequest)
  private
    FContext: TIdContext;
    FRequestInfo: TIdHTTPRequestInfo;
    FCookieFields: TWiRLCookies;
    FQueryFields: TWiRLParam;
    FHeaders: IWiRLHeaders;
    FContentFields: TWiRLParam;
    procedure ParseParams(Params :TStrings; const AValue: String);
  protected
    function GetHttpPathInfo: string; override;
    function GetHttpQuery: string; override;
    function GetRemoteIP: string; override;
    function GetServerPort: Integer; override;
    function GetQueryFields: TWiRLParam; override;
    function GetContentFields: TWiRLParam; override;
    function GetCookieFields: TWiRLCookies; override;
    function GetHeaders: IWiRLHeaders; override;
    function GetContentStream: TStream; override;
    procedure SetContentStream(const Value: TStream); override;
  public
    constructor Create(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo);
    destructor Destroy; override;
  end;

  TWiRLIdHttpServer = class(TIdHTTPServer)
  protected
    procedure DoneWithPostStream(ASender: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo); override;
  end;

implementation

uses
  System.StrUtils,
  WiRL.http.Accept.MediaType,
  WiRL.Core.Context,
  WiRL.Core.Utils;

constructor TWiRLhttpServerIndy.Create;
begin
  inherited;
  FHttpServer := TWiRLIdHttpServer.Create(nil);
  FHttpServer.ParseParams := False;
  FHttpServer.OnCommandGet := DoCommandGet;
  FHttpServer.OnCommandOther := DoCommandOther;
  FHttpServer.OnParseAuthentication := ParseAuthorizationHeader;
end;

destructor TWiRLhttpServerIndy.Destroy;
begin
  FHttpServer.Active := False;
  FHttpServer.Free;
  inherited;
end;

procedure TWiRLhttpServerIndy.DoCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  LRequest: TWiRLRequest;
  LResponse: TWiRLResponse;
begin
  inherited;
  if EndsText('/favicon.ico', ARequestInfo.Document) then
    Exit;

  LRequest := TWiRLHttpRequestIndy.Create(AContext, ARequestInfo);
  try
    LResponse := TWiRLHttpResponseIndy.Create(AContext, AResponseInfo);
    try
      AResponseInfo.FreeContentStream := True;
      if LResponse.Server = '' then
        LResponse.Server := 'WiRL Server (Indy)';
      FListener.HandleRequest(LRequest, LResponse);
    finally
      LResponse.Free;
    end;
  finally
    LRequest.Free;
  end;
end;

procedure TWiRLhttpServerIndy.DoCommandOther(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  inherited;
  DoCommandGet(AContext, ARequestInfo, AResponseInfo);
end;

function TWiRLhttpServerIndy.GetListener: IWiRLListener;
begin
  Result := FListener;
end;

function TWiRLhttpServerIndy.GetPort: Word;
begin
  Result := FHttpServer.DefaultPort;
end;

function TWiRLhttpServerIndy.GetServerImplementation: TObject;
begin
  Result := FHttpServer;
end;

function TWiRLhttpServerIndy.GetThreadPoolSize: Integer;
begin
  Result := FThreadPoolSize;
end;

procedure TWiRLhttpServerIndy.ParseAuthorizationHeader(AContext: TIdContext;
  const AAuthType, AAuthData: string; var VUsername, VPassword: string; var VHandled: Boolean);
begin
  VHandled := True;
end;

procedure TWiRLhttpServerIndy.SetListener(AValue: IWiRLListener);
begin
  FListener := AValue;
end;

procedure TWiRLhttpServerIndy.SetPort(AValue: Word);
begin
  FHttpServer.DefaultPort := AValue;
end;

procedure TWiRLhttpServerIndy.SetThreadPoolSize(AValue: Integer);
begin
  FThreadPoolSize := AValue;
end;

procedure TWiRLhttpServerIndy.SetupThreadPooling(const APoolSize: Integer);
var
  LScheduler: TIdSchedulerOfThreadPool;
begin
  if Assigned(FHttpServer.Scheduler) then
  begin
    FHttpServer.Scheduler.Free;
    FHttpServer.Scheduler := nil;
  end;

  LScheduler := TIdSchedulerOfThreadPool.Create(FHttpServer);
  LScheduler.PoolSize := APoolSize;
  FHttpServer.Scheduler := LScheduler;
  FHttpServer.MaxConnections := LScheduler.PoolSize;
end;

procedure TWiRLhttpServerIndy.Shutdown;
begin
  inherited;
  if FHttpServer.Active then
    FHttpServer.Active := False;
  FHttpServer.Bindings.Clear;
end;

procedure TWiRLhttpServerIndy.Startup;
begin
  inherited;
  FHttpServer.Bindings.Clear;
  FHttpServer.Active := True;
end;

{ TWiRLHttpRequestIndy }

constructor TWiRLHttpRequestIndy.Create(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo);
begin
  inherited Create;
  FContext := AContext;
  FRequestInfo := ARequestInfo;
  FMethod := FRequestInfo.Command;
end;

destructor TWiRLHttpRequestIndy.Destroy;
begin
  FRequestInfo.PostStream.Free;
  FRequestInfo.PostStream := nil;
  FCookieFields.Free;
  FQueryFields.Free;
  FContentFields.Free;
  inherited;
end;

procedure TWiRLHttpRequestIndy.ParseParams(Params :TStrings; const AValue: String);
var
  LIndex, LStrIndex: Integer;
  LTempStr: string;
  LEncoding: IIdTextEncoding;
begin
  Params.BeginUpdate;
  try
    Params.Clear;

    if FRequestInfo.CharSet <> '' then
      LEncoding := CharsetToEncoding(FRequestInfo.CharSet)
    else
      LEncoding := IndyTextEncoding_UTF8;
    LIndex := 1;
    while LIndex <= Length(AValue) do
    begin
      LStrIndex := LIndex;
      while (LStrIndex <= Length(AValue)) and (AValue[LStrIndex] <> '&') do {do not localize}
      begin
        Inc(LStrIndex);
      end;
      LTempStr := Copy(AValue, LIndex, LStrIndex-LIndex);
      LTempStr := StringReplace(LTempStr, '+', ' ', [rfReplaceAll]);
      Params.Add(TIdURI.URLDecode(LTempStr, LEncoding));
      LIndex := LStrIndex + 1;
    end;
  finally
    Params.EndUpdate;
  end;
end;

procedure TWiRLHttpRequestIndy.SetContentStream(const Value: TStream);
begin
  inherited;
  if Assigned(FRequestInfo.PostStream) then
    FRequestInfo.PostStream.Free;
  FRequestInfo.PostStream := Value;
end;

function TWiRLHttpRequestIndy.GetContentStream: TStream;
begin
  Result := FRequestInfo.PostStream;
end;

function TWiRLHttpRequestIndy.GetContentFields: TWiRLParam;
begin
  if not Assigned(FContentFields) then
  begin
    FContentFields := TWiRLParam.Create;
    ParseParams(FContentFields, FRequestInfo.FormParams);
  end;
  Result := FContentFields;
end;

function TWiRLHttpRequestIndy.GetCookieFields: TWiRLCookies;
var
  LIndex: Integer;
begin
  if not Assigned(FCookieFields) then
  begin
    FCookieFields := TWiRLCookies.Create;
    for LIndex := 0 to FRequestInfo.Cookies.Count - 1 do
    begin
      CookieFields.AddClientCookie(FRequestInfo.Cookies[LIndex].ClientCookie);
    end;
  end;
  Result := FCookieFields;
end;

function TWiRLHttpRequestIndy.GetHeaders: IWiRLHeaders;
var
  LIndex: Integer;
  LName, LValue: string;
begin
  if not Assigned(FHeaders) then
  begin
    FHeaders := TWiRLHeaders.Create;
    for LIndex := 0 to FRequestInfo.RawHeaders.Count - 1 do
    begin
      LName := FRequestInfo.RawHeaders.Names[LIndex];
      LValue := FRequestInfo.RawHeaders.Values[LName];
      FHeaders.AddHeader(TWiRLHeader.Create(LName, LValue));
    end;
  end;
  Result := FHeaders;
end;

function TWiRLHttpRequestIndy.GetHttpPathInfo: string;
begin
  Result := FRequestInfo.Document;
end;

function TWiRLHttpRequestIndy.GetHttpQuery: string;
begin
  Result := FRequestInfo.QueryParams;
end;

function TWiRLHttpRequestIndy.GetQueryFields: TWiRLParam;
//const
//  ContentTypeFormUrlencoded = 'application/x-www-form-urlencoded';
begin
  if not Assigned(FQueryFields) then
  begin
    FQueryFields := TWiRLParam.Create;
    ParseParams(FQueryFields, FRequestInfo.QueryParams);
  end;
  Result := FQueryFields;
end;

function TWiRLHttpRequestIndy.GetRemoteIP: string;
begin
  Result := FRequestInfo.RemoteIP;
end;

function TWiRLHttpRequestIndy.GetServerPort: Integer;
var
  LValue: string;
begin
  LValue := FRequestInfo.Host;
  Fetch(LValue, ':');
  if Length(LValue) = 0 then begin
    LValue := IntToStr(FContext.Connection.Socket.Binding.Port);
    // LValue := '80';
  end;
  Result := StrToIntDef(LValue, -1);
end;

{ TWiRLHttpResponseIndy }

constructor TWiRLHttpResponseIndy.Create(AContext: TIdContext;
  AResponseInfo: TIdHTTPResponseInfo);
var
  LName, LValue: string;
  LIndex: Integer;
begin
  inherited Create;
  FContext := AContext;
  FResponseInfo := AResponseInfo;

  FHeaders := TWiRLHeaders.Create;
  for LIndex := 0 to AResponseInfo.RawHeaders.Count - 1 do
  begin
    LName := AResponseInfo.RawHeaders.Names[LIndex];
    LValue := AResponseInfo.RawHeaders.Values[LName];
    FHeaders.AddHeader(TWiRLHeader.Create(LName, LValue));
  end;
end;

destructor TWiRLHttpResponseIndy.Destroy;
begin
//  FHeaders.Free;
  inherited;
end;

function TWiRLHttpResponseIndy.GetContent: string;
begin
  Result := FResponseInfo.ContentText;
end;

function TWiRLHttpResponseIndy.GetContentStream: TStream;
begin
  Result := FResponseInfo.ContentStream;
end;

function TWiRLHttpResponseIndy.GetHeaders: IWiRLHeaders;
begin
  Result := FHeaders;
end;

function TWiRLHttpResponseIndy.GetReasonString: string;
begin
  Result := FResponseInfo.ResponseText;
end;

function TWiRLHttpResponseIndy.GetStatusCode: Integer;
begin
  Result := FResponseInfo.ResponseNo;
end;

function TWiRLHttpResponseIndy.IsUnknownResponseCode: Boolean;
begin
  Result := ReasonString = RSHTTPUnknownResponseCode;
end;

procedure TWiRLHttpResponseIndy.SendCookies;
var
  LWiRLCookie: TWiRLCookie;
  LIdCookie: TIdCookie;
begin
  for LWiRLCookie in Cookies do
  begin
    LIdCookie := FResponseInfo.Cookies.Add;

    LIdCookie.CookieName := LWiRLCookie.CookieName;
    LIdCookie.Value := LWiRLCookie.Value;
    LIdCookie.Domain := LWiRLCookie.Domain;
    LIdCookie.Expires := LWiRLCookie.Expires;
    LIdCookie.HttpOnly := LWiRLCookie.HttpOnly;
    LIdCookie.Path := LWiRLCookie.Path;
    LIdCookie.Secure := LWiRLCookie.Secure;
    LIdCookie.CreatedAt := LWiRLCookie.CreatedAt;
    LIdCookie.HostOnly := LWiRLCookie.HostOnly;
    LIdCookie.LastAccessed := LWiRLCookie.LastAccessed;
    LIdCookie.Persistent := LWiRLCookie.Persistent;
  end;
end;

procedure TWiRLHttpResponseIndy.SendHeaders;

  function IsIndyHeader(const Name: string): Boolean;
  const
    IndyHeaders: array [0..3] of string = ('Date', 'Content-Type', 'Content-Length', 'Connection');
  var
    IndyHeader: string;
  begin
    Result := False;
    for IndyHeader in IndyHeaders do
      if CompareText(Name, IndyHeader) = 0 then
        Exit(True);
  end;

var
  //LIndex :Integer;
  LHeader: TWiRLHeader;
begin
  inherited;
  FResponseInfo.Date := GMTToLocalDateTime(Headers.Values['Date']);
  FResponseInfo.CustomHeaders.Clear;

  for LHeader in Headers do
  begin
    if IsIndyHeader(LHeader.Name) then
      Continue;
    FResponseInfo.CustomHeaders.AddValue(LHeader.Name, LHeader.Value);
  end;
  if ContentType <> '' then
    ContentMediaType.Parse(ContentType);
  FResponseInfo.ContentType := ContentMediaType.AcceptItemOnly;
  FResponseInfo.CharSet := ContentMediaType.Charset;
  if Connection <> '' then
    FResponseInfo.Connection := Connection;

  if HasContentLength then
    FResponseInfo.ContentLength := ContentLength;

  SendCookies;
end;

procedure TWiRLHttpResponseIndy.SetContent(const Value: string);
begin
  inherited;
  FResponseInfo.ContentText := Value;
end;

procedure TWiRLHttpResponseIndy.SetContentStream(const Value: TStream);
begin
  inherited;
  if Assigned(FResponseInfo.ContentStream) then
    FResponseInfo.ContentStream.Free;
  FResponseInfo.ContentStream := Value;
end;

procedure TWiRLHttpResponseIndy.SetReasonString(const Value: string);
begin
  inherited;
  FResponseInfo.ResponseText := Value;
end;

procedure TWiRLHttpResponseIndy.SetStatusCode(const Value: Integer);
begin
  inherited;
  FResponseInfo.ResponseNo := Value;
end;

{ TWiRLIdHttpServer }

procedure TWiRLIdHttpServer.DoneWithPostStream(ASender: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo);
begin
  // Avoid that indy Free the PostStream and put the variable to nil
  // too early. The problem is that indy, when the PostStream is
  // x-www-form-urlencoded, parses the data and frees the stream.
end;

initialization
  TWiRLServerRegistry.Instance.RegisterServer<TWiRLhttpServerIndy>('TIdHttpServer (Indy)');


end.
