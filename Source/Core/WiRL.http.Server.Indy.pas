{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.Server.Indy;

interface

uses
  System.Classes, System.SysUtils, System.SyncObjs,
  IdContext, IdCookie, IdCustomHTTPServer, IdHTTPServer, IdException, IdTCPServer, IdIOHandlerSocket,
  IdSchedulerOfThreadPool, idGlobal, IdGlobalProtocols, IdURI,
  WiRL.http.Core,
  WiRL.http.Cookie,
  WiRL.http.Server,
  WiRL.http.Engines,
  WiRL.Core.Engine,
  WiRL.Core.Response,
  WiRL.Core.Request,
  WiRL.Core.Auth.Context;

type
  TWiRLEngines = TArray<TWiRLEngine>;

  TWiRLhttpServerIndy = class(TWiRLhttpServer)
  private
    FServer: TIdHTTPServer;
  protected
    procedure ParseAuthorizationHeader(AContext: TIdContext; const AAuthType,
        AAuthData: string; var VUsername, VPassword: string; var VHandled: Boolean);

    procedure Startup; override;
    procedure Shutdown; override;

    procedure DoCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure DoCommandOther(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);

    procedure SetupThreadPooling(const APoolSize: Integer = 25);

  public
    constructor Create; override;
    destructor Destroy; override;

    property Server: TIdHTTPServer read FServer;
  end;

  TWiRLHttpResponseIndy = class(TWiRLResponse)
  private
    FContext: TIdContext;
    FResponseInfo: TIdHTTPResponseInfo;
    FCustomHeaders :TStrings;
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
    FHeaderFields: TWiRLHeaderList;
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
    function GetHeaderFields: TWiRLHeaderList; override;
    function GetContentStream: TStream; override;
    procedure SetContentStream(const Value: TStream); override;
  public
    constructor Create(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo);
    destructor Destroy; override;
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
  FServer := TIdHTTPServer.Create(nil);
  FServer.ParseParams := False;
  FServer.OnCommandGet := DoCommandGet;
  FServer.OnCommandOther := DoCommandOther;
  FServer.OnParseAuthentication := ParseAuthorizationHeader;
end;

destructor TWiRLhttpServerIndy.Destroy;
begin
  FServer.Active := False;
  FServer.Free;
  inherited;
end;

procedure TWiRLhttpServerIndy.DoCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  LContext: TWiRLContext;
  LEngine: TWirlCustomEngine;
begin
  inherited;
  LEngine := GetEngine(ARequestInfo.Document);

  LContext := TWiRLContext.Create;
  try
    LContext.Engine := LEngine;
    LContext.Request := TWiRLHttpRequestIndy.Create(AContext, ARequestInfo);
    LContext.Response := TWiRLHttpResponseIndy.Create(AContext, AResponseInfo);
    try

      AResponseInfo.FreeContentStream := True;
      LEngine.HandleRequest(LContext);
      //AResponseInfo.CustomHeaders.AddStrings(LContext.Response.CustomHeaders);
    finally
      LContext.Request.Free;
      LContext.Response.Free;
    end;

  finally
    LContext.Free;
  end;
end;

procedure TWiRLhttpServerIndy.DoCommandOther(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  inherited;
  DoCommandGet(AContext, ARequestInfo, AResponseInfo);
end;

procedure TWiRLhttpServerIndy.ParseAuthorizationHeader(AContext: TIdContext;
    const AAuthType, AAuthData: string; var VUsername, VPassword: string; var
    VHandled: Boolean);
begin
  VHandled := True;
end;

procedure TWiRLhttpServerIndy.SetupThreadPooling(const APoolSize: Integer);
var
  LScheduler: TIdSchedulerOfThreadPool;
begin
  if Assigned(FServer.Scheduler) then
  begin
    FServer.Scheduler.Free;
    FServer.Scheduler := nil;
  end;

  LScheduler := TIdSchedulerOfThreadPool.Create(FServer);
  LScheduler.PoolSize := APoolSize;
  FServer.Scheduler := LScheduler;
  FServer.MaxConnections := LScheduler.PoolSize;
end;

procedure TWiRLhttpServerIndy.Shutdown;
begin
  inherited;
  FServer.Active := False;
  FServer.Bindings.Clear;
end;

procedure TWiRLhttpServerIndy.Startup;
begin
  inherited;
  FServer.Bindings.Clear;
  FServer.DefaultPort := Port;
  FServer.Active := True;
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
  FCookieFields.Free;
  FQueryFields.Free;
  FContentFields.Free;
  FHeaderFields.Free;
  inherited;
end;

procedure TWiRLHttpRequestIndy.ParseParams(Params :TStrings; const AValue: String);
var
  i, j : Integer;
  s: string;
  LEncoding: IIdTextEncoding;
begin
  Params.BeginUpdate;
  try
    Params.Clear;

    if FRequestInfo.CharSet <> '' then
      LEncoding := CharsetToEncoding(FRequestInfo.CharSet)
    else
      LEncoding := IndyTextEncoding_UTF8;
    i := 1;
    while i <= Length(AValue) do
    begin
      j := i;
      while (j <= Length(AValue)) and (AValue[j] <> '&') do {do not localize}
      begin
        Inc(j);
      end;
      s := Copy(AValue, i, j-i);
      s := StringReplace(s, '+', ' ', [rfReplaceAll]);
      Params.Add(TIdURI.URLDecode(s, LEncoding));
      i := j + 1;
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
  i :Integer;
begin
  if not Assigned(FCookieFields) then
  begin
    FCookieFields := TWiRLCookies.Create;
    for i := 0 to FRequestInfo.Cookies.Count - 1 do
    begin
      CookieFields.AddClientCookie(FRequestInfo.Cookies[i].ClientCookie);
    end;
  end;
  Result := FCookieFields;
end;

function TWiRLHttpRequestIndy.GetHeaderFields: TWiRLHeaderList;
begin
  if not Assigned(FHeaderFields) then
  begin
    FHeaderFields := TWiRLHeaderList.Create;
    FHeaderFields.Assign(FRequestInfo.RawHeaders);
  end;
  Result := FHeaderFields;
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
begin
  inherited Create;
  FContext := AContext;
  FResponseInfo := AResponseInfo;
  FCustomHeaders := TStringList.Create;
  FCustomHeaders.Assign(AResponseInfo.CustomHeaders);
end;

destructor TWiRLHttpResponseIndy.Destroy;
begin
  FCustomHeaders.Free;
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

function TWiRLHttpResponseIndy.GetReasonString: string;
begin
  Result := FResponseInfo.ResponseText;
end;

function TWiRLHttpResponseIndy.GetStatusCode: Integer;
begin
  Result := FResponseInfo.ResponseNo;
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
  i :Integer;
begin
  inherited;
  FResponseInfo.Date := GMTToLocalDateTime(HeaderFields['Date']);
  FResponseInfo.CustomHeaders.Clear;

  for i := 0 to HeaderFields.Count - 1 do
  begin
    if IsIndyHeader(HeaderFields.Names[i]) then
      Continue;
    FResponseInfo.CustomHeaders.Add(HeaderFields.Strings[i]);
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

end.
