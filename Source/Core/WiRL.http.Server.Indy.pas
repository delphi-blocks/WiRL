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
  IdContext, IdCustomHTTPServer, IdException, IdTCPServer, IdIOHandlerSocket,
  IdSchedulerOfThreadPool, idGlobal, IdGlobalProtocols, IdURI,
  WiRL.http.Core,
  WiRL.Core.Engine,
  WiRL.Core.Response,
  WiRL.Core.Request,
  WiRL.Core.Auth.Context;

type
  TWiRLEngines = TArray<TWiRLEngine>;

  TWiRLhttpServerIndy = class(TIdCustomHTTPServer)
  private
    FEngine: TWiRLEngine;
  protected
    procedure ParseAuthorizationHeader(AContext: TIdContext; const AAuthType,
        AAuthData: string; var VUsername, VPassword: string; var VHandled: Boolean);
    procedure DoneWithPostStream(ASender: TIdContext; ARequestInfo: TIdHTTPRequestInfo); override;

    procedure Startup; override;
    procedure Shutdown; override;
    procedure DoCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo); override;
    procedure DoCommandOther(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo); override;

    procedure InitComponent; override;
    procedure SetupThreadPooling(const APoolSize: Integer = 25);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function ConfigureEngine(const ABasePath: string): TWiRLEngine;
    property Engine: TWiRLEngine read FEngine;
  end;

  TWiRLHttpResponseIndy = class(TWiRLResponse)
  private
    FContext: TIdContext;
    FResponseInfo: TIdHTTPResponseInfo;
    FCustomHeaders :TStrings;
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
    FCookieFields: TWiRLCookie;
    FQueryFields: TWiRLParam;
    FHeaderFields: TWiRLHeaderList;
    FContentFields: TWiRLParam;
    procedure ParseParams(Params :TStrings; const AValue: String);
  protected
    function GetPathInfo: string; override;
    function GetQuery: string; override;
    function GetServerPort: Integer; override;
    function GetQueryFields: TWiRLParam; override;
    function GetContentFields: TWiRLParam; override;
    function GetCookieFields: TWiRLCookie; override;
    function GetHeaderFields: TWiRLHeaderList; override;
    function GetContentStream: TStream; override;
    procedure SetContentStream(const Value: TStream); override;
    function GetRawPathInfo: string; override;
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

function TWiRLhttpServerIndy.ConfigureEngine(const ABasePath: string): TWiRLEngine;
begin
  FEngine.SetBasePath(ABasePath);
  Result := FEngine;
end;

constructor TWiRLhttpServerIndy.Create;
begin
  inherited Create(nil);
  FEngine := TWiRLEngine.Create;
  ParseParams := False;
  OnParseAuthentication := ParseAuthorizationHeader;
end;

destructor TWiRLhttpServerIndy.Destroy;
begin
  FEngine.Free;
  inherited;
end;

procedure TWiRLhttpServerIndy.DoCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  LContext: TWiRLContext;
begin
  inherited;

  LContext := TWiRLContext.Create;
  try
    LContext.Engine := FEngine;
    LContext.Request := TWiRLHttpRequestIndy.Create(AContext, ARequestInfo);
    LContext.Response := TWiRLHttpResponseIndy.Create(AContext, AResponseInfo);
    try

      AResponseInfo.FreeContentStream := True;
      if not EndsText('/favicon.ico', LContext.Request.PathInfo) then
      begin
        FEngine.HandleRequest(LContext);
      end;
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

procedure TWiRLhttpServerIndy.DoneWithPostStream(ASender: TIdContext; ARequestInfo: TIdHTTPRequestInfo);
begin
//  ARequestInfo.PostStream := nil;
end;

procedure TWiRLhttpServerIndy.InitComponent;
begin
  inherited;
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
  if Assigned(Scheduler) then
  begin
    Scheduler.Free;
    Scheduler := nil;
  end;

  LScheduler := TIdSchedulerOfThreadPool.Create(Self);
  LScheduler.PoolSize := APoolSize;
  Scheduler := LScheduler;
  MaxConnections := LScheduler.PoolSize;
end;

procedure TWiRLhttpServerIndy.Shutdown;
begin
  inherited;
  Bindings.Clear;
end;

procedure TWiRLhttpServerIndy.Startup;
begin
  Bindings.Clear;
  DefaultPort := FEngine.Port;

  inherited;
end;

{ TWiRLHttpRequestIndy }

constructor TWiRLHttpRequestIndy.Create(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo);
begin
  inherited Create;
  FContext := AContext;
  FRequestInfo := ARequestInfo;
  FMethod := FRequestInfo.Command;
end;

destructor TWiRLHttpRequestIndy.Destroy;
begin
  FRequestInfo.PostStream := nil;
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

function TWiRLHttpRequestIndy.GetCookieFields: TWiRLCookie;
var
  i :Integer;
begin
  if not Assigned(FCookieFields) then
  begin
    FCookieFields := TWiRLCookie.Create;
    for i := 0 to FRequestInfo.Cookies.Count - 1 do
    begin
      CookieFields.Add(FRequestInfo.Cookies[i].ClientCookie);
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

function TWiRLHttpRequestIndy.GetPathInfo: string;
begin
  Result := FRequestInfo.Document;
end;

function TWiRLHttpRequestIndy.GetQuery: string;
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

function TWiRLHttpRequestIndy.GetRawPathInfo: string;
begin
  Result := FRequestInfo.URI;
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
