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
  //WiRL.http.Server.Authentication,
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
    function GetCustomHeaders: TStrings; override;
    function GetDate: TDateTime; override;
    function GetExpires: TDateTime; override;
    function GetLastModified: TDateTime; override;
    procedure SetContent(const Value: string); override;
    procedure SetContentStream(const Value: TStream); override;
    procedure SetCustomHeaders(const Value: TStrings); override;
    procedure SetDate(const Value: TDateTime); override;
    procedure SetExpires(const Value: TDateTime); override;
    procedure SetLastModified(const Value: TDateTime); override;
    function GetStatusCode: Integer; override;
    procedure SetStatusCode(const Value: Integer); override;
    function GetContentType: string; override;
    procedure SetContentType(const Value: string); override;
    function GetReasonString: string; override;
    procedure SetReasonString(const Value: string); override;
    function GetContentLength: Int64; override;
    procedure SetContentLength(const Value: Int64); override;
    function GetContentCharSet: string; override;
    procedure SetContentCharSet(const Value: string); override;
  public
    constructor Create(AContext: TIdContext; AResponseInfo: TIdHTTPResponseInfo);
    destructor Destroy; override;
  end;

  TWiRLHttpRequestIndy = class(TWiRLRequest)
  private
    FContext: TIdContext;
    FRequestInfo: TIdHTTPRequestInfo;
    FCookieFields: TStrings;
    FQueryFields: TStrings;
    FContentFields: TStrings;
    procedure ParseParams(Params :TStrings; const AValue: String);
  protected
    function GetPathInfo: string; override;
    function GetQuery: string; override;
    function GetHost: string; override;
    function GetServerPort: Integer; override;
    function GetMethod: string; override;
    function GetQueryFields: TStrings; override;
    function GetContentFields: TStrings; override;
    function GetCookieFields: TStrings; override;
    function GetContentStream: TStream; override;
    function GetAuthorization: string; override;
    function GetAccept: string; override;
    function GetAcceptCharSet: string; override;
    function GetAcceptEncoding: string; override;
    function GetAcceptLanguage: string; override;
    function GetContentType: string; override;
    function GetContentLength: Integer; override;
    function GetContentVersion: string; override;
    function GetRawPathInfo: string; override;
    function DoGetFieldByName(const Name: string): string; override;
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

    AResponseInfo.FreeContentStream := True;
    if not EndsText('/favicon.ico', LContext.Request.PathInfo) then
    begin
      FEngine.HandleRequest(LContext);
    end;
    AResponseInfo.CustomHeaders.AddStrings(LContext.Response.CustomHeaders);
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
  ARequestInfo.PostStream := nil;
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
end;

destructor TWiRLHttpRequestIndy.Destroy;
begin
  FRequestInfo.PostStream := nil;
  FCookieFields.Free;
  FQueryFields.Free;
  FContentFields.Free;
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

function TWiRLHttpRequestIndy.DoGetFieldByName(const Name: string): string;
begin
  Result := FRequestInfo.RawHeaders.Values[Name];
end;

function TWiRLHttpRequestIndy.GetAccept: string;
begin
  Result := FRequestInfo.Accept;
end;

function TWiRLHttpRequestIndy.GetAcceptCharSet: string;
begin
  Result := FRequestInfo.AcceptCharSet;
end;

function TWiRLHttpRequestIndy.GetAcceptEncoding: string;
begin
  Result := FRequestInfo.AcceptEncoding;
end;

function TWiRLHttpRequestIndy.GetAcceptLanguage: string;
begin
  Result := FRequestInfo.AcceptLanguage;
end;

function TWiRLHttpRequestIndy.GetAuthorization: string;
begin
  Result := FRequestInfo.RawHeaders.Values['Authorization'];
end;

function TWiRLHttpRequestIndy.GetContentStream: TStream;
begin
  Result := FRequestInfo.PostStream;
end;

function TWiRLHttpRequestIndy.GetContentFields: TStrings;
begin
  if not Assigned(FContentFields) then
  begin
    FContentFields := TStringList.Create;
    ParseParams(FContentFields, FRequestInfo.FormParams);
  end;
  Result := FContentFields;
end;

function TWiRLHttpRequestIndy.GetContentLength: Integer;
begin
  Result := FRequestInfo.ContentLength;
end;

function TWiRLHttpRequestIndy.GetContentType: string;
begin
  Result := FRequestInfo.ContentType;
end;

function TWiRLHttpRequestIndy.GetContentVersion: string;
begin
  Result := FRequestInfo.ContentVersion;
end;

function TWiRLHttpRequestIndy.GetCookieFields: TStrings;
var
  i :Integer;
begin
  if not Assigned(FCookieFields) then
  begin
    FCookieFields := TStringList.Create;
    for i := 0 to FRequestInfo.Cookies.Count - 1 do
    begin
      CookieFields.Add(FRequestInfo.Cookies[i].ClientCookie);
    end;
  end;
  Result := FCookieFields;
end;

function TWiRLHttpRequestIndy.GetHost: string;
begin
  Result := FRequestInfo.Host;
end;

function TWiRLHttpRequestIndy.GetMethod: string;
begin
  Result := FRequestInfo.Command;
end;

function TWiRLHttpRequestIndy.GetPathInfo: string;
begin
  Result := FRequestInfo.Document;
end;

function TWiRLHttpRequestIndy.GetQuery: string;
begin
  Result := FRequestInfo.QueryParams;
end;

function TWiRLHttpRequestIndy.GetQueryFields: TStrings;
//const
//  ContentTypeFormUrlencoded = 'application/x-www-form-urlencoded';
begin
  if not Assigned(FQueryFields) then
  begin
    FQueryFields := TStringList.Create;
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

function TWiRLHttpResponseIndy.GetContentCharSet: string;
begin
  Result := FResponseInfo.CharSet;
end;

function TWiRLHttpResponseIndy.GetContentLength: Int64;
begin
  Result := FResponseInfo.ContentLength;
end;

function TWiRLHttpResponseIndy.GetContentStream: TStream;
begin
  Result := FResponseInfo.ContentStream;
end;

function TWiRLHttpResponseIndy.GetContentType: string;
begin
  Result := FResponseInfo.ContentType;
end;

function TWiRLHttpResponseIndy.GetCustomHeaders: TStrings;
begin
  Result := FCustomHeaders;
end;

function TWiRLHttpResponseIndy.GetDate: TDateTime;
begin
  Result := FResponseInfo.Date;
end;

function TWiRLHttpResponseIndy.GetExpires: TDateTime;
begin
  Result := FResponseInfo.Expires;
end;

function TWiRLHttpResponseIndy.GetLastModified: TDateTime;
begin
  Result := FResponseInfo.LastModified;
end;

function TWiRLHttpResponseIndy.GetReasonString: string;
begin
  Result := FResponseInfo.ResponseText;
end;

function TWiRLHttpResponseIndy.GetStatusCode: Integer;
begin
  Result := FResponseInfo.ResponseNo;
end;

procedure TWiRLHttpResponseIndy.SetContent(const Value: string);
begin
  inherited;
  FResponseInfo.ContentText := Value;
end;

procedure TWiRLHttpResponseIndy.SetContentCharSet(const Value: string);
begin
  inherited;
  FResponseInfo.CharSet := Value;
end;

procedure TWiRLHttpResponseIndy.SetContentLength(const Value: Int64);
begin
  inherited;
  FResponseInfo.ContentLength := Value;
end;

procedure TWiRLHttpResponseIndy.SetContentStream(const Value: TStream);
begin
  inherited;
  FResponseInfo.ContentStream := Value;
end;

procedure TWiRLHttpResponseIndy.SetContentType(const Value: string);
var
 LMType: TMediaType;
begin
  inherited;

  LMType := TMediaType.Create(Value);
  try
    FResponseInfo.ContentType := LMType.AcceptItemOnly;
    FResponseInfo.CharSet := LMType.Charset;
  finally
    LMType.Free;
  end;
end;

procedure TWiRLHttpResponseIndy.SetCustomHeaders(const Value: TStrings);
begin
  inherited;
  FCustomHeaders.Assign(Value);
end;

procedure TWiRLHttpResponseIndy.SetDate(const Value: TDateTime);
begin
  inherited;
  FResponseInfo.Date := Value;
end;

procedure TWiRLHttpResponseIndy.SetExpires(const Value: TDateTime);
begin
  inherited;
  FResponseInfo.Expires := Value;
end;

procedure TWiRLHttpResponseIndy.SetLastModified(const Value: TDateTime);
begin
  inherited;
  FResponseInfo.LastModified := Value;
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
