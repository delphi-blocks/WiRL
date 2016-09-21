(*
  Copyright 2015-2016, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit MARS.http.Server.Indy;

interface

uses
  System.Classes, System.SysUtils, System.SyncObjs,
  IdContext, IdCustomHTTPServer, IdException, IdTCPServer, IdIOHandlerSocket,
  IdSchedulerOfThreadPool, idGlobal, IdGlobalProtocols, IdURI,
  //MARS.http.Server.Authentication,
  MARS.Core.Engine,
  MARS.Core.Response,
  MARS.Core.Request,
  MARS.Core.Token;

type
  TMARSEngines = TArray<TMARSEngine>;

  TMARShttpServerIndy = class(TIdCustomHTTPServer)
  private
    FEngine: TMARSEngine;
  protected
    procedure ParseAuthorizationHeader(AContext: TIdContext; const AAuthType,
        AAuthData: string; var VUsername, VPassword: string; var VHandled: Boolean);

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

    function ConfigureEngine(const ABasePath: string): TMARSEngine;
    property Engine: TMARSEngine read FEngine;
  end;

  TMARSHttpResponseIndy = class(TMARSResponse)
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
  public
    constructor Create(AContext: TIdContext; AResponseInfo: TIdHTTPResponseInfo);
    destructor Destroy; override;
  end;

  TMARSHttpRequestIndy = class(TMARSRequest)
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
    function GetContent: string; override;
    function GetAuthorization: string; override;
    function GetAccept: string; override;
    function GetContentType: string; override;
    function GetContentLength: Integer; override;
    function GetContentVersion: string; override;
    function GetRawPathInfo: string; override;
    function DoGetFieldByName(const Name: string): string; override;
  public
    constructor Create(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo);
    destructor Destroy; override;
  end;

var
  GetDefaultCharSetEncoding: TEncoding = nil;

implementation

uses
  System.StrUtils,
  MARS.Core.Utils;

function DefaultCharSetEncoding: TEncoding;
begin
  Result := nil;
  if Assigned(GetDefaultCharSetEncoding) then
    Result := GetDefaultCharSetEncoding;
  if Result = nil then
    Result := TEncoding.UTF8;
end;

function EncodingFromContentType(const AContentType: string): TEncoding;
var
  S: string;
begin
  Result := nil;
  S := UpperCase(string(AContentType));
  if (Pos('CHARSET', S) > 0) then // Do not localize
    if (Pos('UTF-8', S) > 0) then // Do not localize
      Result := TEncoding.UTF8
    else if (Pos('ISO-8859-1', S) > 0) then // Do not localize
      Result := TEncoding.ANSI
    else if (Pos('ANSI', S) > 0) then // Do not localize
      Result := TEncoding.ANSI
    else if (Pos('ASCII', S) > 0) then // Do not localize
      Result := TEncoding.ASCII;

  if Result = nil then
    Result := DefaultCharSetEncoding;

end;

function EncodingGetString(const AContentType: string; const AValue: TBytes): string;
var
  Encoding: TEncoding;
begin
  Encoding := EncodingFromContentType(AContentType);
  Result := Encoding.GetString(AValue);
end;

function TMARShttpServerIndy.ConfigureEngine(const ABasePath: string): TMARSEngine;
begin
  FEngine.SetBasePath(ABasePath);
  Result := FEngine;
end;

constructor TMARShttpServerIndy.Create;
begin
  inherited Create(nil);
  FEngine := TMARSEngine.Create;
  ParseParams := False;
  OnParseAuthentication := ParseAuthorizationHeader;
end;

destructor TMARShttpServerIndy.Destroy;
begin
  FEngine.Free;
  inherited;
end;

procedure TMARShttpServerIndy.DoCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  LRequest: TMARSHttpRequestIndy;
  LResponse: TMARSHttpResponseIndy;
begin
  inherited;

  LRequest := TMARSHttpRequestIndy.Create(AContext, ARequestInfo);
  try
    LResponse := TMARSHttpResponseIndy.Create(AContext, AResponseInfo);
    try
      AResponseInfo.FreeContentStream := True;
      // skip browser requests (can be dangerous since it is a bit wide as approach)
      if not EndsText('favicon.ico', string(LRequest.PathInfo)) then
      begin
        FEngine.HandleRequest(LRequest, LResponse);
      end;
      AResponseInfo.CustomHeaders.AddStrings(LResponse.CustomHeaders);
    finally
      FreeAndNil(LResponse);
    end;
  finally
    FreeAndNil(LRequest);
  end;
end;

procedure TMARShttpServerIndy.DoCommandOther(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  inherited;
  DoCommandGet(AContext, ARequestInfo, AResponseInfo);
end;

procedure TMARShttpServerIndy.InitComponent;
begin
  inherited;
end;

procedure TMARShttpServerIndy.ParseAuthorizationHeader(AContext: TIdContext;
    const AAuthType, AAuthData: string; var VUsername, VPassword: string; var
    VHandled: Boolean);
begin
  VHandled := True;
end;

procedure TMARShttpServerIndy.SetupThreadPooling(const APoolSize: Integer);
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

procedure TMARShttpServerIndy.Shutdown;
begin
  inherited;
  Bindings.Clear;
end;

procedure TMARShttpServerIndy.Startup;
begin
  Bindings.Clear;
  DefaultPort := FEngine.Port;

  inherited;
end;

{ TMARSHttpRequestIndy }

constructor TMARSHttpRequestIndy.Create(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo);
begin
  inherited Create;
  FContext := AContext;
  FRequestInfo := ARequestInfo;
end;

destructor TMARSHttpRequestIndy.Destroy;
begin
  FCookieFields.Free;
  FQueryFields.Free;
  FContentFields.Free;
  inherited;
end;

procedure TMARSHttpRequestIndy.ParseParams(Params :TStrings; const AValue: String);
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
      s := ReplaceAll(s, '+', ' ');
      Params.Add(TIdURI.URLDecode(s, LEncoding));
      i := j + 1;
    end;
  finally
    Params.EndUpdate;
  end;
end;

function TMARSHttpRequestIndy.DoGetFieldByName(const Name: string): string;
begin
  Result := FRequestInfo.RawHeaders.Values[Name];
end;

function TMARSHttpRequestIndy.GetAccept: string;
begin
  Result := FRequestInfo.Accept;
end;

function TMARSHttpRequestIndy.GetAuthorization: string;
begin
  Result := FRequestInfo.RawHeaders.Values['Authorization'];
end;

function TMARSHttpRequestIndy.GetContent: string;
var
  LPos :Int64;
  RawContents :TBytes;
begin
  Result := '';

  if FRequestInfo.PostStream.Size > 0 then
  begin
    LPos := FRequestInfo.PostStream.Position;
    try
      SetLength(RawContents, FRequestInfo.PostStream.Size);
      FRequestInfo.PostStream.ReadBuffer(RawContents[0], FRequestInfo.PostStream.Size);
    finally
      FRequestInfo.PostStream.Position := LPos;
    end;
    Result := EncodingGetString(ContentType, RawContents);
  end;
end;

function TMARSHttpRequestIndy.GetContentFields: TStrings;
begin
  if not Assigned(FContentFields) then
  begin
    FContentFields := TStringList.Create;
    ParseParams(FContentFields, FRequestInfo.FormParams);
  end;
  Result := FContentFields;
end;

function TMARSHttpRequestIndy.GetContentLength: Integer;
begin
  Result := FRequestInfo.ContentLength;
end;

function TMARSHttpRequestIndy.GetContentType: string;
begin
  Result := FRequestInfo.ContentType;
end;

function TMARSHttpRequestIndy.GetContentVersion: string;
begin
  Result := FRequestInfo.ContentVersion;
end;

function TMARSHttpRequestIndy.GetCookieFields: TStrings;
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

function TMARSHttpRequestIndy.GetHost: string;
begin
  Result := FRequestInfo.Host;
end;

function TMARSHttpRequestIndy.GetMethod: string;
begin
  Result := FRequestInfo.Command;
end;

function TMARSHttpRequestIndy.GetPathInfo: string;
begin
  Result := FRequestInfo.Document;
end;

function TMARSHttpRequestIndy.GetQuery: string;
begin
  Result := FRequestInfo.QueryParams;
end;

function TMARSHttpRequestIndy.GetQueryFields: TStrings;
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

function TMARSHttpRequestIndy.GetRawPathInfo: string;
begin
  Result := FRequestInfo.URI;
end;

function TMARSHttpRequestIndy.GetServerPort: Integer;
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

{ TMARSHttpResponseIndy }

constructor TMARSHttpResponseIndy.Create(AContext: TIdContext;
  AResponseInfo: TIdHTTPResponseInfo);
begin
  inherited Create;
  FContext := AContext;
  FResponseInfo := AResponseInfo;
  FCustomHeaders := TStringList.Create;
  FCustomHeaders.Assign(AResponseInfo.CustomHeaders);
end;

destructor TMARSHttpResponseIndy.Destroy;
begin
  FCustomHeaders.Free;
  inherited;
end;

function TMARSHttpResponseIndy.GetContent: string;
begin
  Result := FResponseInfo.ContentText;
end;

function TMARSHttpResponseIndy.GetContentLength: Int64;
begin
  Result := FResponseInfo.ContentLength;
end;

function TMARSHttpResponseIndy.GetContentStream: TStream;
begin
  Result := FResponseInfo.ContentStream;
end;

function TMARSHttpResponseIndy.GetContentType: string;
begin
  Result := FResponseInfo.ContentType;
end;

function TMARSHttpResponseIndy.GetCustomHeaders: TStrings;
begin
  Result := FCustomHeaders;
end;

function TMARSHttpResponseIndy.GetDate: TDateTime;
begin
  Result := FResponseInfo.Date;
end;

function TMARSHttpResponseIndy.GetExpires: TDateTime;
begin
  Result := FResponseInfo.Expires;
end;

function TMARSHttpResponseIndy.GetLastModified: TDateTime;
begin
  Result := FResponseInfo.LastModified;
end;

function TMARSHttpResponseIndy.GetReasonString: string;
begin
  Result := FResponseInfo.ResponseText;
end;

function TMARSHttpResponseIndy.GetStatusCode: Integer;
begin
  Result := FResponseInfo.ResponseNo;
end;

procedure TMARSHttpResponseIndy.SetContent(const Value: string);
begin
  inherited;
  FResponseInfo.ContentText := Value;
end;

procedure TMARSHttpResponseIndy.SetContentLength(const Value: Int64);
begin
  inherited;
  FResponseInfo.ContentLength := Value;
end;

procedure TMARSHttpResponseIndy.SetContentStream(const Value: TStream);
begin
  inherited;
  FResponseInfo.ContentStream := Value;
end;

procedure TMARSHttpResponseIndy.SetContentType(const Value: string);
begin
  inherited;
  FResponseInfo.ContentType := Value;
end;

procedure TMARSHttpResponseIndy.SetCustomHeaders(const Value: TStrings);
begin
  inherited;
  FCustomHeaders.Assign(Value);
end;

procedure TMARSHttpResponseIndy.SetDate(const Value: TDateTime);
begin
  inherited;
  FResponseInfo.Date := Value;
end;

procedure TMARSHttpResponseIndy.SetExpires(const Value: TDateTime);
begin
  inherited;
  FResponseInfo.Expires := Value;
end;

procedure TMARSHttpResponseIndy.SetLastModified(const Value: TDateTime);
begin
  inherited;
  FResponseInfo.LastModified := Value;
end;

procedure TMARSHttpResponseIndy.SetReasonString(const Value: string);
begin
  inherited;
  FResponseInfo.ResponseText := Value;
end;

procedure TMARSHttpResponseIndy.SetStatusCode(const Value: Integer);
begin
  inherited;
  FResponseInfo.ResponseNo := Value;
end;

end.
