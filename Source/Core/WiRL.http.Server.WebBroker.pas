unit WiRL.http.Server.WebBroker;

interface

uses
  System.Classes, System.SysUtils, System.NetEncoding, System.DateUtils,
  System.Masks,

  Web.HTTPApp,

  WiRL.Core.Classes,
  WiRL.http.Core,
  WiRL.http.Headers,
  WiRL.http.Accept.MediaType,
  WiRL.http.Cookie,
  WiRL.http.Server,
  WiRL.http.Server.Interfaces,
  WiRL.http.Response,
  WiRL.http.Request,
  WiRL.Core.Context.Server,
  WiRL.Core.Auth.Context,
  WiRL.Core.Exceptions;

type
  // This class implements the IWebDispatch interface to allow
  // WebBroker to dispatch incoming requests to the TWiRLServer instance.
  TWiRLDispatcher = class(TComponent, IWebDispatch)
  private
    FServer: TWiRLServer;
    FDispatchMask: TMask;
  protected
    procedure SetServer(const Value: TWiRLServer); virtual;
  public
    { IWebDispatch }
    function DispatchEnabled: Boolean;
    function DispatchMethodType: TMethodType;
    function DispatchRequest(Sender: TObject; WebRequest: TWebRequest; WebResponse: TWebResponse): Boolean;
    function DispatchMask: TMask;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Server: TWiRLServer read FServer write SetServer;
  end;

  // This is a fake implementation of a TWiRLhttpServer. With WebBroker WiRL does
  // not need to implement its own server.
  // It can rely on the WebBroker framework to handle HTTP requests and responses.
  // Instead it provides an implementation of the IWiRLServerFactory interface to
  // create the request and response objects from the WebBroker framework.
  TWiRLhttpServerWebBroker = class(TInterfacedObject, IWiRLServer, IWiRLServerFactory)
  public
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

    { IWiRLServerFactory }
    function CreateRequest(AContext, ARequest: TObject): TWiRLRequest;
    function CreateResponse(AContext, AResponse: TObject): TWiRLResponse;
  end;

  TWiRLHttpRequestWebBroker = class(TWiRLRequest)
  private
    FWebRequest: TWebRequest;
    FCookieFields: TWiRLCookies;
    FQueryFields: TWiRLParam;
    FHeaders: IWiRLHeaders;
    FContentFields: TWiRLParam;
    FConnection: TWiRLConnection;
    FContentStream: TStream;
    procedure ParseParams(Params: TStrings; const AValue: String);
    //procedure ParseParams(Params :TStrings; const AValue: String);
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
    function GetConnection: TWiRLConnection; override;
  public
    constructor Create(AContext: TObject; AWebRequest: TWebRequest);
    destructor Destroy; override;
  end;

  TWiRLHttpResponseWebBroker = class(TWiRLResponse)
  private
    FWebResponse: TWebResponse;
    FHeaders: IWiRLHeaders;
    FWriterProc: TWriterProc;
    FConnection: TWiRLConnection;
    procedure SendCookies;
  protected
    function GetContentStream: TStream; override;
    procedure SetContentStream(const Value: TStream); override;
    function GetStatusCode: Integer; override;
    procedure SetStatusCode(const Value: Integer); override;
    function GetReasonString: string; override;
    procedure SetReasonString(const Value: string); override;
    function IsUnknownResponseCode: Boolean; override;
    function GetHeaders: IWiRLHeaders; override;
    function GetConnection: TWiRLConnection; override;
  public
    procedure SendHeaders(AImmediate: Boolean); override;

    constructor Create(AContext: TObject; AWebResponse: TWebResponse);
    destructor Destroy; override;
  end;

  // At the moment this is just a stub. WebBroker does not expose
  // any connection object to work with.
  TWiRLConnectionWebBroker = class(TWiRLConnection)
  private
    //FConnection: TXXXConnection;
  public
    procedure Write(AValue: TBytes; const ALength: Integer = -1; const AOffset: Integer = 0); overload; override;
    procedure Write(const AValue: string; AEncoding: TEncoding = nil); overload; override;
    procedure WriteLn(const AValue: string); override;
    procedure WriteLn(); override;
    function Connected: Boolean; override;

    constructor Create;
  end;


implementation

{ TWiRLhttpServerWebBroker }

function TWiRLhttpServerWebBroker.CreateRequest(
  AContext, ARequest: TObject): TWiRLRequest;
begin
  Result := TWiRLHttpRequestWebBroker.Create(AContext, ARequest as TWebRequest);
end;

function TWiRLhttpServerWebBroker.CreateResponse(
  AContext, AResponse: TObject): TWiRLResponse;
begin
  Result := TWiRLHttpResponseWebBroker.Create(AContext, AResponse as TWebResponse);
end;

function TWiRLhttpServerWebBroker.GetListener: IWiRLListener;
begin
end;

function TWiRLhttpServerWebBroker.GetPort: Word;
begin
  Result := 0;
end;

function TWiRLhttpServerWebBroker.GetServerImplementation: TObject;
begin
  Result := nil;
end;

function TWiRLhttpServerWebBroker.GetThreadPoolSize: Integer;
begin
  Result := 1;
end;

procedure TWiRLhttpServerWebBroker.SetListener(AValue: IWiRLListener);
begin
end;

procedure TWiRLhttpServerWebBroker.SetPort(AValue: Word);
begin
end;

procedure TWiRLhttpServerWebBroker.SetThreadPoolSize(AValue: Integer);
begin
end;

procedure TWiRLhttpServerWebBroker.Shutdown;
begin
end;

procedure TWiRLhttpServerWebBroker.Startup;
begin
end;

{ TWiRLHttpResponseWebBroker }

constructor TWiRLHttpResponseWebBroker.Create(AContext: TObject; AWebResponse: TWebResponse);
var
  LName, LValue: string;
  LIndex: Integer;
begin
  inherited Create;
  FWriterProc := nil;
  FWebResponse := AWebResponse;

  FConnection := TWiRLConnectionWebBroker.Create();

  FHeaders := TWiRLHeaders.Create;
  for LIndex := 0 to AWebResponse.CustomHeaders.Count - 1 do
  begin
    LName := AWebResponse.CustomHeaders.Names[LIndex];
    LValue := AWebResponse.CustomHeaders.Values[LName];
    FHeaders.AddHeader(TWiRLHeader.Create(LName, LValue));
  end;
end;

destructor TWiRLHttpResponseWebBroker.Destroy;
begin
  FConnection.Free;
  inherited;
end;

function TWiRLHttpResponseWebBroker.GetConnection: TWiRLConnection;
begin
  Result := FConnection;
end;

function TWiRLHttpResponseWebBroker.GetContentStream: TStream;
begin
  Result := FWebResponse.ContentStream;
end;

function TWiRLHttpResponseWebBroker.GetHeaders: IWiRLHeaders;
begin
  Result := FHeaders;
end;

function TWiRLHttpResponseWebBroker.GetReasonString: string;
begin
  Result := FWebResponse.ReasonString;
end;

function TWiRLHttpResponseWebBroker.GetStatusCode: Integer;
begin
  Result := FWebResponse.StatusCode;
end;

function TWiRLHttpResponseWebBroker.IsUnknownResponseCode: Boolean;
begin
  Result := False;
end;

procedure TWiRLHttpResponseWebBroker.SendCookies;
var
  LWiRLCookie: TWiRLCookie;
  LWebCookie: TCookie;
begin
  for LWiRLCookie in Cookies do
  begin
    LWebCookie := FWebResponse.Cookies.Add;

    LWebCookie.Name := LWiRLCookie.CookieName;
    LWebCookie.Value := LWiRLCookie.Value;
    LWebCookie.Domain := LWiRLCookie.Domain;
    LWebCookie.Expires := LWiRLCookie.Expires;
    LWebCookie.HttpOnly := LWiRLCookie.HttpOnly;
    LWebCookie.Path := LWiRLCookie.Path;
    LWebCookie.Secure := LWiRLCookie.Secure;
    //LWebCookie.CreatedAt := LWiRLCookie.CreatedAt;
    LWebCookie.HttpOnly := LWiRLCookie.HostOnly; // I'm not sure about this...
    //LWebCookie.LastAccessed := LWiRLCookie.LastAccessed;
    //LWebCookie.Persistent := LWiRLCookie.Persistent;
  end;
end;

procedure TWiRLHttpResponseWebBroker.SendHeaders(AImmediate: Boolean);

  function IsStandardHeader(const AHeaderName: string): Boolean;
  const
    StandardHeaders: array of string = ['Content-Type'];
  var
    LHeaderName: string;
  begin
    Result := False;
    for LHeaderName in StandardHeaders do
    begin
      if SameText(AHeaderName, LHeaderName) then
        Exit(True);
    end;
  end;

var
  LHeader: TWiRLHeader;
begin
  inherited;
  for LHeader in Headers do
  begin
    if not IsStandardHeader(LHeader.Name) then
      FWebResponse.CustomHeaders.Values[LHeader.Name] := LHeader.Value;
  end;

  FWebResponse.ContentType := Headers.ContentType;
  FWebResponse.ContentEncoding := Headers.ContentEncoding;
  FWebResponse.ContentLength := Headers.ContentLength;

  SendCookies;
end;

procedure TWiRLHttpResponseWebBroker.SetContentStream(const Value: TStream);
begin
  inherited;
  if Assigned(FWebResponse.ContentStream) then
    FWebResponse.ContentStream.Free;
  FWebResponse.ContentStream := Value;
end;

procedure TWiRLHttpResponseWebBroker.SetReasonString(const Value: string);
begin
  inherited;
  FWebResponse.ReasonString := Value;
end;

procedure TWiRLHttpResponseWebBroker.SetStatusCode(const Value: Integer);
begin
  inherited;
  FWebResponse.StatusCode := Value;
end;

{ TWiRLConnectionWebBroker }

function TWiRLConnectionWebBroker.Connected: Boolean;
begin
  raise EWiRLException.Create('TWiRLConnectionWebBroker.Write not yet implemented');
end;

constructor TWiRLConnectionWebBroker.Create;
begin
  // Not yet implemented
end;

procedure TWiRLConnectionWebBroker.Write(const AValue: string;
  AEncoding: TEncoding);
begin
  raise EWiRLException.Create('TWiRLConnectionWebBroker.Write not yet implemented');
end;

procedure TWiRLConnectionWebBroker.Write(AValue: TBytes; const ALength,
  AOffset: Integer);
begin
  raise EWiRLException.Create('TWiRLConnectionWebBroker.Write not yet implemented');
end;

procedure TWiRLConnectionWebBroker.WriteLn(const AValue: string);
begin
  raise EWiRLException.Create('TWiRLConnectionWebBroker.WriteLn not yet implemented');
end;

procedure TWiRLConnectionWebBroker.WriteLn;
begin
  raise EWiRLException.Create('TWiRLConnectionWebBroker.WriteLn not yet implemented');
end;

{ TWiRLHttpRequestWebBroker }

constructor TWiRLHttpRequestWebBroker.Create(AContext: TObject;
  AWebRequest: TWebRequest);
begin
  FWebRequest := AWebRequest;
  FMethod := FWebRequest.Method;
  FConnection := TWiRLConnectionWebBroker.Create();
end;

destructor TWiRLHttpRequestWebBroker.Destroy;
begin
//  FWebRequest.PostStream.Free;
//  FWebRequest.PostStream := nil;
  FCookieFields.Free;
  FQueryFields.Free;
  FContentFields.Free;
  FConnection.Free;
  FContentStream.Free;
  inherited;
end;

procedure TWiRLHttpRequestWebBroker.ParseParams(Params :TStrings; const AValue: String);
var
  LIndex, LStrIndex: Integer;
  LTempStr: string;
begin
  Params.BeginUpdate;
  try
    Params.Clear;

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
      Params.Add(TNetEncoding.URL.Decode(LTempStr));
      LIndex := LStrIndex + 1;
    end;
  finally
    Params.EndUpdate;
  end;
end;

function TWiRLHttpRequestWebBroker.GetConnection: TWiRLConnection;
begin
  Result := FConnection;
end;

function TWiRLHttpRequestWebBroker.GetContentFields: TWiRLParam;
begin
  if not Assigned(FContentFields) then
  begin
    FContentFields := TWiRLParam.Create;
    if FWebRequest.ContentType = TMediaType.APPLICATION_FORM_URLENCODED_TYPE then
      ParseParams(FContentFields, FWebRequest.Content);
  end;
  Result := FContentFields;
end;

function TWiRLHttpRequestWebBroker.GetContentStream: TStream;
begin
  if not Assigned(FContentStream) then
  begin
    FContentStream := TBytesStream.Create(FWebRequest.RawContent);
  end;
  Result := FContentStream;
end;

function TWiRLHttpRequestWebBroker.GetCookieFields: TWiRLCookies;
var
  LIndex: Integer;
begin
  if not Assigned(FCookieFields) then
  begin
    FCookieFields := TWiRLCookies.Create;
    for LIndex := 0 to FWebRequest.CookieFields.Count - 1 do
    begin
      CookieFields.AddClientCookie(FWebRequest.CookieFields[LIndex]);
    end;
  end;
  Result := FCookieFields;
end;

function TWiRLHttpRequestWebBroker.GetHeaders: IWiRLHeaders;
begin
  if not Assigned(FHeaders) then
  begin
    FHeaders := TWiRLHeaders.Create;
    if FWebRequest.Authorization <> '' then
      FHeaders.AddHeader(TWiRLHeader.Create('Authorization', FWebRequest.Authorization));
    if FWebRequest.CacheControl <> '' then
      FHeaders.AddHeader(TWiRLHeader.Create('Cache-Control', FWebRequest.CacheControl));
    if FWebRequest.Cookie <> '' then
      FHeaders.AddHeader(TWiRLHeader.Create('Cookie', FWebRequest.Cookie));
    if FWebRequest.Date > 0 then
      FHeaders.AddHeader(TWiRLHeader.Create('Date', DateTimeToHTTPDate(FWebRequest.Date)));
    if FWebRequest.Accept <> '' then
      FHeaders.AddHeader(TWiRLHeader.Create('Accept', FWebRequest.Accept));
    if FWebRequest.From <> '' then
      FHeaders.AddHeader(TWiRLHeader.Create('From', FWebRequest.From));
    if FWebRequest.Host <> '' then
      FHeaders.AddHeader(TWiRLHeader.Create('Host', FWebRequest.Host));
    if FWebRequest.IfModifiedSince > 0 then
      FHeaders.AddHeader(TWiRLHeader.Create('If-Modified-Since', DateTimeToHTTPDate(FWebRequest.IfModifiedSince)));
    if FWebRequest.Referer <> '' then
      FHeaders.AddHeader(TWiRLHeader.Create('Referer', FWebRequest.Referer));
    if FWebRequest.UserAgent <> '' then
      FHeaders.AddHeader(TWiRLHeader.Create('User-Agent', FWebRequest.UserAgent));
    if FWebRequest.ContentEncoding <> '' then
      FHeaders.AddHeader(TWiRLHeader.Create('Content-Encoding', FWebRequest.ContentEncoding));
    if FWebRequest.ContentType <> '' then
      FHeaders.AddHeader(TWiRLHeader.Create('Content-Type', FWebRequest.ContentType));
    if FWebRequest.ContentLength <> 0 then
      FHeaders.AddHeader(TWiRLHeader.Create('Content-Length', FWebRequest.ContentLength.ToString));
    if FWebRequest.ContentVersion <> '' then
      FHeaders.AddHeader(TWiRLHeader.Create('Content-Version', FWebRequest.ContentVersion));
    if FWebRequest.DerivedFrom <> '' then
      FHeaders.AddHeader(TWiRLHeader.Create('Derived-From', FWebRequest.DerivedFrom));
    if FWebRequest.Expires > 0 then
      FHeaders.AddHeader(TWiRLHeader.Create('Expires', DateTimeToHTTPDate(FWebRequest.Expires)));
    if FWebRequest.Title <> '' then
      FHeaders.AddHeader(TWiRLHeader.Create('Title', FWebRequest.Title));
  end;
  Result := FHeaders;
end;

function TWiRLHttpRequestWebBroker.GetHttpPathInfo: string;
begin
  Result := FWebRequest.PathInfo;
end;

function TWiRLHttpRequestWebBroker.GetHttpQuery: string;
begin
  Result := FWebRequest.Query;
end;

function TWiRLHttpRequestWebBroker.GetQueryFields: TWiRLParam;
begin
  if not Assigned(FQueryFields) then
  begin
    FQueryFields := TWiRLParam.Create;
    ParseParams(FQueryFields, FWebRequest.Query);
  end;
  Result := FQueryFields;
end;

function TWiRLHttpRequestWebBroker.GetRemoteIP: string;
begin
  Result := FWebRequest.RemoteIP;
end;

function TWiRLHttpRequestWebBroker.GetServerPort: Integer;
begin
  Result := FWebRequest.ServerPort;
end;

procedure TWiRLHttpRequestWebBroker.SetContentStream(const Value: TStream);
begin
  inherited;
  if Assigned(FContentStream) then
    FContentStream.Free;
  FContentStream := Value;
end;

{ TWiRLDispatcher }

constructor TWiRLDispatcher.Create(AOwner: TComponent);
begin
  inherited;
  FDispatchMask := nil;
end;

destructor TWiRLDispatcher.Destroy;
begin
  FDispatchMask.Free;
  FServer := nil;
  inherited;
end;

function TWiRLDispatcher.DispatchEnabled: Boolean;
begin
  Result := False;
  if Assigned(FServer) then
    Result := FServer.Active;
end;

function TWiRLDispatcher.DispatchMask: TMask;
begin
  if not Assigned(FDispatchMask) then
  begin
    FDispatchMask := TMask.Create('*');
  end;
  Result := FDispatchMask;
end;

function TWiRLDispatcher.DispatchMethodType: TMethodType;
begin
  Result := mtAny;
end;

function TWiRLDispatcher.DispatchRequest(Sender: TObject; WebRequest: TWebRequest;
  WebResponse: TWebResponse): Boolean;
var
  LServerFactory: IWiRLServerFactory;
  LRequest: TWiRLRequest;
  LResponse: TWiRLResponse;
  LContext: TWiRLContext;
begin
  if not Assigned(FServer) then
    Exit(False);

  // if there's not a registered engine to handle the request skip the process
  if not Assigned(FServer.FindEngine(WebRequest.PathInfo)) then
    Exit(False);

  if not Supports(FServer.HttpServer, IWiRLServerFactory, LServerFactory) then
    raise EWiRLServerException.Create('Http Server doesn''t implements "IWiRLServerHandler"');

  LContext := TWiRLContext.Create;
  try
    LContext.AddContainer(WebRequest);
    LContext.AddContainer(WebResponse);
    LRequest := LServerFactory.CreateRequest(Sender, WebRequest);
    try
      LResponse := LServerFactory.CreateResponse(Sender, WebResponse);
      try
        if LResponse.Server = '' then
          LResponse.Server := 'WiRL Server (WebBroker)';
        FServer.HandleRequest(LContext, LRequest, LResponse);
      finally
        LResponse.Free;
      end;
    finally
      LRequest.Free;
    end;
  finally
    LContext.Free;
  end;

  Result := True;
end;

procedure TWiRLDispatcher.SetServer(const Value: TWiRLServer);
begin
  FServer := Value;
end;

initialization
  TWiRLServerRegistry.Instance.RegisterServer<TWiRLhttpServerWebBroker>('TIdHttpServer (WebBroker)');

end.
