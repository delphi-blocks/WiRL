{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.Client.Interfaces;

interface

uses
  System.Classes, System.SysUtils, System.JSON, System.Generics.Collections,

  WiRL.Rtti.Utils,
  WiRL.Core.Classes,
  WiRL.Core.Exceptions,
  WiRL.Core.Singleton,
  WiRL.http.Accept.MediaType;

type
  EWiRLClientException = class(EWiRLException);

  EWiRLSocketException = class(EWiRLClientException);

  TWiRLHeader = record
  public
    const ACCEPT = 'Accept';
    const ACCEPT_LANGUAGE = 'Accept-Language';
    const ACCEPT_CHARSET = 'Accept-Charset';
    const ACCEPT_ENCODING = 'Accept-Encoding';
    const CONTENT_TYPE = 'Content-Type';
    const AUTHORIZATION = 'Authorization';
    const USER_AGENT = 'User-Agent';
    const CONTENT_LENGTH = 'Content-Length';
    const HOST = 'Host';
  public
    Name: string;
    Value: string;
    constructor Create(const AName, AValue: string);
  end;

  TWiRLHeaders = TArray<TWiRLHeader>;

  TWiRLHeadersHelper = record helper for TWiRLHeaders
  private
    function GetValue(const AName: string): string;
    procedure SetValue(const AName, AValue: string);
    function GetAccept: string;
    function GetAcceptCharSet: string;
    function GetAcceptEncoding: string;
    function GetAcceptLanguage: string;
    function GetUserAgent: string;
    function GetContentType: string;
    function GetAuthorization: string;
    procedure SetAccept(const AValue: string);
    procedure SetAcceptCharSet(const AValue: string);
    procedure SetAcceptEncoding(const AValue: string);
    procedure SetAcceptLanguage(const AValue: string);
    procedure SetAuthorization(const AValue: string);
    procedure SetContentType(const AValue: string);
    procedure SetUserAgent(const AValue: string);
  public
    property Values[const AName: string]: string read GetValue write SetValue;
    property Accept: string read GetAccept write SetAccept;
    property AcceptCharSet: string read GetAcceptCharSet write SetAcceptCharSet;
    property AcceptEncoding: string read GetAcceptEncoding write SetAcceptEncoding;
    property AcceptLanguage: string read GetAcceptLanguage write SetAcceptLanguage;
    property UserAgent: string read GetUserAgent write SetUserAgent;
    property ContentType: string read GetContentType write SetContentType;
    property Authorization: string read GetAuthorization write SetAuthorization;
  end;

  IWiRLRequest = interface
    ['{818B8DD9-C5DB-404B-B886-0959DD8D753E}']
    /// <summary>Getter for the Accept Property</summary>
    function GetAccept: string;
    /// <summary>Getter for the cceptCharSet Property</summary>
    function GetAcceptCharSet: string;
    /// <summary>Getter for the AcceptEncoding Property</summary>
    function GetAcceptEncoding: string;
    /// <summary>Getter for the AcceptLanguage Property</summary>
    function GetAcceptLanguage: string;
    /// <summary>Getter for the UserAgent Property</summary>
    function GetUserAgent: string;
    /// <summary>Getter for the Authorization Property</summary>
    function GetAuthorization: string;
    /// <summary>Getter for the HeaderValue Property</summary>
    function GetHeaderValue(const AName: string): string;
    /// <summary>Getter for the Headers Property</summary>
    function GetHeaders: TWiRLHeaders;
    /// <summary>Getter for the Content Property</summary>
    function GetContent: string;
    /// <summary>Getter for the ContentStream Property</summary>
    function GetContentStream: TStream;
    /// <summary>Getter for the RawContent Property</summary>
    function GetRawContent: TBytes;
    /// <summary>Getter for the ContentType Property</summary>
    function GetContentType: string;
    /// <summary>Getter for the URL Property</summary>
    function GetURL: string;
    /// <summary>Getter for the HttpMethod Property</summary>
    function GetHttpMethod: string;
    /// <summary>Getter for the ContentMediaType Property</summary>
    function GetContentMediaType: TMediaType;

    /// <summary>Get the HTTP method of the request</summary>
    property HttpMethod: string read GetHttpMethod;
    /// <summary>Get the URL of the request</summary>
    property URL: string read GetURL;
    /// <summary>Get the body of the request as a string</summary>
    property Content: string read GetContent;
    /// <summary>Get the body of the request as a stream</summary>
    property ContentStream: TStream read GetContentStream;
    /// <summary>Get the body of the request as a bytes</summary>
    property RawContent: TBytes read GetRawContent;
    /// <summary>Get all response headers</summary>
    property Headers: TWiRLHeaders read GetHeaders;
    /// <summary>Property to Get Header values</summary>
    /// <param name="AName">Name of the Header</param>
    /// <returns>The string value associated to the given name.</returns>
    property HeaderValue[const AName: string]: string read GetHeaderValue;
    /// <summary>Get ContentType from server response</summary>
    property ContentType: string read GetContentType;
    /// <summary>Get media type info</summary>
    property ContentMediaType: TMediaType read GetContentMediaType;
    /// <summary>Get the accepted media type of the request</summary>
    property Accept: string read GetAccept;
    /// <summary>Get the accepted charset of the request</summary>
    property AcceptCharSet: string read GetAcceptCharSet;
    /// <summary>Get the accepted encoding (gzip, deflate, ...) of the request</summary>
    property AcceptEncoding: string read GetAcceptEncoding;
    /// <summary>Get the accepted langiages of the request</summary>
    property AcceptLanguage: string read GetAcceptLanguage;
    /// <summary>Get the user agent (e.g. the browser) of the request</summary>
    property UserAgent: string read GetUserAgent;
    /// <summary>Get the authorization information of the request</summary>
    property Authorization: string read GetAuthorization;

  end;

  IWiRLResponse = interface
    ['{F75C65E0-9F58-44EB-98DB-01BB3A5AF9F1}']
    /// <summary>Getter for the HeaderValue Property</summary>
    function GetHeaderValue(const AName: string): string;
    /// <summary>Getter for the StatusCode Property</summary>
    function GetStatusCode: Integer;
    /// <summary>Getter for the StatusText Property</summary>
    function GetStatusText: string;
    /// <summary>Getter for the ContentType Property</summary>
    function GetContentType: string;
    /// <summary>Getter for the Content Property</summary>
    function GetContent: string;
    /// <summary>Getter for the ContentStream Property</summary>
    function GetContentStream: TStream;
    /// <summary>Getter for the Headers Property</summary>
    function GetHeaders: TWiRLHeaders;
    /// <summary>Getter for the ContentMediaType Property</summary>
    function GetContentMediaType: TMediaType;
    /// <summary>Getter for the RawContent Property</summary>
    function GetRawContent: TBytes;

    /// <summary>Property to Get Header values</summary>
    /// <param name="AName">Name of the Header</param>
    /// <returns>The string value associated to the given name.</returns>
    property HeaderValue[const AName: string]: string read GetHeaderValue;
    /// <summary>Get StatusText from server response</summary>
    property StatusText: string read GetStatusText;
    /// <summary>Get StatusCode from server response</summary>
    property StatusCode: Integer read GetStatusCode;
    /// <summary>Get ContentType from server response</summary>
    property ContentType: string read GetContentType;
    /// <summary>Get the body from server response as a string</summary>
    property Content: string read GetContent;
    /// <summary>Get the body from server response as a stream</summary>
    property ContentStream: TStream read GetContentStream;
    /// <summary>Get the body from server response as a bytes</summary>
    property RawContent: TBytes read GetRawContent;
    /// <summary>Get all response headers</summary>
    property Headers: TWiRLHeaders read GetHeaders;
    /// <summary>Get media type info</summary>
    property ContentMediaType: TMediaType read GetContentMediaType;
  end;

  EWiRLClientProtocolException = class(EWiRLClientException)
  private
    FResponse: IWiRLResponse;
    function GetStatusCode: Integer;
  public
    constructor Create(AResponse: IWiRLResponse); reintroduce; virtual;
    property StatusCode: Integer read GetStatusCode;
    property Response: IWiRLResponse read FResponse;
  end;

  EWiRLClientResourceException = class(EWiRLClientException)
  private
    FStatusCode: Integer;
    FReasonString: string;
    FJsonResponse: TJSONValue;
    FServerException: string;
  public
    constructor Create(AResponse: IWiRLResponse); reintroduce; virtual;
    destructor Destroy; override;

    property StatusCode: Integer read FStatusCode write FStatusCode;
    property ReasonString: string read FReasonString write FReasonString;
    property JsonResponse: TJSONValue read FJsonResponse write FJsonResponse;
    property ServerException: string read FServerException write FServerException;
  end;

  TWiRLProxyConnectionInfo = class(TPersistent)
  private
    FBasicByDefault: boolean;
    FProxyPort: Integer;
    FPassword: string;
    FUsername: string;
    FProxyServer: string;
  protected
    procedure AssignTo(Destination: TPersistent); override;
  published
    property BasicAuthentication: boolean read FBasicByDefault write FBasicByDefault;
    property ProxyPassword: string read FPassword write FPassword;
    property ProxyPort: Integer read FProxyPort write FProxyPort;
    property ProxyServer: string read FProxyServer write FProxyServer;
    property ProxyUsername: string read FUsername write FUserName;
  end;

  IWiRLClient = interface
  ['{A42C26F5-8B8B-4FE8-A3D4-EF12107F240B}']
    // Setters and getters
    function GetConnectTimeout: Integer;
    procedure SetConnectTimeout(Value: Integer);
    function GetReadTimeout: Integer;
    procedure SetReadTimeout(Value: Integer);
    function GetProxyParams: TWiRLProxyConnectionInfo;
    procedure SetProxyParams(Value: TWiRLProxyConnectionInfo);
    function GetMaxRedirects: Integer;
    procedure SetMaxRedirects(const Value: Integer);
    function GetClientImplementation: TObject;

    // Http methods
    function Delete(const AURL: string; AResponseContent: TStream; const AHeaders: TWiRLHeaders): IWiRLResponse;
    function Get(const AURL: string; AResponseContent: TStream; const AHeaders: TWiRLHeaders): IWiRLResponse;
    function Options(const AURL: string; AResponseContent: TStream; const AHeaders: TWiRLHeaders): IWiRLResponse;
    function Head(const AURL: string; const AHeaders: TWiRLHeaders): IWiRLResponse;
    function Patch(const AURL: string; AContent, AResponse: TStream; const AHeaders: TWiRLHeaders): IWiRLResponse;
    function Post(const AURL: string; AContent, AResponse: TStream; const AHeaders: TWiRLHeaders): IWiRLResponse;
    function Put(const AURL: string; AContent, AResponse: TStream; const AHeaders: TWiRLHeaders): IWiRLResponse;

    // Http properties
    property ConnectTimeout: Integer read GetConnectTimeout write SetConnectTimeout;
    property ReadTimeout: Integer read GetReadTimeout write SetReadTimeout;
    property ProxyParams: TWiRLProxyConnectionInfo read GetProxyParams write SetProxyParams;
    property MaxRedirects: Integer read GetMaxRedirects write SetMaxRedirects;

    property ClientImplementation: TObject read GetClientImplementation;
  end;

  TWiRLClientRegistry = class(TDictionary<string, TClass>)
  private type
    TWiRLClientRegistrySingleton = TWiRLSingleton<TWiRLClientRegistry>;
  protected
    class function GetInstance: TWiRLClientRegistry; static; inline;
  protected
    FDefaultClass: TClass;
    function GetDefaultClass: TClass;
    procedure SetDefaultClass(AClass: TClass);
  public
    constructor Create; virtual;
    class property Instance: TWiRLClientRegistry read GetInstance;

    function CreateClient(const AName: string): IWiRLClient;
    procedure RegisterClient<T: class>(const AName: string; ADefault: Boolean =  False);
  end;

implementation

{ TWiRLClientRegistry }

constructor TWiRLClientRegistry.Create;
begin
  inherited Create;
end;

function TWiRLClientRegistry.CreateClient(const AName: string): IWiRLClient;
var
  LObject: TObject;
  AClientClass: TClass;
begin
  if Self.Count < 1 then
    raise EWiRLException.CreateFmt('CreateClient: no client registered (add "WiRL.http.Client.*" unit to the project)', [AName]);

  if AName = '' then
    AClientClass := GetDefaultClass()
  else if not Self.TryGetValue(AName, AClientClass) then
    raise EWiRLException.CreateFmt('CreateClient: http client [%s] not registered (add "WiRL.http.Client.*" unit to the project)', [AName]);

  LObject := TRttiHelper.CreateInstance(AClientClass);
  if not Supports(LObject, IWiRLClient, Result) then
    raise EWiRLException.CreateFmt('CreateClient: can''t create a http client with class [%s]', [AClientClass.ClassName]);
end;

function TWiRLClientRegistry.GetDefaultClass: TClass;
var
  LClassList: TArray<TPair<string,TClass>>;
begin
  if Assigned(FDefaultClass) then
    Exit(FDefaultClass);
  LClassList := Self.ToArray;
  Result := LClassList[0].Value;
end;

class function TWiRLClientRegistry.GetInstance: TWiRLClientRegistry;
begin
  Result := TWiRLClientRegistrySingleton.Instance;
end;

procedure TWiRLClientRegistry.RegisterClient<T>(const AName: string; ADefault: Boolean = False);
begin
  if not Supports(TClass(T), IWiRLClient) then
    raise EWiRLException.Create(
      Format('Client registration error: [%s] is not a valid client', [TClass(T).QualifiedClassName])
    );

  Self.Add(AName, TClass(T));
  if ADefault then
    SetDefaultClass(TClass(T));
end;

procedure TWiRLClientRegistry.SetDefaultClass(AClass: TClass);
begin
  FDefaultClass := AClass;
end;

{ TWiRLProxyConnectionInfo }

procedure TWiRLProxyConnectionInfo.AssignTo(Destination: TPersistent);
var
  LDest: TWiRLProxyConnectionInfo;
begin
  if Destination is TWiRLProxyConnectionInfo then
  begin
    LDest := TWiRLProxyConnectionInfo(Destination);
    LDest.FPassword := FPassword;
    LDest.FProxyPort := FProxyPort;
    LDest.FProxyServer := FProxyServer;
    LDest.FUsername := FUsername;
    LDest.FBasicByDefault := FBasicByDefault;
  end else
  begin
    inherited AssignTo(Destination);
  end;
end;

{ EWiRLClientProtocolException }

constructor EWiRLClientProtocolException.Create(AResponse: IWiRLResponse);
begin
  inherited Create(AResponse.StatusText);
  FResponse := AResponse;
end;

function EWiRLClientProtocolException.GetStatusCode: Integer;
begin
  Result := FResponse.StatusCode;
end;

{ TWiRLHeader }

constructor TWiRLHeader.Create(const AName, AValue: string);
begin
  Name := AName;
  Value := AValue;
end;

{ EWiRLClientResourceException }

constructor EWiRLClientResourceException.Create(AResponse: IWiRLResponse);
var
  LMessage: string;
begin
  FStatusCode := AResponse.StatusCode;
  FReasonString := AResponse.StatusText;
  FServerException := Exception.ClassName;
  LMessage := FReasonString;

  if AResponse.ContentType = TMediaType.APPLICATION_JSON then
  begin
    FJsonResponse := TJSONObject.ParseJSONValue(AResponse.Content);
    if Assigned(FJsonResponse) then
    begin
      if not FJsonResponse.TryGetValue<string>('message', LMessage) then
        LMessage := FReasonString;

      if not FJsonResponse.TryGetValue<string>('exception', FServerException) then
        LMessage := FServerException;
    end;
  end;

  inherited Create(LMessage);
end;

destructor EWiRLClientResourceException.Destroy;
begin
  FJsonResponse.Free;
  inherited;
end;

{ TWiRLHeadersHelper }

function TWiRLHeadersHelper.GetAccept: string;
begin
  Result := GetValue(TWiRLHeader.ACCEPT);
end;

function TWiRLHeadersHelper.GetAcceptCharSet: string;
begin
  Result := GetValue(TWiRLHeader.ACCEPT_CHARSET);
end;

function TWiRLHeadersHelper.GetAcceptEncoding: string;
begin
  Result := GetValue(TWiRLHeader.ACCEPT_ENCODING);
end;

function TWiRLHeadersHelper.GetAcceptLanguage: string;
begin
  Result := GetValue(TWiRLHeader.ACCEPT_LANGUAGE);
end;

function TWiRLHeadersHelper.GetAuthorization: string;
begin
  Result := GetValue(TWiRLHeader.AUTHORIZATION);
end;

function TWiRLHeadersHelper.GetContentType: string;
begin
  Result := GetValue(TWiRLHeader.CONTENT_TYPE);
end;

function TWiRLHeadersHelper.GetUserAgent: string;
begin
  Result := GetValue(TWiRLHeader.USER_AGENT);
end;

function TWiRLHeadersHelper.GetValue(const AName: string): string;
var
  LHeader: TWiRLHeader;
begin
  Result := '';
  for LHeader in Self do
    if LHeader.Name = AName then
      Exit(LHeader.Value);
end;

procedure TWiRLHeadersHelper.SetAccept(const AValue: string);
begin
  SetValue(TWiRLHeader.ACCEPT, AValue);
end;

procedure TWiRLHeadersHelper.SetAcceptCharSet(const AValue: string);
begin
  SetValue(TWiRLHeader.ACCEPT_CHARSET, AValue);
end;

procedure TWiRLHeadersHelper.SetAcceptEncoding(const AValue: string);
begin
  SetValue(TWiRLHeader.ACCEPT_ENCODING, AValue);
end;

procedure TWiRLHeadersHelper.SetAcceptLanguage(const AValue: string);
begin
  SetValue(TWiRLHeader.ACCEPT_LANGUAGE, AValue);
end;

procedure TWiRLHeadersHelper.SetAuthorization(const AValue: string);
begin
  SetValue(TWiRLHeader.AUTHORIZATION, AValue);
end;

procedure TWiRLHeadersHelper.SetContentType(const AValue: string);
begin
  SetValue(TWiRLHeader.CONTENT_TYPE, AValue);
end;

procedure TWiRLHeadersHelper.SetUserAgent(const AValue: string);
begin
  SetValue(TWiRLHeader.USER_AGENT, AValue);
end;

procedure TWiRLHeadersHelper.SetValue(const AName, AValue: string);
var
  LIndex: Integer;
begin
  for LIndex := Low(Self) to High(Self) do
  begin
    if Self[LIndex].Name = AName then
    begin
      Self[LIndex].Value := AValue;
      Exit;
    end;
  end;
  Self := Self + [TWiRLHeader.Create(AName, AValue)];
end;

end.
