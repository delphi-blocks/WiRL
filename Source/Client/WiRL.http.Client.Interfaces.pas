{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2023 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.Client.Interfaces;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes, System.SysUtils, System.JSON, System.Rtti, System.Generics.Collections,

  WiRL.Rtti.Utils,
  WiRL.Core.Classes,
  WiRL.Core.Exceptions,
  WiRL.Core.Singleton,
  WiRL.Core.Context,
  WiRL.http.Core,
  WiRL.http.Headers,
  WiRL.http.Accept.MediaType;

type
  EWiRLClientException = class(EWiRLException);

  EWiRLSocketException = class(EWiRLClientException);

  IWiRLResponse = interface;

  TWiRLContent = record
  private
    FResponse: IWiRLResponse;
    FContext: TWiRLContextBase;
  public
    function AsType<T>: T; overload;
    procedure AsType(LEntity: TObject); overload;
    constructor Create(AResponse: IWiRLResponse; AContext: TWiRLContextBase);
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
    function GetHeaders: IWiRLHeaders;
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
    property Headers: IWiRLHeaders read GetHeaders;
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
    /// <summary>Getter for the StatusCode Property</summary>
    function GetStatusCode: Integer;
    /// <summary>Getter for the StatusText Property</summary>
    function GetStatusText: string;
    /// <summary>Getter for the ContentType Property</summary>
    function GetContentType: string;
    /// <summary>Getter for the Content Property</summary>
    function GetContentText: string;
    /// <summary>Getter for the ContentStream Property</summary>
    function GetContentStream: TStream;
    /// <summary>Getter for the Headers Property</summary>
    function GetHeaders: IWiRLHeaders;
    /// <summary>Getter for the ContentMediaType Property</summary>
    function GetContentMediaType: TMediaType;
    /// <summary>Getter for the RawContent Property</summary>
    function GetRawContent: TBytes;
    /// <summary>Setter for the StatusCode Property</summary>
    procedure SetStatusCode(AValue: Integer);
    /// <summary>Setter for the StatusText Property</summary>
    procedure SetStatusText(const AValue: string);
    /// <summary>If the ContentStream its owned by the request</summary>
    procedure SetOwnContentStream(const AValue: Boolean);
    /// <summary>Get content as type T</summary>
    function GetContent: TWiRLContent;
    /// <summary>Set the response context (internal usage)</summary>
    procedure SetContext(AContext: TWiRLContextBase);
    /// <summary>Get Status category from server response (100, 200, 300, ...)</summary>
    function GetStatus: TWiRLResponseStatus;

    /// <summary>Get StatusText from server response</summary>
    property StatusText: string read GetStatusText write SetStatusText;
    /// <summary>Get StatusCode from server response</summary>
    property StatusCode: Integer read GetStatusCode write SetStatusCode;
    /// <summary>Get Status category from server response (100, 200, 300, ...)</summary>
    property Status: TWiRLResponseStatus read GetStatus;
    /// <summary>Get ContentType from server response</summary>
    property ContentType: string read GetContentType;
    /// <summary>Get the body from server response as a string</summary>
    property ContentText: string read GetContentText;
    /// <summary>Get the body from server response as a stream</summary>
    property ContentStream: TStream read GetContentStream;
    /// <summary>Get the body from server response as a bytes</summary>
    property RawContent: TBytes read GetRawContent;
    /// <summary>Get all response headers</summary>
    property Headers: IWiRLHeaders read GetHeaders;
    /// <summary>Get media type info</summary>
    property ContentMediaType: TMediaType read GetContentMediaType;
    /// <summary>Get content as type T</summary>
    property Content: TWiRLContent read GetContent;
  end;

  EWiRLClientProtocolException = class(EWiRLClientException)
  private
    FResponse: IWiRLResponse;
    FResponseJson: TJSONValue;

    FReasonString: string;
    FStatusCode: Integer;
    FResponseText: string;
    FServerException: string;
  public
    constructor Create(AResponse: IWiRLResponse); reintroduce; virtual;
    destructor Destroy; override;

    property StatusCode: Integer read FStatusCode;
    property Response: IWiRLResponse read FResponse;
    property ReasonString: string read FReasonString;
    property ResponseText: string read FResponseText;
    property ResponseJson: TJSONValue read FResponseJson;
    property ServerException: string read FServerException;
  end;

  // deprecated: use EWiRLClientProtocolException
  EWiRLClientResourceException = class(EWiRLClientProtocolException)
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
    function Delete(const AURL: string; ARequestStream, AResponseStream: TStream; AHeaders: IWiRLHeaders): IWiRLResponse;
    function Get(const AURL: string; AResponseStream: TStream; AHeaders: IWiRLHeaders): IWiRLResponse;
    function Options(const AURL: string; AResponseStream: TStream; AHeaders: IWiRLHeaders): IWiRLResponse;
    function Head(const AURL: string; AHeaders: IWiRLHeaders): IWiRLResponse;
    function Patch(const AURL: string; AContentStream, AResponseStream: TStream; AHeaders: IWiRLHeaders): IWiRLResponse;
    function Post(const AURL: string; AContentStream, AResponseStream: TStream; AHeaders: IWiRLHeaders): IWiRLResponse;
    function Put(const AURL: string; AContentStream, AResponseStream: TStream; AHeaders: IWiRLHeaders): IWiRLResponse;

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

uses
  WiRL.Client.Application;

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
var
  LMessage: string;
begin
  FResponse := AResponse;
  FStatusCode := AResponse.StatusCode;
  FReasonString := AResponse.StatusText;
  FResponseText := AResponse.ContentText;
  FServerException := Exception.ClassName;
  LMessage := FReasonString;

  if AResponse.ContentType = TMediaType.APPLICATION_JSON then
  begin
    FResponseJson := TJSONObject.ParseJSONValue(FResponseText);
    if Assigned(FResponseJson) then
    begin
      if not FResponseJson.TryGetValue<string>('message', LMessage) then
        LMessage := FReasonString;

      if not FResponseJson.TryGetValue<string>('exception', FServerException) then
        FServerException := Exception.ClassName;
    end;
  end;

  inherited Create(LMessage);
end;

destructor EWiRLClientProtocolException.Destroy;
begin
  FResponseJson.Free;
  inherited;
end;

{ TWiRLContent }

procedure TWiRLContent.AsType(LEntity: TObject);
var
  LApplication: TWiRLClientApplication;
begin
  LApplication := FContext.FindContextDataAs<TWiRLClientApplication>;
  if not Assigned(LApplication) then
    raise EWiRLClientException.Create('Application is not assigned');

  LApplication.StreamToObject(LEntity, FResponse.Headers, FResponse.ContentStream, FContext);
end;

function TWiRLContent.AsType<T>: T;
var
  LApplication: TWiRLClientApplication;
begin
  if not Assigned(FContext) then
    raise EWiRLClientException.Create('Context unavailable');
  LApplication := FContext.FindContextDataAs<TWiRLClientApplication>;
  if not Assigned(LApplication) then
    raise EWiRLClientException.Create('Application is not assigned');

  Result := LApplication.StreamToObject<T>(FResponse.Headers, FResponse.ContentStream, FContext);
end;

constructor TWiRLContent.Create(AResponse: IWiRLResponse;
  AContext: TWiRLContextBase);
begin
  FResponse := AResponse;
  FContext := AContext;
end;

end.
