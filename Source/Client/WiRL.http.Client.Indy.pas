{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.Client.Indy;

interface

uses
  System.SysUtils, System.Classes,

  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP,
  IdHTTPHeaderInfo, IdStack,

  WiRL.http.Client.Interfaces,
  WiRL.http.Core,
  WiRL.http.Cookie,
  WiRL.http.Request,
  WiRL.http.Response;

type
  TWiRLClientRequestIndy = class(TWiRLRequest)
  private
    FIdHTTPRequest: TIdHTTPRequest;
    FCookieFields: TWiRLCookies;
    FHeaderFields: TWiRLHeaderList;
    FContent: TStream;
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
    constructor Create(AIdHTTPRequest: TIdHTTPRequest);
    destructor Destroy; override;
  end;

  TWiRLClientResponseIndy = class(TWiRLResponse)
  private
    FIdHTTPResponse: TIdHTTPResponse;
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

    constructor Create(AIdHTTPResponse: TIdHTTPResponse);
  end;

  TWiRLClientIndy = class(TInterfacedObject, IWiRLClient)
  private
    FHttpClient: TIdHTTP;
    FRequest: TWiRLClientRequestIndy;
    FResponse: TWiRLClientResponseIndy;
    FProxyParams: TWiRLProxyConnectionInfo;
    // Setters and getters
    function GetRequest: TWiRLRequest;
    function GetResponse: TWiRLResponse;
    function GetConnectTimeout: Integer;
    procedure SetConnectTimeout(Value: Integer);
    function GetReadTimeout: Integer;
    procedure SetReadTimeout(Value: Integer);
    function GetProxyParams: TWiRLProxyConnectionInfo;
    procedure SetProxyParams(Value: TWiRLProxyConnectionInfo);
    function GetMaxRedirects: Integer;
    procedure SetMaxRedirects(const Value: Integer);
    function GetClientImplementation: TObject;

    procedure BuildRequestObject;
    procedure BuildResponseObject;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    // Http methods
    procedure Get(const AURL: string; AResponseContent: TStream);
    procedure Post(const AURL: string; ARequestContent, AResponseContent: TStream);
    procedure Put(const AURL: string; ARequestContent, AResponseContent: TStream);
    procedure Delete(const AURL: string; AResponseContent: TStream);
    procedure Options(const AURL: string; AResponseContent: TStream);
    procedure Head(const AURL: string);
    procedure Patch(const AURL: string; ARequestContent, AResponseContent: TStream);
  end;

implementation

{ TWiRLClientIndy }

procedure TWiRLClientIndy.BuildResponseObject;
var
  i: Integer;
  Name, Value: string;
begin
  FResponse.HeaderFields.Clear;
  for i := 0 to FHttpClient.Response.RawHeaders.Count - 1 do
  begin
    Name := FHttpClient.Response.RawHeaders.Names[i];
    Value := FHttpClient.Response.RawHeaders.Values[Name];
    FResponse.HeaderFields[Name] := Value;
  end;
end;

constructor TWiRLClientIndy.Create;
begin
  FHttpClient := TIdHTTP.Create(nil);
  FHttpClient.HTTPOptions := FHttpClient.HTTPOptions + [hoNoProtocolErrorException, hoWantProtocolErrorContent];

  FRequest := TWiRLClientRequestIndy.Create(FHttpClient.Request);
  FResponse := TWiRLClientResponseIndy.Create(FHttpClient.Response);
end;

procedure TWiRLClientIndy.Delete(const AURL: string; AResponseContent: TStream);
begin
  BuildRequestObject;
  try
    FResponse.ContentStream := AResponseContent;
    FHttpClient.Delete(AURL, AResponseContent);
  except
    on E: EIdSocketError do
      Exception.RaiseOuterException(EWiRLSocketException.Create(E.Message));
  end;
  BuildResponseObject;
end;

destructor TWiRLClientIndy.Destroy;
begin
  FHttpClient.Free;
  FRequest.Free;
  FResponse.Free;
  inherited;
end;

procedure TWiRLClientIndy.Get(const AURL: string; AResponseContent: TStream);
begin
  BuildRequestObject;
  try
    FResponse.ContentStream := AResponseContent;
    FHttpClient.Get(AURL, AResponseContent);
  except
    on E: EIdSocketError do
      Exception.RaiseOuterException(EWiRLSocketException.Create(E.Message));
  end;
  BuildResponseObject;
end;

function TWiRLClientIndy.GetClientImplementation: TObject;
begin
  Result := FHttpClient;
end;

function TWiRLClientIndy.GetConnectTimeout: Integer;
begin
  Result := FHttpClient.ConnectTimeout;
end;

function TWiRLClientIndy.GetMaxRedirects: Integer;
begin
  Result := FHttpClient.RedirectMaximum;
end;

function TWiRLClientIndy.GetProxyParams: TWiRLProxyConnectionInfo;
begin
  Result := FProxyParams;
end;

function TWiRLClientIndy.GetReadTimeout: Integer;
begin
  Result := FHttpClient.ReadTimeout;
end;

function TWiRLClientIndy.GetRequest: TWiRLRequest;
begin
  Result := FRequest;
end;

function TWiRLClientIndy.GetResponse: TWiRLResponse;
begin
  Result := FResponse;
end;

procedure TWiRLClientIndy.Head(const AURL: string);
begin
  BuildRequestObject;
  try
    FHttpClient.Head(AURL);
  except
    on E: EIdSocketError do
      Exception.RaiseOuterException(EWiRLSocketException.Create(E.Message));
  end;
  BuildResponseObject;
end;

procedure TWiRLClientIndy.BuildRequestObject;
var
  i: Integer;
begin
  // Copy custom headers
  FHttpClient.Request.CustomHeaders.Clear;
  for i := 0 to FRequest.FHeaderFields.Count - 1 do
  begin
    FHttpClient.Request.CustomHeaders.AddValue(
      FRequest.FHeaderFields.Names[i],
      FRequest.FHeaderFields.ValueFromIndex[i]
    );
  end;

  // Copy standard indy http headers
  FHttpClient.Request.Accept := FRequest.Accept;
  FHttpClient.Request.AcceptCharSet := FRequest.AcceptCharSet;
  FHttpClient.Request.AcceptEncoding := FRequest.AcceptEncoding;
  FHttpClient.Request.AcceptLanguage := FRequest.AcceptLanguage;
  FHttpClient.Request.Host := FRequest.Host;
  FHttpClient.Request.From := FRequest.From;
  FHttpClient.Request.Referer := FRequest.Referer;
  FHttpClient.Request.Range := FRequest.Range;
  if FRequest.UserAgent = '' then
    FHttpClient.Request.UserAgent := 'Mozilla/3.0 (compatible; WiRL with Indy Library)'
  else
    FHttpClient.Request.UserAgent := FRequest.UserAgent;

  // Write proxy setting
  if Assigned(FProxyParams) then
  begin
    FHttpClient.ProxyParams.BasicAuthentication := FProxyParams.BasicAuthentication;
    FHttpClient.ProxyParams.ProxyServer := FProxyParams.ProxyServer;
    FHttpClient.ProxyParams.ProxyPort := FProxyParams.ProxyPort;
    FHttpClient.ProxyParams.ProxyUsername := FProxyParams.ProxyUsername;
    FHttpClient.ProxyParams.ProxyPassword := FProxyParams.ProxyPassword;
  end;
end;

procedure TWiRLClientIndy.Options(const AURL: string;
  AResponseContent: TStream);
begin
  BuildRequestObject;
  try
    FResponse.ContentStream := AResponseContent;
    FHttpClient.Options(AURL, AResponseContent);
  except
    on E: EIdSocketError do
      Exception.RaiseOuterException(EWiRLSocketException.Create(E.Message));
  end;
  BuildResponseObject;
end;

procedure TWiRLClientIndy.Patch(const AURL: string; ARequestContent,
  AResponseContent: TStream);
begin
  BuildRequestObject;
  try
    FRequest.ContentStream := ARequestContent;
    FResponse.ContentStream := AResponseContent;
    FHttpClient.Patch(AURL, ARequestContent, AResponseContent);
  except
    on E: EIdSocketError do
      Exception.RaiseOuterException(EWiRLSocketException.Create(E.Message));
  end;
  BuildResponseObject;
end;

procedure TWiRLClientIndy.Post(const AURL: string; ARequestContent,
  AResponseContent: TStream);
begin
  BuildRequestObject;
  try
    FRequest.ContentStream := ARequestContent;
    FResponse.ContentStream := AResponseContent;
    FHttpClient.Post(AURL, ARequestContent, AResponseContent);
  except
    on E: EIdSocketError do
      Exception.RaiseOuterException(EWiRLSocketException.Create(E.Message));
  end;
  BuildResponseObject;
end;

procedure TWiRLClientIndy.Put(const AURL: string; ARequestContent, AResponseContent: TStream);
begin
  BuildRequestObject;
  try
    FRequest.ContentStream := ARequestContent;
    FResponse.ContentStream := AResponseContent;
    FHttpClient.Put(AURL, ARequestContent, AResponseContent);
  except
    on E: EIdSocketError do
      Exception.RaiseOuterException(EWiRLSocketException.Create(E.Message));
  end;
  BuildResponseObject;
end;

procedure TWiRLClientIndy.SetConnectTimeout(Value: Integer);
begin
  FHttpClient.ConnectTimeout := Value;
end;

procedure TWiRLClientIndy.SetMaxRedirects(const Value: Integer);
begin
  FHttpClient.RedirectMaximum := Value;
end;

procedure TWiRLClientIndy.SetProxyParams(Value: TWiRLProxyConnectionInfo);
begin
  FProxyParams := Value;
end;

procedure TWiRLClientIndy.SetReadTimeout(Value: Integer);
begin
  FHttpClient.ReadTimeout := Value;
end;

{ TWiRLClientResponseIndy }

constructor TWiRLClientResponseIndy.Create(AIdHTTPResponse: TIdHTTPResponse);
begin
  inherited Create;
  FIdHTTPResponse := AIdHTTPResponse;
end;

function TWiRLClientResponseIndy.GetContent: string;
begin
  Result := EncodingFromCharSet(ContentMediaType.Charset).GetString(RawContent);
end;

function TWiRLClientResponseIndy.GetContentStream: TStream;
begin
  Result := FIdHTTPResponse.ContentStream;
end;

function TWiRLClientResponseIndy.GetReasonString: string;
begin
  Result := FIdHTTPResponse.ResponseText;
end;

function TWiRLClientResponseIndy.GetStatusCode: Integer;
begin
  Result := FIdHTTPResponse.ResponseCode;
end;

procedure TWiRLClientResponseIndy.SendHeaders;
begin
  inherited;

end;

procedure TWiRLClientResponseIndy.SetContent(const Value: string);
var
  LStream: TStream;
begin
  LStream := TStringStream.Create(Value, EncodingFromCharSet((ContentMediaType.Charset)));
  ContentStream := LStream;
end;

procedure TWiRLClientResponseIndy.SetContentStream(const Value: TStream);
begin
  inherited;
  FIdHTTPResponse.ContentStream := Value;
end;

procedure TWiRLClientResponseIndy.SetReasonString(const Value: string);
begin
  inherited;
  FIdHTTPResponse.ResponseText := Value;
end;

procedure TWiRLClientResponseIndy.SetStatusCode(const Value: Integer);
begin
  inherited;
  FIdHTTPResponse.ResponseCode := Value;
end;

{ TWiRLClientRequestIndy }

constructor TWiRLClientRequestIndy.Create(AIdHTTPRequest: TIdHTTPRequest);
begin
  inherited Create;
  FIdHTTPRequest := AIdHTTPRequest;
  FHeaderFields := TWiRLHeaderList.Create;
  FCookieFields := TWiRLCookies.Create;
end;

destructor TWiRLClientRequestIndy.Destroy;
begin
  FHeaderFields.Free;
  FCookieFields.Free;
  inherited;
end;

function TWiRLClientRequestIndy.GetContentFields: TWiRLParam;
begin
  raise Exception.Create('Not yet implemented');
end;

function TWiRLClientRequestIndy.GetContentStream: TStream;
begin
  Result := FContent;
end;

function TWiRLClientRequestIndy.GetCookieFields: TWiRLCookies;
begin
  Result := FCookieFields;
end;

function TWiRLClientRequestIndy.GetHeaderFields: TWiRLHeaderList;
begin
  Result := FHeaderFields;
end;

function TWiRLClientRequestIndy.GetHttpPathInfo: string;
begin
  raise Exception.Create('"PathInfo" not available in TWiRLClientRequestIndy');
end;

function TWiRLClientRequestIndy.GetHttpQuery: string;
begin
  Result := '';
end;

function TWiRLClientRequestIndy.GetQueryFields: TWiRLParam;
begin
  raise Exception.Create('"QueryFields" not available in TWiRLClientRequestIndy');
end;

function TWiRLClientRequestIndy.GetRemoteIP: string;
var
  HostArray: TArray<string>;
begin
  Result := '';
  HostArray := Host.Split([':']);
  if Length(HostArray) > 0 then
    Result := HostArray[0];
end;

function TWiRLClientRequestIndy.GetServerPort: Integer;
const
  DefaultPort = 80;
var
  HostArray: TArray<string>;
begin
  Result := DefaultPort;
  HostArray := Host.Split([':']);
  if Length(HostArray) > 1 then
    Result := StrToIntDef(HostArray[1], DefaultPort);
end;

procedure TWiRLClientRequestIndy.SetContentStream(const Value: TStream);
begin
  inherited;
  FContent := Value;
end;

initialization

  TWiRLClientRegistry.Instance.RegisterClient<TWiRLClientIndy>('TIdHttp (Indy)');

end.
