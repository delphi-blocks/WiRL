{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.Client.NetHttp;

{$I ..\Core\WiRL.inc}

interface

uses
  System.SysUtils, System.Classes,

  System.Net.HttpClient,
  System.Net.URLClient,

  WiRL.http.Client.Interfaces,
  WiRL.http.Core,
  WiRL.http.Cookie,
  WiRL.http.Request,
  WiRL.http.Response;

type
  TWiRLClientRequestNetHttp = class(TWiRLRequest)
  private
    FHttpClient: THTTPClient;
    FCookieFields: TWiRLCookies;
    FHeaderFields: TWiRLHeaderList;
//    FContent: TStream;
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
    constructor Create(AHttpClient: THTTPClient);
    destructor Destroy; override;
  end;

  TWiRLClientResponseNetHttp = class(TWiRLResponse)
  private
    FHttpClient: THTTPClient;
    FStatusCode: Integer;
    FReasonString: string;
    FContentStream: TStream;
  protected
    function GetContent: string; override;
    function GetContentStream: TStream; override;
    procedure SetContent(const Value: string); override;
    procedure SetContentStream(const Value: TStream); override;
    function GetStatusCode: Integer; override;
    procedure SetStatusCode(const Value: Integer); override;
    function GetReasonString: string; override;
    procedure SetReasonString(const Value: string); override;
    function GetUnknownResponseCode: string; override;
  public
    procedure SendHeaders; override;

    constructor Create(AHttpClient: THTTPClient);
    destructor Destroy; override;
  end;

  TWiRLClientNetHttp = class(TInterfacedObject, IWiRLClient)
  private
    FHttpClient: THTTPClient;
    FRequest :TWiRLClientRequestNetHttp;
    FResponse :TWiRLClientResponseNetHttp;
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

    // Http methods
    procedure Delete(const AURL: string; AResponseContent: TStream);
    procedure Get(const AURL: string; AResponseContent: TStream);
    procedure Options(const AURL: string; AResponseContent: TStream);
    procedure Head(const AURL: string);
    procedure Patch(const AURL: string; AContent, AResponse: TStream);
    procedure Post(const AURL: string; AContent, AResponse: TStream);
    procedure Put(const AURL: string; AContent, AResponse: TStream);

    function GetRequestHeaders: TNetHeaders;
    procedure BuildResponseObject(AHTTPResponse: IHTTPResponse);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

const
  DefaultUserAgent = 'Mozilla/3.0 (compatible; WiRL with NetHTTP Library)';

{ TWiRLClientNetHttp }

procedure TWiRLClientNetHttp.BuildResponseObject(AHTTPResponse: IHTTPResponse);
var
  Header: TNetHeader;
begin
  FResponse.HeaderFields.Clear;

  for Header in AHTTPResponse.Headers do
  begin
    FResponse.HeaderFields[Header.Name] := Header.Value;
  end;
  FResponse.StatusCode := AHTTPResponse.StatusCode;
  FResponse.ReasonString := AHTTPResponse.StatusText;
end;

constructor TWiRLClientNetHttp.Create;
begin
  inherited;
  FHttpClient := THTTPClient.Create;

  FRequest := TWiRLClientRequestNetHttp.Create(FHttpClient);
  FResponse := TWiRLClientResponseNetHttp.Create(FHttpClient);
end;

procedure TWiRLClientNetHttp.Delete(const AURL: string;
  AResponseContent: TStream);
var
  LHTTPResponse: IHTTPResponse;
begin
  try
    FResponse.ContentStream := AResponseContent;
    LHTTPResponse := FHttpClient.Delete(AURL, AResponseContent, GetRequestHeaders);
  except
    on E: ENetHTTPException do
      Exception.RaiseOuterException(EWiRLSocketException.Create(E.Message));
  end;
  BuildResponseObject(LHTTPResponse);
end;

destructor TWiRLClientNetHttp.Destroy;
begin
  FHttpClient.Free;
  inherited;
end;

procedure TWiRLClientNetHttp.Get(const AURL: string; AResponseContent: TStream);
var
  LHTTPResponse: IHTTPResponse;
begin
  try
    FResponse.ContentStream := AResponseContent;
    LHTTPResponse := FHttpClient.Get(AURL, AResponseContent, GetRequestHeaders);
  except
    on E: ENetHTTPException do
      Exception.RaiseOuterException(EWiRLSocketException.Create(E.Message));
  end;
  BuildResponseObject(LHTTPResponse);
end;

function TWiRLClientNetHttp.GetClientImplementation: TObject;
begin
  Result := FHttpClient;
end;

function TWiRLClientNetHttp.GetConnectTimeout: Integer;
begin
  Result := FHttpClient.ConnectionTimeout;
end;

function TWiRLClientNetHttp.GetMaxRedirects: Integer;
begin
  Result := FHttpClient.MaxRedirects;
end;

function TWiRLClientNetHttp.GetProxyParams: TWiRLProxyConnectionInfo;
begin
  Result := FProxyParams;
end;

function TWiRLClientNetHttp.GetReadTimeout: Integer;
begin
  Result := FHttpClient.ResponseTimeout;
end;

function TWiRLClientNetHttp.GetRequest: TWiRLRequest;
begin
  Result := FRequest;
end;

function TWiRLClientNetHttp.GetRequestHeaders: TNetHeaders;
var
  i: Integer;
begin
  if FRequest.UserAgent = '' then
    FRequest.UserAgent := DefaultUserAgent;

  SetLength(Result, FRequest.FHeaderFields.Count);
  for i := 0 to FRequest.FHeaderFields.Count - 1 do
  begin
    Result[i].Name := FRequest.FHeaderFields.Names[i];
    Result[i].Value := FRequest.FHeaderFields.ValueFromIndex[i];
  end;
end;

function TWiRLClientNetHttp.GetResponse: TWiRLResponse;
begin
  Result := FResponse;
end;

procedure TWiRLClientNetHttp.Head(const AURL: string);
var
  LHTTPResponse: IHTTPResponse;
begin
  try
    LHTTPResponse := FHttpClient.Head(AURL, GetRequestHeaders);
  except
    on E: ENetHTTPException do
      Exception.RaiseOuterException(EWiRLSocketException.Create(E.Message));
  end;
  BuildResponseObject(LHTTPResponse);
end;

procedure TWiRLClientNetHttp.Options(const AURL: string;
  AResponseContent: TStream);
var
  LHTTPResponse: IHTTPResponse;
begin
  try
    FResponse.ContentStream := AResponseContent;
    LHTTPResponse := FHttpClient.Options(AURL, AResponseContent, GetRequestHeaders);
  except
    on E: ENetHTTPException do
      Exception.RaiseOuterException(EWiRLSocketException.Create(E.Message));
  end;
  BuildResponseObject(LHTTPResponse);
end;

procedure TWiRLClientNetHttp.Patch(const AURL: string; AContent,
  AResponse: TStream);
var
  LHTTPResponse: IHTTPResponse;
begin
  try
    FResponse.ContentStream := AResponse;
    LHTTPResponse := FHttpClient.Patch(AURL, AContent, AResponse, GetRequestHeaders);
  except
    on E: ENetHTTPException do
      Exception.RaiseOuterException(EWiRLSocketException.Create(E.Message));
  end;
  BuildResponseObject(LHTTPResponse);
end;

procedure TWiRLClientNetHttp.Post(const AURL: string; AContent,
  AResponse: TStream);
var
  LHTTPResponse: IHTTPResponse;
begin
  try
    FResponse.ContentStream := AResponse;
    LHTTPResponse := FHttpClient.Post(AURL, AContent, AResponse, GetRequestHeaders);
  except
    on E: ENetHTTPException do
      Exception.RaiseOuterException(EWiRLSocketException.Create(E.Message));
  end;
  BuildResponseObject(LHTTPResponse);
end;

procedure TWiRLClientNetHttp.Put(const AURL: string; AContent,
  AResponse: TStream);
var
  LHTTPResponse: IHTTPResponse;
begin
  try
    FResponse.ContentStream := AResponse;
    LHTTPResponse := FHttpClient.Put(AURL, AContent, AResponse, GetRequestHeaders);
  except
    on E: ENetHTTPException do
      Exception.RaiseOuterException(EWiRLSocketException.Create(E.Message));
  end;
  BuildResponseObject(LHTTPResponse);
end;

procedure TWiRLClientNetHttp.SetConnectTimeout(Value: Integer);
begin
  FHttpClient.ConnectionTimeout := Value;
end;

procedure TWiRLClientNetHttp.SetMaxRedirects(const Value: Integer);
begin
  FHttpClient.MaxRedirects := Value;
end;

procedure TWiRLClientNetHttp.SetProxyParams(Value: TWiRLProxyConnectionInfo);
begin
  FProxyParams := Value;
end;

procedure TWiRLClientNetHttp.SetReadTimeout(Value: Integer);
begin
  FHttpClient.ResponseTimeout := Value;
end;

{ TWiRLClientRequestNetHttp }

constructor TWiRLClientRequestNetHttp.Create(AHttpClient: THTTPClient);
begin
  inherited Create;
  FHttpClient := AHttpClient;
  FHeaderFields := TWiRLHeaderList.Create;
  FCookieFields := TWiRLCookies.Create;
end;

destructor TWiRLClientRequestNetHttp.Destroy;
begin
  FHeaderFields.Free;
  FCookieFields.Free;
  inherited;
end;

function TWiRLClientRequestNetHttp.GetContentFields: TWiRLParam;
begin
  raise Exception.Create('Not yet implemented');
end;

function TWiRLClientRequestNetHttp.GetContentStream: TStream;
begin
  raise Exception.Create('Not implemented');
end;

function TWiRLClientRequestNetHttp.GetCookieFields: TWiRLCookies;
begin
  Result := FCookieFields;
end;

function TWiRLClientRequestNetHttp.GetHeaderFields: TWiRLHeaderList;
begin
  Result := FHeaderFields;
end;

function TWiRLClientRequestNetHttp.GetHttpPathInfo: string;
begin
  raise Exception.Create('"PathInfo" not available in TWiRLClientRequestIndy');
end;

function TWiRLClientRequestNetHttp.GetHttpQuery: string;
begin
  Result := '';
end;

function TWiRLClientRequestNetHttp.GetQueryFields: TWiRLParam;
begin
  raise Exception.Create('"QueryFields" not available in TWiRLClientRequestIndy');
end;

function TWiRLClientRequestNetHttp.GetRemoteIP: string;
var
  HostArray: TArray<string>;
begin
  Result := '';
  HostArray := Host.Split([':']);
  if Length(HostArray) > 0 then
    Result := HostArray[0];
end;

function TWiRLClientRequestNetHttp.GetServerPort: Integer;
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

procedure TWiRLClientRequestNetHttp.SetContentStream(const Value: TStream);
begin
  inherited;
  raise Exception.Create('Not implemented');
end;

{ TWiRLClientResponseNetHttp }

constructor TWiRLClientResponseNetHttp.Create(AHttpClient: THTTPClient);
begin
  inherited Create;
  FHttpClient := AHttpClient;
end;

destructor TWiRLClientResponseNetHttp.Destroy;
begin

  inherited;
end;

function TWiRLClientResponseNetHttp.GetContent: string;
begin
  Result := EncodingFromCharSet(ContentMediaType.Charset).GetString(RawContent);
end;

function TWiRLClientResponseNetHttp.GetContentStream: TStream;
begin
  Result := FContentStream;
end;

function TWiRLClientResponseNetHttp.GetReasonString: string;
begin
  Result := FReasonString;
end;

function TWiRLClientResponseNetHttp.GetStatusCode: Integer;
begin
  Result := FStatusCode;
end;

function TWiRLClientResponseNetHttp.GetUnknownResponseCode: string;
begin
  Result := 'Unknown Response Code';
end;

procedure TWiRLClientResponseNetHttp.SendHeaders;
begin
  inherited;

end;

procedure TWiRLClientResponseNetHttp.SetContent(const Value: string);
begin
  inherited;
  raise Exception.Create('Not implemented in the client');
end;

procedure TWiRLClientResponseNetHttp.SetContentStream(const Value: TStream);
begin
  inherited;
  FContentStream := Value;
end;

procedure TWiRLClientResponseNetHttp.SetReasonString(const Value: string);
begin
  inherited;
  FReasonString := Value;
end;

procedure TWiRLClientResponseNetHttp.SetStatusCode(const Value: Integer);
begin
  inherited;
  FStatusCode := Value;
end;

initialization

  TWiRLClientRegistry.Instance.RegisterClient<TWiRLClientNetHttp>(
    'TNetHttpClient (Native)'{$IFDEF HAS_NETHTTP_CLIENT}, True{$ENDIF});

end.
