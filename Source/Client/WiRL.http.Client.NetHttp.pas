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
  WiRL.http.Headers,
  WiRL.http.Accept.MediaType,
  WiRL.http.Cookie;

type
  TWiRLClientResponseNetHttp = class(TInterfacedObject, IWiRLResponse)
  private
    FStatusCode: Integer;
    FStatusText: string;
    FResponse: IHTTPResponse;
    FMediaType: TMediaType;
    FHeaders: IWiRLHeaders;
    FOwnContentStream: Boolean;

    { IWiRLResponse }
    function GetHeaderValue(const AName: string): string;
    function GetStatusCode: Integer;
    function GetStatusText: string;
    function GetContentType: string;
    function GetContent: string;
    function GetContentStream: TStream;
    function GetHeaders: IWiRLHeaders;
    function GetContentMediaType: TMediaType;
    function GetRawContent: TBytes;
    procedure SetStatusCode(AValue: Integer);
    procedure SetStatusText(const AValue: string);
    procedure SetOwnContentStream(const AValue: Boolean);
  public
    constructor Create(AResponse: IHTTPResponse);
    destructor Destroy; override;
  end;

  TWiRLClientNetHttp = class(TInterfacedObject, IWiRLClient)
  private
    FHttpClient: THTTPClient;
    FProxyParams: TWiRLProxyConnectionInfo;
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
    function Get(const AURL: string; AResponseContent: TStream; AHeaders: IWiRLHeaders): IWiRLResponse;
    function Post(const AURL: string; ARequestContent, AResponseContent: TStream; AHeaders: IWiRLHeaders): IWiRLResponse;
    function Put(const AURL: string; ARequestContent, AResponseContent: TStream; AHeaders: IWiRLHeaders): IWiRLResponse;
    function Delete(const AURL: string; AResponseContent: TStream; AHeaders: IWiRLHeaders): IWiRLResponse;
    function Options(const AURL: string; AResponseContent: TStream; AHeaders: IWiRLHeaders): IWiRLResponse;
    function Head(const AURL: string; AHeaders: IWiRLHeaders): IWiRLResponse;
    function Patch(const AURL: string; ARequestContent, AResponseContent: TStream; AHeaders: IWiRLHeaders): IWiRLResponse;

    function GetRequestHeaders(AHeaders: IWiRLHeaders): TNetHeaders;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

const
  HttpClientVendorName = 'TNetHttpClient (Native)';

implementation

const
  DefaultUserAgent = 'Mozilla/3.0 (compatible; WiRL with NetHTTP Library)';

{ TWiRLClientNetHttp }

constructor TWiRLClientNetHttp.Create;
begin
  inherited;
  FHttpClient := THTTPClient.Create;
end;

function TWiRLClientNetHttp.Delete(const AURL: string;
  AResponseContent: TStream; AHeaders: IWiRLHeaders): IWiRLResponse;
var
  LHTTPResponse: IHTTPResponse;
begin
  try
    LHTTPResponse := FHttpClient.Delete(AURL, AResponseContent, GetRequestHeaders(AHeaders));
  except
    on E: ENetHTTPException do
      Exception.RaiseOuterException(EWiRLSocketException.Create(E.Message));
  end;
  Result := TWiRLClientResponseNetHttp.Create(LHTTPResponse);
end;

destructor TWiRLClientNetHttp.Destroy;
begin
  FHttpClient.Free;
  inherited;
end;

function TWiRLClientNetHttp.Get(const AURL: string; AResponseContent: TStream; AHeaders: IWiRLHeaders): IWiRLResponse;
var
  LHTTPResponse: IHTTPResponse;
begin
  try
    LHTTPResponse := FHttpClient.Get(AURL, AResponseContent, GetRequestHeaders(AHeaders));
  except
    on E: ENetHTTPException do
      Exception.RaiseOuterException(EWiRLSocketException.Create(E.Message));
  end;
  Result := TWiRLClientResponseNetHttp.Create(LHTTPResponse);
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

function TWiRLClientNetHttp.GetRequestHeaders(AHeaders: IWiRLHeaders): TNetHeaders;
var
  LHeader: TWiRLHeader;
begin
  for LHeader in AHeaders do
  begin
    Result := Result + [TNameValuePair.Create(LHeader.Name, LHeader.Value)];
  end;

  if AHeaders.UserAgent = '' then
    Result := Result + [TNameValuePair.Create(TWiRLHeader.USER_AGENT, DefaultUserAgent)];
end;

function TWiRLClientNetHttp.Head(const AURL: string; AHeaders: IWiRLHeaders): IWiRLResponse;
var
  LHTTPResponse: IHTTPResponse;
begin
  try
    LHTTPResponse := FHttpClient.Head(AURL, GetRequestHeaders(AHeaders));
  except
    on E: ENetHTTPException do
      Exception.RaiseOuterException(EWiRLSocketException.Create(E.Message));
  end;
  Result := TWiRLClientResponseNetHttp.Create(LHTTPResponse);
end;

function TWiRLClientNetHttp.Options(const AURL: string;
  AResponseContent: TStream; AHeaders: IWiRLHeaders): IWiRLResponse;
var
  LHTTPResponse: IHTTPResponse;
begin
  try
    LHTTPResponse := FHttpClient.Options(AURL, AResponseContent, GetRequestHeaders(AHeaders));
  except
    on E: ENetHTTPException do
      Exception.RaiseOuterException(EWiRLSocketException.Create(E.Message));
  end;
  Result := TWiRLClientResponseNetHttp.Create(LHTTPResponse);
end;

function TWiRLClientNetHttp.Patch(const AURL: string; ARequestContent,
  AResponseContent: TStream; AHeaders: IWiRLHeaders): IWiRLResponse;
var
  LHTTPResponse: IHTTPResponse;
begin
  try
    ARequestContent.Position := 0;
    LHTTPResponse := FHttpClient.Patch(AURL, ARequestContent, AResponseContent, GetRequestHeaders(AHeaders));
  except
    on E: ENetHTTPException do
      Exception.RaiseOuterException(EWiRLSocketException.Create(E.Message));
  end;
  Result := TWiRLClientResponseNetHttp.Create(LHTTPResponse);
end;

function TWiRLClientNetHttp.Post(const AURL: string; ARequestContent,
  AResponseContent: TStream; AHeaders: IWiRLHeaders): IWiRLResponse;
var
  LHTTPResponse: IHTTPResponse;
begin
  try
    ARequestContent.Position := 0;
    LHTTPResponse := FHttpClient.Post(AURL, ARequestContent, AResponseContent, GetRequestHeaders(AHeaders));
  except
    on E: ENetHTTPException do
      Exception.RaiseOuterException(EWiRLSocketException.Create(E.Message));
  end;
  Result := TWiRLClientResponseNetHttp.Create(LHTTPResponse);
end;

function TWiRLClientNetHttp.Put(const AURL: string; ARequestContent,
  AResponseContent: TStream; AHeaders: IWiRLHeaders): IWiRLResponse;
var
  LHTTPResponse: IHTTPResponse;
begin
  try
    ARequestContent.Position := 0;
    LHTTPResponse := FHttpClient.Put(AURL, ARequestContent, AResponseContent, GetRequestHeaders(AHeaders));
  except
    on E: ENetHTTPException do
      Exception.RaiseOuterException(EWiRLSocketException.Create(E.Message));
  end;
  Result := TWiRLClientResponseNetHttp.Create(LHTTPResponse);
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

{ TWiRLClientResponseNetHttp }

constructor TWiRLClientResponseNetHttp.Create(AResponse: IHTTPResponse);
begin
  inherited Create;
  FResponse := AResponse;
  FOwnContentStream := True;
end;

destructor TWiRLClientResponseNetHttp.Destroy;
begin
  FMediaType.Free;
  if FOwnContentStream then
    FreeAndNil(FResponse.ContentStream);
  inherited;
end;

function TWiRLClientResponseNetHttp.GetContent: string;
begin
  Result := EncodingFromCharSet(GetContentMediaType.Charset).GetString(GetRawContent);
end;

function TWiRLClientResponseNetHttp.GetContentMediaType: TMediaType;
begin
  if not Assigned(FMediaType) then
    FMediaType := TMediaType.Create(GetContentType);
  Result := FMediaType;
end;

function TWiRLClientResponseNetHttp.GetContentStream: TStream;
begin
  Result := FResponse.ContentStream;
end;

function TWiRLClientResponseNetHttp.GetContentType: string;
begin
  Result := GetHeaderValue('Content-Type');
end;

function TWiRLClientResponseNetHttp.GetHeaders: IWiRLHeaders;
var
  LHeader: TNameValuePair;
begin
  if not Assigned(FHeaders) then
  begin
    FHeaders := TWiRLHeaders.Create;
    for LHeader in FResponse.Headers do
    begin
      FHeaders.AddHeader(TWiRLHeader.Create(LHeader.Name, LHeader.Value));
    end;
  end;
  Result := FHeaders;
end;

function TWiRLClientResponseNetHttp.GetHeaderValue(const AName: string): string;
begin
  Result := GetHeaders.Values[AName];
end;

function TWiRLClientResponseNetHttp.GetRawContent: TBytes;
begin
  if (GetContentStream <> nil) and (GetContentStream.Size > 0) then
  begin
    GetContentStream.Position := 0;
    SetLength(Result, GetContentStream.Size);
    GetContentStream.ReadBuffer(Result[0], GetContentStream.Size);
  end;
end;

function TWiRLClientResponseNetHttp.GetStatusCode: Integer;
begin
  if FStatusCode <> 0 then
    Result := FStatusCode
  else
    Result := FResponse.StatusCode;
end;

function TWiRLClientResponseNetHttp.GetStatusText: string;
begin
  if FStatusText <> '' then
    Result := FStatusText
  else
    Result := FResponse.StatusText;
end;

procedure TWiRLClientResponseNetHttp.SetOwnContentStream(const AValue: Boolean);
begin
  FOwnContentStream := AValue;
end;

procedure TWiRLClientResponseNetHttp.SetStatusCode(AValue: Integer);
begin
  FStatusCode := AValue;
end;

procedure TWiRLClientResponseNetHttp.SetStatusText(const AValue: string);
begin
  FStatusText := AValue;
end;

initialization

  TWiRLClientRegistry.Instance.RegisterClient<TWiRLClientNetHttp>(
    HttpClientVendorName{$IFDEF HAS_NETHTTP_CLIENT}, True{$ENDIF});

end.
