{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.Request;

interface

uses
  System.Classes, System.SysUtils,

  WiRL.Http.Core,
  WiRL.Http.Headers,
  WiRL.http.Cookie,
  WiRL.Http.Accept.Parser,
  WiRL.Http.Accept.Charset,
  WiRL.Http.Accept.Encoding,
  WiRL.http.Accept.MediaType,
  WiRL.http.Accept.Language,
  WiRL.http.MultipartData;

type
  TWiRLRequest = class;

  // deprecated
  TWiRLRequestHeaderList = class(TObject)
  private
    FRequest: TWiRLRequest;
    function GetValue(const AName: string): string;
    procedure SetValue(const AName, AValue: string);
  public
    property Values[const AName: string]: string read GetValue write SetValue; default;
    constructor Create(ARequest: TWiRLRequest);
  end;

  TWiRLRequest = class
  private
    FPathInfo: string;
    FQuery: string;
    FContentMediaType: TMediaType;
    FAcceptableCharSets: TAcceptCharsetList;
    FAcceptableEncodings: TAcceptEncodingList;
    FAcceptableLanguages: TAcceptLanguageList;
    FAcceptableMediaTypes: TMediaTypeList;
    FMultiPartFormData: TWiRLFormDataMultiPart;
    FApplication: TObject;
    FHeaderFields: TWiRLRequestHeaderList;
    function GetAcceptableMediaTypes: TMediaTypeList;
    function GetContentText: string;
    procedure SetContentText(const Value: string);
    function GetRawContent: TBytes;
    procedure SetRawContent(const Value: TBytes);
    function GetContentMediaType: TMediaType;
    function GetAcceptableCharSets: TAcceptCharsetList;
    function GetAcceptableLanguages: TAcceptLanguageList;
    function GetAcceptableEncodings: TAcceptEncodingList;
    function GetAuthorization: string;
    procedure SetAuthorization(const Value: string);
    function GetAccept: string;
    procedure SetAccept(const Value: string);
    function GetAcceptCharSet: string;
    procedure SetAcceptCharSet(const Value: string);
    function GetAcceptEncoding: string;
    procedure SetAcceptEncoding(const Value: string);
    function GetAcceptLanguage: string;
    procedure SetAcceptLanguage(const Value: string);
    function GetContentType: string;
    procedure SetContentType(const Value: string);
    function GetContentLength: Integer;
    procedure SetContentLength(const Value: Integer);
    function GetContentVersion: string;
    procedure SetContentVersion(const Value: string);
    function GetContentEncoding: string;
    procedure SetContentEncoding(const Value: string);
    function GetHost: string;
    procedure SetHost(const Value: string);
    function GetPathInfo: string;
    function GetQuery: string;
    procedure SetPathInfo(const Value: string);
    procedure SetQuery(const Value: string);
    function GetUserAgent: string;
    procedure SetUserAgent(const Value: string);
    function GetFrom: string;
    function GetRange: string;
    function GetReferer: string;
    procedure SetFrom(const Value: string);
    procedure SetRange(const Value: string);
    procedure SetReferer(const Value: string);
    function GetMultiPartFormData: TWiRLFormDataMultiPart;
    procedure SetApplication(const Value: TObject);
  protected
    FMethod: string;
    function GetHttpQuery: string; virtual; abstract;
    function GetRemoteIP: string; virtual; abstract;
    function GetServerPort: Integer; virtual; abstract;
    function GetHeaders: IWiRLHeaders; virtual; abstract;
    function GetQueryFields: TWiRLParam; virtual; abstract;
    function GetContentFields: TWiRLParam; virtual; abstract;
    function GetCookieFields: TWiRLCookies; virtual; abstract;
    function GetContentStream: TStream; virtual; abstract;
    procedure SetContentStream(const Value: TStream); virtual; abstract;
    function GetHttpPathInfo: string; virtual; abstract;
    function GetConnection: TWiRLConnection; virtual; abstract;
  public
    destructor Destroy; override;

    function HeaderFields: TWiRLRequestHeaderList; deprecated;

    property PathInfo: string read GetPathInfo write SetPathInfo;
    property Query: string read GetQuery write SetQuery;
    property Method: string read FMethod write FMethod;
    property Host: string read GetHost write SetHost;
    property UserAgent: string read GetUserAgent write SetUserAgent;
    property From: string read GetFrom write SetFrom;
    property Referer: string read GetReferer write SetReferer;
    property Range: string read GetRange write SetRange;
    property RemoteIP: string read GetRemoteIP;
    property ServerPort: Integer read GetServerPort;
    property QueryFields: TWiRLParam read GetQueryFields;
    property ContentFields: TWiRLParam read GetContentFields;
    property Headers: IWiRLHeaders read GetHeaders;
    property CookieFields: TWiRLCookies read GetCookieFields;
    property Content: string read GetContentText write SetContentText;
    property RawContent: TBytes read GetRawContent write SetRawContent;
    property ContentStream: TStream read GetContentStream write SetContentStream;
    property ContentType: string read GetContentType write SetContentType;
    property ContentLength: Integer read GetContentLength write SetContentLength;
    property ContentVersion: string read GetContentVersion write SetContentVersion;
    property ContentEncoding: string read GetContentEncoding write SetContentEncoding;
    property Authorization: string read GetAuthorization write SetAuthorization;
    property Accept: string read GetAccept write SetAccept;
    property AcceptableMediaTypes: TMediaTypeList read GetAcceptableMediaTypes;
    property AcceptCharSet: string read GetAcceptCharSet write SetAcceptCharSet;
    property AcceptableCharSets: TAcceptCharsetList read GetAcceptableCharSets;
    property AcceptEncoding: string read GetAcceptEncoding write SetAcceptEncoding;
    property AcceptableEncodings: TAcceptEncodingList read GetAcceptableEncodings;
    property AcceptLanguage: string read GetAcceptLanguage write SetAcceptLanguage;
    property AcceptableLanguages: TAcceptLanguageList read GetAcceptableLanguages;
    property ContentMediaType: TMediaType read GetContentMediaType;
    property MultiPartFormData: TWiRLFormDataMultiPart read GetMultiPartFormData;
    property Application: TObject read FApplication write SetApplication;
    property Connection: TWiRLConnection read GetConnection;
  end;

implementation

{ TWiRLRequest }

destructor TWiRLRequest.Destroy;
begin
  FHeaderFields.Free;
  FContentMediaType.Free;
  FAcceptableCharSets.Free;
  FAcceptableLanguages.Free;
  FAcceptableMediaTypes.Free;
  FAcceptableEncodings.Free;
  FMultiPartFormData.Free;
  inherited;
end;

function TWiRLRequest.GetAccept: string;
begin
  Result := Headers.Values['Accept'];
end;

function TWiRLRequest.GetAcceptableCharSets: TAcceptCharsetList;
begin
  if not Assigned(FAcceptableCharSets) then
    FAcceptableCharSets := TAcceptCharsetList.Create;

  TAcceptHeaderParser<TAcceptCharset>.Parse(AcceptCharSet, FAcceptableCharSets);

  Result := FAcceptableCharSets;
end;

function TWiRLRequest.GetAcceptableEncodings: TAcceptEncodingList;
begin
  if not Assigned(FAcceptableEncodings) then
    FAcceptableEncodings := TAcceptEncodingList.Create;

  TAcceptHeaderParser<TAcceptEncoding>.Parse(AcceptEncoding, FAcceptableEncodings);

  Result := FAcceptableEncodings;
end;

function TWiRLRequest.GetAcceptableLanguages: TAcceptLanguageList;
begin
  if not Assigned(FAcceptableLanguages) then
    FAcceptableLanguages := TAcceptLanguageList.Create;

  TAcceptHeaderParser<TAcceptLanguage>.Parse(AcceptLanguage, FAcceptableLanguages);

  Result := FAcceptableLanguages;
end;

function TWiRLRequest.GetAcceptableMediaTypes: TMediaTypeList;
begin
  if not Assigned(FAcceptableMediaTypes) then
  begin
    FAcceptableMediaTypes := TMediaTypeList.Create;
    TAcceptHeaderParser<TMediaType>.Parse(Accept, FAcceptableMediaTypes);
  end;

  // Last resort: if a request doesn't have a MediaType force */*
  if FAcceptableMediaTypes.Empty then
    FAcceptableMediaTypes.Add(TMediaType.Create(TMediaType.WILDCARD));

  Result := FAcceptableMediaTypes;
end;

function TWiRLRequest.GetAcceptCharSet: string;
begin
  Result := Headers.AcceptCharSet;
end;

function TWiRLRequest.GetAcceptEncoding: string;
begin
  Result := Headers.AcceptEncoding;
end;

function TWiRLRequest.GetAcceptLanguage: string;
begin
  Result := Headers.AcceptLanguage;
end;

function TWiRLRequest.GetAuthorization: string;
begin
  Result := Headers.Authorization;
end;

function TWiRLRequest.GetContentText: string;
begin
  Result := EncodingFromCharSet(ContentMediaType.Charset).GetString(RawContent);
end;

function TWiRLRequest.GetContentEncoding: string;
begin
  Result := Headers.ContentEncoding;
end;

function TWiRLRequest.GetContentLength: Integer;
begin
  Result := Headers.ContentLength;
end;

function TWiRLRequest.GetContentMediaType: TMediaType;
begin
  if not Assigned(FContentMediaType) then
    FContentMediaType := TMediaType.Create(ContentType);
  Result := FContentMediaType;
end;

function TWiRLRequest.GetContentType: string;
begin
  Result := Headers.ContentType;
end;

function TWiRLRequest.GetContentVersion: string;
begin
  Result := Headers.Values['Content-Version'];
end;

function TWiRLRequest.GetFrom: string;
begin
  Result := Headers.Values['From'];
end;

function TWiRLRequest.HeaderFields: TWiRLRequestHeaderList;
begin
  if not Assigned(FHeaderFields) then
  begin
    FHeaderFields := TWiRLRequestHeaderList.Create(Self);
  end;
  Result := FHeaderFields;
end;

function TWiRLRequest.GetHost: string;
begin
  Result := Headers.Values['Host'];
end;

function TWiRLRequest.GetMultiPartFormData: TWiRLFormDataMultiPart;
begin
  if not Assigned(FMultiPartFormData) then
  begin
    FMultiPartFormData := TWiRLFormDataMultiPart.Create(ContentStream, ContentMediaType.Boundary);
  end;
  Result := FMultiPartFormData;
end;

function TWiRLRequest.GetPathInfo: string;
begin
  if FPathInfo <> '' then
    Result := FPathInfo
  else
    Result := GetHttpPathInfo;
end;

function TWiRLRequest.GetQuery: string;
begin
  if FQuery <> '' then
    Result := FQuery
  else
    Result := GetHttpQuery;
end;

function TWiRLRequest.GetRange: string;
begin
  Result := Headers.Values['Range'];
end;

function TWiRLRequest.GetRawContent: TBytes;
var
  LPos :Int64;
begin
  SetLength(Result, 0);
  if (GetContentStream <> nil) and (GetContentStream.Size > 0) then
  begin
    LPos := GetContentStream.Position;
    try
      GetContentStream.Position := 0;
      SetLength(Result, GetContentStream.Size);
      GetContentStream.ReadBuffer(Result[0], GetContentStream.Size);
    finally
      GetContentStream.Position := LPos;
    end;
  end;
end;

function TWiRLRequest.GetReferer: string;
begin
  Result := Headers.Values['Referer'];
end;

function TWiRLRequest.GetUserAgent: string;
begin
  Result := Headers.UserAgent;
end;

procedure TWiRLRequest.SetAccept(const Value: string);
begin
  Headers.Accept := Value;
end;

procedure TWiRLRequest.SetAcceptCharSet(const Value: string);
begin
  Headers.AcceptCharSet := Value;
end;

procedure TWiRLRequest.SetAcceptEncoding(const Value: string);
begin
  Headers.AcceptEncoding := Value;
end;

procedure TWiRLRequest.SetAcceptLanguage(const Value: string);
begin
  Headers.AcceptLanguage := Value;
end;

procedure TWiRLRequest.SetApplication(const Value: TObject);
begin
  FApplication := Value;
  QueryFields.Application := Value;
  ContentFields.Application := Value;
end;

procedure TWiRLRequest.SetAuthorization(const Value: string);
begin
  Headers.Authorization := Value;
end;

procedure TWiRLRequest.SetContentText(const Value: string);
var
  LStream: TStream;
begin
  LStream := TStringStream.Create(Value, EncodingFromCharSet((ContentMediaType.Charset)));
  try
    ContentStream := LStream;
  except
    LStream.Free;
  end;
end;

procedure TWiRLRequest.SetContentEncoding(const Value: string);
begin
  Headers.ContentEncoding := Value;
end;

procedure TWiRLRequest.SetContentLength(const Value: Integer);
begin
  Headers.ContentLength := Value;
end;

procedure TWiRLRequest.SetContentType(const Value: string);
begin
  Headers.ContentType := Value;
end;

procedure TWiRLRequest.SetContentVersion(const Value: string);
begin
  Headers.Values['Content-Version'] := Value;
end;

procedure TWiRLRequest.SetFrom(const Value: string);
begin
  Headers.Values['From'] := Value;
end;

procedure TWiRLRequest.SetHost(const Value: string);
begin
  Headers.Values['Host'] := Value;
end;

procedure TWiRLRequest.SetPathInfo(const Value: string);
begin
  FPathInfo := Value;
end;

procedure TWiRLRequest.SetQuery(const Value: string);
begin
  FQuery := Value;
end;

procedure TWiRLRequest.SetRange(const Value: string);
begin
  Headers.Values['Range'] := Value;
end;

procedure TWiRLRequest.SetRawContent(const Value: TBytes);
var
  LStream: TStream;
begin
  LStream := TBytesStream.Create(Value);
  ContentStream := LStream;
end;

procedure TWiRLRequest.SetReferer(const Value: string);
begin
  Headers.Values['Referer'] := Value;
end;

procedure TWiRLRequest.SetUserAgent(const Value: string);
begin
  Headers.UserAgent := Value;
end;

{ TWiRLRequestHeaderList }

constructor TWiRLRequestHeaderList.Create(ARequest: TWiRLRequest);
begin
  inherited Create;
  FRequest := ARequest;
end;

function TWiRLRequestHeaderList.GetValue(const AName: string): string;
begin
  Result := FRequest.Headers.Values[AName];
end;

procedure TWiRLRequestHeaderList.SetValue(const AName, AValue: string);
begin
  FRequest.Headers.Values[AName] := AValue;
end;

end.
