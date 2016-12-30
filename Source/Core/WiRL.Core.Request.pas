{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Request;

interface

uses
  System.Classes, System.SysUtils,

  WiRL.Http.Accept.Parser,
  WiRL.Http.Accept.Charset,
  WiRL.Http.Accept.Encoding,
  WiRL.http.Accept.MediaType,
  WiRL.http.Accept.Language;


type
  TWiRLMethod = class
  const
    GET = 'GET';
    PUT = 'PUT';
    POST = 'POST';
    HEAD = 'HEAD';
    DELETE = 'DELETE';
    PATCH = 'PATCH';
    OPTIONS = 'OPTIONS';
    TRACE = 'TRACE';
    CONNECT = 'CONNECT';
  end;

  TWiRLRequest = class
  private
    FContentMediaType: TMediaType;
    FAcceptableCharSets: TAcceptCharsetList;
    FAcceptableEncodings: TAcceptEncodingList;
    FAcceptableLanguages: TAcceptLanguageList;
    FAcceptableMediaTypes: TMediaTypeList;
    function GetAcceptableMediaTypes: TMediaTypeList;
    function GetContent: string;
    function GetRawContent: TBytes;
    function GetContentMediaType: TMediaType;
    function GetAcceptableCharSets: TAcceptCharsetList;
    function GetAcceptableLanguages: TAcceptLanguageList;
    function GetAcceptableEncodings: TAcceptEncodingList;
  protected
    function GetPathInfo: string; virtual; abstract;
    function GetQuery: string; virtual; abstract;
    function GetHost: string; virtual; abstract;
    function GetServerPort: Integer; virtual; abstract;
    function GetMethod: string; virtual; abstract;
    function GetQueryFields: TStrings; virtual; abstract;
    function GetContentFields: TStrings; virtual; abstract;
    function GetCookieFields: TStrings; virtual; abstract;
    function GetContentStream: TStream; virtual; abstract;
    function GetAuthorization: string; virtual; abstract;
    function GetAccept: string; virtual; abstract;
    function GetAcceptCharSet: string; virtual; abstract;
    function GetAcceptEncoding: string; virtual; abstract;
    function GetAcceptLanguage: string; virtual; abstract;
    function GetContentType: string; virtual; abstract;
    function GetContentLength: Integer; virtual; abstract;
    function GetContentVersion: string; virtual; abstract;
    function GetRawPathInfo: string; virtual; abstract;
    function DoGetFieldByName(const Name: string): string; virtual; abstract;
  public
    destructor Destroy; override;

    property PathInfo: string read GetPathInfo;
    property Query: string read GetQuery;
    property Method: string read GetMethod;
    property Host: string read GetHost;
    property ServerPort: Integer read GetServerPort;
    property QueryFields: TStrings read GetQueryFields;
    property ContentFields: TStrings read GetContentFields;
    property CookieFields: TStrings read GetCookieFields;
    property Content: string read GetContent;
    property RawContent: TBytes read GetRawContent;
    property ContentStream: TStream read GetContentStream;
    property ContentType: string read GetContentType;
    property ContentLength: Integer read GetContentLength;
    property ContentVersion: string read GetContentVersion;
    property Authorization: string read GetAuthorization;
    property Accept: string read GetAccept;
    property AcceptableMediaTypes: TMediaTypeList read GetAcceptableMediaTypes;
    property AcceptCharSet: string read GetAcceptCharSet;
    property AcceptableCharSets: TAcceptCharsetList read GetAcceptableCharSets;
    property AcceptEncoding: string read GetAcceptEncoding;
    property AcceptableEncodings: TAcceptEncodingList read GetAcceptableEncodings;
    property AcceptLanguage: string read GetAcceptLanguage;
    property AcceptableLanguages: TAcceptLanguageList read GetAcceptableLanguages;
    property RawPathInfo: string read GetRawPathInfo;
    property ContentMediaType: TMediaType read GetContentMediaType;

    function GetFieldByName(const Name: string): string;
  end;

var
  GetDefaultCharSetEncoding: TEncoding = nil;

implementation

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

{ TWiRLRequest }

destructor TWiRLRequest.Destroy;
begin
  FContentMediaType.Free;
  FAcceptableCharSets.Free;
  FAcceptableLanguages.Free;
  FAcceptableMediaTypes.Free;

  inherited;
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
    FAcceptableMediaTypes := TMediaTypeList.Create;

  TAcceptHeaderParser<TMediaType>.Parse(Accept, FAcceptableMediaTypes);

  Result := FAcceptableMediaTypes;
end;

function TWiRLRequest.GetContent: string;
begin
  Result := EncodingGetString(ContentType, RawContent);
end;

function TWiRLRequest.GetFieldByName(const Name: string): string;
begin
  Result := DoGetFieldByName(Name);
end;

function TWiRLRequest.GetContentMediaType: TMediaType;
begin
  if not Assigned(FContentMediaType) then
    FContentMediaType := TMediaType.Create(ContentType);
  Result := FContentMediaType;
end;

function TWiRLRequest.GetRawContent: TBytes;
var
  LPos :Int64;
begin
  if (GetContentStream <> nil) and (GetContentStream.Size > 0) then
  begin
    LPos := GetContentStream.Position;
    try
      SetLength(Result, GetContentStream.Size);
      GetContentStream.ReadBuffer(Result[0], GetContentStream.Size);
    finally
      GetContentStream.Position := LPos;
    end;
  end;
end;

end.
