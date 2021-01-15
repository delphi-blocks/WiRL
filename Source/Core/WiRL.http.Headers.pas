{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.Headers;

interface

uses
  System.Classes, System.SysUtils;

type
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
    const ALLOW = 'Allow';
    const CONNECTION = 'Connection';
    const CONTENT_ENCODING = 'Content-Encoding';
    const CONTENT_LANGUAGE = 'Content-Language';
    const LOCATION = 'Location';
    const WWW_AUTHENTICATE = 'WWW-Authenticate';
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
    function GetAllow: string;
    function GetConnection: string;
    function GetContentEncoding: string;
    function GetContentLanguage: string;
    function GetContentLength: Int64;
    function GetLocation: string;
    function GetWWWAuthenticate: string;
    procedure SetAccept(const AValue: string);
    procedure SetAcceptCharSet(const AValue: string);
    procedure SetAcceptEncoding(const AValue: string);
    procedure SetAcceptLanguage(const AValue: string);
    procedure SetAuthorization(const AValue: string);
    procedure SetContentType(const AValue: string);
    procedure SetUserAgent(const AValue: string);
    procedure SetAllow(const AValue: string);
    procedure SetConnection(const AValue: string);
    procedure SetContentEncoding(const AValue: string);
    procedure SetContentLanguage(const AValue: string);
    procedure SetContentLength(const AValue: Int64);
    procedure SetLocation(const AValue: string);
    procedure SetWWWAuthenticate(const AValue: string);
  public
    procedure Clear;
    property Values[const AName: string]: string read GetValue write SetValue;

    /// <summary>Media type(s) that is/are acceptable for the response</summary>
    property Accept: string read GetAccept write SetAccept;
    /// <summary>Character sets that are acceptable</summary>
    property AcceptCharSet: string read GetAcceptCharSet write SetAcceptCharSet;
    /// <summary>List of acceptable encodings</summary>
    property AcceptEncoding: string read GetAcceptEncoding write SetAcceptEncoding;
    /// <summary>List of acceptable human languages for response</summary>
    property AcceptLanguage: string read GetAcceptLanguage write SetAcceptLanguage;
    /// <summary>The user agent string of the user agent</summary>
    property UserAgent: string read GetUserAgent write SetUserAgent;
    /// <summary>The MIME type of this content</summary>
    property ContentType: string read GetContentType write SetContentType;
    /// <summary>Authentication credentials for HTTP authentication</summary>
    property Authorization: string read GetAuthorization write SetAuthorization;
    /// <summary>Valid methods for a specified resource. To be used for a 405 Method not allowed</summary>
    property Allow: string read GetAllow write SetAllow;
    /// <summary>Control options for the current connection and list of hop-by-hop response fields</summary>
    property Connection: string read GetConnection write SetConnection;
    /// <summary>The type of encoding used on the data</summary>
    property ContentEncoding: string read GetContentEncoding write SetContentEncoding;
    /// <summary>The natural language or languages of the intended audience for the enclosed content</summary>
    property ContentLanguage: string read GetContentLanguage write SetContentLanguage;
    /// <summary>The length of the response body in octets (8-bit bytes)</summary>
    property ContentLength: Int64 read GetContentLength write SetContentLength;
    /// <summary>Used in redirection, or when a new resource has been created</summary>
    property Location: string read GetLocation write SetLocation;
    /// <summary>Indicates the authentication scheme that should be used to access the requested entity</summary>
    property WWWAuthenticate: string read GetWWWAuthenticate write SetWWWAuthenticate;
  end;

implementation

{ TWiRLHeader }

constructor TWiRLHeader.Create(const AName, AValue: string);
begin
  Name := AName;
  Value := AValue;
end;

{ TWiRLHeadersHelper }

procedure TWiRLHeadersHelper.Clear;
begin
  SetLength(Self, 0);
end;

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

function TWiRLHeadersHelper.GetAllow: string;
begin
  Result := GetValue(TWiRLHeader.ALLOW);
end;

function TWiRLHeadersHelper.GetAuthorization: string;
begin
  Result := GetValue(TWiRLHeader.AUTHORIZATION);
end;

function TWiRLHeadersHelper.GetConnection: string;
begin
  Result := GetValue(TWiRLHeader.CONNECTION);
end;

function TWiRLHeadersHelper.GetContentEncoding: string;
begin
  Result := GetValue(TWiRLHeader.CONTENT_ENCODING);
end;

function TWiRLHeadersHelper.GetContentLanguage: string;
begin
  Result := GetValue(TWiRLHeader.CONTENT_LANGUAGE);
end;

function TWiRLHeadersHelper.GetContentLength: Int64;
begin
  Result := StrToInt64Def(GetValue(TWiRLHeader.CONTENT_LENGTH), -1);
end;

function TWiRLHeadersHelper.GetContentType: string;
begin
  Result := GetValue(TWiRLHeader.CONTENT_TYPE);
end;

function TWiRLHeadersHelper.GetLocation: string;
begin
  Result := GetValue(TWiRLHeader.LOCATION);
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

function TWiRLHeadersHelper.GetWWWAuthenticate: string;
begin
  Result := GetValue(TWiRLHeader.WWW_AUTHENTICATE);
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

procedure TWiRLHeadersHelper.SetAllow(const AValue: string);
begin
  SetValue(TWiRLHeader.ALLOW, AValue);
end;

procedure TWiRLHeadersHelper.SetAuthorization(const AValue: string);
begin
  SetValue(TWiRLHeader.AUTHORIZATION, AValue);
end;

procedure TWiRLHeadersHelper.SetConnection(const AValue: string);
begin
  SetValue(TWiRLHeader.CONNECTION, AValue);
end;

procedure TWiRLHeadersHelper.SetContentEncoding(const AValue: string);
begin
  SetValue(TWiRLHeader.CONTENT_ENCODING, AValue);
end;

procedure TWiRLHeadersHelper.SetContentLanguage(const AValue: string);
begin
  SetValue(TWiRLHeader.CONTENT_LANGUAGE, AValue);
end;

procedure TWiRLHeadersHelper.SetContentLength(const AValue: Int64);
begin
  SetValue(TWiRLHeader.CONTENT_LANGUAGE, IntToStr(AValue));
end;

procedure TWiRLHeadersHelper.SetContentType(const AValue: string);
begin
  SetValue(TWiRLHeader.CONTENT_TYPE, AValue);
end;

procedure TWiRLHeadersHelper.SetLocation(const AValue: string);
begin
  SetValue(TWiRLHeader.LOCATION, AValue);
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

procedure TWiRLHeadersHelper.SetWWWAuthenticate(const AValue: string);
begin
  SetValue(TWiRLHeader.WWW_AUTHENTICATE, AValue);
end;

end.
