{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.Response;

interface

uses
  System.SysUtils, System.Classes,

  WiRL.Http.Core,
  WiRL.http.Cookie,
  WiRL.http.Accept.MediaType;

type
  TWiRLResponse = class
  private
    FCookie: TWiRLCookies;
    FMediaType: TMediaType;
    FHeaderFields :TWiRLHeaderList;
    FHasContentLength: Boolean;
    function GetContentType: string;
    procedure SetContentType(const Value: string);
    function GetDate: TDateTime;
    procedure SetDate(const Value: TDateTime);
    function GetExpires: TDateTime;
    procedure SetExpires(const Value: TDateTime);
    function GetLastModified: TDateTime;
    procedure SetLastModified(const Value: TDateTime);
    function GetContentMediaType: TMediaType;
    function GetContentLength: Int64;
    procedure SetContentLength(const Value: Int64);
    function GetConnection: string;
    procedure SetConnection(const Value: string);
    function GetRawContent: TBytes;
    procedure SetRawContent(const Value: TBytes);
    function GetContentEncoding: string;
    procedure SetContentEncoding(const Value: string);
    function GetAllow: string;
    procedure SetAllow(const Value: string);
    function GetServer: string;
    procedure SetServer(const Value: string);
    function GetWWWAuthenticate: string;
    procedure SetWWWAuthenticate(const Value: string);
    function GetLocation: string;
    procedure SetLocation(const Value: string);
    function GetContentLanguage: string;
    procedure SetContentLanguage(const Value: string);
    function GetCookies: TWiRLCookies;
  protected
    function GetHeaderFields: TWiRLHeaderList;
    function GetContent: string; virtual; abstract;
    function GetContentStream: TStream; virtual; abstract;
    procedure SetContent(const Value: string); virtual; abstract;
    procedure SetContentStream(const Value: TStream); virtual; abstract;
    function GetStatusCode: Integer; virtual; abstract;
    procedure SetStatusCode(const Value: Integer); virtual; abstract;
    function GetReasonString: string; virtual; abstract;
    procedure SetReasonString(const Value: string); virtual; abstract;
  public
    procedure SendHeaders; virtual; abstract;
    destructor Destroy; override;

    property HasContentLength: Boolean read FHasContentLength;
    property Date: TDateTime read GetDate write SetDate;
    property Expires: TDateTime read GetExpires write SetExpires;
    property LastModified: TDateTime read GetLastModified write SetLastModified;
    property Content: string read GetContent write SetContent;
    property ContentEncoding: string read GetContentEncoding write SetContentEncoding;
    property ContentLanguage: string read GetContentLanguage write SetContentLanguage;
    property ContentStream: TStream read GetContentStream write SetContentStream;
    property StatusCode: Integer read GetStatusCode write SetStatusCode;
    property ReasonString: string read GetReasonString write SetReasonString;
    property ContentType: string read GetContentType write SetContentType;
    property ContentLength: Int64 read GetContentLength write SetContentLength;
    property HeaderFields: TWiRLHeaderList read GetHeaderFields;
    property ContentMediaType: TMediaType read GetContentMediaType;
    property Connection: string read GetConnection write SetConnection;
    property RawContent: TBytes read GetRawContent write SetRawContent;
    property Allow: string read GetAllow write SetAllow;
    property Server: string read GetServer write SetServer;
    property WWWAuthenticate: string read GetWWWAuthenticate write SetWWWAuthenticate;
    property Location: string read GetLocation write SetLocation;
    property Cookies: TWiRLCookies read GetCookies;
  end;


implementation

uses
  IdGlobal, IdGlobalProtocols;

{ TWiRLResponse }

destructor TWiRLResponse.Destroy;
begin
  FHeaderFields.Free;
  FMediaType.Free;
  FCookie.Free;
  inherited;
end;

function TWiRLResponse.GetAllow: string;
begin
  Result := HeaderFields.Values['Allow'];
end;

function TWiRLResponse.GetConnection: string;
begin
  Result := HeaderFields.Values['Connection'];
end;

function TWiRLResponse.GetContentEncoding: string;
begin
  Result := HeaderFields.Values['Content-Encoding'];
end;

function TWiRLResponse.GetContentLanguage: string;
begin
  Result := HeaderFields.Values['Content-Language'];
end;

function TWiRLResponse.GetContentLength: Int64;
begin
  Result := StrToInt64Def(HeaderFields.Values['Content-Length'], -1);
end;

function TWiRLResponse.GetContentMediaType: TMediaType;
begin
  if not Assigned(FMediaType) then
    FMediaType := TMediaType.Create(ContentType);
  Result := FMediaType;
end;

function TWiRLResponse.GetContentType: string;
begin
  Result := HeaderFields.Values['Content-Type'];
end;

function TWiRLResponse.GetCookies: TWiRLCookies;
begin
  if not Assigned(FCookie) then
    FCookie := TWiRLCookies.Create;
  Result := FCookie;
end;

function TWiRLResponse.GetDate: TDateTime;
var
  LValue: string;
begin
  LValue := HeaderFields.Values['Date'];
  if LValue = '' then
    Result := Now
  else
    Result := GMTToLocalDateTime(LValue);
end;

function TWiRLResponse.GetExpires: TDateTime;
var
  LValue: string;
begin
  LValue := HeaderFields.Values['Expires'];
  if LValue = '' then
    Result := 0
  else
    Result := GMTToLocalDateTime(LValue);
end;

function TWiRLResponse.GetHeaderFields: TWiRLHeaderList;
begin
  if not Assigned(FHeaderFields) then
    FHeaderFields := TWiRLHeaderList.Create;
  Result := FHeaderFields;
end;

function TWiRLResponse.GetLastModified: TDateTime;
var
  LValue: string;
begin
  LValue := HeaderFields.Values['Last-Modified'];
  if LValue = '' then
    Result := 0
  else
    Result := GMTToLocalDateTime(LValue);
end;

function TWiRLResponse.GetLocation: string;
begin
  Result := HeaderFields.Values['Location'];
end;

function TWiRLResponse.GetRawContent: TBytes;
begin
  if (GetContentStream <> nil) and (GetContentStream.Size > 0) then
  begin
    GetContentStream.Position := 0;
    SetLength(Result, GetContentStream.Size);
    GetContentStream.ReadBuffer(Result[0], GetContentStream.Size);
  end;
end;

function TWiRLResponse.GetServer: string;
begin
  Result := HeaderFields.Values['Server'];
end;

function TWiRLResponse.GetWWWAuthenticate: string;
begin
  Result := HeaderFields.Values['WWW-Authenticate'];
end;

procedure TWiRLResponse.SetAllow(const Value: string);
begin
  HeaderFields.Values['Allow'] := Value;
end;

procedure TWiRLResponse.SetConnection(const Value: string);
begin
  HeaderFields.Values['Connection'] := Value;
end;

procedure TWiRLResponse.SetContentEncoding(const Value: string);
begin
  HeaderFields.Values['Content-Encoding'] := Value;
end;

procedure TWiRLResponse.SetContentLanguage(const Value: string);
begin
  HeaderFields.Values['Content-Language'] := Value;
end;

procedure TWiRLResponse.SetContentLength(const Value: Int64);
begin
  FHasContentLength := True;
  HeaderFields.Values['Content-Length'] := IntToStr(Value);
end;

procedure TWiRLResponse.SetContentType(const Value: string);
begin
  HeaderFields.Values['Content-Type'] := Value;
end;

procedure TWiRLResponse.SetDate(const Value: TDateTime);
begin
  HeaderFields.Values['Date'] := LocalDateTimeToHttpStr(Value);
end;

procedure TWiRLResponse.SetExpires(const Value: TDateTime);
begin
  if Value = 0 then
    HeaderFields.Values['Expires'] := ''
  else
    HeaderFields.Values['Expires'] := LocalDateTimeToHttpStr(Value);
end;

procedure TWiRLResponse.SetLastModified(const Value: TDateTime);
begin
  if Value = 0 then
    HeaderFields.Values['Last-Modified'] := ''
  else
    HeaderFields.Values['Last-Modified'] := LocalDateTimeToHttpStr(Value);
end;

procedure TWiRLResponse.SetLocation(const Value: string);
begin
  HeaderFields.Values['Location'] := Value;
end;

procedure TWiRLResponse.SetRawContent(const Value: TBytes);
var
  LStream: TStream;
begin
  LStream := TBytesStream.Create(Value);
  ContentStream := LStream;
end;

procedure TWiRLResponse.SetServer(const Value: string);
begin
  HeaderFields.Values['Server'] := Value;
end;

procedure TWiRLResponse.SetWWWAuthenticate(const Value: string);
begin
  HeaderFields.Values['WWW-Authenticate'] := Value;
end;

end.
