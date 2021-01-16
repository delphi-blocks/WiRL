{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.Response;

interface

uses
  System.SysUtils, System.Classes,

  WiRL.Http.Core,
  WiRL.http.Headers,
  WiRL.http.Cookie,
  WiRL.http.Accept.MediaType;

type
  TWiRLResponse = class;

  // deprecated
  TWiRLResponseHeaderList = class(TObject)
  private
    FResponse: TWiRLResponse;
    function GetValue(const AName: string): string;
    procedure SetValue(const AName, AValue: string);
  public
    property Values[const AName: string]: string read GetValue write SetValue; default;
    constructor Create(AResponse: TWiRLResponse);
  end;

  TWiRLResponse = class
  private
    FCookie: TWiRLCookies;
    FMediaType: TMediaType;
    FHasContentLength: Boolean;
    FHeaderFields: TWiRLResponseHeaderList;
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
    function GetHeaderFields: TWiRLResponseHeaderList;
  protected
    function IsUnknownResponseCode: Boolean; virtual;
    function GetHeaders: IWiRLHeaders; virtual; abstract;
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

    procedure FromWiRLStatus(AStatus: TWiRLHttpStatus);
    procedure Redirect(ACode: Integer; const ALocation: string);
    procedure SetNonStandardReasonString(const AValue: string);

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
    property Headers: IWiRLHeaders read GetHeaders;
    property HeaderFields: TWiRLResponseHeaderList read GetHeaderFields;
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
  FMediaType.Free;
  FCookie.Free;
  FHeaderFields.Free;
  inherited;
end;

procedure TWiRLResponse.FromWiRLStatus(AStatus: TWiRLHttpStatus);
begin
  StatusCode := AStatus.Code;

  if not AStatus.Reason.IsEmpty then
    ReasonString := AStatus.Reason;

  if not AStatus.Location.IsEmpty then
    Location := AStatus.Location;
end;

function TWiRLResponse.GetAllow: string;
begin
  Result := Headers.Allow;
end;

function TWiRLResponse.GetConnection: string;
begin
  Result := Headers.Connection;
end;

function TWiRLResponse.GetContentEncoding: string;
begin
  Result := Headers.ContentEncoding;
end;

function TWiRLResponse.GetContentLanguage: string;
begin
  Result := Headers.ContentLanguage;
end;

function TWiRLResponse.GetContentLength: Int64;
begin
  Result := Headers.ContentLength;
end;

function TWiRLResponse.GetContentMediaType: TMediaType;
begin
  if not Assigned(FMediaType) then
    FMediaType := TMediaType.Create(ContentType);
  Result := FMediaType;
end;

function TWiRLResponse.GetContentType: string;
begin
  Result := Headers.ContentType;
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
  LValue := Headers.Values['Date'];
  if LValue = '' then
    Result := Now
  else
    Result := GMTToLocalDateTime(LValue);
end;

function TWiRLResponse.GetExpires: TDateTime;
var
  LValue: string;
begin
  LValue := Headers.Values['Expires'];
  if LValue = '' then
    Result := 0
  else
    Result := GMTToLocalDateTime(LValue);
end;

function TWiRLResponse.GetHeaderFields: TWiRLResponseHeaderList;
begin
  if not Assigned(FHeaderFields) then
  begin
    FHeaderFields := TWiRLResponseHeaderList.Create(Self);
  end;
  Result := FHeaderFields;
end;

function TWiRLResponse.GetLastModified: TDateTime;
var
  LValue: string;
begin
  LValue := Headers.Values['Last-Modified'];
  if LValue = '' then
    Result := 0
  else
    Result := GMTToLocalDateTime(LValue);
end;

function TWiRLResponse.GetLocation: string;
begin
  Result := Headers.Location;
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
  Result := Headers.Values['Server'];
end;

function TWiRLResponse.IsUnknownResponseCode: Boolean;
begin
  Result := False;
end;

function TWiRLResponse.GetWWWAuthenticate: string;
begin
  Result := Headers.WWWAuthenticate;
end;

procedure TWiRLResponse.Redirect(ACode: Integer; const ALocation: string);
begin
  Assert((ACode >= 300) and (ACode < 400), 'Redirect code must be of 300 class');
  StatusCode := ACode;
  Location := ALocation;
end;

procedure TWiRLResponse.SetAllow(const Value: string);
begin
  Headers.Allow := Value;
end;

procedure TWiRLResponse.SetConnection(const Value: string);
begin
  Headers.Connection := Value;
end;

procedure TWiRLResponse.SetContentEncoding(const Value: string);
begin
  Headers.ContentEncoding := Value;
end;

procedure TWiRLResponse.SetContentLanguage(const Value: string);
begin
  Headers.ContentLanguage := Value;
end;

procedure TWiRLResponse.SetContentLength(const Value: Int64);
begin
  FHasContentLength := True;
  Headers.ContentLength := Value;
end;

procedure TWiRLResponse.SetContentType(const Value: string);
begin
  Headers.ContentType := Value;
end;

procedure TWiRLResponse.SetDate(const Value: TDateTime);
begin
  Headers.Values['Date'] := LocalDateTimeToHttpStr(Value);
end;

procedure TWiRLResponse.SetExpires(const Value: TDateTime);
begin
  if Value = 0 then
    Headers.Values['Expires'] := ''
  else
    Headers.Values['Expires'] := LocalDateTimeToHttpStr(Value);
end;

procedure TWiRLResponse.SetLastModified(const Value: TDateTime);
begin
  if Value = 0 then
    Headers.Values['Last-Modified'] := ''
  else
    Headers.Values['Last-Modified'] := LocalDateTimeToHttpStr(Value);
end;

procedure TWiRLResponse.SetLocation(const Value: string);
begin
  Headers.Location := Value;
end;

procedure TWiRLResponse.SetNonStandardReasonString(const AValue: string);
begin
  if (ReasonString = '') or IsUnknownResponseCode then
    ReasonString := Avalue;
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
  Headers.Values['Server'] := Value;
end;

procedure TWiRLResponse.SetWWWAuthenticate(const Value: string);
begin
  Headers.WWWAuthenticate := Value;
end;

{ TWiRLResponseHeaderList }

constructor TWiRLResponseHeaderList.Create(AResponse: TWiRLResponse);
begin
  inherited Create;
  FResponse := AResponse;
end;

function TWiRLResponseHeaderList.GetValue(const AName: string): string;
begin
  Result := FResponse.Headers.Values[AName];
end;

procedure TWiRLResponseHeaderList.SetValue(const AName, AValue: string);
begin
  FResponse.Headers.Values[AName] := AValue;
end;

end.
