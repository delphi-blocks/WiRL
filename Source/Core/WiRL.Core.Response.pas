{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Response;

interface

uses
  System.SysUtils, System.Classes,

  WiRL.Http.Core,
  WiRL.http.Accept.MediaType;

type
  TWiRLResponse = class
  private
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
    property ContentStream: TStream read GetContentStream write SetContentStream;
    property StatusCode: Integer read GetStatusCode write SetStatusCode;
    property ReasonString: string read GetReasonString write SetReasonString;
    property ContentType: string read GetContentType write SetContentType;
    property ContentLength: Int64 read GetContentLength write SetContentLength;
    property HeaderFields: TWiRLHeaderList read GetHeaderFields;
    property ContentMediaType: TMediaType read GetContentMediaType;
    property Connection: string read GetConnection write SetConnection;
    property RawContent: TBytes read GetRawContent write SetRawContent;


  {
    procedure SendRedirect(const URI: string); virtual; abstract;
    procedure SendStream(AStream: TStream); virtual; abstract;
    function Sent: Boolean; virtual;
    procedure SetCookieField(Values: TStrings; const ADomain, APath: string;
      AExpires: TDateTime; ASecure: Boolean);
    procedure SetCustomHeader(const Name, Value: string);
    property Cookies: TCookieCollection read FCookies;
    property HTTPRequest: TWebRequest read FHTTPRequest;
    property Version: string index 0 read GetStringVariable write SetStringVariable;
    property ReasonString: string index 1 read GetStringVariable write SetStringVariable;
    property Server: string index 2 read GetStringVariable write SetStringVariable;
    property WWWAuthenticate: string index 3 read GetStringVariable write SetStringVariable;
    property Realm: string index 4 read GetStringVariable write SetStringVariable;
    property Allow: string index 5 read GetStringVariable write SetStringVariable;
    property Location: string index 6 read GetStringVariable write SetStringVariable;
    property ContentEncoding: string index 7 read GetStringVariable write SetStringVariable;
    property ContentType: string index 8 read GetStringVariable write SetStringVariable;
    property ContentVersion: string index 9 read GetStringVariable write SetStringVariable;
    property DerivedFrom: string index 10 read GetStringVariable write SetStringVariable;
    property Title: string index 11 read GetStringVariable write SetStringVariable;

    property StatusCode: Integer read GetStatusCode write SetStatusCode;
    property ContentLength: Integer index 0 read GetIntegerVariable write SetIntegerVariable;



    property LogMessage: string read GetLogMessage write SetLogMessage;

   }
  end;


implementation

uses
  IdGlobal, IdGlobalProtocols;

{ TWiRLResponse }

destructor TWiRLResponse.Destroy;
begin
  FHeaderFields.Free;
  FMediaType.Free;
  inherited;
end;

function TWiRLResponse.GetConnection: string;
begin
  Result := HeaderFields.Values['Connection'];
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

function TWiRLResponse.GetRawContent: TBytes;
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

procedure TWiRLResponse.SetConnection(const Value: string);
begin
  HeaderFields.Values['Connection'] := Value;
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

procedure TWiRLResponse.SetRawContent(const Value: TBytes);
var
  LStream: TStream;
begin
  LStream := TBytesStream.Create(Value);
  ContentStream := LStream;
end;

end.
