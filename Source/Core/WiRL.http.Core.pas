{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2023 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.Core;

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.Classes, System.TypInfo,
  WiRL.Core.Converter;

type
  TWiRLHttpMethod = (GET, HEAD, POST, PUT, PATCH, DELETE, OPTIONS, TRACE, CONNECT);

  TWiRLResponseStatus = (
    None,            // Undefined
    Informational,   // 1xx
    Success,         // 2xx
    Redirect,        // 3xx
    ClientError,     // 4xx
    ServerError      // 5xx
  );

  TWiRLHttpMethodHelper = record helper for TWiRLHttpMethod
  public
    class function ConvertFromString(const AMethod: string): TWiRLHttpMethod; static;
  public
    function ToString: string;
    procedure FromString(const AMethod: string);
  end;

  TWiRLResponseStatusHelper = record helper for TWiRLResponseStatus
  public
    class function FromStatusCode(AStatusCode: Integer): TWiRLResponseStatus; static;
  public
    function ToString: string;
  end;

  TWiRLHttpStatus = class
  public const
    // 1xx Informational
    CONTINUE_REQUEST                = 100;
    SWITCHING_PROTOCOLS             = 101;
    PROCESSING                      = 102;
    CHECKPOINT                      = 103;

    // 2xx Success
    OK                              = 200;
    CREATED                         = 201;
    ACCEPTED                        = 202;
    NON_AUTHORITATIVE_INFORMATION   = 203;
    NO_CONTENT                      = 204;
    RESET_CONTENT                   = 205;
    PARTIAL_CONTENT                 = 206;
    MULTI_STATUS                    = 207;
    ALREADY_REPORTED                = 208;
    IM_USED                         = 226;

    // 3xx Redirection
    MULTIPLE_CHOICES                = 300;
    MOVED_PERMANENTLY               = 301;
    MOVED_TEMPORARILY               = 302;  // Deprecated
    FOUND                           = 302;
    SEE_OTHER                       = 303;
    NOT_MODIFIED                    = 304;
    USE_PROXY                       = 305;
    TEMPORARY_REDIRECT              = 307;
    PERMANENT_REDIRECT              = 308;

    // --- 4xx Client Error ---
    BAD_REQUEST                     = 400;
    UNAUTHORIZED                    = 401;
    PAYMENT_REQUIRED                = 402;
    FORBIDDEN                       = 403;
    NOT_FOUND                       = 404;
    METHOD_NOT_ALLOWED              = 405;
    NOT_ACCEPTABLE                  = 406;
    PROXY_AUTHENTICATION_REQUIRED   = 407;
    REQUEST_TIMEOUT                 = 408;
    CONFLICT                        = 409;
    GONE                            = 410;
    LENGTH_REQUIRED                 = 411;
    PRECONDITION_FAILED             = 412;
    PAYLOAD_TOO_LARGE               = 413;
    REQUEST_ENTITY_TOO_LARGE        = 413;
    URI_TOO_LONG                    = 414;
    REQUEST_URI_TOO_LONG            = 414;
    UNSUPPORTED_MEDIA_TYPE          = 415;
    REQUESTED_RANGE_NOT_SATISFIABLE = 416;
    EXPECTATION_FAILED              = 417;
    I_AM_A_TEAPOT                   = 418;
    INSUFFICIENT_SPACE_ON_RESOURCE  = 419;
    METHOD_FAILURE                  = 420;
    DESTINATION_LOCKED              = 421;
    UNPROCESSABLE_ENTITY            = 422;
    LOCKED                          = 423;
    FAILED_DEPENDENCY               = 424;
    UPGRADE_REQUIRED                = 426;
    PRECONDITION_REQUIRED           = 428;
    TOO_MANY_REQUESTS               = 429;
    REQUEST_HEADER_FIELDS_TOO_LARGE = 431;
    UNAVAILABLE_FOR_LEGAL_REASONS   = 451;

    // --- 5xx Server Error ---
    INTERNAL_SERVER_ERROR           = 500;
    NOT_IMPLEMENTED                 = 501;
    BAD_GATEWAY                     = 502;
    SERVICE_UNAVAILABLE             = 503;
    GATEWAY_TIMEOUT                 = 504;
    HTTP_VERSION_NOT_SUPPORTED      = 505;
    VARIANT_ALSO_NEGOTIATES         = 506;
    INSUFFICIENT_STORAGE            = 507;
    LOOP_DETECTED                   = 508;
    BANDWIDTH_LIMIT_EXCEEDED        = 509;
    NOT_EXTENDED                    = 510;
    NETWORK_AUTHENTICATION_REQUIRED = 511;
  private
    FCode: Integer;
    FLocation: string;
    FReason: string;
  public
    constructor Create; overload;
    constructor Create(ACode: Integer); overload;
    constructor Create(ACode: Integer; const AReason: string); overload;
    constructor Create(ACode: Integer; const AReason, ALocation: string); overload;

    property Code: Integer read FCode write FCode;
    property Reason: string read FReason write FReason;
    property Location: string read FLocation write FLocation;
  end;

  TWiRLParam = class(TStringList)
  private
    FApplication: TObject;
    function GetValue(const AName: string): string;
    procedure SetValue(const AName, AValue: string);
    procedure SetApplication(const Value: TObject);
    function GetFormatSetting(TypeInfo: PTypeInfo): string;
  public
    function AsType<T>(const AName: string; const AFormat: string): T; overload;
    function AsType<T>(const AName: string): T; overload;
    procedure From<T>(const AName: string; const AValue: T; const AFormat: string); overload;
    procedure From<T>(const AName: string; const AValue: T); overload;

    property Values[const AName: string]: string read GetValue write SetValue; default;
    property Application: TObject read FApplication write SetApplication;
  end;

  TWiRLConnection = class(TObject)
  public
    procedure Write(AValue: TBytes; const ALength: Integer = -1; const AOffset: Integer = 0); overload; virtual; abstract;
    procedure Write(const AValue: string; AEncoding: TEncoding = nil); overload; virtual; abstract;
    procedure WriteLn(const AValue: string); overload; virtual; abstract;
    procedure WriteLn(); overload; virtual; abstract;
    function Connected: Boolean; virtual; abstract;
  end;

var
  GetDefaultCharSetEncoding: TEncoding = nil;

function EncodingFromCharSet(const ACharset: string): TEncoding;
function ContentStreamToString(const ACharset: string; AContentStream: TStream): string;

function DateTimeToHTTPDate(ADateTime: TDateTime): string;

implementation

uses
  WiRL.Core.Application;

function DateTimeToHTTPDate(ADateTime: TDateTime): string;
const
  HTTPDateFormat = 'ddd, dd mmm yyyy hh:nn:ss "GMT"';
var
  FS: TFormatSettings;
begin
  FS := TFormatSettings.Create('en-US');
  Result := FormatDateTime(HTTPDateFormat, ADateTime, FS);
end;

function DefaultCharSetEncoding: TEncoding;
begin
  Result := nil;
  if Assigned(GetDefaultCharSetEncoding) then
    Result := GetDefaultCharSetEncoding;
  if Result = nil then
    Result := TEncoding.UTF8;
end;

function ContentStreamToString(const ACharset: string; AContentStream: TStream): string;
var
  LEncoding: TEncoding;
  LBuffer: TBytes;
  LPos: Int64;
begin
  Result := '';
  if Assigned(AContentStream) and (AContentStream.Size > 0) then
  begin
    LPos := AContentStream.Position;
    try
      LEncoding := EncodingFromCharSet(ACharset);
      AContentStream.Position := 0;
      SetLength(LBuffer, AContentStream.Size);
      AContentStream.Read(LBuffer[0], AContentStream.Size);
      Result := LEncoding.GetString(LBuffer);
    finally
      AContentStream.Position := LPos;
    end;
  end;
end;

function EncodingFromCharSet(const ACharset: string): TEncoding;
begin
  if CompareText('utf-8', ACharset) = 0 then
    Result := TEncoding.UTF8
  else if CompareText('ISO-8859-1', ACharset) = 0 then
    Result := TEncoding.ANSI
  else if CompareText('ANSI', ACharset) = 0 then
    Result := TEncoding.ANSI
  else if CompareText('ASCII', ACharset) = 0 then
    Result := TEncoding.ASCII
  else
    Result := DefaultCharSetEncoding;
end;

{ TWiRLParam }

function TWiRLParam.AsType<T>(const AName: string; const AFormat: string): T;
begin
  Result := TWiRLConvert.AsType<T>(Values[AName], AFormat);
end;

procedure TWiRLParam.From<T>(const AName: string; const AValue: T; const AFormat: string);
begin
  Values[AName] := TWiRLConvert.From(AValue, AFormat);
end;

procedure TWiRLParam.From<T>(const AName: string; const AValue: T);
begin
  From<T>(AName, AValue, GetFormatSetting(TypeInfo(T)));
end;

function TWiRLParam.GetFormatSetting(TypeInfo: PTypeInfo): string;
begin
  if Assigned(FApplication) then
    Result := (FApplication as TWiRLApplication).GetFormatSettingFor(TypeInfo)
  else
    Result := TWiRLFormatSetting.DEFAULT;
end;

function TWiRLParam.GetValue(const AName: string): string;
begin
  Result := inherited Values[AName];
end;

procedure TWiRLParam.SetApplication(const Value: TObject);
begin
  FApplication := Value;
end;

procedure TWiRLParam.SetValue(const AName, AValue: string);
begin
  inherited Values[AName] := AValue;
end;

function TWiRLParam.AsType<T>(const AName: string): T;
begin
  Result := AsType<T>(AName, GetFormatSetting(TypeInfo(T)));
end;

{ TWiRLHttpMethodHelper }

procedure TWiRLHttpMethodHelper.FromString(const AMethod: string);
begin
  Self := ConvertFromString(AMethod);
end;

class function TWiRLHttpMethodHelper.ConvertFromString(const AMethod: string): TWiRLHttpMethod;
var
  LRes: Integer;
begin
  LRes := GetEnumValue(TypeInfo(TWiRLHttpMethod), AMethod);
  if LRes >= 0 then
    Result := TWiRLHttpMethod(LRes)
  else
    raise EWiRLConvertError.Create('Error converting string type');
end;

function TWiRLHttpMethodHelper.ToString: string;
begin
  case Self of
    TWiRLHttpMethod.GET:     Result := 'GET';
    TWiRLHttpMethod.HEAD:    Result := 'HEAD';
    TWiRLHttpMethod.POST:    Result := 'POST';
    TWiRLHttpMethod.PUT:     Result := 'PUT';
    TWiRLHttpMethod.PATCH:   Result := 'PATCH';
    TWiRLHttpMethod.DELETE:  Result := 'DELETE';
    TWiRLHttpMethod.OPTIONS: Result := 'OPTIONS';
    TWiRLHttpMethod.TRACE:   Result := 'TRACE';
    TWiRLHttpMethod.CONNECT: Result := 'CONNECT';
  end;
end;

{ TWiRLHttpStatus }

constructor TWiRLHttpStatus.Create(ACode: Integer);
begin
  Create(ACode, '', '');
end;

constructor TWiRLHttpStatus.Create(ACode: Integer; const AReason: string);
begin
  Create(ACode, AReason, '');
end;

constructor TWiRLHttpStatus.Create(ACode: Integer; const AReason, ALocation: string);
begin
  FCode := ACode;
  FReason := AReason;
  FLocation := ALocation;
end;

constructor TWiRLHttpStatus.Create;
begin
  Create(200, '', '');
end;

{ TWiRLResponseStatusHelper }

class function TWiRLResponseStatusHelper.FromStatusCode(
  AStatusCode: Integer): TWiRLResponseStatus;
begin
  if (AStatusCode >= 100) and (AStatusCode < 200) then
    Result := TWiRLResponseStatus.Informational
  else if AStatusCode < 300 then
    Result := TWiRLResponseStatus.Success
  else if AStatusCode < 400 then
    Result := TWiRLResponseStatus.Redirect
  else if AStatusCode < 500 then
    Result := TWiRLResponseStatus.ClientError
  else if AStatusCode < 600 then
    Result := TWiRLResponseStatus.ServerError
  else
    Result := TWiRLResponseStatus.None;
end;

function TWiRLResponseStatusHelper.ToString: string;
begin
  case Self of
    TWiRLResponseStatus.Informational: Result := 'Informational';
    TWiRLResponseStatus.Success: Result := 'Success';
    TWiRLResponseStatus.Redirect: Result := 'Redirect';
    TWiRLResponseStatus.ClientError: Result := 'ClientError';
    TWiRLResponseStatus.ServerError: Result := 'ServerError';
    else
      Result := 'Undefined';
  end;
end;

end.
