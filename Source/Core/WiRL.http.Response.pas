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

  WiRL.Core.Attributes,
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

  // General interface for all writers (chunk, SSE, ...)
  IWiRLResponseWriter = interface
    ['{B16D3D6C-685C-4076-9A8C-19861323D2CB}']
    procedure Write(const AValue: string; AEncoding: TEncoding = nil); overload;
    procedure Write(const AValue: TBytes); overload;
  end;

  IWiRLSSEResponseWriter = interface
    ['{B16D3D6C-685C-4076-9A8C-19861323D2CB}']
    procedure Write(const AValue: string); overload;
    procedure Write(const AEvent, AValue: string); overload;
    procedure Write(const AEvent, AValue: string; ARetry: Integer); overload;
    procedure Write(const AId, AEvent, AValue: string; ARetry: Integer); overload;
    procedure Write(const AId, AEvent, AValue: string); overload;
    procedure Write(AId: Integer; const AEvent, AValue: string; ARetry: Integer); overload;
    procedure Write(AId: Integer; const AEvent, AValue: string); overload;
    procedure WriteComment(const AValue: string); overload;
  end;

  TWriterProc = reference to procedure (AWriter: IWiRLResponseWriter);
  TSSEWriterProc = reference to procedure (AWriter: IWiRLSSEResponseWriter);

  TWiRLStreamingResponse = class(TObject)
  public
    procedure SendResponse(AContext: TObject); virtual; abstract;
  end;

  TWiRLChunkedResponse = class(TWiRLStreamingResponse)
  private
    FWriterProc: TWriterProc;
  public
    procedure SendResponse(AContext: TObject); override;
    constructor Create(AWriterProc: TWriterProc);
  end;

  TWiRLSSEResponse = class(TWiRLStreamingResponse)
  private
    FWriterProc: TSSEWriterProc;
  public
    procedure SendResponse(AContext: TObject); override;
    constructor Create(AWriterProc: TSSEWriterProc);
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
    function GetTransferEncoding: string;
    procedure SetTransferEncoding(const Value: string);
  protected
    function IsUnknownResponseCode: Boolean; virtual;
    function GetHeaders: IWiRLHeaders; virtual; abstract;
    function GetContentText: string; virtual;
    function GetContentStream: TStream; virtual; abstract;
    procedure SetContentText(const Value: string); virtual;
    procedure SetContentStream(const Value: TStream); virtual; abstract;
    function GetStatusCode: Integer; virtual; abstract;
    procedure SetStatusCode(const Value: Integer); virtual; abstract;
    function GetReasonString: string; virtual; abstract;
    procedure SetReasonString(const Value: string); virtual; abstract;
    function GetConnection: TWiRLConnection; virtual; abstract;
  public
    procedure SendHeaders(AImmediate: Boolean = False); virtual; abstract;
    destructor Destroy; override;

    procedure FromWiRLStatus(AStatus: TWiRLHttpStatus);
    procedure Redirect(ACode: Integer; const ALocation: string);
    procedure SetNonStandardReasonString(const AValue: string);

    property HasContentLength: Boolean read FHasContentLength;
    property Date: TDateTime read GetDate write SetDate;
    property Expires: TDateTime read GetExpires write SetExpires;
    property LastModified: TDateTime read GetLastModified write SetLastModified;
    property Content: string read GetContentText write SetContentText;
    property ContentText: string read GetContentText write SetContentText;
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
    property RawContent: TBytes read GetRawContent write SetRawContent;
    property Allow: string read GetAllow write SetAllow;
    property Server: string read GetServer write SetServer;
    property WWWAuthenticate: string read GetWWWAuthenticate write SetWWWAuthenticate;
    property Location: string read GetLocation write SetLocation;
    property Cookies: TWiRLCookies read GetCookies;
    property TransferEncoding: string read GetTransferEncoding write SetTransferEncoding;
    property Connection: TWiRLConnection read GetConnection;
  end;


implementation

uses
  System.TypInfo,
  WiRL.Core.Context,
  IdGlobal, IdGlobalProtocols;

type
  TWiRLCunkedResponseWriter = class(TInterfacedObject, IWiRLResponseWriter)
  private
    FContext: TWiRLContextHttp;
  public
    procedure Write(const AValue: string; AEncoding: TEncoding = nil); overload;
    procedure Write(const AValue: TBytes); overload;
    constructor Create(AContext: TWiRLContextHttp);
  end;

  TWiRLSSEResponseWriter = class(TInterfacedObject, IWiRLResponseWriter, IWiRLSSEResponseWriter)
  private
    FContext: TWiRLContextHttp;
  private
    function SplitString(const AValue: string): TArray<string>;
  public
    procedure Write(const AValue: string; AEncoding: TEncoding = nil); overload;
    procedure Write(const AValue: string); overload;
    procedure Write(const AValue: TBytes); overload;
    procedure Write(const AEvent, AValue: string); overload;
    procedure Write(const AEvent, AValue: string; ARetry: Integer); overload;
    procedure Write(const AId, AEvent, AValue: string; ARetry: Integer); overload;
    procedure Write(const AId, AEvent, AValue: string); overload;
    procedure Write(AId: Integer; const AEvent, AValue: string; ARetry: Integer); overload;
    procedure Write(AId: Integer; const AEvent, AValue: string); overload;
    procedure WriteComment(const AValue: string); overload;

    constructor Create(AContext: TWiRLContextHttp);
  end;


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

function TWiRLResponse.GetContentText: string;
var
  LEncoding: TEncoding;
begin
  LEncoding := ContentMediaType.GetDelphiEncoding;
  try
    Result := LEncoding.GetString(RawContent);
  finally
    LEncoding.Free;
  end;
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
  end
  else
    SetLength(Result, 0);
end;

function TWiRLResponse.GetServer: string;
begin
  Result := Headers.Values['Server'];
end;

function TWiRLResponse.GetTransferEncoding: string;
begin
  Result := Headers.TransferEncoding;
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

procedure TWiRLResponse.SetContentText(const Value: string);
var
  LEncoding: TEncoding;
begin
  LEncoding := ContentMediaType.GetDelphiEncoding;
  try
    RawContent := LEncoding.GetBytes(Value);
  finally
    LEncoding.Free;
  end;
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
begin
  if not Assigned(ContentStream) then
    ContentStream := TMemoryStream.Create();
  ContentStream.Position := 0;
  ContentStream.WriteData(Value, Length(Value));
  ContentStream.Size := Length(Value);
end;

procedure TWiRLResponse.SetServer(const Value: string);
begin
  Headers.Values['Server'] := Value;
end;

procedure TWiRLResponse.SetTransferEncoding(const Value: string);
begin
  Headers.TransferEncoding := Value;
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

{ TWiRLChunkedResponse }

constructor TWiRLChunkedResponse.Create(AWriterProc: TWriterProc);
begin
  inherited Create;
  FWriterProc := AWriterProc;
end;

procedure TWiRLChunkedResponse.SendResponse(AContext: TObject);
var
  LContextHttp: TWiRLContextHttp;
  LWriter: IWiRLResponseWriter;
begin
  LContextHttp := AContext as TWiRLContextHttp;
  LContextHttp.Response.Headers.TransferEncoding := 'chunked';
  LContextHttp.Response.SendHeaders(True);
  LWriter := TWiRLCunkedResponseWriter.Create(LContextHttp);
  FWriterProc(LWriter);
  LWriter.Write('');
end;

{ TWiRLCunkedResponseWriter }

constructor TWiRLCunkedResponseWriter.Create(AContext: TWiRLContextHttp);
begin
  inherited Create;
  FContext := AContext;
end;

procedure TWiRLCunkedResponseWriter.Write(const AValue: string; AEncoding: TEncoding);
var
  LEncoding: TEncoding;
begin
  if Assigned(AEncoding) then
    LEncoding := AEncoding
  else
    LEncoding := TEncoding.UTF8;
  Write(LEncoding.GetBytes(AValue));
end;

procedure TWiRLCunkedResponseWriter.Write(const AValue: TBytes);
begin
  // Lenght of the chunk (as hexadecimal string)
  FContext.Response.Connection.WriteLn(IntToHex(Length(AValue)));
  // Chunk content
  FContext.Response.Connection.Write(AValue);
  // New line (CRLF)
  FContext.Response.Connection.WriteLn();
end;

{ TWiRLSSEResponse }

constructor TWiRLSSEResponse.Create(AWriterProc: TSSEWriterProc);
begin
  inherited Create;
  FWriterProc := AWriterProc;
end;

procedure TWiRLSSEResponse.SendResponse(AContext: TObject);
var
  LContextHttp: TWiRLContextHttp;
  LWriter: IWiRLSSEResponseWriter;
begin
  LContextHttp := AContext as TWiRLContextHttp;
  LContextHttp.Response.ContentType := 'text/event-stream';
  LContextHttp.Response.ContentLength := -2;
  LContextHttp.Response.SendHeaders(True);
  LWriter := TWiRLSSEResponseWriter.Create(LContextHttp);
  FWriterProc(LWriter);
end;

{ TWiRLSSEResponseWriter }

constructor TWiRLSSEResponseWriter.Create(AContext: TWiRLContextHttp);
begin
  inherited Create;
  FContext := AContext;
end;

procedure TWiRLSSEResponseWriter.Write(const AValue: string;
  AEncoding: TEncoding);
begin
  Write('', AValue);
end;

procedure TWiRLSSEResponseWriter.Write(const AValue: TBytes);
begin
  raise Exception.Create('Unsupported binary data');
end;

procedure TWiRLSSEResponseWriter.Write(const AValue: string);
begin
  Write('', AValue);
end;

function TWiRLSSEResponseWriter.SplitString(
  const AValue: string): TArray<string>;
var
  LLines: TArray<string>;
begin
  LLines := AValue.Split([sLineBreak]);
  if Length(LLines) = 0 then
    LLines := AValue.Split([#13]);
  if Length(LLines) = 0 then
    LLines := AValue.Split([#10]);
  if Length(LLines) = 0 then
    LLines := [AValue];

  Result := LLines;
end;

procedure TWiRLSSEResponseWriter.Write(const AEvent, AValue: string; ARetry: Integer);
begin
  Write('', AEvent, AValue, ARetry);
end;

procedure TWiRLSSEResponseWriter.Write(const AEvent, AValue: string);
begin
  Write(AEvent, AValue, 0);
end;

procedure TWiRLSSEResponseWriter.WriteComment(const AValue: string);
var
  LLines: TArray<string>;
  LLine: string;
  LMessage: string;
begin
  LLines := SplitString(AValue);

  LMessage := '';

  for LLine in LLines do
    LMessage := LMessage + ': ' + LLine + #13#10;

  FContext.Connection.WriteLn(LMessage);

end;

procedure TWiRLSSEResponseWriter.Write(const AId, AEvent, AValue: string);
begin
  Write(AId, AEvent, AValue, 0);
end;

procedure TWiRLSSEResponseWriter.Write(const AId, AEvent, AValue: string;
  ARetry: Integer);
var
  LLines: TArray<string>;
  LLine: string;
  LMessage: string;
begin
  LLines := SplitString(AValue);

  LMessage := '';
  if AId <> '' then
    LMessage := LMessage + 'id: ' + AId + #13#10;
  if AEvent <> '' then
    LMessage := LMessage + 'event: ' + AEvent + #13#10;
  if ARetry > 0 then
    LMessage := LMessage + 'retry: ' + IntToStr(ARetry) + #13#10;

  for LLine in LLines do
    LMessage := LMessage + 'data: ' + LLine + #13#10;

  FContext.Connection.WriteLn(LMessage);
end;

procedure TWiRLSSEResponseWriter.Write(AId: Integer; const AEvent,
  AValue: string);
begin
  Write(AId.ToString, AEvent, AValue, 0);
end;

procedure TWiRLSSEResponseWriter.Write(AId: Integer; const AEvent,
  AValue: string; ARetry: Integer);
begin
  Write(AId.ToString, AEvent, AValue, ARetry);
end;

end.
