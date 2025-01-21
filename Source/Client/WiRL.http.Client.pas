{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2021 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.Client;

{$I ..\Core\WiRL.inc}

interface

uses
  System.SysUtils, System.Classes, System.Rtti,

{$IFDEF HAS_SYSTEM_THREADING}
  System.Threading,
{$ENDIF}

  WiRL.http.Core,
  WiRL.http.Headers,
  WiRL.http.Client.Interfaces,
  WiRL.http.Accept.MediaType,

  WiRL.http.Request,
  WiRL.http.Response;

type
  TBeforeCommandEvent = procedure (ASender: TObject; ARequest: IWiRLRequest) of object;

  TAfterCommandEvent = procedure (ASender: TObject; ARequest: IWiRLRequest; AResponse: IWiRLResponse) of object;

  THttpMethodFunc = reference to function (ARequest: IWiRLRequest; AResponseStream: TStream) :IWiRLResponse;

  IWiRLInvocation = interface
    ['{83CE1B5E-6D01-4C78-A556-3C4DA54540E8}']
    procedure Target(const AUrl: string);
    procedure ContentType(const AContentType: string);
    procedure Accept(const AAccept: string);
    procedure AcceptLanguage(const AAcceptLanguage: string);
    procedure QueryParam(const AName: string; const AValue: TValue);
    procedure PathParam(const AName: string; const AValue: TValue);
    procedure SetContentStream(AStream: TStream);

    function GetResource: TObject;

    property Resource: TObject read GetResource;
  end;

  TWiRLClientRequest = class(TInterfacedObject, IWiRLRequest)
  private
    FHttpMethod: string;
    FURL: string;
    FContent: TStream;
    FHeaders: IWiRLHeaders;
    FMediaType: TMediaType;
  public
    function GetHeaderValue(const AName: string): string;
    function GetHeaders: IWiRLHeaders;
    function GetContent: string;
    function GetContentStream: TStream;
    function GetRawContent: TBytes;
    function GetAccept: string;
    function GetAcceptCharSet: string;
    function GetAcceptEncoding: string;
    function GetAcceptLanguage: string;
    function GetUserAgent: string;
    function GetAuthorization: string;
    function GetContentType: string;
    function GetURL: string;
    function GetHttpMethod: string;
    function GetContentMediaType: TMediaType;
    constructor Create(const AHttpMethod, AURL: string; AContent: TStream; AHeaders: IWiRLHeaders);
    destructor Destroy; override;
  end;

  {$IFDEF HAS_NEW_ANDROID_PID}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator32 or pidiOSDevice32 or pidAndroidArm32)]
  {$ELSEIF HAS_NEW_PIDS}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidAndroid)]
  {$ENDIF}
  TWiRLClient = class(TComponent)
  private
  const
    DefaultConnectionTimeout = 60000;
    DefaultReadTimeout = 60000;
    DefaultMaxRedirects = 5;
  private
    FHttpClient: IWiRLClient;
    FProxyParams: TWiRLProxyConnectionInfo;
    FWiRLEngineURL: string;
    FOnBeforeCommand: TBeforeCommandEvent;
    FOnAfterCommand: TAfterCommandEvent;
    FNoProtocolErrorException: Boolean;
    FClientVendor: string;
    FReadTimeout: Integer;
    FMaxRedirects: Integer;
    FConnectTimeout: Integer;
{$IFDEF HAS_SYSTEM_THREADING}
    FWorkerTask: ITask;
{$ENDIF}
    procedure SetClientVendor(const Value: string);
    function GetClientImplementation: TObject;

    procedure CreateHttpClient;
    procedure InitHttpClient;
  protected
    procedure DoBeforeCommand(ARequest: IWiRLRequest); virtual;
    procedure DoAfterCommand(ARequest: IWiRLRequest; AResponse: IWiRLResponse); virtual;
    function ExecuteHttpMethod(AHttpMethod: TWiRLHttpMethod; const AURL: string; ARequestStream, AResponseStream: TStream; AHeaders: IWiRLHeaders; AFunc: THttpMethodFunc): IWiRLResponse;
    procedure CheckResponse(AResponse: IWiRLResponse);
    function ParseUrl(const AURL: string): string;
{$IFDEF HAS_SYSTEM_THREADING}
    property WorkerTask: ITask read FWorkerTask;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>Send 'GET' command to url</summary>
    function Get(const AURL: string; AResponseStream: TStream; AHeaders: IWiRLHeaders = nil): IWiRLResponse;
    /// <summary>Send 'POST' command to url</summary>
    function Post(const AURL: string; ARequestStream, AResponseStream: TStream; AHeaders: IWiRLHeaders = nil): IWiRLResponse;
    /// <summary>Send 'PUT' command to url</summary>
    function Put(const AURL: string; ARequestStream, AResponseStream: TStream; AHeaders: IWiRLHeaders = nil): IWiRLResponse;
    /// <summary>Send 'DELETE' command to url</summary>
    function Delete(const AURL: string; ARequestStream, AResponseStream: TStream; AHeaders: IWiRLHeaders = nil): IWiRLResponse;
    /// <summary>Send 'PATCH' command to url</summary>
    function Patch(const AURL: string; ARequestStream, AResponseStream: TStream; AHeaders: IWiRLHeaders = nil): IWiRLResponse;
    /// <summary>Send 'HEAD' command to url</summary>
    function Head(const AURL: string; AHeaders: IWiRLHeaders = nil): IWiRLResponse;
    /// <summary>Send 'OPTIONS' command to url</summary>
    function Options(const AURL: string; AResponseStream: TStream; AHeaders: IWiRLHeaders = nil): IWiRLResponse;

    procedure ExecuteAsync(const AProc: TProc);
    function IsRunningAsync: Boolean;

    property HttpClient: IWiRLClient read FHttpClient;
  published
    property WiRLEngineURL: string read FWiRLEngineURL write FWiRLEngineURL;
    /// <summary> Property to set the ConnectionTimeout</summary>
    property ConnectTimeout: Integer read FConnectTimeout write FConnectTimeout default DefaultConnectionTimeout;
    /// <summary> Property to set the ResponseTimeout</summary>
    property ReadTimeout: Integer read FReadTimeout write FReadTimeout default DefaultReadTimeout;
    /// <summary> Proxy Settings to be used by the client.</summary>
    property ProxyParams: TWiRLProxyConnectionInfo read FProxyParams write FProxyParams;
    /// <summary> Event fired befere the request</summary>
    property OnBeforeCommand: TBeforeCommandEvent read FOnBeforeCommand write FOnBeforeCommand;
    /// <summary> Event fired when a request finishes</summary>
    property OnAfterCommand: TAfterCommandEvent read FOnAfterCommand write FOnAfterCommand;
    /// <summary> Maximum number of redirects</summary>
    property MaxRedirects: Integer read FMaxRedirects write FMaxRedirects default DefaultMaxRedirects;
    /// <summary> Raise an exception for every protocol error (StatusCode >= 400) </summary>
    property NoProtocolErrorException: Boolean read FNoProtocolErrorException write FNoProtocolErrorException default false;
    /// <summary> Vendor of the http client implementation </summary>
    property ClientVendor: string read FClientVendor write SetClientVendor;
    /// <summary> Underlying implementation of the client object </summary>
    property ClientImplementation: TObject read GetClientImplementation;
  end;

implementation

{ TWiRLClient }

procedure TWiRLClient.CheckResponse(AResponse: IWiRLResponse);
begin
  if (not FNoProtocolErrorException) and (AResponse.StatusCode >= 400) then
    raise EWiRLClientProtocolException.Create(AResponse);
end;

constructor TWiRLClient.Create(AOwner: TComponent);
begin
  inherited;
  FWiRLEngineURL := 'http://localhost:8080/rest';
  FProxyParams := TWiRLProxyConnectionInfo.Create;
  // Set defaults
  ConnectTimeout := DefaultConnectionTimeout;
  ReadTimeout := DefaultReadTimeout;
  MaxRedirects := DefaultMaxRedirects;
end;

destructor TWiRLClient.Destroy;
begin
  FProxyParams.Free;
  inherited;
end;

procedure TWiRLClient.DoAfterCommand(ARequest: IWiRLRequest; AResponse: IWiRLResponse);
begin
  if Assigned(FOnAfterCommand) then
    FOnAfterCommand(Self, ARequest, AResponse);
end;

procedure TWiRLClient.DoBeforeCommand(ARequest: IWiRLRequest);
begin
  if Assigned(FOnBeforeCommand) then
    FOnBeforeCommand(Self, ARequest);
end;

procedure TWiRLClient.ExecuteAsync(const AProc: TProc);
begin
{$IFDEF HAS_SYSTEM_THREADING}
  if IsRunningAsync then
    raise EWiRLClientException.Create('Multiple async execution not yet supported');
  FWorkerTask := TTask.Create(AProc).Start;
{$ELSE}
  raise EWiRLClientException.Create('Async execution not yet supported');
{$ENDIF}
end;

function TWiRLClient.ExecuteHttpMethod(AHttpMethod: TWiRLHttpMethod; const AURL: string;
  ARequestStream, AResponseStream: TStream; AHeaders: IWiRLHeaders;
  AFunc: THttpMethodFunc): IWiRLResponse;
var
  LRequest: IWiRLRequest;

  LResponseStream: TStream;
  LInternalStream: Boolean;
begin
  InitHttpClient;

  LRequest := TWiRLClientRequest.Create(AHttpMethod.ToString, ParseUrl(AURL), ARequestStream, AHeaders);
  DoBeforeCommand(LRequest);

  LInternalStream := not Assigned(AResponseStream);
  if LInternalStream then
    LResponseStream := TMemoryStream.Create
  else
    LResponseStream := AResponseStream;
  try
    Result := AFunc(LRequest, LResponseStream);

    if LInternalStream then
      Result.SetOwnContentStream(True);

    DoAfterCommand(LRequest, Result);
    CheckResponse(Result);

  except
    if LInternalStream then
    begin
      if Assigned(Result) then
        Result.SetOwnContentStream(True)
      else
        LResponseStream.Free;
    end;
    raise;
  end;
end;

function TWiRLClient.GetClientImplementation: TObject;
begin
  CreateHttpClient;
  Result := FHttpClient.ClientImplementation;
end;

procedure TWiRLClient.CreateHttpClient;
begin
  if not Assigned(FHttpClient) then
    FHttpClient := TWiRLClientRegistry.Instance.CreateClient(FClientVendor);
end;

procedure TWiRLClient.InitHttpClient;
begin
  CreateHttpClient;
  FHttpClient.ProxyParams := FProxyParams;
  FHttpClient.ConnectTimeout := ConnectTimeout;
  FHttpClient.ReadTimeout := ReadTimeout;
  FHttpClient.MaxRedirects := MaxRedirects;
end;

function TWiRLClient.IsRunningAsync: Boolean;
begin
{$IFDEF HAS_SYSTEM_THREADING}
  Result := Assigned(FWorkerTask) and (FWorkerTask.Status < TTaskStatus.Completed);
{$ELSE}
  Result := False;
{$ENDIF}
end;

function TWiRLClient.Delete(const AURL: string; ARequestStream, AResponseStream: TStream; AHeaders: IWiRLHeaders): IWiRLResponse;
begin
  Result := ExecuteHttpMethod(TWiRLHttpMethod.DELETE, AURL, nil, AResponseStream, AHeaders,
    function (ARequest: IWiRLRequest; AResponseStream: TStream) :IWiRLResponse
    begin
      Result := FHttpClient.Delete(ARequest.URL, ARequestStream, AResponseStream, ARequest.Headers);
    end);
end;

function TWiRLClient.Get(const AURL: string; AResponseStream: TStream; AHeaders: IWiRLHeaders): IWiRLResponse;
begin
  Result := ExecuteHttpMethod(TWiRLHttpMethod.GET, AURL, nil, AResponseStream, AHeaders,
    function (ARequest: IWiRLRequest; AResponseStream: TStream) :IWiRLResponse
    begin
      Result := FHttpClient.Get(ARequest.URL, AResponseStream, ARequest.Headers);
    end);
end;

function TWiRLClient.Head(const AURL: string; AHeaders: IWiRLHeaders): IWiRLResponse;
begin
  Result := ExecuteHttpMethod(TWiRLHttpMethod.HEAD, AURL, nil, nil, AHeaders,
    function (ARequest: IWiRLRequest; AResponseStream: TStream) :IWiRLResponse
    begin
      Result := FHttpClient.Head(ARequest.URL, ARequest.Headers);
    end);
end;

function TWiRLClient.Options(const AURL: string; AResponseStream: TStream; AHeaders: IWiRLHeaders): IWiRLResponse;
begin
  Result := ExecuteHttpMethod(TWiRLHttpMethod.OPTIONS, AURL, nil, AResponseStream, AHeaders,
    function (ARequest: IWiRLRequest; AResponseStream: TStream) :IWiRLResponse
    begin
      Result := FHttpClient.Options(ARequest.URL, AResponseStream, ARequest.Headers);
    end);
end;

function TWiRLClient.ParseUrl(const AURL: string): string;
const
  WiRLEngineURLPattern = '{WiRLEngineURL}';
begin
  Result := StringReplace(AURL, WiRLEngineURLPattern, FWiRLEngineURL, [rfReplaceAll, rfIgnoreCase]);
end;

function TWiRLClient.Patch(const AURL: string; ARequestStream, AResponseStream: TStream; AHeaders: IWiRLHeaders): IWiRLResponse;
begin
  Result := ExecuteHttpMethod(TWiRLHttpMethod.PATCH, AURL, ARequestStream, AResponseStream, AHeaders,
    function (ARequest: IWiRLRequest; AResponseStream: TStream) :IWiRLResponse
    begin
      Result := FHttpClient.Patch(ARequest.URL, ARequestStream, AResponseStream, ARequest.Headers);
    end);
end;

function TWiRLClient.Post(const AURL: string; ARequestStream, AResponseStream: TStream; AHeaders: IWiRLHeaders): IWiRLResponse;
begin
  Result := ExecuteHttpMethod(TWiRLHttpMethod.POST, AURL, ARequestStream, AResponseStream, AHeaders,
    function (ARequest: IWiRLRequest; AResponseStream: TStream) :IWiRLResponse
    begin
      Result := FHttpClient.Post(ARequest.URL, ARequestStream, AResponseStream, ARequest.Headers);
    end);
end;

function TWiRLClient.Put(const AURL: string; ARequestStream, AResponseStream: TStream; AHeaders: IWiRLHeaders): IWiRLResponse;
begin
  Result := ExecuteHttpMethod(TWiRLHttpMethod.PUT, AURL, ARequestStream, AResponseStream, AHeaders,
    function (ARequest: IWiRLRequest; AResponseStream: TStream) :IWiRLResponse
    begin
      Result := FHttpClient.Put(ARequest.URL, ARequestStream, AResponseStream, ARequest.Headers);
    end);
end;

procedure TWiRLClient.SetClientVendor(const Value: string);
begin
  if FClientVendor <> Value then
  begin
    if not TWiRLClientRegistry.Instance.ContainsKey(Value) and (csDesigning in ComponentState) then
      FClientVendor := ''
    else
      FClientVendor := Value;
    CreateHttpClient;
  end;
end;

{ TWiRLClientRequest }

constructor TWiRLClientRequest.Create(const AHttpMethod, AURL: string; AContent: TStream; AHeaders: IWiRLHeaders);
begin
  inherited Create;
  FHttpMethod := AHttpMethod;
  FURL := AURL;
  FContent := AContent;
  if Assigned(AHeaders) then
    FHeaders := AHeaders
  else
    FHeaders := TWiRLHeaders.Create;
end;

destructor TWiRLClientRequest.Destroy;
begin
  FMediaType.Free;
  inherited;
end;

function TWiRLClientRequest.GetAccept: string;
begin
  Result := FHeaders.Accept;
end;

function TWiRLClientRequest.GetAcceptCharSet: string;
begin
  Result := FHeaders.AcceptCharSet;
end;

function TWiRLClientRequest.GetAcceptEncoding: string;
begin
  Result := FHeaders.AcceptEncoding;
end;

function TWiRLClientRequest.GetAcceptLanguage: string;
begin
  Result := FHeaders.AcceptLanguage;
end;

function TWiRLClientRequest.GetAuthorization: string;
begin
  Result := FHeaders.Authorization;
end;

function TWiRLClientRequest.GetContent: string;
begin
  Result := EncodingFromCharSet(GetContentMediaType.Charset).GetString(GetRawContent);
end;

function TWiRLClientRequest.GetContentMediaType: TMediaType;
begin
  if not Assigned(FMediaType) then
    FMediaType := TMediaType.Create(GetContentType);
  Result := FMediaType;
end;

function TWiRLClientRequest.GetContentStream: TStream;
begin
  Result := FContent;
end;

function TWiRLClientRequest.GetContentType: string;
begin
  Result := FHeaders.ContentType;
end;

function TWiRLClientRequest.GetHeaders: IWiRLHeaders;
begin
  Result := FHeaders;
end;

function TWiRLClientRequest.GetHeaderValue(const AName: string): string;
begin
  Result := FHeaders.Values[AName];
end;

function TWiRLClientRequest.GetHttpMethod: string;
begin
  Result := FHttpMethod;
end;

function TWiRLClientRequest.GetRawContent: TBytes;
begin
  if (GetContentStream <> nil) and (GetContentStream.Size > 0) then
  begin
    GetContentStream.Position := 0;
    SetLength(Result, GetContentStream.Size);
    GetContentStream.ReadBuffer(Result[0], GetContentStream.Size);
  end;
end;

function TWiRLClientRequest.GetURL: string;
begin
  Result := FURL;
end;

function TWiRLClientRequest.GetUserAgent: string;
begin
  Result := FHeaders.UserAgent;
end;

end.
