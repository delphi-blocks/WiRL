{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.Client;

{$I ..\Core\WiRL.inc}

interface

uses
  System.SysUtils, System.Classes,

{$IFDEF HAS_SYSTEM_THREADING}
  System.Threading,
{$ENDIF}

  WiRL.http.Client.Interfaces,

  WiRL.http.Request,
  WiRL.http.Response;

type
  TBeforeCommandEvent = procedure (ASender: TObject; ARequest: TWiRLRequest) of object;

  TAfterCommandEvent = procedure (ASender: TObject; ARequest: TWiRLRequest; AResponse: TWiRLResponse) of object;

  {$IFDEF HAS_NEW_PIDS}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator32 or pidiOSDevice32 or pidAndroid32Arm)]
  {$ELSE}
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
    function GetRequest: TWiRLRequest;
    function GetResponse: TWiRLResponse;
    procedure SetClientVendor(const Value: string);
    function GetClientImplementation: TObject;

    procedure CreateHttpClient;
    procedure InitHttpClient;
  protected
    procedure DoBeforeCommand; virtual;
    procedure DoAfterCommand; virtual;
    procedure CheckResponse;
{$IFDEF HAS_SYSTEM_THREADING}
    property WorkerTask: ITask read FWorkerTask;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>Send 'GET' command to url</summary>
    procedure Get(const AURL: string; AResponseContent: TStream);
    /// <summary>Send 'POST' command to url</summary>
    procedure Post(const AURL: string; AContent, AResponse: TStream);
    /// <summary>Send 'PUT' command to url</summary>
    procedure Put(const AURL: string; AContent, AResponse: TStream);
    /// <summary>Send 'DELETE' command to url</summary>
    procedure Delete(const AURL: string; AResponseContent: TStream);
    /// <summary>Send 'PATCH' command to url</summary>
    procedure Patch(const AURL: string; AContent, AResponse: TStream);
    /// <summary>Send 'HEAD' command to url</summary>
    procedure Head(const AURL: string);
    /// <summary>Send 'OPTIONS' command to url</summary>
    procedure Options(const AURL: string; AResponse: TStream);

    function LastCmdSuccess: Boolean;
    function ResponseText: string;

    procedure ExecuteAsync(const AProc: TProc);
    function IsRunningAsync: Boolean;

    property Request: TWiRLRequest read GetRequest;
    property Response: TWiRLResponse read GetResponse;
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
    property NoProtocolErrorException: Boolean read FNoProtocolErrorException write FNoProtocolErrorException;
    /// <summary> Vendor of the http client implementation </summary>
    property ClientVendor: string read FClientVendor write SetClientVendor;
    /// <summary> Underlying implementation of the client object </summary>
    property ClientImplementation: TObject read GetClientImplementation;
  end;

implementation

{ TWiRLClient }

procedure TWiRLClient.CheckResponse;
begin
  if (not FNoProtocolErrorException) and (Response.StatusCode >= 400) then
    raise EWiRLClientProtocolException.Create(Response.StatusCode, Response.ReasonString);
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

procedure TWiRLClient.DoAfterCommand;
begin
  if Assigned(FOnAfterCommand) then
    FOnAfterCommand(Self, FHttpClient.Request, FHttpClient.Response);
end;

procedure TWiRLClient.DoBeforeCommand;
begin
  if Assigned(FOnBeforeCommand) then
    FOnBeforeCommand(Self, FHttpClient.Request);
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

function TWiRLClient.GetClientImplementation: TObject;
begin
  CreateHttpClient;
  Result := FHttpClient.ClientImplementation;
end;

function TWiRLClient.GetRequest: TWiRLRequest;
begin
  CreateHttpClient;
  Result := FHttpClient.Request;
end;

function TWiRLClient.GetResponse: TWiRLResponse;
begin
  CreateHttpClient;
  Result := FHttpClient.Response;
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

function TWiRLClient.LastCmdSuccess: Boolean;
begin
  CreateHttpClient;
  Result := FHttpClient.Response.StatusCode = 200;
end;

procedure TWiRLClient.Delete(const AURL: string; AResponseContent: TStream);
begin
  InitHttpClient;
  DoBeforeCommand;
  FHttpClient.Delete(AURL, AResponseContent);
  DoAfterCommand;
  CheckResponse;
end;

procedure TWiRLClient.Get(const AURL: string; AResponseContent: TStream);
begin
  InitHttpClient;
  DoBeforeCommand;
  FHttpClient.Get(AURL, AResponseContent);
  DoAfterCommand;
  CheckResponse;
end;

procedure TWiRLClient.Head(const AURL: string);
begin
  InitHttpClient;
  DoBeforeCommand;
  FHttpClient.Head(AURL);
  DoAfterCommand;
  CheckResponse;
end;

procedure TWiRLClient.Options(const AURL: string; AResponse: TStream);
begin
  InitHttpClient;
  DoBeforeCommand;
  FHttpClient.Options(AURL, AResponse);
  DoAfterCommand;
  CheckResponse;
end;

procedure TWiRLClient.Patch(const AURL: string; AContent, AResponse: TStream);
begin
  InitHttpClient;
  DoBeforeCommand;
  FHttpClient.Patch(AURL, AContent, AResponse);
  DoAfterCommand;
  CheckResponse;
end;

procedure TWiRLClient.Post(const AURL: string; AContent, AResponse: TStream);
begin
  InitHttpClient;
  DoBeforeCommand;
  FHttpClient.Post(AURL, AContent, AResponse);
  DoAfterCommand;
  CheckResponse;
end;

procedure TWiRLClient.Put(const AURL: string; AContent, AResponse: TStream);
begin
  InitHttpClient;
  DoBeforeCommand;
  FHttpClient.Put(AURL, AContent, AResponse);
  DoAfterCommand;
  CheckResponse;
end;

function TWiRLClient.ResponseText: string;
begin
  CreateHttpClient;
  Result := FHttpClient.Response.ReasonString;
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

end.
