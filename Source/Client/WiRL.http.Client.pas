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

{$I WiRL.inc}

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

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidAndroid)]
  TWiRLClient = class(TComponent)
  private
    FHttpClient: IWiRLClient;
    FProxyParams: THttpProxyConnectionInfo;
    FWiRLEngineURL: string;
    FOnBeforeCommand: TBeforeCommandEvent;
    FOnAfterCommand: TAfterCommandEvent;
    FNoProtocolErrorException: Boolean;
{$IFDEF HAS_SYSTEM_THREADING}
    FWorkerTask: ITask;
{$ENDIF}
    function GetRequest: TWiRLRequest;
    function GetResponse: TWiRLResponse;
    function GetConnectTimeout: Integer;
    function GetReadTimeout: Integer;
    procedure SetConnectTimeout(const Value: Integer);
    procedure SetReadTimeout(const Value: Integer);
    procedure SetProxyParams(const Value: THttpProxyConnectionInfo);
    function GetMaxRedirects: Integer;
    procedure SetMaxRedirects(const Value: Integer);
  protected
    procedure DoBeforeCommand;
    procedure DoAfterCommand;
    procedure CheckResponse;
{$IFDEF HAS_SYSTEM_THREADING}
    property WorkerTask: ITask read FWorkerTask;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>Send 'GET' command to url</summary>
    procedure Get(const AURL, AAccept, AContentType: string; AResponseContent: TStream);
    /// <summary>Send 'POST' command to url</summary>
    procedure Post(const AURL, AAccept, AContentType: string; AContent, AResponse: TStream);
    /// <summary>Send 'PUT' command to url</summary>
    procedure Put(const AURL, AAccept, AContentType: string; AContent, AResponse: TStream);
    /// <summary>Send 'DELETE' command to url</summary>
    procedure Delete(const AURL, AAccept, AContentType: string; AResponseContent: TStream);
    /// <summary>Send 'PATCH' command to url</summary>
    procedure Patch(const AURL, AAccept, AContentType: string; AContent, AResponse: TStream);
    /// <summary>Send 'HEAD' command to url</summary>
    procedure Head(const AURL, AAccept, AContentType: string);
    /// <summary>Send 'OPTIONS' command to url</summary>
    procedure Options(const AURL, AAccept, AContentType: string; AResponse: TStream);

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
    property ConnectTimeout: Integer read GetConnectTimeout write SetConnectTimeout;
    /// <summary> Property to set the ResponseTimeout</summary>
    property ReadTimeout: Integer read GetReadTimeout write SetReadTimeout;
    /// <summary> Proxy Settings to be used by the client.</summary>
    property ProxyParams: THttpProxyConnectionInfo read FProxyParams write SetProxyParams;
    /// <summary> Event fired befere the request</summary>
    property OnBeforeCommand: TBeforeCommandEvent read FOnBeforeCommand write FOnBeforeCommand;
    /// <summary> Event fired when a request finishes</summary>
    property OnAfterCommand: TAfterCommandEvent read FOnAfterCommand write FOnAfterCommand;
    /// <summary> Maximum number of redirects</summary>
    property MaxRedirects: Integer read GetMaxRedirects write SetMaxRedirects default 5;
    /// <summary> Raise an exception for every protocol error (StatusCode >= 400) </summary>
    property NoProtocolErrorException: Boolean read FNoProtocolErrorException write FNoProtocolErrorException;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('WiRL Client', [TWiRLClient]);
end;

{ TWiRLClient }

procedure TWiRLClient.CheckResponse;
begin
  if (not FNoProtocolErrorException) and (Response.StatusCode >= 400) then
    raise EWiRLClientProtocolException.Create(Response.StatusCode, Response.ReasonString);
end;

constructor TWiRLClient.Create(AOwner: TComponent);
begin
  inherited;
  FHttpClient := TWiRLClientRegistry.Instance.CreateClient;
  FWiRLEngineURL := 'http://localhost:8080/rest';
  FProxyParams := THttpProxyConnectionInfo.Create;
  FHttpClient.ProxyParams := ProxyParams;
  MaxRedirects := 5;
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

function TWiRLClient.GetConnectTimeout: Integer;
begin
  Result := FHttpClient.ConnectTimeout;
end;

function TWiRLClient.GetMaxRedirects: Integer;
begin
  Result := FHttpClient.MaxRedirects;
end;

function TWiRLClient.GetReadTimeout: Integer;
begin
  Result := FHttpClient.ReadTimeout;
end;

function TWiRLClient.GetRequest: TWiRLRequest;
begin
  Result := FHttpClient.Request;
end;

function TWiRLClient.GetResponse: TWiRLResponse;
begin
  Result := FHttpClient.Response;
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
  Result := FHttpClient.Response.StatusCode = 200;
end;

procedure TWiRLClient.Delete(const AURL, AAccept, AContentType: string; AResponseContent: TStream);
begin
  FHttpClient.Request.Accept := AAccept;
  FHttpClient.Request.ContentType := AContentType;

  DoBeforeCommand;
  FHttpClient.Delete(AURL, AResponseContent);
  DoAfterCommand;
  CheckResponse;
end;

procedure TWiRLClient.Get(const AURL, AAccept, AContentType: string; AResponseContent: TStream);
begin
  FHttpClient.Request.Accept := AAccept;
  FHttpClient.Request.ContentType := AContentType;

  DoBeforeCommand;
  FHttpClient.Get(AURL, AResponseContent);
  DoAfterCommand;
  CheckResponse;
end;

procedure TWiRLClient.Head(const AURL, AAccept, AContentType: string);
begin
  FHttpClient.Request.Accept := AAccept;
  FHttpClient.Request.ContentType := AContentType;

  DoBeforeCommand;
  FHttpClient.Head(AURL);
  DoAfterCommand;
  CheckResponse;
end;

procedure TWiRLClient.Options(const AURL, AAccept, AContentType: string; AResponse: TStream);
begin
  FHttpClient.Request.Accept := AAccept;
  FHttpClient.Request.ContentType := AContentType;

  DoBeforeCommand;
  FHttpClient.Options(AURL, AResponse);
  DoAfterCommand;
  CheckResponse;
end;

procedure TWiRLClient.Patch(const AURL, AAccept, AContentType: string; AContent, AResponse: TStream);
begin
  FHttpClient.Request.Accept := AAccept;
  FHttpClient.Request.ContentType := AContentType;

  DoBeforeCommand;
  FHttpClient.Patch(AURL, AContent, AResponse);
  DoAfterCommand;
  CheckResponse;
end;

procedure TWiRLClient.Post(const AURL, AAccept, AContentType: string; AContent, AResponse: TStream);
begin
  FHttpClient.Request.Accept := AAccept;
  FHttpClient.Request.ContentType := AContentType;

  DoBeforeCommand;
  FHttpClient.Post(AURL, AContent, AResponse);
  DoAfterCommand;
  CheckResponse;
end;

procedure TWiRLClient.Put(const AURL, AAccept, AContentType: string; AContent, AResponse: TStream);
begin
  FHttpClient.Request.Accept := AAccept;
  FHttpClient.Request.ContentType := AContentType;

  DoBeforeCommand;
  FHttpClient.Put(AURL, AContent, AResponse);
  DoAfterCommand;
  CheckResponse;
end;

function TWiRLClient.ResponseText: string;
begin
  Result := FHttpClient.Response.ReasonString;
end;

procedure TWiRLClient.SetConnectTimeout(const Value: Integer);
begin
  FHttpClient.ConnectTimeout := Value;
end;

procedure TWiRLClient.SetMaxRedirects(const Value: Integer);
begin
  FHttpClient.MaxRedirects := Value;
end;

procedure TWiRLClient.SetReadTimeout(const Value: Integer);
begin
  FHttpClient.ReadTimeout := Value;
end;

procedure TWiRLClient.SetProxyParams(const Value: THttpProxyConnectionInfo);
begin
  ProxyParams.Assign(Value);
end;

end.
