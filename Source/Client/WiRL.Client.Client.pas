{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Client.Client;

{$I WiRL.inc}

interface

uses
  System.SysUtils, System.Classes,

{$IFDEF HAS_SYSTEM_THREADING}
  System.Threading,
{$ENDIF}

  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP,
  IdHTTPHeaderInfo;

type
  TBeforeCommandEvent = procedure (ASender: TObject; ARequest: TIdHTTPRequest) of object;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidAndroid)]
  TWiRLClient = class(TComponent)
  private
    FHttpClient: TIdHTTP;
    FWiRLEngineURL: string;
    FOnBeforeCommand: TBeforeCommandEvent;
{$IFDEF HAS_SYSTEM_THREADING}
    FWorkerTask: ITask;
{$ENDIF}
    function GetRequest: TIdHTTPRequest;
    function GetResponse: TIdHTTPResponse;
    function GetConnectTimeout: Integer;
    function GetReadTimeout: Integer;
    function GetProxyParams: TIdProxyConnectionInfo;
    procedure SetConnectTimeout(const Value: Integer);
    procedure SetReadTimeout(const Value: Integer);
    procedure SetProxyParams(const Value: TIdProxyConnectionInfo);
  protected
    procedure DoBeforeCommand;
{$IFDEF HAS_SYSTEM_THREADING}
    property WorkerTask: ITask read FWorkerTask;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Delete(const AURL, AAccept, AContentType: string; AResponseContent: TStream);
    procedure Get(const AURL, AAccept, AContentType: string; AResponseContent: TStream);
    procedure Post(const AURL, AAccept, AContentType: string; AContent, AResponse: TStream);
    procedure Put(const AURL, AAccept, AContentType: string; AContent, AResponse: TStream);
    procedure Patch(const AURL, AAccept, AContentType: string; AContent, AResponse: TStream);
    procedure Head(const AURL, AAccept, AContentType: string);
    procedure Options(const AURL, AAccept, AContentType: string; AResponse: TStream);

    function LastCmdSuccess: Boolean;
    function ResponseText: string;

    procedure ExecuteAsync(const AProc: TProc);
    function IsRunningAsync: Boolean;

    property Request: TIdHTTPRequest read GetRequest;
    property Response: TIdHTTPResponse read GetResponse;
  published
    property WiRLEngineURL: string read FWiRLEngineURL write FWiRLEngineURL;
    property ConnectTimeout: Integer read GetConnectTimeout write SetConnectTimeout;
    property ReadTimeout: Integer read GetReadTimeout write SetReadTimeout;
    property ProxyParams: TIdProxyConnectionInfo read GetProxyParams write SetProxyParams;
    property OnBeforeCommand: TBeforeCommandEvent read FOnBeforeCommand write FOnBeforeCommand;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('WiRL Client', [TWiRLClient]);
end;

{ TWiRLClient }

constructor TWiRLClient.Create(AOwner: TComponent);
begin
  inherited;
  FHttpClient := TIdHTTP.Create(nil);
  FWiRLEngineURL := 'http://localhost:8080/rest';
end;

destructor TWiRLClient.Destroy;
begin
  FHttpClient.Free;
  inherited;
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
    raise Exception.Create('Multiple async execution not yet supported');
  FWorkerTask := TTask.Create(AProc).Start;
{$ELSE}
  raise Exception.Create('Async execution not yet supported');
{$ENDIF}
end;

function TWiRLClient.GetConnectTimeout: Integer;
begin
  Result := FHttpClient.ConnectTimeout;
end;

function TWiRLClient.GetReadTimeout: Integer;
begin
  Result := FHttpClient.ReadTimeout;
end;

function TWiRLClient.GetProxyParams: TIdProxyConnectionInfo;
begin
  Result := FHttpClient.ProxyParams;
end;

function TWiRLClient.GetRequest: TIdHTTPRequest;
begin
  Result := FHttpClient.Request;
end;

function TWiRLClient.GetResponse: TIdHTTPResponse;
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
  Result := FHttpClient.ResponseCode = 200;
end;

procedure TWiRLClient.Delete(const AURL, AAccept, AContentType: string; AResponseContent: TStream);
begin
  FHttpClient.Request.Accept := AAccept;
  FHttpClient.Request.ContentType := AContentType;

  DoBeforeCommand;
  FHttpClient.Delete(AURL, AResponseContent);
end;

procedure TWiRLClient.Get(const AURL, AAccept, AContentType: string; AResponseContent: TStream);
begin
  FHttpClient.Request.Accept := AAccept;
  FHttpClient.Request.ContentType := AContentType;

  DoBeforeCommand;
  FHttpClient.Get(AURL, AResponseContent);
end;

procedure TWiRLClient.Head(const AURL, AAccept, AContentType: string);
begin
  FHttpClient.Request.Accept := AAccept;
  FHttpClient.Request.ContentType := AContentType;

  DoBeforeCommand;
  FHttpClient.Head(AURL);
end;

procedure TWiRLClient.Options(const AURL, AAccept, AContentType: string; AResponse: TStream);
begin
  FHttpClient.Request.Accept := AAccept;
  FHttpClient.Request.ContentType := AContentType;

  DoBeforeCommand;
  FHttpClient.Options(AURL, AResponse);
end;

procedure TWiRLClient.Patch(const AURL, AAccept, AContentType: string; AContent, AResponse: TStream);
begin
  FHttpClient.Request.Accept := AAccept;
  FHttpClient.Request.ContentType := AContentType;

  DoBeforeCommand;
  FHttpClient.Patch(AURL, AContent, AResponse);
end;

procedure TWiRLClient.Post(const AURL, AAccept, AContentType: string; AContent, AResponse: TStream);
begin
  FHttpClient.Request.Accept := AAccept;
  FHttpClient.Request.ContentType := AContentType;

  DoBeforeCommand;
  FHttpClient.Post(AURL, AContent, AResponse);
end;

procedure TWiRLClient.Put(const AURL, AAccept, AContentType: string; AContent, AResponse: TStream);
begin
  FHttpClient.Request.Accept := AAccept;
  FHttpClient.Request.ContentType := AContentType;

  DoBeforeCommand;
  FHttpClient.Put(AURL, AContent, AResponse);
end;

function TWiRLClient.ResponseText: string;
begin
  Result := FHttpClient.ResponseText;
end;

procedure TWiRLClient.SetConnectTimeout(const Value: Integer);
begin
  FHttpClient.ConnectTimeout := Value;
end;

procedure TWiRLClient.SetReadTimeout(const Value: Integer);
begin
  FHttpClient.ReadTimeout := Value;
end;

procedure TWiRLClient.SetProxyParams(const Value: TIdProxyConnectionInfo);
begin
  FHttpClient.ProxyParams := Value;
end;

end.
