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
  SysUtils, Classes

{$ifdef DelphiXE7_UP}
   , System.Threading
{$endif}

  // Indy
  , IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP

  ;

type
  {$ifdef DelphiXE2_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidAndroid)]
  {$endif}
  TWiRLClient = class(TComponent)
  private
    FHttpClient: TIdHTTP;
    FWiRLEngineURL: string;

{$ifdef DelphiXE7_UP}
    FWorkerTask: ITask;
{$endif}
    function GetRequest: TIdHTTPRequest;
    function GetResponse: TIdHTTPResponse;
    function GetConnectTimeout: Integer;
    function GetReadTimeout: Integer;
    procedure SetConnectTimeout(const Value: Integer);
    procedure SetReadTimeout(const Value: Integer);
  protected
{$ifdef DelphiXE7_UP}
    property WorkerTask: ITask read FWorkerTask;
{$endif}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Delete(const AURL: string; AResponseContent: TStream);
    procedure Get(const AURL: string; AResponseContent: TStream; const AAccept: string);
    procedure Post(const AURL: string; AContent, AResponse: TStream);
    procedure Put(const AURL: string; AContent, AResponse: TStream);
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

procedure TWiRLClient.Delete(const AURL: string; AResponseContent: TStream);
begin
  FHttpClient.Delete(AURL, AResponseContent);
end;

destructor TWiRLClient.Destroy;
begin
  FHttpClient.Free;
  inherited;
end;

procedure TWiRLClient.ExecuteAsync(const AProc: TProc);
begin
{$ifdef DelphiXE7_UP}
  if IsRunningAsync then
    raise Exception.Create('Multiple async execution not yet supported');
  FWorkerTask := TTask.Create(AProc).Start;
{$else}
  raise Exception.Create('Async execution not yet supported');
{$endif}
end;

procedure TWiRLClient.Get(const AURL: string; AResponseContent: TStream;
  const AAccept: string);
begin
  FHttpClient.Request.Accept := AAccept;
  FHttpClient.Get(AURL, AResponseContent);
end;

function TWiRLClient.GetConnectTimeout: Integer;
begin
  Result := FHttpClient.ConnectTimeout;
end;

function TWiRLClient.GetReadTimeout: Integer;
begin
  Result := FHttpClient.ReadTimeout;
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
{$ifdef DelphiXE7_UP}
  Result := Assigned(FWorkerTask) and (FWorkerTask.Status < TTaskStatus.Completed);
{$else}
  Result := False;
{$endif}
end;

function TWiRLClient.LastCmdSuccess: Boolean;
begin
  Result := FHttpClient.ResponseCode = 200;
end;

procedure TWiRLClient.Post(const AURL: string; AContent, AResponse: TStream);
begin
  FHttpClient.Post(AURL, AContent, AResponse);
end;

procedure TWiRLClient.Put(const AURL: string; AContent, AResponse: TStream);
begin
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

end.
