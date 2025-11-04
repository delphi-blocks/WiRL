{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2024 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.Server;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.StrUtils,

  WiRL.Core.Classes,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.http.Server.Interfaces,
  WiRL.Core.Exceptions,
  WiRL.Core.Utils,
  WiRL.Rtti.Utils,
  WiRL.Engine.Core,
  WiRL.Core.Context.Server;

type

  TWiRLServer = class(TComponent, IWiRLListener)
  private const
    DefaultPort = 8080;
    DefaultThreadPoolSize = 0;
  private
    FServerName: string;
    FCurrentEngine: TWiRLCustomEngine;
    FHttpServer: IWiRLServer;
    FActive: Boolean;
    FServerVendor: string;
    FEngineList: TWiRLEngineList;
    FSinkPaths: TList<string>;
    procedure FreeEngines;
    function GetPortProp: Integer;
    procedure SetPortProp(APort: Integer);
    function GetThreadPoolSizeProp: Integer;
    procedure SetThreadPoolSizeProp(const Value: Integer);
    procedure SetServerVendor(const Value: string);
    function GetServerImplementation: TObject;
  protected
    FEngines: TWiRLEngineRegistry;
    function GetActive: Boolean; virtual;
    procedure SetActive(const Value: Boolean); virtual;
    procedure Startup;
    procedure Shutdown;
    procedure Loaded; override;
  public
    function AddEngine<T: constructor, TWiRLCustomEngine>(const ABasePath: string; AOwnsObjects: Boolean = True): T; overload;
    procedure AddEngine(const ABasePath: string; AEngine: TWiRLCustomEngine; AOwnsObjects: Boolean = True); overload;
    function AddEngines(AEngines: TArray<TWiRLCustomEngine>; AOwnsObjects: Boolean = True): TWiRLServer;
    procedure RemoveEngine(AEngine: TWiRLCustomEngine); overload;
    procedure RemoveEngine(const ABasePath: string); overload;
    function GetEngine(const AURL: string): TWiRLCustomEngine;
    function FindEngine(const AURL: string): TWiRLCustomEngine;
    function CurrentEngine<T: constructor, TWiRLCustomEngine>: T;
    function SetServerName(const AName: string): TWiRLServer;
    function SetPort(APort: Integer): TWiRLServer;
    function SetThreadPoolSize(AThreadPoolSize: Integer): TWiRLServer;
    function AddSinkPath(const APath: string): TWiRLServer;

    { IWiRLListener }
    procedure HandleRequest(AContext: TWiRLContext; ARequest: TWiRLRequest; AResponse: TWiRLResponse);

    property HttpServer: IWiRLServer read FHttpServer;
    property Engines: TWiRLEngineList read FEngineList;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active: Boolean read GetActive write SetActive;
    property Port: Integer read GetPortProp write SetPortProp default DefaultPort;
    property SinkPaths: TList<string> read FSinkPaths write FSinkPaths;
    property ThreadPoolSize: Integer read GetThreadPoolSizeProp write SetThreadPoolSizeProp default DefaultThreadPoolSize;
    property ServerName: string read FServerName write FServerName;
    property ServerVendor: string read FServerVendor write SetServerVendor;
    property ServerImplementation: TObject read GetServerImplementation;
  end;

implementation

uses
  WiRL.http.Filters;

{ TWiRLServer }

procedure TWiRLServer.AddEngine(const ABasePath: string;
  AEngine: TWiRLCustomEngine; AOwnsObjects: Boolean);
var
  LEngineInfo: TWiRLEngineInfo;
begin
  LEngineInfo := TWiRLEngineInfo.Create(AEngine, AOwnsObjects);
  FEngines.Add(LEngineInfo);
  AEngine.SetEngineServer(Self);
end;

function TWiRLServer.AddEngine<T>(const ABasePath: string; AOwnsObjects: Boolean): T;
begin
  Result := TRttiHelper.CreateInstance(TClass(T), [nil]) as T;
  TWiRLCustomEngine(Result).BasePath := ABasePath;
  AddEngine(ABasePath, Result, AOwnsObjects);
  FCurrentEngine := Result;
end;

function TWiRLServer.AddEngines(
  AEngines: TArray<TWiRLCustomEngine>; AOwnsObjects: Boolean): TWiRLServer;
var
  LEngine: TWiRLCustomEngine;
begin
  for LEngine in AEngines do
    AddEngine(LEngine.BasePath, LEngine, AOwnsObjects);
  Result := Self;
end;

function TWiRLServer.AddSinkPath(const APath: string): TWiRLServer;
begin
  FSinkPaths.Add(APath);
  Result := Self;
end;

constructor TWiRLServer.Create(AOwner: TComponent);
begin
  inherited;
  FSinkPaths := TList<string>.Create;
  FEngineList := TWiRLEngineList.Create(Self.FEngines);
  FEngines := TWiRLEngineRegistry.Create(True);
  FHttpServer := TWiRLServerRegistry.Instance.CreateServer(FServerVendor);
  FHttpServer.Listener := Self;
  FActive := False;
  FServerName := 'WiRL Server';
  Port := DefaultPort;
  ThreadPoolSize := DefaultThreadPoolSize;
end;

function TWiRLServer.CurrentEngine<T>: T;
begin
  Result := FCurrentEngine as T;
end;

destructor TWiRLServer.Destroy;
begin
  FSinkPaths.Free;
  FEngineList.Free;
  FreeEngines;
  inherited;
end;

function TWiRLServer.FindEngine(const AURL: string): TWiRLCustomEngine;
begin
  Result := FEngines.GetEngine(AURL);
end;

procedure TWiRLServer.FreeEngines;
var
  LEngineInfo: TWiRLEngineInfo;
begin
  for LEngineInfo in FEngines do
  begin
    if LEngineInfo.OwnsObjects then
      LEngineInfo.Engine.Free;
//    LPair.Value.Free;
  end;
  FEngines.Free;
end;

function TWiRLServer.GetActive: Boolean;
begin
  Result := FActive;
end;

function TWiRLServer.GetEngine(const AURL: string): TWiRLCustomEngine;
begin
  Result := FEngines.GetEngine(AURL);

  if not Assigned(Result) then
    raise EWiRLNotFoundException.Create(Format('Engine not found for URL [%s]', [AURL]));
end;

procedure TWiRLServer.HandleRequest(AContext: TWiRLContext; ARequest: TWiRLRequest; AResponse: TWiRLResponse);
var
  LEngine: TWiRLCustomEngine;
  LPath: string;
begin
  for LPath in FSinkPaths do
    if LPath = ARequest.PathInfo then
    begin
      AResponse.StatusCode := 204; //404?
      Exit;
    end;

  AContext.Server := Self;
  AContext.Request := ARequest;
  AContext.Response := AResponse;
  try
    try
      if not TWiRLFilterRegistry.Instance.ApplyPreMatchingRequestFilters(AContext) then
      begin
        LEngine := GetEngine(ARequest.PathInfo);
        AContext.Engine := LEngine;
        LEngine.HandleRequest(AContext);
      end;
      TWiRLFilterRegistry.Instance.ApplyPreMatchingResponseFilters(AContext);
    except
      on E: Exception do
        EWiRLWebApplicationException.HandleException(AContext, E);
    end;
  finally
    AResponse.SendHeaders;
  end;
end;

procedure TWiRLServer.Loaded;
begin
  inherited;
  if Active then
    Startup;
end;

procedure TWiRLServer.RemoveEngine(const ABasePath: string);
var
  LEngineInfo: TWiRLEngineInfo;
begin
  for LEngineInfo in FEngines do
  begin
    if LEngineInfo.Engine.BasePath = ABasePath then
      RemoveEngine(LEngineInfo.Engine);
  end;
end;

procedure TWiRLServer.RemoveEngine(AEngine: TWiRLCustomEngine);
var
  LEngineInfo: TWiRLEngineInfo;
begin
  for LEngineInfo in FEngines do
  begin
    if LEngineInfo.Engine = AEngine then
    begin
      if LEngineInfo.OwnsObjects then
        LEngineInfo.Engine.Free;
      FEngines.Remove(LEngineInfo);
    end;
  end;
end;

procedure TWiRLServer.SetActive(const Value: Boolean);
begin
  if Value <> Active then
  begin
    FActive := Value;
    // don't listen at design time or during loading
    // (if intersection is an empty set)
    if (ComponentState * [csDesigning, csLoading]) = [] then
    begin
      if Value then
        Startup
      else
        Shutdown;
    end;
  end;
end;

function TWiRLServer.SetServerName(const AName: string): TWiRLServer;
begin
  FServerName := AName;
  Result := Self;
end;

function TWiRLServer.GetPortProp: Integer;
begin
  Result := FHttpServer.Port;
end;

function TWiRLServer.GetServerImplementation: TObject;
begin
  Result := FHttpServer.ServerImplementation;
end;

function TWiRLServer.GetThreadPoolSizeProp: Integer;
begin
  Result := FHttpServer.ThreadPoolSize;
end;

procedure TWiRLServer.SetPortProp(APort: Integer);
begin
  FHttpServer.Port := APort;
end;

procedure TWiRLServer.SetServerVendor(const Value: string);
begin
  if TWiRLServerRegistry.Instance.ContainsKey(Value) then
    FServerVendor := Value
  else
    FServerVendor := '';
end;

function TWiRLServer.SetPort(APort: Integer): TWiRLServer;
begin
  Port := APort;
  Result := Self;
end;

function TWiRLServer.SetThreadPoolSize(AThreadPoolSize: Integer): TWiRLServer;
begin
  ThreadPoolSize := AThreadPoolSize;
  Result := Self;
end;

procedure TWiRLServer.SetThreadPoolSizeProp(const Value: Integer);
begin
  FHttpServer.ThreadPoolSize := Value;
end;

procedure TWiRLServer.Shutdown;
var
  LEngineInfo: TWiRLEngineInfo;
begin
  for LEngineInfo in FEngines do
    LEngineInfo.Engine.Shutdown;

  FHttpServer.Shutdown;
end;

procedure TWiRLServer.Startup;
var
  LEngineInfo: TWiRLEngineInfo;
begin
  for LEngineInfo in FEngines do
    LEngineInfo.Engine.Startup;
  FHttpServer.Startup;
end;

end.
