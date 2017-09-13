{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.Server;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.StrUtils,

  WiRL.Core.Classes, WiRL.http.Request, WiRL.http.Response,
  WiRL.http.Server.Interfaces, WiRL.Core.Exceptions,
  WiRL.Core.Utils, WiRL.Rtti.Utils, WiRL.Core.Context;

type
  TWiRLhttpServer = class;

  TWiRLCustomEngine = class(TComponent) //abstract
  private
    FBasePath: string;
    FServer: TWiRLhttpServer;
    procedure SetBasePath(const Value: string);
    procedure SetServer(const Value: TWiRLhttpServer);
    procedure FindDefaultServer;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(ABasePath: string); reintroduce; overload;

    procedure HandleRequest(AContext: TWiRLContext); virtual; abstract;
    procedure Startup; virtual;
    procedure Shutdown; virtual;
  published
    property BasePath: string read FBasePath write SetBasePath;
    property Server: TWiRLhttpServer read FServer write SetServer;
  end;

  TWiRLEngineInfo = class
  private
    FOwnsObjects: Boolean;
    FEngine: TWiRLCustomEngine;
  public
    constructor Create(AEngine: TWiRLCustomEngine; AOwnsObjects: Boolean);

    property Engine: TWiRLCustomEngine read FEngine write FEngine;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

  TWiRLEngineList = class(TObjectList<TWiRLEngineInfo>)
  public
    function TryGetValue(const ABasePath: string; out AEngine: TWiRLCustomEngine): Boolean;
  end;

  TWiRLhttpServer = class(TComponent, IWiRLListener)
  private
  const
    DefaultPort = 8080;
    DefaultThreadPoolSize = 50;
  private
    FHttpServer: IWiRLServer;
    FActive: Boolean;
    FServerVendor: string;
    procedure FreeEngines;
    function GetPortProp: Integer;
    procedure SetPortProp(APort: Integer);
    function GetThreadPoolSizeProp: Integer;
    procedure SetThreadPoolSizeProp(const Value: Integer);
    procedure SetServerVendor(const Value: string);
  protected
    FEngines: TWiRLEngineList;
    function GetActive: Boolean; virtual;
    procedure SetActive(const Value: Boolean); virtual;
    procedure Startup;
    procedure Shutdown;
    procedure Loaded; override;
  public
    function AddEngine<T: constructor, TWiRLCustomEngine>(const ABasePath: string; AOwnsObjects: Boolean = True) :T; overload;
    procedure AddEngine(const ABasePath: string; AEngine: TWiRLCustomEngine; AOwnsObjects: Boolean = True); overload;
    function AddEngines(AEngines: TArray<TWirlCustomEngine>; AOwnsObjects: Boolean = True) :TWiRLhttpServer;
    procedure RemoveEngine(AEngine: TWiRLCustomEngine); overload;
    procedure RemoveEngine(const ABasePath: string); overload;
    function GetEngine(const Url: string): TWiRLCustomEngine;
    function SetPort(APort: Integer): TWiRLhttpServer;
    function SetThreadPoolSize(AThreadPoolSize: Integer): TWiRLhttpServer;

    { IWiRLListener }
    procedure HandleRequest(ARequest: TWiRLRequest; AResponse: TWiRLResponse);

    property HttpServer: IWiRLServer read FHttpServer;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active: Boolean read GetActive write SetActive;
    property Port: Integer read GetPortProp write SetPortProp default DefaultPort;
    property ThreadPoolSize: Integer read GetThreadPoolSizeProp write SetThreadPoolSizeProp default DefaultThreadPoolSize;
    property ServerVendor: string read FServerVendor write SetServerVendor;
  end;

implementation

uses
  WiRL.http.Filters;

{ TWiRLhttpServer }

procedure TWiRLhttpServer.AddEngine(const ABasePath: string;
  AEngine: TWiRLCustomEngine; AOwnsObjects: Boolean);
var
  LEngineInfo: TWiRLEngineInfo;
begin
  LEngineInfo := TWiRLEngineInfo.Create(AEngine, AOwnsObjects);
  FEngines.Add(LEngineInfo);
end;

function TWiRLhttpServer.AddEngine<T>(const ABasePath: string; AOwnsObjects: Boolean): T;
begin
  Result := TRttiHelper.CreateInstance(TClass(T), [nil]) as T;
  TWiRLCustomEngine(Result).BasePath := ABasePath;
  AddEngine(ABasePath, Result, AOwnsObjects);
end;

function TWiRLhttpServer.AddEngines(
  AEngines: TArray<TWirlCustomEngine>; AOwnsObjects: Boolean): TWiRLhttpServer;
var
  LEngine: TWirlCustomEngine;
begin
  for LEngine in AEngines do
    AddEngine(LEngine.BasePath, LEngine, AOwnsObjects);
  Result := Self;
end;

constructor TWiRLhttpServer.Create(AOwner: TComponent);
begin
  inherited;
  FEngines := TWiRLEngineList.Create(True);
  FHttpServer := TWiRLServerRegistry.Instance.CreateServer(FServerVendor);
  FHttpServer.Listener := Self;
  FActive := False;
  Port := DefaultPort;
  ThreadPoolSize := DefaultThreadPoolSize;
end;

destructor TWiRLhttpServer.Destroy;
begin
  FreeEngines;
  inherited;
end;

procedure TWiRLhttpServer.FreeEngines;
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

function TWiRLhttpServer.GetActive: Boolean;
begin
  Result := FActive;
end;

function TWiRLhttpServer.GetEngine(const Url: string): TWiRLCustomEngine;
var
  LUrlTokens: TArray<string>;
  LBaseUrl: string;
begin
  Result := nil;
  LUrlTokens := Url.Split(['/']);
  if Length(LUrlTokens) > 1 then
    LBaseUrl := LUrlTokens[1]
  else
    LBaseUrl := '';

  if FEngines.TryGetValue('/' + LBaseUrl, Result) then
    Exit;

  if FEngines.TryGetValue('/', Result) then
    Exit;

  if Url.Equals('/favicon.ico') then
    Abort;

  if not Assigned(Result) then
    raise EWiRLNotFoundException.CreateFmt('Engine not found for URL [%s]', [Url]);
end;

procedure TWiRLhttpServer.HandleRequest(ARequest: TWiRLRequest;
  AResponse: TWiRLResponse);
var
  LContext: TWiRLContext;
  LEngine: TWirlCustomEngine;
begin
  inherited;
  LContext := TWiRLContext.Create;
  try
    LContext.Request := ARequest;
    LContext.Response := AResponse;
    if not TWiRLFilterRegistry.Instance.ApplyPreMatchingRequestFilters(LContext) then
    begin
      LEngine := GetEngine(ARequest.PathInfo);
      LContext.Engine := LEngine;
      LEngine.HandleRequest(LContext);
    end;
    TWiRLFilterRegistry.Instance.ApplyPreMatchingResponseFilters(LContext);
  finally
    LContext.Free;
  end;
end;

procedure TWiRLhttpServer.Loaded;
begin
  inherited;
  if Active then
    Startup;
end;

procedure TWiRLhttpServer.RemoveEngine(const ABasePath: string);
var
  LEngineInfo: TWiRLEngineInfo;
begin
  for LEngineInfo in FEngines do
  begin
    if LEngineInfo.Engine.BasePath = ABasePath then
      RemoveEngine(LEngineInfo.Engine);
  end;
end;

procedure TWiRLhttpServer.RemoveEngine(AEngine: TWiRLCustomEngine);
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

procedure TWiRLhttpServer.SetActive(const Value: Boolean);
begin
  if Value <> Active then
  begin
    FActive := Value;
    // don't listen at design time or during loading
    // (if intersection is an empty set)
    if (componentState * [csDesigning, csLoading]) = [] then
    begin
      if Value then
        Startup
      else
        Shutdown;
    end;
  end;
end;

function TWiRLhttpServer.GetPortProp: Integer;
begin
  Result := FHttpServer.Port;
end;

function TWiRLhttpServer.GetThreadPoolSizeProp: Integer;
begin
  Result := FHttpServer.ThreadPoolSize;
end;

procedure TWiRLhttpServer.SetPortProp(APort: Integer);
begin
  FHttpServer.Port := APort;
end;

procedure TWiRLhttpServer.SetServerVendor(const Value: string);
begin
  if TWiRLServerRegistry.Instance.ContainsKey(Value) then
    FServerVendor := Value
  else
    FServerVendor := '';
end;

function TWiRLhttpServer.SetPort(APort: Integer): TWiRLhttpServer;
begin
  Port := APort;
  Result := Self;
end;

function TWiRLhttpServer.SetThreadPoolSize(AThreadPoolSize: Integer): TWiRLhttpServer;
begin
  ThreadPoolSize := AThreadPoolSize;
  Result := Self;
end;

procedure TWiRLhttpServer.SetThreadPoolSizeProp(const Value: Integer);
begin
  FHttpServer.ThreadPoolSize := Value;
end;

procedure TWiRLhttpServer.Shutdown;
var
  LEngineInfo: TWiRLEngineInfo;
begin
  for LEngineInfo in FEngines do
    LEngineInfo.Engine.Shutdown;
  FHttpServer.Shutdown;
end;

procedure TWiRLhttpServer.Startup;
var
  LEngineInfo: TWiRLEngineInfo;
begin
  for LEngineInfo in FEngines do
    LEngineInfo.Engine.Startup;
  FHttpServer.Startup;
end;

{ TWiRLEngineInfo }

constructor TWiRLEngineInfo.Create(AEngine: TWiRLCustomEngine;
  AOwnsObjects: Boolean);
begin
  inherited Create;
  FEngine := AEngine;
  FOwnsObjects := AOwnsObjects;
end;

{ TWiRLCustomEngine }

constructor TWiRLCustomEngine.Create(AOwner: TComponent);
begin
  inherited;
  FindDefaultServer;
  BasePath := '/';
end;

constructor TWiRLCustomEngine.Create(ABasePath: string);
begin
  Create(nil);
  BasePath := ABasePath;
end;

procedure TWiRLCustomEngine.FindDefaultServer;
var
  LComponent: TComponent;
begin
  if Assigned(Owner) then
  begin
    for LComponent in Owner do
    begin
      if LComponent is TWiRLhttpServer then
      begin
        Server := TWiRLhttpServer(LComponent);
        Exit;
      end;
    end;
  end;
end;

procedure TWiRLCustomEngine.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Server) then
    FServer := nil;
end;

procedure TWiRLCustomEngine.SetBasePath(const Value: string);
begin
  if StartsText('/', Value) then
    FBasePath := Value
  else
    FBasePath := '/' + Value;
end;

procedure TWiRLCustomEngine.SetServer(const Value: TWiRLhttpServer);
begin
  if FServer <> Value then
  begin
    if Assigned(FServer) then
      FServer.RemoveEngine(Self);
    FServer := Value;
    if Assigned(FServer) then
      FServer.AddEngine(BasePath, Self, False);
  end;
end;

procedure TWiRLCustomEngine.Shutdown;
begin
end;

procedure TWiRLCustomEngine.Startup;
begin
  if not Assigned(FServer) then
    raise EWiRLException.Create('Server not assigned');
end;

{ TWiRLEngineList }

function TWiRLEngineList.TryGetValue(const ABasePath: string;
  out AEngine: TWiRLCustomEngine): Boolean;
var
  LEngineInfo: TWiRLEngineInfo;
begin
  Result := False;
  for LEngineInfo in Self do
  begin
    if LEngineInfo.Engine.BasePath = ABasePath then
    begin
      AEngine := LEngineInfo.Engine;
      Exit(True);
    end;
  end;
end;

end.
