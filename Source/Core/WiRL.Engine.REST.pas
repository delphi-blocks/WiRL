{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2024 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Engine.REST;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.SyncObjs, System.Diagnostics, System.Rtti,

  WiRL.Configuration.Core,
  WiRL.Core.Classes,
  WiRL.Core.Context.Server,
  WiRL.Rtti.Utils,
  WiRL.Core.Exceptions,
  WiRL.Core.Registry,
  WiRL.Core.Application,
  WiRL.http.URL,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.Core.Attributes,
  WiRL.http.Server,
  WiRL.http.Accept.MediaType,
  WiRL.http.Filters,
  WiRL.Engine.Core;

type
  TWiRLRESTEngine = class;

  IWiRLHandleListener = interface
  ['{5C4F450A-1264-449E-A400-DA6C2714FD23}']
  end;

  // Request is a valid resource
  IWiRLHandleRequestEventListener = interface(IWiRLHandleListener)
  ['{969EF9FA-7887-47E6-8996-8B0D6326668E}']
    procedure BeforeHandleRequest(const ASender: TWiRLRESTEngine; const AApplication: TWiRLApplication);
    procedure AfterHandleRequest(const ASender: TWiRLRESTEngine; const AApplication: TWiRLApplication; const AStopWatch: TStopWatch);
  end;

  // Any request even outside the BasePath
  IWiRLHandleRequestEventListenerEx = interface(IWiRLHandleListener)
  ['{45809922-03DB-4B4D-8E2C-64D931978A94}']
    procedure BeforeRequestStart(const ASender: TWiRLRESTEngine; var Handled: Boolean);
    procedure AfterRequestEnd(const ASender: TWiRLRESTEngine; const AStopWatch: TStopWatch);
  end;

  IWiRLHandleExceptionListener = interface(IWiRLHandleListener)
  ['{BDE72935-F73B-4378-8755-01D18EC566B2}']
    procedure HandleException(const ASender: TWiRLRESTEngine; const AApplication: TWiRLApplication; E: Exception);
  end;

  TWiRLApplicationInfo = class
  private
    FEngine: TWiRLRESTEngine;
    FApplication: TWiRLApplication;
    function GetBasePath: string;
  public
    property Application: TWiRLApplication read FApplication;
    property BasePath: string read GetBasePath;
    constructor Create(AApplication: TWiRLApplication; AEngine: TWiRLRESTEngine);
  end;

  TWiRLApplicationList = class(TObjectList<TWiRLApplicationInfo>)
  private
    FEngine: TWiRLRESTEngine;
  public
    constructor Create(AEngine: TWiRLRESTEngine);
    destructor Destroy; override;

    function TryGetValue(const ABasePath: string; out AApplication: TWiRLApplication): Boolean;
    procedure AddApplication(AApplication: TWiRLApplication);
    procedure RemoveApplication(AApplication: TWiRLApplication);
  end;

  TWiRLRESTEngine = class(TWiRLCustomEngine)
  private const
    DefaultEngineName = 'WiRL REST Engine';
  private
    class var FServerFileName: string;
    class var FServerDirectory: string;
    class function GetServerDirectory: string; static;
    class function GetServerFileName: string; static;
  private
    FCurrentApp: IWiRLApplication;
    FApplications: TWiRLApplicationList;
    FSubscribers: TList<IWiRLHandleListener>;
  protected
    procedure DoBeforeHandleRequest(const AApplication: TWiRLApplication); virtual;
    procedure DoAfterHandleRequest(const AApplication: TWiRLApplication; const AStopWatch: TStopWatch); virtual;
    function DoBeforeRequestStart(): Boolean; virtual;
    procedure DoAfterRequestEnd(const AStopWatch: TStopWatch); virtual;
    procedure DoHandleException(AContext: TWiRLContext; AApplication: TWiRLApplication; E: Exception); virtual;
    procedure DefineProperties(Filer: TFiler); override;

    // Handles the parent/child relationship for the designer
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Startup; override;
    procedure Shutdown; override;

    function GetApplication(AURL: TWiRLURL): TWiRLApplication;
    function GetApplicationByName(const AName: string): TWiRLApplication;

    procedure HandleRequest(AContext: TWiRLContext); override;
    procedure HandleException(AContext: TWiRLContext; E: Exception);

    function AddApplication(const ABasePath: string): IWiRLApplication; overload; virtual;
    function AddApplication(const AName, ABasePath: string; const AResources: TArray<string>): IWiRLApplication; overload; virtual; deprecated;
    procedure AddApplication(AApplication: IWiRLApplication); overload; virtual;
    procedure RemoveApplication(AApplication: IWiRLApplication); virtual;
    function CurrentApp: IWiRLApplication;

    function AddSubscriber(const ASubscriber: IWiRLHandleListener): TWiRLRESTEngine;
    function RemoveSubscriber(const ASubscriber: IWiRLHandleListener): TWiRLRESTEngine;

    procedure EnumerateApplications(const ADoSomething: TProc<string, TWiRLApplication>);

    function SetEngineName(const AEngineName: string): TWiRLRESTEngine;
    function SetBasePath(const ABasePath: string): TWiRLRESTEngine;

    class property ServerFileName: string read GetServerFileName;
    class property ServerDirectory: string read GetServerDirectory;
  published
    property Applications: TWiRLApplicationList read FApplications write FApplications;
  end;

implementation

uses
  System.StrUtils,
  WiRL.Core.Application.Worker,
  WiRL.Core.Utils;

function TWiRLRESTEngine.AddApplication(const AName, ABasePath: string;
  const AResources: TArray<string>): IWiRLApplication;
begin
  Result := Self
    .AddApplication(ABasePath)
    .SetAppName(AName)
    .SetResources(AResources);
end;

function TWiRLRESTEngine.AddApplication(const ABasePath: string): IWiRLApplication;
var
  LApplication: TWiRLApplication;
begin
  LApplication := TWiRLApplication.Create(Self);
  try
    LApplication.SetBasePath(ABasePath);
    LApplication.Engine := Self;
    LApplication.AppName := 'App';
    FCurrentApp := LApplication;
  except
    LApplication.Free;
    raise
  end;
  Result := LApplication;
end;

procedure TWiRLRESTEngine.AddApplication(AApplication: IWiRLApplication);
begin
  Applications.AddApplication(AApplication as TWiRLApplication);
end;

function TWiRLRESTEngine.AddSubscriber(const ASubscriber: IWiRLHandleListener): TWiRLRESTEngine;
begin
  FSubscribers.Add(ASubscriber);
  Result := Self;
end;

constructor TWiRLRESTEngine.Create(AOwner: TComponent);
begin
  inherited;
  FApplications := TWiRLApplicationList.Create(Self);
  FSubscribers := TList<IWiRLHandleListener>.Create;
  FEngineName := DefaultEngineName;
  BasePath := '/rest';
end;

function TWiRLRESTEngine.CurrentApp: IWiRLApplication;
begin
  if not Assigned(FCurrentApp) then
    raise EWiRLServerException.Create('No current application defined');
  Result := FCurrentApp;
end;

procedure TWiRLRESTEngine.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Applications', nil, nil, FApplications.Count > 0);
end;

destructor TWiRLRESTEngine.Destroy;
begin
  FCurrentApp := nil;
  FApplications.Free;
  FSubscribers.Free;
  inherited;
end;

procedure TWiRLRESTEngine.DoAfterHandleRequest(const AApplication: TWiRLApplication;
  const AStopWatch: TStopWatch);
var
  LSubscriber: IWiRLHandleListener;
  LHandleRequestEventListener: IWiRLHandleRequestEventListener;
begin
  for LSubscriber in FSubscribers do
    if Supports(LSubscriber, IWiRLHandleRequestEventListener, LHandleRequestEventListener) then
      LHandleRequestEventListener.AfterHandleRequest(Self, AApplication, AStopWatch);
end;

procedure TWiRLRESTEngine.DoAfterRequestEnd(const AStopWatch: TStopWatch);
var
  LSubscriber: IWiRLHandleListener;
  LHandleRequestEventListenerEx: IWiRLHandleRequestEventListenerEx;
begin
  for LSubscriber in FSubscribers do
    if Supports(LSubscriber, IWiRLHandleRequestEventListenerEx, LHandleRequestEventListenerEx) then
      LHandleRequestEventListenerEx.AfterRequestEnd(Self, AStopWatch);
end;

procedure TWiRLRESTEngine.DoBeforeHandleRequest(const AApplication: TWiRLApplication);
var
  LSubscriber: IWiRLHandleListener;
  LHandleRequestEventListener: IWiRLHandleRequestEventListener;
begin
  for LSubscriber in FSubscribers do
    if Supports(LSubscriber, IWiRLHandleRequestEventListener, LHandleRequestEventListener) then
      LHandleRequestEventListener.BeforeHandleRequest(Self, AApplication);
end;

function TWiRLRESTEngine.DoBeforeRequestStart(): Boolean;
var
  LSubscriber: IWiRLHandleListener;
  LHandleRequestEventListenerEx: IWiRLHandleRequestEventListenerEx;
begin
  Result := False;
  for LSubscriber in FSubscribers do
    if Supports(LSubscriber, IWiRLHandleRequestEventListenerEx, LHandleRequestEventListenerEx) then
    begin
      LHandleRequestEventListenerEx.BeforeRequestStart(Self, Result);
      if Result then
        Break;
    end;
end;

procedure TWiRLRESTEngine.DoHandleException(AContext: TWiRLContext; AApplication:
    TWiRLApplication; E: Exception);
var
  LSubscriber: IWiRLHandleListener;
  LHandleExceptionListener: IWiRLHandleExceptionListener;
begin
  for LSubscriber in FSubscribers do
    if Supports(LSubscriber, IWiRLHandleExceptionListener, LHandleExceptionListener) then
      LHandleExceptionListener.HandleException(Self, AApplication, E);
end;

procedure TWiRLRESTEngine.EnumerateApplications(
  const ADoSomething: TProc<string, TWiRLApplication>);
var
  LApplicationInfo: TWiRLApplicationInfo;
begin
  if Assigned(ADoSomething) then
  begin
    TMonitor.Enter(Self);
    try
      for LApplicationInfo in FApplications do
        ADoSomething(LApplicationInfo.Application.BasePath, LApplicationInfo.Application);
    finally
      TMonitor.Exit(Self);
    end;
  end;
end;

function TWiRLRESTEngine.GetApplication(AURL: TWiRLURL): TWiRLApplication;
var
  LApplicationPath: string;
begin
  Result := nil;

  if Length(AURL.PathTokens) < 1 then
    raise EWiRLNotFoundException.Create(
      Format('Engine [%s] not found. URL [%s]', [BasePath, AURL.BasePath]),
      Self.ClassName, 'GetApplication'
    );
  LApplicationPath := TWiRLURL.CombinePath([AURL.PathTokens[0]]);
  if (BasePath <> '') and (BasePath <> TWiRLURL.URL_PATH_SEPARATOR) then
  begin
    if not AURL.MatchPath(BasePath + TWiRLURL.URL_PATH_SEPARATOR) then
      raise EWiRLNotFoundException.Create(
        Format('Engine [%s] not found. URL [%s]', [BasePath, AURL.BasePath]),
        Self.ClassName, 'GetApplication'
      );
    LApplicationPath := TWiRLURL.CombinePath([AURL.PathTokens[0], AURL.PathTokens[1]]);
  end;

  if FApplications.TryGetValue(LApplicationPath, Result) then
  begin
    AURL.BasePath := LApplicationPath;
    Exit;
  end;

  // If it doesn't find a specific Application try with the default one
  if not FApplications.TryGetValue(TWiRLURL.StripFirstPathDelimiter(BasePath), Result) then
  begin
    raise EWiRLNotFoundException.Create(
      Format('Application [%s] not found. URL [%s]', [LApplicationPath, AURL.URL]),
      Self.ClassName, 'GetApplication'
    );
  end;
  AURL.BasePath := BasePath;
end;

function TWiRLRESTEngine.GetApplicationByName(const AName: string): TWiRLApplication;
var
  LApp: TWiRLApplicationInfo;
begin
  Result := nil;
  for LApp in FApplications do
  begin
    if LApp.Application.AppName = AName then
    begin
      Result := LApp.Application;
      Break;
    end;
  end;
end;

procedure TWiRLRESTEngine.HandleException(AContext: TWiRLContext; E: Exception);
begin
  if Assigned(AContext.Application) then
    DoHandleException(AContext, AContext.Application as TWiRLApplication, E);
end;

procedure TWiRLRESTEngine.HandleRequest(AContext: TWiRLContext);
var
  LApplication: TWiRLApplication;
  LAppWorker: TWiRLApplicationWorker;
  LStopWatch, LStopWatchEx: TStopWatch;
begin
  inherited;

  LStopWatchEx := TStopwatch.StartNew;
  try
    if not DoBeforeRequestStart() then
    begin
      LApplication := GetApplication(AContext.RequestURL);
      AContext.Application := LApplication;

      if not TWiRLFilterRegistry.Instance.ApplyPreMatchingResourceFilters(AContext) then
      begin
        LAppWorker := TWiRLApplicationWorker.Create(AContext);
        try
          DoBeforeHandleRequest(LApplication);
          LStopWatch := TStopwatch.StartNew;
          LAppWorker.HandleRequest;
          LStopWatch.Stop;
          DoAfterHandleRequest(LApplication, LStopWatch);
        finally
          LStopWatch.Stop;
          LAppWorker.Free;
        end;
      end;
    end;
  except
    on E: Exception do
    begin
      EWiRLWebApplicationException.HandleException(AContext, E);
    end;
  end;
  LStopWatchEx.Stop;

  DoAfterRequestEnd(LStopWatchEx);
end;

procedure TWiRLRESTEngine.RemoveApplication(AApplication: IWiRLApplication);
begin
  FApplications.RemoveApplication(AApplication as TWiRLApplication);
end;

function TWiRLRESTEngine.RemoveSubscriber(const ASubscriber: IWiRLHandleListener): TWiRLRESTEngine;
begin
  FSubscribers.Remove(ASubscriber);
  Result := Self;
end;

function TWiRLRESTEngine.SetBasePath(const ABasePath: string): TWiRLRESTEngine;
begin
  BasePath := ABasePath;
  Result := Self;
end;

function TWiRLRESTEngine.SetEngineName(const AEngineName: string): TWiRLRESTEngine;
begin
  FEngineName := AEngineName;
  Result := Self;
end;

procedure TWiRLRESTEngine.Shutdown;
var
  LAppInfo: TWiRLApplicationInfo;
begin
  inherited;

  TMonitor.Enter(Self);
  try
    for LAppInfo in FApplications do
      LAppInfo.Application.Shutdown;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TWiRLRESTEngine.Startup;
var
  LAppInfo: TWiRLApplicationInfo;
begin
  inherited;

  TMonitor.Enter(Self);
  try
    for LAppInfo in FApplications do
      LAppInfo.Application.Startup;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TWiRLRESTEngine.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  LAppInfo: TWiRLApplicationInfo;
begin
  inherited;
  for LAppInfo in FApplications do
  begin
    Proc(LAppInfo.Application);
  end;
end;

class function TWiRLRESTEngine.GetServerDirectory: string;
begin
  if FServerDirectory = '' then
    FServerDirectory := ExtractFilePath(ServerFileName);
  Result := FServerDirectory;
end;

class function TWiRLRESTEngine.GetServerFileName: string;
begin
  if FServerFileName = '' then
    FServerFileName := GetModuleName(MainInstance);
  Result := FServerFileName;
end;

{ TWiRLApplicationInfo }

constructor TWiRLApplicationInfo.Create(AApplication: TWiRLApplication; AEngine: TWiRLRESTEngine);
begin
  inherited Create;
  FApplication := AApplication;
  FEngine := AEngine;
end;

function TWiRLApplicationInfo.GetBasePath: string;
begin
  Result := FApplication.Path;
end;

{ TWiRLApplicationList }

procedure TWiRLApplicationList.AddApplication(AApplication: TWiRLApplication);
var
  LAppInfo: TWiRLApplicationInfo;
begin
  LAppInfo := TWiRLApplicationInfo.Create(AApplication, FEngine);
  Add(LAppInfo);
end;

constructor TWiRLApplicationList.Create(AEngine: TWiRLRESTEngine);
begin
  inherited Create(True);
  FEngine := AEngine;
end;

destructor TWiRLApplicationList.Destroy;
var
  LAppInfo: TWiRLApplicationInfo;
begin
  for LAppInfo in Self do
    FreeAndNil(LAppInfo.FApplication);
  inherited;
end;

procedure TWiRLApplicationList.RemoveApplication(
  AApplication: TWiRLApplication);
var
  LAppInfo: TWiRLApplicationInfo;
begin
  for LAppInfo in Self do
  begin
    if LAppInfo.Application = AApplication then
    begin
//      if LAppInfo.OwnsObject then
//        LAppInfo.Application.Free;
      Remove(LAppInfo);
      Exit;
    end;
  end;
end;

function TWiRLApplicationList.TryGetValue(const ABasePath: string;
  out AApplication: TWiRLApplication): Boolean;
var
  LAppInfo: TWiRLApplicationInfo;
begin
  Result := False;
  for LAppInfo in Self do
  begin
    if LAppInfo.BasePath = ABasePath then
    begin
      AApplication := LAppInfo.Application;
      Exit(True);
    end;
  end;
end;

end.
