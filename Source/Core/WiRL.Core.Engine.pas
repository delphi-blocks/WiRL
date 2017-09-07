{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Engine;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.SyncObjs, System.Diagnostics, System.Rtti,

  WiRL.Core.Classes,
  WiRL.Core.Context,
  WiRL.Rtti.Utils,
  WiRL.Core.Exceptions,
  WiRL.Core.Registry,
  WiRL.Core.Application,
  WiRL.http.URL,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.Core.Attributes,
  WiRL.http.Engines,
  WiRL.http.Server,
  WiRL.http.Accept.MediaType,
  WiRL.http.Filters;

type
  TWiRLEngine = class;

  IWiRLHandleListener = interface
  ['{5C4F450A-1264-449E-A400-DA6C2714FD23}']
  end;

  // Request is a valid resource
  IWiRLHandleRequestEventListener = interface(IWiRLHandleListener)
  ['{969EF9FA-7887-47E6-8996-8B0D6326668E}']
    procedure BeforeHandleRequest(const ASender: TWiRLEngine; const AApplication: TWiRLApplication);
    procedure AfterHandleRequest(const ASender: TWiRLEngine; const AApplication: TWiRLApplication; const AStopWatch: TStopWatch);
  end;

  // Any request even outside the BasePath
  IWiRLHandleRequestEventListenerEx = interface(IWiRLHandleListener)
  ['{45809922-03DB-4B4D-8E2C-64D931978A94}']
    procedure BeforeRequestStart(const ASender: TWiRLEngine; var Handled: Boolean);
    procedure AfterRequestEnd(const ASender: TWiRLEngine; const AStopWatch: TStopWatch);
  end;

  IWiRLHandleExceptionListener = interface(IWiRLHandleListener)
  ['{BDE72935-F73B-4378-8755-01D18EC566B2}']
    procedure HandleException(const ASender: TWiRLEngine; const AApplication: TWiRLApplication; E: Exception);
  end;

  TWiRLApplicationInfo = class
  private
    FEngine: TWiRLEngine;
    FBasePath: string;
    FApplication: TWiRLApplication;
    function GetBasePath: string;
  public
    property Application: TWiRLApplication read FApplication;
    property BasePath: string read GetBasePath;
    constructor Create(AApplication: TWiRLApplication; AEngine: TWiRLEngine);
  end;

  TWiRLApplicationList = class(TObjectList<TWiRLApplicationInfo>)
  private
    FEngine: TWiRLEngine;
  public
    constructor Create(AEngine: TWiRLEngine);
    destructor Destroy; override;

    function TryGetValue(const ABasePath: string; out AApplication: TWiRLApplication): Boolean;
    procedure AddApplication(AApplication: TWiRLApplication);
    procedure RemoveApplication(AApplication: TWiRLApplication);
  end;

  TWiRLEngine = class(TWiRLCustomEngine)
  private
  const
    DefaultDisplayName = 'WiRL Engine';
  private
    class var FServerFileName: string;
    class var FServerDirectory: string;
    class function GetServerDirectory: string; static;
    class function GetServerFileName: string; static;
  private
    FRttiContext: TRttiContext;
    FApplications: TWiRLApplicationList;
    FSubscribers: TList<IWiRLHandleListener>;
    FCriticalSection: TCriticalSection;
    FDisplayName: string;

    // Filter handling
    function ApplyPreMatchingFilters(AContext: TWiRLContext): Boolean;
  protected
    procedure DoBeforeHandleRequest(const AApplication: TWiRLApplication); virtual;
    procedure DoAfterHandleRequest(const AApplication: TWiRLApplication; const AStopWatch: TStopWatch); virtual;
    function DoBeforeRequestStart(): Boolean; virtual;
    procedure DoAfterRequestEnd(const AStopWatch: TStopWatch); virtual;
    procedure DoHandleException(AContext: TWiRLContext; AApplication: TWiRLApplication; E: Exception); virtual;

    // Handles the parent/child relationship for the designer
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Startup; override;
    procedure Shutdown; override;

    procedure HandleRequest(AContext: TWiRLContext); override;

    function AddApplication(const ABasePath: string): TWiRLApplication; overload; virtual;
    function AddApplication(const AName, ABasePath: string; const AResources: TArray<string>): TWiRLApplication; overload; virtual; deprecated;
    procedure AddApplication(AApplication: TWiRLApplication); overload; virtual;
    procedure RemoveApplication(AApplication: TWiRLApplication); virtual;

    function AddSubscriber(const ASubscriber: IWiRLHandleListener): TWiRLEngine;
    function RemoveSubscriber(const ASubscriber: IWiRLHandleListener): TWiRLEngine;

    procedure EnumerateApplications(const ADoSomething: TProc<string, TWiRLApplication>);

    function SetDisplayName(const ADisplayName: string): TWiRLEngine;
    function SetBasePath(const ABasePath: string): TWiRLEngine;

    property Applications: TWiRLApplicationList read FApplications;

    class property ServerFileName: string read GetServerFileName;
    class property ServerDirectory: string read GetServerDirectory;
  published
    property DisplayName: string read FDisplayName write FDisplayName;
  end;

implementation

uses
  System.StrUtils,
  WiRL.Core.Application.Worker,
  WiRL.Core.Utils;

function TWiRLEngine.AddApplication(const AName, ABasePath: string;
  const AResources: TArray<string>): TWiRLApplication;
begin
  Result := Self
    .AddApplication(ABasePath)
    .SetDisplayName(AName)
    .SetResources(AResources);
end;

function TWiRLEngine.AddApplication(const ABasePath: string): TWiRLApplication;
begin
  Result := TWiRLApplication.Create(Self);
  try
    Result.SetBasePath(ABasePath);
    Result.Engine := Self;
  except
    Result.Free;
    raise
  end;
end;

procedure TWiRLEngine.AddApplication(AApplication: TWiRLApplication);
begin
  Applications.AddApplication(AApplication);
end;

function TWiRLEngine.AddSubscriber(const ASubscriber: IWiRLHandleListener): TWiRLEngine;
begin
  FSubscribers.Add(ASubscriber);
  Result := Self;
end;

function TWiRLEngine.ApplyPreMatchingFilters(AContext: TWiRLContext): Boolean;
var
  LRequestFilter: IWiRLContainerRequestFilter;
  LRequestContext: TWiRLContainerRequestContext;
  LAborted: Boolean;
begin
  LAborted := False;
  TWiRLFilterRegistry.Instance.FetchRequestFilter(True,
    procedure (ConstructorInfo: TWiRLFilterConstructorInfo)
    begin
      // The check doesn't have any sense but I must use SUPPORT and I hate using it without a check
      if not Supports(ConstructorInfo.ConstructorFunc(), IWiRLContainerRequestFilter, LRequestFilter) then
        raise EWiRLNotImplementedException.Create(
          Format('Request Filter [%s] does not implement requested interface [IWiRLContainerRequestFilter]', [ConstructorInfo.TypeTClass.ClassName]),
          Self.ClassName, 'ApplyPreMatchingFilters'
        );
      LRequestContext := TWiRLContainerRequestContext.Create(AContext);
      try
        LRequestFilter.Filter(LRequestContext);
        LAborted := LAborted or LRequestContext.Aborted;
      finally
        LRequestContext.Free;
      end;
    end
  );
  Result := LAborted;
end;

constructor TWiRLEngine.Create(AOwner: TComponent);
begin
  inherited;
  TWiRLDebug.LogMessage('TWiRLEngine.Create');
  FRttiContext := TRttiContext.Create;
  FApplications := TWiRLApplicationList.Create(Self);
  FCriticalSection := TCriticalSection.Create;
  FSubscribers := TList<IWiRLHandleListener>.Create;
  FDisplayName := DefaultDisplayName;
  BasePath := '/rest';
end;

destructor TWiRLEngine.Destroy;
begin
  TWiRLDebug.LogMessage('TWiRLEngine.Destroy');
  FCriticalSection.Free;
  FApplications.Free;
  FSubscribers.Free;
  inherited;
end;

procedure TWiRLEngine.DoAfterHandleRequest(const AApplication: TWiRLApplication;
  const AStopWatch: TStopWatch);
var
  LSubscriber: IWiRLHandleListener;
  LHandleRequestEventListener: IWiRLHandleRequestEventListener;
begin
  for LSubscriber in FSubscribers do
    if Supports(LSubscriber, IWiRLHandleRequestEventListener, LHandleRequestEventListener) then
      LHandleRequestEventListener.AfterHandleRequest(Self, AApplication, AStopWatch);
end;

procedure TWiRLEngine.DoAfterRequestEnd(const AStopWatch: TStopWatch);
var
  LSubscriber: IWiRLHandleListener;
  LHandleRequestEventListenerEx: IWiRLHandleRequestEventListenerEx;
begin
  for LSubscriber in FSubscribers do
    if Supports(LSubscriber, IWiRLHandleRequestEventListenerEx, LHandleRequestEventListenerEx) then
      LHandleRequestEventListenerEx.AfterRequestEnd(Self, AStopWatch);
end;

procedure TWiRLEngine.DoBeforeHandleRequest(const AApplication: TWiRLApplication);
var
  LSubscriber: IWiRLHandleListener;
  LHandleRequestEventListener: IWiRLHandleRequestEventListener;
begin
  for LSubscriber in FSubscribers do
    if Supports(LSubscriber, IWiRLHandleRequestEventListener, LHandleRequestEventListener) then
      LHandleRequestEventListener.BeforeHandleRequest(Self, AApplication);
end;

function TWiRLEngine.DoBeforeRequestStart(): Boolean;
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

procedure TWiRLEngine.DoHandleException(AContext: TWiRLContext; AApplication:
    TWiRLApplication; E: Exception);
var
  LSubscriber: IWiRLHandleListener;
  LHandleExceptionListener: IWiRLHandleExceptionListener;
begin
  for LSubscriber in FSubscribers do
    if Supports(LSubscriber, IWiRLHandleExceptionListener, LHandleExceptionListener) then
      LHandleExceptionListener.HandleException(Self, AApplication, E);
end;

procedure TWiRLEngine.EnumerateApplications(
  const ADoSomething: TProc<string, TWiRLApplication>);
var
  LApplicationInfo: TWiRLApplicationInfo;
begin
  if Assigned(ADoSomething) then
  begin
    FCriticalSection.Enter;
    try
      for LApplicationInfo in FApplications do
        ADoSomething(LApplicationInfo.Application.BasePath, LApplicationInfo.Application);
    finally
      FCriticalSection.Leave;
    end;
  end;
end;

procedure TWiRLEngine.HandleRequest(AContext: TWiRLContext);
var
  LApplication: TWiRLApplication;
  LAppWorker: TWiRLApplicationWorker;
  LApplicationPath: string;
  LStopWatch, LStopWatchEx: TStopWatch;
begin
  inherited;
  if EndsText('/favicon.ico', AContext.Request.PathInfo) then
    Exit;
  LApplication := nil;
  LStopWatchEx := TStopwatch.StartNew;
  try
    if not ApplyPreMatchingFilters(AContext) then
    begin
      if not DoBeforeRequestStart() then
      begin
        if Length(AContext.URL.PathTokens) < 1 then
          raise EWiRLNotFoundException.Create(
            Format('Engine [%s] not found. URL [%s]', [BasePath, AContext.URL.BasePath]),
            Self.ClassName, 'HandleRequest'
          );
        LApplicationPath := TWiRLURL.CombinePath([AContext.URL.PathTokens[0]]);
        if (BasePath <> '') and (BasePath <> TWiRLURL.URL_PATH_SEPARATOR) then
        begin
          if not AContext.URL.MatchPath(BasePath + TWiRLURL.URL_PATH_SEPARATOR) then
            raise EWiRLNotFoundException.Create(
              Format('Engine [%s] not found. URL [%s]', [BasePath, AContext.URL.BasePath]),
              Self.ClassName, 'HandleRequest'
            );
          LApplicationPath := TWiRLURL.CombinePath([AContext.URL.PathTokens[0], AContext.URL.PathTokens[1]]);
        end;
        // Change the URI BasePath (?)
        AContext.URL.BasePath := LApplicationPath;

        if FApplications.TryGetValue(LApplicationPath, LApplication) then
        begin
          AContext.Application := LApplication;
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
        end
        else
          raise EWiRLNotFoundException.Create(
            Format('Application [%s] not found. URL [%s]', [LApplicationPath, AContext.URL.URL]),
            Self.ClassName, 'HandleRequest'
          );
      end;
    end;
  except
    on E: Exception do
    begin
      EWiRLWebApplicationException.HandleException(AContext, E);
      DoHandleException(AContext, LApplication, E);
    end;
  end;
  LStopWatchEx.Stop;

  DoAfterRequestEnd(LStopWatchEx);
  AContext.Response.SendHeaders;
end;

procedure TWiRLEngine.RemoveApplication(AApplication: TWiRLApplication);
begin
  FApplications.RemoveApplication(AApplication);
end;

function TWiRLEngine.RemoveSubscriber(const ASubscriber: IWiRLHandleListener): TWiRLEngine;
begin
  FSubscribers.Remove(ASubscriber);
  Result := Self;
end;

function TWiRLEngine.SetBasePath(const ABasePath: string): TWiRLEngine;
begin
  BasePath := ABasePath;
  Result := Self;
end;

function TWiRLEngine.SetDisplayName(const ADisplayName: string): TWiRLEngine;
begin
  FDisplayName := ADisplayName;
  Result := Self;
end;

procedure TWiRLEngine.Shutdown;
var
  LAppInfo: TWiRLApplicationInfo;
begin
  FCriticalSection.Enter;
  try
    for LAppInfo in FApplications do
      LAppInfo.Application.Shutdown;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TWiRLEngine.Startup;
var
  LAppInfo: TWiRLApplicationInfo;
begin
  FCriticalSection.Enter;
  try
    for LAppInfo in FApplications do
      LAppInfo.Application.Startup;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TWiRLEngine.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  LAppInfo: TWiRLApplicationInfo;
begin
  inherited;
  TWiRLDebug.LogMessage('GetChildren start ');
  for LAppInfo in FApplications do
  begin
    TWiRLDebug.LogMessage('app: ' + LAppInfo.Application.BasePath);
    Proc(LAppInfo.Application);
  end;
  TWiRLDebug.LogMessage('GetChildren end');
end;

class function TWiRLEngine.GetServerDirectory: string;
begin
  if FServerDirectory = '' then
    FServerDirectory := ExtractFilePath(ServerFileName);
  Result := FServerDirectory;
end;

class function TWiRLEngine.GetServerFileName: string;
begin
  if FServerFileName = '' then
    FServerFileName := GetModuleName(MainInstance);
  Result := FServerFileName;
end;

{ TWiRLApplicationInfo }

constructor TWiRLApplicationInfo.Create(AApplication: TWiRLApplication;
  AEngine: TWiRLEngine);
begin
  inherited Create;
  FApplication := AApplication;
  FEngine := AEngine;
end;

function TWiRLApplicationInfo.GetBasePath: string;
begin
  if not Assigned(FEngine) then
    raise EWiRLException.Create('Application BasePath: Engine not assigned');
  if FBasePath = '' then
    FBasePath := TWiRLURL.CombinePath([FEngine.BasePath, FApplication.BasePath]);
  Result := FBasePath;
end;

{ TWiRLApplicationList }

procedure TWiRLApplicationList.AddApplication(AApplication: TWiRLApplication);
var
  LAppInfo: TWiRLApplicationInfo;
begin
  LAppInfo := TWiRLApplicationInfo.Create(AApplication, FEngine);
  Add(LAppInfo);
end;

constructor TWiRLApplicationList.Create(AEngine: TWiRLEngine);
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
