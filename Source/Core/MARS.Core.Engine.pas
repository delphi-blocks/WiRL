(*
  Copyright 2015-2016, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit MARS.Core.Engine;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.SyncObjs, System.Diagnostics, System.Rtti,

  MARS.Core.Classes,
  MARS.Rtti.Utils,
  MARS.Core.MediaType,
  MARS.Core.Exceptions,
  MARS.Core.Registry,
  MARS.Core.Application,
  MARS.Core.URL,
  MARS.Core.Request,
  MARS.Core.Response,
  MARS.Core.Attributes,
  MARS.http.Filters;

{$M+}

type
  TMARSEngine = class;

  IMARSHandleListener = interface
  ['{5C4F450A-1264-449E-A400-DA6C2714FD23}']
  end;

  // Request is a valid resource
  IMARSHandleRequestEventListener = interface(IMARSHandleListener)
  ['{969EF9FA-7887-47E6-8996-8B0D6326668E}']
    procedure BeforeHandleRequest(const ASender: TMARSEngine; const AApplication: TMARSApplication);
    procedure AfterHandleRequest(const ASender: TMARSEngine; const AApplication: TMARSApplication; const AStopWatch: TStopWatch);
  end;

  // Any request even outside the BasePath
  IMARSHandleRequestEventListenerEx = interface(IMARSHandleListener)
  ['{45809922-03DB-4B4D-8E2C-64D931978A94}']
    procedure BeforeRequestStart(const ASender: TMARSEngine; var Handled: Boolean);
    procedure AfterRequestEnd(const ASender: TMARSEngine; const AStopWatch: TStopWatch);
  end;

  IMARSHandleExceptionListener = interface(IMARSHandleListener)
  ['{BDE72935-F73B-4378-8755-01D18EC566B2}']
    procedure HandleException(const ASender: TMARSEngine; const AApplication: TMARSApplication; E: Exception);
  end;

  TMARSEngine = class
  private
    class threadvar FRequest: TMARSRequest;
    class threadvar FResponse: TMARSResponse;
    class threadvar FURL: TMARSURL;
  private
    class var FServerFileName: string;
    class var FServerDirectory: string;
    class function GetServerDirectory: string; static;
    class function GetServerFileName: string; static;
  private
    FRttiContext: TRttiContext;
    FApplications: TMARSApplicationDictionary;
    FSubscribers: TList<IMARSHandleListener>;
    FCriticalSection: TCriticalSection;
    FBasePath: string;
    FPort: Integer;
    FThreadPoolSize: Integer;
    FName: string;

    function GetURL: TMARSURL;
    function GetRequest: TMARSRequest;
    function GetResponse: TMARSResponse;

    // Filter handling
    procedure ApplyPreMatchingFilters;

  protected
    procedure DoBeforeHandleRequest(const AApplication: TMARSApplication); virtual;
    procedure DoAfterHandleRequest(const AApplication: TMARSApplication; const AStopWatch: TStopWatch); virtual;
    function DoBeforeRequestStart() :Boolean; virtual;
    procedure DoAfterRequestEnd(const AStopWatch: TStopWatch); virtual;
    procedure DoHandleException(const AApplication: TMARSApplication; E: Exception); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure HandleRequest(ARequest: TMARSRequest; AResponse: TMARSResponse);

    function AddApplication(const ABasePath: string): TMARSApplication; overload; virtual;
    function AddApplication(const AName, ABasePath: string; const AResources: array of string): TMARSApplication; overload; virtual; deprecated;

    function AddSubscriber(const ASubscriber: IMARSHandleListener): TMARSEngine;
    function RemoveSubscriber(const ASubscriber: IMARSHandleListener): TMARSEngine;

    procedure EnumerateApplications(const ADoSomething: TProc<string, TMARSApplication>);

    function SetName(const AName: string): TMARSEngine;
    function SetBasePath(const ABasePath: string): TMARSEngine;
    function SetPort(APort: Integer): TMARSEngine;
    function SetThreadPoolSize(AThreadPoolSize: Integer): TMARSEngine;

    property Applications: TMARSApplicationDictionary read FApplications;
    property BasePath: string read FBasePath write FBasePath;
    property Name: string read FName write FName;
    property Port: Integer read FPort write FPort;
    property ThreadPoolSize: Integer read FThreadPoolSize write FThreadPoolSize;

    property CurrentRequest: TMARSRequest read GetRequest;
    property CurrentResponse: TMARSResponse read GetResponse;
    property CurrentURL: TMARSURL read GetURL;

    class property ServerFileName: string read GetServerFileName;
    class property ServerDirectory: string read GetServerDirectory;
  end;

implementation

uses
  System.StrUtils,
  MARS.Core.Utils;

function TMARSEngine.AddApplication(const AName, ABasePath: string;
  const AResources: array of string): TMARSApplication;
begin
  Result := Self
    .AddApplication(ABasePath)
    .SetName(AName)
    .SetResources(AResources);
end;

function TMARSEngine.AddApplication(const ABasePath: string): TMARSApplication;
begin
  Result := TMARSApplication.Create(Self);
  Result.SetBasePath(ABasePath);
  try
    Applications.Add(TMARSURL.CombinePath([BasePath, ABasePath]), Result);
  except
    Result.Free;
    raise
  end;
end;

function TMARSEngine.AddSubscriber(const ASubscriber: IMARSHandleListener): TMARSEngine;
begin
  FSubscribers.Add(ASubscriber);
  Result := Self;
end;

procedure TMARSEngine.ApplyPreMatchingFilters;
var
  LRequestFilter: IMARSContainerRequestFilter;
begin
  TMARSFilterRegistry.Instance.FetchRequestFilter(True,
    procedure (ConstructorInfo :TMARSFilterConstructorInfo)
    begin
      // The check doesn't have any sense but I must use SUPPORT and I hate using it without a check
      if not Supports(ConstructorInfo.ConstructorFunc(), IMARSContainerRequestFilter, LRequestFilter) then
        raise EMARSNotImplementedException.Create(
          Format('Request Filter [%s] does not implement requested interface [IMARSContainerRequestFilter]', [ConstructorInfo.TypeTClass.ClassName]),
          Self.ClassName, 'ApplyPreMatchingFilters'
        );
      LRequestFilter.Filter(FRequest);
    end
  );
end;

constructor TMARSEngine.Create;
begin
  FRttiContext := TRttiContext.Create;
  FApplications := TMARSApplicationDictionary.Create([doOwnsValues]);
  FCriticalSection := TCriticalSection.Create;
  FSubscribers := TList<IMARSHandleListener>.Create;
  FPort := 8080;
  FThreadPoolSize := 50;
  FBasePath := '/rest';
  FName := 'MARS Engine';

  inherited Create;
end;

destructor TMARSEngine.Destroy;
begin
  FCriticalSection.Free;
  FApplications.Free;
  FSubscribers.Free;
  inherited;
end;

procedure TMARSEngine.DoAfterHandleRequest(const AApplication: TMARSApplication;
  const AStopWatch: TStopWatch);
var
  LSubscriber: IMARSHandleListener;
  LHandleRequestEventListener: IMARSHandleRequestEventListener;
begin
  for LSubscriber in FSubscribers do
    if Supports(LSubscriber, IMARSHandleRequestEventListener, LHandleRequestEventListener) then
      LHandleRequestEventListener.AfterHandleRequest(Self, AApplication, AStopWatch);
end;

procedure TMARSEngine.DoAfterRequestEnd(const AStopWatch: TStopWatch);
var
  LSubscriber: IMARSHandleListener;
  LHandleRequestEventListenerEx: IMARSHandleRequestEventListenerEx;
begin
  for LSubscriber in FSubscribers do
    if Supports(LSubscriber, IMARSHandleRequestEventListenerEx, LHandleRequestEventListenerEx) then
      LHandleRequestEventListenerEx.AfterRequestEnd(Self, AStopWatch);
end;

procedure TMARSEngine.DoBeforeHandleRequest(const AApplication: TMARSApplication);
var
  LSubscriber: IMARSHandleListener;
  LHandleRequestEventListener: IMARSHandleRequestEventListener;
begin
  for LSubscriber in FSubscribers do
    if Supports(LSubscriber, IMARSHandleRequestEventListener, LHandleRequestEventListener) then
      LHandleRequestEventListener.BeforeHandleRequest(Self, AApplication);
end;

function TMARSEngine.DoBeforeRequestStart(): Boolean;
var
  LSubscriber: IMARSHandleListener;
  LHandleRequestEventListenerEx: IMARSHandleRequestEventListenerEx;
begin
  Result := False;
  for LSubscriber in FSubscribers do
    if Supports(LSubscriber, IMARSHandleRequestEventListenerEx, LHandleRequestEventListenerEx) then
    begin
      LHandleRequestEventListenerEx.BeforeRequestStart(Self, Result);
      if Result then
        Break;
    end;
end;

procedure TMARSEngine.DoHandleException(const AApplication: TMARSApplication;
  E: Exception);
var
  LSubscriber: IMARSHandleListener;
  LHandleExceptionListener: IMARSHandleExceptionListener;
begin
  if E is EMARSWebApplicationException then
  begin
    FResponse.StatusCode := EMARSWebApplicationException(E).Status;
    FResponse.Content := EMARSWebApplicationException(E).ToJSON;
    FResponse.ContentType := TMediaType.APPLICATION_JSON;
  end
  else if E is Exception then
  begin
    FResponse.StatusCode := 500;
    FResponse.Content := EMARSWebApplicationException.ExceptionToJSON(E);
    FResponse.ContentType := TMediaType.APPLICATION_JSON;
  end;

  for LSubscriber in FSubscribers do
    if Supports(LSubscriber, IMARSHandleExceptionListener, LHandleExceptionListener) then
      LHandleExceptionListener.HandleException(Self, AApplication, E);
end;

procedure TMARSEngine.EnumerateApplications(
  const ADoSomething: TProc<string, TMARSApplication>);
var
  LPair: TPair<string, TMARSApplication>;
begin
  if Assigned(ADoSomething) then
  begin
    FCriticalSection.Enter;
    try
      for LPair in FApplications do
        ADoSomething(LPair.Key, LPair.Value);
    finally
      FCriticalSection.Leave;
    end;
  end;
end;

procedure TMARSEngine.HandleRequest(ARequest: TMARSRequest; AResponse: TMARSResponse);
var
  LApplication: TMARSApplication;
  LApplicationPath: string;
  LStopWatch: TStopWatch;
  LStopWatchEx: TStopWatch;
begin
  FURL := nil;
  try
    LApplication := nil;
    LStopWatchEx := TStopwatch.StartNew;
    FRequest := ARequest;
    FResponse := AResponse;
    try
      ApplyPreMatchingFilters;
      if not DoBeforeRequestStart() then
      begin
        FURL := TMARSURL.Create(ARequest); // The object must exists until the end of this procedure
        LApplicationPath := TMARSURL.CombinePath([FURL.PathTokens[0]]);
        if (BasePath <> '') and (BasePath <> TMARSURL.URL_PATH_SEPARATOR) then
        begin
          if not FURL.MatchPath(BasePath + TMARSURL.URL_PATH_SEPARATOR) then
            raise EMARSNotFoundException.Create(
              Format('Requested URL [%s] does not match base engine URL [%s]', [FURL.URL, BasePath]),
              Self.ClassName, 'HandleRequest'
            );

          LApplicationPath := TMARSURL.CombinePath([FURL.PathTokens[0], FURL.PathTokens[1]]);
        end;

        if FApplications.TryGetValue(LApplicationPath, LApplication) then
        begin
          FURL.BasePath := LApplicationPath;
          DoBeforeHandleRequest(LApplication);
          LApplication.ApplyRequestFilters;
          LStopWatch := TStopwatch.StartNew;
          try
            LApplication.HandleRequest(ARequest, AResponse, FURL);
          finally
            LStopWatch.Stop;
          end;
          DoAfterHandleRequest(LApplication, LStopWatch);
        end
        else
          raise EMARSNotFoundException.Create(
            Format('Application [%s] not found, please check the URL [%s]', [LApplicationPath, FURL.URL]),
            Self.ClassName, 'HandleRequest'
          );
      end;
    except
      on E: Exception do
        DoHandleException(LApplication, E);
    end;
    LStopWatchEx.Stop;

    try
      if Assigned(LApplication) then
        LApplication.ApplyResponseFilters;
    except
      on E: Exception do
      begin
        DoHandleException(LApplication, E);
      end
    end;

    DoAfterRequestEnd(LStopWatchEx);
  finally
    FreeAndNil(FURL);
  end;
end;

function TMARSEngine.RemoveSubscriber(const ASubscriber: IMARSHandleListener): TMARSEngine;
begin
  FSubscribers.Remove(ASubscriber);
  Result := Self;
end;

function TMARSEngine.SetBasePath(const ABasePath: string): TMARSEngine;
begin
  if StartsText('/', ABasePath) then
    FBasePath := ABasePath
  else
    FBasePath := '/' + ABasePath;

  Result := Self;
end;

function TMARSEngine.SetName(const AName: string): TMARSEngine;
begin
  FName := AName;
  Result := Self;
end;

function TMARSEngine.SetPort(APort: Integer): TMARSEngine;
begin
  FPort := APort;
  Result := Self;
end;

function TMARSEngine.SetThreadPoolSize(AThreadPoolSize: Integer): TMARSEngine;
begin
  FThreadPoolSize := AThreadPoolSize;
  Result := Self;
end;

function TMARSEngine.GetRequest: TMARSRequest;
begin
  Result := FRequest;
end;

function TMARSEngine.GetResponse: TMARSResponse;
begin
  Result := FResponse;
end;

class function TMARSEngine.GetServerDirectory: string;
begin
  if FServerDirectory = '' then
    FServerDirectory := ExtractFilePath(ServerFileName);
  Result := FServerDirectory;
end;

class function TMARSEngine.GetServerFileName: string;
begin
  if FServerFileName = '' then
    FServerFileName := GetModuleName(MainInstance);
  Result := FServerFileName;
end;

function TMARSEngine.GetURL: TMARSURL;
begin
  Result := FURL;
end;

end.
