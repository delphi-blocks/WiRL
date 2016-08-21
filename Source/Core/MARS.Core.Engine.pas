(*
  Copyright 2015-2016, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit MARS.Core.Engine;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.SyncObjs, System.Diagnostics,

  MARS.Core.Classes,
  MARS.Core.MediaType,
  MARS.Core.Exceptions,
  MARS.Core.Registry,
  MARS.Core.Application,
  MARS.Core.URL,
  MARS.Core.Request,
  MARS.Core.Response;

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
    class threadvar FURL: TMARSURL;
  private
    FApplications: TMARSApplicationDictionary;
    FSubscribers: TList<IMARSHandleListener>;
    FCriticalSection: TCriticalSection;
    FBasePath: string;
    FPort: Integer;
    FThreadPoolSize: Integer;
    FName: string;

    function GetURL: TMARSURL;
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

    function AddApplication(const AName, ABasePath: string): TMARSApplication; overload; virtual;
    function AddApplication(const AName, ABasePath: string; const AResources: array of string): TMARSApplication; overload; virtual;

    procedure AddSubscriber(const ASubscriber: IMARSHandleListener);
    procedure RemoveSubscriber(const ASubscriber: IMARSHandleListener);

    procedure EnumerateApplications(const ADoSomething: TProc<string, TMARSApplication>);

    property Applications: TMARSApplicationDictionary read FApplications;
    property BasePath: string read FBasePath write FBasePath;
    property Name: string read FName write FName;
    property Port: Integer read FPort write FPort;
    property ThreadPoolSize: Integer read FThreadPoolSize write FThreadPoolSize;

    property CurrentURL: TMARSURL read GetURL;
  end;

implementation

uses
  MARS.Core.Utils;

function TMARSEngine.AddApplication(const AName, ABasePath: string;
  const AResources: array of string): TMARSApplication;
var
  LResource: string;
begin
  Result := TMARSApplication.Create(Self);
  try
    Result.Name := AName;
    Result.BasePath := ABasePath;
    for LResource in AResources do
      Result.AddResource(LResource);

    Applications.Add(TMARSURL.CombinePath([BasePath, ABasePath]), Result);
  except
    Result.Free;
    raise
  end;
end;

function TMARSEngine.AddApplication(const AName,
  ABasePath: string): TMARSApplication;
begin
  Result := TMARSApplication.Create(Self);
  try
    Result.Name := AName;
    Result.BasePath := ABasePath;

    Applications.Add(TMARSURL.CombinePath([BasePath, ABasePath]), Result);
  except
    Result.Free;
    raise
  end;
end;

procedure TMARSEngine.AddSubscriber(const ASubscriber: IMARSHandleListener);
begin
  FSubscribers.Add(ASubscriber);
end;

constructor TMARSEngine.Create;
begin
  FApplications := TMARSApplicationDictionary.Create([doOwnsValues]);
  FCriticalSection := TCriticalSection.Create;
  FSubscribers := TList<IMARSHandleListener>.Create;
  FPort := 8080;
  FThreadPoolSize := 75;
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
  LURL: TMARSURL;
  LApplicationPath: string;
  LStopWatch: TStopWatch;
  LStopWatchEx: TStopWatch;
begin
  LStopWatchEx := TStopwatch.StartNew;
  if not DoBeforeRequestStart() then
  try
    LURL := TMARSURL.Create(ARequest);
    try
      LApplicationPath := TMARSURL.CombinePath([LURL.PathTokens[0]]);
      if (BasePath <> '') and (BasePath <> TMARSURL.URL_PATH_SEPARATOR) then
      begin
        if not LURL.MatchPath(BasePath + TMARSURL.URL_PATH_SEPARATOR) then
          raise EMARSNotFoundException.Create(
            Format('Requested URL [%s] does not match base engine URL [%s]', [LURL.URL, BasePath]),
            Self.ClassName, 'HandleRequest'
          );

        LApplicationPath := TMARSURL.CombinePath([LURL.PathTokens[0], LURL.PathTokens[1]]);
      end;

      if FApplications.TryGetValue(LApplicationPath, LApplication) then
      begin
        LURL.BasePath := LApplicationPath;
        FURL := LURL;
        DoBeforeHandleRequest(LApplication);
        LStopWatch := TStopwatch.StartNew;
        try
          LApplication.HandleRequest(ARequest, AResponse, LURL);
        finally
          LStopWatch.Stop;
        end;
        DoAfterHandleRequest(LApplication, LStopWatch);
      end
      else
        raise EMARSNotFoundException.Create(
          Format('Application [%s] not found, please check the URL [%s]', [LApplicationPath, LURL.URL]),
          Self.ClassName, 'HandleRequest'
        );
    finally
      LURL.Free;
    end;
  except
    on E: EMARSWebApplicationException do
    begin
      AResponse.StatusCode := E.Status;
      AResponse.Content := E.ToJSON;
      AResponse.ContentType := TMediaType.APPLICATION_JSON;
      DoHandleException(LApplication, E);
    end;

    on E: Exception do
    begin
      AResponse.StatusCode := 500;
      AResponse.Content := EMARSWebApplicationException.ExceptionToJSON(E);
      AResponse.ContentType := TMediaType.APPLICATION_JSON;
      DoHandleException(LApplication, E);
    end
  end;
  LStopWatchEx.Stop;
  DoAfterRequestEnd(LStopWatchEx);
end;

procedure TMARSEngine.RemoveSubscriber(
  const ASubscriber: IMARSHandleListener);
begin
  FSubscribers.Remove(ASubscriber);
end;

function TMARSEngine.GetURL: TMARSURL;
begin
  Result := FURL;
end;

end.
