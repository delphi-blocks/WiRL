{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Diagnostics.Manager;

{$I WiRL.inc}

interface

uses
  System.Classes, System.SysUtils, System.StrUtils, System.Generics.Collections,
  System.SyncObjs, System.Diagnostics,

  WiRL.Core.JSON,
  WiRL.Core.Classes,
  WiRL.Core.Singleton,
  WiRL.Core.Context,
  WiRL.Core.Auth.Context,
  WiRL.Core.Engine,
  WiRL.Core.Application;

type
  TWiRLDiagnosticInfo = class
  private
    FRequestCount: Integer;
    FLastRequestTime: TDateTime;
    FCriticalSection: TCriticalSection;
    FBasePath: string;
    FTotalExecutionTime: Int64;
    function GetAverageTimePerRequest: Double;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AcquireRequest(AExecutionTimeInMilliseconds: Integer = 0); virtual;

    function ToJSON: TJSONObject; virtual;

    property BasePath: string read FBasePath write FBasePath;
    property RequestCount: Integer read FRequestCount;
    property LastRequestTime: TDateTime read FLastRequestTime;
    property TotalExecutionTime: Int64 read FTotalExecutionTime;
    property AverageTimePerRequest: Double read GetAverageTimePerRequest;
  end;

  TWiRLDiagnosticAppInfo = class(TWiRLDiagnosticInfo)
  end;

  TWiRLDiagnosticEngineInfo = class(TWiRLDiagnosticInfo)
  private
    FLastSessionEnd: TDateTime;
    FActiveSessionCount: Integer;
    FSessionCount: Integer;
    FLastSessionStart: TDateTime;
  public
    constructor Create;

    procedure AcquireNewSession();
    procedure AcquireEndSession();

    function ToJSON: TJSONObject; override;

    property ActiveSessionCount: Integer read FActiveSessionCount;
    property SessionCount: Integer read FSessionCount;
    property LastSessionStart: TDateTime read FLastSessionStart;
    property LastSessionEnd: TDateTime read FLastSessionEnd;
  end;

  TWiRLDiagnosticsManager = class(TNonInterfacedObject, IWiRLHandleListener, IWiRLHandleRequestEventListener)
  private
    type TDiagnosticsManagerSingleton = TWiRLSingleton<TWiRLDiagnosticsManager>;
  private
    class var FEngine: TWiRLEngine;
    FEngineInfo: TWiRLDiagnosticEngineInfo;
    FAppDictionary: TObjectDictionary<string, TWiRLDiagnosticAppInfo>;
    FCriticalSection: TCriticalSection;
  protected
    class function GetInstance: TWiRLDiagnosticsManager; static; inline;
    function GetAppInfo(const App: string; const ADoSomething: TProc<TWiRLDiagnosticAppInfo>): Boolean; overload;
  public
  public
    constructor Create;
    destructor Destroy; override;

    class procedure SetEngine(AEngine: TWiRLEngine);

    function ToJSON: TJSONObject; virtual;
    procedure RetrieveAppInfo(const App: string; const ADoSomething: TProc<TWiRLDiagnosticAppInfo>);

    // IWiRLTokenEventListener
    procedure OnTokenStart(const AToken: string);
    procedure OnTokenEnd(const AToken: string);

    // IWiRLHandleRequestEventListener
    procedure BeforeHandleRequest(const ASender: TWiRLEngine; const AApplication: TWiRLApplication);
    procedure AfterHandleRequest(const ASender: TWiRLEngine; const AApplication: TWiRLApplication; const AStopWatch: TStopWatch);

    class property Instance: TWiRLDiagnosticsManager read GetInstance;
  end;


implementation

uses
  System.Math, System.DateUtils,
  WiRL.Core.Utils;

{ TWiRLDiagnosticsManager }

procedure TWiRLDiagnosticsManager.AfterHandleRequest(const ASender: TWiRLEngine;
  const AApplication: TWiRLApplication; const AStopWatch: TStopWatch);
var
  LStopWatch: TStopwatch;
begin
  LStopWatch := AStopWatch;

  GetAppInfo(AApplication.Name,
    procedure (AAppInfo: TWiRLDiagnosticAppInfo)
    begin
      AAppInfo.AcquireRequest(LStopWatch.ElapsedMilliseconds);

      FCriticalSection.Enter;
      try
        FEngineInfo.AcquireRequest(LStopWatch.ElapsedMilliseconds);
      finally
        FCriticalSection.Leave;
      end
    end
  );
end;

procedure TWiRLDiagnosticsManager.BeforeHandleRequest(const ASender: TWiRLEngine;
  const AApplication: TWiRLApplication);
begin

end;

constructor TWiRLDiagnosticsManager.Create;
begin
  TDiagnosticsManagerSingleton.CheckInstance(Self);
  FAppDictionary := TObjectDictionary<string, TWiRLDiagnosticAppInfo>.Create([doOwnsValues]);
  FCriticalSection := TCriticalSection.Create;

  FEngineInfo := TWiRLDiagnosticEngineInfo.Create;

  inherited Create;

  FEngine.AddSubscriber(Self);
end;

destructor TWiRLDiagnosticsManager.Destroy;
begin
  FEngine.RemoveSubscriber(Self);

  FEngineInfo.Free;
  FCriticalSection.Free;
  FAppDictionary.Free;
  inherited;
end;

function TWiRLDiagnosticsManager.GetAppInfo(const App: string;
  const ADoSomething: TProc<TWiRLDiagnosticAppInfo>): Boolean;
var
  LInfo: TWiRLDiagnosticAppInfo;
  LWiRLApp: TWiRLApplication;
begin
  Result := False;

  FCriticalSection.Enter;
  try
    if FEngine.Applications.TryGetValue(App, LWiRLApp) then // real application
    begin
      if not LWiRLApp.SystemApp then // skip system app
      begin
        if not FAppDictionary.TryGetValue(App, LInfo) then // find or create
        begin
          LInfo := TWiRLDiagnosticAppInfo.Create;
          LInfo.BasePath := LWiRLApp.BasePath;
          FAppDictionary.Add(App, LInfo);
        end;

        if Assigned(ADoSomething) then
          ADoSomething(LInfo);
      end;
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

class function TWiRLDiagnosticsManager.GetInstance: TWiRLDiagnosticsManager;
begin
  Result := TDiagnosticsManagerSingleton.Instance;
end;

procedure TWiRLDiagnosticsManager.OnTokenEnd(const AToken: string);
begin
  inherited;
  FCriticalSection.Enter;
  try
    FEngineInfo.AcquireEndSession;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TWiRLDiagnosticsManager.OnTokenStart(const AToken: string);
begin
  inherited;

  FCriticalSection.Enter;
  try
    FEngineInfo.AcquireNewSession;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TWiRLDiagnosticsManager.RetrieveAppInfo(const App: string;
  const ADoSomething: TProc<TWiRLDiagnosticAppInfo>);
begin
  GetAppInfo(App, ADoSomething);
end;

class procedure TWiRLDiagnosticsManager.SetEngine(AEngine: TWiRLEngine);
begin
  FEngine := AEngine;
end;

function TWiRLDiagnosticsManager.ToJSON: TJSONObject;
var
  LObj: TJSONObject;
  LPair: TPair<string, TWiRLDiagnosticAppInfo>;
  LAppArray: TJSONArray;
begin
  LObj := TJSONObject.Create;
  FCriticalSection.Enter;
  try
    LObj.AddPair('engine',
      TJSONObject.Create(
        TJSONPair.Create(
          FEngineInfo.BasePath, FEngineInfo.ToJSON
        )
      )
    );
    LAppArray := TJSONArray.Create;

    for LPair in FAppDictionary do
      LAppArray.Add(TJsonObject.Create(TJSONPair.Create(LPair.Key, LPair.Value.ToJSON)));

    LObj.AddPair('apps', LAppArray);
  finally
    FCriticalSection.Leave;
  end;

  Result := LObj;
end;

{ TAppInfo }

procedure TWiRLDiagnosticInfo.AcquireRequest(AExecutionTimeInMilliseconds:
    Integer = 0);
begin
  FCriticalSection.Enter;
  try
    Inc(FRequestCount);
    FTotalExecutionTime := FTotalExecutionTime + AExecutionTimeInMilliseconds;
    FLastRequestTime := Now;
  finally
    FCriticalSection.Leave;
  end;
end;

constructor TWiRLDiagnosticInfo.Create;
begin
  inherited Create;

  FRequestCount := 0;
  FLastRequestTime := 0;

  FCriticalSection := TCriticalSection.Create;
end;

destructor TWiRLDiagnosticInfo.Destroy;
begin
  FCriticalSection.Free;
  inherited;
end;

function TWiRLDiagnosticInfo.GetAverageTimePerRequest: Double;
begin
  if FRequestCount = 0 then
    Result := 0
  else
  Result := RoundTo(FTotalExecutionTime / FRequestCount, -2);
end;

function TWiRLDiagnosticInfo.ToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;

  Result.AddPair('BasePath', BasePath);
  Result.AddPair('RequestCount', TJSONNumber.Create(FRequestCount));
  Result.AddPair('LastRequestTime', DateToISO8601(FLastRequestTime));
  Result.AddPair('TotalExecutionTime', TJSONNumber.Create(FTotalExecutionTime));
  Result.AddPair('AverageTimePerRequest', TJSONNumber.Create(AverageTimePerRequest));
end;

{ TEngineInfo }

procedure TWiRLDiagnosticEngineInfo.AcquireEndSession;
begin
  FCriticalSection.Enter;
  try
    FLastSessionEnd := Now;
    Dec(FActiveSessionCount);
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TWiRLDiagnosticEngineInfo.AcquireNewSession;
begin
  FCriticalSection.Enter;
  try
    Inc(FSessionCount);
    Inc(FActiveSessionCount);
    FLastSessionStart := Now;
  finally
    FCriticalSection.Leave;
  end;
end;

constructor TWiRLDiagnosticEngineInfo.Create;
begin
  inherited Create;
  FLastSessionEnd := 0;
  FSessionCount := 0;
  FLastSessionStart := 0;
  FActiveSessionCount := 0;
end;

function TWiRLDiagnosticEngineInfo.ToJSON: TJSONObject;
begin
  Result := inherited ToJSON;

  Result.AddPair('SessionCount', TJSONNumber.Create(FSessionCount));
  Result.AddPair('ActiveSessionCount', TJSONNumber.Create(FActiveSessionCount));
  Result.AddPair('LastSessionStart',  DateToISO8601(FLastSessionStart));
  Result.AddPair('LastSessionEnd', DateToISO8601(FLastSessionEnd));
end;

end.
