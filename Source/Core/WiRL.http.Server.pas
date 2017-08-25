unit WiRL.http.Server;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,

  WiRL.http.Engines, WiRL.Core.Engine, WiRL.Core.Exceptions, WiRL.Core.Utils, WiRL.Rtti.Utils;

type
  TWiRLhttpServer = class abstract
  private
    FActive: Boolean;
    procedure FreeEngines;
  protected
    FEngines: TDictionary<string,TWiRLCustomEngine>;
    FThreadPoolSize: Integer;
    FPort: Integer;
    function GetActive: Boolean; virtual;
    procedure SetActive(const Value: Boolean); virtual;
    procedure Startup; virtual;
    procedure Shutdown; virtual;
  public
    function AddEngine<T: constructor, TWiRLCustomEngine>(const ABasePath: string) :T;
    function AddEngines(AEngines: TArray<TWirlCustomEngine>) :TWiRLhttpServer;
    function GetEngine(const Url: string): TWiRLCustomEngine;
    function SetPort(APort: Integer): TWiRLhttpServer;
    function SetThreadPoolSize(AThreadPoolSize: Integer): TWiRLhttpServer;
    // TODO: Remove this method and the dependency from TWiRLEngine
    function ConfigureEngine(const ABasePath: string) :TWiRLEngine; deprecated 'Use AddEngine<TWiRLEngine>';

    property Active: Boolean read GetActive write SetActive;
    property Port: Integer read FPort write FPort;
    property ThreadPoolSize: Integer read FThreadPoolSize write FThreadPoolSize;

    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

{ TWiRLhttpServer }

function TWiRLhttpServer.AddEngine<T>(const ABasePath: string): T;
begin
  Result := TRttiHelper.CreateInstance(TClass(T), ABasePath) as T;
  //Result := T.Create(ABasePath);
  FEngines.Add(ABasePath, Result);
end;

function TWiRLhttpServer.AddEngines(
  AEngines: TArray<TWirlCustomEngine>): TWiRLhttpServer;
var
  LEngine: TWirlCustomEngine;
begin
  for LEngine in AEngines do
    FEngines.Add(LEngine.BasePath, LEngine);
  Result := Self;
end;

function TWiRLhttpServer.ConfigureEngine(const ABasePath: string): TWiRLEngine;
begin
  Result := AddEngine<TWiRLEngine>(ABasePath);
end;

constructor TWiRLhttpServer.Create;
begin
  inherited;
  FActive := False;
  FPort := 8080;
  FThreadPoolSize := 50;

  FEngines := TDictionary<string,TWiRLCustomEngine>.Create;
end;

destructor TWiRLhttpServer.Destroy;
begin
  FreeEngines;
  inherited;
end;

procedure TWiRLhttpServer.FreeEngines;
var
  LPair: TPair<string,TWiRLCustomEngine>;
begin
  for LPair in FEngines do
    LPair.Value.Free;
  FEngines.Free;
end;

function TWiRLhttpServer.GetActive: Boolean;
begin
  Result := FActive;
end;

function TWiRLhttpServer.GetEngine(const Url: string): TWiRLCustomEngine;
var
  UrlTokens: TArray<string>;
  BaseUrl: string;
begin
  Result := nil;
  UrlTokens := Url.Split(['/']);
  if Length(UrlTokens) > 1 then
    BaseUrl := UrlTokens[1]
  else
    BaseUrl := '';

  if FEngines.TryGetValue('/' + BaseUrl, Result) then
    Exit;

  if FEngines.TryGetValue('/', Result) then
    Exit;

  if not Assigned(Result) then
    raise EWiRLNotFoundException.CreateFmt('Engine not found for URL [%s]', [Url]);
end;

procedure TWiRLhttpServer.SetActive(const Value: Boolean);
begin
  if Value <> Active then
  begin
    FActive := Value;
    if Value then
      Startup
    else
      Shutdown;
  end;
end;

function TWiRLhttpServer.SetPort(APort: Integer): TWiRLhttpServer;
begin
  FPort := APort;
  Result := Self;
end;

function TWiRLhttpServer.SetThreadPoolSize(AThreadPoolSize: Integer): TWiRLhttpServer;
begin
  FThreadPoolSize := AThreadPoolSize;
  Result := Self;
end;

procedure TWiRLhttpServer.Shutdown;
var
  LPair: TPair<string,TWiRLCustomEngine>;
begin
  for LPair in FEngines do
    LPair.Value.Shutdown;
end;

procedure TWiRLhttpServer.Startup;
var
  LPair: TPair<string,TWiRLCustomEngine>;
begin
  for LPair in FEngines do
    LPair.Value.Startup;
end;

end.
