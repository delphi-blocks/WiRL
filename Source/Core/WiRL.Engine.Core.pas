unit WiRL.Engine.Core;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,

  WiRL.Core.Context.Server;

type
  TWiRLCustomEngine = class(TComponent) //abstract
  private
    FBasePath: string;
    FServer: TComponent;
    procedure SetBasePath(const Value: string);
    procedure SetServer(const Value: TComponent);
    procedure FindDefaultServer;
  protected
    FEngineName: string;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetBasePath(const AURL: string): string;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(ABasePath: string); reintroduce; overload;

    procedure SetEngineServer(AServer: TComponent);

    function CanHandle(const AURL: string): Boolean; virtual;
    procedure HandleRequest(AContext: TWiRLContext); virtual; abstract;

    procedure Startup; virtual;
    procedure Shutdown; virtual;
  published
    property BasePath: string read FBasePath write SetBasePath;
    property EngineName: string read FEngineName write FEngineName;
    property Server: TComponent read FServer write SetServer;
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

  TWiRLEngineRegistry = class(TObjectList<TWiRLEngineInfo>)
  public
    function GetEngine(const AURL: string): TWiRLCustomEngine;
  end;

  TEngineListEnumerator = class
  private
    FIndex: Integer;
    FRegistry: TWiRLEngineRegistry;
  public
    constructor Create(ARegistry: TWiRLEngineRegistry);
    function GetCurrent: TWiRLCustomEngine; inline;
    function MoveNext: Boolean;
    property Current: TWiRLCustomEngine read GetCurrent;
  end;

  TWiRLEngineList = class(TObject)
  private
    FRegistry: TWiRLEngineRegistry;
  public
    function GetEnumerator: TEngineListEnumerator;
    constructor Create(ARegistry: TWiRLEngineRegistry);
  end;


implementation

uses
  System.StrUtils,
  WiRL.Core.Classes,
  WiRL.http.Server;

{ TWiRLCustomEngine }

constructor TWiRLCustomEngine.Create(AOwner: TComponent);
begin
  inherited;
  FindDefaultServer;
  BasePath := '/';
end;

function TWiRLCustomEngine.CanHandle(const AURL: string): Boolean;
var
  LBasePath: string;
begin
  Result := False;
  LBasePath := GetBasePath(AURL);
  if BasePath = LBasePath then
    Result := True;
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
      if LComponent is TWiRLServer then
      begin
        Server := TWiRLServer(LComponent);
        Exit;
      end;
    end;
  end;
end;

function TWiRLCustomEngine.GetBasePath(const AURL: string): string;
var
  LTokens: TArray<string>;
begin
  LTokens := AURL.Split(['/']);
  if Length(LTokens) > 1 then
    Result := '/' + LTokens[1]
  else
    Result := '/';
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
  if Value <> FBasePath then
  begin
    if StartsText('/', Value) then
      FBasePath := Value
    else
      FBasePath := '/' + Value;
    if FBasePath.IndexOf('/', 1) > 0 then
      raise EWiRLException.CreateFmt('BasePath [%s] should not contains any slash', [FBasePath]);
  end;
end;

procedure TWiRLCustomEngine.SetEngineServer(AServer: TComponent);
begin
  if FServer <> AServer then
    FServer := AServer;
end;

procedure TWiRLCustomEngine.SetServer(const Value: TComponent);
begin
  if Assigned(Value) and not (Value is TWiRLServer) then
    raise EWiRLException.Create('Please select a TWiRLServer component');

  if FServer <> Value then
  begin
    if Assigned(FServer) then
      (FServer as TWiRLServer).RemoveEngine(Self);
    FServer := Value;
    if Assigned(FServer) then
      (FServer as TWiRLServer).AddEngine(BasePath, Self, False);
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

{ TWiRLEngineInfo }

constructor TWiRLEngineInfo.Create(AEngine: TWiRLCustomEngine; AOwnsObjects: Boolean);
begin
  inherited Create;
  FEngine := AEngine;
  FOwnsObjects := AOwnsObjects;
end;

{ TWiRLEngineRegistry }

function TWiRLEngineRegistry.GetEngine(const AURL: string): TWiRLCustomEngine;
var
  LEngineInfo: TWiRLEngineInfo;
begin
  Result := nil;

  for LEngineInfo in Self do
    if LEngineInfo.Engine.CanHandle(AURL) then
      Exit(LEngineInfo.Engine);

  // If it doesn't find a specific Engine try with the default one
  if not Assigned(Result) then
    for LEngineInfo in Self do
      if LEngineInfo.Engine.CanHandle('/') then
        Exit(LEngineInfo.Engine);
end;

{ TEngineListEnumerator }

constructor TEngineListEnumerator.Create(ARegistry: TWiRLEngineRegistry);
begin
  inherited Create;
  FRegistry := ARegistry;
  FIndex := -1;
end;

function TEngineListEnumerator.GetCurrent: TWiRLCustomEngine;
begin
  Result := FRegistry[FIndex].FEngine;
end;

function TEngineListEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FRegistry.Count - 1;
  if Result then
    Inc(FIndex);
end;

{ TWiRLEngineList }

constructor TWiRLEngineList.Create(ARegistry: TWiRLEngineRegistry);
begin
  inherited Create;
  FRegistry := ARegistry;
end;

function TWiRLEngineList.GetEnumerator: TEngineListEnumerator;
begin
  Result := TEngineListEnumerator.Create(FRegistry);
end;

end.
