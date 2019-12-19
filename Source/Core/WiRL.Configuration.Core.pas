unit WiRL.Configuration.Core;

interface

uses
  System.Classes, System.SysUtils, System.TypInfo, System.Generics.Collections,
  WiRL.Core.JSON,

  WiRL.Core.Singleton,
  WiRL.Core.Exceptions,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON, System.JSON;

type
  TAppConfigurator = class(TInterfacedObject)
  protected
    function GetConfigByInterfaceRef(AInterfaceRef: TGUID): IInterface; virtual; abstract;
  public
    function Configure<T: IInterface>: T;
  end;

  IWiRLApplication = interface
    ['{1F764F15-45D9-40E1-9F79-216748466BF7}']
    function SetResources(const AResources: TArray<string>): IWiRLApplication; overload;
    function SetResources(const AResources: string): IWiRLApplication; overload;
    function SetFilters(const AFilters: TArray<string>): IWiRLApplication; overload;
    function SetFilters(const AFilters: string): IWiRLApplication; overload;
    function SetWriters(const AWriters: TArray<string>): IWiRLApplication; overload;
    function SetWriters(const AWriters: string): IWiRLApplication; overload;
    function SetReaders(const AReaders: TArray<string>): IWiRLApplication; overload;
    function SetReaders(const AReaders: string): IWiRLApplication; overload;
    function SetBasePath(const ABasePath: string): IWiRLApplication;
    function SetAppName(const AAppName: string): IWiRLApplication;
    function SetUseUTCDate(AValue: Boolean): IWiRLApplication;
    function SetSystemApp(ASystem: Boolean): IWiRLApplication;
    function GetAppConfigurator: TAppConfigurator;

    property Plugin: TAppConfigurator read GetAppConfigurator;
  end;

  IWiRLConfiguration = interface
    ['{E53BA2F7-6CC5-4710-AB18-B0F30E909655}']
    function BackToApp: IWiRLApplication;
  end;

  // A non-reference-counted IInterface implementation.
  TWiRLConfigurationNRef = class(TPersistent, IWiRLConfiguration)
  private
    FNeonConfig: TNeonConfiguration;
    FApplication: IWiRLApplication;
    function GetAsJSON: TJSONObject;
    function GetAsString: string;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(AStream: TStream);
    procedure SaveToJSONObject(const AName: string; AJSON: TJSONObject);

    property Application: IWiRLApplication read FApplication write FApplication;
    property AsString: string read GetAsString;
    property AsJSON: TJSONObject read GetAsJSON;
    function BackToApp: IWiRLApplication;
  end;

  TWiRLConfigurationNRefClass = class of TWiRLConfigurationNRef;

  ImplementsAttribute = class(TCustomAttribute)
  private
    FInterfaceRef: TGUID;
  public
    property InterfaceRef: TGUID read FInterfaceRef;
    constructor Create(const AInterfaceRef: TGUID);
  end;

  TWiRLConfigRegistry = class(TObjectDictionary<TWiRLConfigurationNRefClass, TWiRLConfigurationNRef>)
  end;

  TWiRLConfigClassRegistry = class(TDictionary<TGUID, TWiRLConfigurationNRefClass>)
  private type
    TWiRLConfigClassRegistrySingleton = TWiRLSingleton<TWiRLConfigClassRegistry>;
  protected
    class function GetInstance: TWiRLConfigClassRegistry; static; inline;
  public
    constructor Create; virtual;
    function GetImplementationOf(AInterfaceRef: TGUID): TWiRLConfigurationNRefClass;
    procedure RegisterConfigClass(AConfigurationClass: TWiRLConfigurationNRefClass);

    class property Instance: TWiRLConfigClassRegistry read GetInstance;
  end;

implementation

uses
  WiRL.Rtti.Utils;

{ TWiRLConfigurationNRef }

function TWiRLConfigurationNRef.BackToApp: IWiRLApplication;
begin
  Result := FApplication;
end;

constructor TWiRLConfigurationNRef.Create;
begin
  FNeonConfig := TNeonConfiguration.Create;
  FNeonConfig.SetVisibility([mvPublished]);
  FNeonConfig.SetPrettyPrint(False);
end;

destructor TWiRLConfigurationNRef.Destroy;
begin
  FNeonConfig := nil;
  inherited;
end;

function TWiRLConfigurationNRef.GetAsJSON: TJSONObject;
begin
  Result := TNeon.ObjectToJSON(Self, FNeonConfig) as TJSONObject;
end;

function TWiRLConfigurationNRef.GetAsString: string;
begin
  Result := TNeon.ObjectToJSONString(Self, FNeonConfig);
end;

procedure TWiRLConfigurationNRef.SaveToFile(const AFileName: string);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(AFileName, fmCreate or fmOpenReadWrite);
  try
    SaveToStream(LStream);
  finally
    LStream.Free;
  end;
end;

procedure TWiRLConfigurationNRef.SaveToJSONObject(const AName: string; AJSON: TJSONObject);
begin
  AJSON.AddPair(AName, GetAsJSON);
end;

procedure TWiRLConfigurationNRef.SaveToStream(AStream: TStream);
begin
  TNeon.PrintToStream(GetAsJSON, AStream, True);
end;

function TWiRLConfigurationNRef.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TWiRLConfigurationNRef._AddRef: Integer;
begin
  Result := -1;
end;

function TWiRLConfigurationNRef._Release: Integer;
begin
  Result := -1;
end;

{ ImplementsAttribute }

constructor ImplementsAttribute.Create(const AInterfaceRef: TGUID);
begin
  FInterfaceRef := AInterfaceRef;
end;

{ TWiRLConfigClassRegistry }

constructor TWiRLConfigClassRegistry.Create;
begin
  inherited Create();
end;

function TWiRLConfigClassRegistry.GetImplementationOf(
  AInterfaceRef: TGUID): TWiRLConfigurationNRefClass;
begin
  if not TryGetValue(AInterfaceRef, Result) then
    raise EWiRLException.Create('Implementation class not found');
end;

class function TWiRLConfigClassRegistry.GetInstance: TWiRLConfigClassRegistry;
begin
  Result := TWiRLConfigClassRegistrySingleton.Instance;
end;

procedure TWiRLConfigClassRegistry.RegisterConfigClass(
  AConfigurationClass: TWiRLConfigurationNRefClass);
var
  LImplementsAttribute: ImplementsAttribute;
begin
  LImplementsAttribute := TRttiHelper.FindAttribute<ImplementsAttribute>(AConfigurationClass);
  if not Assigned(LImplementsAttribute) then
    raise EWiRLException.CreateFmt('Attribute [Implements] not found for [%s] class', [AConfigurationClass.ClassName]);
  Add(LImplementsAttribute.InterfaceRef, AConfigurationClass);
end;

{ TAppConfigurator }

function TAppConfigurator.Configure<T>: T;
var
  LInterfaceRef: TGUID;
  LConfig: IInterface;
begin
  try
    LInterfaceRef := GetTypeData(TypeInfo(T))^.GUID;

    LConfig := GetConfigByInterfaceRef(LInterfaceRef);
    if not Supports(LConfig, LInterfaceRef, Result) then
      raise EWiRLException.Create('Invalid config');
  except
    on E: Exception do
    begin
      raise EWiRLException.CreateFmt('%s (%s)', [E.Message, GetTypeName(TypeInfo(T))]);
    end;
  end;
end;

end.
