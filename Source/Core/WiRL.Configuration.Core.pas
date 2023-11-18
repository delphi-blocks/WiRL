{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2023 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Configuration.Core;

{$I ..\Core\WiRL.inc}

interface

uses
  System.Classes, System.SysUtils, System.TypInfo, System.Rtti, System.Generics.Collections,
  System.Generics.Defaults, System.JSON,

  WiRL.Core.Classes,
  WiRL.Core.JSON,
  WiRL.Core.Singleton,
  WiRL.Core.Exceptions,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON;

type
  TAppConfigurator = class(TInterfacedObject)
  protected
    function GetConfigByInterfaceRef(AInterfaceRef: TGUID): IInterface; virtual; abstract;
  public
    function Configure<T: IInterface>: T;
  end;

  IWiRLApplication = interface;

  {$M+}
  TWiRLConfiguration = class;
  {$M-}

  TWiRLConfigurationClass = class of TWiRLConfiguration;

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
    function SetSystemApp(ASystem: Boolean): IWiRLApplication;
    function SetErrorMediaType(const AMediaType: string): IWiRLApplication;
    function AddApplication(const ABasePath: string): IWiRLApplication;
    function AddConfiguration(const AConfiguration: TWiRLConfiguration): IWiRLApplication;

    function GetConfigByClassRef(AClass: TWiRLConfigurationClass): TWiRLConfiguration;
    function GetAppConfigurator: TAppConfigurator;
    function GetPath: string;

    property Plugin: TAppConfigurator read GetAppConfigurator;
  end;

  IWiRLConfiguration = interface
  ['{E53BA2F7-6CC5-4710-AB18-B0F30E909655}']
    function BackToApp: IWiRLApplication;
    function ApplyConfig: IWiRLApplication;
  end;

  /// <summary>
  ///   A non-reference counted IInterface implementation
  /// </summary>
  {$M+}
  {$IFDEF HAS_NO_REF_COUNT}
  TWiRLConfiguration = class(TNoRefCountObject, IWiRLConfiguration)
  {$ELSE}
  TWiRLConfiguration = class(TSingletonImplementation, IWiRLConfiguration)
  {$ENDIF}
  protected
    FNeonConfig: TNeonConfiguration;
    FApplication: IWiRLApplication;
    function GetAsJSON: TJSONObject;
    function GetAsString: string;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure DoAfterCreate; virtual;

    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(AStream: TStream);
    procedure SaveToJSONObject(const AName: string; AJSON: TJSONObject);

    property Application: IWiRLApplication read FApplication write FApplication;
    property AsString: string read GetAsString;
    property AsJSON: TJSONObject read GetAsJSON;

    function BackToApp: IWiRLApplication;
    function ApplyConfig: IWiRLApplication; virtual;
  end;
  {$M-}

  ImplementsAttribute = class(TCustomAttribute)
  private
    FInterfaceRef: TGUID;
  public
    property InterfaceRef: TGUID read FInterfaceRef;
    constructor Create(AInterfaceRef: TGUID);
  end;

  TWiRLConfigRegistry = class(TObjectDictionary<TWiRLConfigurationClass, TWiRLConfiguration>)
  public
    procedure Add(AConfiguration: TWiRLConfiguration); overload;
    function GetApplicationConfig(AClass: TWiRLConfigurationClass; AApp: IWiRLApplication): TWiRLConfiguration;
  end;

  TWiRLConfigClassRegistry = class(TDictionary<TGUID, TWiRLConfigurationClass>)
  private type
    TWiRLConfigClassRegistrySingleton = TWiRLSingleton<TWiRLConfigClassRegistry>;
  protected
    class function GetInstance: TWiRLConfigClassRegistry; static; inline;
  public
    constructor Create; virtual;
    function GetImplementationOf(AInterfaceRef: TGUID): TWiRLConfigurationClass;
    procedure RegisterConfigClass(AConfigurationClass: TWiRLConfigurationClass);

    class property Instance: TWiRLConfigClassRegistry read GetInstance;
  end;

implementation

uses
  WiRL.Rtti.Utils;

{ TWiRLConfiguration }

function TWiRLConfiguration.BackToApp: IWiRLApplication;
begin
  Result := FApplication;
end;

constructor TWiRLConfiguration.Create;
begin
  FNeonConfig := TNeonConfiguration.Create;
  FNeonConfig.SetVisibility([mvPublished]);
  FNeonConfig.SetPrettyPrint(False);
end;

destructor TWiRLConfiguration.Destroy;
begin
  FNeonConfig := nil;
  inherited;
end;

procedure TWiRLConfiguration.DoAfterCreate;
begin
  // Do nothing, allow subclasses to operate on FApplication
end;

function TWiRLConfiguration.GetAsJSON: TJSONObject;
begin
  Result := TNeon.ObjectToJSON(Self, FNeonConfig) as TJSONObject;
end;

function TWiRLConfiguration.GetAsString: string;
begin
  Result := TNeon.ObjectToJSONString(Self, FNeonConfig);
end;

function TWiRLConfiguration.ApplyConfig: IWiRLApplication;
begin
  Result := FApplication;
end;

procedure TWiRLConfiguration.SaveToFile(const AFileName: string);
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

procedure TWiRLConfiguration.SaveToJSONObject(const AName: string; AJSON: TJSONObject);
begin
  AJSON.AddPair(AName, GetAsJSON);
end;

procedure TWiRLConfiguration.SaveToStream(AStream: TStream);
begin
  TNeon.PrintToStream(GetAsJSON, AStream, True);
end;

{ ImplementsAttribute }

constructor ImplementsAttribute.Create(AInterfaceRef: TGUID);
begin
  FInterfaceRef := AInterfaceRef;
end;

{ TWiRLConfigClassRegistry }

constructor TWiRLConfigClassRegistry.Create;
begin
  inherited Create();
end;

function TWiRLConfigClassRegistry.GetImplementationOf(AInterfaceRef: TGUID): TWiRLConfigurationClass;
begin
  if not TryGetValue(AInterfaceRef, Result) then
    raise EWiRLException.Create('Implementation class not found');
end;

class function TWiRLConfigClassRegistry.GetInstance: TWiRLConfigClassRegistry;
begin
  Result := TWiRLConfigClassRegistrySingleton.Instance;
end;

procedure TWiRLConfigClassRegistry.RegisterConfigClass(AConfigurationClass: TWiRLConfigurationClass);
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

{ TWiRLConfigRegistry }

procedure TWiRLConfigRegistry.Add(AConfiguration: TWiRLConfiguration);
begin
  Add(TWiRLConfigurationClass(AConfiguration.ClassType), AConfiguration);
end;

function TWiRLConfigRegistry.GetApplicationConfig(
  AClass: TWiRLConfigurationClass; AApp: IWiRLApplication): TWiRLConfiguration;
begin
  if not TryGetValue(AClass, Result) then
  begin
    Result := TRttiHelper.CreateInstance(AClass) as TWiRLConfiguration;
    try
      Result.Application := AApp;
      Result.DoAfterCreate;
      Add(Result);
    except
      Result.Free;
      raise;
    end;
  end;

end;

end.
