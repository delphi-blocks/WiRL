{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2025 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Application;

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.Classes, System.Rtti, System.TypInfo,
  System.Generics.Collections,

  WiRL.http.Filters,
  WiRL.Core.Declarations,
  WiRL.Core.Classes,
  WiRL.Core.MessageBodyReader,
  WiRL.Core.MessageBodyWriter,
  WiRL.Core.Registry,
  WiRL.Core.Context,
  WiRL.Core.Metadata,
  WiRL.Core.Auth.Context,
  WiRL.Core.Injection,
  WiRL.Configuration.Core;

type
  TWiRLApplication = class(TComponent, IWiRLApplication)
  private
    FResourceRegistry: TWiRLResourceRegistry;
    FFilterRegistry: TWiRLFilterRegistry;
    FWriterRegistry: TWiRLWriterRegistry;
    FReaderRegistry: TWiRLReaderRegistry;
    FConfigRegistry: TWiRLConfigRegistry;
    FAppConfigurator: TAppConfigurator;
    FProxy: TWiRLProxyApplication;
    FBasePath: string;
    FAppName: string;
    FSystemApp: Boolean;
    FEngine: TComponent;
    FErrorMediaType: string;
    function AddResource(const AResource: string): Boolean;
    function AddFilter(const AFilter: string): Boolean;
    function AddWriter(const AWriter: string): Boolean;
    function AddReader(const AReader: string): Boolean;

    procedure SetEngine(const Value: TComponent);
    function GetPath: string;
    procedure ReadFilters(Reader: TReader);
    procedure WriteFilters(Writer: TWriter);
    procedure ReadResources(Reader: TReader);
    procedure WriteResources(Writer: TWriter);
    procedure ReadWriters(Reader: TReader);
    procedure WriteWriters(Writer: TWriter);
    procedure ReadReaders(Reader: TReader);
    procedure WriteReaders(Writer: TWriter);
    function GetEnginePath: string;
    class function GetRttiContext: TRttiContext; static;
  protected
    procedure SetParentComponent(AParent: TComponent); override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetAppConfigurator: TAppConfigurator;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Startup;
    procedure Shutdown;

    // Fluent-like configuration methods
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
    function SetErrorMediaType(const AMediaType: string): IWiRLApplication;
    function SetSystemApp(ASystem: Boolean): IWiRLApplication;
    function AddApplication(const ABasePath: string): IWiRLApplication;
    function AddConfiguration(const AConfiguration: TWiRLConfiguration): IWiRLApplication;

    function Configure<T: IInterface>: T;
    function GetConfiguration<T: TWiRLConfiguration>: T;

    function GetConfigByInterfaceRef(AInterfaceRef: TGUID): IInterface;
    function GetConfigByClassRef(AClass: TWiRLConfigurationClass): TWiRLConfiguration;

    { TODO -opaolo -c : Remove from here? 16/03/2021 15:13:12 }
    function GetResourceCtor(const AResourceName: string): TWiRLConstructorProxy;

    // Handles the parent/child relationship for the designer
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;

    // Remove???
    function GetFormatSettingFor(ATypeInfo: PTypeInfo): string;

    property SystemApp: Boolean read FSystemApp;
    property FilterRegistry: TWiRLFilterRegistry read FFilterRegistry write FFilterRegistry;
    property WriterRegistry: TWiRLWriterRegistry read FWriterRegistry write FWriterRegistry;
    property ReaderRegistry: TWiRLReaderRegistry read FReaderRegistry write FReaderRegistry;
    property Engine: TComponent read FEngine write SetEngine;

    class property RttiContext: TRttiContext read GetRttiContext;
  published
    property Path: string read GetPath;
    property EnginePath: string read GetEnginePath;
    property AppName: string read FAppName write FAppName;
    property BasePath: string read FBasePath write FBasePath;
    property ErrorMediaType: string read FErrorMediaType write FErrorMediaType;

    property Proxy: TWiRLProxyApplication read FProxy write FProxy;
    // Fake property to display the right property editors
    property Resources: TWiRLResourceRegistry read FResourceRegistry write FResourceRegistry;
    property Filters: TWiRLFilterRegistry read FFilterRegistry write FFilterRegistry;
    property Writers: TWiRLWriterRegistry read FWriterRegistry write FWriterRegistry;
    property Readers: TWiRLReaderRegistry read FReaderRegistry write FReaderRegistry;
    property Configs: TWiRLConfigRegistry read FConfigRegistry write FConfigRegistry;
  end;

  TAppConfiguratorImpl = class(TAppConfigurator)
  private
    FApplication: TWiRLApplication;
  protected
    function GetConfigByInterfaceRef(AInterfaceRef: TGUID): IInterface; override;
  public
    property Application: TWiRLApplication read FApplication;
    constructor Create(AApplication: TWiRLApplication);
  end;

implementation

uses
  System.StrUtils,

  WiRL.http.URL,
  WiRL.http.Accept.MediaType,
  WiRL.Configuration.Converter,
  WiRL.Core.Exceptions,
  WiRL.Core.Attributes,
  WiRL.Core.Converter,
  WiRL.Core.Utils,
  WiRL.Engine.REST,
  WiRL.Rtti.Utils;

function ExtractToken(const AString: string; const ATokenIndex: Integer; const ADelimiter: Char = '/'): string;
var
  LTokens: TArray<string>;
begin
  LTokens := TArray<string>(SplitString(AString, ADelimiter));

  Result := '';
  if ATokenIndex < Length(LTokens) then
    Result := LTokens[ATokenIndex]
  else
    raise EWiRLServerException.Create(
      Format('ExtractToken, index: %d from %s', [ATokenIndex, AString]), 'ExtractToken');
end;

{ TWiRLApplication }

function TWiRLApplication.AddApplication(
  const ABasePath: string): IWiRLApplication;
begin
  Result := (FEngine as TWiRLRESTEngine).AddApplication(ABasePath);
end;

function TWiRLApplication.AddConfiguration(
  const AConfiguration: TWiRLConfiguration): IWiRLApplication;
begin
  FConfigRegistry.Add(AConfiguration);
  Result := Self;
end;

function TWiRLApplication.AddFilter(const AFilter: string): Boolean;
var
  LRegistry: TWiRLFilterRegistry;
  LInfo: TWiRLFilterConstructorProxy;
begin
  if csDesigning in ComponentState then
  begin
    FFilterRegistry.AddFilterName(AFilter);
    Exit(True);
  end;

  Result := False;
  LRegistry := TWiRLFilterRegistry.Instance;

  if IsMask(AFilter) then // has wildcards and so on...
  begin
    for LInfo in LRegistry do
    begin
      if MatchesMask(LInfo.TypeTClass.QualifiedClassName, AFilter) then
      begin
        FFilterRegistry.Add(LInfo);
        Result := True;
      end;
    end;
  end
  else // exact match
  begin
    if LRegistry.FilterByClassName(AFilter, LInfo) then
    begin
      FFilterRegistry.Add(LInfo);
      Result := True;
    end;
  end;
end;

function TWiRLApplication.AddReader(const AReader: string): Boolean;
var
  LGlobalRegistry: TWiRLReaderRegistry;
  LReader: TWiRLReaderRegistry.TReaderInfo;
begin
  if csDesigning in ComponentState then
  begin
    FReaderRegistry.AddReaderName(AReader);
    Exit(True);
  end;

  Result := False;
  LGlobalRegistry := TMessageBodyReaderRegistry.Instance;

  if IsMask(AReader) then // has wildcards and so on...
  begin
    FReaderRegistry.Assign(LGlobalRegistry);
    Result := True;
  end
  else // exact match
  begin
    LReader := LGlobalRegistry.GetReaderByName(AReader);
    if Assigned(LReader) then
    begin
      FReaderRegistry.Add(LReader);
      Result := True;
    end;
  end;
end;

function TWiRLApplication.AddResource(const AResource: string): Boolean;

  function AddResourceToApplicationRegistry(const AInfo: TWiRLConstructorProxy): Boolean;
  var
    LClass: TClass;
    LResult: Boolean;
  begin
    LResult := False;
    LClass := AInfo.TypeTClass;
    TRttiHelper.ForEachAttribute<PathAttribute>(RttiContext.GetType(LClass),
      procedure (AAttribute: PathAttribute)
      begin
        if not FResourceRegistry.ContainsKey(TWiRLURL.StripFirstPathDelimiter(AAttribute.Value)) then
        begin
          FResourceRegistry.Add(TWiRLURL.StripFirstPathDelimiter(AAttribute.Value), AInfo.Clone);
          LResult := True;
        end;
      end
    );
    Result := LResult;
  end;

var
  LRegistry: TWiRLResourceRegistry;
  LInfo: TWiRLConstructorProxy;
  LKey: string;
begin
  if csDesigning in ComponentState then
  begin
    FResourceRegistry.AddResourceName(AResource);
    Exit(True);
  end;

  Result := False;
  LRegistry := TWiRLResourceRegistry.Instance;

  if IsMask(AResource) then // has wildcards and so on...
  begin
    for LKey in LRegistry.Keys.ToArray do
    begin
      if MatchesMask(LKey, AResource) then
      begin
        if LRegistry.TryGetValue(LKey, LInfo) and AddResourceToApplicationRegistry(LInfo) then
          Result := True;
      end;
    end;
  end
  else // exact match
    if LRegistry.TryGetValue(AResource, LInfo) then
      Result := AddResourceToApplicationRegistry(LInfo);
end;

function TWiRLApplication.AddWriter(const AWriter: string): Boolean;
var
  LGlobalRegistry: TWiRLWriterRegistry;
  LWriter: TWiRLWriterRegistry.TWriterInfo;
begin
  if csDesigning in ComponentState then
  begin
    FWriterRegistry.AddWriterName(AWriter);
    Exit(True);
  end;

  Result := False;
  LGlobalRegistry := TMessageBodyWriterRegistry.Instance;

  if IsMask(AWriter) then // has wildcards and so on...
  begin
    FWriterRegistry.Assign(LGlobalRegistry);
    Result := True;
  end
  else // exact match
  begin
    LWriter := LGlobalRegistry.GetWriterByName(AWriter);
    if Assigned(LWriter) then
    begin
      FWriterRegistry.Add(LWriter);
      Result := True;
    end;
  end;
end;

function TWiRLApplication.SetBasePath(const ABasePath: string): IWiRLApplication;
begin
  FBasePath := ABasePath;
  Result := Self;
end;

function TWiRLApplication.SetAppName(const AAppName: string): IWiRLApplication;
begin
  FAppName := AAppName;
  Result := Self;
end;

procedure TWiRLApplication.SetEngine(const Value: TComponent);
begin
  if FEngine <> Value then
  begin
    if Assigned(FEngine) then
      (FEngine as TWiRLRESTEngine).RemoveApplication(Self);
    FEngine := Value;
    if Assigned(FEngine) then
      (FEngine as TWiRLRESTEngine).AddApplication(Self);
  end;
end;

function TWiRLApplication.SetErrorMediaType(
  const AMediaType: string): IWiRLApplication;
begin
  FErrorMediaType := AMediaType;
  Result := Self;
end;

function TWiRLApplication.SetReaders(const AReaders: TArray<string>): IWiRLApplication;
var
  LReader: string;
begin
  if Length(AReaders) = 0 then
    FreaderRegistry.Clear
  else
    for LReader in AReaders do
      Self.AddReader(LReader);

  Result := Self;
end;

function TWiRLApplication.SetReaders(const AReaders: string): IWiRLApplication;
begin
  Result := SetReaders(AReaders.Split([',']));
end;

function TWiRLApplication.SetResources(const AResources: string): IWiRLApplication;
begin
  Result := SetResources(AResources.Split([',']));
end;

function TWiRLApplication.SetResources(const AResources: TArray<string>): IWiRLApplication;
var
  LResource: string;
begin
  if Length(AResources) = 0 then
    FResourceRegistry.Clear
  else
    for LResource in AResources do
      Self.AddResource(LResource);

  Result := Self;
end;

function TWiRLApplication.SetFilters(const AFilters: string): IWiRLApplication;
begin
  Result := SetFilters(AFilters.Split([',']));
end;

function TWiRLApplication.SetFilters(const AFilters: TArray<string>): IWiRLApplication;
var
  LFilter: string;
begin
  if Length(AFilters) = 0 then
    FFilterRegistry.Clear
  else
    for LFilter in AFilters do
      Self.AddFilter(LFilter);

  Result := Self;
end;

procedure TWiRLApplication.SetParentComponent(AParent: TComponent);
begin
  inherited;
  if AParent is TWiRLRESTEngine then
    Engine := AParent as TWiRLRESTEngine;
end;

function TWiRLApplication.SetWriters(const AWriters: TArray<string>): IWiRLApplication;
var
  LWriter: string;
begin
  if Length(AWriters) = 0 then
    FWriterRegistry.Clear
  else
    for LWriter in AWriters do
      Self.AddWriter(LWriter);

  Result := Self;
end;

function TWiRLApplication.SetWriters(const AWriters: string): IWiRLApplication;
begin
  Result := SetWriters(AWriters.Split([',']));
end;

procedure TWiRLApplication.Shutdown;
begin
  FProxy.Reset();
end;

procedure TWiRLApplication.Startup;
begin
  if FWriterRegistry.Count = 0 then
    FWriterRegistry.Assign(TMessageBodyWriterRegistry.Instance);

  if FReaderRegistry.Count = 0 then
    FReaderRegistry.Assign(TMessageBodyReaderRegistry.Instance);

  FProxy.Process();
end;

procedure TWiRLApplication.WriteFilters(Writer: TWriter);
var
  LFilter: TWiRLFilterConstructorProxy;
begin
  Writer.WriteListBegin;
  for LFilter in FFilterRegistry do
    Writer.WriteString(LFilter.FilterQualifiedClassName);
  Writer.WriteListEnd;
end;

procedure TWiRLApplication.WriteReaders(Writer: TWriter);
var
  LReaderInfo: TWiRLReaderRegistry.TReaderInfo;
begin
  Writer.WriteListBegin;
  for LReaderInfo in FReaderRegistry do
    Writer.WriteString(LReaderInfo.ReaderName);
  Writer.WriteListEnd;
end;

procedure TWiRLApplication.WriteResources(Writer: TWriter);
var
  LResourceName: string;
begin
  Writer.WriteListBegin;
  for LResourceName in FResourceRegistry.Keys do
    Writer.WriteString(LResourceName);
  Writer.WriteListEnd;
end;

procedure TWiRLApplication.WriteWriters(Writer: TWriter);
var
  LWriterInfo: TWiRLWriterRegistry.TWriterInfo;
begin
  Writer.WriteListBegin;
  for LWriterInfo in FWriterRegistry do
    Writer.WriteString(LWriterInfo.WriterName);
  Writer.WriteListEnd;
end;

function TWiRLApplication.GetConfigByClassRef(AClass: TWiRLConfigurationClass): TWiRLConfiguration;
begin
  Result := FConfigRegistry.GetApplicationConfig(AClass, Self);
end;

function TWiRLApplication.GetConfigByInterfaceRef(AInterfaceRef: TGUID): IInterface;
var
  LConfig: TWiRLConfiguration;
  LConfigClass: TWiRLConfigurationClass;
begin
  LConfigClass := TWiRLConfigClassRegistry.Instance.GetImplementationOf(AInterfaceRef);
  LConfig := GetConfigByClassRef(LConfigClass);

  if not Supports(LConfig, AInterfaceRef, Result) then
    raise EWiRLException.Create('Invalid config');
end;

function TWiRLApplication.Configure<T>: T;
var
  LInterfaceRef: TGUID;
  LInterface: IInterface;
begin
  LInterfaceRef := GetTypeData(TypeInfo(T))^.GUID;
  LInterface := GetConfigByInterfaceRef(LInterfaceRef);
  if not Supports(LInterface, LInterfaceRef, Result) then
    raise EWiRLException.Create('Invalid config');
end;

constructor TWiRLApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FResourceRegistry := TWiRLResourceRegistry.Create;
  FFilterRegistry := TWiRLFilterRegistry.Create;
  FFilterRegistry.OwnsObjects := False;
  FWriterRegistry := TWiRLWriterRegistry.Create(False);
  FReaderRegistry := TWiRLReaderRegistry.Create(False);
  FConfigRegistry := TWiRLConfigRegistry.Create([doOwnsValues]);
  FAppConfigurator := TAppConfiguratorImpl.Create(Self);
  FProxy := TWiRLProxyApplication.Create(FResourceRegistry);

  FErrorMediaType := TMediaType.APPLICATION_JSON;
  if csDesigning in ComponentState then
  begin
    AddResource('*');
    AddFilter('*');
  end;
end;

procedure TWiRLApplication.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Resource.List', ReadResources, WriteResources, FResourceRegistry.Count > 0);
  Filer.DefineProperty('Filter.List', ReadFilters, WriteFilters, FFilterRegistry.Count > 0);
  Filer.DefineProperty('Reader.List', ReadReaders, WriteReaders, FReaderRegistry.Count > 0);
  Filer.DefineProperty('Writer.List', ReadWriters, WriteWriters, FWriterRegistry.Count > 0);
end;

destructor TWiRLApplication.Destroy;
begin
  Engine := nil;
  FProxy.Free;
  FReaderRegistry.Free;
  FWriterRegistry.Free;
  FFilterRegistry.Free;
  FResourceRegistry.Free;
  FConfigRegistry.Free;
  FAppConfigurator.Free;
  inherited;
end;

function TWiRLApplication.GetAppConfigurator: TAppConfigurator;
begin
  Result := FAppConfigurator;
end;

function TWiRLApplication.GetConfiguration<T>: T;
begin
  Result := GetConfigByClassRef(TWiRLConfigurationClass(T)) as T;
end;

function TWiRLApplication.GetEnginePath: string;
begin
  Result := (Engine as TWiRLRESTEngine).BasePath;
end;

function TWiRLApplication.GetFormatSettingFor(ATypeInfo: PTypeInfo): string;
begin
  Result := GetConfiguration<TWiRLFormatSettingConfig>.GetFormatSettingFor(ATypeInfo);
end;

function TWiRLApplication.GetParentComponent: TComponent;
begin
  Result := FEngine;
end;

function TWiRLApplication.GetPath: string;
begin
  if not Assigned(FEngine) then
    Result := BasePath
  else
    Result := TWiRLURL.CombinePath([(FEngine as TWiRLRESTEngine).BasePath, BasePath]);
end;

function TWiRLApplication.GetResourceCtor(const AResourceName: string): TWiRLConstructorProxy;
begin
  FResourceRegistry.TryGetValue(AResourceName, Result);
end;

class function TWiRLApplication.GetRttiContext: TRttiContext;
begin
  Result := TRttiHelper.Context;
end;

function TWiRLApplication.HasParent: Boolean;
begin
  Result := Assigned(FEngine);
end;

procedure TWiRLApplication.ReadFilters(Reader: TReader);
begin
  Reader.ReadListBegin;
  FFilterRegistry.Clear;
  while not Reader.EndOfList do
    AddFilter(Reader.ReadString);
  Reader.ReadListEnd;
end;

procedure TWiRLApplication.ReadReaders(Reader: TReader);
begin
  Reader.ReadListBegin;
  FReaderRegistry.Clear;
  while not Reader.EndOfList do
    AddReader(Reader.ReadString);
  Reader.ReadListEnd;
end;

procedure TWiRLApplication.ReadResources(Reader: TReader);
begin
  Reader.ReadListBegin;
  FResourceRegistry.Clear;
  while not Reader.EndOfList do
    AddResource(Reader.ReadString);
  Reader.ReadListEnd;
end;

procedure TWiRLApplication.ReadWriters(Reader: TReader);
begin
  Reader.ReadListBegin;
  FWriterRegistry.Clear;
  while not Reader.EndOfList do
  begin
    AddWriter(Reader.ReadString);
  end;
  Reader.ReadListEnd;
end;

function TWiRLApplication.SetSystemApp(ASystem: Boolean): IWiRLApplication;
begin
  FSystemApp := ASystem;
  Result := Self;
end;

{ TAppConfiguratorImpl }

constructor TAppConfiguratorImpl.Create(AApplication: TWiRLApplication);
begin
  inherited Create;
  FApplication := AApplication;
end;

function TAppConfiguratorImpl.GetConfigByInterfaceRef(AInterfaceRef: TGUID): IInterface;
begin
  Result := FApplication.GetConfigByInterfaceRef(AInterfaceRef);
end;

end.


