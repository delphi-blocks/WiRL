{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Client.Application;

{$I ..\Core\WiRL.inc}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  WiRL.Configuration.Core,
  WiRL.Core.Classes,
  WiRL.Core.MessageBodyReader,
  WiRL.Core.MessageBodyWriter,
  WiRL.http.Filters,
  WiRL.http.Client;

type
  TWiRLClientApplication = class;

  TWiRLInvocation = record
  private
    FWiRLInvocation: IWiRLInvocation;
  public
    function Target(const AUrl: string): TWiRLInvocation;
    function Accept(const AAccept: string): TWiRLInvocation;
    function ContentType(const AContentType: string): TWiRLInvocation;
    function AcceptLanguage(const AAcceptLanguage: string): TWiRLInvocation;
    function Header(const AName, AValue: string): TWiRLInvocation;
    function QueryParam(const AName, AValue: string): TWiRLInvocation;

    function Get<T>: T;
    function Post<T, V>(const Value: T): V;
    function Put<T, V>(const Value: T): V;
    function Delete<T>: T;
    function Patch<T, V>(const Value: T): V;

    constructor Create(AApplication: TWiRLClientApplication);
  end;

  {$IFDEF HAS_NEW_PIDS}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator32 or pidiOSDevice32 or pidAndroid32Arm)]
  {$ELSE}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidAndroid)]
  {$ENDIF}
  TWiRLClientApplication = class(TComponent, IWiRLApplication)
  private
    FAppName: string;
    FDefaultMediaType: string;
    FClient: TWiRLClient;
    //FFilterRegistry: TWiRLFilterRegistry;
    FWriterRegistry: TWiRLWriterRegistry;
    FReaderRegistry: TWiRLReaderRegistry;
    FConfigRegistry: TWiRLConfigRegistry;
    FAppConfigurator: TAppConfigurator;
  protected
    function GetPath: string; virtual;
    function AddFilter(const AFilter: string): Boolean;
    function AddWriter(const AWriter: string): Boolean;
    function AddReader(const AReader: string): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    { IWiRLApplication }
    function SetWriters(const AWriters: TArray<string>): IWiRLApplication; overload;
    function SetWriters(const AWriters: string): IWiRLApplication; overload;
    function SetReaders(const AReaders: TArray<string>): IWiRLApplication; overload;
    function SetReaders(const AReaders: string): IWiRLApplication; overload;
    function SetResources(const AResources: string): IWiRLApplication; overload;
    function SetResources(const AResources: System.TArray<System.string>): IWiRLApplication; overload;
    function SetFilters(const AFilters: System.TArray<System.string>): IWiRLApplication; overload;
    function SetFilters(const AFilters: string): IWiRLApplication; overload;
    function SetBasePath(const ABasePath: string): IWiRLApplication;
    function SetSystemApp(ASystem: Boolean): IWiRLApplication;
    function SetErrorMediaType(const AMediaType: string): IWiRLApplication;
    function SetAppName(const AAppName: string): IWiRLApplication;
    function GetAppConfigurator: TAppConfigurator;
    function AddApplication(const ABasePath: string): IWiRLApplication;
    function GetConfigByClassRef(AClass: TWiRLConfigurationClass): TWiRLConfiguration;
    function AddConfiguration(const AConfiguration: TWiRLConfiguration): IWiRLApplication;

    function GetConfigByInterfaceRef(AInterfaceRef: TGUID): IInterface;

    //property FilterRegistry: TWiRLFilterRegistry read FFilterRegistry write FFilterRegistry;
    property WriterRegistry: TWiRLWriterRegistry read FWriterRegistry write FWriterRegistry;
    property ReaderRegistry: TWiRLReaderRegistry read FReaderRegistry write FReaderRegistry;
    property Configs: TWiRLConfigRegistry read FConfigRegistry write FConfigRegistry;
    property Plugin: TAppConfigurator read GetAppConfigurator;

    function Resource(const AUrl: string): TWiRLInvocation;
  published
    property DefaultMediaType: string read FDefaultMediaType write FDefaultMediaType;
    property AppName: string read FAppName write FAppName;
    property Client: TWiRLClient read FClient write FClient;
    property Path: string read GetPath;
  end;

  TAppConfiguratorImpl = class(TAppConfigurator)
  private
    FApplication: TWiRLClientApplication;
  protected
    function GetConfigByInterfaceRef(AInterfaceRef: TGUID): IInterface; override;
  public
    property Application: TWiRLClientApplication read FApplication;
    constructor Create(AApplication: TWiRLClientApplication);
  end;

implementation

uses
  WiRL.Rtti.Utils,
  WiRL.Client.Utils,
  WiRL.Client.CustomResource,
  WiRL.Client.Resource,
  WiRL.http.Client.Interfaces,
  WiRL.Core.Utils,
  WiRL.http.URL;

type
  TWiRLResourceWrapper = class(TInterfacedObject, IWiRLInvocation)
  private
    FApp: TWiRLClientApplication;
    FResource: TWiRLClientCustomResource;
  protected
    function GetResource: TObject;
  public
    procedure Target(const AUrl: string);
    procedure ContentType(const AContentType: string);
    procedure Accept(const AAccept: string);
    procedure AcceptLanguage(const AAcceptLanguage: string);
    procedure QueryParam(const AName, AValue: string);

    constructor Create(AApplication: TWiRLClientApplication);
    destructor Destroy; override;
  end;

{ TWiRLClientApplication }

function TWiRLClientApplication.AddApplication(
  const ABasePath: string): IWiRLApplication;
begin
  raise EWiRLException.CreateFmt('Method not found for class [%s]', [Self.ClassName]);
end;

function TWiRLClientApplication.AddConfiguration(
  const AConfiguration: TWiRLConfiguration): IWiRLApplication;
begin
  FConfigRegistry.Add(AConfiguration);
  Result := Self;
end;

function TWiRLClientApplication.AddFilter(const AFilter: string): Boolean;
begin
  raise EWiRLException.CreateFmt('Method not found for class [%s]', [Self.ClassName]);
end;

function TWiRLClientApplication.AddReader(const AReader: string): Boolean;
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

function TWiRLClientApplication.AddWriter(const AWriter: string): Boolean;
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

constructor TWiRLClientApplication.Create(AOwner: TComponent);
begin
  inherited;
  //FFilterRegistry := TWiRLFilterRegistry.Create;
  //FFilterRegistry.OwnsObjects := False;

  FWriterRegistry := TWiRLWriterRegistry.Create(False);
  FReaderRegistry := TWiRLReaderRegistry.Create(False);
  FConfigRegistry := TWiRLConfigRegistry.Create([doOwnsValues]);

  FDefaultMediaType := 'application/json';
  FAppName := 'app';
  if TWiRLComponentHelper.IsDesigning(Self) then
    FClient := TWiRLComponentHelper.FindDefault<TWiRLClient>(Self);

  FAppConfigurator := TAppConfiguratorImpl.Create(Self);
end;

destructor TWiRLClientApplication.Destroy;
begin
  FReaderRegistry.Free;
  FWriterRegistry.Free;
  //FFilterRegistry.Free;
  FConfigRegistry.Free;

  FAppConfigurator.Free;
  inherited;
end;

function TWiRLClientApplication.GetAppConfigurator: TAppConfigurator;
begin
  Result := FAppConfigurator;
end;

function TWiRLClientApplication.GetConfigByClassRef(
  AClass: TWiRLConfigurationClass): TWiRLConfiguration;
begin
  Result := FConfigRegistry.GetApplicationConfig(AClass, Self);
end;

function TWiRLClientApplication.GetConfigByInterfaceRef(
  AInterfaceRef: TGUID): IInterface;
var
  LConfig: TWiRLConfiguration;
  LConfigClass: TWiRLConfigurationClass;
begin
  LConfigClass := TWiRLConfigClassRegistry.Instance.GetImplementationOf(AInterfaceRef);
  LConfig := GetConfigByClassRef(LConfigClass);

  if not Supports(LConfig, AInterfaceRef, Result) then
    raise EWiRLException.Create('Invalid config');
end;

function TWiRLClientApplication.GetPath: string;
var
  LEngine: string;
begin
  LEngine := '';
  if Assigned(FClient) then
    LEngine := FClient.WiRLEngineURL;

  Result := TWiRLURL.CombinePath([LEngine, AppName])
end;

function TWiRLClientApplication.Resource(
  const AUrl: string): TWiRLInvocation;
begin
  Result := TWiRLInvocation.Create(Self);
  Result.Target(AUrl);
end;

function TWiRLClientApplication.SetReaders(const AReaders: TArray<string>): IWiRLApplication;
var
  LReader: string;
begin
  for LReader in AReaders do
    Self.AddReader(LReader);
  Result := Self;
end;

function TWiRLClientApplication.SetAppName(
  const AAppName: string): IWiRLApplication;
begin
  raise EWiRLException.CreateFmt('Method not found for class [%s]', [Self.ClassName]);
end;

function TWiRLClientApplication.SetBasePath(
  const ABasePath: string): IWiRLApplication;
begin
  raise EWiRLException.CreateFmt('Method not found for class [%s]', [Self.ClassName]);
end;

function TWiRLClientApplication.SetErrorMediaType(
  const AMediaType: string): IWiRLApplication;
begin
  raise EWiRLException.CreateFmt('Method not found for class [%s]', [Self.ClassName]);
end;

function TWiRLClientApplication.SetFilters(
  const AFilters: string): IWiRLApplication;
begin
  raise EWiRLException.CreateFmt('Method not found for class [%s]', [Self.ClassName]);
end;

function TWiRLClientApplication.SetFilters(
  const AFilters: System.TArray<System.string>): IWiRLApplication;
begin
  raise EWiRLException.CreateFmt('Method not found for class [%s]', [Self.ClassName]);
end;

function TWiRLClientApplication.SetReaders(const AReaders: string): IWiRLApplication;
begin
  SetReaders(AReaders.Split([',']));
  Result := Self;
end;

function TWiRLClientApplication.SetResources(
  const AResources: System.TArray<System.string>): IWiRLApplication;
begin
  raise EWiRLException.CreateFmt('Method not found for class [%s]', [Self.ClassName]);
end;

function TWiRLClientApplication.SetResources(
  const AResources: string): IWiRLApplication;
begin
  raise EWiRLException.CreateFmt('Method not found for class [%s]', [Self.ClassName]);
end;

function TWiRLClientApplication.SetSystemApp(
  ASystem: Boolean): IWiRLApplication;
begin
  raise EWiRLException.CreateFmt('Method not found for class [%s]', [Self.ClassName]);
end;

function TWiRLClientApplication.SetWriters(const AWriters: TArray<string>): IWiRLApplication;
var
  LWriter: string;
begin
  for LWriter in AWriters do
    Self.AddWriter(LWriter);
  Result := Self;
end;

function TWiRLClientApplication.SetWriters(const AWriters: string): IWiRLApplication;
begin
  SetWriters(AWriters.Split([',']));
  Result := Self;
end;

{ TAppConfiguratorImpl }

constructor TAppConfiguratorImpl.Create(AApplication: TWiRLClientApplication);
begin
  inherited Create;
  FApplication := AApplication;
end;

function TAppConfiguratorImpl.GetConfigByInterfaceRef(
  AInterfaceRef: TGUID): IInterface;
begin
  Result := FApplication.GetConfigByInterfaceRef(AInterfaceRef);
end;

{ TWiRLInvocation }

function TWiRLInvocation.Accept(
  const AAccept: string): TWiRLInvocation;
begin
  FWiRLInvocation.Accept(AAccept);
  Result := Self;
end;

function TWiRLInvocation.AcceptLanguage(
  const AAcceptLanguage: string): TWiRLInvocation;
begin
//  Result := (FWiRLInvocation.Resource as TWiRLClientCustomResource).Get<T>;
  Result := Header('Accept-Language', AAcceptLanguage);
  Result := Self;
end;

function TWiRLInvocation.ContentType(
  const AContentType: string): TWiRLInvocation;
begin
  FWiRLInvocation.ContentType(AContentType);
  Result := Self;
end;

constructor TWiRLInvocation.Create(AApplication: TWiRLClientApplication);
begin
  FWiRLInvocation := TWiRLResourceWrapper.Create(AApplication);
end;

function TWiRLInvocation.Delete<T>: T;
begin
  Result := (FWiRLInvocation.Resource as TWiRLClientCustomResource).GenericDelete<T>;
end;

function TWiRLInvocation.Get<T>: T;
begin
  Result := (FWiRLInvocation.Resource as TWiRLClientCustomResource).GenericGet<T>;
end;

function TWiRLInvocation.Header(const AName, AValue: string): TWiRLInvocation;
begin
  (FWiRLInvocation.Resource as TWiRLClientCustomResource).HeaderFields[AName] := AValue;
  Result := Self;
end;

function TWiRLInvocation.Patch<T, V>(const Value: T): V;
begin
  Result := (FWiRLInvocation.Resource as TWiRLClientCustomResource).GenericPatch<T,V>(Value);
end;

function TWiRLInvocation.Post<T, V>(const Value: T): V;
begin
  Result := (FWiRLInvocation.Resource as TWiRLClientCustomResource).GenericPost<T,V>(Value);
end;

function TWiRLInvocation.Put<T, V>(const Value: T): V;
begin
  Result := (FWiRLInvocation.Resource as TWiRLClientCustomResource).GenericPut<T,V>(Value);
end;

function TWiRLInvocation.QueryParam(const AName,
  AValue: string): TWiRLInvocation;
begin
  FWiRLInvocation.QueryParam(AName, AValue);
  Result := Self;
end;

function TWiRLInvocation.Target(const AUrl: string): TWiRLInvocation;
begin
  FWiRLInvocation.Target(AUrl);
  Result := Self;
end;

{ TWiRLResourceWrapper }

procedure TWiRLResourceWrapper.Accept(const AAccept: string);
begin
  if not Assigned(FResource) then
    raise EWiRLClientException.Create('Resource not found');
  FResource.SpecificAccept := AAccept;
end;

procedure TWiRLResourceWrapper.AcceptLanguage(const AAcceptLanguage: string);
begin
  if not Assigned(FResource) then
    raise EWiRLClientException.Create('Resource not found');
  // FResource.SpecificAcceptLanguage := AAccept;
end;

procedure TWiRLResourceWrapper.ContentType(const AContentType: string);
begin
  if not Assigned(FResource) then
    raise EWiRLClientException.Create('Resource not found');
  FResource.SpecificContentType := AContentType;
end;

constructor TWiRLResourceWrapper.Create(AApplication: TWiRLClientApplication);
begin
  inherited Create;
  FApp := AApplication;
  FResource := nil;
end;

destructor TWiRLResourceWrapper.Destroy;
begin
  FResource.Free;
  inherited;
end;

function TWiRLResourceWrapper.GetResource: TObject;
begin
  if not Assigned(FResource) then
    raise EWiRLClientException.Create('Resource not found');
  Result := FResource;
end;

procedure TWiRLResourceWrapper.QueryParam(const AName, AValue: string);
begin
  if not Assigned(FResource) then
    raise EWiRLClientException.Create('Resource not found');
  FResource.QueryParams.Values[AName] := AValue;
end;

procedure TWiRLResourceWrapper.Target(const AUrl: string);
begin
  if not Assigned(FResource) then
  begin
    FResource := TWiRLClientResource.Create(nil);
    FResource.Application := FApp;
  end;
  FResource.Resource := AUrl;
end;

end.
