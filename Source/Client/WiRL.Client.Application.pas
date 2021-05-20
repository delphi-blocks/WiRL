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
  System.SysUtils, System.Classes, System.Rtti, System.Generics.Collections,
  System.Contnrs, System.Types,
  WiRL.Configuration.Core,
  WiRL.Core.Classes,
  WiRL.Core.MessageBodyReader,
  WiRL.Core.MessageBodyWriter,
  WiRL.http.Client.Interfaces,
  WiRL.Client.Filters,
  WiRL.http.Headers,
  WiRL.http.Client;

type
  TWiRLClientApplication = class;

  TWiRLInvocation = record
  private
    FWiRLInvocation: IWiRLInvocation;
  public
    function Filters(const AFilters: TStringDynArray): TWiRLInvocation;
    function Target(const AUrl: string): TWiRLInvocation;
    function Accept(const AAccept: string): TWiRLInvocation;
    function ContentType(const AContentType: string): TWiRLInvocation;
    function AcceptLanguage(const AAcceptLanguage: string): TWiRLInvocation;
    function Header(const AName, AValue: string): TWiRLInvocation;
    function Authorization(const AValue: string): TWiRLInvocation;
    function QueryParam(const AName: string; const AValue: TValue): TWiRLInvocation; overload;
    function QueryParam<T>(const AName: string; const AValue: T): TWiRLInvocation; overload;
    function PathParam(const AName: string; const AValue: TValue): TWiRLInvocation; overload;
    function PathParam<T>(const AName: string; const AValue: T): TWiRLInvocation; overload;

    function Get<T>: T; overload;
    procedure Get(AResponseEntity: TObject); overload;
    function Post<T, V>(const ARequestEntity: T): V; overload;
    procedure Post<T>(const ARequestEntity: T; AResponseEntity: TObject); overload;
    function Put<T, V>(const ARequestEntity: T): V; overload;
    procedure Put<T>(const ARequestEntity: T; AResponseEntity: TObject); overload;
    function Delete<T>: T; overload;
    procedure Delete(AResponseEntity: TObject); overload;
    function Patch<T, V>(const ARequestEntity: T): V; overload;
    procedure Patch<T>(const ARequestEntity: T; AResponseEntity: TObject); overload;

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
    FDefaultClient: TWiRLClient;
    FClient: TWiRLClient;
    //FFilterRegistry: TWiRLFilterRegistry;
    FWriterRegistry: TWiRLWriterRegistry;
    FReaderRegistry: TWiRLReaderRegistry;
    FConfigRegistry: TWiRLConfigRegistry;
    FAppConfigurator: TAppConfigurator;
    FResources: TObjectList<TObject>;
    FFilterRegistry: TWiRLClientFilterRegistry;
    function GetClient: TWiRLClient;
    procedure SetClient(const Value: TWiRLClient);
    function GetDefaultClient: TWiRLClient;
    function CheckFilterNameBinding(AClientResource: TObject; AAttribute: TCustomAttribute): Boolean;
  protected
    function GetPath: string; virtual;
    function AddFilter(const AFilter: string): Boolean;
    function AddWriter(const AWriter: string): Boolean;
    function AddReader(const AReader: string): Boolean;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    // Handles the parent/child relationship for the designer
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  public
    property Resources: TObjectList<TObject> read FResources;
    procedure ApplyRequestFilter(AClientResource: TObject; const AHttpMethod: string; ARequestStream: TStream; out AResponse: IWiRLResponse);
    procedure ApplyResponseFilter(AClientResource: TObject; const AHttpMethod: string; ARequestStream: TStream; AResponse: IWiRLResponse);
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

    property FilterRegistry: TWiRLClientFilterRegistry read FFilterRegistry write FFilterRegistry;
    property WriterRegistry: TWiRLWriterRegistry read FWriterRegistry write FWriterRegistry;
    property ReaderRegistry: TWiRLReaderRegistry read FReaderRegistry write FReaderRegistry;
    property Configs: TWiRLConfigRegistry read FConfigRegistry write FConfigRegistry;
    property Plugin: TAppConfigurator read GetAppConfigurator;

    function HasDefaultClient: Boolean;
    function Resource(const AUrl: string): TWiRLInvocation;
  published
    property DefaultMediaType: string read FDefaultMediaType write FDefaultMediaType;
    property AppName: string read FAppName write FAppName;
    property Client: TWiRLClient read GetClient write SetClient;
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
  WiRL.Configuration.Converter,
  WiRL.Client.Utils,
  WiRL.Client.CustomResource,
  WiRL.Client.Resource,
  WiRL.Core.Utils,
  WiRL.Core.Converter,
  WiRL.http.URL;

type
  TWiRLResourceWrapper = class(TInterfacedObject, IWiRLInvocation)
  private
    FApp: TWiRLClientApplication;
    FResource: TWiRLClientCustomResource;
    function ValueToString(const AValue: TValue): string;
  protected
    function GetResource: TObject;
  public
    procedure Target(const AUrl: string);
    procedure ContentType(const AContentType: string);
    procedure Accept(const AAccept: string);
    procedure AcceptLanguage(const AAcceptLanguage: string);
    procedure QueryParam(const AName: string; const AValue: TValue);
    procedure PathParam(const AName: string; const AValue: TValue);

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
var
  LRegistry: TWiRLClientFilterRegistry;
  LInfo: TWiRLClientFilterConstructorInfo;
begin
  if csDesigning in ComponentState then
  begin
    FFilterRegistry.AddFilterName(AFilter);
    Exit(True);
  end;

  Result := False;
  LRegistry := TWiRLClientFilterRegistry.Instance;

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

function TWiRLClientApplication.AddReader(const AReader: string): Boolean;
var
  LGlobalRegistry: TWiRLReaderRegistry;
  LReader: TWiRLReaderRegistry.TReaderInfo;
begin
//  if csDesigning in ComponentState then
//  begin
//    FReaderRegistry.AddReaderName(AReader);
//    Exit(True);
//  end;

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
//  if csDesigning in ComponentState then
//  begin
//    FWriterRegistry.AddWriterName(AWriter);
//    Exit(True);
//  end;

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

procedure TWiRLClientApplication.ApplyRequestFilter(AClientResource: TObject;
  const AHttpMethod: string; ARequestStream: TStream; out AResponse: IWiRLResponse);
var
  LInfo: TWiRLClientFilterConstructorInfo;
  LRequestContext: TWiRLClientRequestContext;
begin
  LRequestContext := TWiRLClientRequestContext.Create(AClientResource, AHttpMethod, ARequestStream);
  try
    for LInfo in FFilterRegistry do
    begin
      if Supports(LInfo.TypeTClass, IWiRLClientRequestFilter) then
      begin
        if CheckFilterNameBinding(AClientResource, LInfo.Attribute) then
        begin
          LRequestContext.Response := AResponse;
          LInfo.GetRequestFilter.Filter(LRequestContext);
          if LRequestContext.Aborted then
            AResponse := LRequestContext.Response;
        end;
      end;
    end;
  finally
    LRequestContext.Free;
  end;
end;

procedure TWiRLClientApplication.ApplyResponseFilter(AClientResource: TObject;
  const AHttpMethod: string; ARequestStream: TStream; AResponse: IWiRLResponse);
var
  LInfo: TWiRLClientFilterConstructorInfo;
  LResponseContext: TWiRLClientResponseContext;
begin
  LResponseContext := TWiRLClientResponseContext.Create(AClientResource, AHttpMethod, ARequestStream, AResponse);
  try
    for LInfo in FFilterRegistry do
    begin
      if Supports(LInfo.TypeTClass, IWiRLClientResponseFilter) then
      begin
        if CheckFilterNameBinding(AClientResource, LInfo.Attribute) then
          LInfo.GetResponseFilter.Filter(LResponseContext);
      end;
    end;
  finally
    LResponseContext.Free;
  end;
end;

function TWiRLClientApplication.CheckFilterNameBinding(AClientResource: TObject;
  AAttribute: TCustomAttribute): Boolean;
var
  LResource: TWiRLClientCustomResource;
begin
  Result := True;
  if Assigned(AAttribute) then
  begin
    LResource := AClientResource as TWiRLClientCustomResource;
    Result := LResource.HasFilter(AAttribute);
  end;
end;

constructor TWiRLClientApplication.Create(AOwner: TComponent);
begin
  inherited;
  FFilterRegistry := TWiRLClientFilterRegistry.Create;
  FFilterRegistry.OwnsObjects := False;

  FClient := GetDefaultClient;

  FWriterRegistry := TWiRLWriterRegistry.Create(False);
  FReaderRegistry := TWiRLReaderRegistry.Create(False);
  FConfigRegistry := TWiRLConfigRegistry.Create([doOwnsValues]);

  FResources := TObjectList<TObject>.Create;

  FDefaultMediaType := 'application/json';
  FAppName := 'app';
  if TWiRLComponentHelper.IsDesigning(Self) then
    FClient := TWiRLComponentHelper.FindDefault<TWiRLClient>(Self);

  FAppConfigurator := TAppConfiguratorImpl.Create(Self);
end;

destructor TWiRLClientApplication.Destroy;
begin
  FDefaultClient.Free;
  FReaderRegistry.Free;
  FWriterRegistry.Free;
  FFilterRegistry.Free;
  FConfigRegistry.Free;

  FAppConfigurator.Free;
  FResources.Free;
  inherited;
end;

function TWiRLClientApplication.GetAppConfigurator: TAppConfigurator;
begin
  Result := FAppConfigurator;
end;

procedure TWiRLClientApplication.GetChildren(Proc: TGetChildProc;
  Root: TComponent);
var
  LResource: TObject;
begin
  inherited;
  for LResource in FResources do
  begin
    Proc(LResource as TWiRLClientCustomResource);
  end;
end;

function TWiRLClientApplication.GetClient: TWiRLClient;
begin
  if not Assigned(FClient) then
    FClient := FDefaultClient;
  Result := FClient;
end;

function TWiRLClientApplication.GetDefaultClient: TWiRLClient;
begin
  if not Assigned(FDefaultClient) then
  begin
    FDefaultClient := TWiRLClient.Create(Self);
    FDefaultClient.Name := 'WiRLClient1';
    FDefaultClient.SetSubComponent(True);
  end;
  Result := FDefaultClient;
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

function TWiRLClientApplication.HasDefaultClient: Boolean;
begin
  Result := FClient = FDefaultClient;
end;

procedure TWiRLClientApplication.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FClient then
    begin
      FClient := nil;
    end;
    // If it's a resource remove from the list (without free it)
    //FResources.Extract(AComponent);
  end;
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

procedure TWiRLClientApplication.SetClient(const Value: TWiRLClient);
begin
  FClient := Value;
end;

function TWiRLClientApplication.SetErrorMediaType(
  const AMediaType: string): IWiRLApplication;
begin
  raise EWiRLException.CreateFmt('Method not found for class [%s]', [Self.ClassName]);
end;

function TWiRLClientApplication.SetFilters(
  const AFilters: string): IWiRLApplication;
begin
  SetFilters(AFilters.Split([',']));
  Result := Self;
end;

function TWiRLClientApplication.SetFilters(
  const AFilters: System.TArray<System.string>): IWiRLApplication;
var
  LFilter: string;
begin
  for LFilter in AFilters do
    Self.AddFilter(LFilter);
  Result := Self;
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

function TWiRLInvocation.Authorization(const AValue: string): TWiRLInvocation;
begin
  Header('Authorization', AValue);
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

procedure TWiRLInvocation.Delete(AResponseEntity: TObject);
begin
  (FWiRLInvocation.Resource as TWiRLClientCustomResource).Delete(AResponseEntity);
end;

function TWiRLInvocation.Delete<T>: T;
begin
  Result := (FWiRLInvocation.Resource as TWiRLClientCustomResource).Delete<T>;
end;

function TWiRLInvocation.Filters(const AFilters: TStringDynArray): TWiRLInvocation;
begin
  (FWiRLInvocation.Resource as TWiRLClientCustomResource).SetFilters(AFilters);
  Result := Self;
end;

procedure TWiRLInvocation.Get(AResponseEntity: TObject);
begin
  (FWiRLInvocation.Resource as TWiRLClientCustomResource).Get(AResponseEntity);
end;

function TWiRLInvocation.Get<T>: T;
begin
  Result := (FWiRLInvocation.Resource as TWiRLClientCustomResource).Get<T>;
end;

function TWiRLInvocation.Header(const AName, AValue: string): TWiRLInvocation;
begin
  (FWiRLInvocation.Resource as TWiRLClientCustomResource).Headers.Values[AName] := AValue;
  Result := Self;
end;

function TWiRLInvocation.Patch<T, V>(const ARequestEntity: T): V;
begin
  Result := (FWiRLInvocation.Resource as TWiRLClientCustomResource).Patch<T,V>(ARequestEntity);
end;

procedure TWiRLInvocation.Patch<T>(const ARequestEntity: T; AResponseEntity: TObject);
begin
  (FWiRLInvocation.Resource as TWiRLClientCustomResource).Patch(ARequestEntity, AResponseEntity);
end;

function TWiRLInvocation.PathParam(const AName: string;
  const AValue: TValue): TWiRLInvocation;
begin
  FWiRLInvocation.PathParam(AName, AValue);
  Result := Self;
end;

function TWiRLInvocation.PathParam<T>(const AName: string;
  const AValue: T): TWiRLInvocation;
begin
  FWiRLInvocation.PathParam(AName, TValue.From<T>(AValue));
  Result := Self;
end;

function TWiRLInvocation.Post<T, V>(const ARequestEntity: T): V;
begin
  Result := (FWiRLInvocation.Resource as TWiRLClientCustomResource).Post<T,V>(ARequestEntity);
end;

procedure TWiRLInvocation.Post<T>(const ARequestEntity: T; AResponseEntity: TObject);
begin
  (FWiRLInvocation.Resource as TWiRLClientCustomResource).Post(ARequestEntity, AResponseEntity);
end;

function TWiRLInvocation.Put<T, V>(const ARequestEntity: T): V;
begin
  Result := (FWiRLInvocation.Resource as TWiRLClientCustomResource).Put<T,V>(ARequestEntity);
end;

procedure TWiRLInvocation.Put<T>(const ARequestEntity: T; AResponseEntity: TObject);
begin
  (FWiRLInvocation.Resource as TWiRLClientCustomResource).Put(ARequestEntity, AResponseEntity);
end;

function TWiRLInvocation.QueryParam<T>(const AName: string;
  const AValue: T): TWiRLInvocation;
begin
  FWiRLInvocation.QueryParam(AName, TValue.From<T>(AValue));
  Result := Self;
end;

function TWiRLInvocation.QueryParam(const AName: string;
  const AValue: TValue): TWiRLInvocation;
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

function TWiRLResourceWrapper.ValueToString(const AValue: TValue): string;
var
  LConfig: TWiRLFormatSettingConfig;
begin
  LConfig := FApp.GetConfigByClassRef(TWiRLFormatSettingConfig) as TWiRLFormatSettingConfig;
  Result := TWiRLConvert.From(AValue, AValue.TypeInfo, LConfig.GetFormatSettingFor(AValue.TypeInfo));
end;

procedure TWiRLResourceWrapper.Accept(const AAccept: string);
begin
  if not Assigned(FResource) then
    raise EWiRLClientException.Create('Resource not found');
  FResource.Headers.Accept := AAccept;
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
  FResource.Headers.ContentType := AContentType;
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

procedure TWiRLResourceWrapper.PathParam(const AName: string; const AValue: TValue);
begin
  if not Assigned(FResource) then
    raise EWiRLClientException.Create('Resource not found');

  FResource.PathParams.Values[AName] := ValueToString(AValue);
end;

procedure TWiRLResourceWrapper.QueryParam(const AName: string; const AValue: TValue);
begin
  if not Assigned(FResource) then
    raise EWiRLClientException.Create('Resource not found');
  FResource.QueryParams.Values[AName] := ValueToString(AValue);
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
