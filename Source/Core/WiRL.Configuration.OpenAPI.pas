{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2025 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Configuration.OpenAPI;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Generics.Defaults,

  WiRL.Core.Declarations,
  WiRL.Core.Registry,
  WiRL.Core.Exceptions,
  WiRL.Configuration.Core,
  WiRL.Core.Auth.Context,
  Neon.Core.Persistence,
  OpenAPI.Model.Classes;

{$SCOPEDENUMS ON}

type
  TOpenAPIDocCallback = reference to procedure (ADocument: TOpenAPIDocument);

  IWiRLConfigurationOpenAPI = interface(IWiRLConfiguration)
  ['{BB768622-918C-4E54-A9B5-4BF6646B8F7A}']
    function SetOpenAPIResource(AClass: TClass): IWiRLConfigurationOpenAPI;
    function SetXMLDocFolder(const AFolder: string): IWiRLConfigurationOpenAPI;
    function SetOASDocFolder(const AFolder: string): IWiRLConfigurationOpenAPI;
    function SetGUIDocFolder(const AFolder: string): IWiRLConfigurationOpenAPI;
    function SetAPILogo(const AName: string): IWiRLConfigurationOpenAPI;
    function SetAPIServer(const AHost, ADescription: string): IWiRLConfigurationOpenAPI;
    function SetAPIDocument(ADocument: TOpenAPIDocument): IWiRLConfigurationOpenAPI;
    function SetAPIDocumentCallback(ACallback: TOpenAPIDocCallback): IWiRLConfigurationOpenAPI;
    function SetNeonConfiguration(AConfig: INeonConfiguration): IWiRLConfigurationOpenAPI;
  end;

  TConfigurator = reference to procedure (AOpenAPIConf: IWiRLConfigurationOpenAPI);

  TServerPair = TPair<string, string>;

  [Implements(IWiRLConfigurationOpenAPI)]
  TWiRLConfigurationOpenAPI = class sealed(TWiRLConfiguration, IWiRLConfigurationOpenAPI)
  private
    FClass: TClass;
    FNeonConfig: INeonConfiguration;
    FDocument: TOpenAPIDocument;
    FCallback: TOpenAPIDocCallback;
    FAPILogo: string;
    FServers: TArray<TOpenAPIServer>;
    FFolderXMLDoc: string;
    FFolderGUIDoc: string;
    FFolderOASDoc: string;
  public
    class function Default: IWiRLConfigurationOpenAPI; static;
  public
    constructor Create; override;
    destructor Destroy; override;

    function ApplyConfig: IWiRLApplication; override;

    function SetOpenAPIResource(AClass: TClass): IWiRLConfigurationOpenAPI;
    function SetXMLDocFolder(const AFolder: string): IWiRLConfigurationOpenAPI;
    function SetOASDocFolder(const AFolder: string): IWiRLConfigurationOpenAPI;
    function SetGUIDocFolder(const AFolder: string): IWiRLConfigurationOpenAPI;
    function SetAPILogo(const ALogo: string): IWiRLConfigurationOpenAPI;
    function SetAPIServer(const AHost, ADescription: string): IWiRLConfigurationOpenAPI;
    function SetAPIDocument(ADocument: TOpenAPIDocument): IWiRLConfigurationOpenAPI;
    function SetAPIDocumentCallback(ACallback: TOpenAPIDocCallback): IWiRLConfigurationOpenAPI;
    function SetNeonConfiguration(AConfig: INeonConfiguration): IWiRLConfigurationOpenAPI;

    property NeonConfig: INeonConfiguration read FNeonConfig;
  published
    property APILogo: string read FAPILogo write FAPILogo;
    property FolderXMLDoc: string read FFolderXMLDoc write FFolderXMLDoc;
    property FolderOASDoc: string read FFolderOASDoc write FFolderOASDoc;
    property FolderGUIDoc: string read FFolderGUIDoc write FFolderGUIDoc;
    property Document: TOpenAPIDocument read FDocument write FDocument;
    property Callback: TOpenAPIDocCallback read FCallback write FCallback;
  end;

implementation

uses
  WiRL.http.URL,
  WiRL.http.Filters,
  WiRL.Core.OpenAPI.Resource;

function TWiRLConfigurationOpenAPI.ApplyConfig: IWiRLApplication;
var
  LServer: TOpenAPIServer;
begin
  if not Assigned(FClass) then
    FClass := TOpenAPIResourceDefault;

  if not TWiRLResourceRegistry.Instance.ResourceExists(FClass) then
    TWiRLResourceRegistry.Instance.RegisterResource(FClass);

  FApplication.SetResources(FClass.QualifiedClassName);

  for LServer in FServers do
    FDocument.Servers.Add(LServer);

  Result := inherited ApplyConfig;
end;

constructor TWiRLConfigurationOpenAPI.Create;
begin
  inherited;
  FServers := [];
  FDocument := TOpenAPIDocument.Create(TOpenAPIVersion.v303);
end;

class function TWiRLConfigurationOpenAPI.Default: IWiRLConfigurationOpenAPI;
begin
  Result := TWiRLConfigurationOpenAPI.Create
    .SetXMLDocFolder('.');
end;

destructor TWiRLConfigurationOpenAPI.Destroy;
begin
  FDocument.Free;
  inherited;
end;

function TWiRLConfigurationOpenAPI.SetOpenAPIResource(AClass: TClass): IWiRLConfigurationOpenAPI;
begin
  FClass := AClass;
  Result := Self;
end;

function TWiRLConfigurationOpenAPI.SetAPIDocument(ADocument: TOpenAPIDocument): IWiRLConfigurationOpenAPI;
begin
  FDocument.Free;
  FDocument := ADocument;
  Result := Self;
end;

function TWiRLConfigurationOpenAPI.SetAPIDocumentCallback(ACallback: TOpenAPIDocCallback): IWiRLConfigurationOpenAPI;
begin
  FCallback := ACallback;
  Result := Self;
end;

function TWiRLConfigurationOpenAPI.SetAPILogo(const ALogo: string): IWiRLConfigurationOpenAPI;
begin
  FAPILogo := ALogo;
  Result := Self;
end;

function TWiRLConfigurationOpenAPI.SetAPIServer(const AHost, ADescription: string): IWiRLConfigurationOpenAPI;
var
  LURL: string;
  LServer: TOpenAPIServer;
begin
  LURL := TWiRLURL.CombinePath([AHost, FApplication.GetPath]);
  LServer := TOpenAPIServer.Create(LURL, ADescription);
  FServers := FServers + [LServer];
  Result := Self;
end;

function TWiRLConfigurationOpenAPI.SetGUIDocFolder(const AFolder: string): IWiRLConfigurationOpenAPI;
begin
  FFolderGUIDoc := AFolder;
  Result := Self;
end;

function TWiRLConfigurationOpenAPI.SetOASDocFolder(const AFolder: string): IWiRLConfigurationOpenAPI;
begin
  FFolderOASDoc := AFolder;
  Result := Self;
end;

function TWiRLConfigurationOpenAPI.SetNeonConfiguration(AConfig: INeonConfiguration): IWiRLConfigurationOpenAPI;
begin
  FNeonConfig := AConfig;
  Result := Self;
end;

function TWiRLConfigurationOpenAPI.SetXMLDocFolder(const AFolder: string): IWiRLConfigurationOpenAPI;
begin
  FFolderXMLDoc := AFolder;
  Result := Self;
end;

initialization
  TWiRLConfigClassRegistry.Instance.RegisterConfigClass(TWiRLConfigurationOpenAPI);

end.