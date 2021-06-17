{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2021 WiRL Team                                      }
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
  WiRL.Configuration.Core,
  WiRL.Core.Auth.Context;

{$SCOPEDENUMS ON}

type
  IWiRLConfigurationOpenAPI = interface(IWiRLConfiguration)
  ['{BB768622-918C-4E54-A9B5-4BF6646B8F7A}']

    function SetUseSwaggerUI(): IWiRLConfigurationOpenAPI;
    function SetOpenAPIResource(AClass: TClass): IWiRLConfigurationOpenAPI;
    function SetXMLDocFolder(const AFolder: string): IWiRLConfigurationOpenAPI;
    function SetSwaggerUIFolder(const AFolder: string): IWiRLConfigurationOpenAPI;
    function SetDocumentationFolder(const AFolder: string): IWiRLConfigurationOpenAPI;

    function SetAPITitle(const ATitle: string): IWiRLConfigurationOpenAPI;
    function SetAPILogo(const ALogo: string): IWiRLConfigurationOpenAPI;
    function SetAPIDescription(const ADescription: string): IWiRLConfigurationOpenAPI;
    function SetAPIVersion(const AVersion: string): IWiRLConfigurationOpenAPI;
    function AddAPIServer(const AURL, ADescription: string): IWiRLConfigurationOpenAPI;
  end;

  TConfigurator = reference to procedure(AOpenAPIConf: IWiRLConfigurationOpenAPI);

  TServerPair = TPair<string, string>;

  [Implements(IWiRLConfigurationOpenAPI)]
  TWiRLConfigurationOpenAPI = class sealed(TWiRLConfiguration, IWiRLConfigurationOpenAPI)
  private
    const FOLDER_DOC = 'c:\doc';
    const FOLDER_XMLDOC = 'c:\xmldoc';
  private
    FClass: TClass;
    FDescription: string;
    FFolderXMLDoc: string;
    FServers: TArray<TServerPair>;
    FTitle: string;
    FVersion: string;
    FFolderDocumentation: string;
    FFolderSwaggerUI: string;
    FLogo: string;
  public
    class function Default: IWiRLConfigurationOpenAPI; static;
  public
    constructor Create; override;
    procedure DoAfterCreate; override;

    function SetUseSwaggerUI(): IWiRLConfigurationOpenAPI;
    function SetOpenAPIResource(AClass: TClass): IWiRLConfigurationOpenAPI;
    function SetXMLDocFolder(const AFolder: string): IWiRLConfigurationOpenAPI;
    function SetSwaggerUIFolder(const AFolder: string): IWiRLConfigurationOpenAPI;
    function SetDocumentationFolder(const AFolder: string): IWiRLConfigurationOpenAPI;

    function SetAPITitle(const ATitle: string): IWiRLConfigurationOpenAPI;
    function SetAPILogo(const ALogo: string): IWiRLConfigurationOpenAPI;
    function SetAPIDescription(const ADescription: string): IWiRLConfigurationOpenAPI;
    function SetAPIVersion(const AVersion: string): IWiRLConfigurationOpenAPI;
    function AddAPIServer(const AURL, ADescription: string): IWiRLConfigurationOpenAPI;
  published
    property Title: string read FTitle write FTitle;
    property Logo: string read FLogo write FLogo;
    property Version: string read FVersion write FVersion;
    property Description: string read FDescription write FDescription;
    property Servers: TArray<TServerPair> read FServers write FServers;

    property FolderXMLDoc: string read FFolderXMLDoc write FFolderXMLDoc;
    property FolderSwaggerUI: string read FFolderSwaggerUI write FFolderSwaggerUI;
    property FolderDocumentation: string read FFolderDocumentation write FFolderDocumentation;
  end;

implementation

uses
  WiRL.http.Filters,
  WiRL.Core.OpenAPI.Resource;

constructor TWiRLConfigurationOpenAPI.Create;
begin
  inherited;
end;

class function TWiRLConfigurationOpenAPI.Default: IWiRLConfigurationOpenAPI;
begin
  Result := TWiRLConfigurationOpenAPI.Create
    .SetDocumentationFolder('.')
    .SetXMLDocFolder('.')
  ;
end;

procedure TWiRLConfigurationOpenAPI.DoAfterCreate;
begin
  if not Assigned(FClass) then
    FClass := TOpenAPIResourceDefault;

  TWiRLResourceRegistry.Instance.RegisterResource(FClass);
  FApplication.SetResources(FClass.QualifiedClassName);
end;

function TWiRLConfigurationOpenAPI.SetAPIDescription(const ADescription: string): IWiRLConfigurationOpenAPI;
begin
  FDescription := ADescription;
  Result := Self;
end;

function TWiRLConfigurationOpenAPI.SetAPILogo(const ALogo: string): IWiRLConfigurationOpenAPI;
begin
  FLogo := ALogo;
  Result := Self;
end;

function TWiRLConfigurationOpenAPI.AddAPIServer(const AURL, ADescription: string): IWiRLConfigurationOpenAPI;
begin
  FServers := FServers + [TServerPair.Create(AURL, ADescription)];
  Result := Self;
end;

function TWiRLConfigurationOpenAPI.SetAPITitle(const ATitle: string): IWiRLConfigurationOpenAPI;
begin
  FTitle := ATitle;
  Result := Self;
end;

function TWiRLConfigurationOpenAPI.SetAPIVersion(const AVersion: string): IWiRLConfigurationOpenAPI;
begin
  FVersion := AVersion;
  Result := Self;
end;

function TWiRLConfigurationOpenAPI.SetDocumentationFolder(const AFolder: string): IWiRLConfigurationOpenAPI;
begin
  FFolderDocumentation := AFolder;
  Result := Self;
end;

function TWiRLConfigurationOpenAPI.SetOpenAPIResource(AClass: TClass): IWiRLConfigurationOpenAPI;
begin
  FClass := AClass;
  Result := Self;
end;

function TWiRLConfigurationOpenAPI.SetSwaggerUIFolder(const AFolder: string): IWiRLConfigurationOpenAPI;
begin
  FFolderSwaggerUI := AFolder;
  Result := Self;
end;

function TWiRLConfigurationOpenAPI.SetUseSwaggerUI: IWiRLConfigurationOpenAPI;
begin
  //TWiRLFilterRegistry.Instance.RegisterFilter<TSwaggerUIFilter>;
  //FApplication.SetFilters(TSwaggerUIFilter.QualifiedClassName);

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