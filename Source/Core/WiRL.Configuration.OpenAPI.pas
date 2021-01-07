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
  System.SysUtils, System.Classes, System.Generics.Defaults,

  WiRL.Core.Registry,
  WiRL.Configuration.Core,
  WiRL.Core.Auth.Context;

{$SCOPEDENUMS ON}

type
  IWiRLConfigurationOpenAPI = interface(IWiRLConfiguration)
  ['{BB768622-918C-4E54-A9B5-4BF6646B8F7A}']
    function SetEnableGeneration(AEnabled: Boolean; AClass: TClass): IWiRLConfigurationOpenAPI;
    function SetXMLDocFolder(const AFolder: string): IWiRLConfigurationOpenAPI;
    function SetDocumentationFolder(const AFolder: string): IWiRLConfigurationOpenAPI;
  end;

  TConfigurator = reference to procedure(AOpenAPIConf: IWiRLConfigurationOpenAPI);

  [Implements(IWiRLConfigurationOpenAPI)]
  TWiRLConfigurationOpenAPI = class sealed(TWiRLConfiguration, IWiRLConfigurationOpenAPI)
  private
    const FOLDER_DOC = 'd2lybC5zdXBlcnNlY3JldC5zZWVkLmZvci5zaWduaW5n';
    const FOLDER_XMLDOC = 'd2lybC5zdXBlcnNlY3JldC5zZWVkLmZvci5zaWduaW5n';
  private
    FEnabled: Boolean;
    FFolderXMLDoc: string;
    FFolderDoc: string;
  public
    class function Default: IWiRLConfigurationOpenAPI; static;
  public
    // Interface IWiRLConfigurationOpenAPI
    function SetEnableGeneration(AEnabled: Boolean; AClass: TClass): IWiRLConfigurationOpenAPI;
    function SetXMLDocFolder(const AFolder: string): IWiRLConfigurationOpenAPI;
    function SetDocumentationFolder(const AFolder: string): IWiRLConfigurationOpenAPI;
  published
    property FolderDoc: string read FFolderDoc write FFolderDoc;
    property FolderXMLDoc: string read FFolderXMLDoc write FFolderXMLDoc;
  end;

implementation

class function TWiRLConfigurationOpenAPI.Default: IWiRLConfigurationOpenAPI;
begin
  Result := TWiRLConfigurationOpenAPI.Create
    .SetDocumentationFolder('.')
    .SetXMLDocFolder('.')
  ;
end;

function TWiRLConfigurationOpenAPI.SetDocumentationFolder(const AFolder: string): IWiRLConfigurationOpenAPI;
begin
  FFolderDoc := AFolder;
  Result := Self;
end;

function TWiRLConfigurationOpenAPI.SetEnableGeneration(AEnabled: Boolean; AClass: TClass): IWiRLConfigurationOpenAPI;
begin
  if AEnabled = FEnabled then
    Exit(Self);

  FEnabled := AEnabled;
  if AEnabled then
    TWiRLResourceRegistry.Instance.RegisterResource(AClass)
  else
    TWiRLResourceRegistry.Instance.UnregisterResource(AClass);

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
