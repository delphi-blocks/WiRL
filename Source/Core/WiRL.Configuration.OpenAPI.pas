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
  WiRL.Core.Auth.Context,
  OpenAPI.Model.Classes;

{$SCOPEDENUMS ON}

type
  IWiRLConfigurationOpenAPI = interface(IWiRLConfiguration)
  ['{BB768622-918C-4E54-A9B5-4BF6646B8F7A}']

    function SetOpenAPIResource(AClass: TClass): IWiRLConfigurationOpenAPI;
    function SetXMLDocFolder(const AFolder: string): IWiRLConfigurationOpenAPI;
    function SetGUIDocFolder(const AFolder: string): IWiRLConfigurationOpenAPI;
    function SetAPILogo(const AName: string): IWiRLConfigurationOpenAPI;

    function SetAPIDocument(ADocument: TOpenAPIDocument): IWiRLConfigurationOpenAPI;
  end;

  TConfigurator = reference to procedure(AOpenAPIConf: IWiRLConfigurationOpenAPI);

  TServerPair = TPair<string, string>;

  [Implements(IWiRLConfigurationOpenAPI)]
  TWiRLConfigurationOpenAPI = class sealed(TWiRLConfiguration, IWiRLConfigurationOpenAPI)
  private
    FClass: TClass;
    FDocument: TOpenAPIDocument;
    FAPILogo: string;
    FFolderXMLDoc: string;
    FFolderGUIDoc: string;
  public
    class function Default: IWiRLConfigurationOpenAPI; static;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure DoAfterCreate; override;

    function SetOpenAPIResource(AClass: TClass): IWiRLConfigurationOpenAPI;
    function SetXMLDocFolder(const AFolder: string): IWiRLConfigurationOpenAPI;
    function SetGUIDocFolder(const AFolder: string): IWiRLConfigurationOpenAPI;
    function SetAPILogo(const ALogo: string): IWiRLConfigurationOpenAPI;

    function SetAPIDocument(ADocument: TOpenAPIDocument): IWiRLConfigurationOpenAPI;
  published
    property APILogo: string read FAPILogo write FAPILogo;
    property FolderXMLDoc: string read FFolderXMLDoc write FFolderXMLDoc;
    property FolderGUIDoc: string read FFolderGUIDoc write FFolderGUIDoc;
    property Document: TOpenAPIDocument read FDocument write FDocument;
  end;

implementation

uses
  WiRL.http.Filters,
  WiRL.Core.OpenAPI.Resource;

constructor TWiRLConfigurationOpenAPI.Create;
begin
  inherited;
  FDocument := TOpenAPIDocument.Create(TOpenAPIVersion.v303);
end;

class function TWiRLConfigurationOpenAPI.Default: IWiRLConfigurationOpenAPI;
begin
  Result := TWiRLConfigurationOpenAPI.Create
    .SetXMLDocFolder('.')
  ;
end;

destructor TWiRLConfigurationOpenAPI.Destroy;
begin
  FDocument.Free;
  inherited;
end;

procedure TWiRLConfigurationOpenAPI.DoAfterCreate;
begin
  if not Assigned(FClass) then
    FClass := TOpenAPIResourceDefault;

  TWiRLResourceRegistry.Instance.RegisterResource(FClass);
  FApplication.SetResources(FClass.QualifiedClassName);
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

function TWiRLConfigurationOpenAPI.SetAPILogo(const ALogo: string): IWiRLConfigurationOpenAPI;
begin
  FAPILogo := ALogo;
  Result := Self;
end;

function TWiRLConfigurationOpenAPI.SetGUIDocFolder(const AFolder: string): IWiRLConfigurationOpenAPI;
begin
  FFolderGUIDoc := AFolder;
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