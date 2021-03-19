{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2021 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Metadata.XMLDoc;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  System.Generics.Defaults, System.Rtti, System.TypInfo,
  Xml.XMLDoc, Xml.XMLIntf,

  WiRL.http.Core,
  WiRL.http.Accept.MediaType,
  WiRL.Core.Attributes,
  WiRL.Core.Exceptions,
  WiRL.Core.Declarations,
  WiRL.Core.Application,
  WiRL.Core.Registry,
  WiRL.Core.Metadata;

type
  TWiRLOpenAPITags = class
  private
    FSummary: string;
  public
    property Summary: string read FSummary write FSummary;
  end;

  TWiRLProxyEngineXMLDoc = class(TWiRLProxyEngine)
  private
    function LoadXMLUnit(const AUnitName: string): IXMLDocument;
  public
    constructor Create(AContext: TWiRLProxyContext); override;
    procedure ProcessApp;
    procedure ProcessResource(AResource: TWiRLProxyResource);
  end;

implementation

uses
  Xml.xmldom, Xml.omnixmldom,
  Xml.Internal.OmniXML,
  System.IOUtils;

{ TWiRLProxyEngineXMLDoc }

constructor TWiRLProxyEngineXMLDoc.Create(AContext: TWiRLProxyContext);
begin
  inherited;
  DefaultDOMVendor := OmniXML4Factory.Description;  // 'Omni XML'
end;

function TWiRLProxyEngineXMLDoc.LoadXMLUnit(const AUnitName: string): IXMLDocument;
var
  LFileName: string;
begin
  Result := TXMLDocument.Create(nil);
  try
    LFileName := TPath.Combine(FContext.XMLDocFolder, AUnitName + '.pas');
    Result.LoadFromFile(LFileName);
  except
    FreeAndNil(Result);
  end;
end;

procedure TWiRLProxyEngineXMLDoc.ProcessApp;
var
  LResource: TWiRLProxyResource;
  LPair: TPair<string, TWiRLProxyResource>;
begin

  //1. Riempire l'oggetto TWiRLProxyApplication con i valori della classe scelta


  // Loop on every resource of the application
  for LPair in FContext.Proxy.Resources do
  begin
    LResource := FContext.Proxy.NewResource(LPair.Key);
    ProcessResource(LResource);
  end;
end;

procedure TWiRLProxyEngineXMLDoc.ProcessResource(AResource: TWiRLProxyResource);
var
  LDoc: IXMLDocument;
  LNodeClass: IXMLNode;
begin
  //1.   Find the Unit containing the resource
  //2.   Open the file
  //3.   Search the class
  //3.1    Ex: <namespace name="Server.Resources.Demo"><class name="TParametersResource" >
  //4.   Load the <attributes> tag
  //5.   Load the <devnotes> tag
  //5.1    Search the standard tags: <summary>, <param name="AParam">, <returns>, <remarks>
  //5.2    Search the wirl-api tags: <response code="200">, <url>, <example>
  //5.3    https://github.com/microsoft/OpenAPI.NET.CSharpAnnotations/wiki/C%23-Comment-Tag-Guide
  //6.   Load the <members> tag

  LDoc := LoadXMLUnit(AResource.ClassType.UnitName);
  LDoc.Active := True;
  LNodeClass := LDoc.DocumentElement.ChildNodes[AResource.ClassType.ClassName];
  //AResource.Summary := LDoc.ChildNodes

end;

end.
