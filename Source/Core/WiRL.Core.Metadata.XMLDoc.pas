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
  WiRL.Core.Utils,
  WiRL.Core.Attributes,
  WiRL.Core.Exceptions,
  WiRL.Core.Declarations,
  WiRL.Core.Application,
  WiRL.Core.Registry,
  WiRL.Core.Metadata;

type
  TWiRLXMLDocContext = record
  private
    FProxy: TWiRLProxyApplication;
    FXMLDocFolder: string;
  public
    property Proxy: TWiRLProxyApplication read FProxy write FProxy;
    property XMLDocFolder: string read FXMLDocFolder write FXMLDocFolder;
  end;

  TWiRLOpenAPITags = class
  private
    FSummary: string;
  public
    property Summary: string read FSummary write FSummary;
  end;

  TWiRLProxyEngineXMLDoc = class
  private
    FPathEngine: TWiRLTemplatePaths;

    FContext: TWiRLXMLDocContext;

    function BuildXPath(const ATag, AAttrName, AAttrValue: string): string;
    function SelectText(ANode: IXMLNode): string; overload;
    function SelectText(ARoot: IXMLNode; const ANodePath: WideString): string; overload;

    function SelectNode(ARoot: IXMLNode; const ANodePath: WideString): IXMLNode;
    function SelectNodes(ARoot: IXMLNode; const ANodePath: WideString): IXMLNodeList;

  public // to be private
    function LoadXMLUnit(const AUnitName: string): IXMLDocument;
  public
    constructor Create(AContext: TWiRLXMLDocContext);
    destructor Destroy; override;
    procedure ProcessXMLDoc;
    procedure ProcessParams(AMethod: TWiRLProxyMethod; ANode: IXMLNode);
    procedure ProcessResponses(AMethod: TWiRLProxyMethod; ANode: IXMLNode);
    procedure ProcessMethods(AMethod: TWiRLProxyMethod; ANode: IXMLNode);
    procedure ProcessResource(AResource: TWiRLProxyResource);
  public
    class procedure Process(AContext: TWiRLXMLDocContext);
  end;


implementation

uses
  Xml.xmldom, Xml.omnixmldom,
  Xml.Internal.OmniXML,
  System.IOUtils;


{ TWiRLProxyEngineXMLDoc }

function TWiRLProxyEngineXMLDoc.SelectNode(ARoot: IXMLNode; const ANodePath: WideString): IXMLNode;
var
  LSelect: IDOMNodeSelect;
  LResult: IDOMNode;
  LDocAccess: IXMLDocumentAccess;
  LDoc: TXMLDocument;
begin
  Result := nil;

  if not Assigned(ARoot) or not Supports(ARoot.DOMNode, IDomNodeSelect, LSelect) then
    Exit;

  LResult := LSelect.SelectNode(ANodePath);
  if Assigned(LResult) then
  begin
    if Supports(ARoot.OwnerDocument, IXmlDocumentAccess, LDocAccess) then
      LDoc := LDocAccess.DocumentObject
    else
      LDoc := nil;
    Result := TXMLNode.Create(LResult, nil, LDoc);
  end;
end;

function TWiRLProxyEngineXMLDoc.SelectNodes(ARoot: IXMLNode; const ANodePath: WideString): IXMLNodeList;
var
  LSelect: IDOMNodeSelect;
  LDocAccess: IXMLDocumentAccess;
  LNodeAccess: IXMLNodeAccess;
  LResult: IDOMNodeList;
  LDoc: TXMLDocument;
  LNode: IDOMNode;
  LIndex: Integer;
begin
  Result := nil;
  if not Assigned(ARoot)
    or not Supports(ARoot, IXmlNodeAccess, LNodeAccess)
    or not Supports(ARoot.DOMNode, IDomNodeSelect, LSelect) then
    Exit;

  LResult := LSelect.selectNodes(ANodePath);
  if Assigned(LResult) then
  begin
    Result := TXMLNodeList.Create(LNodeAccess.GetNodeObject, '', nil);
    if Supports(ARoot.OwnerDocument, IXmlDocumentAccess, LDocAccess) then
      LDoc := LDocAccess.DocumentObject
    else
      LDoc := nil;

    for LIndex := 0 to LResult.length - 1 do
    begin
      LNode := LResult.item[LIndex];
      Result.Add(TXmlNode.Create(LNode, nil, LDoc));
    end;
  end;
end;

function TWiRLProxyEngineXMLDoc.SelectText(ANode: IXMLNode): string;
begin
  Result := '';
  if Assigned(ANode) {and (ANode.NodeType = ntText)} then
    Result := ANode.Text;
end;

function TWiRLProxyEngineXMLDoc.SelectText(ARoot: IXMLNode; const ANodePath: WideString): string;
var
  LNode: IXMLNode;
begin
  Result := '';
  LNode := SelectNode(ARoot, ANodePath);
  if Assigned(LNode) {and (LNode.NodeType = ntText)} then
    Result := LNode.Text;
end;

function TWiRLProxyEngineXMLDoc.BuildXPath(const ATag, AAttrName, AAttrValue: string): string;
begin
  Result := Format('%s[@%s=%s]', [ATag, AAttrName, QuotedStr(AAttrValue)]);
end;

constructor TWiRLProxyEngineXMLDoc.Create(AContext: TWiRLXMLDocContext);
begin
  FContext := AContext;
  FPathEngine := TWiRLTemplatePaths.Create();
  DefaultDOMVendor := OmniXML4Factory.Description;  // 'Omni XML'
end;

destructor TWiRLProxyEngineXMLDoc.Destroy;
begin
  FPathEngine.Free;
  inherited;
end;

function TWiRLProxyEngineXMLDoc.LoadXMLUnit(const AUnitName: string): IXMLDocument;
var
  LFileName: string;
begin
  Result := TXMLDocument.Create(nil);

  LFileName := TPath.Combine(FContext.XMLDocFolder, AUnitName + '.xml');
  Result.LoadFromFile(LFileName);
end;

procedure TWiRLProxyEngineXMLDoc.ProcessXMLDoc;
var
  //LResource: TWiRLProxyResource;
  LPair: TPair<string, TWiRLProxyResource>;
begin
  //1. Riempire l'oggetto TWiRLProxyApplication con i valori della classe scelta

  // Loop on every resource of the application
  for LPair in FContext.Proxy.Resources do
    ProcessResource(LPair.Value);
end;

class procedure TWiRLProxyEngineXMLDoc.Process(AContext: TWiRLXMLDocContext);
var
  LEngine: TWiRLProxyEngineXMLDoc;
begin
  LEngine := Self.Create(AContext);
  try
    LEngine.ProcessXMLDoc();
  finally
    LEngine.Free;
  end;
end;

procedure TWiRLProxyEngineXMLDoc.ProcessMethods(AMethod: TWiRLProxyMethod; ANode: IXMLNode);
var
  LDevNotes: IXMLNode;
  //LAttributes, LParameters: IXMLNode;
begin
  if not Assigned(ANode) then
    Exit;

  LDevNotes := SelectNode(ANode, 'devnotes');
  AMethod.Summary := SelectText(LDevNotes, 'summary');
  AMethod.Remarks := SelectText(LDevNotes, 'remarks');

  ProcessParams(AMethod, LDevNotes);
  ProcessResponses(AMethod, ANode);
end;

procedure TWiRLProxyEngineXMLDoc.ProcessParams(AMethod: TWiRLProxyMethod; ANode: IXMLNode);
var
  LXMLParameter: IXMLNode;
  LParam: TWiRLProxyParameter;
begin
  for LParam in AMethod.Params do
  begin
    LXMLParameter := SelectNode(ANode, BuildXPath('param', 'name', LParam.Name));
    if Assigned(LXMLParameter) then
    begin
      LParam.Summary := SelectText(LXMLParameter);
    end;
  end;

end;

procedure TWiRLProxyEngineXMLDoc.ProcessResponses(AMethod: TWiRLProxyMethod; ANode: IXMLNode);
var
  LIndex: Integer;
  LResponse: TWiRLProxyMethodResponse;
  LXMLResponse: IXMLNode;
  LResponses: IXMLNodeList;
begin
  LResponses := SelectNodes(ANode, '//response');
  // Responses
  for LIndex := 0 to LResponses.Count - 1 do
  begin
    LXMLResponse := LResponses[LIndex];
    LResponse := TWiRLProxyMethodResponse.Create;
    LResponse.Code := LXMLResponse.AttributeNodes['code'].Text.ToInteger;
    LResponse.Description := SelectText(LXMLResponse);
    AMethod.Responses.Add(LResponse);
  end;
end;

procedure TWiRLProxyEngineXMLDoc.ProcessResource(AResource: TWiRLProxyResource);
var
  LXPathClass: string;
  LDoc: IXMLDocument;
  LDevNotes, LAttributes, LMembers: IXMLNode;
  LMember: IXMLNode;
  LMethod: TWiRLProxyMethod;
begin
  //1.   Find the Unit containing the resource
  //2.   Open the file
  //3.   Search the class
  //3.1    Ex: <class name="TParametersResource" >
  //4.   Load the <attributes> tag
  //5.   Load the <devnotes> tag
  //5.1    Search the standard tags: <summary>, <param name="AParam">, <returns>, <remarks>
  //5.2    Search the wirl-api tags: <response code="200">, <url>, <example>
  //5.3    https://github.com/microsoft/OpenAPI.NET.CSharpAnnotations/wiki/C%23-Comment-Tag-Guide
  //6.   Load the <members> tag

  LDoc := LoadXMLUnit(AResource.ResourceClass.UnitName);
  LDoc.Active := True;
  LXPathClass := BuildXPath('/namespace/class', 'name', AResource.ResourceClass.ClassName);
  LDevNotes := SelectNode(LDoc.DocumentElement, LXPathClass + '/devnotes');
  if Assigned(LDevNotes) then
  begin
    AResource.Summary := SelectText(LDevNotes, 'summary');
    AResource.Remarks := SelectText(LDevNotes, 'remarks');
  end;
  LAttributes := SelectNode(LDoc.DocumentElement, LXPathClass + '/attributes');

  LMembers := SelectNode(LDoc.DocumentElement, LXPathClass + '/members');
  for LMethod in AResource.Methods do
  begin
    LMember := SelectNode(LMembers, BuildXPath('function', 'name', LMethod.Name));
    ProcessMethods(LMethod, LMember);
  end;

end;

end.
