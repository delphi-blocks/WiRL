{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2025 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Metadata.OASDoc;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections,

  Neon.Core.Nullables,
  Neon.Core.Attributes,

  OpenAPI.Model.Classes,
  OpenAPI.Neon.Serializers,

  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,
  WiRL.Core.Application,
  WiRL.Core.Metadata;


type
  TOASBase = class
  protected
    FDescription: NullString;
  public
    function ToJSON: string;
    procedure FromJSON(const AJSON: string); overload;

    property Description: NullString read FDescription write FDescription;
  end;


  TOASHeader = class(TOASBase)
  private
    FDelphiType: NullString;
  public
    property DelphiType: NullString read FDelphiType write FDelphiType;
  end;

  TOASHeaders = class(TObjectDictionary<string, TOASHeader>)
  public
    constructor Create;
  end;

  TOASResponse = class(TOASBase)
  private
    FSchema: NullString;
    FRef: NullString;
    FHeaders: TOASHeaders;
  public
    function GetResponseType: TResponseType;

    property Headers: TOASHeaders read FHeaders write FHeaders;
    property Ref: NullString read FRef write FRef;
    property Schema: NullString read FSchema write FSchema;
  end;

  TOASResponses = class(TObjectDictionary<string, TOASResponse>)
  public
    constructor Create;
  end;

  TOASParam = class(TOASBase)
  private
    FRequired: NullBoolean;
  public
    property Required: NullBoolean read FRequired write FRequired;
  end;

  TOASParams = class(TObjectDictionary<string, TOASParam>)
  public
    constructor Create;
  end;

  TOASMethod = class(TOASBase)
  private
    FId: NullString;
    FSummary: NullString;
    FParams: TOASParams;
    FResponses: TOASResponses;
  public
    constructor Create;
    destructor Destroy; override;

    property Id: NullString read FId write FId;
    property Summary: NullString read FSummary write FSummary;
    property Params: TOASParams read FParams write FParams;
    property Responses: TOASResponses read FResponses write FResponses;
  end;

  TOASMethods = class(TObjectDictionary<string, TOASMethod>)
  public
    constructor Create;
  end;

  TOASResource = class(TOASBase)
  private
    FMethods: TOASMethods;
    FName: NullString;
  public
    constructor Create;
    destructor Destroy; override;

    property Name: NullString read FName write FName;
    property Methods: TOASMethods read FMethods write FMethods;
  end;


  TWiRLJSONDocContext = record
    Proxy: TWiRLProxyApplication;
    Document: TOpenAPIDocument;
    JSONDocFolder: string;
  end;

  TWiRLProxyEngineJSON = class
  private
    FContext: TWiRLJSONDocContext;
    //FPathEngine: TWiRLTemplatePaths;

    function LoadDocument(const AFileName: string): TJSONValue;
    function LoadResource(const AResource: string): TOASResource;
  public
    constructor Create(AContext: TWiRLJSONDocContext);

    procedure ProcessAPI;

    procedure ProcessInfo();
    procedure ProcessServers();
    procedure ProcessSecurity();

    procedure ProcessResource(AResource: TWiRLProxyResource);
    procedure ProcessMethod(AMethod: TWiRLProxyMethod; ADoc: TOASMethod);
    procedure ProcessParams(AMethod: TWiRLProxyMethod; ADoc: TOASMethod);
    procedure ProcessResponses(AMethod: TWiRLProxyMethod; ADoc: TOASMethod);
  public
    class procedure Process(AContext: TWiRLJSONDocContext);
  end;


implementation

uses
  System.IOUtils;

{ TOASMethod }

constructor TOASMethod.Create;
begin
  FParams := TOASParams.Create;
  FResponses := TOASResponses.Create;
end;

destructor TOASMethod.Destroy;
begin
  FParams.Free;
  FResponses.Free;
  inherited;
end;

{ TOASResource }

constructor TOASResource.Create;
begin
  FMethods := TOASMethods.Create;
end;

destructor TOASResource.Destroy;
begin
  FMethods.Free;
  inherited;
end;

{ TOASBase }

procedure TOASBase.FromJSON(const AJSON: string);
begin
  TNeon.JSONToObject(Self, AJSON, TNeonConfiguration.Camel);
end;

function TOASBase.ToJSON: string;
begin
  Result := TNeon.ObjectToJSONString(Self, TNeonConfiguration.Camel);
end;

{ TWiRLProxyEngineJSON }

constructor TWiRLProxyEngineJSON.Create(AContext: TWiRLJSONDocContext);
begin
  FContext := AContext;
end;

function TWiRLProxyEngineJSON.LoadDocument(const AFileName: string): TJSONValue;
var
  LFullName, LContent: string;
begin
  Result := nil;
  LFullName := TPath.Combine(FContext.JSONDocFolder, AFileName + '.json');
  if not TFile.Exists(LFullName) then
    Exit;

  LContent := TFile.ReadAllText(LFullName);
  Result := TJSONObject.ParseJSONValue(LContent);
end;

function TWiRLProxyEngineJSON.LoadResource(const AResource: string): TOASResource;
var
  LJSON: TJSONValue;
begin
  LJSON := LoadDocument(AResource);
  if not Assigned(LJSON) then
    Exit(nil);

  try
    Result := TNeon.JSONToObject<TOASResource>(LJSON, TNeonConfiguration.Camel);
  except
    Result := nil;
  end;

  LJSON.Free;
end;

class procedure TWiRLProxyEngineJSON.Process(AContext: TWiRLJSONDocContext);
var
  LEngine: TWiRLProxyEngineJSON;
begin
  LEngine := Self.Create(AContext);
  try
    LEngine.ProcessAPI;
  finally
    LEngine.Free;
  end;
end;

procedure TWiRLProxyEngineJSON.ProcessAPI;
var
  LPair: TPair<string, TWiRLProxyResource>;
begin
  // 1. Search for TOpenAPIInfo.json
  ProcessInfo();

  // 2. Search for TOpenAPIServers.json
  ProcessServers();

  // 3. Search for TOpenAPISecurityMap.json
  ProcessSecurity();

  // 4. Loop on every resource of the application
  for LPair in FContext.Proxy.Resources do
    ProcessResource(LPair.Value)
end;

procedure TWiRLProxyEngineJSON.ProcessMethod(AMethod: TWiRLProxyMethod; ADoc: TOASMethod);
begin
  if ADoc.Id.HasValue then
    AMethod.Code := ADoc.Id;

  if ADoc.Summary.HasValue then
    AMethod.Summary := ADoc.Summary;

  if ADoc.Description.HasValue then
    AMethod.Description := ADoc.Description;

  ProcessParams(AMethod, ADoc);
  ProcessResponses(AMethod, ADoc);
end;


procedure TWiRLProxyEngineJSON.ProcessParams(AMethod: TWiRLProxyMethod; ADoc: TOASMethod);
var
  LOASParam: TOASParam;
  LParam: TWiRLProxyParameter;
begin
  for LParam in AMethod.Params do
  begin
    if ADoc.Params.TryGetValue(LParam.Code, LOASParam) then
    begin
      if LOASParam.Description.HasValue then
        LParam.Description := LOASParam.Description;

      if LOASParam.Required.HasValue then
        LParam.Required := LOASParam.Required;
    end;
  end;
end;

procedure TWiRLProxyEngineJSON.ProcessResource(AResource: TWiRLProxyResource);
var
  LOASResource: TOASResource;
  LMethod: TWiRLProxyMethod;
  LOASMethod: TOASMethod;
begin
  LOASResource := LoadResource(AResource.Code);
  if not Assigned(LOASResource) then
    Exit;

  try
    if LOASResource.Name.HasValue then
      AResource.Name := LOASResource.Name;

    if LOASResource.Description.HasValue then
      AResource.Description := LOASResource.Description;

    for LMethod in AResource.Methods do
      if LOASResource.Methods.TryGetValue(LMethod.Code, LOASMethod) then
        ProcessMethod(LMethod, LOASMethod);

  finally
    LOASResource.Free;
  end;
end;

procedure TWiRLProxyEngineJSON.ProcessResponses(AMethod: TWiRLProxyMethod; ADoc: TOASMethod);
var
  LResponse: TOASResponse;
  LPair: TPair<string, TOASResponse>;
begin
  for LPair in ADoc.Responses do
  begin
    LResponse := LPair.Value;

    case LResponse.GetResponseType of
      TResponseType.Content:   AMethod.Responses.AddResponse(LPair.Key, LResponse.Description);
      TResponseType.Ref:       AMethod.Responses.AddResponseRef(LPair.Key, LResponse.Ref);
      TResponseType.RefSchema: AMethod.Responses.AddResponseSchemaRef(LPair.Key, LResponse.Schema, LResponse.Description);
    end;

  end;
end;

procedure TWiRLProxyEngineJSON.ProcessInfo;
var
  LJSON: TJSONValue;
begin
  if not Assigned(FContext.Document) then
    Exit;

  LJSON := LoadDocument(TOpenAPIInfo.ClassName);
  if not Assigned(LJSON) then
    Exit;

  try
    TNeon.JSONToObject(FContext.Document.Info, LJSON, TOpenAPISerializer.GetNeonConfig);
  finally
    LJSON.Free;
  end;
end;

procedure TWiRLProxyEngineJSON.ProcessSecurity;
var
  LJSON: TJSONValue;
begin
  if not Assigned(FContext.Document) then
    Exit;

  LJSON := LoadDocument(TOpenAPISecuritySchemeMap.ClassName);
  if not Assigned(LJSON) then
    Exit;

  try
    FContext.Document.Components.SecuritySchemes.Clear;
    TNeon.JSONToObject(FContext.Document.Components.SecuritySchemes, LJSON, TOpenAPISerializer.GetNeonConfig);
  finally
    LJSON.Free;
  end;
end;

procedure TWiRLProxyEngineJSON.ProcessServers;
var
  LJSON: TJSONValue;
begin
  if not Assigned(FContext.Document) then
    Exit;

  LJSON := LoadDocument(TOpenAPIServers.ClassName);
  if not Assigned(LJSON) then
    Exit;

  try
    FContext.Document.Servers.Clear;
    TNeon.JSONToObject(FContext.Document.Servers, LJSON, TOpenAPISerializer.GetNeonConfig);
  finally
    LJSON.Free;
  end;
end;

{ TOASResponse }

function TOASResponse.GetResponseType: TResponseType;
begin
  if FRef.HasValue then
    Exit(TResponseType.Ref);

  if FSchema.HasValue then
    Exit(TResponseType.RefSchema);

  Result := TResponseType.Content;
end;

{ TOASHeaders }

constructor TOASHeaders.Create;
begin
  inherited Create([doOwnsValues]);
end;

{ TOASResponses }

constructor TOASResponses.Create;
begin
  inherited Create([doOwnsValues]);
end;

{ TOASParams }

constructor TOASParams.Create;
begin
  inherited Create([doOwnsValues]);
end;

{ TOASMethods }

constructor TOASMethods.Create;
begin
  inherited Create([doOwnsValues]);
end;

end.
