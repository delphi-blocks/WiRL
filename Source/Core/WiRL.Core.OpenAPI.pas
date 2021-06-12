{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2021 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.OpenAPI;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections,
  System.Rtti, System.RegularExpressions,

  OpenAPI.Model.Classes,
  OpenAPI.Neon.Serializers,
  Neon.Core.Types,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,
  Neon.Core.Persistence.JSON.Schema,
  Neon.Core.Serializers.RTL,
  WiRL.Core.Metadata,
  WiRL.Core.Metadata.XMLDoc,
  WiRL.Configuration.Auth,
  WiRL.Configuration.Neon,
  WiRL.Configuration.OpenAPI,
  WiRL.Core.JSON,
  WiRL.Core.Application,
  WiRL.Core.Engine,
  WiRL.Core.Context.Server,
  WiRL.Core.Registry,
  WiRL.Core.Attributes,
  WiRL.Core.Auth.Resource,
  WiRL.http.Accept.MediaType,
  WiRL.Rtti.Utils,
  WiRL.http.Filters;

type
  TTest = class
  private
    FAge: Integer;
    FName: string;
  published
    property Age: Integer read FAge write FAge;
    property Name: string read FName write FName;
  end;

  TOpenAPIInfo = class
  private
    FTitle: string;
    FVersion: string;
    FDescription: string;
    FSchemes: TArray<string>;
    FOpenAPIResource: string;
    FApplication: TWiRLApplication;
    FHost: string;
  public
    constructor Create(AApplication: TWiRLApplication; const ASwaggerResource: string);

    procedure AddScheme(const AScheme: string);

    property Host: string read FHost write FHost;
    property Title: string read FTitle write FTitle;
    property Version: string read FVersion write FVersion;
    property Description: string read FDescription write FDescription;
    property Schemes: TArray<string> read FSchemes write FSchemes;
    property Application: TWiRLApplication read FApplication write FApplication;
    property OpenAPIResource: string read FOpenAPIResource write FOpenAPIResource;
  end;

  TOpenAPIv2Engine = class
  private
    const OPENAPI_VERSION = '2.0';
  private
    FInfo: TOpenAPIInfo;
    FApplication: TWiRLApplication;
    FConfigurationOpenAPI: TWiRLConfigurationOpenAPI;
    FConfigurationNeon: TWiRLConfigurationNeon;
    FConfigurationAuth: TWiRLConfigurationAuth;
    FSwaggerResource: string;

    function GetNeonConfig: INeonConfiguration;

    function NodeExists(AObject: TJSONObject; const ANodeName: string): Boolean;
    function FindNode(AObject: TJSONObject; const ANodeName: string): TJSONObject;
    function FindOrCreateNode(AObject: TJSONObject; const ANodeName: string): TJSONObject;

    procedure AddErrorDefinition(AJson: TJSONObject); overload;
    procedure AddDefinition(AJson: TJSONObject; const AName: string; AType: TRttiType); overload;
    procedure AddDefinition(AJson: TJSONObject; const AName: string; ASchema: TJSONObject); overload;

    procedure AddSecurityDefinition(AJson: TJSONObject; AResource: TWiRLProxyResource);
    procedure AddSecurity(AJson: TJSONObject; const AName: string);

    function AddOperationResponse(AMethod: TWiRLProxyMethod; AJsonPath: TJSONObject; const ATagName: string): TJSONObject;
    function AddOperation(AMethod: TWiRLProxyMethod; AJsonPath: TJSONObject; const ATagName: string): TJSONObject;

    procedure AddResource(AResource: TWiRLProxyResource; APaths: TJSONObject);
    function CreateParameter(AParameter: TWiRLProxyParameter): TJSONObject;
  protected
    constructor Create(AApplication: TWiRLApplication; const ASwaggerResource: string); overload;
    constructor Create(AInfo: TOpenAPIInfo); overload;

    function NewXMLDocEngine: TWiRLProxyEngineXMLDoc;
    function Build(): TJSONObject;
  public
    destructor Destroy; override;
    class function Generate(AApplication: TWiRLApplication; const ASwaggerResource: string): TJSONObject; overload;
    class function Generate(AInfo: TOpenAPIInfo): TJSONObject; overload;
  end;


  TOpenAPIv3Engine = class
  private
    FDocument: TOpenAPIDocument;

    FApplication: TWiRLApplication;
    FConfigurationOpenAPI: TWiRLConfigurationOpenAPI;
    FConfigurationNeon: TWiRLConfigurationNeon;
    FConfigurationAuth: TWiRLConfigurationAuth;
    FSwaggerResource: string;

    function GetNeonConfig: INeonConfiguration;

    function TypeToSchemaJSON(AType: TRttiType): TJSONObject;
    function ClassToSchemaJSON(AClass: TClass): TJSONObject;

    function AddOperation(AMethod: TWiRLProxyMethod; AOpenAPIPath: TOpenAPIPathItem; const ATagName: string): TOpenAPIOperation;
    procedure AddResource(AResource: TWiRLProxyResource);
    function CreateParameter(AParameter: TWiRLProxyParameter): TOpenAPIParameter;
  protected
    constructor Create(AApplication: TWiRLApplication; const ASwaggerResource: string); overload;
    function NewXMLDocEngine: TWiRLProxyEngineXMLDoc;
    function Build(): TJSONObject;
  public
    destructor Destroy; override;
    class function Generate(AApplication: TWiRLApplication; const ASwaggerResource: string): TJSONObject; overload;
  end;


implementation

uses
  System.StrUtils, System.TypInfo,

  WiRL.Core.Exceptions,
  WiRL.Core.Utils,
  WiRL.http.Server;

procedure TOpenAPIv2Engine.AddDefinition(AJson: TJSONObject; const AName: string; AType: TRttiType);
begin
  AJson.AddPair(AName, TNeonSchemaGenerator.TypeToJSONSchema(AType, GetNeonConfig));
end;

procedure TOpenAPIv2Engine.AddDefinition(AJson: TJSONObject; const AName: string; ASchema: TJSONObject);
begin
  AJson.AddPair(AName, ASchema);
end;

procedure TOpenAPIv2Engine.AddErrorDefinition(AJson: TJSONObject);
var
  LType: TRttiType;
begin
  LType := TRttiHelper.Context.GetType(TWebExceptionSchema);
  if Assigned(LType) then
    AddDefinition(AJson, 'Error', LType);
end;

function TOpenAPIv2Engine.AddOperation(AMethod: TWiRLProxyMethod;
    AJsonPath: TJSONObject; const ATagName: string): TJSONObject;
var
  LOperation: TJSONObject;
  LResponses: TJSONObject;
  LParameters: TJSONArray;
  LParameter: TJSONObject;
  LProduces: TJSONArray;
  LConsumes: TJSONArray;
  LOkResponse: TJSONObject;
  LResponse: TWiRLProxyMethodResponse;
  LParam: TWiRLProxyParameter;
  LProduce, LConsume: TMediaType;
begin
  // Operation = Path + HttpMethod
  // If more object's method use the same operation add info on the
  // same operation
  LOperation := FindOrCreateNode(AJsonPath, AMethod.HttpVerb.ToLower);

  LOperation.AddPair('summary', 'Function ' + AMethod.Name);
  if not AMethod.Summary.IsEmpty then
    LOperation.AddPair('description', AMethod.Summary);

  {
  if not Assigned(LOperation.GetValue('summary')) then
  begin
    if not AMethod.Summary.IsEmpty then
      LOperation.AddPair('summary', AMethod.Summary)
    else
      LOperation.AddPair('summary', AMethod.Name);
  end;
  }

  if not Assigned(LOperation.GetValue('tags')) then
    if not ATagName.IsEmpty then
      LOperation.AddPair('tags', TJSONArray.Create.Add(ATagName));

  LProduces := LOperation.GetValue('produces') as TJSONArray;

  for LProduce in AMethod.Produces do
  begin
    if not Assigned(LProduces) then
    begin
      LProduces := TJSONArray.Create;
      LOperation.AddPair('produces', LProduces);
    end;

    // Check if the produce already exists
    if not ExistsInArray(LProduces, LProduce.Value) then
      LProduces.Add(LProduce.Value);
  end;

  LConsumes := LOperation.GetValue('consumes') as TJSONArray;
  for LConsume in AMethod.Consumes do
  begin
    if not Assigned(LConsumes) then
    begin
      LConsumes := TJSONArray.Create;
      LOperation.AddPair('consumes', LConsumes);
    end;

    // Check if the consume already exists
    if not ExistsInArray(LConsumes, LConsume.Value) then
      LConsumes.Add(LConsume.Value);
  end;

  if not Assigned(LOperation.GetValue('parameters')) then
  begin
    LParameters := nil;
    for LParam in AMethod.Params do
    begin
      if not Assigned(LParameters) then
      begin
        LParameters := TJSONArray.Create;
        LOperation.AddPair('parameters', LParameters)
      end;

      LParameter := CreateParameter(LParam);

      if Assigned(LParameter) then
        LParameters.Add(LParameter);
    end;
  end;

  if not Assigned(LOperation.GetValue('responses')) then
  begin

    LResponses := TJSONObject.Create;
    LOperation.AddPair('responses', LResponses);

    if AMethod.Responses.Count > 0 then
    begin
      for LResponse in AMethod.Responses do
      begin
        LOkResponse := TJSONObject.Create(TJSONPair.Create('description', LResponse.Description));
        LResponses.AddPair(LResponse.Code.ToString, LOkResponse);

        LResponses.AddPair('default', TJSONObject.Create(TJSONPair.Create('description', 'Error')));
        if AMethod.MethodResult.IsFunction then
          LOkResponse.AddPair('schema', TNeonSchemaGenerator.TypeToJSONSchema(AMethod.MethodResult.RttiType, GetNeonConfig));
      end;
    end
    else
    begin
      LOkResponse := TJSONObject.Create(TJSONPair.Create('description', 'Ok'));
      LResponses.AddPair('200', LOkResponse);
      LResponses.AddPair('default', TJSONObject.Create(TJSONPair.Create('description', 'Error')));
      if AMethod.MethodResult.IsFunction then
        LOkResponse.AddPair('schema', TNeonSchemaGenerator.TypeToJSONSchema(AMethod.MethodResult.RttiType, GetNeonConfig));
    end;
  end;


  if not Assigned(LOperation.GetValue('responses')) then
  begin
    LResponses := TJSONObject.Create;
    LOperation.AddPair('responses', LResponses);
    LOkResponse := TJSONObject.Create(TJSONPair.Create('description', 'Ok'));
    LResponses.AddPair('200', LOkResponse);
    LResponses.AddPair('default', TJSONObject.Create(TJSONPair.Create('description', 'Error')));
    if AMethod.MethodResult.IsFunction then
      LOkResponse.AddPair('schema', TNeonSchemaGenerator.TypeToJSONSchema(AMethod.MethodResult.RttiType, GetNeonConfig));
  end;

  Result := LOperation;
end;

function TOpenAPIv2Engine.AddOperationResponse(AMethod: TWiRLProxyMethod;
  AJsonPath: TJSONObject; const ATagName: string): TJSONObject;
begin
{
  if not Assigned(LOperation.GetValue('responses')) then
  begin

    LResponses := TJSONObject.Create;
    LOperation.AddPair('responses', LResponses);

    if AMethod.Responses.Count > 0 then
    begin
      for LResponse in AMethod.Responses do
      begin


      end;
    end
    else
    begin
      LOkResponse := TJSONObject.Create(TJSONPair.Create('description', 'Ok'));
      LResponses.AddPair('200', LOkResponse);
      LResponses.AddPair('default', TJSONObject.Create(TJSONPair.Create('description', 'Error')));
      if AMethod.MethodResult.IsFunction then
        LOkResponse.AddPair('schema', TNeonSchemaGenerator.TypeToJSONSchema(AMethod.MethodResult.RttiType, GetNeonConfig));
    end;
  end;
}
end;

procedure TOpenAPIv2Engine.AddResource(AResource: TWiRLProxyResource; APaths: TJSONObject);
var
  LMethodPath: string;
  LMethod: TWiRLProxyMethod;
  LJsonPath: TJSONObject;
  LOperation: TJSONObject;
begin

  if AResource.Path <> '' then
  begin
    // Loop on every method of the current resource object
    for LMethod in AResource.Methods do
    begin
      if LMethod.Name <> '' then
      begin
        LMethodPath := IncludeLeadingSlash(FApplication.EnginePath) +
          IncludeLeadingSlash(FApplication.BasePath) + IncludeLeadingSlash(AResource.Path);
        if (not LMethodPath.EndsWith('/')) and (not LMethod.Path.StartsWith('/')) then
          LMethodPath := LMethodPath + '/';
        LMethodPath := LMethodPath + LMethod.Path;
        // If the resource is already documented add the information on
        // the same json object
        LJsonPath := FindOrCreateNode(APaths, LMethodPath);

        LOperation := AddOperation(LMethod, LJsonPath, AResource.Name);

        if Length(LMethod.Auth.Roles) > 0 then
          AddSecurity(LOperation, 'bearerAuth');

        if LMethod.AuthHandler then
          AddSecurity(LOperation, 'basicAuth');
      end;
    end;
  end;
end;

procedure TOpenAPIv2Engine.AddSecurity(AJson: TJSONObject; const AName: string);
var
  LSec: TJSONArray;
begin
  LSec := TJSONArray.Create;
  LSec.AddElement(TJSONObject.Create
    .AddPair(AName, TJSONArray.Create));

  AJson.AddPair('security', LSec);
end;

procedure TOpenAPIv2Engine.AddSecurityDefinition(AJson: TJSONObject; AResource: TWiRLProxyResource);
var
  LDef: TJSONObject;
begin
  LDef := TJSONObject.Create;
  LDef.AddPair('type', 'basic');
  LDef.AddPair('description', 'Basic Authentication (Login)');
  AJson.AddPair('basicAuth', LDef);

  LDef := TJSONObject.Create;
  LDef.AddPair('type', 'apiKey');
  LDef.AddPair('in', 'header');
  LDef.AddPair('name', 'Authorization');
  LDef.AddPair('description', 'Bearer Authentication (Methods)');
  AJson.AddPair('bearerAuth', LDef);
end;

function TOpenAPIv2Engine.Build(): TJSONObject;
var
  LInfo: TJSONObject;
  LTag: TJSONObject;
  LTags: TJSONArray;
  LPaths: TJSONObject;
  LSchemes: TJSONArray;
  LDefinitions, LSecurityDefinitions: TJSONObject;
  LRes: TWiRLProxyResource;
  LPair: TPair<string, TWiRLProxyResource>;

  LXMLDocEngine: TWiRLProxyEngineXMLDoc;
begin
  LXMLDocEngine := NewXMLDocEngine;
  LXMLDocEngine.Process();
  try
    { TODO -opaolo -c : manage the empty strings scenario 07/01/2021 16:29:30 }
    LInfo := TJSONObject.Create
      .AddPair('title', FConfigurationOpenAPI.Title)
      .AddPair('version', FConfigurationOpenAPI.Version)
      .AddPair('description', FConfigurationOpenAPI.Description);

    // Paths object
    LPaths := TJSONObject.Create;
    LTags := TJSONArray.Create;

    LSecurityDefinitions := TJSONObject.Create;

    for LPair in FApplication.Proxy.Resources do
    begin
      LRes := LPair.Value;
      if LRes.IsSwagger(FSwaggerResource) then
        Continue;

      // Adds a tag to the tags array
      // Tags are a group (resource) of operations (methods)
      LTag := TJSONObject.Create;
      LTag.AddPair('name', LRes.Name);
      if not LRes.Summary.IsEmpty then
        LTag.AddPair('description', LRes.Summary);
      LTags.Add(LTag);

      if LRes.Auth then
      begin
        if not NodeExists(LSecurityDefinitions, 'basicAuth') then
          AddSecurityDefinition(LSecurityDefinitions, LRes);
      end;

      AddResource(LRes, LPaths);
    end;

    LDefinitions := TJSONObject.Create;

    AddErrorDefinition(LDefinitions);

    LSchemes := TJSONArray.Create;
    LSchemes.Add('http');

    Result := TJSONObject.Create
      .AddPair('swagger', OPENAPI_VERSION)
      .AddPair('info', LInfo)
      .AddPair('host', FConfigurationOpenAPI.Host)
      .AddPair('schemes', LSchemes)
      .AddPair('securityDefinitions', LSecurityDefinitions)
      .AddPair('tags', LTags)
      .AddPair('paths', LPaths)
      .AddPair('definitions', LDefinitions)
  finally
    LXMLDocEngine.Free;
  end;
end;

constructor TOpenAPIv2Engine.Create(AApplication: TWiRLApplication; const
    ASwaggerResource: string);
begin
  FSwaggerResource := ASwaggerResource;

  FApplication := AApplication;
  FConfigurationNeon := FApplication.GetConfiguration<TWiRLConfigurationNeon>;
  FConfigurationOpenAPI := FApplication.GetConfiguration<TWiRLConfigurationOpenAPI>;
end;

constructor TOpenAPIv2Engine.Create(AInfo: TOpenAPIInfo);
begin
  FInfo := AInfo;

  FSwaggerResource := AInfo.OpenAPIResource;
  FApplication := AInfo.Application;
  FConfigurationNeon := FApplication.GetConfiguration<TWiRLConfigurationNeon>;
  FConfigurationOpenAPI := FApplication.GetConfiguration<TWiRLConfigurationOpenAPI>;
end;

function TOpenAPIv2Engine.CreateParameter(AParameter: TWiRLProxyParameter): TJSONObject;

  function GetParamLocation(AParameter: TWiRLProxyParameter): string;
  begin
    Result := '';
    case AParameter.Kind of
      TMethodParamType.Path:      Result := 'path';
      TMethodParamType.Query:     Result := 'query';
      TMethodParamType.Form:      Result := 'formData';
      TMethodParamType.Header:    Result := 'header';
      //TMethodParamType.Cookie:    Result := 'path';
      TMethodParamType.Body:      Result := 'body';
      TMethodParamType.FormData:  Result := 'formData';
      //TMethodParamType.MultiPart: ;
    end;
  end;

var
  LParamType: string;
begin
  LParamType := GetParamLocation(AParameter);
  if LParamType.IsEmpty then
    Result := nil
  else
  begin
    if LParamType <> 'body' then
      Result := TNeonSchemaGenerator.TypeToJSONSchema(AParameter.RttiParam.ParamType, GetNeonConfig)
    else
    begin
      Result := TJSONObject.Create
        .AddPair('schema', TNeonSchemaGenerator.TypeToJSONSchema(AParameter.RttiParam.ParamType, GetNeonConfig))
    end;

    Result.AddPair(TJSONPair.Create('name', AParameter.Name));
    Result.AddPair(TJSONPair.Create('in', LParamType));

    if LParamType = 'path' then
      Result.AddPair(TJSONPair.Create('required', TJSONTrue.Create))
    else
      Result.AddPair(TJSONPair.Create('required', TJSONFalse.Create));

    if not AParameter.Summary.IsEmpty then
      Result.AddPair(TJSONPair.Create('description', AParameter.Summary));

  end;
end;

destructor TOpenAPIv2Engine.Destroy;
begin
  FInfo.Free;
  inherited;
end;

function TOpenAPIv2Engine.FindNode(AObject: TJSONObject; const ANodeName: string): TJSONObject;
begin
  Result := AObject.GetValue(ANodeName) as TJSONObject;
end;

function TOpenAPIv2Engine.FindOrCreateNode(AObject: TJSONObject; const ANodeName: string): TJSONObject;
begin
  Result := AObject.GetValue(ANodeName) as TJSONObject;
  if not Assigned(Result) then
  begin
    Result := TJSONObject.Create;
    AObject.AddPair(ANodeName, Result);
  end;
end;

class function TOpenAPIv2Engine.Generate(AApplication: TWiRLApplication; const
    ASwaggerResource: string): TJSONObject;
var
  LEngine: TOpenAPIv2Engine;
begin
  LEngine := TOpenAPIv2Engine.Create(AApplication, ASwaggerResource);
  try
    Result := LEngine.Build();
  finally
    LEngine.Free;
  end;
end;

class function TOpenAPIv2Engine.Generate(AInfo: TOpenAPIInfo): TJSONObject;
var
  LEngine: TOpenAPIv2Engine;
begin
  LEngine := TOpenAPIv2Engine.Create(AInfo);
  try
    Result := LEngine.Build();
  finally
    LEngine.Free;
  end;
end;

function TOpenAPIv2Engine.GetNeonConfig: INeonConfiguration;
begin
  Result := TNeonConfiguration.Camel;

  Result
   .SetIgnoreFieldPrefix(True)
   .SetUseUTCDate(True)
   .SetPrettyPrint(True)
   .GetSerializers
     .RegisterSerializer(TGUIDSerializer)
     .RegisterSerializer(TStreamSerializer)
  ;

  RegisterOpenAPISerializers(Result.GetSerializers);
end;

function TOpenAPIv2Engine.NodeExists(AObject: TJSONObject; const ANodeName: string): Boolean;
begin
  Result := Assigned(AObject.GetValue(ANodeName));
end;

function TOpenAPIv2Engine.NewXMLDocEngine: TWiRLProxyEngineXMLDoc;
var
  LContext: TWiRLXMLDocContext;
begin
  LContext.Proxy := FApplication.Proxy;
  //FConfigurationOpenAPI.FolderXMLDoc
  //FApplication.
  LContext.XMLDocFolder := 'D:\projects\GitHub\Project WiRL\WiRL\Demos\18.Swagger\Docs';
  Result := TWiRLProxyEngineXMLDoc.Create(LContext);
end;

{ TOpenAPIInfo }

procedure TOpenAPIInfo.AddScheme(const AScheme: string);
begin
  FSchemes := FSchemes + [AScheme];
end;

constructor TOpenAPIInfo.Create(AApplication: TWiRLApplication; const ASwaggerResource: string);
begin
  FApplication := AApplication;
  FOpenAPIResource := ASwaggerResource;
end;

{ TOpenAPIv3Engine }

function TOpenAPIv3Engine.Build: TJSONObject;
var
  LRes: TWiRLProxyResource;
  LPair: TPair<string, TWiRLProxyResource>;
  LXMLDocEngine: TWiRLProxyEngineXMLDoc;

  LPath: TOpenAPIPathItem;
  LOp: TOpenAPIOperation;
  LPar: TOpenAPIParameter;
  LResp: TOpenAPIResponse;
  LJ: TJSONObject;
begin
  {
  LPath := FDocument.AddPath('/path1');
  LOp := LPath.AddOperation(TOperationType.get);
  LResp := Lop.AddResponse(200);
  LJ := ClassToSchemaJSON(TTest);
  LResp.AddMediaType('application/json').Schema
    .SetJSONObject(LJ);

  LPar := LOp.AddParameter('param1', 'query');
  LPar.Name := 'pippo';
  LPar.In_ := 'query';
  LPar.Schema.Type_ := 'string';

  Result := TNeon.ObjectToJSON(FDocument, GetNeonConfig) as TJSONObject;
  Exit;
  }
  LXMLDocEngine := NewXMLDocEngine;
  LXMLDocEngine.Process();
  try

    FDocument.Info.Title := FConfigurationOpenAPI.Title;
    FDocument.Info.Description := FConfigurationOpenAPI.Description;

    for LPair in FApplication.Proxy.Resources do
    begin
      LRes := LPair.Value;
      if LRes.IsSwagger(FSwaggerResource) then
        Continue;

      // Adds a tag to the tags array
      // Tags are a group (resource) of operations (methods)
      FDocument.AddTag(LRes.Name, LRes.Summary);

      if LRes.Auth then
      begin
        FDocument.Components.AddSecurityHttp('basic_auth', 'Basic Authentication', 'basic', '');
        FDocument.Components.AddSecurityHttp('jwt_auth', 'JWT (Bearer) Authentication', 'bearer', 'JWT');
      end;

      AddResource(LRes);
    end;
    {

    LDefinitions := TJSONObject.Create;

    AddErrorDefinition(LDefinitions);

    LSchemes := TJSONArray.Create;
    LSchemes.Add('http');

    Result := TJSONObject.Create
      .AddPair('swagger', OPENAPI_VERSION)
      .AddPair('info', LInfo)
      .AddPair('host', FConfigurationOpenAPI.Host)
      .AddPair('schemes', LSchemes)
      .AddPair('securityDefinitions', LSecurityDefinitions)
      .AddPair('tags', LTags)
      .AddPair('paths', LPaths)
      .AddPair('definitions', LDefinitions)


    }

    Result := TNeon.ObjectToJSON(FDocument, GetNeonConfig) as TJSONObject;
  finally
    LXMLDocEngine.Free;
  end;
end;

function TOpenAPIv3Engine.ClassToSchemaJSON(AClass: TClass): TJSONObject;
begin
  Result := TypeToSchemaJSON(
    TRttiHelper.Context.GetType(AClass)
  );
end;

constructor TOpenAPIv3Engine.Create(AApplication: TWiRLApplication;
  const ASwaggerResource: string);
begin
  FSwaggerResource := ASwaggerResource;
  FApplication := AApplication;

  FConfigurationNeon := FApplication.GetConfiguration<TWiRLConfigurationNeon>;
  FConfigurationOpenAPI := FApplication.GetConfiguration<TWiRLConfigurationOpenAPI>;

  FDocument := TOpenAPIDocument.Create('3.0.3');
end;

destructor TOpenAPIv3Engine.Destroy;
begin
  FDocument.Free;
  inherited;
end;

function TOpenAPIv3Engine.AddOperation(AMethod: TWiRLProxyMethod;
    AOpenAPIPath: TOpenAPIPathItem; const ATagName: string): TOpenAPIOperation;
var
  LResponseStatus: TWiRLProxyMethodResponse;
  LParam: TWiRLProxyParameter;
  LProduce, LConsume: TMediaType;

  LResponse: TOpenAPIResponse;
  LMediaType: TOpenAPIMediaType;
  LParameter: TOpenAPIParameter;
  LRequestBody: TOpenAPIRequestBody;
begin
  // Operation = Path + HttpMethod
  // If more object's method use the same operation add info on the
  // same operation
  //LOperation := FindOrCreateNode(AJsonPath, AMethod.HttpVerb.ToLower);

  Result := AOpenAPIPath.AddOperation(TOperationType.FromString(AMethod.HttpVerb.ToLower));
  Result.Summary := 'Function ' + AMethod.Name;
  Result.Description := AMethod.Summary;

  // Add a reference tag
  Result.AddTag(ATagName);

  // Add Response's MediaTypes: 1 response n MediaType
  for LProduce in AMethod.Produces do
  begin
    // 200 is the default, parse for others returncodes
    LResponse := Result.AddResponse(200);
    //LResponse.Description := Description for all 200 responses (ex: Person Object)
    LMediaType := LResponse.AddMediaType(LProduce.Value);
    LMediaType.Schema.SetJSONObject(TypeToSchemaJSON(AMethod.MethodResult.RttiType));
  end;

  // Describes input params
  for LParam in AMethod.Params do
  begin
    // if it's BodyParam or FormParam then
    // Add as requestBody

    if LParam.Kind = TMethodParamType.Body then
    begin
      LRequestBody := Result.AddRequestBody(LParam.Summary);

      // Add Request's MediaTypes (if param is BodyParam)
      for LConsume in AMethod.Consumes do
      begin
        LMediaType := LRequestBody.AddMediaType(LConsume.Value);
        LMediaType.Schema.SetJSONObject(TypeToSchemaJSON(LParam.RttiParam.ParamType));
      end;

      Continue;
    end;

    if LParam.Kind = TMethodParamType.FormData then
    begin
      { TODO -opaolo -c : finire 09/06/2021 16:06:03 }
      Continue;
    end;

    //LParameter := AOpenAPIPath.AddParameter()
    LParameter := CreateParameter(LParam);
    AOpenAPIPath.Parameters.Add(LParameter);
  end;

  if AMethod.MethodResult.IsFunction then
  begin
    if not AMethod.Responses.Contains(TStatusCategory.Success) then
      AMethod.Responses.AddResponse(200, AMethod.Summary);

    if not AMethod.Responses.Contains(TStatusCategory.ClientError) or
       not AMethod.Responses.Contains(TStatusCategory.ServerError) then
      AMethod.Responses.AddResponse(500, 'Generic Server Error');

    // ResponseStatus Attribute
    for LResponseStatus in AMethod.Responses do
    begin
      LResponse := Result.AddResponse(LResponseStatus.Code);
      LResponse.Description := LResponseStatus.Description;
      case LResponseStatus.Category of
        Informational: ;
        Success:
        begin
          for LProduce in AMethod.Produces do
          begin
            LMediaType := LResponse.AddMediaType(LProduce.Value);
            LMediaType.Schema.SetJSONObject(TypeToSchemaJSON(LParam.RttiParam.ParamType));
          end;
        end;
        Redirection: ;
        ClientError: { TODO -opaolo -c : Error Schema 11/06/2021 12:04:05 };
        ServerError: { TODO -opaolo -c : Error Schema 11/06/2021 12:04:05 };
        Custom: ;
      end;
    end;
  end;
end;

procedure TOpenAPIv3Engine.AddResource(AResource: TWiRLProxyResource);
var
  LMethodPath: string;
  LMethod: TWiRLProxyMethod;
  LOperation: TOpenAPIOperation;
  LPathItem: TOpenAPIPathItem;
begin
  if AResource.Path <> '' then
  begin
    // Loop on every method of the current resource object
    for LMethod in AResource.Methods do
    begin
      if LMethod.Name <> '' then
      begin
        LMethodPath := IncludeLeadingSlash(FApplication.EnginePath) +
          IncludeLeadingSlash(FApplication.BasePath) + IncludeLeadingSlash(AResource.Path);
        if (not LMethodPath.EndsWith('/')) and (not LMethod.Path.StartsWith('/')) then
          LMethodPath :=  '/' + LMethodPath + '/';
        LMethodPath := LMethodPath + LMethod.Path;

        // If the resource is already documented add the information on
        // the same json object

        //LJsonPath := FindOrCreateNode(APaths, LMethodPath);
        LPathItem := FDocument.AddPath(LMethodPath);

        LOperation := AddOperation(LMethod, LPathItem, AResource.Name);

        {
        if Length(LMethod.Auth.Roles) > 0 then
        begin
          FDocument.AddSecurity('bearerAuth');
          AddSecurity(LOperation, 'bearerAuth');
        end;

        if LMethod.AuthHandler then
          AddSecurity(LOperation, 'basicAuth');
        }
      end;
    end;
  end;
end;

function TOpenAPIv3Engine.CreateParameter(AParameter: TWiRLProxyParameter): TOpenAPIParameter;

  function GetParamLocation(AParameter: TWiRLProxyParameter): string;
  begin
    Result := '';
    case AParameter.Kind of
      TMethodParamType.Path:      Result := 'path';
      TMethodParamType.Query:     Result := 'query';
      TMethodParamType.Header:    Result := 'header';
      TMethodParamType.Cookie:    Result := 'cookie';
    end;
  end;

var
  LParamType: string;
begin
  LParamType := GetParamLocation(AParameter);
  if LParamType.IsEmpty then
    Result := nil
  else
  begin
    Result := TOpenAPIParameter.Create;
    Result.Name := AParameter.Name;
    Result.In_ := LParamType;

    // Read the parameter's annotation (NotNull, Min, Max, etc...)
    if LParamType = 'path' then
      Result.Required := True
    else
      Result.Required := False;

    Result.Description := AParameter.Summary;
    Result.Schema.SetJSONObject(TypeToSchemaJSON(AParameter.RttiParam.ParamType));
  end;
end;

class function TOpenAPIv3Engine.Generate(AApplication: TWiRLApplication;
  const ASwaggerResource: string): TJSONObject;
var
  LEngine: TOpenAPIv3Engine;
begin
  LEngine := TOpenAPIv3Engine.Create(AApplication, ASwaggerResource);
  try
    Result := LEngine.Build();

  finally
    //LEngine.Free;
  end;
end;

function TOpenAPIv3Engine.GetNeonConfig: INeonConfiguration;
begin
  Result := TNeonConfiguration.Camel;

  Result
   .SetIgnoreFieldPrefix(True)
   .SetUseUTCDate(True)
   .SetPrettyPrint(True)
   .GetSerializers
     .RegisterSerializer(TGUIDSerializer)
     .RegisterSerializer(TStreamSerializer)
  ;

  RegisterOpenAPISerializers(Result.GetSerializers);
end;

function TOpenAPIv3Engine.NewXMLDocEngine: TWiRLProxyEngineXMLDoc;
var
  LContext: TWiRLXMLDocContext;
begin
  LContext.Proxy := FApplication.Proxy;
  //FConfigurationOpenAPI.FolderXMLDoc
  //FApplication.
  LContext.XMLDocFolder := 'D:\projects\GitHub\Project WiRL\WiRL\Demos\18.Swagger\Docs';
  Result := TWiRLProxyEngineXMLDoc.Create(LContext);
end;

function TOpenAPIv3Engine.TypeToSchemaJSON(AType: TRttiType): TJSONObject;
var
  LConf: INeonConfiguration;
begin
  LConf := GetNeonConfig;
  Result := TNeonSchemaGenerator.TypeToJSONSchema(AType, LConf);
end;

end.
