{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2025 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.OpenAPI;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Rtti,
  System.Generics.Collections,

  OpenAPI.Model.Classes,
  OpenAPI.Model.Reference,
  OpenAPI.Model.Schema,
  OpenAPI.Neon.Serializers,

  Neon.Core.Types,
  Neon.Core.Tags,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,
  Neon.Core.Persistence.JSON.Schema,
  Neon.Core.Serializers.RTL,

  WiRL.Core.Declarations,
  WiRL.Core.Exceptions,
  WiRL.Core.Metadata,
  WiRL.Core.Metadata.XMLDoc,
  WiRL.Core.Metadata.OASDoc,
  WiRL.Configuration.Auth,
  WiRL.Configuration.Neon,
  WiRL.Configuration.Errors,
  WiRL.Configuration.OpenAPI,
  WiRL.Core.Application,
  WiRL.http.Accept.MediaType;

type
  TOpenAPIv3EngineConfig = record
    Application: TWiRLApplication;
    SwaggerResource: string;
  end;

  /// <summary>
  ///   OpenAPI v3 engine
  /// </summary>
  TOpenAPIv3Engine = class
  private type
    JSchemaProcessor = class
    private
      class function SearchForTags(AType: TRttiType): TAttributeTags;
    public
      class procedure ProcessEntity(AEntity: TWiRLProxyEntity);
      class procedure ProcessResource(AResource: TWiRLProxyResource);
    end;

  private
    FDocument: TOpenAPIDocument;

    FConfig: TOpenAPIv3EngineConfig;
    FNeonConfigApp: INeonConfiguration;
    FNeonConfigCustom: INeonConfiguration;
    FNeonConfigOpenAPI: INeonConfiguration;
    FConfigurationErrors: TWiRLConfigurationErrors;
    FConfigurationOpenAPI: TWiRLConfigurationOpenAPI;
    FErrorSchemaName: string;

    procedure AddComponentsSecuritySchemes;
    procedure AddComponentsResponses;

    procedure AddTags;
    procedure AddParams(AMethod: TWiRLProxyMethod; AOperation: TOpenAPIOperation);
    procedure AddOperation(AMethod: TWiRLProxyMethod; AOpenAPIPath: TOpenAPIPathItem; const ATagName: string);
    procedure AddOperations(AResource: TWiRLProxyResource);

    procedure AddMethodResponses(AMethod: TWiRLProxyMethod);
    procedure AddOperationResponses(AOperation: TOpenAPIOperation; AMethod: TWiRLProxyMethod);

    procedure FillResponse(ASource: TWiRLProxyMethodResponse; AMethod:
        TWiRLProxyMethod; AResponse: TOpenAPIResponse);

    function CreateParameter(AParameter: TWiRLProxyParameter): TOpenAPIParameter;

    procedure ClearDocument;
    function GetDocument: TOpenAPIDocument;

    procedure EntityToSchema(AEntity: TWiRLProxyEntity);
    function ClassToSchema(AClass: TClass): string;
    function TypeToSchema(AType: TRttiType): string;
  protected
    constructor Create(const AConfig: TOpenAPIv3EngineConfig); overload;
    procedure ProcessXMLDoc;
    procedure ProcessOASDoc;
    procedure BuildOpenAPIDocument;
    function Build(): TJSONObject;
  public
    property Document: TOpenAPIDocument read GetDocument;
  public
    class function Generate(const AConfig: TOpenAPIv3EngineConfig): TJSONObject;
  end;


implementation

uses
  System.StrUtils,
  System.TypInfo,
  WiRL.Rtti.Utils,
  WiRL.Core.Utils,
  WiRL.http.Server;

{ TOpenAPIv3Engine }

function TOpenAPIv3Engine.Build: TJSONObject;
begin
  BuildOpenAPIDocument();
  // Neon JSON conversion
  Result := TNeon.ObjectToJSON(FDocument, FNeonConfigOpenAPI) as TJSONObject;
end;

procedure TOpenAPIv3Engine.BuildOpenAPIDocument;
begin
  ClearDocument();

  if not FConfigurationOpenAPI.FolderXMLDoc.IsEmpty then
    ProcessXMLDoc();

  if not FConfigurationOpenAPI.FolderOASDoc.IsEmpty then
    ProcessOASDoc();

  // Adds the error entity to the components schemas
  FErrorSchemaName := ClassToSchema(FConfigurationErrors.ErrorClass);

  AddComponentsResponses;

  AddComponentsSecuritySchemes;

  AddTags;

  if Assigned(FConfigurationOpenAPI.Callback) then
    FConfigurationOpenAPI.Callback(Document);
end;

function TOpenAPIv3Engine.ClassToSchema(AClass: TClass): string;
begin
  Result := TypeToSchema(TRttiHelper.Context.GetType(AClass));
end;

procedure TOpenAPIv3Engine.ClearDocument;
begin
  FDocument.Paths.Clear;
  FDocument.Components.Schemas.Clear;
  FDocument.Components.Responses.Clear;
  FDocument.Components.Parameters.Clear;
  FDocument.Components.Examples.Clear;
  FDocument.Components.RequestBodies.Clear;
  FDocument.Components.Headers.Clear;
  FDocument.Components.SecuritySchemes.Clear;
  FDocument.Components.Links.Clear;
  FDocument.Components.Callbacks.Clear;
  FDocument.Security.Clear;
  FDocument.Tags.Clear;
end;

procedure TOpenAPIv3Engine.AddComponentsResponses;
begin
  FDocument.Components.AddResponse('BadRequest', 'Bad request')
    .AddMediaType(TMediaType.APPLICATION_JSON)
      .Schema.SetSchemaReference(FErrorSchemaName);

  FDocument.Components.AddResponse('NotFound', 'Resource not found')
    .AddMediaType(TMediaType.APPLICATION_JSON)
      .Schema.SetSchemaReference(FErrorSchemaName);

  FDocument.Components.AddResponse('Unauthorized', 'Unauthorized request')
    .AddMediaType(TMediaType.APPLICATION_JSON)
      .Schema.SetSchemaReference(FErrorSchemaName);
end;

procedure TOpenAPIv3Engine.AddOperation(AMethod: TWiRLProxyMethod;
    AOpenAPIPath: TOpenAPIPathItem; const ATagName: string);
var
  LOperation: TOpenAPIOperation;
begin
  LOperation := AOpenAPIPath.AddOperation(TOperationType.FromString(AMethod.HttpVerb.ToLower));
  LOperation.OperationId := AMethod.Name;
  if AMethod.Summary.IsEmpty then
    LOperation.Summary := 'Method ' + AMethod.Name
  else
    LOperation.Summary := AMethod.Summary;

  LOperation.Description := AMethod.Description;

  // Add a reference tag
  LOperation.AddTag(ATagName);

  AddParams(AMethod, LOperation);

  AddMethodResponses(AMethod);

  AddOperationResponses(LOperation, AMethod);

  if Length(AMethod.Auth.Roles) > 0 then
    LOperation.Security.AddSecurityRequirement(
      FDocument.Components.SecuritySchemes, 'jwt_auth', []
    );

  if AMethod.AuthHandler then
    LOperation.Security.AddSecurityRequirement(
      FDocument.Components.SecuritySchemes, 'basic_auth', []
    );
end;

procedure TOpenAPIv3Engine.AddOperationResponses(AOperation: TOpenAPIOperation; AMethod: TWiRLProxyMethod);
var
  LResponse: TWiRLProxyMethodResponse;
  LResponseAPI: TOpenAPIResponse;
begin
  for LResponse in AMethod.Responses do
  begin
    LResponseAPI := AOperation.AddResponse(LResponse.Code);
    FillResponse(LResponse, AMethod, LResponseAPI);
  end;
end;

procedure TOpenAPIv3Engine.AddMethodResponses(AMethod: TWiRLProxyMethod);
begin
  if (AMethod.Status.Code > 0) then
    AMethod.Responses.AddResponse(AMethod.Status.Code.ToString, AMethod.Status.Reason)
  else
  begin
    if AMethod.IsFunction then
      AMethod.Responses.AddResponse('200', 'Operation Success')
    else
      AMethod.Responses.AddResponse('204', 'No Content');
  end;

  AMethod.Responses.AddResponseSchemaRef('default', FErrorSchemaName, 'Generic API Server Error');
end;

procedure TOpenAPIv3Engine.AddOperations(AResource: TWiRLProxyResource);
var
  LFullPath: string;
  LMethod: TWiRLProxyMethod;
  LPathItem: TOpenAPIPathItem;
begin
  // Loop on every method of the current resource object
  for LMethod in AResource.Methods do
  begin
    if LMethod.Name <> '' then
    begin
      LFullPath := IncludeLeadingSlash(CombineURL(AResource.Path, LMethod.Path));

      // If the resource is already documented add the information
      // on the same json object
      LPathItem := FDocument.AddPath(LFullPath);

      AddOperation(LMethod, LPathItem, AResource.Name);
    end;
  end;
end;

procedure TOpenAPIv3Engine.AddParams(AMethod: TWiRLProxyMethod; AOperation: TOpenAPIOperation);
var
  LParam: TWiRLProxyParameter;
  LRequestBody: TOpenAPIRequestBody;
  LConsume: TMediaType;
  LMediaType: TOpenAPIMediaType;
  LParameter: TOpenAPIParameter;
begin
  // Describes input params
  for LParam in AMethod.Params do
  begin
    // if it's BodyParam or FormParam then add as requestBody
    if LParam.Kind = TMethodParamType.Body then
    begin
      LRequestBody := AOperation.SetRequestBody(LParam.Summary);

      // Add Request's MediaTypes (if param is BodyParam)
      for LConsume in AMethod.Consumes do
      begin
        LMediaType := LRequestBody.AddMediaType(LConsume.Value);
        if Assigned(LParam.Entity) then
        begin
          EntityToSchema(LParam.Entity);
          LMediaType
            .Schema
            .SetSchemaReference(LParam.Entity.Name);
        end
        else
          LMediaType
            .Schema
            .WithNeonConfig(FNeonConfigCustom)
            .SetJSONFromType(LParam.RttiParam.ParamType);
      end;

      Continue;
    end;

    if LParam.Kind = TMethodParamType.MultiPart then
    begin
      { TODO -opaolo -c : how to describe it? 09/06/2021 16:06:03 }
      Continue;
    end;

    LParameter := CreateParameter(LParam);
    AOperation.Parameters.Add(LParameter);
  end;
end;

procedure TOpenAPIv3Engine.AddTags;
var
  LRes: TWiRLProxyResource;
  LPair: TPair<string, TWiRLProxyResource>;
begin
  for LPair in FConfig.Application.Proxy.Resources do
  begin
    LRes := LPair.Value;
    if LRes.IsSwagger(FConfig.SwaggerResource) then
      Continue;

    // Adds a tag to the tags array
    // Tags are a group (resource) of operations (methods)
    FDocument.AddTag(LRes.Name, LRes.Summary);

    if not LRes.Path.IsEmpty then
      AddOperations(LRes);
  end;
end;

procedure TOpenAPIv3Engine.AddComponentsSecuritySchemes;
var
  LRes: TWiRLProxyResource;
  LPair: TPair<string, TWiRLProxyResource>;
begin
  for LPair in FConfig.Application.Proxy.Resources do
  begin
    LRes := LPair.Value;

    case LRes.Auth.AuthType of
      TWiRLProxyAuthType.None: ;
      TWiRLProxyAuthType.Unknown: ;
      TWiRLProxyAuthType.Basic:
      begin
        FDocument.Components.AddSecurityHttp('basic_auth', 'Basic Authentication', 'basic', '');
        FDocument.Components.AddSecurityHttp('jwt_auth', 'JWT (Bearer) Authentication', 'bearer', 'JWT');
      end;
      TWiRLProxyAuthType.Cookie:
      begin
        FDocument.Components.AddSecurityApiKey('cookie_auth', 'Cookie Based authentication', LRes.Auth.HeaderName, TAPIKeyLocation.Cookie);
        //FDocument.Components.AddSecurityHttp('jwt_auth', 'JWT (Bearer) Authentication', 'bearer', 'JWT');
      end;

    end;
  end;
end;

constructor TOpenAPIv3Engine.Create(const AConfig: TOpenAPIv3EngineConfig);
begin
  FConfig := AConfig;

  FConfigurationErrors := FConfig.Application.GetConfiguration<TWiRLConfigurationErrors>;
  FConfigurationOpenAPI := FConfig.Application.GetConfiguration<TWiRLConfigurationOpenAPI>;

  // We need 3 types of Neon serialization:
  // 1. We use the Neon config from the App configuration to serialize entities, errors, etc...
  // 2. We use a custom Neon config if passed (for entities, errors, etc...)
  // 3. We use the OpenAPI Neon configuration for OpenAPI structures

  FNeonConfigApp := FConfig.Application.GetConfiguration<TWiRLConfigurationNeon>.GetNeonConfig;
  FNeonConfigCustom := FConfigurationOpenAPI.NeonConfig;
  FNeonConfigOpenAPI := TOpenAPISerializer.GetNeonConfig;

  // If a custom NeonConfig hasn't been passed as config then use
  // NeonAppConfig to serialize Entities
  if not Assigned(FNeonConfigCustom) then
    FNeonConfigCustom := FNeonConfigApp;

  FDocument := FConfigurationOpenAPI.Document;
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
      Result.Required := AParameter.Required;

    Result.Description := AParameter.Summary;
    Result
      .Schema
      .WithNeonConfig(FNeonConfigCustom)
      .SetJSONFromType(AParameter.RttiParam.ParamType);
  end;
end;

procedure TOpenAPIv3Engine.FillResponse(ASource: TWiRLProxyMethodResponse;
    AMethod: TWiRLProxyMethod; AResponse: TOpenAPIResponse);
var
  LMediaType: TOpenAPIMediaType;
  LProduce: TMediaType;
begin
  case ASource.Category of

    //2xx responses.
    TStatusCategory.Success:
    begin
      AResponse.Description := ASource.Description;
      if ASource.HttpCode = 204 then
        Exit;

      for LProduce in AMethod.Produces do
      begin
        LMediaType := AResponse.AddMediaType(LProduce.Value);

        if Assigned(AMethod.MethodResult.Entity) then
        begin
          EntityToSchema(AMethod.MethodResult.Entity);
          LMediaType
            .Schema
            .SetSchemaReference(AMethod.MethodResult.Entity.Name);
        end
        else
          LMediaType
            .Schema
            .WithNeonConfig(FNeonConfigCustom)
            .SetJSONFromType(AMethod.MethodResult.RttiType);
      end;
    end;

    //3xx responses.
    TStatusCategory.Redirection: ;

    //4xx and 5xx responses.
    TStatusCategory.ClientError,
    TStatusCategory.ServerError,
    // nan responses: default, standard, etc...
    TStatusCategory.Custom:
    begin
      case ASource.ResponseType of
        TResponseType.Content:
        begin
          AResponse.Description := ASource.Description;
          LMediaType := AResponse.AddMediaType(TMediaType.APPLICATION_JSON);
          LMediaType.Schema.SetSchemaReference(FErrorSchemaName);
        end;

        TResponseType.Ref:
        begin
          AResponse.Reference.Id := ASource.Ref;
        end;

        TResponseType.RefSchema:
        begin
          AResponse.Description := ASource.Description;
          LMediaType := AResponse.AddMediaType(TMediaType.APPLICATION_JSON);
          LMediaType.Schema.SetSchemaReference(ASource.Schema);
        end;
      end;
    end;

  end;
end;

procedure TOpenAPIv3Engine.EntityToSchema(AEntity: TWiRLProxyEntity);
begin
  JSchemaProcessor.ProcessEntity(AEntity);

  if FDocument.Components.SchemaExists(AEntity.Name) then
    Exit;

  FDocument.Components
    .AddSchema(AEntity.Name)
    .WithNeonConfig(FNeonConfigCustom)
    .SetJSONFromType(AEntity.RttiType);
end;

class function TOpenAPIv3Engine.Generate(const AConfig: TOpenAPIv3EngineConfig): TJSONObject;
var
  LEngine: TOpenAPIv3Engine;
begin
  LEngine := TOpenAPIv3Engine.Create(AConfig);
  try
    Result := LEngine.Build();
  finally
    LEngine.Free;
  end;
end;

function TOpenAPIv3Engine.GetDocument: TOpenAPIDocument;
begin
  Result := FConfigurationOpenAPI.Document;
end;

function TOpenAPIv3Engine.TypeToSchema(AType: TRttiType): string;
var
  LAttr: JsonSchemaAttribute;
  LSummary: string;
begin
  Result := '';
  LAttr := TRttiHelper.FindAttribute<JsonSchemaAttribute>(AType);
  if Assigned(LAttr) then
  begin
    LAttr.ParseTags;

    if LAttr.Tags.Exists('title') then
      Result := LAttr.Tags.GetValueAs<string>('title');

    if LAttr.Tags.Exists('description') then
      LSummary := LAttr.Tags.GetValueAs<string>('description');
  end;

  if Result.IsEmpty then
    Result := AType.Name;

  if FDocument.Components.SchemaExists(Result) then
    Exit;

  FDocument.Components
    .AddSchema(Result)
    .WithNeonConfig(FNeonConfigCustom)
    .SetJSONFromType(AType);
end;

procedure TOpenAPIv3Engine.ProcessOASDoc;
var
  LContext: TWiRLJSONDocContext;
begin
  LContext.Proxy := FConfig.Application.Proxy;
  LContext.Document := FConfigurationOpenAPI.Document;
  LContext.JSONDocFolder := TWiRLTemplatePaths.Render(FConfigurationOpenAPI.FolderOASDoc);
  TWiRLProxyEngineJSON.Process(LContext);
end;

procedure TOpenAPIv3Engine.ProcessXMLDoc;
var
  LContext: TWiRLXMLDocContext;
begin
  LContext.Proxy := FConfig.Application.Proxy;
  LContext.XMLDocFolder := TWiRLTemplatePaths.Render(FConfigurationOpenAPI.FolderXMLDoc);
  TWiRLProxyEngineXMLDoc.Process(LContext);
end;

{ TOpenAPIv3Engine.JSchemaProcessor }

class procedure TOpenAPIv3Engine.JSchemaProcessor.ProcessEntity(AEntity: TWiRLProxyEntity);
var
  LTags: TAttributeTags;
begin
  LTags := SearchForTags(AEntity.RttiType);
  if not Assigned(LTags) then
    Exit;

  if LTags.Exists('title') then
    AEntity.Name := LTags.GetValueAs<string>('title');

  if LTags.Exists('description') then
    AEntity.Summary := LTags.GetValueAs<string>('description');
end;

class procedure TOpenAPIv3Engine.JSchemaProcessor.ProcessResource(AResource: TWiRLProxyResource);
var
  LTags: TAttributeTags;
begin
  LTags := SearchForTags(AResource.RttiType);
  if not Assigned(LTags) then
    Exit;

  if LTags.Exists('title') then
    AResource.Name := LTags.GetValueAs<string>('title');

  if LTags.Exists('description') then
    AResource.Summary := LTags.GetValueAs<string>('description');
end;

class function TOpenAPIv3Engine.JSchemaProcessor.SearchForTags(AType: TRttiType): TAttributeTags;
var
  LAttr: JsonSchemaAttribute;
begin
  LAttr := TRttiHelper.FindAttribute<JsonSchemaAttribute>(AType);
  if not Assigned(LAttr) then
    Exit(nil);

  LAttr.ParseTags;
  Result := LAttr.Tags;
end;

end.
