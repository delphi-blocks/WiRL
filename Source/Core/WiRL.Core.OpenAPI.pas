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
  OpenAPI.Model.Schema,
  OpenAPI.Neon.Serializers,

  Neon.Core.Types,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,
  Neon.Core.Persistence.JSON.Schema,
  Neon.Core.Serializers.RTL,

  WiRL.Core.Declarations,
  WiRL.Core.Exceptions,
  WiRL.Core.Metadata,
  WiRL.Core.Metadata.XMLDoc,
  WiRL.Configuration.Auth,
  WiRL.Configuration.Neon,
  WiRL.Configuration.OpenAPI,
  WiRL.Core.Application,
  WiRL.http.Accept.MediaType,
  WiRL.http.Filters;

type
  TOpenAPIv3Engine = class
  private
    FDocument: TOpenAPIDocument;

    FApplication: TWiRLApplication;
    FConfigurationOpenAPI: TWiRLConfigurationOpenAPI;
    FConfigurationNeon: TWiRLConfigurationNeon;
    //FConfigurationAuth: TWiRLConfigurationAuth;
    FSwaggerResource: string;

    function GetNeonConfig: INeonConfiguration;

    function TypeToSchemaJSON(AType: TRttiType): TJSONObject;
    function ClassToSchemaJSON(AClass: TClass): TJSONObject;

    function AddOperation(AMethod: TWiRLProxyMethod; AOpenAPIPath: TOpenAPIPathItem; const ATagName: string): TOpenAPIOperation;
    procedure AddResource(AResource: TWiRLProxyResource);
    function CreateParameter(AParameter: TWiRLProxyParameter): TOpenAPIParameter;
  protected
    constructor Create(AApplication: TWiRLApplication; const ASwaggerResource: string); overload;
    procedure ProcessXMLDoc;
    function Build(): TJSONObject;
  public
    class function Generate(AApplication: TWiRLApplication; const ASwaggerResource: string): TJSONObject; overload;
  end;


implementation

uses
  System.StrUtils, System.TypInfo,

  WiRL.Rtti.Utils,
  WiRL.Core.Utils,
  WiRL.http.Server;

{ TOpenAPIv3Engine }

function TOpenAPIv3Engine.Build: TJSONObject;
var
  LRes: TWiRLProxyResource;
  LSchema: TOpenAPISchema;
  LPair: TPair<string, TWiRLProxyResource>;
begin
  ProcessXMLDoc();

  for LPair in FApplication.Proxy.Resources do
  begin
    LRes := LPair.Value;

    case LRes.Auth.AuthType of
      None: ;
      Unknown: ;
      Basic:
      begin
        FDocument.Components.AddSecurityHttp('basic_auth', 'Basic Authentication', 'basic', '');
        FDocument.Components.AddSecurityHttp('jwt_auth', 'JWT (Bearer) Authentication', 'bearer', 'JWT');
      end;
      Cookie:
      begin
        FDocument.Components.AddSecurityApiKey('cookie_auth', 'Cookie Based authentication', LRes.Auth.HeaderName, TAPIKeyLocation.Cookie);
        //FDocument.Components.AddSecurityHttp('jwt_auth', 'JWT (Bearer) Authentication', 'bearer', 'JWT');
      end;

    end;
  end;

  for LPair in FApplication.Proxy.Resources do
  begin
    LRes := LPair.Value;
    if LRes.IsSwagger(FSwaggerResource) then
      Continue;

    // Adds a tag to the tags array
    // Tags are a group (resource) of operations (methods)
    FDocument.AddTag(LRes.Name, LRes.Summary);

    LSchema := FDocument.Components.AddSchema('Error');
    LSchema.SetJSONObject(ClassToSchemaJSON(TWebExceptionSchema));

    AddResource(LRes);
  end;

  Result := TNeon.ObjectToJSON(FDocument, GetNeonConfig) as TJSONObject;
end;

function TOpenAPIv3Engine.ClassToSchemaJSON(AClass: TClass): TJSONObject;
begin
  Result := TypeToSchemaJSON(TRttiHelper.Context.GetType(AClass));
end;

constructor TOpenAPIv3Engine.Create(AApplication: TWiRLApplication; const ASwaggerResource: string);
begin
  FSwaggerResource := ASwaggerResource;
  FApplication := AApplication;

  FConfigurationNeon := FApplication.GetConfiguration<TWiRLConfigurationNeon>;
  FConfigurationOpenAPI := FApplication.GetConfiguration<TWiRLConfigurationOpenAPI>;

  FDocument := FConfigurationOpenAPI.Document;
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
  Result := AOpenAPIPath.AddOperation(TOperationType.FromString(AMethod.HttpVerb.ToLower));
  Result.Summary := 'Function ' + AMethod.Name;
  Result.Description := AMethod.Summary;

  // Add a reference tag
  Result.AddTag(ATagName);

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
        if Assigned(LParam.Entity) then
        begin
          if not FDocument.Components.SchemaExists(LParam.Entity.Name) then
          begin
            FDocument.Components
              .AddSchema(LParam.Entity.Name)
              .SetJSONObject(TypeToSchemaJSON(LParam.RttiParam.ParamType));
          end;
          LMediaType.Schema.SetSchemaReference(LParam.Entity.Name);
        end
        else
          LMediaType.Schema.SetJSONObject(TypeToSchemaJSON(LParam.RttiParam.ParamType));
      end;

      Continue;
    end;

    if LParam.Kind = TMethodParamType.MultiPart then
    begin
      { TODO -opaolo -c : finire 09/06/2021 16:06:03 }
      Continue;
    end;

    LParameter := CreateParameter(LParam);
    AOpenAPIPath.Parameters.Add(LParameter);
  end;

  if AMethod.IsFunction then
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

            if Assigned(AMethod.MethodResult.Entity) then
            begin
              if not FDocument.Components.SchemaExists(AMethod.MethodResult.RttiType.Name) then
              begin
                FDocument.Components
                  .AddSchema(AMethod.MethodResult.RttiType.Name)
                  .SetJSONObject(TypeToSchemaJSON(AMethod.MethodResult.RttiType));
              end;
              LMediaType.Schema.SetSchemaReference(AMethod.MethodResult.Entity.Name);
            end;
            LMediaType.Schema.SetJSONObject(TypeToSchemaJSON(AMethod.MethodResult.RttiType));
          end;
        end;
        Redirection: ;
        ClientError, ServerError:
        begin
          LMediaType := LResponse.AddMediaType('application/json');
          LMediaType.Schema.Reference.Ref := '#/components/schemas/' + 'Error';
        end;
        Custom: ;
      end;
    end;
  end;

  if Length(AMethod.Auth.Roles) > 0 then
    Result.Security.AddSecurityRequirement(
      FDocument.Components.SecuritySchemes, 'jwt_auth', []
    );

  if AMethod.AuthHandler then
    Result.Security.AddSecurityRequirement(
      FDocument.Components.SecuritySchemes, 'basic_auth', []
    );
end;

procedure TOpenAPIv3Engine.AddResource(AResource: TWiRLProxyResource);
var
  LFullPath: string;
  LMethod: TWiRLProxyMethod;
  LPathItem: TOpenAPIPathItem;
begin
  if AResource.Path <> '' then
  begin
    // Loop on every method of the current resource object
    for LMethod in AResource.Methods do
    begin
      if LMethod.Name <> '' then
      begin
        LFullPath := IncludeLeadingSlash(CombineURL(AResource.Path, LMethod.Path));

        // If the resource is already documented add the information on
        // the same json object
        LPathItem := FDocument.AddPath(LFullPath);

        AddOperation(LMethod, LPathItem, AResource.Name);
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
    LEngine.Free;
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

procedure TOpenAPIv3Engine.ProcessXMLDoc;
var
  LContext: TWiRLXMLDocContext;
begin
  LContext.Proxy := FApplication.Proxy;
  LContext.XMLDocFolder := TWiRLTemplatePaths.Render(FConfigurationOpenAPI.FolderXMLDoc);
  TWiRLProxyEngineXMLDoc.Process(LContext);
end;

function TOpenAPIv3Engine.TypeToSchemaJSON(AType: TRttiType): TJSONObject;
var
  LConf: INeonConfiguration;
begin
  LConf := GetNeonConfig;
  Result := TNeonSchemaGenerator.TypeToJSONSchema(AType, LConf);
end;

end.
