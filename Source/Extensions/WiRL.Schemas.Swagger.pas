{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Schemas.Swagger;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections,
  System.Rtti,

  Neon.Core.Persistence.Swagger,

  WiRL.Core.Application,
  WiRL.Core.Engine,
  WiRL.Core.Context,
  WiRL.Core.Registry,
  WiRL.Core.Attributes,
  WiRL.http.Accept.MediaType,
  WiRL.Rtti.Utils,
  WiRL.http.Filters;

type
  [PreMatching]
  TSwaggerFilter = class(TInterfacedObject, IWiRLContainerRequestFilter)
  private
    const SwaggerVersion = '2.0';
  private
    FWiRLContext: TWiRLContext;
    FApplication: TWiRLApplication;

    function GetPath(ARttiObject: TRttiObject): string;

    procedure AddApplicationResource(APaths: TJSONObject; ATags: TJSONArray; AApplication: TWiRLApplication);
    procedure AddResource(const AName: string; APaths: TJSONObject; AApplication: TWiRLApplication; LResource: TClass);
    procedure AddOperation(AJsonPath: TJSONObject; const AMethodName, ATagName: string; AResourceMethod: TRttiMethod);
    function CreateParameter(ARttiParameter: TRttiParameter): TJSONObject;

    function BuildSwagger: TJSONObject;
  public
    procedure Filter(ARequestContext: TWiRLContainerRequestContext);
  end;

  function ExistsInArray(AArray: TJSONArray; const AValue: string): Boolean;
  function IncludeLeadingSlash(const AValue: string): string;

implementation

uses
  System.StrUtils, System.TypInfo,
  WiRL.http.Server,
  WiRL.Core.JSON;

function ExistsInArray(AArray: TJSONArray; const AValue: string): Boolean;
var
  LValue: TJSONValue;
begin
  for LValue in AArray do
  begin
    if LValue.Value.Equals(AValue) then
      Exit(True);
  end;
  Result := False;
end;

function IncludeLeadingSlash(const AValue: string): string;
begin
  if not AValue.StartsWith('/') then
    Result := '/' + AValue
  else
    Result := AValue;
end;

{ TSwaggerFilter }

procedure TSwaggerFilter.AddApplicationResource(APaths: TJSONObject; ATags: TJSONArray; AApplication: TWiRLApplication);
var
  LResourceName: string;
  LResource: TClass;
begin
  FApplication := AApplication;
  // Loop on every resource of the application
  for LResourceName in AApplication.Resources.Keys do
  begin
    ATags.Add(TJSONObject.Create.AddPair('name', LResourceName));
    LResource := AApplication.GetResourceInfo(LResourceName).TypeTClass;
    AddResource(LResourceName, APaths, AApplication, LResource);
  end;
end;

procedure TSwaggerFilter.AddResource(const AName: string; APaths: TJSONObject;
  AApplication: TWiRLApplication; LResource: TClass);

  function FindOrCreatePath(APaths: TJSONObject; const AResourcePath: string): TJSONObject;
  begin
    Result := APaths.GetValue(AResourcePath) as TJSONObject;
    if not Assigned(Result) then
    begin
      Result := TJSONObject.Create;
      APaths.AddPair(AResourcePath, Result);
    end;
  end;

var
  LMethodPath: string;
  LResourcePath: string;
  LResourceType: TRttiType;
  LResourceMethod: TRttiMethod;
  LMethodName: string;
  LResourceMethodPath: string;
  LJsonPath: TJSONObject;
begin
  LResourceType := TRttiHelper.Context.GetType(LResource);
  LResourcePath := GetPath(LResourceType);
  if LResourcePath <> '' then
  begin
    // Loop on every method of the current resource object
    for LResourceMethod in LResourceType.GetMethods do
    begin
      LMethodName := '';
      TRttiHelper.HasAttribute<HttpMethodAttribute>(LResourceMethod,
        procedure (AAttr: HttpMethodAttribute)
        begin
          LMethodName := AAttr.ToString.ToLower;
        end
      );
      if LMethodName <> '' then
      begin
        LResourceMethodPath := GetPath(LResourceMethod);

        LMethodPath := IncludeLeadingSlash((AApplication.Engine as TWiRLEngine).BasePath) +
          IncludeLeadingSlash(AApplication.BasePath) + IncludeLeadingSlash(LResourcePath);
        if (not LMethodPath.EndsWith('/')) and (not LResourceMethodPath.StartsWith('/')) then
          LMethodPath := LMethodPath + '/';
        LMethodPath := LMethodPath + LResourceMethodPath;
        // If the resource is already documented add the information on
        // the same json object
        LJsonPath := FindOrCreatePath(APaths, LMethodPath);
        AddOperation(LJsonPath, LMethodName, AName, LResourceMethod);
      end;
    end;
  end;
end;

function TSwaggerFilter.BuildSwagger: TJSONObject;
var
  LInfo: TJSONObject;
  LTags: TJSONArray;
  LPaths: TJSONObject;
  LServer: TWiRLServer;
  LEngine: TWiRLCustomEngine;
  LAppInfo: TWiRLApplicationInfo;
begin
  LServer := FWiRLContext.Server as TWiRLServer;

  // Info object
  LInfo := TJSONObject.Create
    .AddPair('title', ChangeFileExt(ExtractFileName(ParamStr(0)), ''))
    .AddPair('version', '1.0');

  // Paths object
  LPaths := TJSONObject.Create;
  LTags := TJSONArray.Create;

  for LEngine in LServer.Engines do
  begin
    if LEngine is TWiRLEngine then
    begin
      for LAppInfo in TWiRLEngine(LEngine).Applications do
      begin
        AddApplicationResource(LPaths, LTags, LAppInfo.Application);
      end;
    end;
  end;

  Result := TJSONObject.Create
    .AddPair('swagger', SwaggerVersion)
    .AddPair('host', FWiRLContext.Request.Host)
    .AddPair('info', LInfo)
    .AddPair('tags', LTags)
    .AddPair('paths', LPaths)
end;

procedure TSwaggerFilter.AddOperation(AJsonPath: TJSONObject; const AMethodName, ATagName: string; AResourceMethod: TRttiMethod);

  function FindOrCreateOperation(APath: TJSONObject; const AMethodName: string) :TJSONObject;
  begin
    Result := APath.GetValue(AMethodName) as TJSONObject;
    if not Assigned(Result) then
    begin
      Result := TJSONObject.Create;
      AJsonPath.AddPair(AMethodName, Result);
    end;
  end;

var
  LOperation: TJSONObject;
  LResponses: TJSONObject;
  LParameters: TJSONArray;
  LParameter: TJSONObject;
  LProduces: TJSONArray;
  LConsumes: TJSONArray;
  LRttiParameter: TRttiParameter;
  LOkResponse: TJSONObject;
begin
  // Operation = Path + HttpMethod
  // If more object method use the same operation add info on the
  // same operation
  LOperation := FindOrCreateOperation(AJsonPath, AMethodName);

  if not Assigned(LOperation.GetValue('summary')) then
    LOperation.AddPair('summary', AResourceMethod.Name);

  if not ATagName.IsEmpty then
    LOperation.AddPair('tags', TJSONArray.Create.Add(ATagName));

  LProduces := LOperation.GetValue('produces') as TJSONArray;
  TRttiHelper.HasAttribute<ProducesAttribute>(AResourceMethod,
    procedure (AAttr: ProducesAttribute)
    begin
      if not Assigned(LProduces) then
      begin
        LProduces := TJSONArray.Create;
        LOperation.AddPair('produces', LProduces);
      end;

      // Check if the produce already exists
      if not ExistsInArray(LProduces, AAttr.Value) then
        LProduces.Add(AAttr.Value);
    end
  );

  LConsumes := LOperation.GetValue('consumes') as TJSONArray;
  TRttiHelper.HasAttribute<ConsumesAttribute>(AResourceMethod,
    procedure (AAttr: ConsumesAttribute)
    begin
      if not Assigned(LConsumes) then
      begin
        LConsumes := TJSONArray.Create;
        LOperation.AddPair('consumes', LConsumes);
      end;

      // Check if the consume already exists
      if not ExistsInArray(LConsumes, AAttr.Value) then
        LConsumes.Add(AAttr.Value);
    end
  );

  if not Assigned(LOperation.GetValue('parameters')) then
  begin
    LParameters := nil;
    for LRttiParameter in AResourceMethod.GetParameters do
    begin
      if not Assigned(LParameters) then
      begin
        LParameters := TJSONArray.Create;
        LOperation.AddPair('parameters', LParameters)
      end;
      LParameter := CreateParameter(LRttiParameter);
      if Assigned(LParameter) then
        LParameters.Add(LParameter);
    end;
  end;

  if not Assigned(LOperation.GetValue('responses')) then
  begin
    LResponses := TJSONObject.Create;
    LOperation.AddPair('responses', LResponses);
    LOkResponse := TJSONObject.Create(TJSONPair.Create('description', 'Ok'));
    LResponses.AddPair('200', LOkResponse);
    LResponses.AddPair('default', TJSONObject.Create(TJSONPair.Create('description', 'Error')));
    if Assigned(AResourceMethod.ReturnType) and (AResourceMethod.ReturnType.TypeKind <> tkUnknown) then
      LOkResponse.AddPair('schema', TNeonSchemaGenerator.TypeToJSONSchema(AResourceMethod.ReturnType, FApplication.SerializerConfig));
  end;
end;

function TSwaggerFilter.CreateParameter(ARttiParameter: TRttiParameter): TJSONObject;

  function GetParamPosition(ARttiParameter: TRttiParameter): string;
  begin
    if TRttiHelper.HasAttribute<PathParamAttribute>(ARttiParameter) then
      Result := 'path'
    else if TRttiHelper.HasAttribute<QueryParamAttribute>(ARttiParameter) then
      Result := 'query'
    else if TRttiHelper.HasAttribute<FormParamAttribute>(ARttiParameter) then
      Result := 'formData'
    else if TRttiHelper.HasAttribute<HeaderParamAttribute>(ARttiParameter) then
      Result := 'header'
    else if TRttiHelper.HasAttribute<BodyParamAttribute>(ARttiParameter) then
      Result := 'body'
    else
      Result := ''
  end;

  function GetParamName(ARttiParameter: TRttiParameter): string;
  var
    LParam: MethodParamAttribute;
  begin
    LParam := TRttiHelper.FindAttribute<MethodParamAttribute>(ARttiParameter);
    if Assigned(LParam) then
      Result := LParam.Value;

    if Result.IsEmpty then
      Result := ARttiParameter.Name;
  end;

var
  LParamType: string;
begin
  LParamType := GetParamPosition(ARttiParameter);
  if LParamType.IsEmpty then
    Result := nil
  else
  begin
    if LParamType <> 'body' then
      Result := TNeonSchemaGenerator.TypeToJSONSchema(ARttiParameter.ParamType, FApplication.SerializerConfig)
    else
    begin
      Result := TJSONObject.Create
        .AddPair('schema', TNeonSchemaGenerator.TypeToJSONSchema(ARttiParameter.ParamType, FApplication.SerializerConfig))
    end;

    Result.AddPair(TJSONPair.Create('name', GetParamName(ARttiParameter)));
    Result.AddPair(TJSONPair.Create('in', LParamType));

    if LParamType = 'path' then
      Result.AddPair(TJSONPair.Create('required', TJSONTrue.Create))
    else
      Result.AddPair(TJSONPair.Create('required', TJSONFalse.Create));
  end;
end;

procedure TSwaggerFilter.Filter(ARequestContext: TWiRLContainerRequestContext);
var
  LSwagger: TJSONObject;
begin
  FWiRLContext := ARequestContext.Context;

  if ARequestContext.Request.PathInfo.StartsWith('/swagger') then
  begin
    LSwagger := BuildSwagger;
    try
      ARequestContext.Response.ContentType := TMediaType.APPLICATION_JSON;
      ARequestContext.Response.Content :=  TJSONHelper.ToJSON(LSwagger);
      ARequestContext.Abort;
    finally
      LSwagger.Free;
    end;
  end;
end;

function TSwaggerFilter.GetPath(ARttiObject: TRttiObject): string;
var
  LPath: string;
begin
  LPath := '';
  TRttiHelper.HasAttribute<PathAttribute>(ARttiObject,
    procedure (AAttr: PathAttribute)
    begin
      LPath := AAttr.Value;
    end
  );
  Result := LPath;
end;

initialization
  TWiRLFilterRegistry.Instance.RegisterFilter<TSwaggerFilter>;

end.
