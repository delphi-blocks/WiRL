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

  WiRL.Core.Application,
  WiRL.Core.Engine,
  WiRL.Core.Context,
  WiRL.Core.Registry,
  WiRL.Core.Attributes,
  WiRL.http.Accept.MediaType,
  WiRL.Rtti.Utils,
  WiRL.http.Filters;

type
  TSwaggerSchemaCompiler = class
  public
    class procedure SetTypeProperties(AType: TRttiObject; AJSON: TJSONObject); static;
  end;

  [PreMatching]
  TSwaggerFilter = class(TInterfacedObject, IWiRLContainerRequestFilter)
  private
    const
      SwaggerVersion = '2.0';

    function GetPath(ARttiObject: TRttiObject): string;

    procedure BuildSwagger(AContext: TWiRLContext; ASwagger: TJSONObject);
    procedure AddApplicationResource(APaths: TJSONObject; AApplication: TWiRLApplication);
    procedure AddResource(APaths: TJSONObject; AApplication: TWiRLApplication; LResource: TClass);
    procedure AddOperation(AJsonPath: TJSONObject; const AMethodName: string; AResourceMethod: TRttiMethod);
    function CreateParameter(ARttiParameter: TRttiParameter): TJSONObject;
  public
    procedure Filter(ARequestContext: TWiRLContainerRequestContext);
  end;

  function ExistsInArray(AArray: TJSONArray; const AValue: string): Boolean;

implementation

uses
  System.TypInfo,
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

{ TSwaggerFilter }

procedure TSwaggerFilter.AddApplicationResource(APaths: TJSONObject;
  AApplication: TWiRLApplication);
var
  LResourceName: string;
  LResource: TClass;
begin
  // Loop on every resource of the application
  for LResourceName in AApplication.Resources.Keys do
  begin
    LResource := AApplication.GetResourceInfo(LResourceName).TypeTClass;
    AddResource(APaths, AApplication, LResource);
  end;
end;

procedure TSwaggerFilter.AddResource(APaths: TJSONObject;
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
        LMethodPath := (AApplication.Engine as TWiRLEngine).BasePath + AApplication.BasePath + LResourcePath;
        if (not LMethodPath.EndsWith('/')) and (not LResourceMethodPath.StartsWith('/')) then
          LMethodPath := LMethodPath + '/';
        LMethodPath := LMethodPath + LResourceMethodPath;
        // If the resource is already documented add the information on
        // the same json object
        LJsonPath := FindOrCreatePath(APaths, LMethodPath);
        AddOperation(LJsonPath, LMethodName, LResourceMethod);
      end;
    end;
  end;

end;

procedure TSwaggerFilter.BuildSwagger(AContext: TWiRLContext; ASwagger: TJSONObject);
var
  LInfo: TJSONObject;
  LPaths: TJSONObject;
  LServer: TWiRLServer;
  LEngine: TWiRLCustomEngine;
  LAppInfo: TWiRLApplicationInfo;
begin
  LServer := AContext.Server as TWiRLServer;

  // Info object
  LInfo := TJSONObject.Create;
  LInfo.AddPair('title', ChangeFileExt(ExtractFileName(ParamStr(0)), ''));
  LInfo.AddPair('version', '1.0');

  // Paths object
  LPaths := TJSONObject.Create;

  for LEngine in LServer.Engines do
  begin
    if LEngine is TWiRLEngine then
    begin
      for LAppInfo in TWiRLEngine(LEngine).Applications do
      begin
        AddApplicationResource(LPaths, LAppInfo.Application);
      end;
    end;
  end;
  // Swagger object (root)
  ASwagger.AddPair('swagger', SwaggerVersion);
  ASwagger.AddPair('host', AContext.Request.Host);
//  ASwagger.AddPair('basePath', LEngine.BasePath);
  ASwagger.AddPair('info', LInfo);
  ASwagger.AddPair('paths', LPaths);
end;

procedure TSwaggerFilter.AddOperation(AJsonPath: TJSONObject;
  const AMethodName: string; AResourceMethod: TRttiMethod);

  function CreateResponseSchema(AResourceMethod: TRttiMethod): TJSONObject;
  begin
    Result := TJSONObject.Create;
    TSwaggerSchemaCompiler.SetTypeProperties(AResourceMethod.ReturnType, Result);
  end;

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
  // Operation = Path+HttpMethod
  // If more object method use the same operation add info on the
  // same operation
  LOperation := FindOrCreateOperation(AJsonPath, AMethodName);

  if not Assigned(LOperation.GetValue('summary')) then
    LOperation.AddPair('summary', AResourceMethod.Name);

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
      LOkResponse.AddPair('schema', CreateResponseSchema(AResourceMethod));
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

  function GetParamSchema(ARttiParameter: TRttiParameter): TJSONObject;
  begin
    Result := TJSONObject.Create;
    Result.AddPair('type', 'string');
    Result.AddPair('format', 'byte');
  end;

var
  LParameter: TJSONObject;
  LParamType: string;
begin
  LParamType := GetParamPosition(ARttiParameter);
  if LParamType <> '' then
  begin
    LParameter := TJSONObject.Create;

    LParameter.AddPair(TJSONPair.Create('name', ARttiParameter.Name));
    LParameter.AddPair(TJSONPair.Create('in', LParamType));

    if LParamType = 'path' then
      LParameter.AddPair(TJSONPair.Create('required', TJSONTrue.Create))
    else
      LParameter.AddPair(TJSONPair.Create('required', TJSONFalse.Create));

    if LParamType <> 'body' then
    begin
      TSwaggerSchemaCompiler.SetTypeProperties(ARttiParameter, LParameter);
    end
    else
    begin
      LParameter.AddPair(TJSONPair.Create('schema', GetParamSchema(ARttiParameter)));
    end;
  end
  else
    LParameter := nil;
  Result := LParameter;
end;

procedure TSwaggerFilter.Filter(ARequestContext: TWiRLContainerRequestContext);
var
  LSwagger: TJSONObject;
begin
  if ARequestContext.Request.PathInfo.StartsWith('/swagger') then
  begin
    LSwagger := TJSONObject.Create;
    try
      BuildSwagger(ARequestContext.Context, LSwagger);
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

{ TSwaggerSchemaCompiler }

class procedure TSwaggerSchemaCompiler.SetTypeProperties(AType: TRttiObject; AJSON: TJSONObject);
var
  LTypeKind: TTypeKind;
  LPTypeInfo: PTypeInfo;
begin
  LPTypeInfo := PTypeInfo(AType.Handle);
  LTypeKind := PTypeInfo(AType.Handle)^.Kind;

  case LTypeKind of
    tkInteger, tkInt64:
    begin
      AJSON.AddPair(TJSONPair.Create('type', 'integer'));
    end;

    tkClass, tkRecord:
    begin
      AJSON.AddPair(TJSONPair.Create('type', 'object'));
    end;

    tkDynArray, tkArray:
    begin
      AJSON.AddPair(TJSONPair.Create('type', 'array'));
    end;

    tkEnumeration:
    begin
      if (LPTypeInfo = System.TypeInfo(Boolean)) then
        AJSON.AddPair(TJSONPair.Create('type', 'boolean'))
      else
        AJSON.AddPair(TJSONPair.Create('type', 'string'));
    end;

    tkFloat:
    begin
      if (LPTypeInfo = System.TypeInfo(TDateTime)) then
      begin
        AJSON.AddPair(TJSONPair.Create('type', 'string'));
        AJSON.AddPair(TJSONPair.Create('format', 'date-time'));
      end
      else if (LPTypeInfo = System.TypeInfo(TDate)) then
      begin
        AJSON.AddPair(TJSONPair.Create('type', 'string'));
        AJSON.AddPair(TJSONPair.Create('format', 'date'));
      end
      else if (LPTypeInfo = System.TypeInfo(TTime)) then
      begin
        AJSON.AddPair(TJSONPair.Create('type', 'string'));
        AJSON.AddPair(TJSONPair.Create('format', 'date-time'));
      end
      else
        AJSON.AddPair(TJSONPair.Create('type', 'number'));
    end;

  else
    AJSON.AddPair(TJSONPair.Create('type', 'string'));
  end;
end;

initialization
  TWiRLFilterRegistry.Instance.RegisterFilter<TSwaggerFilter>;

end.
