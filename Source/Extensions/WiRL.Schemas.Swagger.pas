{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
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

implementation

{ TSwaggerFilter }

procedure TSwaggerFilter.AddApplicationResource(APaths: TJSONObject;
  AApplication: TWiRLApplication);
var
  LResourceName: string;
  LResource: TClass;
begin
  // Loop on every resource of the application
  for LResourceName in AApplication.Resources do
  begin
    LResource := AApplication.GetResourceInfo(LResourceName).TypeTClass;
    AddResource(APaths, AApplication, LResource);
  end;
end;

procedure TSwaggerFilter.AddResource(APaths: TJSONObject;
  AApplication: TWiRLApplication; LResource: TClass);

  function FindOrCreatePath(APaths: TJSONObject; const AResourcePath: string) :TJSONObject;
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
        LMethodPath := AApplication.BasePath + LResourcePath + GetPath(LResourceMethod);
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
  LEngine: TWiRLEngine;
  LAppPair: TPair<string,TWiRLApplication>;
begin
  LEngine := AContext.Engine as TWiRLEngine;

  // Info object
  LInfo := TJSONObject.Create;
  LInfo.AddPair('title', LEngine.Name);
  LInfo.AddPair('version', '1.0');

  // Paths object
  LPaths := TJSONObject.Create;
  for LAppPair in LEngine.Applications do
  begin
    AddApplicationResource(LPaths, LAppPair.Value);
  end;

  // Swagger object (root)
  ASwagger.AddPair('swagger', SwaggerVersion);
  ASwagger.AddPair('host', AContext.Request.Host);
  ASwagger.AddPair('basePath', LEngine.BasePath);
  ASwagger.AddPair('info', LInfo);
  ASwagger.AddPair('paths', LPaths);
end;

procedure TSwaggerFilter.AddOperation(AJsonPath: TJSONObject; 
  const AMethodName: string; AResourceMethod: TRttiMethod);

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
    LResponses.AddPair('200', TJSONObject.Create(TJSONPair.Create('description', 'Ok')));
    LResponses.AddPair('default', TJSONObject.Create(TJSONPair.Create('description', 'Error')));
  end;
end;

function TSwaggerFilter.CreateParameter(
  ARttiParameter: TRttiParameter): TJSONObject;

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

  function GetParamType(ARttiParameter: TRttiParameter): string;
  begin
    case ARttiParameter.ParamType.TypeKind of
      tkInteger: Result :=  'integer';
      tkFloat: Result := 'number';
      tkInt64: Result := 'integer';
    else
      Result := 'string';
    end;
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
      LParameter.AddPair(TJSONPair.Create('type', GetParamType(ARttiParameter)));
      //LParameter.AddPair(TJSONPair.Create('format', 'int64'));
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
      ARequestContext.Response.Content := LSwagger.ToJSON;
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
    end);
  Result := LPath;
end;

initialization
  TWiRLFilterRegistry.Instance.RegisterFilter<TSwaggerFilter>;


end.
