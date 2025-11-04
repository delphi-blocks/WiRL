{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2025 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Application.Worker;

interface

uses
  System.SysUtils, System.Classes, System.Rtti, System.Generics.Collections,

  WiRL.http.Core,
  WiRL.http.Headers,
  WiRL.http.Filters,
  WiRL.http.Accept.MediaType,
  WiRL.Core.Declarations,
  WiRL.Core.Classes,
  WiRL.Core.Metadata,
  WiRL.Core.Application,
  WiRL.Core.GarbageCollector,
  WiRL.Core.MessageBodyReader,
  WiRL.Core.MessageBodyWriter,
  WiRL.Core.Registry,
  WiRL.Core.Context.Server,
  WiRL.Core.Auth.Context,
  WiRL.Core.Validators,
  WiRL.Core.Injection,
  WiRL.Configuration.Auth;

type
  TWiRLResourceLocator = class
  private
    FContext: TWiRLContext;
    FApplication: TWiRLApplication;
    FMethod: TWiRLProxyMethod;
    FProxy: TWiRLProxyApplication;
    FResource: TWiRLProxyResource;
    FHasLocated: Boolean;
    procedure LocateResource;
    procedure LocateResourceMethod;
  public
    constructor Create(AProxy: TWiRLProxyApplication; AContext: TWiRLContext);
    procedure Process();
  public
    property HasLocated: Boolean read FHasLocated write FHasLocated;
    property Resource: TWiRLProxyResource read FResource write FResource;
    property Method: TWiRLProxyMethod read FMethod write FMethod;
  end;

  TWiRLApplicationWorker = class
  private
    FContext: TWiRLContextServer;
    FAppConfig: TWiRLApplication;
    FAuthContext: TWiRLAuthContext;
    FLocator: TWiRLResourceLocator;
    FGC: TWiRLGarbageCollector;

    function HasRowConstraints(const AAttrArray: TAttributeArray): Boolean;
    procedure ValidateMethodParam(const AAttrArray: TAttributeArray; AValue: TValue; ARawConstraint: Boolean);
    function GetConstraintErrorMessage(AAttr: TCustomConstraintAttribute): string;
    function RequestOwnedObject(const AValue: TValue): Boolean;
  protected
    procedure InternalHandleRequest;

    procedure ContextInjection(AInstance: TObject);
    function ContextInjectionByType(const AObject: TRttiObject; out AValue: TValue): Boolean;

    procedure CheckAuthorization(AAuth: TWiRLAuthContext);
    function FillAnnotatedParam(AParam: TWiRLProxyParameter; AResourceInstance: TObject): TValue;
    procedure FillResourceMethodParameters(AInstance: TObject; var AArgumentArray: TArgumentArray);
    procedure InvokeResourceMethod(AInstance: TObject; const AWriter: IMessageBodyWriter; AMediaType: TMediaType); virtual;

    function GetAuthToken: string;
    function CreateAuthContext: TWiRLAuthContext;
  public
    constructor Create(AContext: TWiRLContextServer);
    destructor Destroy; override;

    // Filters handling
    function ApplyRequestFilters: Boolean;
    procedure ApplyResponseFilters;

    // HTTP Request handling
    procedure HandleRequest;
  end;

implementation

uses
  System.StrUtils, System.TypInfo, System.DateUtils,

  WiRL.Configuration.JWT,
  WiRL.Configuration.Converter,
  WiRL.http.URL,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.http.MultipartData,
  WiRL.Core.Attributes,
  WiRL.Core.Exceptions,
  WiRL.Core.Converter,
  WiRL.Rtti.Utils,
  WiRL.Engine.REST;

type
  TRequestParam = class(TObject)
  strict private
    FContentStream: TStream;
    FParamName: string;
    FTypeKind: TTypeKind;
    FHeaders: IWiRLHeaders;
    FMediaType: TMediaType;
    FStringValue: string;
    FStreamValue: TStream;
    FOwnMediaType: Boolean;

    procedure AssingMediaType(const AContentType: string);
  public
    class function ParamNameToParamIndex(AContext: TWiRLContext; const AParamName: string): Integer;
    class function GetDefaultValue(LParam: TRttiParameter): string;
  public
    property ParamName: string read FParamName;
    property TypeKind: TTypeKind read FTypeKind;
    property Headers: IWiRLHeaders read FHeaders;
    property MediaType: TMediaType read FMediaType;
    property StringValue: string read FStringValue;
    property StreamValue: TStream read FStreamValue;

    function AsStream: TStream;
    function AsString: string;

    constructor Create(AContext: TWiRLContext; AParam: TWiRLProxyParameter; AAttr: TCustomAttribute; const ADefault: string);
    destructor Destroy; override;
  end;

{ TWiRLApplicationWorker }

constructor TWiRLApplicationWorker.Create(AContext: TWiRLContextServer);
begin
  Assert(Assigned(AContext.Application), 'AContext.Application cannot be nil');

  FContext := AContext;
  FAppConfig := AContext.Application as TWiRLApplication;
  FLocator := TWiRLResourceLocator.Create(FAppConfig.Proxy, FContext);
  FGC := TWiRLGarbageCollector.Create(nil);
  FContext.AddContainerOnce(FGC);
  //FResource := TWiRLProxyResource.Create(FAppConfig.Resources);
end;

destructor TWiRLApplicationWorker.Destroy;
begin
  FLocator.Free;
  FGC.Free;
  inherited;
end;

function TWiRLApplicationWorker.ApplyRequestFilters: Boolean;
var
  LRequestFilter: IWiRLContainerRequestFilter;
  LAborted: Boolean;
begin
  Result := False;
  LAborted := False;

  { TODO -opaolo -c : Return an Exception? 16/03/2021 15:19:17 }
  if not FLocator.HasLocated then
    Exit;

  // Run filters
  FAppConfig.FilterRegistry.FetchRequestFilter(TWiRLFilterType.Normal,
    procedure (ConstructorInfo: TWiRLFilterConstructorProxy)
    var
      LRequestContext: TWiRLContainerRequestContext;
    begin
      if FLocator.Method.HasFilter(ConstructorInfo.Attribute) then
      begin
        LRequestFilter := ConstructorInfo.GetRequestFilter;
        ContextInjection(LRequestFilter as TObject);
        LRequestContext := TWiRLContainerRequestContext.Create(FContext,
          FLocator.Resource, FLocator.Method);
        try
          LRequestFilter.Filter(LRequestContext);
          LAborted := LAborted or LRequestContext.Aborted;
        finally
          LRequestContext.Free;
        end;
      end;
    end
  );
  Result := LAborted;
end;

procedure TWiRLApplicationWorker.ApplyResponseFilters;
var
  LResponseFilter: IWiRLContainerResponseFilter;
begin
  { TODO -opaolo -c : Return an Exception? 16/03/2021 15:19:17 }
  if not FLocator.HasLocated then
    Exit;

  // Run filters
  FAppConfig.FilterRegistry.FetchResponseFilter(TWiRLFilterType.Normal,
    procedure (ConstructorInfo: TWiRLFilterConstructorProxy)
    var
      LResponseContext: TWiRLContainerResponseContext;
    begin
      if FLocator.Method.HasFilter(ConstructorInfo.Attribute) then
      begin
        LResponseFilter := ConstructorInfo.GetResponseFilter;
        ContextInjection(LResponseFilter as TObject);
        LResponseContext := TWiRLContainerResponseContext.Create(FContext, FLocator.Resource);
        try
          LResponseFilter.Filter(LResponseContext);
        finally
          LResponseContext.Free;
        end;
      end;
    end
  );
end;

function TWiRLApplicationWorker.GetAuthToken: string;

  function ExtractJWTToken(const AAuth: string): string;
  var
    LAuthParts: TArray<string>;
  begin
    LAuthParts := AAuth.Split([#32]);
    if Length(LAuthParts) < 2 then
      Exit;

    if SameText(LAuthParts[0], 'Bearer') then
      Result := LAuthParts[1];
  end;

begin
  case FAppConfig.GetConfiguration<TWiRLConfigurationAuth>.TokenLocation of
    TAuthTokenLocation.Bearer: Result := ExtractJWTToken(FContext.Request.Authorization);
    TAuthTokenLocation.Cookie: Result := FContext.Request.CookieFields['token'];
    TAuthTokenLocation.Header: Result := FContext.Request.Headers.Values[FAppConfig.GetConfiguration<TWiRLConfigurationAuth>.TokenCustomHeader];
  end;
end;

procedure TWiRLApplicationWorker.CheckAuthorization(AAuth: TWiRLAuthContext);
var
  LAllowedRoles: TStringList;
  LAllowed: Boolean;
  LRole: string;
begin
  if not FLocator.Method.Auth.HasAuth then
    Exit;

  if FLocator.Method.Auth.PermitAll then
    LAllowed := FAuthContext.Verified
  else if FLocator.Method.Auth.DenyAll then
    LAllowed := False
  else
  begin
    LAllowedRoles := TStringList.Create;
    try
      LAllowedRoles.Sorted := True;
      LAllowedRoles.Duplicates := TDuplicates.dupIgnore;
      LAllowedRoles.AddStrings(FLocator.Method.Auth.Roles);

      LAllowed := False;
      for LRole in LAllowedRoles do
      begin
        LAllowed := AAuth.Subject.HasRole(LRole);
        if LAllowed then
          Break;
      end;
    finally
      LAllowedRoles.Free;
    end;
  end;

  if not LAllowed then
    raise EWiRLNotAuthorizedException.Create('Method call not authorized', Self.ClassName);
end;

procedure TWiRLApplicationWorker.ContextInjection(AInstance: TObject);
begin
  TWiRLContextInjectionRegistry.Instance.
    ContextInjection(AInstance, FContext);
end;

function TWiRLApplicationWorker.ContextInjectionByType(const AObject: TRttiObject; out AValue: TValue): Boolean;
begin
  Result := TWiRLContextInjectionRegistry.Instance.
    ContextInjectionByType(AObject, FContext, AValue);
end;

function TWiRLApplicationWorker.FillAnnotatedParam(AParam: TWiRLProxyParameter; AResourceInstance: TObject): TValue;

  function GetObjectFromParam(AMethod: TWiRLProxyMethod; AParam: TRttiParameter; AParamValue: TRequestParam): TValue;
  var
    LReader: IMessageBodyReader;
    //LContentStream: TStream;
  begin
    LReader := FAppConfig.ReaderRegistry.FindReader(AParam.ParamType, AMethod.RttiObject.GetAttributes, AParamValue.MediaType);
    if Assigned(LReader) then
    begin
      ContextInjection(LReader as TObject);
      Result := LReader.ReadFrom(AParam.ParamType, AParamValue.MediaType, AParamValue.Headers, AParamValue.AsStream);
    end
    else if AParam.ParamType.IsInstance then
      Result := TRttiHelper.CreateInstance(AParam.ParamType, AParamValue.AsString)
    else
      Result := TRttiHelper.CreateNewValue(AParam.ParamType);

    if Result.IsEmpty then
      raise EWiRLServerException.Create(Format('Unsupported media type [%s] for param [%s]', [FContext.Request.ContentMediaType.AcceptItemOnly, AParam.Name]), Self.ClassName);

  end;

  function GetSimpleParam(AMethod: TWiRLProxyMethod; AParam: TRttiParameter; AParamValue: TRequestParam): TValue;
  var
    LFormat: string;
  begin
    LFormat := FAppConfig.GetFormatSettingFor(AParam.ParamType.Handle);
    Result := TWiRLConvert.AsType(AParamValue.AsString, AParam.GetAttributes, AParam.ParamType.Handle, LFormat);
  end;

  function GetArrayFromParam(AMethod: TWiRLProxyMethod; AParam: TRttiParameter; AParamValue: TRequestParam): TValue;
  var
    LFormat: string;
    LItemType: TRttiType;
    LParamList: TArray<string>;
    LIndex: Integer;
    LItem: TValue;
  begin
    LParamList := AParamValue.AsString.Split([DefaultArraySeparator]);
    LItemType := TRttiDynamicArrayType(AParam.ParamType).ElementType;
    LFormat := FAppConfig.GetFormatSettingFor(LItemType.Handle);
    Result := TRttiHelper.CreateArrayValue(AParam.ParamType, Length(LParamList));
    for LIndex := Low(LParamList) to High(LParamList) do
    begin
      LItem := TWiRLConvert.AsType(LParamList[LIndex], AParam.GetAttributes, LItemType.Handle, LFormat);
      Result.SetArrayElement(LIndex, LItem);
    end;
  end;

var
  LParam: TRttiParameter;
  LAttr: TCustomAttribute;
  LDefaultValue: string;
  LParamAttr: TCustomAttribute;
  LParamValue: TRequestParam;
begin
  LParam := AParam.RttiParam;

  LDefaultValue := TRequestParam.GetDefaultValue(LParam);

  // Find the appropriate attribute
  LParamAttr := nil;
  for LAttr in LParam.GetAttributes do
  begin
    if (LAttr is MethodParamAttribute) or (LAttr is ContextAttribute) then
    begin
      LParamAttr := LAttr;
      Break;
    end;
  end;

  if not Assigned(LParamAttr) then
    raise EWiRLServerException.Create('Non annotated params are not allowed');

  // In case of context injection find the right object and exit
  if LParamAttr is ContextAttribute then
  begin
    if (not AParam.RttiParam.ParamType.IsInstance) or (not ContextInjectionByType(AParam.RttiParam, Result)) then
      raise EWiRLServerException.Create('Context injection failure');
    Exit;
  end;

  if AParam.RttiParam.ParamType.IsInstance and
    TRttiHelper.IsObjectOfType(AParam.RttiParam.ParamType, TWiRLFormDataPart) then
  begin
    Result := FContext.Request.MultiPartFormData[AParam.Name];
    Exit;
  end;

  try
    LParamValue := TRequestParam.Create(FContext, AParam, LParamAttr, LDefaultValue);
    try
      if HasRowConstraints(AParam.Attributes) then
        // TODO: this code forces a conversion to string (probably not a good idea)
        ValidateMethodParam(AParam.Attributes, LParamValue.AsString, True);

      // TODO: Modify, try first GetObjectFromParam (to rename!) and then GetSimpleParam
      if LParam.ParamType.TypeKind in [tkDynArray] then
        Result := GetArrayFromParam(FLocator.Method, LParam, LParamValue)
      else if LParam.ParamType.TypeKind in [tkClass, tkInterface, tkRecord, tkDynArray] then
        Result := GetObjectFromParam(FLocator.Method, LParam, LParamValue)
      else
        Result := GetSimpleParam(FLocator.Method, LParam, LParamValue);

      ValidateMethodParam(AParam.Attributes, Result, False);
    finally
      LParamValue.Free;
    end;
  except
    FGC.AddGarbage(Result);
    raise;
  end;
end;

procedure TWiRLApplicationWorker.FillResourceMethodParameters(AInstance: TObject; var AArgumentArray: TArgumentArray);
var
  LMethodParam: TWiRLProxyParameter;
begin
  try
    if FLocator.Method.Params.Count = 0 then
      Exit;

    AArgumentArray := [];
    for LMethodParam in FLocator.Method.Params do
    begin
      if not LMethodParam.Rest then
        raise EWiRLServerException.Create('Non annotated params are not allowed');

      AArgumentArray := AArgumentArray + [FillAnnotatedParam(LMethodParam, AInstance)];
    end;
  except
    on E: Exception do
    begin
      raise EWiRLWebApplicationException.Create(E, 400,
        TValuesUtil.MakeValueArray(
          Pair.S('issuer', Self.ClassName),
          Pair.S('method', 'FillResourceMethodParameters')
         )
        );
     end;
  end;
end;

function TWiRLApplicationWorker.CreateAuthContext: TWiRLAuthContext;
begin
  if Assigned(FAppConfig.GetConfiguration<TWiRLConfigurationJWT>.ClaimClass) then
    Result := TWiRLAuthContext.Create(FAppConfig.GetConfiguration<TWiRLConfigurationJWT>.ClaimClass)
  else
    Result := TWiRLAuthContext.Create;
end;

procedure TWiRLApplicationWorker.HandleRequest;
var
  LProcessResource: Boolean;
  LToken: string;
  LJWTConf: TWiRLConfigurationJWT;
begin
  FAuthContext := CreateAuthContext;
  try
    LToken := GetAuthToken;

    LJWTConf := FAppConfig.GetConfiguration<TWiRLConfigurationJWT>;

    if not LToken.IsEmpty then
    begin
      if LJWTConf.VerificationMode = TJWTVerificationMode.Verify then
      begin
        FAuthContext.Verify(LToken, LJWTConf.KeyPair.PublicKey.Key);

        if LJWTConf.CheckExpiration then
          if FAuthContext.Expired then
            raise EWiRLNotAuthorizedException.Create('Token expired');
      end
      else
        FAuthContext.DeserializeOnly(LToken);
    end;

    try
      FContext.AuthContext := FAuthContext;
      try
        // We have the URL so let's find the resource in the registry
        FLocator.Process();

        // Check method authorization with token validity and roles
        CheckAuthorization(FAuthContext);

        LProcessResource := not ApplyRequestFilters;

        if LProcessResource then
          InternalHandleRequest;
      except
        on E: Exception do
        begin
          EWiRLWebApplicationException.HandleException(FContext, E);
        end;
      end;
    finally
      ApplyResponseFilters;
    end;
  finally
    FContext.RemoveContainer(FAuthContext);
    FreeAndNil(FAuthContext);
    //FContext.AuthContext := nil;
  end;
end;

function TWiRLApplicationWorker.HasRowConstraints(const AAttrArray: TAttributeArray): Boolean;
var
  LAttr: TCustomAttribute;
begin
  Result := False;
  // Loop inside every ConstraintAttribute
  for LAttr in AAttrArray do
  begin
    if LAttr is TCustomConstraintAttribute then
    begin
      if TCustomConstraintAttribute(LAttr).RawConstraint then
        Exit(True);
    end;
  end;
end;

procedure TWiRLApplicationWorker.InternalHandleRequest;
var
  LInstance: TObject;
  LWriter: IMessageBodyWriter;
  LMediaType: TMediaType;
begin
  LInstance := FLocator.Resource.CreateInstance();
  try
    FAppConfig.WriterRegistry.FindWriter(
      FLocator.Method,
      FContext.Request.AcceptableMediaTypes,
      LWriter,
      LMediaType
    );

    try

      if FLocator.Method.IsFunction and not Assigned(LWriter) then
        raise EWiRLUnsupportedMediaTypeException.Create(
          Format('MediaType [%s] not supported on resource [%s]',
            [FContext.Request.AcceptableMediaTypes.ToString, FLocator.Resource.Path]),
          Self.ClassName, 'InternalHandleRequest'
        );

      ContextInjection(LInstance);

      if Assigned(LWriter) then
        ContextInjection(LWriter as TObject);

      try
        FContext.Request.Application := FAppConfig;
        // Set the Response Status Code before the method invocation so, inside the method,
        // we can override: HTTP response code, reason and location
        FContext.Response.FromWiRLStatus(FLocator.Method.Status);

        InvokeResourceMethod(LInstance, LWriter, LMediaType);
      finally
        LWriter := nil;
      end;

    finally
      LMediaType.Free;
    end;

  finally
    LInstance.Free;
  end;
end;

procedure TWiRLApplicationWorker.InvokeResourceMethod(AInstance: TObject;
  const AWriter: IMessageBodyWriter; AMediaType: TMediaType);

  procedure AddResultToGarbage(AMethodResult: TValue);
  begin
    if (not FLocator.Method.MethodResult.IsSingleton) then
      FGC.AddGarbage(AMethodResult);
  end;

  procedure AddArgumentsToGarbage(AArgumentArray: TArgumentArray);
  var
    LArgument: TValue;
    LParameters: TArray<TRttiParameter>;
    LArgIndex: Integer;
  begin
    LParameters := FLocator.Method.RttiObject.GetParameters;
    LArgIndex := 0;
    for LArgument in AArgumentArray do
    begin
      // Context arguments will be released by TWiRLContext if needed
      if not TRttiHelper.HasAttribute<ContextAttribute>(LParameters[LArgIndex]) then
        if not RequestOwnedObject(LArgument) then
          FGC.AddGarbage(LArgument);
      Inc(LArgIndex);
    end;
  end;

var
  LMethodResult: TValue;
  LArgumentArray: TArgumentArray;
  LStream: TMemoryStream;
  LContentType: string;
begin
  // The returned object MUST be initially nil (needs to be consistent with the Free method)
  LMethodResult := nil;
  LContentType := FContext.Response.ContentType;
  try
    LArgumentArray := [];

    FillResourceMethodParameters(AInstance, LArgumentArray);
    AddArgumentsToGarbage(LArgumentArray);

    LMethodResult := FLocator.Method.RttiObject.Invoke(AInstance, LArgumentArray);

    if FLocator.Method.IsFunction then
    begin
      AddResultToGarbage(LMethodResult);

      if LMethodResult.IsInstanceOf(TWiRLResponse) then
      begin
        // Request is already done
      end
      else if Assigned(AWriter) then // MessageBodyWriters mechanism
      begin
        if FContext.Response.ContentType = LContentType then
          FContext.Response.ContentType := AMediaType.ToString;

        LStream := TMemoryStream.Create;
        try
          LStream.Position := 0;
          FContext.Response.ContentStream := LStream;
          AWriter.WriteTo(LMethodResult, FLocator.Method.AllAttributes, AMediaType, FContext.Response.Headers, FContext.Response.ContentStream);
          LStream.Position := 0;
        except
          on E: Exception do
          begin
            raise EWiRLServerException.Create(E.Message, 'TWiRLApplicationWorker', 'InvokeResourceMethod');
          end;
        end;
      end
      else if LMethodResult.Kind <> tkUnknown then
        // fallback (no MBW, no TWiRLResponse)
        raise EWiRLNotImplementedException.Create(
          'Resource''s returned type not supported',
          Self.ClassName, 'InvokeResourceMethod'
        );
    end
    else // It's a procedure so we must set the 204 (no content) status code
    begin
      FContext.Response.StatusCode := 204;
    end;
  finally
    FGC.CollectGarbage;
  end;
end;

function TWiRLApplicationWorker.RequestOwnedObject(const AValue: TValue): Boolean;
var
  LObject: TObject;
  LIndex: Integer;
begin
  Result := False;
  if not AValue.IsObject then
    Exit(True);
  LObject := AValue.AsObject;
  if Assigned(FContext.Request.ContentStream) then
  begin
    if LObject = FContext.Request.ContentStream then
      Exit(True);
    for LIndex := 0 to FContext.Request.MultiPartFormData.Count - 1 do
    begin
      if FContext.Request.MultiPartFormData.GetPart(LIndex) = LObject then
        Exit(True);
    end;
  end;
end;

function TWiRLApplicationWorker.GetConstraintErrorMessage(AAttr: TCustomConstraintAttribute): string;
const
  AttributeSuffix = 'Attribute';
var
  AttributeName: string;
begin
  if AAttr.ErrorMessage <> '' then
    Result := AAttr.ErrorMessage
  else
  begin
    if Pos(AttributeSuffix, AAttr.ClassName) = Length(AAttr.ClassName) - Length(AttributeSuffix) + 1 then
      AttributeName := Copy(AAttr.ClassName, 1, Length(AAttr.ClassName) - Length(AttributeSuffix))
    else
      AttributeName := AAttr.ClassName;
    Result := Format('Constraint [%s] not enforced', [AttributeName]);
  end;
end;

procedure TWiRLApplicationWorker.ValidateMethodParam(
  const AAttrArray: TAttributeArray; AValue: TValue; ARawConstraint: Boolean);
var
  LAttr: TCustomAttribute;
  LValidator: IConstraintValidator<TCustomConstraintAttribute>;
  LIntf: IInterface;
begin
  // Loop inside every ConstraintAttribute
  for LAttr in AAttrArray do
  begin
    if LAttr is TCustomConstraintAttribute then
    begin
      if TCustomConstraintAttribute(LAttr).RawConstraint <> ARawConstraint then
        Continue;

      LIntf := TCustomConstraintAttribute(LAttr).GetValidator;

      if not Supports(LIntf as TObject, IConstraintValidator<TCustomConstraintAttribute>, LValidator) then
        raise EWiRLException.Create('Validator interface is not valid');

      if not LValidator.IsValid(AValue, FContext) then
        raise EWiRLValidationError.Create(GetConstraintErrorMessage(TCustomConstraintAttribute(LAttr)));
    end;
  end;
end;

{ TRequestParam }

procedure TRequestParam.AssingMediaType(const AContentType: string);
begin
  FMediaType := TMediaType.Create(AContentType);
  FOwnMediaType := True;
end;

function TRequestParam.AsStream: TStream;
begin
  if Assigned(StreamValue) then
    Exit(StreamValue);

  if not Assigned(FContentStream) then
  begin
    FContentStream := TStringStream.Create(AsString);
  end;
  Result := FContentStream;
end;

function TRequestParam.AsString: string;
begin
  if Assigned(StreamValue) then
    Result := ContentStreamToString(MediaType.Charset, StreamValue)
  else
    Result := StringValue;
end;

constructor TRequestParam.Create(AContext: TWiRLContext;
  AParam: TWiRLProxyParameter; AAttr: TCustomAttribute; const ADefault: string);
var
  LParamIndex: Integer;
  LMethod: TWiRLProxyMethod;
  LConsumesAttribute: ConsumesAttribute;
begin
  FHeaders := nil;
  FMediaType := nil;
  FStreamValue := nil;
  FStringValue := '';

  FTypeKind := AParam.RttiParam.ParamType.TypeKind;
  FParamName := (AAttr as MethodParamAttribute).Value;
  if FParamName.IsEmpty then
    FParamName := AParam.Name;

  LMethod := AContext.ResourceMethod as TWiRLProxyMethod;
  if AAttr is MethodParamAttribute then
  begin
    LConsumesAttribute := TRttiHelper.FindAttribute<ConsumesAttribute>(AParam.RttiParam);
    if Assigned(LConsumesAttribute) then
      AssingMediaType(LConsumesAttribute.Value);
  end;

  if AAttr is PathParamAttribute then
  begin

    LParamIndex := ParamNameToParamIndex(AContext, FParamName);
    if LParamIndex = -1 then
      raise EWiRLWebApplicationException.CreateFmt(
        'Formal param [%s] does not match the path param(s) [%s]',
        [FParamName, LMethod.Path]);
    FStringValue := AContext.RequestURL.PathTokens[LParamIndex];
  end
  else if AAttr is QueryParamAttribute then
    FStringValue := AContext.Request.QueryFields.Values[FParamName]
  else if AAttr is FormParamAttribute then
  begin
    if AContext.Request.ContentMediaType.MediaType = TMediaType.MULTIPART_FORM_DATA then
    begin
      FHeaders := AContext.Request.MultiPartFormData[FParamName].Headers;
      FMediaType := AContext.Request.MultiPartFormData[FParamName].ContentMediaType;
      FStreamValue := AContext.Request.MultiPartFormData[FParamName].ContentStream;
    end
    else
    begin
      FStringValue := AContext.Request.ContentFields.Values[FParamName];
    end;
  end
  else if AAttr is CookieParamAttribute then
    FStringValue := AContext.Request.CookieFields[FParamName]
  else if AAttr is HeaderParamAttribute then
    FStringValue := AContext.Request.Headers.Values[FParamName]
  else if AAttr is BodyParamAttribute then
  begin
    FHeaders := AContext.Request.Headers;
    FMediaType := AContext.Request.ContentMediaType;
    FStreamValue := AContext.Request.ContentStream;
  end
  else
    raise EWiRLServerException.Create('Unsupported method attribute');

  if not Assigned(FMediaType) then
    AssingMediaType('');

  if (not Assigned(FStreamValue)) and (FStringValue = '') then
    FStringValue := ADefault;
end;

destructor TRequestParam.Destroy;
begin
  FreeAndNil(FContentStream);
  if FOwnMediaType then
    FreeAndNil(FMediaType);
  inherited;
end;

class function TRequestParam.GetDefaultValue(LParam: TRttiParameter): string;
var
  LDefaultValue: string;
begin
  LDefaultValue := '';
  TRttiHelper.HasAttribute<DefaultValueAttribute>(LParam,
    procedure (LAttr: DefaultValueAttribute)
    begin
      LDefaultValue := LAttr.Value;
    end
  );
  Result := LDefaultValue;
end;

class function TRequestParam.ParamNameToParamIndex(AContext: TWiRLContext;
  const AParamName: string): Integer;
var
  LResURL: TWiRLURL;
  LPair: TPair<Integer, string>;
  LEngine: TWiRLRESTEngine;
  LApplication: TWiRLApplication;
  LResource: TWiRLProxyResource;
  LMethod: TWiRLProxyMethod;
begin
  LResource := AContext.Resource as TWiRLProxyResource;
  LMethod := AContext.ResourceMethod as TWiRLProxyMethod;

  LApplication := TWiRLApplication(AContext.Application);
  LEngine := TWiRLRESTEngine(AContext.Engine);

  LResURL := TWiRLURL.MockURL(LEngine.BasePath,
    LApplication.BasePath, LResource.Path, LMethod.Path);
  try
    Result := -1;
    for LPair in LResURL.PathParams do
    begin
      if SameText(AParamName, LPair.Value) then
      begin
        Result := LPair.Key;
        Break;
      end;
    end;
  finally
    LResURL.Free;
  end;
end;

{ TWiRLResourceLocator }

constructor TWiRLResourceLocator.Create(AProxy: TWiRLProxyApplication; AContext: TWiRLContext);
begin
  FProxy := AProxy;
  FContext := AContext;
end;

procedure TWiRLResourceLocator.LocateResource;
var
  LResourcePair: TPair<string, TWiRLProxyResource>;
begin
  FResource := nil;
  FApplication := FContext.Application as TWiRLApplication;
  for LResourcePair in FProxy.Resources do
  begin
    if FContext.ResourceURL.MatchResource(LResourcePair.Key) then
    begin
      FResource := LResourcePair.Value;
      Exit;
    end;
  end;
end;

procedure TWiRLResourceLocator.LocateResourceMethod;
var
  LConsumesMatch: Boolean;
  LMethod: TWiRLProxyMethod;
  LPrototypeURL: TWiRLURL;
  LPathMatches,
  LProducesMatch,
  LHttpMethodMatches: Boolean;
  LMedia: TMediaType;
begin
  FMethod := nil;

  for LMedia in FContext.Request.AcceptableMediaTypes do
  begin

    for LMethod in FResource.Methods do
    begin
      // Skip the non-REST methods (no GET/POST/PUT methods)
      if not LMethod.Rest then
        Continue;

      LHttpMethodMatches := LMethod.HttpVerb = FContext.Request.Method;

      if not LHttpMethodMatches then
        Continue;

      LPrototypeURL := TWiRLURL.MockURL(FApplication.EnginePath, FApplication.BasePath, FResource.Path, LMethod.Path);
      try
        LPathMatches := LPrototypeURL.MatchPath(FContext.RequestURL);
      finally
        LPrototypeURL.Free;
      end;

      if not LPathMatches then
        Continue;

      LProducesMatch := FResource.MatchProduces(LMethod, LMedia);
      LConsumesMatch := FResource.MatchConsumes(LMethod, FContext.Request.ContentMediaType);

      if LProducesMatch and LConsumesMatch then
      begin
        FMethod := LMethod;
        Break;
      end;
    end;

    // Already found for the first MediaType, no further search
    if Assigned(FMethod) then
      Break;
  end;
end;

procedure TWiRLResourceLocator.Process;
begin
  LocateResource;
  if not Assigned(FResource) then
    raise EWiRLNotFoundException.Create(
      Format('Resource [%s] not found', [FContext.RequestURL.URL]),
      Self.ClassName, 'HandleRequest'
    );

  LocateResourceMethod;
  if not Assigned(FMethod) then
    raise EWiRLNotFoundException.Create(
      Format('Resource''s method [%s] not found to handle resource [%s]', [FContext.Request.Method, FContext.RequestURL.URL]),
      Self.ClassName, 'HandleRequest'
    );

  FContext.Resource := FResource;
  FContext.ResourceMethod := FMethod;
  FHasLocated := True;
end;

end.
