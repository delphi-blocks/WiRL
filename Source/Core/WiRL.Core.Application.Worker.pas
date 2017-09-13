{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Application.Worker;

interface

uses
  System.SysUtils, System.Classes, System.Rtti, System.Generics.Collections,

  WiRL.Core.Application,
  WiRL.Core.Classes,
  WiRL.Core.Resource,
  WiRL.Core.MessageBodyReader,
  WiRL.Core.MessageBodyWriter,
  WiRL.Core.Registry,
  WiRL.http.Accept.MediaType,
  WiRL.Core.Context,
  WiRL.Core.Auth.Context,
  WiRL.Core.Validators,
  WiRL.http.Filters,
  WiRL.Core.Injection;


type
  TWiRLApplicationWorker = class
  private type
    TParamReader = record
    private
      FWorker: TWiRLApplicationWorker;
      FContext: TWiRLContext;
      FParam: TRttiParameter;
      FDefaultValue: string;
    public
      function AsString(AAttr: TCustomAttribute): string;
      function AsInteger(AAttr: TCustomAttribute): Integer;
      function AsChar(AAttr: TCustomAttribute): Char;
      function AsFloat(AAttr: TCustomAttribute): Double;
      constructor Create(AWorker: TWiRLApplicationWorker; AParam: TRttiParameter; const ADefaultValue: string);
    end;
  private
    FContext: TWiRLContext;
    FAppConfig: TWiRLApplication;
    FAuthContext: TWiRLAuthContext;
    FResource: TWiRLResource;

    procedure CollectGarbage(const AValue: TValue);
    function HasRowConstraints(const AAttrArray: TAttributeArray): Boolean;
    procedure ValidateMethodParam(const AAttrArray: TAttributeArray; AValue: TValue; ARawConstraint: Boolean);
    function GetConstraintErrorMessage(AAttr: TCustomConstraintAttribute): string;
  protected
    procedure InternalHandleRequest;

    procedure ContextInjection(AInstance: TObject);
    function ContextInjectionByType(const AObject: TRttiObject; out AValue: TValue): Boolean;

    procedure CheckAuthorization(AAuth: TWiRLAuthContext);
    function FillAnnotatedParam(AParam: TRttiParameter; const AAttrArray: TAttributeArray; AResourceInstance: TObject): TValue;
    procedure FillResourceMethodParameters(AInstance: TObject; var AArgumentArray: TArgumentArray);
    procedure InvokeResourceMethod(AInstance: TObject; const AWriter: IMessageBodyWriter; AMediaType: TMediaType); virtual;
    function ParamNameToParamIndex(const AParamName: string): Integer;

    procedure AuthContextFromConfig(AContext: TWiRLAuthContext);
    function GetAuthContext: TWiRLAuthContext;
  public
    constructor Create(AContext: TWiRLContext);
    destructor Destroy; override;

    // Filters handling
    function ApplyRequestFilters: Boolean;
    procedure ApplyResponseFilters;
    // HTTP Request handling
    procedure HandleRequest;
  end;

implementation

uses
  System.StrUtils, System.TypInfo,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.Core.Exceptions,
  WiRL.Core.Utils,
  WiRL.Rtti.Utils,
  WiRL.http.URL,
  WiRL.Core.Attributes,
  WiRL.Core.Engine,
  WiRL.Core.JSON;

{ TWiRLApplicationWorker }

constructor TWiRLApplicationWorker.Create(AContext: TWiRLContext);
begin
  Assert(Assigned(AContext.Application), 'AContext.Application cannot be nil');

  FContext := AContext;
  FAppConfig := AContext.Application as TWiRLApplication;

  FResource := TWiRLResource.Create(AContext);
end;

destructor TWiRLApplicationWorker.Destroy;
begin
  FResource.Free;
  inherited;
end;

function TWiRLApplicationWorker.ApplyRequestFilters: Boolean;
var
  LRequestFilter: IWiRLContainerRequestFilter;
  LAborted: Boolean;
begin
  Result := False;
  LAborted := False;
  // Find resource type
  if not FResource.Found then
    Exit;

  // Find resource method
  if not Assigned(FResource.Method) then
    Exit;

  // Run filters
  FAppConfig.FilterRegistry.FetchRequestFilter(False,
    procedure (ConstructorInfo: TWiRLFilterConstructorInfo)
    var
      LRequestContext: TWiRLContainerRequestContext;
    begin
      if FResource.Method.HasFilter(ConstructorInfo.Attribute) then
      begin
        LRequestFilter := ConstructorInfo.GetRequestFilter;
        ContextInjection(LRequestFilter as TObject);
        LRequestContext := TWiRLContainerRequestContext.Create(FContext, FResource);
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
  // Find resource type
  if not FResource.Found then
    Exit;

  // Find resource method
  if not Assigned(FResource.Method) then
    Exit;

  // Run filters
  FAppConfig.FilterRegistry.FetchResponseFilter(False,
    procedure (ConstructorInfo: TWiRLFilterConstructorInfo)
    var
      LResponseContext: TWiRLContainerResponseContext;
    begin
      if FResource.Method.HasFilter(ConstructorInfo.Attribute) then
      begin
        LResponseFilter := ConstructorInfo.GetResponseFilter;
        ContextInjection(LResponseFilter as TObject);
        LResponseContext := TWiRLContainerResponseContext.Create(FContext, FResource);
        try
          LResponseFilter.Filter(LResponseContext);
        finally
          LResponseContext.Free;
        end;
      end;
    end
  );
end;

procedure TWiRLApplicationWorker.AuthContextFromConfig(AContext: TWiRLAuthContext);
var
  LToken: string;

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
  case FAppConfig.TokenLocation of
    TAuthTokenLocation.Bearer: LToken := ExtractJWTToken(FContext.Request.Authorization);
    TAuthTokenLocation.Cookie: LToken := FContext.Request.CookieFields['token'];
    TAuthTokenLocation.Header: LToken := FContext.Request.HeaderFields[FAppConfig.TokenCustomHeader];
  end;

  if LToken.IsEmpty then
    Exit;

  AContext.Verify(LToken, FAppConfig.Secret);
end;

procedure TWiRLApplicationWorker.CheckAuthorization(AAuth: TWiRLAuthContext);
var
  LAllowedRoles: TStringList;
  LAllowed: Boolean;
  LRole: string;
begin
  // Non Auth-annotated method are "PermitAll"
  if not FResource.Method.Auth.HasAuth then
    Exit;

  if FResource.Method.Auth.PermitAll then
    Exit;

  if FResource.Method.Auth.DenyAll then
    LAllowed := False
  else
  begin
    LAllowedRoles := TStringList.Create;
    try
      LAllowedRoles.Sorted := True;
      LAllowedRoles.Duplicates := TDuplicates.dupIgnore;
      LAllowedRoles.AddStrings(FResource.Method.Auth.Roles);

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

procedure TWiRLApplicationWorker.CollectGarbage(const AValue: TValue);
var
  LIndex: Integer;
  LValue: TValue;
begin
  case AValue.Kind of
    tkClass: begin
      // If the request content stream is used as a param to a resource
      // it will be freed at the end process
      if AValue.AsObject <> FContext.Request.ContentStream then
        if not TRttiHelper.HasAttribute<SingletonAttribute>(AValue.AsObject.ClassType) then
          AValue.AsObject.Free;
    end;

    tkInterface: TObject(AValue.AsInterface).Free;

    tkArray,
    tkDynArray:
    begin
      for LIndex := 0 to AValue.GetArrayLength -1 do
      begin
        LValue := AValue.GetArrayElement(LIndex);
        case LValue.Kind of
          tkClass: LValue.AsObject.Free;
          tkInterface: TObject(LValue.AsInterface).Free;
          tkArray, tkDynArray: CollectGarbage(LValue); //recursion
        end;
      end;
    end;
  end;
end;

procedure TWiRLApplicationWorker.ContextInjection(AInstance: TObject);
begin
  TWiRLContextInjectionRegistry.Instance.
    ContextInjection(AInstance, FContext);
end;

function TWiRLApplicationWorker.ContextInjectionByType(const AObject: TRttiObject;
  out AValue: TValue): Boolean;
begin
  Result := TWiRLContextInjectionRegistry.Instance.
    ContextInjectionByType(AObject, FContext, AValue);
end;

function TWiRLApplicationWorker.FillAnnotatedParam(AParam: TRttiParameter;
    const AAttrArray: TAttributeArray; AResourceInstance: TObject): TValue;

  function CreateParamInstance(AParam: TRttiParameter; const AValue: string): TObject;
  begin
    Result := TRttiHelper.CreateInstance(AParam.ParamType, AValue);
    if not Assigned(Result) then
      raise EWiRLServerException.Create(Format('Unsupported data type for param [%s]', [AParam.Name]), Self.ClassName);
  end;

var
  LAttr: TCustomAttribute;
  LParamName: string;
  LContextValue: TValue;
  LReader: IMessageBodyReader;
  LDefaultValue: string;
  LParamReader: TParamReader;
begin
  // Search a default value
  LDefaultValue := '';
  TRttiHelper.HasAttribute<DefaultValueAttribute>(AParam, procedure (LAttr: DefaultValueAttribute)
  begin
    LDefaultValue := LAttr.Value;
  end);

  LParamName := '';
  for LAttr in AAttrArray do
  begin
    // Loop only inside attributes that define how to read the parameter
    if not ( (LAttr is ContextAttribute) or (LAttr is MethodParamAttribute) ) then
      Continue;

    LParamReader := TParamReader.Create(Self, AParam, LDefaultValue);

    // context injection
    if (LAttr is ContextAttribute) and (AParam.ParamType.IsInstance) then
    begin
      if ContextInjectionByType(AParam, LContextValue) then
        Exit(LContextValue);
    end;

    LParamName := (LAttr as MethodParamAttribute).Value;
    if (LParamName = '') or (LAttr is BodyParamAttribute) then
      LParamName := AParam.Name;

    case AParam.ParamType.TypeKind of
      tkInt64,
      tkInteger:
      begin
        Result := TValue.From<Integer>(LParamReader.AsInteger(LAttr));
      end;

      tkFloat:
      begin
        Result := TValue.From<Double>(LParamReader.AsFloat(LAttr));
      end;

      tkChar,
      tkWChar:
      begin
        Result := TValue.From(LParamReader.AsChar(LAttr));
      end;

//      tkEnumeration: ;
//      tkSet: ;

      tkClass:
      begin
        if HasRowConstraints(AAttrArray) then
        begin
          ValidateMethodParam(AAttrArray, LParamReader.AsString(LAttr), True);
        end;
        if LAttr is BodyParamAttribute then
        begin
          LReader := FAppConfig.ReaderRegistry.FindReader(AParam.ParamType, FContext.Request.ContentMediaType);
          if Assigned(LReader) then
            Result := LReader.ReadFrom(AParam, FContext.Request.ContentMediaType, FContext.Request)
          else
            Result := TRttiHelper.CreateInstance(AParam.ParamType, LParamReader.AsString(LAttr));
          if Result.AsObject = nil then
            raise EWiRLServerException.Create(Format('Unsupported media type [%s] for param [%s]', [FContext.Request.ContentMediaType.AcceptItemOnly, LParamName]), Self.ClassName);
        end
        else
          Result := TRttiHelper.CreateInstance(AParam.ParamType, LParamReader.AsString(LAttr));

        if Result.AsObject = nil then
          raise EWiRLServerException.Create(Format('Unsupported data type for param [%s]', [LParamName]), Self.ClassName);
      end;

//      tkMethod: ;

      tkLString,
      tkUString,
      tkWString,
      tkString:
      begin
        Result := TValue.From(LParamReader.AsString(LAttr));
      end;

      tkVariant:
      begin
        Result := TValue.From(LParamReader.AsString(LAttr));
      end;

//      tkArray: ;
//      tkRecord: ;
//      tkInterface: ;
//      tkDynArray: ;
//      tkClassRef: ;
//      tkPointer: ;
//      tkProcedure: ;
      else
        raise EWiRLServerException.Create(Format('Unsupported data type for param [%s]', [LParamName]), Self.ClassName);
    end;
    ValidateMethodParam(AAttrArray, Result, False);
  end;
end;

procedure TWiRLApplicationWorker.FillResourceMethodParameters(AInstance: TObject;
  var AArgumentArray: TArgumentArray);
var
  LParam: TRttiParameter;
  LParamArray: TArray<TRttiParameter>;
  LAttrArray: TArray<TCustomAttribute>;

  LIndex: Integer;
begin
  try
    { TODO -opaolo -c : Move the functionality on TResource/TResourceMethod? 18/01/2017 19:29:42 }
    LParamArray := FResource.Method.RttiObject.GetParameters;

    // The method has no parameters so simply call as it is
    if Length(LParamArray) = 0 then
      Exit;

    SetLength(AArgumentArray, Length(LParamArray));

    for LIndex := Low(LParamArray) to High(LParamArray) do
    begin
      LParam := LParamArray[LIndex];

      LAttrArray := LParam.GetAttributes;

      if Length(LAttrArray) = 0 then
        raise EWiRLServerException.Create('Non annotated params are not allowed')
      else
        AArgumentArray[LIndex] := FillAnnotatedParam(LParam, LAttrArray, AInstance);
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

function TWiRLApplicationWorker.GetAuthContext: TWiRLAuthContext;
begin
  if Assigned(FAppConfig.ClaimClass) then
    Result := TWiRLAuthContext.Create(FAppConfig.ClaimClass)
  else
    Result := TWiRLAuthContext.Create;

  AuthContextFromConfig(Result);
end;

procedure TWiRLApplicationWorker.HandleRequest;
var
  LProcessResource: Boolean;
begin
  FAuthContext := GetAuthContext;
  try
    FContext.AuthContext := FAuthContext;
    try
      LProcessResource := not ApplyRequestFilters;
      if LProcessResource then
        InternalHandleRequest;
    except
      on E: Exception do
        EWiRLWebApplicationException.HandleException(FContext, E);
    end;
    ApplyResponseFilters;
  finally
    FreeAndNil(FAuthContext);
  end;
  FContext.AuthContext := nil;
end;

function TWiRLApplicationWorker.HasRowConstraints(
  const AAttrArray: TAttributeArray): Boolean;
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
  if not FResource.Found then
    raise EWiRLNotFoundException.Create(
      Format('Resource [%s] not found', [FContext.URL.Resource]),
      Self.ClassName, 'HandleRequest'
    );

  if not Assigned(FResource.Method) then
    raise EWiRLNotFoundException.Create(
      Format('Resource''s method [%s] not found to handle resource [%s]', [FContext.Request.Method, FContext.URL.Resource + FContext.URL.SubResources.ToString]),
      Self.ClassName, 'HandleRequest'
    );

  CheckAuthorization(FAuthContext);

  LInstance := FResource.CreateInstance();
  try
    FAppConfig.WriterRegistry.FindWriter(
      FResource.Method,
      FContext.Request.AcceptableMediaTypes,
      LWriter,
      LMediaType
    );

    if FResource.Method.IsFunction and not Assigned(LWriter) then
      raise EWiRLUnsupportedMediaTypeException.Create(
        Format('MediaType [%s] not supported on resource [%s]',
          [FContext.Request.AcceptableMediaTypes.ToString, FResource.Path]),
        Self.ClassName, 'InternalHandleRequest'
      );

    ContextInjection(LInstance);

    if Assigned(LWriter) then
      ContextInjection(LWriter as TObject);

    try
      // The Status Code is 200 (default)
      // Set the Response Status Code (201 for POSTs)

      InvokeResourceMethod(LInstance, LWriter, LMediaType);
    finally
      LWriter := nil;
      LMediaType.Free;
    end;

  finally
    LInstance.Free;
  end;
end;

procedure TWiRLApplicationWorker.InvokeResourceMethod(AInstance: TObject;
  const AWriter: IMessageBodyWriter; AMediaType: TMediaType);
var
  LMethodResult: TValue;
  LArgument: TValue;
  LArgumentArray: TArgumentArray;
  LStream: TMemoryStream;
  LContentType: string;
begin
  // The returned object MUST be initially nil (needs to be consistent with the Free method)
  LMethodResult := nil;
  LContentType := FContext.Response.ContentType;
  FillResourceMethodParameters(AInstance, LArgumentArray);
  try
    LMethodResult := FResource.Method.RttiObject.Invoke(AInstance, LArgumentArray);
  except
    on E: Exception do
    begin
      raise EWiRLServerException.Create(E.Message,
        'TWiRLApplicationWorker', 'InvokeResourceMethod: RttiObject.Invoke');
    end;
  end;

  try
    if not FResource.Method.IsFunction then
      Exit;

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
        AWriter.WriteTo(LMethodResult, FResource.Method.AllAttributes, AMediaType, FContext.Response);
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
  finally
    if (not FResource.Method.MethodResult.IsSingleton) then
      CollectGarbage(LMethodResult);
    for LArgument in LArgumentArray do
      CollectGarbage(LArgument);
  end;
end;

function TWiRLApplicationWorker.ParamNameToParamIndex(const AParamName: string): Integer;
var
  LResURL: TWiRLURL;
  LPair: TPair<Integer, string>;
begin
  LResURL := TWiRLURL.CreateDummy(TWiRLEngine(FContext.Engine).BasePath,
    FAppConfig.BasePath, FResource.Path, FResource.Method.Path);
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
//  LObj: TObject;
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

{ TWiRLApplicationWorker.TParamReader }

function TWiRLApplicationWorker.TParamReader.AsString(AAttr: TCustomAttribute): string;
var
  LParamName: string;
  LParamIndex: Integer;
  LAttrArray: TArray<TCustomAttribute>;
begin
  LAttrArray := FParam.GetAttributes;
  LParamName := (AAttr as MethodParamAttribute).Value;
  if LParamName = '' then
    LParamName := FParam.Name;

  if AAttr is PathParamAttribute then
  begin
    LParamIndex := FWorker.ParamNameToParamIndex(LParamName);
    Result := FContext.URL.PathTokens[LParamIndex];
  end
  else if AAttr is QueryParamAttribute then
    Result := FContext.Request.QueryFields.Values[LParamName]
  else if AAttr is FormParamAttribute then
    Result := FContext.Request.ContentFields.Values[LParamName]
  else if AAttr is CookieParamAttribute then
    Result := FContext.Request.CookieFields[LParamName]
  else if AAttr is HeaderParamAttribute then
    Result := FContext.Request.HeaderFields[LParamName]
  else if AAttr is BodyParamAttribute then
    Result := FContext.Request.Content;
  if Result = '' then
    Result := FDefaultValue;

  FWorker.ValidateMethodParam(LAttrArray, Result, True);
end;

function TWiRLApplicationWorker.TParamReader.AsInteger(AAttr: TCustomAttribute): Integer;
var
  LValue: string;
begin
  LValue := AsString(AAttr);
  if LValue = '' then
    Result := 0
  else
    Result := StrToInt(LValue);
end;

function TWiRLApplicationWorker.TParamReader.AsFloat(AAttr: TCustomAttribute): Double;
var
  LValue: string;
begin
  LValue := AsString(AAttr);
  if LValue = '' then
    Result := 0
  else
    Result := StrToFloat(LValue);
end;

function TWiRLApplicationWorker.TParamReader.AsChar(AAttr: TCustomAttribute): Char;
var
  LValue: string;
begin
  LValue := AsString(AAttr);
  if LValue = '' then
    Result := #0
  else
    Result := LValue.Chars[0];
end;

constructor TWiRLApplicationWorker.TParamReader.Create(AWorker: TWiRLApplicationWorker; AParam: TRttiParameter;
  const ADefaultValue: string);
begin
  FWorker := AWorker;
  FContext := AWorker.FContext;
  FParam := AParam;
  FDefaultValue := ADefaultValue;
end;

end.
