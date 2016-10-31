(*
  Copyright 2015-2016, WiRL - REST Library

  Home: https://github.com/WiRL-library

*)
unit WiRL.Core.Application;

{$I WiRL.inc}

interface


uses
  System.SysUtils, System.Classes, System.Rtti,
  System.Generics.Collections,
  WiRL.Core.Request,
  WiRL.Core.Response,
  WiRL.Core.Classes,
  WiRL.Core.URL,
  WiRL.Core.MessageBodyWriter,
  WiRL.Core.Registry,
  WiRL.Core.MediaType,
  WiRL.Core.Token,
  WiRL.http.Filters;

type
  TSecretGenerator = reference to function(): TBytes;
  TAttributeArray = TArray<TCustomAttribute>;
  TArgumentArray = array of TValue;

  TWiRLApplication = class
  private
    const SCRT_PREFIX = 'bWFycy5zZWNyZXQu';
  private
//    class threadvar FRequest: TWiRLRequest;
//    class threadvar FResponse: TWiRLResponse;
    class threadvar FAuthContext: TWiRLAuthContext;
  private
    FSecret: TBytes;
    FRttiContext: TRttiContext;
    FResourceRegistry: TObjectDictionary<string, TWiRLConstructorInfo>;
    FFilterRegistry :TWiRLFilterRegistry;
    FBasePath: string;
    FName: string;
    FClaimClass: TWiRLSubjectClass;
    FEngine: TObject;
    FSystemApp: Boolean;
    function GetResources: TArray<string>;
    function GetRequest: TWiRLRequest;
    function GetResponse: TWiRLResponse;
    function GetURL: TWiRLURL;
  protected
    function CheckFilterBinding(AFilterClass, AResourceClass :TClass; const AMethod: TRttiMethod) :Boolean;

    procedure InternalHandleRequest(ARequest: TWiRLRequest; AResponse: TWiRLResponse; const AURL: TWiRLURL);
    function FindMethodToInvoke(const AURL: TWiRLURL;
      const AInfo: TWiRLConstructorInfo): TRttiMethod; virtual;

    function FillAnnotatedParam(AParam: TRttiParameter; const AAttrArray: TAttributeArray;
      AResourceInstance: TObject; AMethod: TRttiMethod): TValue;
    function FillNonAnnotatedParam(AParam: TRttiParameter): TValue;
    procedure FillResourceMethodParameters(AInstance: TObject; AMethod: TRttiMethod; var AArgumentArray: TArgumentArray);
    procedure InvokeResourceMethod(AInstance: TObject; AMethod: TRttiMethod;
      const AWriter: IMessageBodyWriter; ARequest: TWiRLRequest;
      AMediaType: TMediaType); virtual;

    function GetNewToken(ARequest: TWiRLRequest): TWiRLAuthContext;
    procedure CheckAuthorization(const AMethod: TRttiMethod; const AToken: TWiRLAuthContext);
    procedure ContextInjection(AInstance: TObject);
    function ContextInjectionByType(const AType: TClass; out AValue: TValue): Boolean;

    function ParamNameToParamIndex(AResourceInstance: TObject; const AParamName: string; AMethod: TRttiMethod): Integer;

    property Engine: TObject read FEngine;
    property Request: TWiRLRequest read GetRequest;
    property Response: TWiRLResponse read GetResponse;
    property URL: TWiRLURL read GetURL;
  public
    constructor Create(const AEngine: TObject);
    destructor Destroy; override;

    // Fluent-like configuration methods
    function SetResources(const AResources: array of string): TWiRLApplication;
    function SetFilters(const AFilters: array of string): TWiRLApplication;
    function SetSecret(ASecretGen: TSecretGenerator): TWiRLApplication;
    function SetBasePath(const ABasePath: string): TWiRLApplication;
    function SetName(const AName: string): TWiRLApplication;
    function SetClaimsClass(AClaimClass: TWiRLSubjectClass): TWiRLApplication;
    function SetSystemApp(ASystem: Boolean): TWiRLApplication;

    procedure GenerateToken;
    function AddResource(AResource: string): Boolean;
    function AddFilter(AFilter: string): Boolean;
    procedure HandleRequest(ARequest: TWiRLRequest; AResponse: TWiRLResponse; const AURL: TWiRLURL);
    procedure CollectGarbage(const AValue: TValue);

    // Filters handler
    procedure ApplyRequestFilters;
    procedure ApplyResponseFilters;

    property Name: string read FName;
    property BasePath: string read FBasePath;
    property SystemApp: Boolean read FSystemApp;
    property ClaimClass: TWiRLSubjectClass read FClaimClass;
    property Resources: TArray<string> read GetResources;
  end;

  TWiRLApplicationDictionary = class(TObjectDictionary<string, TWiRLApplication>)
  end;

implementation

uses
  System.StrUtils, System.TypInfo,
  WiRL.Core.Exceptions,
  WiRL.Core.Utils,
  WiRL.Rtti.Utils,
  WiRL.Core.Attributes,
  WiRL.Core.Engine,
  WiRL.Core.JSON;

function TWiRLApplication.AddFilter(AFilter: string): Boolean;
var
  LRegistry: TWiRLFilterRegistry;
  LInfo: TWiRLFilterConstructorInfo;
begin
  Result := False;
  LRegistry := TWiRLFilterRegistry.Instance;

  if IsMask(AFilter) then // has wildcards and so on...
  begin
    for LInfo in LRegistry do
    begin
      if MatchesMask(LInfo.TypeTClass.QualifiedClassName, AFilter) then
      begin
        FFilterRegistry.Add(LInfo);
        Result := True;
      end;
    end;
  end
  else // exact match
  begin
    if LRegistry.FilterByClassName(AFilter, LInfo) then
    begin
      FFilterRegistry.Add(LInfo);
      Result := True;
    end;
  end;
end;

function TWiRLApplication.AddResource(AResource: string): Boolean;

  function AddResourceToApplicationRegistry(const AInfo: TWiRLConstructorInfo): Boolean;
  var
    LClass: TClass;
    LResult: Boolean;
  begin
    LResult := False;
    LClass := AInfo.TypeTClass;
    TRttiHelper.HasAttribute<PathAttribute>(FRttiContext.GetType(LClass),
      procedure (AAttribute: PathAttribute)
      var
        LURL: TWiRLURL;
      begin
        LURL := TWiRLURL.CreateDummy(AAttribute.Value);
        try
          if not FResourceRegistry.ContainsKey(LURL.PathTokens[0]) then
          begin
            FResourceRegistry.Add(LURL.PathTokens[0], AInfo.Clone);
            LResult := True;
          end;
        finally
          LURL.Free;
        end;
      end
    );
    Result := LResult;
  end;

var
  LRegistry: TWiRLResourceRegistry;
  LInfo: TWiRLConstructorInfo;
  LKey: string;
begin
  Result := False;
  LRegistry := TWiRLResourceRegistry.Instance;

  if IsMask(AResource) then // has wildcards and so on...
  begin
    for LKey in LRegistry.Keys.ToArray do
    begin
      if MatchesMask(LKey, AResource) then
      begin
        if LRegistry.TryGetValue(LKey, LInfo) and AddResourceToApplicationRegistry(LInfo) then
          Result := True;
      end;
    end;
  end
  else // exact match
    if LRegistry.TryGetValue(AResource, LInfo) then
      Result := AddResourceToApplicationRegistry(LInfo);
end;

function TWiRLApplication.SetBasePath(const ABasePath: string): TWiRLApplication;
begin
  FBasePath := ABasePath;
  Result := Self;
end;

function TWiRLApplication.SetName(const AName: string): TWiRLApplication;
begin
  FName := AName;
  Result := Self;
end;

function TWiRLApplication.SetClaimsClass(AClaimClass: TWiRLSubjectClass): TWiRLApplication;
begin
  FClaimClass := AClaimClass;
  Result := Self;
end;

function TWiRLApplication.SetFilters(
  const AFilters: array of string): TWiRLApplication;
var
  LFilter: string;
begin
  Result := Self;
  for LFilter in AFilters do
    Self.AddFilter(LFilter);
end;

function TWiRLApplication.SetResources(const AResources: array of string): TWiRLApplication;
var
  LResource: string;
begin
  Result := Self;
  for LResource in AResources do
    Self.AddResource(LResource);
end;

procedure TWiRLApplication.ApplyRequestFilters;
var
  LInfo: TWiRLConstructorInfo;
  LMethod: TRttiMethod;
  FilterImpl :TObject;
  RequestFilter :IWiRLContainerRequestFilter;
begin
  // Find resource method
  if not FResourceRegistry.TryGetValue(URL.Resource, LInfo) then
    Exit;

  LMethod := FindMethodToInvoke(URL, LInfo);
  if not Assigned(LMethod) then
    Exit;

  // Run filters

  FFilterRegistry.FetchRequestFilter(False, procedure (ConstructorInfo :TWiRLFilterConstructorInfo) begin
    if CheckFilterBinding(ConstructorInfo.TypeTClass, LInfo.TypeTClass, LMethod) then
    begin
      FilterImpl := ConstructorInfo.ConstructorFunc();

      // The check doesn't have any sense but I must use SUPPORT and I hate using it without a check
      if not Supports(FilterImpl, IWiRLContainerRequestFilter, RequestFilter) then
        raise EWiRLNotImplementedException.Create(
          Format('Request Filter [%s] does not implement requested interface [IWiRLContainerRequestFilter]', [ConstructorInfo.TypeTClass.ClassName]),
          Self.ClassName,
          'ApplyRequestFilters'
        );
      ContextInjection(FilterImpl);
      RequestFilter.Filter(Request);
    end;
  end);
end;

procedure TWiRLApplication.ApplyResponseFilters;
var
  LInfo: TWiRLConstructorInfo;
  LMethod: TRttiMethod;
  LFilterImpl: TObject;
  LResponseFilter: IWiRLContainerResponseFilter;
begin
  // Find resource method
  if not FResourceRegistry.TryGetValue(URL.Resource, LInfo) then
    Exit;

  LMethod := FindMethodToInvoke(URL, LInfo);
  if not Assigned(LMethod) then
    Exit;

  // Run filters
  FFilterRegistry.FetchResponseFilter(
    procedure (ConstructorInfo :TWiRLFilterConstructorInfo)
    begin
      if CheckFilterBinding(ConstructorInfo.TypeTClass, LInfo.TypeTClass, LMethod) then
      begin
        LFilterImpl := ConstructorInfo.ConstructorFunc();
        // The check doesn't have any sense but I must use SUPPORT and I hate using it without a check
        if not Supports(LFilterImpl, IWiRLContainerResponseFilter, LResponseFilter) then
          raise EWiRLNotImplementedException.Create(
            Format('Response Filter [%s] does not implement requested interface [IWiRLContainerResponseFilter]', [ConstructorInfo.TypeTClass.ClassName]),
            Self.ClassName,
            'ApplyResponseFilters'
          );
        ContextInjection(LFilterImpl);
        LResponseFilter.Filter(Request, Response);
      end;
    end
  );
end;

procedure TWiRLApplication.CheckAuthorization(const AMethod: TRttiMethod;
  const AToken: TWiRLAuthContext);
var
  LDenyAll, LPermitAll, LRolesAllowed: Boolean;
  LAllowedRoles: TStringList;
  LAllowed: Boolean;
  LRole: string;
begin
  LAllowed := True; // Default = True for non annotated-methods
  LDenyAll := False;
  LPermitAll := False;
  LAllowedRoles := TStringList.Create;
  try
    LAllowedRoles.Sorted := True;
    LAllowedRoles.Duplicates := TDuplicates.dupIgnore;

    TRttiHelper.ForEachAttribute<AuthorizationAttribute>(AMethod,
      procedure (AAttribute: AuthorizationAttribute)
      begin
        if AAttribute is DenyAllAttribute then
          LDenyAll := True
        else if AAttribute is PermitAllAttribute then
          LPermitAll := True
        else if AAttribute is RolesAllowedAttribute then
        begin
          LRolesAllowed := True;
          LAllowedRoles.AddStrings(RolesAllowedAttribute(AAttribute).Roles);
        end;
      end
    );

  if LDenyAll then
    LAllowed := False
  else
  begin
    if LRolesAllowed then
    begin
      LAllowed := False;
      for LRole in LAllowedRoles do
      begin
        LAllowed := AToken.Subject.HasRole(LRole);
        if LAllowed then
          Break;
      end;
    end;

    if LPermitAll then
      LAllowed := True;
  end;

  if not LAllowed then
    raise EWiRLNotAuthorizedException.Create('Method call not authorized', Self.ClassName);

  finally
    LAllowedRoles.Free;
  end;
end;

function HasNameBindingAttribute(AClass :TRttiType; AMethod: TRttiMethod; Attrib :TCustomAttribute) :Boolean;
var
  HasAttrib :Boolean;
begin
  // First search inside the method attributes
  HasAttrib := False;
  TRttiHelper.ForEachAttribute<TCustomAttribute>(AMethod,
    procedure (AMethodAttrib: TCustomAttribute)
    begin
      if AMethodAttrib is Attrib.ClassType then
        HasAttrib := True;
    end
  );

  // Then inside the class attributes
  if not HasAttrib then
  begin
    TRttiHelper.ForEachAttribute<TCustomAttribute>(AClass,
      procedure (AMethodAttrib: TCustomAttribute)
      begin
        if AMethodAttrib is Attrib.ClassType then
          HasAttrib := True;
      end
    );
  end;

  Result := HasAttrib;
end;

function TWiRLApplication.CheckFilterBinding(AFilterClass, AResourceClass: TClass;
  const AMethod: TRttiMethod): Boolean;
var
  LFilterType: TRttiType;
  LResourceType: TRttiType;
  LHasBinding: Boolean;
begin
  LHasBinding := True;
  LFilterType := FRttiContext.GetType(AFilterClass);
  LResourceType := FRttiContext.GetType(AResourceClass);

  // Check for attributes that subclass "NameBindingAttribute"
  TRttiHelper.ForEachAttribute<NameBindingAttribute>(LFilterType,
    procedure (AFilterAttrib: NameBindingAttribute)
    begin
      LHasBinding := LHasBinding and HasNameBindingAttribute(LResourceType, AMethod, AFilterAttrib);
    end
  );

  // Check for attributes annotaded by "NameBinding" attribute
  TRttiHelper.ForEachAttribute<TCustomAttribute>(LFilterType,
    procedure (AFilterAttrib: TCustomAttribute)
    begin
      if TRttiHelper.HasAttribute<NameBindingAttribute>(FRttiContext.GetType(AFilterAttrib.ClassType)) then
        LHasBinding := LHasBinding and HasNameBindingAttribute(LResourceType, AMethod, AFilterAttrib);
    end
  );

  Result := LHasBinding;
end;

procedure TWiRLApplication.CollectGarbage(const AValue: TValue);
var
  LIndex: Integer;
  LValue: TValue;
begin
  case AValue.Kind of
    tkClass: AValue.AsObject.Free;

    { TODO -opaolo -c : could be dangerous?? 14/01/2015 13:18:38 }
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

procedure TWiRLApplication.ContextInjection(AInstance: TObject);
var
  LType: TRttiType;
begin
  LType := FRttiContext.GetType(AInstance.ClassType);
  // Context injection
  TRttiHelper.ForEachFieldWithAttribute<ContextAttribute>(LType,
    function (AField: TRttiField; AAttrib: ContextAttribute): Boolean
    var
      LFieldClassType: TClass;
      LValue: TValue;
    begin
      Result := True; // enumerate all
      if (AField.FieldType.IsInstance) then
      begin
        LFieldClassType := TRttiInstanceType(AField.FieldType).MetaclassType;

        if not ContextInjectionByType(LFieldClassType, LValue) then
          raise EWiRLServerException.Create(
            Format('Unable to inject class [%s] in resource [%s]', [LFieldClassType.ClassName, AInstance.ClassName]),
            Self.ClassName, 'ContextInjection'
          );

        AField.SetValue(AInstance, LValue);
      end;
    end
  );

  // properties
  TRttiHelper.ForEachPropertyWithAttribute<ContextAttribute>(LType,
    function (AProperty: TRttiProperty; AAttrib: ContextAttribute): Boolean
    var
      LPropertyClassType: TClass;
      LValue: TValue;
    begin
      Result := True; // enumerate all
      if (AProperty.PropertyType.IsInstance) then
      begin
        LPropertyClassType := TRttiInstanceType(AProperty.PropertyType).MetaclassType;
        if ContextInjectionByType(LPropertyClassType, LValue) then
          AProperty.SetValue(AInstance, LValue);
      end;
    end
  );
end;

function TWiRLApplication.ContextInjectionByType(const AType: TClass;
  out AValue: TValue): Boolean;
begin
  Result := True;
  // AuthContext
  if (AType.InheritsFrom(TWiRLAuthContext)) then
    AValue := FAuthContext
  // Claims (Subject)
  else if (AType.InheritsFrom(TWiRLSubject)) then
    AValue := FAuthContext.Subject
  // HTTP request
  else if (AType.InheritsFrom(TWiRLRequest)) then
    AValue := Request
  // HTTP response
  else if (AType.InheritsFrom(TWiRLResponse)) then
    AValue := Response
  // URL info
  else if (AType.InheritsFrom(TWiRLURL)) then
    AValue := URL
  // Engine
  else if (AType.InheritsFrom(TWiRLEngine)) then
    AValue := Engine
  // Application
  else if (AType.InheritsFrom(TWiRLApplication)) then
    AValue := Self
  else
    Result := False;
end;

constructor TWiRLApplication.Create(const AEngine: TObject);
begin
  inherited Create;
  FEngine := AEngine;
  FRttiContext := TRttiContext.Create;
  FResourceRegistry := TObjectDictionary<string, TWiRLConstructorInfo>.Create([doOwnsValues]);
  FFilterRegistry := TWiRLFilterRegistry.Create;
  FFilterRegistry.OwnsObjects := False;
end;

destructor TWiRLApplication.Destroy;
begin
  FResourceRegistry.Free;
  FFilterRegistry.Free;
  inherited;
end;

function TWiRLApplication.FindMethodToInvoke(const AURL: TWiRLURL;
  const AInfo: TWiRLConstructorInfo): TRttiMethod;
var
  LResourceType: TRttiType;
  LMethod: TRttiMethod;
  LResourcePath: string;
  LAttribute: TCustomAttribute;
  LPrototypeURL: TWiRLURL;
  LPathMatches: Boolean;
  LHttpMethodMatches: Boolean;
  LMethodPath: string;
begin
  LResourceType := FRttiContext.GetType(AInfo.TypeTClass);
  Result := nil;
  LResourcePath := '';

  TRttiHelper.HasAttribute<PathAttribute>(LResourceType,
    procedure (APathAttribute: PathAttribute)
    begin
      LResourcePath := APathAttribute.Value;
    end
  );

  for LMethod in LResourceType.GetMethods do
  begin
    LMethodPath := '';
    LHttpMethodMatches := False;

    for LAttribute in LMethod.GetAttributes do
    begin
      if LAttribute is PathAttribute then
        LMethodPath := PathAttribute(LAttribute).Value;

      if LAttribute is HttpMethodAttribute then
        LHttpMethodMatches := HttpMethodAttribute(LAttribute).Matches(Request);
    end;

    if LHttpMethodMatches then
    begin
      LPrototypeURL := TWiRLURL.CreateDummy([TWiRLEngine(Engine).BasePath, BasePath, LResourcePath, LMethodPath]);
      try
        LPathMatches := LPrototypeURL.MatchPath(URL);
      finally
        LPrototypeURL.Free;
      end;

      if LPathMatches and LHttpMethodMatches then
      begin
        Result := LMethod;
        Break;
      end;
    end;
  end;
end;

function TWiRLApplication.GetRequest: TWiRLRequest;
begin
  Result := TWiRLEngine(Engine).CurrentRequest;
end;

function TWiRLApplication.GetResponse: TWiRLResponse;
begin
  Result := TWiRLEngine(Engine).CurrentResponse;
end;

function TWiRLApplication.GetResources: TArray<string>;
begin
  Result := FResourceRegistry.Keys.ToArray;
end;

function TWiRLApplication.GetURL: TWiRLURL;
begin
  Result := TWiRLEngine(Engine).CurrentURL;
end;

function ExtractToken(const AString: string; const ATokenIndex: Integer; const ADelimiter: Char = '/'): string;
var
  LTokens: TArray<string>;
begin
  LTokens := TArray<string>(SplitString(AString, ADelimiter));

  Result := '';
  if ATokenIndex < Length(LTokens) then
    Result := LTokens[ATokenIndex]
  else
    raise EWiRLServerException.Create(
      Format('ExtractToken, index: %d from %s', [ATokenIndex, AString]), 'ExtractToken');
end;


function TWiRLApplication.FillAnnotatedParam(AParam: TRttiParameter;
  const AAttrArray: TAttributeArray; AResourceInstance: TObject;
  AMethod: TRttiMethod): TValue;
var
  LAttr: TCustomAttribute;
  LParamName, LParamValue: string;
  LParamIndex: Integer;
  LParamClassType: TClass;
  LContextValue: TValue;
begin
  if Length(AAttrArray) > 1 then
    raise EWiRLServerException.Create('Only 1 attribute per param permitted', Self.ClassName);

  LParamName := '';
  LParamValue := '';
  LAttr := AAttrArray[0];

  // context injection
  if (LAttr is ContextAttribute) and (AParam.ParamType.IsInstance) then
  begin
    LParamClassType := TRttiInstanceType(AParam.ParamType).MetaclassType;
    if ContextInjectionByType(LParamClassType, LContextValue) then
      Result := LContextValue;
  end
  else // http values injection
  begin
    if LAttr is PathParamAttribute then
    begin
      LParamName := (LAttr as PathParamAttribute).Value;
      if LParamName = '' then
        LParamName := AParam.Name;

      LParamIndex := ParamNameToParamIndex(AResourceInstance, LParamName, AMethod);
      LParamValue := URL.PathTokens[LParamIndex];
    end
    else
    if LAttr is QueryParamAttribute then
    begin
      LParamName := (LAttr as QueryParamAttribute).Value;
      if LParamName = '' then
        LParamName := AParam.Name;

      // Prendere il valore (come stringa) dalla lista QueryFields
      LParamValue := Request.QueryFields.Values[LParamName];
    end
    else
    if LAttr is FormParamAttribute then
    begin
      LParamName := (LAttr as FormParamAttribute).Value;
      if LParamName = '' then
        LParamName := AParam.Name;

      // Prendere il valore (come stringa) dalla lista ContentFields
      LParamValue := Request.ContentFields.Values[LParamName];
    end
    else
    if LAttr is CookieParamAttribute then
    begin
      LParamName := (LAttr as CookieParamAttribute).Value;
      if LParamName = '' then
        LParamName := AParam.Name;

      // Prendere il valore (come stringa) dalla lista CookieFields
      LParamValue := Request.CookieFields.Values[LParamName];
    end
    else
    if LAttr is HeaderParamAttribute then
    begin
      LParamName := (LAttr as HeaderParamAttribute).Value;
      if LParamName = '' then
        LParamName := AParam.Name;

      // Prendere il valore (come stringa) dagli Header HTTP
      LParamValue := string(Request.GetFieldByName(LParamName));
    end
    else
    if LAttr is BodyParamAttribute then
    begin
      LParamName := AParam.Name;
      LParamValue := Request.Content;
    end;

    case AParam.ParamType.TypeKind of
      tkInt64,
      tkInteger: Result := TValue.From(StrToInt(LParamValue));
      tkFloat: Result := TValue.From<Double>(StrToFloat(LParamValue));

      tkChar: Result := TValue.From(AnsiChar(LParamValue[1]));
      tkWChar: ;
      tkEnumeration: ;
      tkSet: ;
      tkClass:
      begin
        // TODO: better error handling in case of an invalid type cast
        // TODO: add a dynamic way to create the right class
        if MatchStr(AParam.ParamType.Name, ['TJSONObject']) then
          Result := TJSONObject.ParseJSONValue(LParamValue) as TJSONObject
        else if MatchStr(AParam.ParamType.Name, ['TJSONArray']) then
          Result := TJSONObject.ParseJSONValue(LParamValue) as TJSONArray
        else if MatchStr(AParam.ParamType.Name, ['TJSONValue']) then
          Result := TJSONObject.ParseJSONValue(LParamValue) as TJSONValue
        else
          raise EWiRLServerException.Create(Format('Unsupported class [%s] for param [%s]', [AParam.ParamType.Name, LParamName]), Self.ClassName);
      end;

      tkMethod: ;

      tkLString,
      tkUString,
      tkWString,
      tkString: Result := TValue.From(LParamValue);

      tkVariant: Result := TValue.From(LParamValue);

      tkArray: ;
      tkRecord: ;
      tkInterface: ;
      tkDynArray: ;
      tkClassRef: ;
      tkPointer: ;
      tkProcedure: ;
    else
      raise EWiRLServerException.Create(Format('Unsupported data type for param [%s]', [LParamName]), Self.ClassName);
    end;
  end;
end;

function TWiRLApplication.FillNonAnnotatedParam(AParam: TRttiParameter): TValue;
var
  LClass: TClass;
begin
  // 1) Valid objects (TWiRLRequest, )
  if AParam.ParamType.IsInstance then
  begin
    LClass := AParam.ParamType.AsInstance.MetaclassType;
    if LClass.InheritsFrom(TWiRLRequest) then
      Result := TValue.From(Request)
    else
      Result := TValue.From(nil);
      //Exception.Create('Only TWiRLRequest object if the method is not annotated');
  end
  else
  begin
    // 2) parameter default value
    Result := TValue.Empty;
  end;
end;

procedure TWiRLApplication.FillResourceMethodParameters(AInstance: TObject; AMethod: TRttiMethod; var AArgumentArray: TArgumentArray);
var
  LParam: TRttiParameter;
  LParamArray: TArray<TRttiParameter>;
  LAttrArray: TArray<TCustomAttribute>;

  LIndex: Integer;
begin
  try
    LParamArray := AMethod.GetParameters;

    // The method has no parameters so simply call as it is
    if Length(LParamArray) = 0 then
      Exit;

    SetLength(AArgumentArray, Length(LParamArray));

    for LIndex := Low(LParamArray) to High(LParamArray) do
    begin
      LParam := LParamArray[LIndex];

      LAttrArray := LParam.GetAttributes;

      if Length(LAttrArray) = 0 then
        AArgumentArray[LIndex] := FillNonAnnotatedParam(LParam)
      else
        AArgumentArray[LIndex] := FillAnnotatedParam(LParam, LAttrArray, AInstance, AMethod);
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

procedure TWiRLApplication.HandleRequest(ARequest: TWiRLRequest; AResponse: TWiRLResponse; const AURL: TWiRLURL);
begin
  FAuthContext := GetNewToken(ARequest);
  try
    InternalHandleRequest(ARequest, AResponse, AURL);
  finally
    FAuthContext.Free;
  end;
end;

procedure TWiRLApplication.GenerateToken;
begin
  FAuthContext.Generate(FSecret);
end;

function TWiRLApplication.GetNewToken(ARequest: TWiRLRequest): TWiRLAuthContext;
var
  LAuth: string;
  LAuthParts: TArray<string>;
begin
  if Assigned(FClaimClass) then
    Result := TWiRLAuthContext.Create(FClaimClass)
  else
    Result := TWiRLAuthContext.Create;

  LAuth := string(ARequest.Authorization);

  if LAuth = '' then
    Exit;

  LAuthParts := LAuth.Split([#32]);
  if Length(LAuthParts) < 2 then
    Exit;

  if SameText(LAuthParts[0], 'Bearer') then
    Result.Verify(LAuthParts[1], FSecret);
end;

procedure TWiRLApplication.InternalHandleRequest{(ARequest: TWiRLRequest;
  AResponse: TWiRLResponse; const AURL: TWiRLURL)};
var
  LInfo: TWiRLConstructorInfo;
  LMethod: TRttiMethod;
  LInstance: TObject;
  LWriter: IMessageBodyWriter;
  LMediaType: TMediaType;
begin
  if not FResourceRegistry.TryGetValue(URL.Resource, LInfo) then
    raise EWiRLNotFoundException.Create(
      Format('Resource [%s] not found', [URL.Resource]),
      Self.ClassName, 'HandleRequest'
    );

  LMethod := FindMethodToInvoke(URL, LInfo);

  if not Assigned(LMethod) then
    raise EWiRLNotFoundException.Create(
      Format('Resource''s method [%s] not found to handle resource [%s]', [ARequest.Method, URL.Resource + URL.SubResources.ToString]),
      Self.ClassName, 'HandleRequest'
    );

  CheckAuthorization(LMethod, FAuthContext);

  LInstance := LInfo.ConstructorFunc();
  try
    TWiRLMessageBodyRegistry.Instance.FindWriter(LMethod, string(Request.Accept),
      LWriter, LMediaType);

    ContextInjection(LInstance);
    try
      InvokeResourceMethod(LInstance, LMethod, LWriter, ARequest, LMediaType);
    finally
      LWriter := nil;
      LMediaType.Free;
    end;

  finally
    LInstance.Free;
  end;
end;

procedure TWiRLApplication.InvokeResourceMethod(AInstance: TObject;
  AMethod: TRttiMethod; const AWriter: IMessageBodyWriter;
  ARequest: TWiRLRequest; AMediaType: TMediaType);
var
  LMethodResult: TValue;
  LArgument: TValue;
  LArgumentArray: TArgumentArray;
  LStream: TMemoryStream;
  LContentType: string;
begin
  // The returned object MUST be initially nil (needs to be consistent with the Free method)
  LMethodResult := nil;
  try
    LContentType := Response.ContentType;
    FillResourceMethodParameters(AInstance, AMethod, LArgumentArray);
    LMethodResult := AMethod.Invoke(AInstance, LArgumentArray);

    if LMethodResult.IsInstanceOf(TWiRLResponse) then
    begin
      // Request is already done
    end
    else if Assigned(AWriter) then // MessageBodyWriters mechanism
    begin
      if Response.ContentType = LContentType then
        Response.ContentType := AMediaType.ToString;

      LStream := TMemoryStream.Create;
      try
        AWriter.WriteTo(LMethodResult, AMethod.GetAttributes, AMediaType, Response.CustomHeaders, LStream);
        LStream.Position := 0;
        Response.ContentStream := LStream;
      except
        on E: Exception do
        begin
          LStream.Free;
          raise EWiRLServerException.Create(E.Message, 'TWiRLApplication', 'InvokeResourceMethod');
        end;
      end;
    end
    else // fallback (no MBW, no TWiRLResponse)
    begin
      // handle result
      case LMethodResult.Kind of

        tkString, tkLString, tkUString, tkWString, // string types
        tkInteger, tkInt64, tkFloat, tkVariant:    // Treated as string, nothing more
        begin
          Response.Content := LMethodResult.AsString;
          if (Response.ContentType = LContentType) then
            Response.ContentType := TMediaType.TEXT_PLAIN; // or check Produces of method!
          Response.StatusCode := 200;
        end;

        tkUnknown : ; // it's a procedure, not a function!

        //tkRecord: ;
        //tkInterface: ;
        //tkDynArray: ;
        else
          raise EWiRLNotImplementedException.Create(
            'Resource''s returned type not supported',
            Self.ClassName, 'InvokeResourceMethod'
          );
      end;
    end;
  finally
    if (not TRttiHelper.HasAttribute<SingletonAttribute>(AMethod)) then
      CollectGarbage(LMethodResult);
    for LArgument in LArgumentArray do
      CollectGarbage(LArgument);
  end;
end;

function TWiRLApplication.ParamNameToParamIndex(AResourceInstance: TObject;
  const AParamName: string; AMethod: TRttiMethod): Integer;
var
  LParamIndex: Integer;
  LAttrib: TCustomAttribute;
  LSubResourcePath: string;
begin
  LParamIndex := -1;

  LSubResourcePath := '';
  for LAttrib in AMethod.GetAttributes do
  begin
    if LAttrib is PathAttribute then
    begin
      LSubResourcePath := PathAttribute(LAttrib).Value;
      Break;
    end;
  end;

  TRttiHelper.HasAttribute<PathAttribute>(FRttiContext.GetType(AResourceInstance.ClassType),
  procedure (AResourcePathAttrib: PathAttribute)
  var
    LResURL: TWiRLURL;
    LPair: TPair<Integer, string>;
  begin
    LResURL := TWiRLURL.CreateDummy([TWiRLEngine(Engine).BasePath, BasePath, AResourcePathAttrib.Value, LSubResourcePath]);
    try
      LParamIndex := -1;
      for LPair in LResURL.PathParams do
      begin
        if SameText(AParamName, LPair.Value) then
        begin
          LParamIndex := LPair.Key;
          Break;
        end;
      end;
    finally
      LResURL.Free;
    end;
  end);

  Result := LParamIndex;
end;

function TWiRLApplication.SetSecret(ASecretGen: TSecretGenerator): TWiRLApplication;
begin
  if Assigned(ASecretGen) then
    FSecret := ASecretGen;
  Result := Self;
end;

function TWiRLApplication.SetSystemApp(ASystem: Boolean): TWiRLApplication;
begin
  FSystemApp := ASystem;
  Result := Self;
end;

end.
