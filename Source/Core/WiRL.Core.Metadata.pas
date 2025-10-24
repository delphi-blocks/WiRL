{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2025 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Metadata;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes, System.SysUtils, System.Generics.Collections, System.Rtti,
  System.TypInfo,

  Neon.Core.Nullables,
  WiRL.http.Core,
  WiRL.http.Accept.MediaType,
  WiRL.Core.Attributes,
  WiRL.Core.Exceptions,
  WiRL.Core.Declarations,
  WiRL.Core.Registry;

type
  TWiRLProxyResource = class;

  TWiRLProxyBase = class
  protected
    FCode: string;
    FName: string;
    FSummary: string;
    FRemarks: string;
    FDescription: string;
    FProcessed: Boolean;
  public
    procedure Process(); virtual;
    procedure Reset(); virtual;

    property Code: string read FCode write FCode;
    property Name: string read FName write FName;
    property Summary: string read FSummary write FSummary;
    property Remarks: string read FRemarks write FRemarks;
    property Description: string read FDescription write FDescription;
  end;

  TWiRLProxyFilter = class(TWiRLProxyBase)
  private
    FAttribute: TCustomAttribute;
    FFilterType: TClass;
  public
    constructor Create(AAttribute: TCustomAttribute);
    procedure Process(); override;
  public
    property FilterType: TClass read FFilterType;
  end;

  TWiRLProxyFilters = class(TObjectList<TWiRLProxyFilter>);

  TWiRLProxyEntity = class(TWiRLProxyBase)
  private
    FRttiType: TRttiType;
  public
    constructor Create(AType: TRttiType);
    procedure Process(); override;
  public
    property RttiType: TRttiType read FRttiType write FRttiType;
  end;

  TWiRLProxyParameter = class(TWiRLProxyBase)
  private
    FRttiParam: TRttiParameter;
    FAttributes: TArray<TCustomAttribute>;
    FInjected: Boolean;
    FValue: string;
    FKind: TMethodParamType;
    FRest: Boolean;
    FContext: TRttiType;
    FEntity: TWiRLProxyEntity;
    FRequired: Boolean;
    procedure ProcessAttributes;
  public
    constructor Create(AParam: TRttiParameter);
    destructor Destroy; override;

    procedure Process(); override;
  public
    property Rest: Boolean read FRest write FRest;
    property Kind: TMethodParamType read FKind write FKind;
    property Value: string read FValue write FValue;
    property Injected: Boolean read FInjected write FInjected;
    property Entity: TWiRLProxyEntity read FEntity write FEntity;
    property Context: TRttiType read FContext write FContext;
    property Required: Boolean read FRequired write FRequired;

    property RttiParam: TRttiParameter read FRttiParam write FRttiParam;
    property Attributes: TArray<TCustomAttribute> read FAttributes write FAttributes;
  end;

  TWiRLProxyParameters = class(TObjectList<TWiRLProxyParameter>);

  TResponseType = (Content, Ref, RefSchema);
  TStatusCategory = (Unknown, Informational, Success, Redirection, ClientError, ServerError, Custom);
  TWiRLProxyMethodResponse = class
  private
    FCode: string;
    FRef: NullString;
    FDescription: NullString;
    FSchema: NullString;
    function GetHttpCode: Integer;
    function GetCategory: TStatusCategory;
    function GetResponseType: TResponseType;
  public
    property Code: string read FCode write FCode;
    property Ref: NullString read FRef write FRef;
    property Description: NullString read FDescription write FDescription;
    property Schema: NullString read FSchema write FSchema;

    property HttpCode: Integer read GetHttpCode;
    property Category: TStatusCategory read GetCategory;
    property ResponseType: TResponseType read GetResponseType;
  end;

  TWiRLProxyMethodResponses = class(TObjectList<TWiRLProxyMethodResponse>)
  public
    function Contains(ACategory: TStatusCategory): Boolean;

    function AddResponse(const ACode, ADescription: string): TWiRLProxyMethodResponse;
    function AddResponseRef(const ACode, ARef: string): TWiRLProxyMethodResponse;
    function AddResponseSchemaRef(const ACode, ASchema, ADescription: string): TWiRLProxyMethodResponse;
  end;

  TWiRLProxyMethodResult = class(TWiRLProxyBase)
  private
    FRttiType: TRttiType;
    FResultType: TTypeKind;
    FIsClass: Boolean;
    FIsRecord: Boolean;
    FIsSingleton: Boolean;
    FIsProcedure: Boolean;
    FIsFunction: Boolean;
    FIsArray: Boolean;
    FIsSimple: Boolean;
    FEntity: TWiRLProxyEntity;
  public
    constructor Create(AResultType: TRttiType);
    destructor Destroy; override;

    procedure Process(); override;
    procedure SetAsSingleton;
  public
    property IsProcedure: Boolean read FIsProcedure;
    property IsFunction: Boolean read FIsFunction;

    property ResultType: TTypeKind read FResultType;
    property IsClass: Boolean read FIsClass;
    property IsRecord: Boolean read FIsRecord;
    property IsArray: Boolean read FIsArray;
    property IsSimple: Boolean read FIsSimple;
    property IsSingleton: Boolean read FIsSingleton;
    property RttiType: TRttiType read FRttiType;
    property Entity: TWiRLProxyEntity read FEntity write FEntity;
  end;

  TWiRLProxyMethodAuth = class(TWiRLProxyBase)
  private
    FDenyAll: Boolean;
    FRoles: TStringArray;
    FPermitAll: Boolean;
    FHasAuth: Boolean;
  public
    procedure Process(); override;

    procedure SetPermitAll;
    procedure SetDenyAll;
    procedure SetRoles(ARoles: TStrings);
  public
    property HasAuth: Boolean read FHasAuth;
    property DenyAll: Boolean read FDenyAll;
    property PermitAll: Boolean read FPermitAll;
    property Roles: TStringArray read FRoles;
  end;

  TWiRLProxyMethod = class(TWiRLProxyBase)
  private
    FResource: TWiRLProxyResource;
    FRttiMethod: TRttiMethod;
    FHttpVerb: string;
    FPath: string;
    FConsumes: TMediaTypeList;
    FProduces: TMediaTypeList;
    FMethodResult: TWiRLProxyMethodResult;
    FAsync: Boolean;
    FRest: Boolean;
    FIsFunction: Boolean;
    FAuth: TWiRLProxyMethodAuth;
    FFilters: TWiRLProxyFilters;
    FAllAttributes: TArray<TCustomAttribute>;
    FStatus: TWiRLHttpStatus;
    FParams: TWiRLProxyParameters;
    FAuthHandler: Boolean;
    FResponses: TWiRLProxyMethodResponses;

    procedure ProcessMethodResult;
    procedure ProcessAttributes;
    procedure ProcessParams;
  public
    constructor Create(AResource: TWiRLProxyResource; ARttiMethod: TRttiMethod);
    destructor Destroy; override;

    procedure Process(); override;

    function NewParam(AParam: TRttiParameter): TWiRLProxyParameter;

    function HasFilter(AAttribute: TCustomAttribute): Boolean;
  public
    property Rest: Boolean read FRest;
    property Path: string read FPath;
    property Async: Boolean read FAsync;
    property Auth: TWiRLProxyMethodAuth read FAuth;
    property AuthHandler: Boolean read FAuthHandler;
    property IsFunction: Boolean read FIsFunction;
    property HttpVerb: string read FHttpVerb write FHttpVerb;
    property MethodResult: TWiRLProxyMethodResult read FMethodResult;
    property Consumes: TMediaTypeList read FConsumes;
    property Produces: TMediaTypeList read FProduces;
    property Filters: TWiRLProxyFilters read FFilters;
    property Status: TWiRLHttpStatus read FStatus write FStatus;
    property Params: TWiRLProxyParameters read FParams write FParams;
    property Responses: TWiRLProxyMethodResponses read FResponses write FResponses;

    property AllAttributes: TArray<TCustomAttribute> read FAllAttributes;
    property RttiObject: TRttiMethod read FRttiMethod;
  end;

  TWiRLProxyMethods = class(TObjectList<TWiRLProxyMethod>);

  TWiRLProxyAuthType = (None, Unknown, Basic, Cookie);

  TWiRLProxyAuth = class
  private
    FAuthType: TWiRLProxyAuthType;
    FHeaderName: string;
  public
    property AuthType: TWiRLProxyAuthType read FAuthType write FAuthType;
    property HeaderName: string read FHeaderName write FHeaderName;
  end;

  TWiRLProxyResource = class(TWiRLProxyBase)
  private
    FContext: TWiRLResourceRegistry;
    FResourceClass: TClass;
    FConstructor: TWiRLConstructorProxy;
    FPath: string;
    FAuth: TWiRLProxyAuth;
    FRttiType: TRttiType;
    FMethods: TWiRLProxyMethods;
    FProduces: TMediaTypeList;
    FConsumes: TMediaTypeList;
    FFilters: TWiRLProxyFilters;

    procedure ProcessAttributes;
    procedure ProcessMethods;
  public
    constructor Create(const AName: string; AContext: TWiRLResourceRegistry);
    destructor Destroy; override;
  public
    procedure Process(); override;

    function CreateInstance: TObject;

    function MatchProduces(AMethod: TWiRLProxyMethod; AMediaType: TMediaType): Boolean;
    function MatchConsumes(AMethod: TWiRLProxyMethod; AMediaType: TMediaType): Boolean;
    function IsSwagger(const ASwaggerResource: string): Boolean;
    function NewMethod(AMethod: TRttiMethod; const AVerb: string): TWiRLProxyMethod;
    function GetSanitizedPath: string;
  public
    property Path: string read FPath;
    property Auth: TWiRLProxyAuth read FAuth write FAuth;
    property Methods: TWiRLProxyMethods read FMethods;
    property Produces: TMediaTypeList read FProduces;
    property Consumes: TMediaTypeList read FConsumes;
    property Filters: TWiRLProxyFilters read FFilters;

    // Rtti-based properties (to be removed)
    // Introduce: ClassName, UnitName
    property ResourceClass: TClass read FResourceClass write FResourceClass;
    property RttiType: TRttiType read FRttiType;
  end;

  //TWiRLProxyResources = class(TObjectList<TWiRLProxyResource>);
  TWiRLProxyResources = class(TObjectDictionary<string, TWiRLProxyResource>);

  TWiRLProxyApplication = class(TWiRLProxyBase)
  private
    FContext: TWiRLResourceRegistry;
    FResources: TWiRLProxyResources;
  public
    constructor Create(AContext: TWiRLResourceRegistry);
    destructor Destroy; override;
    procedure ProcessResources;
  public
    procedure Process(); override;
    procedure Reset(); override;
    function NewResource(const AName: string): TWiRLProxyResource;
    function GetResource(const AName: string): TWiRLProxyResource;

    property Resources: TWiRLProxyResources read FResources write FResources;
  end;

implementation

uses
  WiRL.Core.Auth.Resource,
  WiRL.http.URL,
  WiRL.Rtti.Utils;


function IsFilter(AAttribute: TCustomAttribute): Boolean;
begin
  Result := False;

  // If the Attribute is NameBinding
  if AAttribute is NameBindingAttribute then
    Result := True;

  // If the Attribute has a NameBinding attribute
  if not Result then
    if TRttiHelper.HasAttribute<NameBindingAttribute>(
      TRttiHelper.Context.GetType(AAttribute.ClassType)) then
      Result := True;
end;

{ TWiRLProxyResource }

constructor TWiRLProxyResource.Create(const AName: string; AContext: TWiRLResourceRegistry);
begin
  FName := AName;
  FContext := AContext;

  FAuth := TWiRLProxyAuth.Create;
  FMethods := TWiRLProxyMethods.Create(True);
  FFilters := TWiRLProxyFilters.Create(True);
  FProduces := TMediaTypeList.Create;
  FConsumes := TMediaTypeList.Create;

  FContext.TryGetValue(AName, FConstructor);
  if not Assigned(FConstructor) then
    EWiRLServerException.CreateFmt('Resource [%s] not found', [AName]);

  FResourceClass := FConstructor.TypeTClass;

  FRttiType := TRttiHelper.Context.GetType(FResourceClass);

  // If a Resource inherits from Add TWiRLAuth* add a SecurityDefinition
  if FResourceClass.InheritsFrom(TWiRLAuthBasicResource) then
    FAuth.AuthType := TWiRLProxyAuthType.Basic;
end;

destructor TWiRLProxyResource.Destroy;
begin
  FFilters.Free;
  FMethods.Free;
  FProduces.Free;
  FConsumes.Free;
  FAuth.Free;

  inherited;
end;

function TWiRLProxyResource.CreateInstance: TObject;
begin
  Result := FConstructor.ConstructorFunc();
end;

function TWiRLProxyResource.GetSanitizedPath: string;
begin
  Result := Path.Trim(['/']);
end;

function TWiRLProxyResource.IsSwagger(const ASwaggerResource: string): Boolean;
begin
  if SameText(FName.Trim(['/']), ASwaggerResource.Trim(['/'])) then
    Result := True
  else
    Result := False;
end;

function TWiRLProxyResource.MatchConsumes(AMethod: TWiRLProxyMethod; AMediaType: TMediaType): Boolean;
begin
  Result := False;

  if AMethod.Consumes.Empty then
    Exit(True);

  if AMethod.Consumes.IsWildCard then
    Exit(True);

  if AMethod.Consumes.Contains(AMediaType) then
    Exit(True);
end;

function TWiRLProxyResource.MatchProduces(AMethod: TWiRLProxyMethod; AMediaType: TMediaType): Boolean;
begin
  Result := False;

  if AMethod.Produces.Empty or AMediaType.IsWildcard then
    Exit(True);

  // It's a procedure, so no "Produces" mechanism
  if not AMethod.IsFunction then
    Exit(True);

  // Tries to match the Produces MediaType
  if AMethod.Produces.Contains(AMediaType) then
    Exit(True);


  // If the method result it's an object there is no Produces let the MBWs choose the output
  if AMethod.Produces.IsWildCard and (AMethod.MethodResult.IsClass or AMethod.MethodResult.IsRecord) then
    Exit(True);
end;

function TWiRLProxyResource.NewMethod(AMethod: TRttiMethod; const AVerb: string): TWiRLProxyMethod;
begin
  Result := TWiRLProxyMethod.Create(Self, AMethod);
  Result.HttpVerb := AVerb;
  FMethods.Add(Result);
end;

procedure TWiRLProxyResource.Process;
begin
  inherited;

  FCode := FResourceClass.ClassName;

  ProcessAttributes;
  ProcessMethods;
  //FSummary := FindReadXMLDoc();

  FProcessed := True;
end;

procedure TWiRLProxyResource.ProcessAttributes;
var
  LAttribute: TCustomAttribute;
  LMediaList: TArray<string>;
  LMedia: string;
begin
  // Global loop to retrieve and process ALL attributes at once
  for LAttribute in FRttiType.GetAttributes do
  begin
    // Path Attribute
    if LAttribute is PathAttribute then
      FPath := PathAttribute(LAttribute).Value

    // Consumes Attribute
    else if LAttribute is ConsumesAttribute then
    begin
      LMediaList := ConsumesAttribute(LAttribute).Value.Split([',']);

      for LMedia in LMediaList do
        FConsumes.Add(TMediaType.Create(LMedia));
    end

    // Produces Attribute
    else if LAttribute is ProducesAttribute then
    begin
      LMediaList := ProducesAttribute(LAttribute).Value.Split([',']);

      for LMedia in LMediaList do
        FProduces.Add(TMediaType.Create(LMedia));
    end

    // Filters
    else if IsFilter(LAttribute) then
    begin
      FFilters.Add(TWiRLProxyFilter.Create(LAttribute));
    end

    // BasicAuth handler
    else if LAttribute is BasicAuthAttribute then
      FAuth.AuthType := TWiRLProxyAuthType.Basic

    // CookieAuth handler
    else if LAttribute is CookieAuthAttribute then
    begin
      FAuth.AuthType := TWiRLProxyAuthType.Cookie;
      FAuth.HeaderName := (LAttribute as CookieAuthAttribute).CookieName;
    end
  end;
end;

procedure TWiRLProxyResource.ProcessMethods;
var
  LResourceMethod: TRttiMethod;
  LHttpVerb: string;
  LResMethod: TWiRLProxyMethod;
begin
  // Loop on every method of the current resource object
  for LResourceMethod in FRttiType.GetMethods do
  begin

    LHttpVerb := '';
    TRttiHelper.HasAttribute<HttpMethodAttribute>(LResourceMethod,
      procedure (AAttr: HttpMethodAttribute)
      begin
        LHttpVerb := AAttr.ToString.ToLower;
      end
    );

    // This method is a REST handler
    if not LHttpVerb.IsEmpty then
    begin
      LResMethod := NewMethod(LResourceMethod, LHttpverb);
      LResMethod.Process();
    end;
  end;
end;

{ TWiRLProxyMethod }

constructor TWiRLProxyMethod.Create(AResource: TWiRLProxyResource; ARttiMethod: TRttiMethod);
begin
  FResource := AResource;
  FRttiMethod := ARttiMethod;
  FConsumes := TMediaTypeList.Create;
  FProduces := TMediaTypeList.Create;
  FFilters := TWiRLProxyFilters.Create(True);
  FAuth := TWiRLProxyMethodAuth.Create;
  FStatus := TWiRLHttpStatus.Create;
  FParams := TWiRLProxyParameters.Create(True);
  FResponses := TWiRLProxyMethodResponses.Create(True);
  FMethodResult := TWiRLProxyMethodResult.Create(FRttiMethod.ReturnType);
  FName := FRttiMethod.Name;
  FIsFunction := Assigned(FRttiMethod.ReturnType);
end;

destructor TWiRLProxyMethod.Destroy;
begin
  FResponses.Free;
  FParams.Free;
  FStatus.Free;
  FAuth.Free;
  FFilters.Free;
  FMethodResult.Free;
  FConsumes.Free;
  FProduces.Free;
  inherited;
end;

function TWiRLProxyMethod.HasFilter(AAttribute: TCustomAttribute): Boolean;
var
  LFilter: TWiRLProxyFilter;
begin
  // Any non decorated filter should be used
  if not Assigned(AAttribute) then
    Exit(True);
  Result := False;
  for LFilter in FFilters do
  begin
    if AAttribute is LFilter.FilterType then
    begin
      Result := True;
      Break;
    end;
  end;

  if not Result then
    for LFilter in FResource.Filters do
    begin
      if AAttribute is LFilter.FilterType then
      begin
        Result := True;
        Break;
      end;
    end;

end;

function TWiRLProxyMethod.NewParam(AParam: TRttiParameter): TWiRLProxyParameter;
begin
  Result := TWiRLProxyParameter.Create(AParam);
  FParams.Add(Result);
end;

procedure TWiRLProxyMethod.Process;
begin
  inherited;
  FCode := FRttiMethod.Name;

  ProcessAttributes();
  ProcessParams();
  ProcessMethodResult();

  FProcessed := True;
end;

procedure TWiRLProxyMethod.ProcessAttributes;
var
  LAttribute: TCustomAttribute;
  LStatus: ResponseStatusAttribute;
  LMediaList: TArray<string>;
  LMedia: string;
begin
  FRest := False;

  // Global loop to retrieve and process all attributes at once
  for LAttribute in FRttiMethod.GetAttributes do
  begin
    // Add the attribute in the AllAttribute array
    FAllAttributes := FAllAttributes + [LAttribute];

    // Method HTTP Method
    if LAttribute is HttpMethodAttribute then
    begin
      FHttpVerb := HttpMethodAttribute(LAttribute).ToString;
      FRest := True;
    end

    // Method Path
    else if LAttribute is PathAttribute then
      FPath := PathAttribute(LAttribute).Value

    // Method is Async
    else if LAttribute is AsyncResponseAttribute then
      FAsync := True

    // Method Result is Singleton
    else if LAttribute is SingletonAttribute then
      FMethodResult.SetAsSingleton

    // Method Authorization
    else if LAttribute is RolesAllowedAttribute then
      FAuth.SetRoles(RolesAllowedAttribute(LAttribute).Roles)
    else if LAttribute is PermitAllAttribute then
      FAuth.SetPermitAll
    else if LAttribute is DenyAllAttribute then
      FAuth.SetDenyAll

    // Method that handles Authorization (via CustomAttribute)
    else if LAttribute is BasicAuthAttribute then
      FAuthHandler := True

    // Method Consumes
    else if LAttribute is ConsumesAttribute then
    begin
      LMediaList := ConsumesAttribute(LAttribute).Value.Split([',']);

      for LMedia in LMediaList do
        FConsumes.Add(TMediaType.Create(LMedia));
    end

    // Method Produces
    else if LAttribute is ProducesAttribute then
    begin
      LMediaList := ProducesAttribute(LAttribute).Value.Split([',']);

      for LMedia in LMediaList do
        FProduces.Add(TMediaType.Create(LMedia));
    end

    // Filters
    else if IsFilter(LAttribute) then
    begin
      FFilters.Add(TWiRLProxyFilter.Create(LAttribute));
    end

    // ResponseRedirection
    else if LAttribute is ResponseRedirectionAttribute then
    begin
      LStatus := (LAttribute as ResponseStatusAttribute);
      FStatus.Code := LStatus.Code;
      FStatus.Reason := LStatus.Reason;
      FStatus.Location := (LStatus as ResponseRedirectionAttribute).Location;
    end

    // ResponseStatus
    else if LAttribute is ResponseStatusAttribute then
    begin
      LStatus := (LAttribute as ResponseStatusAttribute);
      FStatus.Code := LStatus.Code;
      FStatus.Reason := LStatus.Reason;
    end

  end;
end;

procedure TWiRLProxyMethod.ProcessMethodResult;
begin
  FMethodResult.Process();
end;

procedure TWiRLProxyMethod.ProcessParams;
var
  LParam: TRttiParameter;
  LWiRLParameter: TWiRLProxyParameter;
begin
  for LParam in FRttiMethod.GetParameters do
  begin
    LWiRLParameter := NewParam(LParam);
    LWiRLParameter.Process();
    if not LWiRLParameter.Rest then
      raise EWiRLServerException.CreateFmt(
        'Non annotated params [%s] are not allowed. Method-> [%s.%s]',
        [LWiRLParameter.Name, FResource.ResourceClass.ClassName, FName]
      );
  end;
end;

{ TWiRLProxyMethodResult }

constructor TWiRLProxyMethodResult.Create(AResultType: TRttiType);
begin
  FRttiType := AResultType;
end;

destructor TWiRLProxyMethodResult.Destroy;
begin
  FEntity.Free;

  inherited;
end;

procedure TWiRLProxyMethodResult.Process;
begin
  inherited;

  if Assigned(FRttiType) then
  begin
    FIsFunction := True;
    FResultType := FRttiType.TypeKind;
    case FResultType of
      tkClass:
      begin
        FIsClass := True;
        FEntity := TWiRLProxyEntity.Create(FRttiType);
        FEntity.Process();
      end;

      tkRecord:
      begin
        FIsRecord := True;
        FEntity := TWiRLProxyEntity.Create(FRttiType);
        FEntity.Process();
      end;

      tkArray, tkDynArray:
      begin
        { TODO -opaolo -c : Finire TArray<TPet> 07/10/2025 13:49:17 }
        FIsArray := True;
      end
    else
      FIsSimple := True;
    end;
  end
  else
    FIsProcedure := True;

  FProcessed := True;
end;

procedure TWiRLProxyMethodResult.SetAsSingleton;
begin
  FIsSingleton := True;
end;

procedure TWiRLProxyMethodAuth.Process;
begin
  inherited;

  FProcessed := True;
end;

procedure TWiRLProxyMethodAuth.SetDenyAll;
begin
  FHasAuth := True;
  FDenyAll := True;
  FPermitAll := False;
end;

procedure TWiRLProxyMethodAuth.SetPermitAll;
begin
  FHasAuth := True;
  FDenyAll := False;
  FPermitAll := True;
end;

procedure TWiRLProxyMethodAuth.SetRoles(ARoles: TStrings);
begin
  if ARoles.Count > 0 then
  begin
    FHasAuth := True;
    FDenyAll := False;
    FPermitAll := False;
    FRoles := ARoles.ToStringArray;
  end
  else
  begin
    FHasAuth := False;
    FRoles := [];
  end;
end;

{ TWiRLProxyFilter }

constructor TWiRLProxyFilter.Create(AAttribute: TCustomAttribute);
begin
  FAttribute := AAttribute;
  FFilterType := FAttribute.ClassType;
end;

procedure TWiRLProxyFilter.Process;
begin
  inherited;

  FProcessed := True;
end;

{ TWiRLProxyParameter }

constructor TWiRLProxyParameter.Create(AParam: TRttiParameter);
begin
  FRttiParam := AParam;
  FName := FRttiParam.Name;
end;

destructor TWiRLProxyParameter.Destroy;
begin
  FEntity.Free;

  inherited;
end;

procedure TWiRLProxyParameter.Process;
begin
  inherited;

  FCode := FRttiParam.Name;
  ProcessAttributes;
  FProcessed := True;
end;

procedure TWiRLProxyParameter.ProcessAttributes;
var
  LAttr: TCustomAttribute;
begin
  FAttributes := FRttiParam.GetAttributes;

  for LAttr in FAttributes do
  begin
    // Loop only inside attributes that define how to read the parameter
    if not ( (LAttr is ContextAttribute) or (LAttr is MethodParamAttribute) ) then
      Continue;

    FRest := True;

    // context injection
    if (LAttr is ContextAttribute) and (FRttiParam.ParamType.IsInstance) then
    begin
      FInjected := True;
      Continue;
      //if ContextInjectionByType(FParam, LContextValue) then
        //Exit(LContextValue);
    end;

    // Param Kind
    if LAttr is PathParamAttribute then
      Kind := TMethodParamType.Path
    else if LAttr is QueryParamAttribute then
      Kind := TMethodParamType.Query
    else if LAttr is FormParamAttribute then
      Kind := TMethodParamType.Form
    else if LAttr is HeaderParamAttribute then
      Kind := TMethodParamType.Header
    else if LAttr is CookieParamAttribute then
      Kind := TMethodParamType.Cookie
    else if LAttr is BodyParamAttribute then
    begin
      Kind := TMethodParamType.Body;

      FEntity := TWiRLProxyEntity.Create(FRttiParam.ParamType);
      FEntity.Process();
    end
    else if LAttr is FormDataParamAttribute then
      Kind := TMethodParamType.FormData
    else if LAttr is MultipartAttribute then
      Kind := TMethodParamType.MultiPart;

    // Param Name
    FName := (LAttr as MethodParamAttribute).Value;
    if (FName = '') or (LAttr is BodyParamAttribute) then
      FName := FRttiParam.Name;
  end;
end;

{ TWiRLProxyApplication }

constructor TWiRLProxyApplication.Create(AContext: TWiRLResourceRegistry);
begin
  FResources := TWiRLProxyResources.Create([doOwnsValues]);
  FContext := AContext;
end;

destructor TWiRLProxyApplication.Destroy;
begin
  FResources.Free;
  inherited;
end;

function TWiRLProxyApplication.GetResource(const AName: string): TWiRLProxyResource;
begin
  if not FResources.TryGetValue(AName, Result) then
    raise EWiRLNotFoundException.CreateFmt('Resource [%s] not found', [AName]);
end;

function TWiRLProxyApplication.NewResource(const AName: string): TWiRLProxyResource;
begin
  Result := TWiRLProxyResource.Create(AName, FContext);
  FResources.Add(AName, Result);
end;

procedure TWiRLProxyApplication.Process;
begin
  inherited;

  ProcessResources;
  FProcessed := True;
end;

procedure TWiRLProxyApplication.Reset;
begin
  inherited;

  FResources.Clear;
end;

procedure TWiRLProxyApplication.ProcessResources;
var
  LResourceName: string;
  LResource: TWiRLProxyResource;
begin
  // Loop on every resource of the application
  for LResourceName in FContext.Keys do
  begin
    LResource := NewResource(LResourceName);
    LResource.Process();
  end;
end;

{ TWiRLProxyBase }

procedure TWiRLProxyBase.Process;
begin
  if FProcessed then
    raise EWiRLServerException.Create(Self.ClassName + ' already processed');
end;

procedure TWiRLProxyBase.Reset;
begin
  FProcessed := False;
end;

{ TWiRLProxyMethodResponses }

function TWiRLProxyMethodResponses.AddResponse(const ACode, ADescription: string): TWiRLProxyMethodResponse;
begin
  if ACode.IsEmpty then
    raise EWiRLWebApplicationException.Create('OpenAPI: Code cannot be empty');

  Result := TWiRLProxyMethodResponse.Create;
  Result.Code := ACode;
  Result.Description := ADescription;
  Self.Add(Result);
end;

function TWiRLProxyMethodResponses.AddResponseRef(const ACode, ARef: string): TWiRLProxyMethodResponse;
begin
  if ACode.IsEmpty then
    raise EWiRLWebApplicationException.Create('OpenAPI: Code cannot be empty');

  Result := TWiRLProxyMethodResponse.Create;
  Result.Code := ACode;
  Result.Ref := ARef;
  Self.Add(Result);
end;

function TWiRLProxyMethodResponses.AddResponseSchemaRef(const ACode, ASchema, ADescription: string): TWiRLProxyMethodResponse;
begin
  if ACode.IsEmpty then
    raise EWiRLWebApplicationException.Create('OpenAPI: Code cannot be empty');

  Result := TWiRLProxyMethodResponse.Create;
  Result.Code := ACode;
  Result.Description := ADescription;
  Result.Schema := ASchema;
  Self.Add(Result);
end;

function TWiRLProxyMethodResponses.Contains(ACategory: TStatusCategory): Boolean;
var
  LRes: TWiRLProxyMethodResponse;
begin
  Result := False;
  for LRes in Self do
    if LRes.Category = ACategory then
      Exit(True);
end;

{ TWiRLProxyEntity }

constructor TWiRLProxyEntity.Create(AType: TRttiType);
begin
  FRttiType := AType;
end;

procedure TWiRLProxyEntity.Process;
begin
  inherited;
  FCode := FRttiType.Name;
  FName := FRttiType.Name;

  //ProcessAttributes;
  //FSummary := FindReadXMLDoc();

  FProcessed := True;
end;

{ TWiRLProxyMethodResponse }

function TWiRLProxyMethodResponse.GetCategory: TStatusCategory;
var
  LHundreds: UInt8;
begin
  if FCode.IsEmpty then
    Exit(TStatusCategory.Unknown);

  LHundreds := StrToIntDef(FCode.Chars[0], 0);
  case LHundreds of
    1: Result := TStatusCategory.Informational;
    2: Result := TStatusCategory.Success;
    3: Result := TStatusCategory.Redirection;
    4: Result := TStatusCategory.ClientError;
    5: Result := TStatusCategory.ServerError;
    else
      Result := TStatusCategory.Custom;
  end;
end;

function TWiRLProxyMethodResponse.GetHttpCode: Integer;
begin
  Result := StrToIntDef(FCode, 0);
end;

function TWiRLProxyMethodResponse.GetResponseType: TResponseType;
begin
  if FRef.HasValue then
    Exit(TResponseType.Ref);

  if FSchema.HasValue then
    Exit(TResponseType.RefSchema);

  Result := TResponseType.Content;
end;

end.
