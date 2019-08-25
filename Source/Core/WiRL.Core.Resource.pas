{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Resource;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, System.Rtti,
  System.TypInfo,

  WiRL.http.Core,
  WiRL.http.Accept.MediaType,
  WiRL.Core.Attributes,
  WiRL.Core.Exceptions,
  WiRL.Core.Declarations,
  WiRL.Core.Context,
  WiRL.Core.Registry;

type
  TWiRLResource = class;

  TWiRLFilter = class
  private
    FAttribute: TCustomAttribute;
    //FFilter: TWiRLFilterConstructorInfo;
    FFilterType: TClass;
  public
    constructor Create(AAttribute: TCustomAttribute);
  public
    property FilterType: TClass read FFilterType;
  end;

  TWiRLFilterList = class(TObjectList<TWiRLFilter>)
  end;

  TWiRLMethodParam = class
  private
    FParam: TRttiParameter;
    FAttributes: TArray<TCustomAttribute>;
    FInjected: Boolean;
    FValue: string;
    FKind: TMethodParamType;
    FName: string;
    FRest: Boolean;
  public
    constructor Create(AParam: TRttiParameter);
    procedure ProcessAttributes;
  public
    property Rest: Boolean read FRest write FRest;
    property Name: string read FName write FName;
    property Kind: TMethodParamType read FKind write FKind;
    property Value: string read FValue write FValue;
    property Injected: Boolean read FInjected write FInjected;

    property RttiParam: TRttiParameter read FParam write FParam;
    property Attributes: TArray<TCustomAttribute> read FAttributes write FAttributes;
  end;

  TWiRLMethodParamList = class(TObjectList<TWiRLMethodParam>)
  end;

  TWiRLMethodResult = class
  private
    FRttiObject: TRttiType;
    FResultType: TTypeKind;
    FIsClass: Boolean;
    FIsRecord: Boolean;
    FIsSingleton: Boolean;
  public
    constructor Create(AResultType: TRttiType);
    procedure SetAsSingleton;
  public
    property ResultType: TTypeKind read FResultType;
    property IsClass: Boolean read FIsClass;
    property IsRecord: Boolean read FIsRecord;
    property IsSingleton: Boolean read FIsSingleton;
  end;

  TWiRLMethodAuthorization = class
  private
    FDenyAll: Boolean;
    FRoles: TStringArray;
    FPermitAll: Boolean;
    FHasAuth: Boolean;
  public
    procedure SetPermitAll;
    procedure SetDenyAll;
    procedure SetRoles(ARoles: TStrings);
  public
    property HasAuth: Boolean read FHasAuth;
    property DenyAll: Boolean read FDenyAll;
    property PermitAll: Boolean read FPermitAll;
    property Roles: TStringArray read FRoles;
  end;

  TWiRLResourceMethod = class
  private
    FResource: TWiRLResource;
    FRttiMethod: TRttiMethod;
    FHttpMethod: string;
    FPath: string;
    FConsumes: TMediaTypeList;
    FProduces: TMediaTypeList;
    FMethodResult: TWiRLMethodResult;
    FAsync: Boolean;
    FRest: Boolean;
    FIsFunction: Boolean;
    FAuth: TWiRLMethodAuthorization;
    FFilters: TWiRLFilterList;
    FAllAttributes: TArray<TCustomAttribute>;
    FStatus: TWiRLHttpStatus;
    FParams: TWiRLMethodParamList;

    procedure ProcessAttributes;
    procedure ProcessParams;
  public
    constructor Create(AResource: TWiRLResource; ARttiMethod: TRttiMethod);
    destructor Destroy; override;

    function HasFilter(AAttribute: TCustomAttribute): Boolean;
  public
    property Rest: Boolean read FRest;
    property Path: string read FPath;
    property Async: Boolean read FAsync;
    property Auth: TWiRLMethodAuthorization read FAuth;
    property IsFunction: Boolean read FIsFunction;
    property HttpMethod: string read FHttpMethod;
    property MethodResult: TWiRLMethodResult read FMethodResult;
    property Consumes: TMediaTypeList read FConsumes;
    property Produces: TMediaTypeList read FProduces;
    property Filters: TWiRLFilterList read FFilters;
    property Status: TWiRLHttpStatus read FStatus write FStatus;
    property Params: TWiRLMethodParamList read FParams write FParams;
    property AllAttributes: TArray<TCustomAttribute> read FAllAttributes;
    property RttiObject: TRttiMethod read FRttiMethod;
  end;

  TWiRLResourceMethodList = class(TObjectList<TWiRLResourceMethod>)

  end;

  TWiRLResource = class
  private
    FContext: TWiRLContext;
    FAppPath: string;
    FEnginePath: string;
    FRttiType: TRttiType;
    FInfo: TWiRLConstructorInfo;

    FMethod: TWiRLResourceMethod;
    FMethods: TWiRLResourceMethodList;
    FFound: Boolean;
    FPath: string;
    FProduces: TMediaTypeList;
    FConsumes: TMediaTypeList;
    FTypeClass: TClass;
    FFilters: TWiRLFilterList;
    procedure ProcessResource(AInfo: TWiRLConstructorInfo);

    procedure ProcessAttributes;
    procedure ProcessMethods;

    function MatchProduces(AMethod: TWiRLResourceMethod; AMediaType: TMediaType): Boolean;
    function MatchConsumes(AMethod: TWiRLResourceMethod; AMediaType: TMediaType): Boolean;
  public
    constructor Create(AContext: TWiRLContext);
    destructor Destroy; override;

    function CreateInstance: TObject;
    function GetResourceMethod: TWiRLResourceMethod;
    function GetRequestMethod(AContext: TWiRLContext; const ARequestedPath: string): TWiRLResourceMethod;
  public
    property Path: string read FPath;
    property Methods: TWiRLResourceMethodList read FMethods;
    property Produces: TMediaTypeList read FProduces;
    property Consumes: TMediaTypeList read FConsumes;
    property Filters: TWiRLFilterList read FFilters;
    property Found: Boolean read FFound write FFound;

    { TODO -opaolo -c : Remove if we chache the structure 20/01/2017 17:52:40 }
    property Method: TWiRLResourceMethod read FMethod;

    // Rtti-based properties (to be removed)
    property TypeClass: TClass read FTypeClass;
    property RttiObject: TRttiType read FRttiType;
  end;

implementation

uses
  WiRL.http.URL,
  WiRL.Rtti.Utils,
  WiRL.Core.Engine,
  WiRL.Core.Application;


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

{ TWiRLResource }

constructor TWiRLResource.Create(AContext: TWiRLContext);
var
  LApp: TWiRLApplication;
begin
  FContext := AContext;

  FMethods := TWiRLResourceMethodList.Create(True);
  FFilters := TWiRLFilterList.Create(True);

  LApp := (FContext.Application as TWiRLApplication);

  FEnginePath := (FContext.Engine as TWiRLEngine).BasePath;
  FAppPath := LApp.BasePath;
  FInfo := LApp.GetResourceInfo(FContext.RequestURL.Resource);

  ProcessResource(FInfo);

  FMethod := GetResourceMethod;
end;

destructor TWiRLResource.Destroy;
begin
  FFilters.Free;
  FMethods.Free;
  inherited;
end;

function TWiRLResource.CreateInstance: TObject;
begin
  Result := FInfo.ConstructorFunc();
end;

function TWiRLResource.GetRequestMethod(AContext: TWiRLContext;
  const ARequestedPath: string): TWiRLResourceMethod;
begin
  Result := nil;
end;

function TWiRLResource.GetResourceMethod: TWiRLResourceMethod;
var
  LConsumesMatch: Boolean;
  LMethod: TWiRLResourceMethod;
  LPrototypeURL: TWiRLURL;
  LPathMatches,
  LProducesMatch,
  LHttpMethodMatches: Boolean;
  LMedia: TMediaType;
begin
  Result := nil;

  for LMedia in FContext.Request.AcceptableMediaTypes do
  begin

    for LMethod in FMethods do
    begin
      // Skip the non-REST methods (no GET/POST/PUT methods)
      if not LMethod.Rest then
        Continue;

      LHttpMethodMatches := LMethod.HttpMethod = FContext.Request.Method;

      if not LHttpMethodMatches then
        Continue;

      LPrototypeURL := TWiRLURL.MockURL(FEnginePath, FAppPath, FPath, LMethod.Path);
      try
        LPathMatches := LPrototypeURL.MatchPath(FContext.RequestURL);
      finally
        LPrototypeURL.Free;
      end;

      if not LPathMatches then
        Continue;

      LProducesMatch := MatchProduces(LMethod, LMedia);
      LConsumesMatch := MatchConsumes(LMethod, FContext.Request.ContentMediaType);

      if LProducesMatch and LConsumesMatch then
      begin
        Result := LMethod;
        Break;
      end;
    end;

    // Already found for the first MediaType, no further search
    if Assigned(Result) then
      Break;
  end;
end;

function TWiRLResource.MatchConsumes(AMethod: TWiRLResourceMethod; AMediaType: TMediaType): Boolean;
begin
  Result := False;

  if AMethod.Consumes.Empty then
    Exit(True);

  if AMethod.Consumes.IsWildCard then
    Exit(True);

  if AMethod.Consumes.Contains(AMediaType) then
    Exit(True);
end;

function TWiRLResource.MatchProduces(AMethod: TWiRLResourceMethod; AMediaType: TMediaType): Boolean;
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

procedure TWiRLResource.ProcessAttributes;
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
      FFilters.Add(TWiRLFilter.Create(LAttribute));
    end
  end;
end;

procedure TWiRLResource.ProcessMethods;
var
  LRttiMethod: TRttiMethod;
  LMethod: TWiRLResourceMethod;
begin
  // Get all the methods in resource
  if Assigned(FRttiType) then
    for LRttiMethod in FRttiType.GetMethods do
    begin
      // Only REST methods get added
      if Length(LRttiMethod.GetAttributes) > 0 then
      begin
        LMethod := TWiRLResourceMethod.Create(Self, LRttiMethod);
        FMethods.Add(LMethod);
      end;
    end;
end;

procedure TWiRLResource.ProcessResource(AInfo: TWiRLConstructorInfo);
begin
  if Assigned(AInfo) then
  begin
    FTypeClass := AInfo.TypeTClass;
    // Get the RttiType of the resource
    FRttiType := TWiRLApplication.RttiContext.GetType(FTypeClass);

    ProcessAttributes;

    ProcessMethods;

    FFound := True;
  end
  else
    FFound := False;
end;

{ TWiRLResourceMethod }

constructor TWiRLResourceMethod.Create(AResource: TWiRLResource; ARttiMethod: TRttiMethod);
begin
  FResource := AResource;
  FRttiMethod := ARttiMethod;
  FConsumes := TMediaTypeList.Create;
  FProduces := TMediaTypeList.Create;
  FFilters := TWiRLFilterList.Create(True);
  FAuth := TWiRLMethodAuthorization.Create;
  FStatus := TWiRLHttpStatus.Create;
  FParams := TWiRLMethodParamList.Create(True);
  FMethodResult := TWiRLMethodResult.Create(FRttiMethod.ReturnType);

  FIsFunction := Assigned(FRttiMethod.ReturnType);

  ProcessAttributes;
  ProcessParams;
end;

destructor TWiRLResourceMethod.Destroy;
begin
  FParams.Free;
  FStatus.Free;
  FAuth.Free;
  FFilters.Free;
  FMethodResult.Free;
  FConsumes.Free;
  FProduces.Free;
  inherited;
end;

function TWiRLResourceMethod.HasFilter(AAttribute: TCustomAttribute): Boolean;
var
  LFilter: TWiRLFilter;
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

procedure TWiRLResourceMethod.ProcessAttributes;
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
    SetLength(FAllAttributes, Length(FAllAttributes) + 1);
    FAllAttributes[Length(FAllAttributes) - 1] := LAttribute;

    // Method HTTP Method
    if LAttribute is HttpMethodAttribute then
    begin
      FHttpMethod := HttpMethodAttribute(LAttribute).ToString;
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
      FFilters.Add(TWiRLFilter.Create(LAttribute));
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

procedure TWiRLResourceMethod.ProcessParams;
var
  LParam: TRttiParameter;
  LWiRLParam: TWiRLMethodParam;
begin
  // Global loop to retrieve and process ALL params at once
  for LParam in FRttiMethod.GetParameters do
  begin
    LWiRLParam := TWiRLMethodParam.Create(LParam);
    if not LWiRLParam.Rest then
    begin
      LWiRLParam.Free;
      raise EWiRLServerException.Create('Non annotated params are not allowed');
    end;
    FParams.Add(LWiRLParam);
  end;
end;

{ TWiRLMethodResult }

constructor TWiRLMethodResult.Create(AResultType: TRttiType);
begin
  if Assigned(AResultType) then
  begin
    FRttiObject := AResultType;
    FResultType := AResultType.TypeKind;
    case FResultType of
      tkClass:  FIsClass := True;
      tkRecord: FIsRecord := True;
    end;
  end;
end;

procedure TWiRLMethodResult.SetAsSingleton;
begin
  FIsSingleton := True;
end;

{ TWiRLMethodAuthorization }

procedure TWiRLMethodAuthorization.SetDenyAll;
begin
  FHasAuth := True;
  FDenyAll := True;
  FPermitAll := False;
end;

procedure TWiRLMethodAuthorization.SetPermitAll;
begin
  FHasAuth := True;
  FDenyAll := False;
  FPermitAll := True;
end;

procedure TWiRLMethodAuthorization.SetRoles(ARoles: TStrings);
begin
  FHasAuth := True;
  FDenyAll := False;
  FPermitAll := False;
  FRoles := ARoles.ToStringArray;
end;

{ TWiRLFilter }

constructor TWiRLFilter.Create(AAttribute: TCustomAttribute);
begin
  FAttribute := AAttribute;
  FFilterType := FAttribute.ClassType;
end;

{ TWiRLMethodParam }

constructor TWiRLMethodParam.Create(AParam: TRttiParameter);
begin
  FParam := AParam;

  ProcessAttributes;
end;

procedure TWiRLMethodParam.ProcessAttributes;
var
  LAttr: TCustomAttribute;
begin
  FAttributes := FParam.GetAttributes;

  for LAttr in FAttributes do
  begin
    // Loop only inside attributes that define how to read the parameter
    if not ( (LAttr is ContextAttribute) or (LAttr is MethodParamAttribute) ) then
      Continue;

    FRest := True;

    // context injection
    if (LAttr is ContextAttribute) and (FParam.ParamType.IsInstance) then
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
      Kind := TMethodParamType.Body
    else if LAttr is FormDataParamAttribute then
      Kind := TMethodParamType.FormData
    else if LAttr is MultipartAttribute then
      Kind := TMethodParamType.MultiPart;

    // Param Name
    FName := (LAttr as MethodParamAttribute).Value;
    if (FName = '') or (LAttr is BodyParamAttribute) then
      FName := FParam.Name;
  end;
end;

end.
