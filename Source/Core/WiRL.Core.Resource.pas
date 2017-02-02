{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Resource;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, System.Rtti,
  System.TypInfo,

  WiRL.http.Filters,
  WiRL.http.Accept.MediaType,
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

    property FilterType: TClass read FFilterType;
  end;

  TWiRLFilterList = class(TObjectList<TWiRLFilter>)
  end;

  TWiRLMethodResult = class
  private
    FResultType: TTypeKind;
    FIsClass: Boolean;
    FIsSingleton: Boolean;
  public
    constructor Create(AResultType: TRttiType);
    procedure SetAsSingleton;

    property ResultType: TTypeKind read FResultType;
    property IsClass: Boolean read FIsClass;
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

    procedure ProcessAttributes;
  public
    constructor Create(AResource: TWiRLResource; ARttiMethod: TRttiMethod);
    destructor Destroy; override;

    function HasFilter(AAttribute: TCustomAttribute): Boolean;

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
  public
    constructor Create(AContext: TWiRLContext);
    destructor Destroy; override;

    function CreateInstance: TObject;
    function GetResourceMethod: TWiRLResourceMethod;
    function GetRequestMethod(AContext: TWiRLContext; const ARequestedPath: string): TWiRLResourceMethod;

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
  WiRL.Core.URL,
  WiRL.Core.Attributes,
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

  FEnginePath := TWiRLEngine(FContext.Engine).BasePath;
  FAppPath := LApp.BasePath;
  FInfo := LApp.GetResourceInfo(FContext.URL.Resource);

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
  LMethod: TWiRLResourceMethod;
  LPrototypeURL: TWiRLURL;
  LPathMatches,
  LProducesMatch,
  LHttpMethodMatches: Boolean;

begin
  Result := nil;

  for LMethod in FMethods do
  begin
    // Skip the non-Rest methods (no GET/POST/PUT methods)
    if not LMethod.Rest then
      Continue;

    LPathMatches := False;
    LProducesMatch := False;

    LHttpMethodMatches := LMethod.HttpMethod = FContext.Request.Method;

    if LHttpMethodMatches then
    begin
      LPrototypeURL := TWiRLURL.CreateDummy(FEnginePath, FAppPath, FPath, LMethod.Path);
      try
        LPathMatches := LPrototypeURL.MatchPath(FContext.URL);
      finally
        LPrototypeURL.Free;
      end;
    end;

    // It's a procedure, so no Produces mechanism
    if not LMethod.IsFunction then
      LProducesMatch := True
    else
    begin
      // Match the Produces MediaType
      if (LMethod.Produces.Count = 0) or
         ((LMethod.Produces.Count = 1) and LMethod.Produces.Contains(TMediaType.WILDCARD)) then
      begin
        if LMethod.MethodResult.IsClass then
          LProducesMatch := True;
      end
      else
      begin
        if FContext.Request.AcceptableMediaTypes.Intersected(LMethod.Produces) then
          LProducesMatch := True;
      end;
    end;

    if LPathMatches and LHttpMethodMatches and LProducesMatch then
    begin
      Result := LMethod;
      Break;
    end;
  end;

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
    end;
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
  FMethodResult := TWiRLMethodResult.Create(FRttiMethod.ReturnType);

  FIsFunction := Assigned(FRttiMethod.ReturnType);

  ProcessAttributes;
end;

destructor TWiRLResourceMethod.Destroy;
begin
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
  LMediaList: TArray<string>;
  LMedia: string;
begin
  FRest := False;

  // Global loop to retrieve and process ALL attributes at once
  for LAttribute in FRttiMethod.GetAttributes do
  begin
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
    end;
  end;
end;

{ TWiRLMethodResult }

constructor TWiRLMethodResult.Create(AResultType: TRttiType);
begin
  if Assigned(AResultType) then
  begin
    FResultType := AResultType.TypeKind;
    if FResultType = tkClass then
      FIsClass := True;
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

end.
