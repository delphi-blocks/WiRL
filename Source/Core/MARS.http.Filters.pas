unit MARS.http.Filters;

interface

uses
  System.SysUtils, System.Generics.Collections, System.Generics.Defaults,
  System.Classes, System.Rtti,

  MARS.Core.Singleton,
  MARS.Core.Exceptions,
  MARS.Core.Attributes,
  MARS.Core.Request,
  MARS.Core.Response,
  MARS.Rtti.Utils;

type
  IMARSContainerRequestFilter = interface
  ['{58406938-14A1-438F-946A-F0723920B511}']
    procedure Filter(Request: TMARSRequest);
  end;

  IMARSContainerResponseFilter = interface
  ['{F952495E-00DB-44C6-ACED-33F1F2F25527}']
    procedure Filter(Request :TMARSRequest; Response: TMARSResponse);
  end;

  // the lower the number the higher the priority
  // By default, when the @Priority annotation is absent
  // on a component the USER priority value is used
  TMARSPriorities = class(TObject)
  public
    const AUTHENTICATION = 1000;
    const AUTHORIZATION = 2000;
    const HEADER_DECORATOR = 3000;
    const ENTITY_CODER = 4000;
    const USER = 5000;
  end;

  TMARSFilterConstructorInfo = class
  private
    FConstructorFunc: TFunc<TObject>;
    FTypeTClass: TClass;
    FPriority: Integer;
  public
    constructor Create(AClass: TClass; const AConstructorFunc: TFunc<TObject>; APriority :Integer);

    property TypeTClass: TClass read FTypeTClass;
    property Priority: Integer read FPriority;
    property ConstructorFunc: TFunc<TObject> read FConstructorFunc write FConstructorFunc;
    function Clone: TMARSFilterConstructorInfo;
  end;

  TMARSFilterRegistry = class(TObjectList<TMARSFilterConstructorInfo>)
  private
    type
      TMARSFilterRegistrySingleton = TMARSSingleton<TMARSFilterRegistry>;
    var
      FRttiContext: TRttiContext;
      // True if the list has been sorted since the first item was added
      FSorted :Boolean;
    function GetPriority(FilterClass: TClass) :Integer;
  protected
    class function GetInstance: TMARSFilterRegistry; static; inline;
  public
    constructor Create; virtual;

    procedure Sort;

    function RegisterFilter<T: class>: TMARSFilterConstructorInfo; overload;
    function RegisterFilter<T: class>(const AConstructorFunc: TFunc<TObject>): TMARSFilterConstructorInfo; overload;

    procedure FetchRequestFilter(const PreMatching :Boolean; ARequestProc: TProc<TMARSFilterConstructorInfo>);
    procedure FetchResponseFilter(AResponseProc: TProc<TMARSFilterConstructorInfo>);

    class property Instance: TMARSFilterRegistry read GetInstance;
  end;


implementation

{ TMARSConstructorInfo }

function TMARSFilterConstructorInfo.Clone: TMARSFilterConstructorInfo;
begin
  Result := TMARSFilterConstructorInfo.Create(FTypeTClass, FConstructorFunc, FPriority);
end;

constructor TMARSFilterConstructorInfo.Create(AClass: TClass;
  const AConstructorFunc: TFunc<TObject>; APriority :Integer);
begin
  inherited Create;
  FConstructorFunc := AConstructorFunc;
  FTypeTClass := AClass;
  FPriority := APriority;

  // Default constructor function
  if not Assigned(FConstructorFunc) then
    FConstructorFunc :=
      function: TObject
      var
        LContext: TRttiContext;
        LType: TRttiType;
        LValue: TValue;
      begin
        LType := LContext.GetType(FTypeTClass);
        LValue := LType.GetMethod('Create').Invoke(LType.AsInstance.MetaclassType, []);
        Result := LValue.AsObject;
      end;
end;

{ TMARSFilterRegistry }

constructor TMARSFilterRegistry.Create;
var
  Comparer: IComparer<TMARSFilterConstructorInfo>;
begin
  Comparer := TDelegatedComparer<TMARSFilterConstructorInfo>.Create(
    function(const Left, Right: TMARSFilterConstructorInfo): Integer
    begin
      Result := Left.Priority - Right.Priority;
    end
  );

  TMARSFilterRegistrySingleton.CheckInstance(Self);
  FRttiContext := TRttiContext.Create;

  inherited Create(Comparer, True);
end;

procedure TMARSFilterRegistry.FetchRequestFilter(const PreMatching: Boolean;
  ARequestProc: TProc<TMARSFilterConstructorInfo>);
var
  ConstructorInfo: TMARSFilterConstructorInfo;
  FilterType :TRttiType;
  IsPreMatching :Boolean;
begin
  Sort;
  for ConstructorInfo in Self do
  begin
    if Supports(ConstructorInfo.TypeTClass, IMARSContainerRequestFilter) then
    begin
      FilterType := FRttiContext.GetType(ConstructorInfo.TypeTClass);
      IsPreMatching := TRttiHelper.HasAttribute<PreMatchingAttribute>(FilterType);

      if PreMatching and IsPreMatching then
        ARequestProc(ConstructorInfo)
      else if not PreMatching and not IsPreMatching then
        ARequestProc(ConstructorInfo);
    end;
  end;
end;

procedure TMARSFilterRegistry.FetchResponseFilter(AResponseProc: TProc<TMARSFilterConstructorInfo>);
var
  ConstructorInfo :TMARSFilterConstructorInfo;
begin
  Sort;
  for ConstructorInfo in Self do
  begin
    if Supports(ConstructorInfo.TypeTClass, IMARSContainerResponseFilter) then
    begin
      AResponseProc(ConstructorInfo);
    end;
  end;
end;

class function TMARSFilterRegistry.GetInstance: TMARSFilterRegistry;
begin
  Result := TMARSFilterRegistrySingleton.Instance;
end;

function TMARSFilterRegistry.GetPriority(FilterClass: TClass): Integer;
var
  Priority :Integer;
begin
  Priority := TMARSPriorities.USER;
  TRttiHelper.HasAttribute<PriorityAttribute>(FRttiContext.GetType(FilterClass), procedure (Attrib: PriorityAttribute) begin
    Priority := Attrib.Value;
  end);
  Result := Priority;
end;

function TMARSFilterRegistry.RegisterFilter<T>(
  const AConstructorFunc: TFunc<TObject>): TMARSFilterConstructorInfo;
begin
  if not Supports(TClass(T), IMARSContainerRequestFilter) and not Supports(TClass(T), IMARSContainerResponseFilter) then
    raise EMARSException.CreateFmt('Filter registration error: [%s] should be a valid filter', [TClass(T).QualifiedClassName]);

  Result := TMARSFilterConstructorInfo.Create(TClass(T), AConstructorFunc, GetPriority(TClass(T)));
  Add(Result);
  FSorted := False;
end;

procedure TMARSFilterRegistry.Sort;
begin
  if not FSorted then
  begin
    inherited Sort;
    FSorted := True;
  end;
end;

function TMARSFilterRegistry.RegisterFilter<T>: TMARSFilterConstructorInfo;
begin
  Result := RegisterFilter<T>(nil);
end;

end.
