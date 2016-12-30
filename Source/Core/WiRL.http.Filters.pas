{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.Filters;

interface

uses
  System.SysUtils, System.Generics.Collections, System.Generics.Defaults,
  System.Classes, System.Rtti,

  WiRL.Core.Singleton,
  WiRL.Core.Exceptions,
  WiRL.Core.Attributes,
  WiRL.Core.Request,
  WiRL.Core.Response,
  WiRL.Rtti.Utils;

type
  IWiRLContainerRequestFilter = interface
  ['{58406938-14A1-438F-946A-F0723920B511}']
    procedure Filter(Request: TWiRLRequest);
  end;

  IWiRLContainerResponseFilter = interface
  ['{F952495E-00DB-44C6-ACED-33F1F2F25527}']
    procedure Filter(Request: TWiRLRequest; Response: TWiRLResponse);
  end;

  // the lower the number the higher the priority
  // By default, when the @Priority annotation is absent
  // on a component the USER priority value is used
  TWiRLPriorities = class(TObject)
  public
    const AUTHENTICATION = 1000;
    const AUTHORIZATION = 2000;
    const HEADER_DECORATOR = 3000;
    const ENTITY_CODER = 4000;
    const USER = 5000;
  end;

  TWiRLFilterConstructorInfo = class
  private
    FConstructorFunc: TFunc<TObject>;
    FTypeTClass: TClass;
    FPriority: Integer;
  public
    constructor Create(AClass: TClass; const AConstructorFunc: TFunc<TObject>; APriority :Integer);

    property TypeTClass: TClass read FTypeTClass;
    property Priority: Integer read FPriority;
    property ConstructorFunc: TFunc<TObject> read FConstructorFunc write FConstructorFunc;
    function Clone: TWiRLFilterConstructorInfo;
  end;

  TWiRLFilterRegistry = class(TObjectList<TWiRLFilterConstructorInfo>)
  private
    type
      TWiRLFilterRegistrySingleton = TWiRLSingleton<TWiRLFilterRegistry>;
    var
      FRttiContext: TRttiContext;
      // True if the list has been sorted since the first item was added
      FSorted: Boolean;
    function GetPriority(FilterClass: TClass) :Integer;
  protected
    class function GetInstance: TWiRLFilterRegistry; static; inline;
  public
    constructor Create; virtual;

    procedure Sort;

    function FilterByClassName(const AClassName: string; out AConstructorInfo: TWiRLFilterConstructorInfo) :Boolean;
    function RegisterFilter<T: class>: TWiRLFilterConstructorInfo; overload;
    function RegisterFilter<T: class>(const AConstructorFunc: TFunc<TObject>): TWiRLFilterConstructorInfo; overload;

    procedure FetchRequestFilter(const PreMatching: Boolean; ARequestProc :TProc<TWiRLFilterConstructorInfo>);
    procedure FetchResponseFilter(AResponseProc: TProc<TWiRLFilterConstructorInfo>);

    class property Instance: TWiRLFilterRegistry read GetInstance;
  end;


implementation

{ TWiRLConstructorInfo }

function TWiRLFilterConstructorInfo.Clone: TWiRLFilterConstructorInfo;
begin
  Result := TWiRLFilterConstructorInfo.Create(FTypeTClass, FConstructorFunc, FPriority);
end;

constructor TWiRLFilterConstructorInfo.Create(AClass: TClass;
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
      end
    ;
end;

{ TWiRLFilterRegistry }

constructor TWiRLFilterRegistry.Create;
var
  LComparer: IComparer<TWiRLFilterConstructorInfo>;
begin
  LComparer := TDelegatedComparer<TWiRLFilterConstructorInfo>.Create(
    function(const Left, Right: TWiRLFilterConstructorInfo): Integer
    begin
      Result := Left.Priority - Right.Priority;
    end
  );

  //TWiRLFilterRegistrySingleton.CheckInstance(Self);
  FRttiContext := TRttiContext.Create;

  inherited Create(LComparer, True);
end;

procedure TWiRLFilterRegistry.FetchRequestFilter(const PreMatching: Boolean;
  ARequestProc: TProc<TWiRLFilterConstructorInfo>);
var
  LConstructorInfo: TWiRLFilterConstructorInfo;
  LFilterType: TRttiType;
  LIsPreMatching: Boolean;
begin
  Sort;
  for LConstructorInfo in Self do
  begin
    if Supports(LConstructorInfo.TypeTClass, IWiRLContainerRequestFilter) then
    begin
      LFilterType := FRttiContext.GetType(LConstructorInfo.TypeTClass);
      LIsPreMatching := TRttiHelper.HasAttribute<PreMatchingAttribute>(LFilterType);

      if PreMatching and LIsPreMatching then
        ARequestProc(LConstructorInfo)
      else if not PreMatching and not LIsPreMatching then
        ARequestProc(LConstructorInfo);
    end;
  end;
end;

procedure TWiRLFilterRegistry.FetchResponseFilter(AResponseProc: TProc<TWiRLFilterConstructorInfo>);
var
  LConstructorInfo: TWiRLFilterConstructorInfo;
begin
  Sort;
  for LConstructorInfo in Self do
  begin
    if Supports(LConstructorInfo.TypeTClass, IWiRLContainerResponseFilter) then
    begin
      AResponseProc(LConstructorInfo);
    end;
  end;
end;

function TWiRLFilterRegistry.FilterByClassName(const AClassName: string;
  out AConstructorInfo: TWiRLFilterConstructorInfo): Boolean;
var
  LItem: TWiRLFilterConstructorInfo;
begin
  Result := False;
  for LItem in Self do
  begin
    if CompareText(LItem.TypeTClass.QualifiedClassName, AClassName) = 0 then
    begin
      AConstructorInfo := LItem;
      Exit(True);
    end;
  end;
end;

class function TWiRLFilterRegistry.GetInstance: TWiRLFilterRegistry;
begin
  Result := TWiRLFilterRegistrySingleton.Instance;
end;

function TWiRLFilterRegistry.GetPriority(FilterClass: TClass): Integer;
var
  LPriority: Integer;
begin
  LPriority := TWiRLPriorities.USER;
  TRttiHelper.HasAttribute<PriorityAttribute>(FRttiContext.GetType(FilterClass),
    procedure (Attrib: PriorityAttribute)
    begin
      LPriority := Attrib.Value;
    end
  );
  Result := LPriority;
end;

function TWiRLFilterRegistry.RegisterFilter<T>(
  const AConstructorFunc: TFunc<TObject>): TWiRLFilterConstructorInfo;
begin
  if not Supports(TClass(T), IWiRLContainerRequestFilter) and not Supports(TClass(T), IWiRLContainerResponseFilter) then
    raise EWiRLServerException.Create(
      Format('Filter registration error: [%s] should be a valid filter', [TClass(T).QualifiedClassName]),
      Self.ClassName, 'RegisterFilter',
    );

  Result := TWiRLFilterConstructorInfo.Create(TClass(T), AConstructorFunc, GetPriority(TClass(T)));
  Add(Result);
  FSorted := False;
end;

procedure TWiRLFilterRegistry.Sort;
begin
  if not FSorted then
  begin
    inherited Sort;
    FSorted := True;
  end;
end;

function TWiRLFilterRegistry.RegisterFilter<T>: TWiRLFilterConstructorInfo;
begin
  Result := RegisterFilter<T>(nil);
end;

end.
