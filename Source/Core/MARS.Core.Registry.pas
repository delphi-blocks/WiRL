(*
  Copyright 2015-2016, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit MARS.Core.Registry;

{$I MARS.inc}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Rtti,
  MARS.Core.Singleton, MARS.Core.Exceptions, MARS.Core.Attributes;

type
  TMARSConstructorInfo = class
  private
    FConstructorFunc: TFunc<TObject>;
    FTypeTClass: TClass;
  public
    constructor Create(AClass: TClass; const AConstructorFunc: TFunc<TObject>);

    property TypeTClass: TClass read FTypeTClass;
    property ConstructorFunc: TFunc<TObject> read FConstructorFunc write FConstructorFunc;
    function Clone: TMARSConstructorInfo;
  end;

  TMARSResourceRegistry = class(TObjectDictionary<string, TMARSConstructorInfo>)
  private
    type
      TMARSResourceRegistrySingleton = TMARSSingleton<TMARSResourceRegistry>;
  protected
    class function GetInstance: TMARSResourceRegistry; static; inline;
  public
    constructor Create; virtual;
    function RegisterResource<T: class>: TMARSConstructorInfo; overload;
    function RegisterResource<T: class>(const AConstructorFunc: TFunc<TObject>): TMARSConstructorInfo; overload;

    function GetResourceClass(const AResource: string; out Value: TClass): Boolean;
    function GetResourceInstance<T: class>: T;

    class property Instance: TMARSResourceRegistry read GetInstance;
  end;

  TMARSFilterRegistry = class(TObjectDictionary<string, TMARSConstructorInfo>)
  private
    type
      TMARSFilterRegistrySingleton = TMARSSingleton<TMARSFilterRegistry>;
    var
      FRttiContext: TRttiContext;
  protected
    class function GetInstance: TMARSFilterRegistry; static; inline;
  public
    constructor Create; virtual;
    function RegisterFilter<T: class>: TMARSConstructorInfo; overload;
    function RegisterFilter<T: class>(const AConstructorFunc: TFunc<TObject>): TMARSConstructorInfo; overload;

    function GetFilterClass(const AResource: string; out Value: TClass): Boolean;
    function GetFilterInstance<T: class>: T;
    procedure FetchRequestFilter(const PreMatching :Boolean; ARequestProc: TProc<TMARSConstructorInfo>);
    procedure FetchResponseFilter(AResponseProc: TProc<TMARSConstructorInfo>);

    class property Instance: TMARSFilterRegistry read GetInstance;
  end;

implementation

uses
  MARS.http.Filters, MARS.Rtti.Utils;

{ TMARSResourceRegistry }

function TMARSResourceRegistry.GetResourceInstance<T>: T;
var
  LInfo: TMARSConstructorInfo;
begin
  if Self.TryGetValue(T.ClassName, LInfo) then
  begin
    if LInfo.ConstructorFunc <> nil then
      Result := LInfo.ConstructorFunc() as T;
  end;
end;

function TMARSResourceRegistry.RegisterResource<T>: TMARSConstructorInfo;
begin
  Result := RegisterResource<T>(nil);
end;

function TMARSResourceRegistry.RegisterResource<T>(
  const AConstructorFunc: TFunc<TObject>): TMARSConstructorInfo;
begin
  Result := TMARSConstructorInfo.Create(TClass(T), AConstructorFunc);
  Self.Add(T.QualifiedClassName, Result);
end;

constructor TMARSResourceRegistry.Create;
begin
  TMARSResourceRegistrySingleton.CheckInstance(Self);

  inherited Create([doOwnsValues]);
end;

class function TMARSResourceRegistry.GetInstance: TMARSResourceRegistry;
begin
  Result := TMARSResourceRegistrySingleton.Instance;
end;

function TMARSResourceRegistry.GetResourceClass(const AResource: string;
  out Value: TClass): Boolean;
var
  LInfo: TMARSConstructorInfo;
begin
  Value := nil;
  Result := Self.TryGetValue(AResource, LInfo);
  if Result then
    Value := LInfo.TypeTClass;
end;

{ TMARSConstructorInfo }

function TMARSConstructorInfo.Clone: TMARSConstructorInfo;
begin
  Result := TMARSConstructorInfo.Create(FTypeTClass, FConstructorFunc);
end;

constructor TMARSConstructorInfo.Create(AClass: TClass;
  const AConstructorFunc: TFunc<TObject>);
begin
  inherited Create;
  FConstructorFunc := AConstructorFunc;
  FTypeTClass := AClass;

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
begin
  TMARSFilterRegistrySingleton.CheckInstance(Self);
  FRttiContext := TRttiContext.Create;

  inherited Create([doOwnsValues]);
end;

procedure TMARSFilterRegistry.FetchRequestFilter(const PreMatching: Boolean;
  ARequestProc: TProc<TMARSConstructorInfo>);
var
  Pair: TPair<string, TMARSConstructorInfo>;
  FilterType :TRttiType;
  IsPreMatching :Boolean;
begin
  for Pair in Self do
  begin
    if Supports(Pair.Value.TypeTClass, IMARSContainerRequestFilter) then
    begin
      FilterType := FRttiContext.GetType(Pair.Value.TypeTClass);
      IsPreMatching := TRttiHelper.HasAttribute<PreMatchingAttribute>(FilterType);

      if PreMatching and IsPreMatching then
        ARequestProc(Pair.Value)
      else if not PreMatching and not IsPreMatching then
        ARequestProc(Pair.Value);
    end;
  end;
end;

procedure TMARSFilterRegistry.FetchResponseFilter(AResponseProc: TProc<TMARSConstructorInfo>);
var
  Pair: TPair<string, TMARSConstructorInfo>;
begin
  for Pair in Self do
  begin
    if Supports(Pair.Value.TypeTClass, IMARSContainerResponseFilter) then
    begin
      AResponseProc(Pair.Value);
    end;
  end;
end;

function TMARSFilterRegistry.GetFilterClass(const AResource: string;
  out Value: TClass): Boolean;
var
  LInfo: TMARSConstructorInfo;
begin
  Value := nil;
  Result := Self.TryGetValue(AResource, LInfo);
  if Result then
    Value := LInfo.TypeTClass;
end;

function TMARSFilterRegistry.GetFilterInstance<T>: T;
var
  LInfo: TMARSConstructorInfo;
begin
  if Self.TryGetValue(T.ClassName, LInfo) then
  begin
    if LInfo.ConstructorFunc <> nil then
      Result := LInfo.ConstructorFunc() as T;
  end;
end;

class function TMARSFilterRegistry.GetInstance: TMARSFilterRegistry;
begin
  Result := TMARSFilterRegistrySingleton.Instance;
end;

function TMARSFilterRegistry.RegisterFilter<T>(
  const AConstructorFunc: TFunc<TObject>): TMARSConstructorInfo;
begin
  if not Supports(TClass(T), IMARSContainerRequestFilter) and not Supports(TClass(T), IMARSContainerResponseFilter) then
    raise EMARSException.CreateFmt('Filter registration error: [%s] should be a valid filter', [TClass(T).QualifiedClassName]);

  Result := TMARSConstructorInfo.Create(TClass(T), AConstructorFunc);
  Self.Add(T.QualifiedClassName, Result);
end;

function TMARSFilterRegistry.RegisterFilter<T>: TMARSConstructorInfo;
begin
  Result := RegisterFilter<T>(nil);
end;

end.
