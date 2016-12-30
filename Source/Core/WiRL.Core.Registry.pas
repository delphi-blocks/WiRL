{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Registry;

{$I WiRL.inc}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Rtti,
  WiRL.Core.Singleton, WiRL.Core.Exceptions, WiRL.Core.Attributes,
  WiRL.Rtti.Utils;

type
  TWiRLConstructorInfo = class
  private
    FConstructorFunc: TFunc<TObject>;
    FTypeTClass: TClass;
  public
    constructor Create(AClass: TClass; const AConstructorFunc: TFunc<TObject>);

    property TypeTClass: TClass read FTypeTClass;
    property ConstructorFunc: TFunc<TObject> read FConstructorFunc write FConstructorFunc;
    function Clone: TWiRLConstructorInfo;
  end;

  TWiRLResourceRegistry = class(TObjectDictionary<string, TWiRLConstructorInfo>)
  private
    type
      TWiRLResourceRegistrySingleton = TWiRLSingleton<TWiRLResourceRegistry>;
  protected
    class function GetInstance: TWiRLResourceRegistry; static; inline;
  public
    constructor Create; virtual;
    function RegisterResource<T: class>: TWiRLConstructorInfo; overload;
    function RegisterResource<T: class>(const AConstructorFunc: TFunc<TObject>): TWiRLConstructorInfo; overload;

    function GetResourceClass(const AResource: string; out Value: TClass): Boolean;
    function GetResourceInstance<T: class>: T;

    class property Instance: TWiRLResourceRegistry read GetInstance;
  end;

implementation

{ TWiRLResourceRegistry }

function TWiRLResourceRegistry.GetResourceInstance<T>: T;
var
  LInfo: TWiRLConstructorInfo;
begin
  if Self.TryGetValue(T.ClassName, LInfo) then
  begin
    if LInfo.ConstructorFunc <> nil then
      Result := LInfo.ConstructorFunc() as T;
  end;
end;

function TWiRLResourceRegistry.RegisterResource<T>: TWiRLConstructorInfo;
begin
  Result := RegisterResource<T>(nil);
end;

function TWiRLResourceRegistry.RegisterResource<T>(
  const AConstructorFunc: TFunc<TObject>): TWiRLConstructorInfo;
begin
  Result := TWiRLConstructorInfo.Create(TClass(T), AConstructorFunc);
  Self.Add(T.QualifiedClassName, Result);
end;

constructor TWiRLResourceRegistry.Create;
begin
  TWiRLResourceRegistrySingleton.CheckInstance(Self);

  inherited Create([doOwnsValues]);
end;

class function TWiRLResourceRegistry.GetInstance: TWiRLResourceRegistry;
begin
  Result := TWiRLResourceRegistrySingleton.Instance;
end;

function TWiRLResourceRegistry.GetResourceClass(const AResource: string;
  out Value: TClass): Boolean;
var
  LInfo: TWiRLConstructorInfo;
begin
  Value := nil;
  Result := Self.TryGetValue(AResource, LInfo);
  if Result then
    Value := LInfo.TypeTClass;
end;

{ TWiRLConstructorInfo }

function TWiRLConstructorInfo.Clone: TWiRLConstructorInfo;
begin
  Result := TWiRLConstructorInfo.Create(FTypeTClass, FConstructorFunc);
end;

constructor TWiRLConstructorInfo.Create(AClass: TClass;
  const AConstructorFunc: TFunc<TObject>);
begin
  inherited Create;
  FConstructorFunc := AConstructorFunc;
  FTypeTClass := AClass;

  // Default constructor function
  if not Assigned(FConstructorFunc) then
    FConstructorFunc :=
      function: TObject
      begin
        Result := TRttiHelper.CreateInstance(FTypeTClass);
      end;
end;

end.
