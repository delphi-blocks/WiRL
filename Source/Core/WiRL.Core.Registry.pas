{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2023 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Registry;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Rtti,
  WiRL.Core.Singleton, WiRL.Core.Exceptions, WiRL.Core.Attributes,
  WiRL.Rtti.Utils;

type
  TWiRLConstructorProxy = class
  private
    FConstructorFunc: TFunc<TObject>;
    FTypeTClass: TClass;
  public
    constructor Create(AClass: TClass; const AConstructorFunc: TFunc<TObject>);

    property TypeTClass: TClass read FTypeTClass;
    property ConstructorFunc: TFunc<TObject> read FConstructorFunc write FConstructorFunc;
    function Clone: TWiRLConstructorProxy;
  end;

  TWiRLResourceRegistry = class(TObjectDictionary<string, TWiRLConstructorProxy>)
  private type
    TWiRLResourceRegistrySingleton = TWiRLSingleton<TWiRLResourceRegistry>;
  protected
    class function GetInstance: TWiRLResourceRegistry; static; inline;
  public
    constructor Create; virtual;
    function AddResourceName(const AResourceName: string): TWiRLConstructorProxy;

    function RegisterResource(AClass: TClass): TWiRLConstructorProxy; overload;
    function RegisterResource<T: class>: TWiRLConstructorProxy; overload;
    function RegisterResource<T: class>(const AConstructorFunc: TFunc<TObject>): TWiRLConstructorProxy; overload;

    function ResourceExists(AClass: TClass): Boolean; overload;
    function ResourceExists<T: class>: Boolean; overload;

    procedure UnregisterResource(AClass: TClass);

    function GetResourceClass(const AResourceName: string; out Value: TClass): Boolean;
    function GetResourceInstance<T: class>: T;

    class property Instance: TWiRLResourceRegistry read GetInstance;
  end;

implementation

{ TWiRLResourceRegistry }

function TWiRLResourceRegistry.GetResourceInstance<T>: T;
var
  LInfo: TWiRLConstructorProxy;
begin
  Result := default(T);
  if Self.TryGetValue(T.ClassName, LInfo) then
  begin
    if LInfo.ConstructorFunc <> nil then
      Result := LInfo.ConstructorFunc() as T;
  end;
end;

function TWiRLResourceRegistry.RegisterResource<T>: TWiRLConstructorProxy;
begin
  Result := RegisterResource<T>(nil);
end;

function TWiRLResourceRegistry.RegisterResource(AClass: TClass): TWiRLConstructorProxy;
begin
  Result := TWiRLConstructorProxy.Create(AClass, nil);
  Self.Add(AClass.QualifiedClassName, Result);
end;

function TWiRLResourceRegistry.RegisterResource<T>(
  const AConstructorFunc: TFunc<TObject>): TWiRLConstructorProxy;
begin
  Result := TWiRLConstructorProxy.Create(TClass(T), AConstructorFunc);
  Self.Add(T.QualifiedClassName, Result);
end;

procedure TWiRLResourceRegistry.UnregisterResource(AClass: TClass);
begin
  Self.Remove(AClass.QualifiedClassName);
end;

function TWiRLResourceRegistry.AddResourceName(const AResourceName: string): TWiRLConstructorProxy;
begin
  Self.Add(AResourceName, nil);
  Result := nil;
end;

constructor TWiRLResourceRegistry.Create;
begin
  inherited Create([doOwnsValues]);
end;

function TWiRLResourceRegistry.ResourceExists(AClass: TClass): Boolean;
var
  LItem: TWiRLConstructorProxy;
begin
  Result := TryGetValue(AClass.QualifiedClassName, LItem);
end;

function TWiRLResourceRegistry.ResourceExists<T>: Boolean;
begin
  Result := ResourceExists(TClass(T));
end;

class function TWiRLResourceRegistry.GetInstance: TWiRLResourceRegistry;
begin
  Result := TWiRLResourceRegistrySingleton.Instance;
end;

function TWiRLResourceRegistry.GetResourceClass(const AResourceName: string; out Value: TClass): Boolean;
var
  LInfo: TWiRLConstructorProxy;
begin
  Value := nil;
  Result := Self.TryGetValue(AResourceName, LInfo);
  if Result then
    Value := LInfo.TypeTClass;
end;

{ TWiRLConstructorProxy }

function TWiRLConstructorProxy.Clone: TWiRLConstructorProxy;
begin
  Result := TWiRLConstructorProxy.Create(FTypeTClass, FConstructorFunc);
end;

constructor TWiRLConstructorProxy.Create(AClass: TClass;
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
