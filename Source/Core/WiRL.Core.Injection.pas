{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2025 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Injection;

interface

uses
  System.Classes, System.SysUtils, System.Rtti, System.Generics.Defaults,
  System.Generics.Collections,

  WiRL.Core.Context,
  WiRL.Core.Attributes,
  WiRL.Core.Singleton,
  WiRL.Core.Exceptions,
  WiRL.Rtti.Utils;

type
  /// <summary>
  /// Factory interface to create context objects. The class implementing this
  /// interface must be registered in the global TWiRLContextInjectionRegistry.
  /// If a custom disposer is needed, the class must also implement IContextObjectDisposer.
  /// </summary>
  IContextObjectFactory = interface
  ['{43596462-9B26-4B84-BD5C-0225900F6C93}']
    function CreateContextObject(const AObject: TRttiObject; AContext: TWiRLContextBase): TValue;
  end;

  /// <summary>
  /// Optional interface for custom disposal of context objects. 
  /// Classes implementing this interface must also implement IContextObjectFactory 
  /// and be registered in the global TWiRLContextInjectionRegistry.
  /// This allows factories to define custom cleanup logic for their context objects.
  /// </summary>
  IContextObjectDisposer = interface
  ['{2E2950CE-93C7-463D-8C2A-B336E8FD3BD4}']
    procedure DisposeContextObject(AObject: TObject; AContext: TWiRLContextBase);
  end;

  TWiRLContextInjectionRegistry = class
  private
    type
      TWiRLContextInjectionRegistrySingleton = TWiRLSingleton<TWiRLContextInjectionRegistry>;

      TEntryInfo = record
        ContextClass: TClass;
        FactoryClass: TClass;
        ConstructorFunc: TFunc<IInterface>;
      end;
  private
    FRegistry: TList<TEntryInfo>;
    class function GetInstance: TWiRLContextInjectionRegistry; static; inline;
    function CustomContextInjectionByType(const AObject: TRttiObject;
      AContext: TWiRLContextBase; out AValue: TValue; AOptions: TContextOptions = []): Boolean;
    function IsSigleton(const AObject: TRttiObject): Boolean;
    function GetContextObject(AFactory: IInterface; const AObject: TRttiObject; AContext: TWiRLContextBase): TValue;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure RegisterFactory<T: class>(const AFactoryClass: TClass); overload;
    procedure RegisterFactory<T: class>(const AFactoryClass: TClass; const AConstructorFunc: TFunc<IInterface>); overload;

    procedure ContextInjection(AInstance: TObject; AContext: TWiRLContextBase);
    function ContextInjectionByType(const AObject: TRttiObject;
      AContext: TWiRLContextBase; out AValue: TValue; AOptions: TContextOptions = []): Boolean;

    class property Instance: TWiRLContextInjectionRegistry read GetInstance;
  end;

implementation

uses
  WiRL.Configuration.Core,
  WiRL.http.URL,
  WiRL.http.Request,
  WiRL.http.Response;

{ TWiRLContextInjectionRegistry }

constructor TWiRLContextInjectionRegistry.Create;
begin
  inherited;
  FRegistry := TList<TEntryInfo>.Create;
end;

function TWiRLContextInjectionRegistry.CustomContextInjectionByType(
  const AObject: TRttiObject; AContext: TWiRLContextBase; out AValue: TValue; AOptions: TContextOptions): Boolean;
var
  LType: TClass;
  LEntry: TEntryInfo;
  LFactory: IInterface;
  LContextOwned: Boolean;
begin
  Result := False;
  LType := TRttiHelper.GetType(AObject).AsInstance.MetaclassType;

  for LEntry in FRegistry do
  begin
    if LType.InheritsFrom(LEntry.ContextClass) then
    begin
      LFactory := LEntry.ConstructorFunc();
      AValue := GetContextObject(LFactory, AObject, AContext);
      if AValue.IsObject then  // Only object should be released
      begin
        if not (TContextOption.NoRecursive in AOptions) then
          ContextInjection(AValue.AsObject, AContext);

        LContextOwned := not IsSigleton(AObject); // Singleton should not be released
        AContext.ContextData.Add(AValue.AsObject, LFactory, LContextOwned);
      end;
      Exit(True);
    end;
  end;
end;

destructor TWiRLContextInjectionRegistry.Destroy;
begin
  FRegistry.Free;
  inherited;
end;

function TWiRLContextInjectionRegistry.GetContextObject(
  AFactory: IInterface; const AObject: TRttiObject; AContext: TWiRLContextBase): TValue;
var
  LContextFactory: IContextObjectFactory;
  LContextHttpFactory: IContextHttpFactory;
begin
  if Supports(AFactory, IContextObjectFactory, LContextFactory) then
  begin
    Result := LContextFactory.CreateContextObject(AObject, AContext);
  end
  else if Supports(AFactory, IContextHttpFactory, LContextHttpFactory) then
  begin
    Result := LContextHttpFactory.CreateContextObject(AObject, AContext as TWiRLContextHttp);
  end
  else
    raise EWiRLServerException.CreateFmt('Interface IContextObjectFactory or IContextHttpFactory not implemented in []', [TObject(AFactory).ClassName]);
end;

class function TWiRLContextInjectionRegistry.GetInstance: TWiRLContextInjectionRegistry;
begin
  Result := TWiRLContextInjectionRegistrySingleton.Instance;
end;

function TWiRLContextInjectionRegistry.IsSigleton(
  const AObject: TRttiObject): Boolean;
begin
  Result :=
    TRttiHelper.HasAttribute<SingletonAttribute>(AObject) or
    TRttiHelper.HasAttribute<SingletonAttribute>(TRttiHelper.GetType(AObject));
end;

procedure TWiRLContextInjectionRegistry.RegisterFactory<T>(
  const AFactoryClass: TClass; const AConstructorFunc: TFunc<IInterface>);
var
  LEntryInfo: TEntryInfo;
begin
  LEntryInfo.ContextClass := TClass(T);
  LEntryInfo.FactoryClass := AFactoryClass;
  LEntryInfo.ConstructorFunc := AConstructorFunc;
  FRegistry.Add(LEntryInfo)
end;

procedure TWiRLContextInjectionRegistry.RegisterFactory<T>(const AFactoryClass: TClass);
begin
  Self.RegisterFactory<T>(AFactoryClass,
    function: IInterface
    var
      LInstance: TObject;
    begin
      LInstance := (TRttiHelper.CreateInstance(AFactoryClass));
      if not Supports(LInstance, IInterface, Result) then
        raise EWiRLServerException.Create('Interface IContextObjectFactory or IContextHttpFactory not implemented');
    end);
end;

function TWiRLContextInjectionRegistry.ContextInjectionByType(const AObject: TRttiObject;
  AContext: TWiRLContextBase; out AValue: TValue; AOptions: TContextOptions): Boolean;
var
  LType: TClass;
begin
  LType := TRttiHelper.GetType(AObject).AsInstance.MetaclassType;

  AValue := AContext.FindContextDataAs(LType);
  if not AValue.IsEmpty then
    Exit(True);

  Result := CustomContextInjectionByType(AObject, AContext, AValue, AOptions);
end;

// Must be thread safe
procedure TWiRLContextInjectionRegistry.ContextInjection(AInstance: TObject; AContext: TWiRLContextBase);
var
  LType: TRttiType;
  LFieldClassType: TClass;
begin
  LType := TRttiHelper.Context.GetType(AInstance.ClassType);
  // Context injection
  TRttiHelper.ForEachFieldWithAttribute<ContextAttribute>(LType,
    function (AField: TRttiField; AAttrib: ContextAttribute): Boolean
    var
      LValue: TValue;
    begin
      Result := True; // enumerate all
      if (AField.FieldType.IsInstance) then
      begin
        LFieldClassType := TRttiInstanceType(AField.FieldType).MetaclassType;

        if not ContextInjectionByType(AField, AContext, LValue, AAttrib.Options) then
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
      LValue: TValue;
    begin
      Result := True; // enumerate all
      if (AProperty.PropertyType.IsInstance) then
      begin
        if ContextInjectionByType(AProperty, AContext, LValue) then
          AProperty.SetValue(AInstance, LValue);
      end;
    end
  );
end;

end.
