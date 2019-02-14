{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
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
  IContextFactory = interface
  ['{43596462-9B26-4B84-BD5C-0225900F6C93}']
    function CreateContext(const AObject: TRttiObject;
      AContext: TWiRLContext): TValue;
  end;

  TWiRLContextInjectionRegistry = class
  private
    type
      TWiRLContextInjectionRegistrySingleton = TWiRLSingleton<TWiRLContextInjectionRegistry>;
      TEntryInfo = record
        ContextClass: TClass;
        FactoryClass: TClass;
        ConstructorFunc: TFunc<IContextFactory>
      end;
  private
    FRegistry: TList<TEntryInfo>;
    class function GetInstance: TWiRLContextInjectionRegistry; static; inline;
    function CustomContextInjectionByType(const AObject: TRttiObject;
      AContext: TWiRLContext; out AValue: TValue): Boolean;
    function IsSigleton(const AObject: TRttiObject): Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure RegisterFactory<T: class>(const AFactoryClass: TClass); overload;
    procedure RegisterFactory<T: class>(const AFactoryClass: TClass; const AConstructorFunc: TFunc<IContextFactory>); overload;

    procedure ContextInjection(AInstance: TObject; AContext: TWiRLContext);
    function ContextInjectionByType(const AObject: TRttiObject;
      AContext: TWiRLContext; out AValue: TValue): Boolean;

    class property Instance: TWiRLContextInjectionRegistry read GetInstance;
  end;

implementation

uses
  WiRL.Core.Engine,
  WiRL.Core.Application,
  WiRL.http.URL,
  WiRL.http.Server,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.Core.Auth.Context;

{ TWiRLContextInjectionRegistry }

constructor TWiRLContextInjectionRegistry.Create;
begin
  inherited;
  FRegistry := TList<TEntryInfo>.Create;
end;

function TWiRLContextInjectionRegistry.CustomContextInjectionByType(
  const AObject: TRttiObject; AContext: TWiRLContext; out AValue: TValue): Boolean;
var
  LType: TClass;
  LEntry: TEntryInfo;
  LContextFactory: IContextFactory;
  LContextOwned: Boolean;
begin
  Result := False;
  LType := TRttiHelper.GetType(AObject).AsInstance.MetaclassType;
  for LEntry in FRegistry do
  begin
    if LEntry.ContextClass = LType then
    begin
      LContextFactory := LEntry.ConstructorFunc();
      AValue := LContextFactory.CreateContext(AObject, AContext);
      if AValue.IsObject then  // Only object should be released
      begin
        LContextOwned := not IsSigleton(AObject); // Singleton should'n be released
        AContext.CustomContext.Add(AValue.AsObject, LContextOwned);
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
  const AFactoryClass: TClass);
begin
  Self.RegisterFactory<T>(AFactoryClass,
    function :IContextFactory
    var
      LInstance: TObject;
    begin
      LInstance := (TRttiHelper.CreateInstance(AFactoryClass));
      if not Supports(LInstance, IContextFactory, Result) then
        raise Exception.Create('Interface IContextFactory not implemented');
    end);
end;

function TWiRLContextInjectionRegistry.ContextInjectionByType(const AObject: TRttiObject;
  AContext: TWiRLContext; out AValue: TValue): Boolean;
var
  LType: TClass;
begin
  Result := True;
  LType := TRttiHelper.GetType(AObject).AsInstance.MetaclassType;

  // AuthContext
  if (LType.InheritsFrom(TWiRLAuthContext)) then
    AValue := AContext.AuthContext
  // Claims (Subject)
  else if (LType.InheritsFrom(TWiRLSubject)) then
    AValue := AContext.AuthContext.Subject
  // HTTP Server
  else if (LType.InheritsFrom(TWiRLServer)) then
    AValue := AContext.Server as TWiRLServer
  // HTTP request
  else if (LType.InheritsFrom(TWiRLRequest)) then
    AValue := AContext.Request
  // HTTP response
  else if (LType.InheritsFrom(TWiRLResponse)) then
    AValue := AContext.Response
  // URL info
  else if (LType.InheritsFrom(TWiRLURL)) then
    AValue := AContext.URL
  // Engine
  else if (LType.InheritsFrom(TWiRLEngine)) then
    AValue := AContext.Engine as TWiRLEngine
  // Application
  else if (LType.InheritsFrom(TWiRLApplication)) then
    AValue := AContext.Application
  else if CustomContextInjectionByType(AObject, AContext, AValue) then
    //
  else
    Result := False;
end;

procedure TWiRLContextInjectionRegistry.RegisterFactory<T>(
  const AFactoryClass: TClass; const AConstructorFunc: TFunc<IContextFactory>);
var
  LEntryInfo: TEntryInfo;
begin
  LEntryInfo.ContextClass := TClass(T);
  LEntryInfo.FactoryClass := AFactoryClass;
  LEntryInfo.ConstructorFunc := AConstructorFunc;
  FRegistry.Add(LEntryInfo)
end;

// Must be thread safe
procedure TWiRLContextInjectionRegistry.ContextInjection(AInstance: TObject; AContext: TWiRLContext);
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

        if not ContextInjectionByType(AField, AContext, LValue) then
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
