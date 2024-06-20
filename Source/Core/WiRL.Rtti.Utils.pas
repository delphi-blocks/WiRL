{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2021 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Rtti.Utils;

{$I ..\Core\WiRL.inc}

interface

uses
  System.SysUtils, System.Classes, System.Rtti, System.TypInfo,
  WiRL.Core.Declarations;

type

  TRttiHelper = class
  private
    class var FContext: TRttiContext;
  public
    // TRttiObject helpers functions
    class function FindAttribute<T: TCustomAttribute>(AClass: TClass): T; overload; static;

    class function FindAttribute<T: TCustomAttribute>(AType: TRttiObject): T; overload; static;

    class function HasAttribute<T: TCustomAttribute>(AClass: TClass): Boolean; overload; static;

    class function HasAttribute<T: TCustomAttribute>(ARttiObj: TRttiObject): Boolean; overload; static;

    class function HasAttribute<T: TCustomAttribute>(
      ARttiObj: TRttiObject; const ADoSomething: TProc<T>): Boolean; overload; static;

    class function ForEachAttribute<T: TCustomAttribute>(
      ARttiObj: TRttiObject; const ADoSomething: TProc<T>): Integer; overload; static;

    // TRttiType helpers functions
    class function ForEachMethodWithAttribute<T: TCustomAttribute>(
      ARttiType: TRttiType; const ADoSomething: TFunc<TRttiMethod, T, Boolean>): Integer; static;

    class function ForEachFieldWithAttribute<T: TCustomAttribute>(
      ARttiType: TRttiType; const ADoSomething: TFunc<TRttiField, T, Boolean>): Integer; overload; static;

    class function ForEachPropertyWithAttribute<T: TCustomAttribute>(
      ARttiType: TRttiType; const ADoSomething: TFunc<TRttiProperty, T, Boolean>): Integer; overload; static;

    class function IsDynamicArrayOf<T: class>(ARttiType: TRttiType;
      const AAllowInherithance: Boolean = True): Boolean; overload; static;

    class function IsDynamicArrayOf(ARttiType: TRttiType; const AClass: TClass;
      const AAllowInherithance: Boolean = True): Boolean; overload; static;

    class function IsObjectOfType<T: class>(ARttiType: TRttiType;
      const AAllowInherithance: Boolean = True): Boolean; overload; static;

    class function IsObjectOfType(ARttiType: TRttiType; const AClass: TClass;
      const AAllowInherithance: Boolean = True): Boolean; overload; static;

    class function IsInterfaceOfType(ARttiType: TRttiType; const IID: TGUID;
      const AAllowInherithance: Boolean = True): Boolean; overload; static;

    class function IsInterfaceOfType(ATypeInfo: Pointer; const IID: TGUID;
      const AAllowInherithance: Boolean = True): Boolean; overload; static;

    // Create new value data
    class function CreateNewValue(AType: TRttiType): TValue; static;

    // Create instance of class with parameterless constructor
    class function CreateInstanceValue(AType: TRttiType): TValue; overload;

    // Create array of TValue with ALength elements
    class function CreateArrayValue(AType: TRttiType; ALength: NativeInt): TValue; overload;
    class function CreateArrayValue(ATypeInfo: PTypeInfo; ALength: NativeInt): TValue; overload;

    // Create instance of class with parameterless constructor
    class function CreateInstance(AClass: TClass): TObject;  overload;
    class function CreateInstance(AType: TRttiType): TObject; overload;
    class function CreateInstance(const ATypeName: string): TObject; overload;

    // Create instance of class with one string parameter
    class function CreateInstance(AClass: TClass; const AValue: string): TObject;  overload;
    class function CreateInstance(AType: TRttiType; const AValue: string): TObject; overload;
    class function CreateInstance(const ATypeName, AValue: string): TObject; overload;

    // Create instance of class with an array of TValue
    class function CreateInstance(AClass: TClass; const Args: array of TValue): TObject;  overload;
    class function CreateInstance(AType: TRttiType;  const Args: array of TValue): TObject; overload;
    class function CreateInstance(const ATypeName: string; const Args: array of TValue): TObject; overload;

    // Rtti general helper functions
    class function IfHasAttribute<T: TCustomAttribute>(AInstance: TObject): Boolean; overload;
    class function IfHasAttribute<T: TCustomAttribute>(AInstance: TObject; const ADoSomething: TProc<T>): Boolean; overload;

    class function ForEachAttribute<T: TCustomAttribute>(AInstance: TObject; const ADoSomething: TProc<T>): Integer; overload;

    class function ForEachFieldWithAttribute<T: TCustomAttribute>(AInstance: TObject; const ADoSomething: TFunc<TRttiField, T, Boolean>): Integer; overload;
    class function ForEachField(AInstance: TObject; const ADoSomething: TFunc<TRttiField, Boolean>): Integer;

    class function GetType(AObject: TRttiObject): TRttiType; overload;
    class function GetType(ATypeInfo: Pointer): TRttiType; overload;

    class function ObjectAsType<T>(AObject: TObject): T; static;

    class constructor Create;
    class destructor Destroy;

    class property Context: TRttiContext read FContext;
  end;

  {$IFDEF CUSTOM_ATTRIBUTE_BUG}
  TRttiPatch = class
  private
    class function CheckFreeDescs(const APackage: TRTTIPackage;
      const AHandle: THandle): Boolean; static;
    class constructor Create;
    class destructor Destroy;
  public
    class var AutoFreeDescs: Boolean;
  end;
  {$ENDIF}

implementation

uses
  Generics.Collections,
  System.DateUtils,
  WiRL.Core.Exceptions,
  WiRL.Core.Utils;

{$IFDEF CUSTOM_ATTRIBUTE_BUG}
type
  TRTTIPackageHelper = class Helper for TRTTIPackage
    function HandleToObject: TDictionary<Pointer,TRttiObject>;
  end;
{$ENDIF}

{ TRttiHelper }

class function TRttiHelper.CreateNewValue(AType: TRttiType): TValue;
var
  LAllocatedMem: Pointer;
begin
  case AType.TypeKind of
    tkInteger: Result := TValue.From<Integer>(0);
    tkInt64:   Result := TValue.From<Int64>(0);
    tkChar:    Result := TValue.From<UTF8Char>(#0);
    tkWChar:   Result := TValue.From<Char>(#0);
    tkFloat:   Result := TValue.From<Double>(0);
    tkString:  Result := TValue.From<UTF8String>('');
    tkWString: Result := TValue.From<string>('');
    tkLString: Result := TValue.From<UTF8String>('');
    tkUString: Result := TValue.From<string>('');
    tkClass:   Result := CreateInstance(AType);
    tkRecord:
    begin
      LAllocatedMem := AllocMem(AType.TypeSize);
      try
        TValue.Make(LAllocatedMem, AType.Handle, Result);
      finally
        FreeMem(LAllocatedMem);
      end;
    end;
  else
    raise EWiRLServerException.CreateFmt('Error creating type: %s', [AType.Name]);
  end;
end;

class destructor TRttiHelper.Destroy;
begin
  FContext.Free;
end;

class function TRttiHelper.CreateInstance(AClass: TClass): TObject;
var
  LType: TRttiType;
begin
  LType := FContext.GetType(AClass);
  Result := CreateInstanceValue(LType).AsObject;
end;

class function TRttiHelper.CreateInstance(AType: TRttiType): TObject;
begin
  Result := CreateInstanceValue(AType).AsObject;
end;

class function TRttiHelper.CreateInstance(const ATypeName: string): TObject;
var
  LType: TRttiType;
begin
  LType := Context.FindType(ATypeName);
  Result := CreateInstanceValue(LType).AsObject;
end;

class function TRttiHelper.CreateInstance(AClass: TClass; const AValue: string): TObject;
var
  LType: TRttiType;
begin
  LType := FContext.GetType(AClass);
  Result := CreateInstance(LType, AValue);
end;

class function TRttiHelper.CreateInstance(AType: TRttiType;
  const AValue: string): TObject;
var
  LMethod: TRttiMethod;
  LMetaClass: TClass;
begin
  Result := nil;
  if Assigned(AType) then
  begin
    for LMethod in AType.GetMethods do
    begin
      if LMethod.HasExtendedInfo and LMethod.IsConstructor then
      begin
        if Length(LMethod.GetParameters) = 1 then
        begin
          if LMethod.GetParameters[0].ParamType.TypeKind in [tkLString, tkUString, tkWString, tkString] then
          begin
            LMetaClass := AType.AsInstance.MetaclassType;
            Exit(LMethod.Invoke(LMetaClass, [AValue]).AsObject);
          end;
        end;
      end;
    end;
  end;
end;

class function TRttiHelper.CreateInstance(const ATypeName, AValue: string): TObject;
var
  LType: TRttiType;
begin
  LType := Context.FindType(ATypeName);
  Result := CreateInstance(LType, AValue);
end;

class function TRttiHelper.CreateInstanceValue(AType: TRttiType): TValue;
var
  LMethod: TRTTIMethod;
  LMetaClass: TClass;
begin
  Result := nil;
  if Assigned(AType) then
    for LMethod in AType.GetMethods do
    begin
      if LMethod.HasExtendedInfo and LMethod.IsConstructor then
      begin
        if Length(LMethod.GetParameters) = 0 then
        begin
          LMetaClass := AType.AsInstance.MetaclassType;
          Exit(LMethod.Invoke(LMetaClass, []));
        end;
      end;
    end;
end;

class function TRttiHelper.FindAttribute<T>(AClass: TClass): T;
begin
  Result := FindAttribute<T>(Context.GetType(AClass));
end;

class function TRttiHelper.ForEachAttribute<T>(AInstance: TObject;
  const ADoSomething: TProc<T>): Integer;
var
  LContext: TRttiContext;
  LType: TRttiType;
begin
  Result := 0;
  LType := LContext.GetType(AInstance.ClassType);
  if Assigned(LType) then
    Result := TRttiHelper.ForEachAttribute<T>(LType, ADoSomething);
end;

class function TRttiHelper.ForEachField(AInstance: TObject;
  const ADoSomething: TFunc<TRttiField, Boolean>): Integer;
var
  LContext: TRttiContext;
  LField: TRttiField;
  LType: TRttiType;
  LBreak: Boolean;
begin
  Result := 0;
  LType := LContext.GetType(AInstance.ClassType);
  for LField in LType.GetFields do
  begin
    LBreak := False;

    if Assigned(ADoSomething) then
    begin
      if not ADoSomething(LField) then
        LBreak := True
      else
        Inc(Result);
    end;

    if LBreak then
      Break;
  end;
end;

class function TRttiHelper.ForEachFieldWithAttribute<T>(AInstance: TObject;
  const ADoSomething: TFunc<TRttiField, T, Boolean>): Integer;
var
  LContext: TRttiContext;
  LType: TRttiType;
begin
  Result := 0;
  LType := LContext.GetType(AInstance.ClassType);
  if Assigned(LType) then
    Result := TRttiHelper.ForEachFieldWithAttribute<T>(LType, ADoSomething);
end;

class function TRttiHelper.IfHasAttribute<T>(AInstance: TObject): Boolean;
begin
  Result := TRttiHelper.IfHasAttribute<T>(AInstance, nil);
end;

class function TRttiHelper.IfHasAttribute<T>(AInstance: TObject;
  const ADoSomething: TProc<T>): Boolean;
var
  LContext: TRttiContext;
  LType: TRttiType;
begin
  Result := False;
  LType := LContext.GetType(AInstance.ClassType);
  if Assigned(LType) then
    Result := TRttiHelper.HasAttribute<T>(LType, ADoSomething);
end;

class function TRttiHelper.ForEachAttribute<T>(ARttiObj: TRttiObject;
    const ADoSomething: TProc<T>): Integer;
var
  LAttribute: TCustomAttribute;
begin
  Result := 0;
  for LAttribute in ARttiObj.GetAttributes do
  begin
    if LAttribute.InheritsFrom(TClass(T)) then
    begin
      if Assigned(ADoSomething) then
        ADoSomething(T(LAttribute));
      Inc(Result);
    end;
  end;
end;

class function TRttiHelper.HasAttribute<T>(ARttiObj: TRttiObject): Boolean;
begin
  Result := HasAttribute<T>(ARttiObj, nil);
end;

class function TRttiHelper.HasAttribute<T>(ARttiObj: TRttiObject; const
    ADoSomething: TProc<T>): Boolean;
var
  LAttribute: TCustomAttribute;
begin
  Result := False;
  for LAttribute in ARttiObj.GetAttributes do
  begin
    if LAttribute.InheritsFrom(TClass(T)) then
    begin
      Result := True;

      if Assigned(ADoSomething) then
        ADoSomething(T(LAttribute));

      Break;
    end;
  end;
end;

class function TRttiHelper.ForEachFieldWithAttribute<T>(ARttiType: TRttiType;
  const ADoSomething: TFunc<TRttiField, T, Boolean>): Integer;
var
  LField: TRttiField;
  LBreak: Boolean;
begin
  Result := 0;
  for LField in ARttiType.GetFields do
  begin
    LBreak := False;
    if TRttiHelper.HasAttribute<T>(LField,
       procedure (AAttrib: T)
       begin
         if Assigned(ADoSomething) then
         begin
           if not ADoSomething(LField, AAttrib) then
             LBreak := True;
         end;
       end
    )
    then
      Inc(Result);

    if LBreak then
      Break;
  end;
end;

class function TRttiHelper.ForEachMethodWithAttribute<T>(ARttiType: TRttiType;
  const ADoSomething: TFunc<TRttiMethod, T, Boolean>): Integer;
var
  LMethod: TRttiMethod;
  LBreak: Boolean;
begin
  Result := 0;
  for LMethod in ARttiType.GetMethods do
  begin
    LBreak := False;
    if TRttiHelper.HasAttribute<T>(LMethod,
       procedure (AAttrib: T)
       begin
         if Assigned(ADoSomething) then
         begin
           if not ADoSomething(LMethod, AAttrib) then
             LBreak := True;
         end;
       end
    )
    then
      Inc(Result);

    if LBreak then
      Break;
  end;
end;

class function TRttiHelper.ForEachPropertyWithAttribute<T>(ARttiType: TRttiType;
  const ADoSomething: TFunc<TRttiProperty, T, Boolean>): Integer;
var
  LProperty: TRttiProperty;
  LBreak: Boolean;
begin
  Result := 0;
  for LProperty in ARttiType.GetProperties do
  begin
    LBreak := False;
    if TRttiHelper.HasAttribute<T>(LProperty,
       procedure (AAttrib: T)
       begin
         if Assigned(ADoSomething) then
         begin
           if not ADoSomething(LProperty, AAttrib) then
             LBreak := True;
         end;
       end
    )
    then
      Inc(Result);

    if LBreak then
      Break;
  end;
end;

class function TRttiHelper.GetType(AObject: TRttiObject): TRttiType;
begin
  if AObject is TRttiParameter then
    Result := TRttiParameter(AObject).ParamType
  else if AObject is TRttiField then
    Result := TRttiField(AObject).FieldType
  else if AObject is TRttiProperty then
    Result := TRttiProperty(AObject).PropertyType
  else if AObject is TRttiManagedField then
    Result := TRttiManagedField(AObject).FieldType
  else
    raise EWiRLServerException.Create('Object doesn''t have a type');
end;

class function TRttiHelper.GetType(ATypeInfo: Pointer): TRttiType;
begin
  Result := Context.GetType(ATypeInfo)
end;

class function TRttiHelper.HasAttribute<T>(AClass: TClass): Boolean;
begin
  Result := HasAttribute<T>(Context.GetType(AClass));
end;

class function TRttiHelper.IsDynamicArrayOf(ARttiType: TRttiType;
  const AClass: TClass; const AAllowInherithance: Boolean): Boolean;
begin
  Result := False;
  if ARttiType is TRttiDynamicArrayType then
    Result := TRttiHelper.IsObjectOfType(
      TRttiDynamicArrayType(ARttiType).ElementType, AClass, AAllowInherithance);
end;

class function TRttiHelper.IsDynamicArrayOf<T>(ARttiType: TRttiType;
  const AAllowInherithance: Boolean): Boolean;
begin
  Result := TRttiHelper.IsDynamicArrayOf(ARttiType, TClass(T), AAllowInherithance);
end;

class function TRttiHelper.IsInterfaceOfType(ATypeInfo: Pointer;
  const IID: TGUID; const AAllowInherithance: Boolean): Boolean;
var
  LType: TRttiType;
begin
  LType := TRttiHelper.Context.GetType(ATypeInfo);
  Result := TRttiHelper.IsInterfaceOfType(LType, IID);
end;

class function TRttiHelper.IsInterfaceOfType(ARttiType: TRttiType;
  const IID: TGUID; const AAllowInherithance: Boolean): Boolean;
var
  LInterfaceType: TRttiInterfaceType;
begin
  Result := False;
  if ARttiType is TRttiInterfaceType then
  begin
    LInterfaceType := TRttiInterfaceType(ARttiType);
    repeat
      if LInterfaceType.GUID = IID then
        Exit(True);
      LInterfaceType := LInterfaceType.BaseType;
    until (LInterfaceType = nil) or (not AAllowInherithance);
  end;
end;

class function TRttiHelper.IsObjectOfType(ARttiType: TRttiType;
  const AClass: TClass; const AAllowInherithance: Boolean): Boolean;
begin
  Result := False;
  if ARttiType is TRttiInstanceType then
  begin
    if AAllowInherithance then
      Result := TRttiInstanceType(ARttiType).MetaclassType.InheritsFrom(AClass)
    else
      Result := TRttiInstanceType(ARttiType).MetaclassType = AClass;
  end;
end;

class function TRttiHelper.IsObjectOfType<T>(ARttiType: TRttiType;
  const AAllowInherithance: Boolean): Boolean;
begin
  Result := TRttiHelper.IsObjectOfType(ARttiType, TClass(T), AAllowInherithance);
end;

class function TRttiHelper.ObjectAsType<T>(AObject: TObject): T;
begin
  Result := TValue.From<TObject>(AObject).AsType<T>;
end;

class function TRttiHelper.FindAttribute<T>(AType: TRttiObject): T;
var
  LAttribute: TCustomAttribute;
begin
  Result := nil;
  for LAttribute in AType.GetAttributes do
  begin
    if LAttribute.InheritsFrom(TClass(T)) then
    begin
      Result := LAttribute as T;

      Break;
    end;
  end;
end;

class function TRttiHelper.CreateInstance(AClass: TClass;
  const Args: array of TValue): TObject;
var
  LType: TRttiType;
begin
  LType := FContext.GetType(AClass);
  Result := CreateInstance(LType, Args);
end;

class function TRttiHelper.CreateInstance(AType: TRttiType;
  const Args: array of TValue): TObject;
var
  LMethod: TRttiMethod;
  LMetaClass: TClass;
begin
  Result := nil;
  if Assigned(AType) then
  begin
    for LMethod in AType.GetMethods do
    begin
      if LMethod.HasExtendedInfo and LMethod.IsConstructor then
      begin
        if Length(LMethod.GetParameters) = Length(Args) then
        begin
          LMetaClass := AType.AsInstance.MetaclassType;
          Exit(LMethod.Invoke(LMetaClass, Args).AsObject);
        end;
      end;
    end;
  end;
  if not Assigned(Result) then
    raise EWiRLServerException.CreateFmt('TRttiHelper.CreateInstance: can''t create object [%s]', [AType.Name]);
end;

class constructor TRttiHelper.Create;
begin
  FContext := TRttiContext.Create;
end;

class function TRttiHelper.CreateArrayValue(ATypeInfo: PTypeInfo;
  ALength: NativeInt): TValue;
var
  LArrayPtr: Pointer;
begin
  LArrayPtr := nil;
  DynArraySetLength(LArrayPtr, ATypeInfo, 1, @ALength);
  try
    TValue.Make(@LArrayPtr, ATypeInfo, Result); // makes copy of array
  finally
    DynArrayClear(LArrayPtr, ATypeInfo);
  end;
end;

class function TRttiHelper.CreateInstance(const ATypeName: string;
  const Args: array of TValue): TObject;
var
  LType: TRttiType;
begin
  LType := Context.FindType(ATypeName);
  Result := CreateInstance(LType, Args);
end;

class function TRttiHelper.CreateArrayValue(AType: TRttiType;
  ALength: NativeInt): TValue;
begin
  Result := CreateArrayValue(AType.Handle, ALength);
end;

{$IFDEF CUSTOM_ATTRIBUTE_BUG}
// Call back routine
procedure OnUnloadModule(const AInstance :THandle);
var
  LPackage :TRTTIPackage;
begin
  { If descriptors related to the AInstance package must be freed now.
    NOTE: This helps to resolve a bug related to packages with custom
    attributes (quality report RSP-11620). }
  if TRttiPatch.AutoFreeDescs then
    for LPackage In TRttiHelper.Context.GetPackages do
      if TRttiPatch.CheckFreeDescs(LPackage, AInstance) then
        Break;
end;

class constructor TRttiPatch.Create;
begin
  AddModuleUnloadProc (TModuleUnloadProcLW (@OnUnloadModule));
  AutoFreeDescs := False;
end;

class destructor TRttiPatch.Destroy;
var
  LPackage :TRTTIPackage;
begin
  for LPackage in TRttiHelper.Context.GetPackages do
    if not (AutoFreeDescs and CheckFreeDescs (LPackage, HInstance)) then
      { NOTE: This call must be done here, before the Finalization section of
        the System unit calls FinalizeMemoryManager. }
      RemoveModuleUnloadProc(TModuleUnloadProcLW(@OnUnloadModule));
end;

class function TRttiPatch.CheckFreeDescs(const APackage :TRTTIPackage;
  const AHandle :THandle) :Boolean;
begin
  Result := APackage.Handle = AHandle;
  if Result then
    // free the TRTTIObject instances (descriptors) of the package
    APackage.HandleToObject.Clear;
end;

{ TRTTIPackageHelper }

function TRTTIPackageHelper.HandleToObject: TDictionary<Pointer, TRttiObject>;
begin
  with Self do
    Result := FHandleToObject;
end;
{$ENDIF}

initialization
{$IFDEF CUSTOM_ATTRIBUTE_BUG}
  TRttiPatch.ClassName;
{$ENDIF}

end.
