unit WiRL.Persistence.DynamicTypes;

interface

uses
  System.Classes, System.SysUtils, Data.DB, System.Rtti, System.TypInfo,
  System.Generics.Collections;

type
  IDynamicType = interface
  ['{DD163E75-134C-4035-809C-D9E1EEEC4225}']
  end;

  IDynamicMember = interface
  ['{18FB3C44-DBB8-408D-B30C-16E29A738FF0}']
    function GetValue(AInstance: Pointer): TValue;
    procedure SetValue(AInstance: Pointer);
    function MemberType: TRttiType;
    function IsWritable: Boolean;
    function IsReadable: Boolean;
  end;

  IDynamicStream = interface(IDynamicType)
  ['{968D03E7-273F-4E94-A3EA-ECB7A73F0715}']
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
  end;

  IDynamicList = interface(IDynamicType)
  ['{9F4A2D72-078B-4EA2-B86E-068206AD0F16}']
    function NewItem: TValue;
    function GetItemType: TRttiType;
    procedure Add(AItem: TValue);
    procedure Clear;
    function Count: Integer;
    // Enumerator functions
    function Current: TValue;
    function MoveNext: Boolean;
  end;

  TDynamicMember = class(TInterfacedObject, IDynamicMember)
  private
    FType: TRttiType;
    FInstance: Pointer;
  public
    constructor Create(AInstance: Pointer; AType: TRttiType);

    function GetValue(AInstance: Pointer): TValue;
    procedure SetValue(AInstance: Pointer);
    function MemberType: TRttiType;
    function IsWritable: Boolean;
    function IsReadable: Boolean;
  end;

  TDynamicStream = class(TInterfacedObject, IDynamicStream)
  private
    FInstance: TObject;
    FLoadMethod: TRttiMethod;
    FSaveMethod: TRttiMethod;
    constructor Create(AInstance: TObject; ALoadMethod, ASaveMethod: TRttiMethod);
  public
    class function GuessType(AInstance: TObject): IDynamicStream;
  public
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
  end;

  TDynamicList = class(TInterfacedObject, IDynamicList)
  private
    FInstance: TObject;
    FItemType: TRttiType;
    FAddMethod: TRttiMethod;
    FClearMethod: TRttiMethod;
    FMoveNextMethod: TRttiMethod;
    FCurrentProperty: TRttiProperty;
    FCountProperty: TRttiProperty;
    constructor Create(AInstance: TObject; AItemType: TRttiType;
      AAddMethod, AClearMethod, AMoveNextMethod: TRttiMethod;
      ACurrentProperty, ACountProperty: TRttiProperty);
  public
    class function GuessType(AInstance: TObject): IDynamicList;
  public
    function NewItem: TValue;
    function GetItemType: TRttiType;
    procedure Add(AItem: TValue);
    procedure Clear;
    function Count: Integer;
    // Enumerator functions
    function Current: TValue;
    function MoveNext: Boolean;
  end;

implementation

uses
  WiRL.Rtti.Utils;

{ TDynamicStream }

constructor TDynamicStream.Create(AInstance: TObject;
  ALoadMethod, ASaveMethod: TRttiMethod);
begin
  FInstance := AInstance;
  FLoadMethod := ALoadMethod;
  FSaveMethod := ASaveMethod;
end;

class function TDynamicStream.GuessType(AInstance: TObject): IDynamicStream;
var
  LType: TRttiType;
  LLoadMethod, LSaveMethod: TRttiMethod;
begin
  if not Assigned(AInstance) then
    Exit(nil);

  LType := TRttiHelper.Context.GetType(AInstance.ClassType);

  if not Assigned(LType) then
    Exit(nil);

  LLoadMethod := LType.GetMethod('LoadFromStream');
  if not Assigned(LLoadMethod) then
    Exit(nil);

  LSaveMethod := LType.GetMethod('SaveToStream');
  if not Assigned(LSaveMethod) then
    Exit(nil);

  Result := Self.Create(AInstance, LLoadMethod, LSaveMethod);
end;

procedure TDynamicStream.LoadFromStream(AStream: TStream);
begin
  FLoadMethod.Invoke(FInstance, [AStream]);
end;

procedure TDynamicStream.SaveToStream(AStream: TStream);
begin
  FSaveMethod.Invoke(FInstance, [AStream]);
end;

{ TDynamicList }

procedure TDynamicList.Add(AItem: TValue);
begin
  FAddMethod.Invoke(FInstance, [AItem]);
end;

procedure TDynamicList.Clear;
begin
  FClearMethod.Invoke(FInstance, []);
end;

function TDynamicList.Count: Integer;
begin
  Result := FCountProperty.GetValue(FInstance).AsInteger;
end;

constructor TDynamicList.Create(AInstance: TObject; AItemType: TRttiType;
  AAddMethod, AClearMethod, AMoveNextMethod: TRttiMethod;
  ACurrentProperty, ACountProperty: TRttiProperty);
begin
  FInstance := AInstance;
  FItemType := AItemType;
  FAddMethod := AAddMethod;
  FClearMethod := AClearMethod;
  FMoveNextMethod := AMoveNextMethod;
  FCurrentProperty := ACurrentProperty;
  FCountProperty := ACountProperty;
end;

function TDynamicList.Current: TValue;
begin
  Result := FCurrentProperty.GetValue(FInstance);
end;

function TDynamicList.GetItemType: TRttiType;
begin
  Result := FItemType;
end;

class function TDynamicList.GuessType(AInstance: TObject): IDynamicList;
var
  LMethodGetEnumerator, LMethodAdd: TRttiMethod;
  LMethodClear, LMethodMoveNext: TRttiMethod;
  LEnumObject: TObject;
  LListType, LItemType, LEnumType: TRttiType;
  LCountProp, LCurrentProp: TRttiProperty;
begin
  Result := nil;
  LListType := TRttiHelper.Context.GetType(AInstance.ClassType);

  LMethodGetEnumerator := LListType.GetMethod('GetEnumerator');
  if not Assigned(LMethodGetEnumerator) or
     (LMethodGetEnumerator.MethodKind <> mkFunction) or
     (LMethodGetEnumerator.ReturnType.Handle.Kind <> tkClass)
  then
    Exit;

  LMethodClear := LListType.GetMethod('Clear');
  if not Assigned(LMethodClear) then
    Exit;

  LMethodAdd := LListType.GetMethod('Add');
  if not Assigned(LMethodAdd) or (Length(LMethodAdd.GetParameters) <> 1) then
    Exit;

  LItemType := LMethodAdd.GetParameters[0].ParamType;

  LCountProp := LListType.GetProperty('Count');
  if not Assigned(LCountProp) then
    Exit;

  LEnumObject := LMethodGetEnumerator.Invoke(AInstance, []).AsObject;
  if not Assigned(LEnumObject) then
    Exit;

  try
    LEnumType := TRttiHelper.Context.GetType(LEnumObject.ClassType);

    LCurrentProp := LEnumType.GetProperty('Current');
    if not Assigned(LCurrentProp) then
      Exit;

    LMethodMoveNext := LEnumType.GetMethod('MoveNext');
    if not Assigned(LMethodMoveNext) or
       (Length(LMethodMoveNext.GetParameters) <> 0) or
       (LMethodMoveNext.MethodKind <> mkFunction) or
       (LMethodMoveNext.ReturnType.Handle <> TypeInfo(Boolean))
    then
      Exit;

    Result := TDynamicList.Create(
      AInstance,
      LItemType,
      LMethodAdd,
      LMethodClear,
      LMethodMoveNext,
      LCurrentProp,
      LCountProp
    );

  finally
    LEnumObject.Free;
  end;
end;

function TDynamicList.MoveNext: Boolean;
begin
  Result := FMoveNextMethod.Invoke(FInstance, []).AsBoolean;
end;

function TDynamicList.NewItem: TValue;
begin
  Result := TRttiHelper.CreateNewValue(FItemType);
end;

{ TDynamicMember }

constructor TDynamicMember.Create(AInstance: Pointer; AType: TRttiType);
begin
  FInstance := AInstance;
  FType := AType;
end;

function TDynamicMember.GetValue(AInstance: Pointer): TValue;
begin

end;

function TDynamicMember.IsReadable: Boolean;
begin

end;

function TDynamicMember.IsWritable: Boolean;
begin

end;

function TDynamicMember.MemberType: TRttiType;
begin

end;

procedure TDynamicMember.SetValue(AInstance: Pointer);
begin

end;

end.
