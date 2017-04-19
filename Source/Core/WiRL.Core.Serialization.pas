{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Serialization;

interface

uses
  System.SysUtils, System.Classes, System.Rtti, System.SyncObjs,
  WiRL.Core.JSON, REST.Json, System.JSON;


type
  EWiRLJSONMapperException = class(Exception);

  TWiRLJSONSerializer = class
  private
    function WriteString(const AValue: TValue): TJSONValue;
    function WriteChar(const AValue: TValue): TJSONValue;
    function WriteEnum(const AValue: TValue): TJSONValue;
    function WriteInteger(const AValue: TValue): TJSONValue;
    function WriteFloat(const AValue: TValue): TJSONValue;
    function WriteObject(const AValue: TValue): TJSONValue;
    function WriteInterface(const AValue: TValue): TJSONValue;
    function WriteArray(const AValue: TValue): TJSONValue;
    function WriteRecord(const AValue: TValue): TJSONValue;
    function WriteSet(const AValue: TValue): TJSONValue;
    function WriteVariant(const AValue: TValue): TJSONValue;

    function WriteDataMembers(AValue: TValue): TJSONValue;
  public
    function RecordToJSON(ARecord: TValue): TJSONObject;
    function ObjectToJSON(AObject: TObject): TJSONObject;
  end;

  TWiRLJSONDeserializer = class
  private
    function ReadString(AJSONValue: TJSONValue; AType: TRttiType; AKind: TTypeKind): TValue;
    function ReadChar(AJSONValue: TJSONValue; AType: TRttiType; AKind: TTypeKind): TValue;
    function ReadEnum(AJSONValue: TJSONValue; AType: TRttiType): TValue;
    function ReadInteger(AJSONValue: TJSONValue; AType: TRttiType): TValue;
    function ReadInt64(AJSONValue: TJSONValue; AType: TRttiType): TValue;
    function ReadFloat(AJSONValue: TJSONValue; AType: TRttiType): TValue;
    function ReadInterface(AJSONValue: TJSONValue; AType: TRttiType): TValue;
    function ReadArray(AJSONValue: TJSONValue; AType: TRttiType; const ADataValue: TValue): TValue;
    function ReadDynArray(AJSONValue: TJSONValue; AType: TRttiType; const ADataValue: TValue): TValue;
    function ReadSet(AJSONValue: TJSONValue; AType: TRttiType): TValue;
    function ReadVariant(AJSONValue: TJSONValue; AType: TRttiType): TValue;
  private
    procedure ReadDataMembers(AJSONValue: TJSONValue; AType: TRttiType; AData: Pointer);
    function GetValueFromJSON(AInstanceValue: TValue; AJSONValue: TJSONValue; AType: TRttiType): TValue;
    procedure SetMemberValue(const AValue: TValue; AMember: TRttiMember; AInstance: Pointer);
    function CreateNewValue(AType: TRttiType; AItemJSON: TJSONValue; var AAllocatedData: Pointer): TValue;
  public
    procedure JSONToObject(AObject: TObject; AJSONObject: TJSONObject);
  end;

  TWiRLJSONMapper = class
  public
    class function ObjectToJSON(AObject: TObject): TJSONObject;

    class function ObjectToJSONString(AObject: TObject): string;

    class procedure JSONToObject(AObject: TObject; AJSONObject: TJSONObject); overload;

    class function JsonToObject<T: class, constructor>(AJsonObject: TJSOnObject): T; overload;
    class function JsonToObject(AType: TRttiType; AJsonObject: TJSOnObject): TObject; overload;

    class function JsonToObject<T: class, constructor>(const AJson: string): T; overload;
    class function JsonToObject(AType: TRttiType; const AJson: string): TObject; overload;
  end;

implementation

uses
  System.TypInfo,
  WiRL.Rtti.Utils,
  WiRL.Core.Utils;


{ TWiRLJSONSerializer }

function TWiRLJSONSerializer.ObjectToJSON(AObject: TObject): TJSONObject;
var
  LType: TRttiType;
  LProperty: TRttiProperty;
  LJSONValue: TJSONValue;
begin
  Result := TJSONObject.Create;
  try
    if Assigned(AObject) then
    begin
      LType := TRttiHelper.Context.GetType(AObject.ClassType);
      for LProperty in LType.GetProperties do
      begin
        if not LProperty.IsWritable then
          Continue;

        if LProperty.IsReadable and (LProperty.Visibility in [mvPublic, mvPublished]) then
        begin
          try
            LJSONValue := WriteDataMembers(LProperty.GetValue(AObject));
          except
            raise Exception.CreateFmt(
              'Error converting property (%s) of type (%s)',
                [LProperty.Name, LProperty.PropertyType.Name]
            );
          end;
          if Assigned(LJSONValue) then
            Result.AddPair(LProperty.Name, LJSONValue);
        end;
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TWiRLJSONSerializer.RecordToJSON(ARecord: TValue): TJSONObject;
var
  LType: TRttiType;
  LField: TRttiField;
  LJSONValue: TJSONValue;
begin
  Result := TJSONObject.Create;
  try
    begin
      LType := TRttiHelper.Context.GetType(ARecord.TypeInfo);
      for LField in LType.GetFields do
      begin
        if LField.Visibility in [mvPublic, mvPublished] then
        begin
          try
            LJSONValue := WriteDataMembers(LField.GetValue(ARecord.GetReferenceToRawData));
          except
            raise Exception.CreateFmt(
              'Error converting property (%s) of type (%s)',
                [LField.Name, LField.FieldType.Name]
            );
          end;
          if Assigned(LJSONValue) then
            Result.AddPair(LField.Name, LJSONValue);
        end;
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TWiRLJSONSerializer.WriteArray(const AValue: TValue): TJSONValue;
var
  LIndex: Integer;
begin
  Result := TJSONArray.Create;
  for LIndex := 0 to AValue.GetArrayLength - 1 do
  begin
    (Result as TJSONArray).AddElement(
      WriteDataMembers(AValue.GetArrayElement(LIndex))
    );
  end;
end;

function TWiRLJSONSerializer.WriteChar(const AValue: TValue): TJSONValue;
begin
  if AValue.AsString = #0 then
    Result := TJSONString.Create('')
  else
    Result := TJSONString.Create(AValue.AsString);
end;

function TWiRLJSONSerializer.WriteDataMembers(AValue: TValue): TJSONValue;
begin
  Result := nil;

  case AValue.Kind of
    tkChar,
    tkWChar:
    begin
      Result := WriteChar(AValue);
    end;

    tkString,
    tkLString,
    tkWString,
    tkUString:
    begin
      Result := WriteString(AValue);
    end;

    tkEnumeration:
    begin
      Result := WriteEnum(AValue);
    end;

    tkInteger,
    tkInt64:
    begin
      Result := WriteInteger(AValue);
    end;

    tkFloat:
    begin
      Result := WriteFloat(AValue);
    end;

    tkClass:
    begin
      Result := WriteObject(AValue);
    end;

    tkArray,
    tkDynArray:
    begin
      Result := WriteArray(AValue);
    end;

    tkSet:
    begin
      Result := WriteSet(AValue);
    end;

    tkRecord:
    begin
      Result := WriteRecord(AValue);
    end;

    tkInterface:
    begin
      Result := WriteInterface(AValue);
    end;

    tkVariant:
    begin
      Result := WriteVariant(AValue);
    end;
    {
    tkUnknown,
    tkMethod,
    tkPointer,
    tkProcedure,
    tkClassRef:
    }
  end;
end;

function TWiRLJSONSerializer.WriteEnum(const AValue: TValue): TJSONValue;
begin
  if AValue.TypeInfo = System.TypeInfo(Boolean) then
  begin
    if AValue.AsBoolean then
      Result := TJSONTrue.Create
    else
      Result := TJSONFalse.Create;
  end
  else
    Result := TJSONString.Create(GetEnumName(AValue.TypeInfo, AValue.AsOrdinal));
end;

function TWiRLJSONSerializer.WriteFloat(const AValue: TValue): TJSONValue;
begin
  if (AValue.TypeInfo = System.TypeInfo(TDateTime)) or
     (AValue.TypeInfo = System.TypeInfo(TDate)) or
     (AValue.TypeInfo = System.TypeInfo(TTime)) then
    Result := TJSONString.Create(TJSONHelper.DateToJSON(AValue.AsType<TDateTime>))
  else
    Result := TJSONNumber.Create(AValue.AsExtended);
end;

function TWiRLJSONSerializer.WriteInteger(const AValue: TValue): TJSONValue;
begin
  Result := TJSONNumber.Create(AValue.AsInt64);
end;

function TWiRLJSONSerializer.WriteInterface(const AValue: TValue): TJSONValue;
var
  LInterface: IInterface;
  LObject: TObject;
begin
  LInterface := AValue.AsInterface;
  LObject := LInterface as TObject;
  Result := WriteObject(LObject);
end;

function TWiRLJSONSerializer.WriteObject(const AValue: TValue): TJSONValue;
begin
  Result := ObjectToJSON(AValue.AsObject);
end;

function TWiRLJSONSerializer.WriteRecord(const AValue: TValue): TJSONValue;
begin
  Result := RecordToJSON(AValue);
end;

function TWiRLJSONSerializer.WriteSet(const AValue: TValue): TJSONValue;
begin
  Result := TJSONString.Create(SetToString(AValue.TypeInfo, AValue.GetReferenceToRawData, True));
end;

function TWiRLJSONSerializer.WriteString(const AValue: TValue): TJSONValue;
begin
  Result := TJSONString.Create(AValue.AsString);
end;

function TWiRLJSONSerializer.WriteVariant(const AValue: TValue): TJSONValue;
begin
  Result := TJSONString.Create(AValue.AsString);
end;

{ TWiRLJSONDeserializer }

function TWiRLJSONDeserializer.ReadArray(AJSONValue: TJSONValue; AType:
    TRttiType; const ADataValue: TValue): TValue;
var
  LIndex: NativeInt;
  LValue: TValue;
  LItemType: TRttiType;
  LJSONArray: TJSONArray;
  LJSONItem: TJSONValue;
begin
  // Free previous elements?

  Result := ADataValue;
  LJSONArray := AJSONValue as TJSONArray;
  LItemType := (AType as TRttiArrayType).ElementType;

  // Check static array bounds
  for LIndex := 0 to LJSONArray.Count - 1 do
  begin
    LJSONItem := LJSONArray.Items[LIndex];
    LValue := GetValueFromJSON(Result, LJSONItem, LItemType);
    Result.SetArrayElement(LIndex, LValue);
  end;

end;

function TWiRLJSONDeserializer.ReadChar(AJSONValue: TJSONValue; AType:
    TRttiType; AKind: TTypeKind): TValue;
begin
  if (AJSONValue is TJSONNull) or AJSONValue.Value.IsEmpty then
    Exit(#0);

  case AKind of
    // AnsiChar
    tkChar:  Result := TValue.From<AnsiChar>(AnsiChar(AJSONValue.Value.Chars[0]));

    // WideChar
    tkWChar: Result := TValue.From<Char>(AJSONValue.Value.Chars[0]);
  end;
end;

procedure TWiRLJSONDeserializer.ReadDataMembers(AJSONValue: TJSONValue; AType:
    TRttiType; AData: Pointer);
var
  LField: TRttiField;
  LValue: TValue;
  LProperty: TRttiProperty;
  LJSONObject: TJSONObject;
  LJSONValue: TJSONValue;
  LVisibility: set of TMemberVisibility;
begin
  if not Assigned(AData) then
    Exit;

  // Objects, Records, Interfaces are all represented by JSON objects
  LJSONObject := AJSONValue as TJSONObject;

  case AType.TypeKind of
    tkClass:     LVisibility := [mvPublic, mvPublished];
    tkRecord:    LVisibility := [mvPublic];
    tkInterface: LVisibility := [mvPrivate, mvProtected, mvPublic, mvPublished];
  end;

  if (AType.TypeKind = tkClass) or (AType.TypeKind = tkInterface) then
  for LProperty in AType.GetProperties do
  begin
    if not LProperty.IsWritable then
      Continue;

    if LProperty.Visibility in LVisibility then
    begin
      LJSONValue := LJSONObject.GetValue(LProperty.Name);

      // Property not found in JSON, continue to the next one
      if not Assigned(LJSONValue) then
        Continue;

      LValue := GetValueFromJSON(LProperty.GetValue(AData), LJSONValue, LProperty.PropertyType);
      SetMemberValue(LValue, LProperty, AData);
    end;
  end;

  if AType.TypeKind = tkRecord then
  for LField in AType.GetFields do
  begin
    if LField.Visibility in LVisibility then
    begin
      LJSONValue := LJSONObject.GetValue(LField.Name);

      // Property not found in JSON, continue to the next one
      if not Assigned(LJSONValue) then
        Continue;

      LValue := GetValueFromJSON(LField.GetValue(AData), LJSONValue, LField.FieldType);
      SetMemberValue(LValue, LField, AData);
    end;
  end;
end;

function TWiRLJSONDeserializer.ReadDynArray(AJSONValue: TJSONValue; AType:
    TRttiType; const ADataValue: TValue): TValue;
var
  LIndex: NativeInt;
  LValue: TValue;
  LItemType: TRttiType;
  LArrayLength: NativeInt;
  LJSONArray: TJSONArray;
  LJSONItem: TJSONValue;
  LAllocated: Pointer;
begin
  LAllocated := nil;
  // Clear previous elements?
  Result := ADataValue;

  LJSONArray := AJSONValue as TJSONArray;
  LItemType := (AType as TRttiDynamicArrayType).ElementType;
  LArrayLength := LJSONArray.Count;
  DynArraySetLength(PPointer(Result.GetReferenceToRawData)^, Result.TypeInfo, 1, @LArrayLength);

  for LIndex := 0 to LJSONArray.Count - 1 do
  begin
    LJSONItem := LJSONArray.Items[LIndex];

    LValue := CreateNewValue(LItemType, LJSONItem, LAllocated);

    GetValueFromJSON(LValue, LJSONItem, LItemType);
    Result.SetArrayElement(LIndex, LValue);
  end;
end;

function TWiRLJSONDeserializer.ReadEnum(AJSONValue: TJSONValue; AType: TRttiType): TValue;
begin
  if AType.Handle = System.TypeInfo(Boolean) then
  begin
    if AJSONValue is TJSONTrue then
      Result := True
    else if AJSONValue is TJSONFalse then
      Result := False
    else
      raise EWiRLJSONMapperException.Create('Invalid JSON value. Boolean expected');
  end
  else
  begin
    TValue.Make(GetEnumValue(AType.Handle, AJSONValue.Value), AType.Handle, Result);
  end;
end;

function TWiRLJSONDeserializer.ReadFloat(AJSONValue: TJSONValue; AType:
    TRttiType): TValue;
begin
  if AJSONValue is TJSONNull then
    Exit(0);

  if AType.Handle = System.TypeInfo(TDate) then
    Result := TValue.From<TDate>(TJSONHelper.JSONToDate(AJSONValue.Value, True))
  else if AType.Handle = System.TypeInfo(TTime) then
    Result := TValue.From<TTime>(TJSONHelper.JSONToDate(AJSONValue.Value, True))
  else if AType.Handle = System.TypeInfo(TDateTime) then
    Result := TValue.From<TDateTime>(TJSONHelper.JSONToDate(AJSONValue.Value, True))
  else
  begin
    if AJSONValue is TJSONNumber then
      Result := (AJSONValue as TJSONNumber).AsDouble
    else
      raise EWiRLJSONMapperException.Create('Invalid JSON value. Float expected');
  end;
end;

function TWiRLJSONDeserializer.ReadInt64(AJSONValue: TJSONValue; AType:
    TRttiType): TValue;
var
  LNumber: TJSONNumber;
begin
  if AJSONValue is TJSONNull then
    Exit(0);

  LNumber := AJSONValue as TJSONNumber;
  Result := LNumber.AsInt64
end;

function TWiRLJSONDeserializer.ReadInteger(AJSONValue: TJSONValue; AType:
    TRttiType): TValue;
var
  LNumber: TJSONNumber;
begin
  if AJSONValue is TJSONNull then
    Exit(0);

  LNumber := AJSONValue as TJSONNumber;
  Result := LNumber.AsInt;
end;

function TWiRLJSONDeserializer.ReadInterface(AJSONValue: TJSONValue; AType:
    TRttiType): TValue;
begin

end;

function TWiRLJSONDeserializer.ReadSet(AJSONValue: TJSONValue; AType:
    TRttiType): TValue;
begin
  TValue.Make(StringToSet(AType.Handle, AJSONValue.Value), aType.Handle, Result);
end;

function TWiRLJSONDeserializer.ReadString(AJSONValue: TJSONValue; AType:
    TRttiType; AKind: TTypeKind): TValue;
begin
  case AKind of
    // AnsiString
    tkLString: Result := TValue.From<AnsiString>(AnsiString(AJSONValue.Value));

    //WideString
    tkWString: Result := TValue.From<WideString>(AJSONValue.Value);

    //UnicodeString
    tkUString: Result := TValue.From<string>(AJSONValue.Value);

    //ShortString
    tkString:  Result := TValue.From<ShortString>(AnsiString(AJSONValue.Value));

    // Future string types treated as unicode strings
    else
      Result := AJSONValue.Value;
  end;
end;

function TWiRLJSONDeserializer.ReadVariant(AJSONValue: TJSONValue; AType:
    TRttiType): TValue;
begin

end;

procedure TWiRLJSONDeserializer.SetMemberValue(const AValue: TValue; AMember:
    TRttiMember; AInstance: Pointer);
begin
  if AValue.IsEmpty then
    Exit;

  if AMember is TRttiField then
    (AMember as TRttiField).SetValue(AInstance, AValue)
  else if AMember is TRttiProperty then
    (AMember as TRttiProperty).SetValue(AInstance, AValue)
end;


function TWiRLJSONDeserializer.CreateNewValue(AType: TRttiType; AItemJSON:
    TJSONValue; var AAllocatedData: Pointer): TValue;
begin
  AAllocatedData := nil;
  case aType.TypeKind of
    tkInteger: Result := TValue.From<Integer>(0);
    tkInt64:   Result := TValue.From<Int64>(0);
    tkChar:    Result := TValue.From<Char>(#0);
    tkWChar:   Result := TValue.From<WideChar>(#0);
    tkFloat:   Result := TValue.From<Double>(0);
    tkString:  Result := TValue.From<string>('');
    tkWString: Result := TValue.From<WideString>('');
    tkLString: Result := TValue.From<AnsiString>('');
    tkUString: Result := TValue.From<string>('');
    tkClass:   Result := TRttiHelper.CreateInstanceValue(AType);
    {
    tkRecord:
    begin
      AAllocatedData := AllocMem(aType.TypeSize);
      TValue.Make(AAllocatedData, aType.Handle, Result);
      CheckRecordForCreateClasses(Result.GetReferenceToRawData, aType, aItemNode);
    end;
    }
  else
    raise EWiRLJSONMapperException.Create('Item not supported');
  end;
end;

function TWiRLJSONDeserializer.GetValueFromJSON(AInstanceValue: TValue;
    AJSONValue: TJSONValue; AType: TRttiType): TValue;
begin
  Result := TValue.Empty;

  case AType.TypeKind of
    // Simple types recursive calls
    tkInt64:       Result := ReadInt64(AJSONValue, AType);
    tkInteger:     Result := ReadInteger(AJSONValue, AType);
    tkChar:        Result := ReadChar(AJSONValue, AType, AType.TypeKind);
    tkWChar:       Result := ReadChar(AJSONValue, AType, AType.TypeKind);
    tkEnumeration: Result := ReadEnum(AJSONValue, AType);
    tkFloat:       Result := ReadFloat(AJSONValue, AType);
    tkLString:     Result := ReadString(AJSONValue, AType, AType.TypeKind);
    tkWString:     Result := ReadString(AJSONValue, AType, AType.TypeKind);
    tkUString:     Result := ReadString(AJSONValue, AType, AType.TypeKind);
    tkString:      Result := ReadString(AJSONValue, AType, AType.TypeKind);
    tkSet:         Result := ReadSet(AJSONValue, AType);
    tkVariant:     Result := ReadVariant(AJSONValue, AType);
    tkArray:       Result := ReadArray(AJSONValue, AType, AInstanceValue);
    tkDynArray:    Result := ReadDynArray(AJSONValue, AType, AInstanceValue);

    // Complex types recursive calls
    tkClass:       ReadDataMembers(AJSONValue, AType, AInstanceValue.AsObject);
    //tkInterface:   ReadDataMembers(AValue, AType, ADataValue.AsObject);
    //tkRecord:      ReadDataMembers(AValue, AType, ADataValue.GetReferenceToRawData);

    // Not supported (yet)
    {
    tkUnknown: ;
    tkClassRef: ;
    tkPointer: ;
    tkMethod: ;
    tkProcedure: ;
    }
  end;
end;

procedure TWiRLJSONDeserializer.JSONToObject(AObject: TObject; AJSONObject:
    TJSONObject);
var
  LType: TRttiType;
begin
  LType := TRttiHelper.Context.GetType(AObject.ClassType);
  ReadDataMembers(AJSONObject, LType, AObject);
end;

{ TWiRLJSONMapper }

class function TWiRLJSONMapper.ObjectToJSON(AObject: TObject): TJSONObject;
var
  LWriter: TWiRLJSONSerializer;
begin
  LWriter := TWiRLJSONSerializer.Create;
  try
    Result := LWriter.ObjectToJSON(AObject);
  finally
    LWriter.Free;
  end;
end;

class function TWiRLJSONMapper.ObjectToJSONString(AObject: TObject): string;
var
  LObj: TJSONObject;
begin
  LObj := ObjectToJSON(AObject);
  try
    Result := TJSONHelper.ToJSON(LObj);
  finally
    LObj.Free;
  end;
end;

class function TWiRLJSONMapper.JsonToObject(AType: TRttiType; const AJson: string): TObject;
var
  LJObj: TJSONObject;
begin
  Result := TRttiHelper.CreateInstance(AType);
  LJObj := TJSONObject.ParseJSONValue(AJson) as TJSONObject;
  try
    TJson.JsonToObject(Result, LJObj);
  finally
    LJObj.Free;
  end;
end;

class function TWiRLJSONMapper.JsonToObject(AType: TRttiType; AJsonObject: TJSOnObject): TObject;
begin
  Result := TRttiHelper.CreateInstance(AType);
  TJson.JsonToObject(Result, AJsonObject);
end;

class procedure TWiRLJSONMapper.JSONToObject(AObject: TObject; AJSONObject: TJSONObject);
var
  LReader: TWiRLJSONDeserializer;
begin
  LReader := TWiRLJSONDeserializer.Create;
  try
    LReader.JSONToObject(AObject, AJSONObject);
  finally
    LReader.Free;
  end;
end;

class function TWiRLJSONMapper.JsonToObject<T>(const AJson: string): T;
begin
  Result := TJson.JsonToObject<T>(AJson);
end;

class function TWiRLJSONMapper.JsonToObject<T>(AJsonObject: TJSOnObject): T;
begin
  Result := TJson.JsonToObject<T>(AJsonObject);
end;

end.
