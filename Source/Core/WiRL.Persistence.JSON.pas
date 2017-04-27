{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Persistence.JSON;

interface

uses
  System.SysUtils, System.Classes, System.Rtti, System.SyncObjs,
  System.JSON, Data.DB,
  WiRL.Persistence.Attributes,
  WiRL.Persistence.Core,
  WiRL.Persistence.Utils,
  WiRL.Core.JSON;


type
  TNeonSerializerJSON = class(TNeonBase)
  private
    function WriteString(const AValue: TValue): TJSONValue;
    function WriteChar(const AValue: TValue): TJSONValue;
    function WriteEnum(const AValue: TValue): TJSONValue;
    function WriteInteger(const AValue: TValue): TJSONValue;
    function WriteFloat(const AValue: TValue): TJSONValue;
    function WriteInterface(const AValue: TValue): TJSONValue;
    function WriteVariant(const AValue: TValue): TJSONValue;
    /// <summary>
    ///   Writer for static and dynamic arrays
    /// </summary>
    function WriteArray(const AValue: TValue): TJSONValue;
    /// <summary>
    ///   Writer for the set type
    /// </summary>
    /// <remarks>
    ///   The output is a string with the values comma separated and enclosed by square brackets
    /// </remarks>
    /// <returns>[First,Second,Third]</returns>
    function WriteSet(const AValue: TValue): TJSONValue;

    /// <summary>
    ///   Writer for a record type
    /// </summary>
    function WriteRecord(const AValue: TValue): TJSONValue;
    /// <summary>
    ///   Writer for a standard TObject (descendants)  type (no list, stream or streamable)
    /// </summary>
    function WriteObject(const AValue: TValue): TJSONValue;
    /// <summary>
    ///   Writer for TStream (descendants) objects
    /// </summary>
    function WriteStream(const AValue: TValue): TJSONValue;
    /// <summary>
    ///   Writer for TDataSet (descendants) objects
    /// </summary>
    function WriteDataSet(const AValue: TValue): TJSONValue;
    /// <summary>
    ///   Writer for "Enumerable" objects (Lists, Generic Lists, TStrings, etc...)
    /// </summary>
    /// <remarks>
    ///   Objects must have GetEnumerator, Clear, Add methods
    /// </remarks>
    function WriteEnumerator(const AValue: TValue): TJSONValue;
    /// <summary>
    ///   Writer for "Streamable" objects
    /// </summary>
    /// <remarks>
    ///   Objects must have LoadFromStream and SaveToStream methods
    /// </remarks>
    function WriteStreamable(const AValue: TValue): TJSONValue;
  private
    /// <summary>
    ///   This method chooses the right Writer based on the Kind of the AValue parameter
    /// </summary>
    function WriteDataMember(const AValue: TValue): TJSONValue;
  public
    function RecordToJSON(const ARecord: TValue): TJSONValue;
    function ObjectToJSON(AObject: TObject): TJSONValue;
    function TValueToJSON(const AValue: TValue): TJSONValue;
  end;

  TNeonDeserializerJSON = class(TNeonBase)
  private
    function ReadString(AJSONValue: TJSONValue; AType: TRttiType; AKind: TTypeKind): TValue;
    function ReadChar(AJSONValue: TJSONValue; AType: TRttiType; AKind: TTypeKind): TValue;
    function ReadEnum(AJSONValue: TJSONValue; AType: TRttiType): TValue;
    function ReadInteger(AJSONValue: TJSONValue; AType: TRttiType): TValue;
    function ReadInt64(AJSONValue: TJSONValue; AType: TRttiType): TValue;
    function ReadFloat(AJSONValue: TJSONValue; AType: TRttiType): TValue;
    function ReadSet(AJSONValue: TJSONValue; AType: TRttiType): TValue;
    function ReadVariant(AJSONValue: TJSONValue; AType: TRttiType): TValue;
  private
    function ReadArray(AJSONValue: TJSONValue; AType: TRttiType; const AData: TValue): TValue;
    function ReadDynArray(AJSONValue: TJSONValue; AType: TRttiType; const AData: TValue): TValue;
    function ReadObject(AJSONValue: TJSONValue; AType: TRttiType; const AData: TValue): TValue;
    function ReadInterface(AJSONValue: TJSONValue; AType: TRttiType; const AData: TValue): TValue;
    function ReadRecord(AJSONValue: TJSONValue; AType: TRttiType; const AData: TValue): TValue;
    function ReadDataSet(AJSONValue: TJSONValue; AType: TRttiType; const AData: TValue): TValue;
    function ReadStream(AJSONValue: TJSONValue; AType: TRttiType; const AData: TValue): TValue;

    function ReadDataMember(AJSONValue: TJSONValue; AType: TRttiType; const AData: TValue): TValue;
    procedure SetMemberValue(const AValue: TValue; AMember: TRttiMember; AInstance: Pointer);
    function CreateNewValue(AType: TRttiType; AItemJSON: TJSONValue; var AAllocatedData: Pointer): TValue;
  public
    procedure JSONToObject(AObject: TObject; AJSON: TJSONValue);
    function JSONToTValue(AJSON: TJSONValue; AType: TRttiType): TValue; overload;
    function JSONToTValue(AJSON: TJSONValue; AType: TRttiType; const AData: TValue): TValue; overload;
    function JSONToArray(AJSON: TJSONValue; AType: TRttiType): TValue;
  end;

  TNeonMapperJSON = class
  public
    class function ObjectToJSON(AObject: TObject): TJSONValue;
    class function TValueToJSON(const AValue: TValue): TJSONValue;
    class function ObjectToJSONString(AObject: TObject): string;
  public
    class procedure JSONToObject(AObject: TObject; AJSON: TJSONValue); overload;
  end;

implementation

uses
  Winapi.Windows, System.TypInfo, System.NetEncoding,
  WiRL.Rtti.Utils,
  WiRL.Core.Utils;

{ TNeonSerializerJSON }

function TNeonSerializerJSON.ObjectToJSON(AObject: TObject): TJSONValue;
begin
  if not Assigned(AObject) then
    Exit(TJSONObject.Create);

  Result := WriteDataMember(AObject);
  if Assigned(Result) then
    Exit;
end;

function TNeonSerializerJSON.RecordToJSON(const ARecord: TValue): TJSONValue;
begin
  Result := WriteRecord(ARecord);
end;

function TNeonSerializerJSON.TValueToJSON(const AValue: TValue): TJSONValue;
begin
  Result := WriteDataMember(AValue);
end;

function TNeonSerializerJSON.WriteArray(const AValue: TValue): TJSONValue;
var
  LIndex: Integer;
  LArray: TJSONArray;
begin
  LArray := TJSONArray.Create;
  for LIndex := 0 to AValue.GetArrayLength - 1 do
    LArray.AddElement(WriteDataMember(AValue.GetArrayElement(LIndex)));

  Result := LArray;
end;

function TNeonSerializerJSON.WriteChar(const AValue: TValue): TJSONValue;
begin
  if AValue.AsString = #0 then
    Result := TJSONString.Create('')
  else
    Result := TJSONString.Create(AValue.AsString);
end;

function TNeonSerializerJSON.WriteDataMember(const AValue: TValue): TJSONValue;
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
      if AValue.AsObject is TDataSet then
        Result := WriteDataSet(AValue)
      else if AValue.AsObject is TStream then
        Result := WriteStream(AValue)
      else
      begin
        Result := WriteEnumerator(AValue);
        if not Assigned(Result) then
          Result := WriteStreamable(AValue);
        if not Assigned(Result) then
          Result := WriteObject(AValue);
      end;
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

function TNeonSerializerJSON.WriteDataSet(const AValue: TValue): TJSONValue;
var
  LDataSet: TDataSet;
begin
  LDataSet := AValue.AsObject as TDataSet;
  Result := TDataSetUtils.DataSetToJSONArray(LDataSet);
end;

function TNeonSerializerJSON.WriteEnum(const AValue: TValue): TJSONValue;
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

function TNeonSerializerJSON.WriteFloat(const AValue: TValue): TJSONValue;
begin
  if (AValue.TypeInfo = System.TypeInfo(TDateTime)) or
     (AValue.TypeInfo = System.TypeInfo(TDate)) or
     (AValue.TypeInfo = System.TypeInfo(TTime)) then
    Result := TJSONString.Create(TJSONHelper.DateToJSON(AValue.AsType<TDateTime>))
  else
    Result := TJSONNumber.Create(AValue.AsExtended);
end;

function TNeonSerializerJSON.WriteInteger(const AValue: TValue): TJSONValue;
begin
  Result := TJSONNumber.Create(AValue.AsInt64);
end;

function TNeonSerializerJSON.WriteInterface(const AValue: TValue): TJSONValue;
var
  LInterface: IInterface;
  LObject: TObject;
begin
  LInterface := AValue.AsInterface;
  LObject := LInterface as TObject;
  Result := WriteObject(LObject);
end;

function TNeonSerializerJSON.WriteObject(const AValue: TValue): TJSONValue;
var
  LJSONValue: TJSONValue;
  LObject: TObject;
  LType: TRttiType;
  LProperty: TRttiProperty;
begin
  Result := TJSONObject.Create;
  LObject := AValue.AsObject;

  if not Assigned(LObject) then
    Exit(TJSONNull.Create);

  LType := TRttiHelper.Context.GetType(LObject.ClassType);

  for LProperty in LType.GetProperties do
  begin

    if LProperty.Name = 'Parent' then
      Continue;

    if LProperty.Name = 'Owner' then
      Continue;

    if not LProperty.IsWritable and
       not (LProperty.PropertyType.TypeKind in [tkClass, tkInterface]) then
      Continue;

    if LProperty.IsReadable and (LProperty.Visibility in FConfig.Visibility) then
    begin
      try
        LJSONValue := WriteDataMember(LProperty.GetValue(LObject));
        if Assigned(LJSONValue) then
          (Result as TJSONObject).AddPair(LProperty.Name, LJSONValue);
      except
        LogError(Format('Error converting property [%s] [%s type] of object [%s]',
          [LProperty.Name, LProperty.PropertyType.Name, LObject.ClassName]));
      end;
    end;
  end;
end;

function TNeonSerializerJSON.WriteEnumerator(const AValue: TValue): TJSONValue;
var
  LObject: TObject;
  LMethodGetEnumerator, LMethodAdd: TRttiMethod;
  LMethodClear, LMethodMoveNext: TRttiMethod;
  LEnumObject: TObject;
  LListType, LEnumType: TRttiType;
  LCurrentProp: TRttiProperty;
  LValue: TValue;
  LJSONValue: TJSONValue;
begin
  Result := nil;
  LObject := AValue.AsObject;
  LListType := TRttiHelper.Context.GetType(LObject.ClassType);

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

  LEnumObject := LMethodGetEnumerator.Invoke(LObject, []).AsObject;
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

    Result := TJSONArray.Create;
    while LMethodMoveNext.Invoke(LEnumObject, []).AsBoolean do
    begin
      LValue := LCurrentProp.GetValue(LEnumObject);
      LJSONValue := WriteDataMember(LValue);

      (Result as TJSONArray).AddElement(LJSONValue);
    end;

  finally
    LEnumObject.Free;
  end;
end;

function TNeonSerializerJSON.WriteRecord(const AValue: TValue): TJSONValue;
var
  LType: TRttiType;
  LField: TRttiField;
  LJSONValue: TJSONValue;
begin
  Result := TJSONObject.Create;
  LType := TRttiHelper.Context.GetType(AValue.TypeInfo);
  for LField in LType.GetFields do
  begin
    if LField.Visibility in FConfig.Visibility then
    begin
      try
        LJSONValue := WriteDataMember(LField.GetValue(AValue.GetReferenceToRawData));
        if Assigned(LJSONValue) then
          (Result as TJSONObject).AddPair(LField.Name, LJSONValue);
      except
        LogError(Format('Error converting field [%s] [%s type]',
          [LField.Name, LField.FieldType.Name]));
      end;
    end;
  end;
end;

function TNeonSerializerJSON.WriteSet(const AValue: TValue): TJSONValue;
begin
  Result := TJSONString.Create(SetToString(AValue.TypeInfo, AValue.GetReferenceToRawData, True));
end;

function TNeonSerializerJSON.WriteStream(const AValue: TValue): TJSONValue;
var
  LStream: TStream;
  LBase64Stream: TStringStream;
begin
  LStream := AValue.AsObject as TStream;

  LBase64Stream := TStringStream.Create;
  try
    LStream.Position := soFromBeginning;
    TNetEncoding.Base64.Encode(LStream, LBase64Stream);
    Result := TJSONString.Create(LBase64Stream.DataString);
  finally
    LBase64Stream.Free;
  end;
end;

function TNeonSerializerJSON.WriteStreamable(const AValue: TValue): TJSONValue;
var
  LObject: TObject;
  LType: TRttiType;
  LMethodLoadFromStream, LMethodSaveToStream: TRttiMethod;
  LBinaryStream: TMemoryStream;
  LBase64Stream: TStringStream;
begin
  Result := nil;
  LObject := AValue.AsObject;
  LType := TRttiHelper.Context.GetType(LObject.ClassType);

  if not Assigned(LObject) then
    Exit;

  LMethodLoadFromStream := LType.GetMethod('LoadFromStream');
  { TODO -opaolo -c : check parameters (number and type) 24/04/2017 17:50:43 }
  if not Assigned(LMethodLoadFromStream) then
    Exit;

  LMethodSaveToStream := LType.GetMethod('SaveToStream');
  { TODO -opaolo -c : check parameters (number and type) 24/04/2017 17:50:43 }
  if not Assigned(LMethodSaveToStream) then
    Exit;

  LBinaryStream := TMemoryStream.Create;
  LBase64Stream := TStringStream.Create;
  try
    LMethodSaveToStream.Invoke(LObject, [LBinaryStream]);
    LBinaryStream.Position := soFromBeginning;
    TNetEncoding.Base64.Encode(LBinaryStream, LBase64Stream);
    Result := TJSONString.Create(LBase64Stream.DataString);
  finally
    LBinaryStream.Free;
    LBase64Stream.Free;
  end;
end;

function TNeonSerializerJSON.WriteString(const AValue: TValue): TJSONValue;
begin
  Result := TJSONString.Create(AValue.AsString);
end;

function TNeonSerializerJSON.WriteVariant(const AValue: TValue): TJSONValue;
begin
  Result := TJSONString.Create(AValue.AsString);
end;

{ TNeonDeserializerJSON }

function TNeonDeserializerJSON.ReadArray(AJSONValue: TJSONValue; AType:
    TRttiType; const AData: TValue): TValue;
var
  LIndex: NativeInt;
  LItemValue: TValue;
  LItemType: TRttiType;
  LJSONArray: TJSONArray;
  LJSONItem: TJSONValue;
  LAllocated: Pointer;
begin
  // TValue record copy (but the TValue only copy the reference to Data)
  Result := AData;
  LAllocated := nil;

  // Clear (and Free) previous elements?
  LJSONArray := AJSONValue as TJSONArray;
  LItemType := (AType as TRttiArrayType).ElementType;

  // Check static array bounds
  for LIndex := 0 to LJSONArray.Count - 1 do
  begin
    LJSONItem := LJSONArray.Items[LIndex];
    LItemValue := CreateNewValue(LItemType, LJSONItem, LAllocated);
    LItemValue := ReadDataMember(LJSONItem, LItemType, Result);
    Result.SetArrayElement(LIndex, LItemValue);
  end;
end;

function TNeonDeserializerJSON.ReadDynArray(AJSONValue: TJSONValue; AType:
    TRttiType; const AData: TValue): TValue;
var
  LIndex: NativeInt;
  LItemValue: TValue;
  LItemType: TRttiType;
  LArrayLength: NativeInt;
  LJSONArray: TJSONArray;
  LJSONItem: TJSONValue;
  LAllocated: Pointer;
begin
  Result := AData;
  LAllocated := nil;

  // Clear (and Free) previous elements?
  LJSONArray := AJSONValue as TJSONArray;
  LItemType := (AType as TRttiDynamicArrayType).ElementType;
  LArrayLength := LJSONArray.Count;
  DynArraySetLength(PPointer(Result.GetReferenceToRawData)^, Result.TypeInfo, 1, @LArrayLength);

  for LIndex := 0 to LJSONArray.Count - 1 do
  begin
    LJSONItem := LJSONArray.Items[LIndex];

    LItemValue := CreateNewValue(LItemType, LJSONItem, LAllocated);
    LItemValue := ReadDataMember(LJSONItem, LItemType, LItemValue);

    Result.SetArrayElement(LIndex, LItemValue);
  end;
end;

function TNeonDeserializerJSON.ReadChar(AJSONValue: TJSONValue; AType:
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

function TNeonDeserializerJSON.ReadDataMember(AJSONValue: TJSONValue; AType:
    TRttiType; const AData: TValue): TValue;
begin
  case AType.TypeKind of
    // Simple types
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
    tkArray:       Result := ReadArray(AJSONValue, AType, AData);
    tkDynArray:    Result := ReadDynArray(AJSONValue, AType, AData);

    // Complex types recursive calls
    tkClass:
    begin
      if AData.AsObject is TDataSet then
        Result := ReadDataSet(AJSONValue, AType, AData)
      else
        Result := ReadObject(AJSONValue, AType, AData);
    end;
    tkInterface:   Result := ReadInterface(AJSONValue, AType, AData);
    tkRecord:      Result := ReadRecord(AJSONValue, AType, AData);

    // Not supported (yet)
    {
    tkUnknown: ;
    tkClassRef: ;
    tkPointer: ;
    tkMethod: ;
    tkProcedure: ;
    }
    else Result := TValue.Empty;
  end;
end;

function TNeonDeserializerJSON.ReadDataSet(AJSONValue: TJSONValue;
  AType: TRttiType; const AData: TValue): TValue;
var
  LJSONArray: TJSONArray;
  LJSONItem: TJSONObject;
  LJSONField: TJSONValue;
  LDataSet: TDataSet;
  LIndex: Integer;
  LItemIntex: Integer;
  LName: string;
begin
  Result := AData;
  LDataSet := AData.AsObject as TDataSet;
  LJSONArray := AJSONValue as TJSONArray;

  for LIndex := 0 to LJSONArray.Count - 1 do
  begin
    LJSONItem := LJSONArray.Items[LIndex] as TJSONObject;

    LDataSet.Append;
    for LItemIntex := 0 to LDataSet.Fields.Count - 1 do
    begin
      LName := LDataSet.Fields[LItemIntex].FieldName;

      LJSONField := LJSONItem.GetValue(LName);
      if Assigned(LJSONField) then
      begin
        { TODO -opaolo -c : Be more specific (field and json type) 27/04/2017 17:16:09 }
        LDataSet.FieldByName(LName).AsString := LJSONField.Value;
      end;
    end;
    LDataSet.Post;
  end;
end;

function TNeonDeserializerJSON.ReadEnum(AJSONValue: TJSONValue; AType: TRttiType): TValue;
begin
  if AType.Handle = System.TypeInfo(Boolean) then
  begin
    if AJSONValue is TJSONTrue then
      Result := True
    else if AJSONValue is TJSONFalse then
      Result := False
    else
      raise ENeonException.Create('Invalid JSON value. Boolean expected');
  end
  else
  begin
    TValue.Make(GetEnumValue(AType.Handle, AJSONValue.Value), AType.Handle, Result);
  end;
end;

function TNeonDeserializerJSON.ReadFloat(AJSONValue: TJSONValue; AType:
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
      raise ENeonException.Create('Invalid JSON value. Float expected');
  end;
end;

function TNeonDeserializerJSON.ReadInt64(AJSONValue: TJSONValue; AType:
    TRttiType): TValue;
var
  LNumber: TJSONNumber;
begin
  if AJSONValue is TJSONNull then
    Exit(0);

  LNumber := AJSONValue as TJSONNumber;
  Result := LNumber.AsInt64
end;

function TNeonDeserializerJSON.ReadInteger(AJSONValue: TJSONValue; AType: TRttiType): TValue;
var
  LNumber: TJSONNumber;
begin
  if AJSONValue is TJSONNull then
    Exit(0);

  LNumber := AJSONValue as TJSONNumber;
  Result := LNumber.AsInt;
end;

function TNeonDeserializerJSON.ReadInterface(AJSONValue: TJSONValue; AType:
    TRttiType; const AData: TValue): TValue;
begin
  Result := AData;
end;

function TNeonDeserializerJSON.ReadObject(AJSONValue: TJSONValue; AType:
    TRttiType; const AData: TValue): TValue;
var
  LProperty: TRttiProperty;
  LJSONObject: TJSONObject;
  LJSONValue: TJSONValue;
  LPData: Pointer;
  LMemberValue: TValue;
begin
  Result := AData;
  LPData := AData.AsObject;
  LJSONObject := AJSONValue as TJSONObject;

  if (AType.TypeKind = tkClass) or (AType.TypeKind = tkInterface) then
  for LProperty in AType.GetProperties do
  begin
    if not LProperty.IsWritable then
      Continue;

    if LProperty.Visibility in FConfig.Visibility then
    begin
      LJSONValue := LJSONObject.GetValue(LProperty.Name);

      // Property not found in JSON, continue to the next one
      if not Assigned(LJSONValue) then
        Continue;

      LMemberValue := ReadDataMember(LJSONValue, LProperty.PropertyType, LProperty.GetValue(LPData));
      SetMemberValue(LMemberValue, LProperty, LPData);
    end;
  end;
end;

function TNeonDeserializerJSON.ReadRecord(AJSONValue: TJSONValue; AType:
    TRttiType; const AData: TValue): TValue;
var
  LField: TRttiField;
  LJSONObject: TJSONObject;
  LJSONValue: TJSONValue;
  LPData: Pointer;
  LMemberValue: TValue;
begin
  Result := AData;
  LPData := AData.GetReferenceToRawData;

  if not Assigned(LPData) then
    Exit;

  // Objects, Records, Interfaces are all represented by JSON objects
  LJSONObject := AJSONValue as TJSONObject;

  for LField in AType.GetFields do
  begin
    if LField.Visibility in FConfig.Visibility then
    begin
      LJSONValue := LJSONObject.GetValue(LField.Name);

      // Property not found in JSON, continue to the next one
      if not Assigned(LJSONValue) then
        Continue;

      LMemberValue := ReadDataMember(LJSONValue, LField.FieldType, LField.GetValue(LPData));
      SetMemberValue(LMemberValue, LField, LPData);
    end;
  end;
end;

function TNeonDeserializerJSON.ReadSet(AJSONValue: TJSONValue; AType: TRttiType): TValue;
var
  LSetStr: string;
begin
  LSetStr := AJSONValue.Value;
  LSetStr := LSetStr.Replace(sLineBreak, '', [rfReplaceAll]);
  LSetStr := LSetStr.Replace(' ', '', [rfReplaceAll]);
  TValue.Make(StringToSet(AType.Handle, LSetStr), aType.Handle, Result);
end;

function TNeonDeserializerJSON.ReadStream(AJSONValue: TJSONValue;
  AType: TRttiType; const AData: TValue): TValue;
begin

end;

function TNeonDeserializerJSON.ReadString(AJSONValue: TJSONValue; AType:
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

function TNeonDeserializerJSON.ReadVariant(AJSONValue: TJSONValue; AType: TRttiType): TValue;
begin

end;

procedure TNeonDeserializerJSON.SetMemberValue(const AValue: TValue; AMember:
    TRttiMember; AInstance: Pointer);
begin
  if AValue.IsEmpty then
    Exit;

  if AMember is TRttiField then
    (AMember as TRttiField).SetValue(AInstance, AValue)
  else if AMember is TRttiProperty then
    (AMember as TRttiProperty).SetValue(AInstance, AValue)
end;


function TNeonDeserializerJSON.CreateNewValue(AType: TRttiType; AItemJSON:
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
    raise ENeonException.Create('Item not supported');
  end;
end;

function TNeonDeserializerJSON.JSONToArray(AJSON: TJSONValue; AType: TRttiType): TValue;
begin
  Result := ReadDataMember(AJSON, AType, TValue.Empty);
end;

procedure TNeonDeserializerJSON.JSONToObject(AObject: TObject; AJSON: TJSONValue);
var
  LType: TRttiType;
  LValue: TValue;
begin
  LType := TRttiHelper.Context.GetType(AObject.ClassType);
  LValue := AObject;
  ReadDataMember(AJSON, LType, AObject);
end;

function TNeonDeserializerJSON.JSONToTValue(AJSON: TJSONValue; AType: TRttiType;
  const AData: TValue): TValue;
begin
  Result := ReadDataMember(AJSON, AType, AData);
end;

function TNeonDeserializerJSON.JSONToTValue(AJSON: TJSONValue; AType: TRttiType): TValue;
begin
  Result := ReadDataMember(AJSON, AType, TValue.Empty);
end;

{ TNeonMapperJSON }

class function TNeonMapperJSON.ObjectToJSON(AObject: TObject): TJSONValue;
var
  LWriter: TNeonSerializerJSON;
begin
  LWriter := TNeonSerializerJSON.Create(TNeonConfiguration.Default);
  try
    Result := LWriter.ObjectToJSON(AObject);
  finally
    LWriter.Free;
  end;
end;

class function TNeonMapperJSON.ObjectToJSONString(AObject: TObject): string;
var
  LJSON: TJSONValue;
begin
  LJSON := ObjectToJSON(AObject);
  try
    Result := TJSONHelper.ToJSON(LJSON);
  finally
    LJSON.Free;
  end;
end;

class function TNeonMapperJSON.TValueToJSON(const AValue: TValue): TJSONValue;
var
  LWriter: TNeonSerializerJSON;
begin
  LWriter := TNeonSerializerJSON.Create(TNeonConfiguration.Default);
  try
    Result := LWriter.TValueToJSON(AValue);
  finally
    LWriter.Free;
  end;
end;

class procedure TNeonMapperJSON.JSONToObject(AObject: TObject; AJSON: TJSONValue);
var
  LReader: TNeonDeserializerJSON;
begin
  LReader := TNeonDeserializerJSON.Create(TNeonConfiguration.Default);
  try
    LReader.JSONToObject(AObject, AJSON);
  finally
    LReader.Free;
  end;
end;

end.
