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
  WiRL.Core.JSON;


type
  TJSONSerializer = class
  private
    class var FContext: TRttiContext;
  public
    class function ValueToJSONValue(AValue: TValue): TJSONValue;
    class function ObjectToJSON(AObject: TObject): TJSONObject;
    class function ObjectToJSONString(AObject: TObject): string;
  end;


implementation

uses
  System.TypInfo,
  WiRL.Core.Utils;

{ TJSONSerializer }

class function TJSONSerializer.ValueToJSONValue(AValue: TValue): TJSONValue;
var
  LIndex: Integer;
begin
  case AValue.Kind of
    tkChar,
    tkString,
    tkWChar,
    tkLString,
    tkWString,
    tkVariant,
    tkUString:
    begin
      Result := TJSONString.Create(AValue.AsString);
    end;

    tkEnumeration:
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

    tkInteger,
    tkInt64:
    begin
      Result := TJSONNumber.Create(AValue.AsInt64);
    end;

    tkFloat:
    begin
      if (AValue.TypeInfo = System.TypeInfo(TDateTime)) or
         (AValue.TypeInfo = System.TypeInfo(TDate)) or
         (AValue.TypeInfo = System.TypeInfo(TTime)) then
        Result := TJSONString.Create(TJSONHelper.DateToJSON(AValue.AsType<TDateTime>))
      else
        Result := TJSONNumber.Create(AValue.AsExtended);
    end;

    tkClass:
    begin
      Result := ObjectToJSON(AValue.AsObject);
    end;

    tkArray,
    tkDynArray:
    begin
      Result := TJSONArray.Create;
      for LIndex := 0 to AValue.GetArrayLength - 1 do
      begin
        (Result as TJSONArray).AddElement(
          ValueToJSONValue(AValue.GetArrayElement(LIndex))
        );
      end;
    end;

    tkSet:
    begin
      Result := TJSONString.Create(AValue.ToString);
    end;

    //tkRecord:

    {
    tkUnknown,
    tkMethod,
    tkPointer,
    tkProcedure,
    tkInterface,
    tkClassRef:
    }

    else
      Result := nil;
  end;
end;

class function TJSONSerializer.ObjectToJSON(AObject: TObject): TJSONObject;
var
  LType: TRttiType;
  LProperty: TRttiProperty;
  LJSONValue: TJSONValue;
begin
  Result := TJSONObject.Create;
  try
    if Assigned(AObject) then
    begin
      LType := FContext.GetType(AObject.ClassType);
      for LProperty in LType.GetProperties do
      begin
        if not LProperty.IsWritable then
          Continue;

        if LProperty.IsReadable and (LProperty.Visibility in [mvPublic, mvPublished]) then
        begin
          try
            LJSONValue := ValueToJSONValue(LProperty.GetValue(AObject));
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

class function TJSONSerializer.ObjectToJSONString(AObject: TObject): string;
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

end.
