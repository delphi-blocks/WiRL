{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Persistence.Utils;

interface

uses
  System.Classes, System.SysUtils, Data.DB, System.Rtti,
  WiRL.Core.JSON, WiRL.Persistence.Core;

type
  TDataSetUtils = class
  private
    class function RecordToXML(const ADataSet: TDataSet; const ARootPath: string; AConfig: TNeonConfiguration): string; static;
    class function RecordToCSV(const ADataSet: TDataSet; AConfig: TNeonConfiguration): string; static;
  public
    class function RecordToJSONObject(const ADataSet: TDataSet; AConfig: TNeonConfiguration): TJSONObject; static;
    class function DataSetToJSONArray(const ADataSet: TDataSet; AConfig: TNeonConfiguration): TJSONArray; overload; static;
    class function DataSetToJSONArray(const ADataSet: TDataSet; const AAcceptFunc: TFunc<Boolean>; AConfig: TNeonConfiguration): TJSONArray; overload; static;

    class function DataSetToXML(const ADataSet: TDataSet; AConfig: TNeonConfiguration): string; overload; static;
    class function DataSetToXML(const ADataSet: TDataSet; const AAcceptFunc: TFunc<Boolean>; AConfig: TNeonConfiguration): string; overload; static;

    class function DataSetToCSV(const ADataSet: TDataSet; AConfig: TNeonConfiguration): string; static;

    class function DatasetMetadataToJSONObject(const ADataSet: TDataSet; AConfig: TNeonConfiguration): TJSONObject; static;
  end;


implementation

uses
  System.StrUtils, System.DateUtils,

  WiRL.Rtti.Utils,
  WiRL.Core.Utils;

type
  TJSONFieldType = (NestedObject, NestedArray, SimpleValue);

class function TDataSetUtils.RecordToCSV(const ADataSet: TDataSet; AConfig: TNeonConfiguration): string;
var
  LField: TField;
begin
  if not Assigned(ADataSet) then
    raise Exception.Create('DataSet not assigned');
  if not ADataSet.Active then
    raise Exception.Create('DataSet is not active');
  if ADataSet.IsEmpty then
    raise Exception.Create('DataSet is empty');

  Result := '';
  for LField in ADataSet.Fields do
  begin
    Result := Result + LField.AsString + ',';
  end;
  Result := Result.TrimRight([',']);
end;

class function TDataSetUtils.RecordToJSONObject(const ADataSet: TDataSet; AConfig: TNeonConfiguration): TJSONObject;
var
  LField: TField;
  LPairName: string;
begin
  Result := TJSONObject.Create;

  for LField in ADataSet.Fields do
  begin
    LPairName := LField.FieldName;

    if ContainsStr(LPairName, '.') then
      Continue;

    case LField.DataType of
      ftString:        Result.AddPair(LPairName, LField.AsString);
      ftSmallint:      Result.AddPair(LPairName, TJSONNumber.Create(LField.AsInteger));
      ftInteger:       Result.AddPair(LPairName, TJSONNumber.Create(LField.AsInteger));
      ftWord:          Result.AddPair(LPairName, TJSONNumber.Create(LField.AsInteger));
      ftBoolean:       Result.AddPair(LPairName, TJSONHelper.BooleanToTJSON(LField.AsBoolean));
      ftFloat:         Result.AddPair(LPairName, TJSONNumber.Create(LField.AsFloat));
      ftCurrency:      Result.AddPair(LPairName, TJSONNumber.Create(LField.AsCurrency));
      ftBCD:           Result.AddPair(LPairName, TJSONNumber.Create(LField.AsFloat));
      ftDate:          Result.AddPair(LPairName, TJSONHelper.DateToJSON(LField.AsDateTime, AConfig.UseUTCDate));
      ftTime:          Result.AddPair(LPairName, TJSONHelper.DateToJSON(LField.AsDateTime, AConfig.UseUTCDate));
      ftDateTime:      Result.AddPair(LPairName, TJSONHelper.DateToJSON(LField.AsDateTime, AConfig.UseUTCDate));
//        ftBytes: ;
//        ftVarBytes: ;
      ftAutoInc:       Result.AddPair(LPairName, TJSONNumber.Create(LField.AsInteger));
//        ftBlob: ;
      ftMemo:          Result.AddPair(LPairName, LField.AsString);
//        ftGraphic: ;
//        ftFmtMemo: ;
//        ftParadoxOle: ;
//        ftDBaseOle: ;
//        ftTypedBinary: ;
//        ftCursor: ;
      ftFixedChar:     Result.AddPair(LPairName, LField.AsString);
      ftWideString:    Result.AddPair(LPairName, LField.AsWideString);
      ftLargeint:      Result.AddPair(LPairName, TJSONNumber.Create(LField.AsLargeInt));
//        ftADT: ;
//        ftArray: ;
//        ftReference: ;
//        ftDataSet: ;
//        ftOraBlob: ;
//        ftOraClob: ;
      ftVariant:       Result.AddPair(LPairName, LField.AsString);
//        ftInterface: ;
//        ftIDispatch: ;
      ftGuid:          Result.AddPair(LPairName, LField.AsString);
      ftTimeStamp:     Result.AddPair(LPairName, TJSONHelper.DateToJSON(LField.AsDateTime, AConfig.UseUTCDate));
      ftFMTBcd:        Result.AddPair(LPairName, TJSONNumber.Create(LField.AsFloat));
      ftFixedWideChar: Result.AddPair(LPairName, LField.AsString);
      ftWideMemo:      Result.AddPair(LPairName, LField.AsString);
//        ftOraTimeStamp: ;
//        ftOraInterval: ;
      ftLongWord:      Result.AddPair(LPairName, TJSONNumber.Create(LField.AsInteger));
      ftShortint:      Result.AddPair(LPairName, TJSONNumber.Create(LField.AsInteger));
      ftByte:          Result.AddPair(LPairName, TJSONNumber.Create(LField.AsInteger));
      ftExtended:      Result.AddPair(LPairName, TJSONNumber.Create(LField.AsFloat));
//        ftConnection: ;
//        ftParams: ;
//        ftStream: ;
//        ftTimeStampOffset: ;
//        ftObject: ;
      ftSingle:        Result.AddPair(LPairName, TJSONNumber.Create(LField.AsFloat));
    end;
  end;
end;

class function TDataSetUtils.RecordToXML(const ADataSet: TDataSet; const ARootPath: string; AConfig: TNeonConfiguration): string;
var
  LField: TField;
begin
  Result := '';
  for LField in ADataSet.Fields do
  begin
    Result := Result
      + Format('<%s>%s</%s>', [LField.FieldName, LField.AsString, LField.FieldName]);
  end;
end;

class function TDataSetUtils.DataSetToJSONArray(const ADataSet: TDataSet; AConfig: TNeonConfiguration): TJSONArray;
begin
  Result := DataSetToJSONArray(ADataSet, nil, AConfig);
end;

class function TDataSetUtils.DataSetToCSV(const ADataSet: TDataSet; AConfig: TNeonConfiguration): string;
var
  LBookmark: TBookmark;
begin
  Result := '';
  if not Assigned(ADataSet) then
    Exit;

  if not ADataSet.Active then
    ADataSet.Open;

  ADataSet.DisableControls;
  try
    LBookmark := ADataSet.Bookmark;
    try
      ADataSet.First;
      while not ADataSet.Eof do
      try
        Result := Result + TDataSetUtils.RecordToCSV(ADataSet, AConfig) + sLineBreak;
      finally
        ADataSet.Next;
      end;
    finally
      ADataSet.GotoBookmark(LBookmark);
    end;
  finally
    ADataSet.EnableControls;
  end;
end;

class function TDataSetUtils.DataSetToJSONArray(const ADataSet: TDataSet; const AAcceptFunc: TFunc<Boolean>; AConfig: TNeonConfiguration): TJSONArray;
var
  LBookmark: TBookmark;
begin
  Result := TJSONArray.Create;
  if not Assigned(ADataSet) then
    Exit;

  if not ADataSet.Active then
    ADataSet.Open;

  ADataSet.DisableControls;
  try
    LBookmark := ADataSet.Bookmark;
    try
      ADataSet.First;
      while not ADataSet.Eof do
      try
        if (not Assigned(AAcceptFunc)) or (AAcceptFunc()) then
          Result.AddElement(RecordToJSONObject(ADataSet, AConfig));
      finally
        ADataSet.Next;
      end;
    finally
      ADataSet.GotoBookmark(LBookmark);
    end;
  finally
    ADataSet.EnableControls;
  end;
end;

class function TDataSetUtils.DataSetToXML(const ADataSet: TDataSet; AConfig: TNeonConfiguration): string;
begin
  Result := DataSetToXML(ADataSet, nil, AConfig);
end;

class function TDataSetUtils.DataSetToXML(const ADataSet: TDataSet; const AAcceptFunc: TFunc<Boolean>; AConfig: TNeonConfiguration): string;
var
  LBookmark: TBookmark;
begin
  Result := '';
  if not Assigned(ADataSet) then
    Exit;

  if not ADataSet.Active then
    ADataSet.Open;

  ADataSet.DisableControls;
  try
    LBookmark := ADataSet.Bookmark;
    try
      ADataSet.First;
      while not ADataSet.Eof do
      try
        if (not Assigned(AAcceptFunc)) or (AAcceptFunc()) then
          Result := Result + '<row>' + RecordToXML(ADataSet, '', AConfig) + '</row>';
      finally
        ADataSet.Next;
      end;
    finally
      ADataSet.GotoBookmark(LBookmark);
    end;
  finally
    ADataSet.EnableControls;
  end;
end;

class function TDataSetUtils.DatasetMetadataToJSONObject(const ADataSet: TDataSet; AConfig: TNeonConfiguration): TJSONObject;
  procedure AddPropertyValue(APropertyName: string);
  begin
    TValueToJSONObject(Result, APropertyName, ReadPropertyValue(ADataSet, APropertyName));
  end;
begin
  Result := TJSONObject.Create;
  AddPropertyValue('Eof');
  AddPropertyValue('Bof');
  AddPropertyValue('RecNo');
  AddPropertyValue('Name');
end;


end.
