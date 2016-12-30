{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Data.Resolver;

interface

uses
  System.SysUtils, System.Classes, System.DateUtils,
  Data.DB, Generics.Collections, System.JSON,

  WiRL.Core.JSON;

type
  TWiRLResolverFormats = record
  public
    const
      UNIX_DATE: string = 'unix';
    var
      // Same used in FormatSetting plus "unix" and "ISO"
      DateFormat: string;
      DateSeparator: Char;
      TimeSeparator: Char;
      // TimestampFormat?
      // a format for every not trivial Field.DataType?
    procedure Reset;
  end;

  TWiRLResolver = class(TComponent)
  private
    FDataSet: TDataSet;
    FFormats: TWiRLResolverFormats;
    procedure DoInsertObject(AValues: TJSONObject);
    procedure DoInsertArray(AValues: TJSONArray);
    procedure DoUpdateObject(AValues: TJSONObject);
    procedure DoUpdateArray(AValues: TJSONArray);
    procedure DoDeleteById(AID: Variant);
  public
    constructor Create(AOwner: TComponent); override;

    { Basic CRUD operations }
    procedure InsertDataSet(AValues: TJSONValue); overload;
    procedure UpdateDataSet(AValues: TJSONValue); overload;
    procedure DeleteDataSet(AID :Variant); overload;

    property Formats: TWiRLResolverFormats read FFormats write FFormats;

    { Static methods }
    class procedure InsertDataSet(ADataSet: TDataSet; AValues: TJSONValue; AFormats: TWiRLResolverFormats); overload;
    class procedure InsertDataSet(ADataSet: TDataSet; AValues: TJSONValue); overload;
    class procedure UpdateDataSet(ADataSet: TDataSet; AValues: TJSONValue; AFormats: TWiRLResolverFormats); overload;
    class procedure UpdateDataSet(ADataSet: TDataSet; AValues: TJSONValue); overload;
    class procedure DeleteDataSet(ADataSet: TDataSet; AID: Variant; AFormats: TWiRLResolverFormats); overload;
    class procedure DeleteDataSet(ADataSet: TDataSet; AID: Variant); overload;
  published
    property DataSet: TDataSet read FDataSet write FDataSet;
  end;

var
  ResolverFormats: TWiRLResolverFormats;

implementation

function WiRLStrToDateTime(const AValue: string; AFormats: TWiRLResolverFormats) :TDate;
var
  LFormat: TFormatSettings;
begin
  if AFormats.DateFormat = TWiRLResolverFormats.UNIX_DATE then
    Result := UnixToDateTime(StrToInt(AValue), False)
  else
  begin
    LFormat.ShortDateFormat := AFormats.DateFormat;
    LFormat.DateSeparator := AFormats.DateSeparator;
    LFormat.TimeSeparator := AFormats.TimeSeparator;
    Result := StrToDate(AValue, LFormat);
  end;
end;

function WiRLStrToDate(const AValue: string; AFormats: TWiRLResolverFormats): TDate;
begin
  Result := Trunc(WiRLStrToDateTime(AValue, AFormats));
end;

function ValueToVariant(ADataType: TFieldType; AValue: TJSONValue; AFormats: TWiRLResolverFormats): Variant;
begin
  if AValue.Null then
    Exit();

  case ADataType of
//    ftUnknown: ;
//    ftString: Values.AsVariant;
//    ftSmallint: ;
//    ftInteger: ;
//    ftWord: ;
//    ftBoolean: ;
//    ftFloat: ;
//    ftCurrency: ;
//    ftBCD: ;
    ftDate: Result := WiRLStrToDate(AValue.Value, AFormats);
//    ftTime: ;
    ftDateTime: Result := WiRLStrToDateTime(AValue.Value, AFormats);
//    ftBytes: ;
//    ftVarBytes: ;
//    ftAutoInc: ;
//    ftBlob: ;
//    ftMemo: ;
//    ftGraphic: ;
//    ftFmtMemo: ;
//    ftParadoxOle: ;
//    ftDBaseOle: ;
//    ftTypedBinary: ;
//    ftCursor: ;
//    ftFixedChar: ;
//    ftWideString: ;
//    ftLargeint: ;
//    ftADT: ;
//    ftArray: ;
//    ftReference: ;
//    ftDataSet: ;
//    ftOraBlob: ;
//    ftOraClob: ;
//    ftVariant: ;
//    ftInterface: ;
//    ftIDispatch: ;
//    ftGuid: ;
    ftTimeStamp: Result := WiRLStrToDateTime(AValue.Value, AFormats);
//    ftFMTBcd: ;
//    ftFixedWideChar: ;
//    ftWideMemo: ;
    ftOraTimeStamp: Result := WiRLStrToDateTime(AValue.Value, AFormats);
//    ftOraInterval: ;
//    ftLongWord: ;
//    ftShortint: ;
//    ftByte: ;
//    ftExtended: ;
//    ftConnection: ;
//    ftParams: ;
//    ftStream: ;
//    ftTimeStampOffset: ;
//    ftObject: ;
//    ftSingle: ;
      else
        Result := AValue.Value;
  end;
end;

class procedure TWiRLResolver.UpdateDataSet(ADataSet: TDataSet;
  AValues: TJSONValue; AFormats: TWiRLResolverFormats);
var
  LResolver: TWiRLResolver;
begin
  LResolver := TWiRLResolver.Create(nil);
  try
    LResolver.DataSet := ADataSet;
    LResolver.Formats := AFormats;
    LResolver.UpdateDataSet(AValues);
  finally
    LResolver.Free;
  end;
end;

class procedure TWiRLResolver.DeleteDataSet(ADataSet: TDataSet; AID: Variant;
    AFormats: TWiRLResolverFormats);
var
  LResolver: TWiRLResolver;
begin
  LResolver := TWiRLResolver.Create(nil);
  try
    LResolver.DataSet := ADataSet;
    LResolver.Formats := AFormats;
    LResolver.DeleteDataSet(AID);
  finally
    LResolver.Free;
  end;
end;

constructor TWiRLResolver.Create(AOwner: TComponent);
begin
  inherited;
  FFormats := ResolverFormats;
end;

class procedure TWiRLResolver.DeleteDataSet(ADataSet: TDataSet; AID: Variant);
begin
  TWiRLResolver.DeleteDataSet(ADataSet, AID, ResolverFormats);
end;

procedure TWiRLResolver.DeleteDataSet(AID :Variant);
begin
  DoDeleteById(AID);
end;

procedure TWiRLResolver.DoDeleteById(AID: Variant);
var
  LPSDataSet: IProviderSupportNG;
  LDeleteStatement: string;
  LParams: TParams;
  LKeyFieldName: string;
  LField: TField;
  LTableName: string;
begin
  if not Supports(DataSet, IProviderSupportNG, LPSDataSet) then
    raise Exception.CreateFmt('Cannot apply updates. DataSet [%s] doesn''t implement IProviderSupport', [DataSet.Name]);

  LTableName := LPSDataSet.PSGetTableName;
  if LTableName = '' then
    raise Exception.CreateFmt('Cannot get Table name from DataSet [%s]', [DataSet.Name]);

  LKeyFieldName := LPSDataSet.PSGetKeyFields;
  if LKeyFieldName = '' then
    raise Exception.CreateFmt('Cannot get primary key from DataSet [%s]', [DataSet.Name]);

  LDeleteStatement := 'DELETE FROM ' + LTableName + ' WHERE ' + LKeyFieldName + ' = ?';

  LParams := TParams.Create(nil);
  try
    LField := DataSet.FindField(LKeyFieldName);
    if not Assigned(LField) then
      raise Exception.CreateFmt('Cannot find field [%s] in DataSet [%s]', [LKeyFieldName, DataSet.Name]);
    TParam(LParams.Add).AssignFieldValue(LField, AID);

    LPSDataSet.PSStartTransaction;
    try
      LPSDataSet.PSExecuteStatement(LDeleteStatement, LParams);
      LPSDataSet.PSEndTransaction(True);
    except
      LPSDataSet.PSEndTransaction(False);
      raise;
    end;
  finally
    LParams.Free;
  end;
end;

procedure TWiRLResolver.DoInsertArray(AValues: TJSONArray);
var
  LJson: TJSONValue;
begin
  for LJson in AValues do
    DoInsertObject(LJson as TJSONObject);
end;

procedure TWiRLResolver.DoInsertObject(AValues: TJSONObject);
const
  FIELD_SEPARATOR = ', ' + sLineBreak;
var
  LPair: TJSONPair;
  LField: TField;
//  KeyFieldName :string;
//  KeyValue :string;
  LTableName: string;
  LFieldValue: Variant;
  LPSDataSet: IProviderSupportNG;
  LInsertStatement: string;
  LParams: TParams;
  LStmtFields, LStmtValues: string;
begin
  if not Supports(DataSet, IProviderSupportNG, LPSDataSet) then
    raise Exception.CreateFmt('Cannot apply updates. DataSet [%s] doesn''t implement IProviderSupport', [DataSet.Name]);

  LTableName := LPSDataSet.PSGetTableName;
  if LTableName = '' then
    raise Exception.CreateFmt('Cannot get Table name from DataSet [%s]', [DataSet.Name]);

//  KeyFieldName := PSDataSet.PSGetKeyFields;
//  if KeyFieldName = '' then
//    raise Exception.CreateFmt('Cannot get primary key from DataSet [%s]', [DataSet.Name]);
//  KeyValue := Values.GetValue(KeyFieldName).Value;

  //InsertStatement := 'UPDATE ' + TableName + ' SET ' + sLineBreak;
  LStmtFields := '';
  LStmtValues := '';

  LParams := TParams.Create(nil);
  try
    for LPair in AValues do
    begin
      LField := DataSet.FindField(LPair.JsonString.Value);
      if not Assigned(LField) then
        raise Exception.CreateFmt('Cannot find field [%s] in DataSet [%s]', [LPair.JsonString.Value, DataSet.Name]);

      if pfInUpdate in LField.ProviderFlags then
      begin
        LStmtFields := LStmtFields + LField.FieldName + FIELD_SEPARATOR;
        LStmtValues := LStmtValues + '?' + FIELD_SEPARATOR;
        LFieldValue := ValueToVariant(LField.DataType, LPair.JsonValue, Formats);
        //InsertStatement := InsertStatement + Field.FieldName + ' = ?' + FieldSep;
        TParam(LParams.Add).AssignFieldValue(LField, LFieldValue);
      end;
    end;
    // Remove the last ", \n"
    LStmtFields := Copy(LStmtFields, 1, Length(LStmtFields) - Length(FIELD_SEPARATOR));
    LStmtValues := Copy(LStmtValues, 1, Length(LStmtValues) - Length(FIELD_SEPARATOR));

    LInsertStatement := 'INSERT INTO ' + LTableName + '(' + LStmtFields + ') VALUES (' + LStmtValues + ')';

    LPSDataSet.PSStartTransaction;
    try
      if LPSDataSet.PSExecuteStatement(LInsertStatement, LParams) < 1 then
        raise Exception.CreateFmt('Cannot find selected record on DataSet [%s]', [DataSet.Name]);
      LPSDataSet.PSEndTransaction(True);
    except
      LPSDataSet.PSEndTransaction(False);
      raise;
    end;
  finally
    LParams.Free;
  end;
end;

procedure TWiRLResolver.DoUpdateArray(AValues: TJSONArray);
var
  LJson: TJSONValue;
begin
  for LJson in AValues do
    DoUpdateObject(LJson as TJSONObject);
end;

procedure TWiRLResolver.DoUpdateObject(AValues: TJSONObject);
const
  FIELD_SEPARATOR = ', ' + sLineBreak;
var
  LPair: TJSONPair;
  LField: TField;
  LKeyFieldName: string;
  LKeyValue: string;
  LTableName: string;
  LFieldValue: Variant;
  LPSDataSet: IProviderSupportNG;
  LUpdateStatement: string;
  LParams: TParams;
begin
  if not Supports(DataSet, IProviderSupportNG, LPSDataSet) then
    raise Exception.CreateFmt('Cannot apply updates. DataSet [%s] doesn''t implement IProviderSupport', [DataSet.Name]);

  LTableName := LPSDataSet.PSGetTableName;
  if LTableName = '' then
    raise Exception.CreateFmt('Cannot get Table name from DataSet [%s]', [DataSet.Name]);

  LKeyFieldName := LPSDataSet.PSGetKeyFields;
  if LKeyFieldName = '' then
    raise Exception.CreateFmt('Cannot get primary key from DataSet [%s]', [DataSet.Name]);
  LKeyValue := AValues.GetValue(LKeyFieldName).Value;

  LUpdateStatement := 'UPDATE ' + LTableName + ' SET ' + sLineBreak;

  LParams := TParams.Create(nil);
  try
    for LPair in AValues do
    begin
      LField := DataSet.FindField(LPair.JsonString.Value);
      if not Assigned(LField) then
        raise Exception.CreateFmt('Cannot find field [%s] in DataSet [%s]', [LPair.JsonString.Value, DataSet.Name]);

      if (pfInUpdate in LField.ProviderFlags) and (not (pfInKey in LField.ProviderFlags)) then
      begin
        LFieldValue := ValueToVariant(LField.DataType, LPair.JsonValue, Formats);
        LUpdateStatement := LUpdateStatement + LField.FieldName + ' = ?' + FIELD_SEPARATOR;
        TParam(LParams.Add).AssignFieldValue(LField, LFieldValue);
      end;
    end;
    // Remove the last ", \n"
    LUpdateStatement := Copy(LUpdateStatement, 1, Length(LUpdateStatement) - Length(FIELD_SEPARATOR)) + sLineBreak;
    LUpdateStatement := LUpdateStatement + ' WHERE ' + LKeyFieldName + ' = ' + QuotedStr(LKeyValue);

    LPSDataSet.PSStartTransaction;
    try
      if LPSDataSet.PSExecuteStatement(LUpdateStatement, LParams) < 1 then
        raise Exception.CreateFmt('Cannot find selected record on DataSet [%s]', [DataSet.Name]);
      LPSDataSet.PSEndTransaction(True);
    except
      LPSDataSet.PSEndTransaction(False);
      raise;
    end;
  finally
    LParams.Free;
  end;
end;

class procedure TWiRLResolver.InsertDataSet(ADataSet: TDataSet; AValues:
    TJSONValue; AFormats: TWiRLResolverFormats);
var
  LResolver: TWiRLResolver;
begin
  LResolver := TWiRLResolver.Create(nil);
  try
    LResolver.DataSet := ADataSet;
    LResolver.Formats := AFormats;
    LResolver.InsertDataSet(AValues);
  finally
    LResolver.Free;
  end;
end;

class procedure TWiRLResolver.InsertDataSet(ADataSet: TDataSet; AValues: TJSONValue);
begin
  InsertDataSet(ADataSet, AValues, ResolverFormats);
end;

procedure TWiRLResolver.InsertDataSet(AValues: TJSONValue);
begin
  if AValues is TJSONArray then
    DoInsertArray(AValues as TJSONArray)
  else if AValues is TJSONObject then
    DoInsertObject(AValues as TJSONObject)
  else
    raise Exception.CreateFmt('Cannot insert [%s]', [AValues.ClassName]);
end;

class procedure TWiRLResolver.UpdateDataSet(ADataSet: TDataSet; AValues: TJSONValue);
begin
  UpdateDataSet(ADataSet, AValues, ResolverFormats);
end;

procedure TWiRLResolver.UpdateDataSet(AValues: TJSONValue);
begin
  if AValues is TJSONArray then
    DoUpdateArray(AValues as TJSONArray)
  else if AValues is TJSONObject then
    DoUpdateObject(AValues as TJSONObject)
  else
    raise Exception.CreateFmt('Cannot update [%s]', [AValues.ClassName]);
end;

{ TWiRLResolverFormats }

procedure TWiRLResolverFormats.Reset;
begin
  DateFormat := UNIX_DATE;
  DateSeparator := '/';
  TimeSeparator := ':';
end;

initialization
  ResolverFormats.Reset;

end.
