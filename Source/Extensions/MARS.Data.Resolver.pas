unit MARS.Data.Resolver;

interface

uses
  System.SysUtils, System.Classes, System.DateUtils, Data.DB, Generics.Collections,
  System.JSON, MARS.Core.JSON;

type
  TMARSResolverFormats = record
  public
    const
      UNIX_DATE :string = 'unix';
    var
    // Same used in FormatSetting plus "unix" and "ISO"
    DateFormat: string;
    DateSeparator: Char;
    TimeSeparator: Char;
    // TimestampFormat?
    // a format for every not trivial Field.DataType?
    procedure Reset;
  end;

  TMARSResolver = class(TComponent)
  private
    FDataSet: TDataSet;
    FFormats: TMARSResolverFormats;
    procedure DoInsertObject(Values :TJSONObject);
    procedure DoInsertArray(Values :TJSONArray);
    procedure DoUpdateObject(Values :TJSONObject);
    procedure DoUpdateArray(Values :TJSONArray);
    procedure DoDeleteById(Id :Variant);
  public
    constructor Create(AOwner: TComponent); override;

    { Basic CRUD operations }
    procedure InsertDataSet(Values :TJSONValue); overload;
    procedure UpdateDataSet(Values :TJSONValue); overload;
    procedure DeleteDataSet(Id: Variant); overload;

    property Formats :TMARSResolverFormats read FFormats write FFormats;

    { Static methods }
    class procedure InsertDataSet(DataSet :TDataSet; Values :TJSONValue; Formats :TMARSResolverFormats); overload;
    class procedure InsertDataSet(DataSet :TDataSet; Values :TJSONValue); overload;
    class procedure UpdateDataSet(DataSet :TDataSet; Values :TJSONValue; Formats :TMARSResolverFormats); overload;
    class procedure UpdateDataSet(DataSet :TDataSet; Values :TJSONValue); overload;
    class procedure DeleteDataSet(DataSet :TDataSet; Id: Variant; Formats :TMARSResolverFormats); overload;
    class procedure DeleteDataSet(DataSet :TDataSet; Id: Variant); overload;
  published
    property DataSet :TDataSet read FDataSet write FDataSet;
  end;

var
  ResolverFormats :TMARSResolverFormats;

implementation

function MARSStrToDateTime(const Value: string; Formats :TMARSResolverFormats) :TDate;
var
  FS :TFormatSettings;
begin
  if Formats.DateFormat = TMARSResolverFormats.UNIX_DATE then
    Result := UnixToDateTime(StrToInt(Value), False)
  else
  begin
    FS.ShortDateFormat := Formats.DateFormat;
    FS.DateSeparator := Formats.DateSeparator;
    FS.TimeSeparator := Formats.TimeSeparator;
    Result := StrToDate(Value, FS);
  end;
end;

function MARSStrToDate(const Value: string; Formats :TMARSResolverFormats) :TDate;
begin
  Result := Trunc(MARSStrToDateTime(Value, Formats));
end;

function ValueToVariant(DataType :TFieldType; Value :TJSONValue; Formats :TMARSResolverFormats) :Variant;
begin
  if Value.Null then
    Exit();
  case DataType of
//    ftUnknown: ;
//    ftString: Values.AsVariant;
//    ftSmallint: ;
//    ftInteger: ;
//    ftWord: ;
//    ftBoolean: ;
//    ftFloat: ;
//    ftCurrency: ;
//    ftBCD: ;
    ftDate: Result := MARSStrToDate(Value.Value, Formats);
//    ftTime: ;
    ftDateTime: Result := MARSStrToDateTime(Value.Value, Formats);
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
    ftTimeStamp: Result := MARSStrToDateTime(Value.Value, Formats);
//    ftFMTBcd: ;
//    ftFixedWideChar: ;
//    ftWideMemo: ;
    ftOraTimeStamp: Result := MARSStrToDateTime(Value.Value, Formats);
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
        Result := Value.Value;
  end;
end;

class procedure TMARSResolver.UpdateDataSet(DataSet: TDataSet;
  Values: TJSONValue; Formats :TMARSResolverFormats);
var
  Resolver :TMARSResolver;
begin
  Resolver := TMARSResolver.Create(nil);
  try
    Resolver.DataSet := DataSet;
    Resolver.Formats := Formats;
    Resolver.UpdateDataSet(Values);
  finally
    Resolver.Free;
  end;
end;

class procedure TMARSResolver.DeleteDataSet(DataSet: TDataSet; Id: Variant;
  Formats: TMARSResolverFormats);
var
  Resolver :TMARSResolver;
begin
  Resolver := TMARSResolver.Create(nil);
  try
    Resolver.DataSet := DataSet;
    Resolver.Formats := Formats;
    Resolver.DeleteDataSet(Id);
  finally
    Resolver.Free;
  end;
end;

constructor TMARSResolver.Create(AOwner: TComponent);
begin
  inherited;
  FFormats := ResolverFormats;
end;

class procedure TMARSResolver.DeleteDataSet(DataSet: TDataSet; Id: Variant);
begin
  TMARSResolver.DeleteDataSet(DataSet, Id, ResolverFormats);
end;

procedure TMARSResolver.DeleteDataSet(Id: Variant);
begin
  DoDeleteById(Id);
end;

procedure TMARSResolver.DoDeleteById(Id: Variant);
var
  PSDataSet :IProviderSupportNG;
  DeleteStatement :string;
  Params :TParams;
  KeyFieldName :string;
  Field :TField;
  TableName :string;
begin
  if not Supports(DataSet, IProviderSupportNG, PSDataSet) then
    raise Exception.CreateFmt('Cannot apply updates. DataSet [%s] doesn''t implement IProviderSupport', [DataSet.Name]);

  TableName := PSDataSet.PSGetTableName;
  if TableName = '' then
    raise Exception.CreateFmt('Cannot get Table name from DataSet [%s]', [DataSet.Name]);

  KeyFieldName := PSDataSet.PSGetKeyFields;
  if KeyFieldName = '' then
    raise Exception.CreateFmt('Cannot get primary key from DataSet [%s]', [DataSet.Name]);

  DeleteStatement := 'DELETE FROM ' + TableName + ' WHERE ' + KeyFieldName + ' = ?';

  Params := TParams.Create(nil);
  try
    Field := DataSet.FindField(KeyFieldName);
    if not Assigned(Field) then
      raise Exception.CreateFmt('Cannot find field [%s] in DataSet [%s]', [KeyFieldName, DataSet.Name]);
    TParam(Params.Add).AssignFieldValue(Field, Id);

    PSDataSet.PSStartTransaction;
    try
      PSDataSet.PSExecuteStatement(DeleteStatement, Params);
      PSDataSet.PSEndTransaction(True);
    except
      PSDataSet.PSEndTransaction(False);
      raise;
    end;
  finally
    Params.Free;
  end;
end;

procedure TMARSResolver.DoInsertArray(Values: TJSONArray);
var
  Json :TJSONValue;
begin
  for Json in Values do
    DoInsertObject(Json as TJSONObject);
end;

procedure TMARSResolver.DoInsertObject(Values: TJSONObject);
const
  FieldSep = ', ' + sLineBreak;
var
  Pair :TJSONPair;
  Field :TField;
//  KeyFieldName :string;
//  KeyValue :string;
  TableName :string;
  FieldValue :Variant;
  PSDataSet :IProviderSupportNG;
  InsertStatement :string;
  Params :TParams;
  StmtFields, StmtValues :string;
begin
  if not Supports(DataSet, IProviderSupportNG, PSDataSet) then
    raise Exception.CreateFmt('Cannot apply updates. DataSet [%s] doesn''t implement IProviderSupport', [DataSet.Name]);

  TableName := PSDataSet.PSGetTableName;
  if TableName = '' then
    raise Exception.CreateFmt('Cannot get Table name from DataSet [%s]', [DataSet.Name]);

//  KeyFieldName := PSDataSet.PSGetKeyFields;
//  if KeyFieldName = '' then
//    raise Exception.CreateFmt('Cannot get primary key from DataSet [%s]', [DataSet.Name]);
//  KeyValue := Values.GetValue(KeyFieldName).Value;

  //InsertStatement := 'UPDATE ' + TableName + ' SET ' + sLineBreak;
  StmtFields := '';
  StmtValues := '';

  Params := TParams.Create(nil);
  try
    for Pair in Values do
    begin
      Field := DataSet.FindField(Pair.JsonString.Value);
      if not Assigned(Field) then
        raise Exception.CreateFmt('Cannot find field [%s] in DataSet [%s]', [Pair.JsonString.Value, DataSet.Name]);

      if pfInUpdate in Field.ProviderFlags then
      begin
        StmtFields := StmtFields + Field.FieldName + FieldSep;
        StmtValues := StmtValues + '?' + FieldSep;
        FieldValue := ValueToVariant(Field.DataType, Pair.JsonValue, Formats);
        //InsertStatement := InsertStatement + Field.FieldName + ' = ?' + FieldSep;
        TParam(Params.Add).AssignFieldValue(Field, FieldValue);
      end;
    end;
    // Remove the last ", \n"
    StmtFields := Copy(StmtFields, 1, Length(StmtFields) - Length(FieldSep));
    StmtValues := Copy(StmtValues, 1, Length(StmtValues) - Length(FieldSep));

    InsertStatement := 'INSERT INTO ' + TableName + '(' + StmtFields + ') VALUES (' + StmtValues + ')';

    PSDataSet.PSStartTransaction;
    try
      if PSDataSet.PSExecuteStatement(InsertStatement, Params) < 1 then
        raise Exception.CreateFmt('Cannot find selected record on DataSet [%s]', [DataSet.Name]);
      PSDataSet.PSEndTransaction(True);
    except
      PSDataSet.PSEndTransaction(False);
      raise;
    end;
  finally
    Params.Free;
  end;
end;

procedure TMARSResolver.DoUpdateArray(Values: TJSONArray);
var
  Json :TJSONValue;
begin
  for Json in Values do
    DoUpdateObject(Json as TJSONObject);
end;

procedure TMARSResolver.DoUpdateObject(Values: TJSONObject);
const
  FieldSep = ', ' + sLineBreak;
var
  Pair :TJSONPair;
  Field :TField;
  KeyFieldName :string;
  KeyValue :string;
  TableName :string;
  FieldValue :Variant;
  PSDataSet :IProviderSupportNG;
  UpdateStatement :string;
  Params :TParams;
begin
  if not Supports(DataSet, IProviderSupportNG, PSDataSet) then
    raise Exception.CreateFmt('Cannot apply updates. DataSet [%s] doesn''t implement IProviderSupport', [DataSet.Name]);

  TableName := PSDataSet.PSGetTableName;
  if TableName = '' then
    raise Exception.CreateFmt('Cannot get Table name from DataSet [%s]', [DataSet.Name]);

  KeyFieldName := PSDataSet.PSGetKeyFields;
  if KeyFieldName = '' then
    raise Exception.CreateFmt('Cannot get primary key from DataSet [%s]', [DataSet.Name]);
  KeyValue := Values.GetValue(KeyFieldName).Value;

  UpdateStatement := 'UPDATE ' + TableName + ' SET ' + sLineBreak;

  Params := TParams.Create(nil);
  try
    for Pair in Values do
    begin
      Field := DataSet.FindField(Pair.JsonString.Value);
      if not Assigned(Field) then
        raise Exception.CreateFmt('Cannot find field [%s] in DataSet [%s]', [Pair.JsonString.Value, DataSet.Name]);

      if (pfInUpdate in Field.ProviderFlags) and (not (pfInKey in Field.ProviderFlags)) then
      begin
        FieldValue := ValueToVariant(Field.DataType, Pair.JsonValue, Formats);
        UpdateStatement := UpdateStatement + Field.FieldName + ' = ?' + FieldSep;
        TParam(Params.Add).AssignFieldValue(Field, FieldValue);
      end;
    end;
    // Remove the last ", \n"
    UpdateStatement := Copy(UpdateStatement, 1, Length(UpdateStatement) - Length(FieldSep)) + sLineBreak;
    UpdateStatement := UpdateStatement + ' WHERE ' + KeyFieldName + ' = ' + QuotedStr(KeyValue);

    PSDataSet.PSStartTransaction;
    try
      if PSDataSet.PSExecuteStatement(UpdateStatement, Params) < 1 then
        raise Exception.CreateFmt('Cannot find selected record on DataSet [%s]', [DataSet.Name]);
      PSDataSet.PSEndTransaction(True);
    except
      PSDataSet.PSEndTransaction(False);
      raise;
    end;
  finally
    Params.Free;
  end;
end;

class procedure TMARSResolver.InsertDataSet(DataSet: TDataSet; Values :TJSONValue;
  Formats: TMARSResolverFormats);
var
  Resolver :TMARSResolver;
begin
  Resolver := TMARSResolver.Create(nil);
  try
    Resolver.DataSet := DataSet;
    Resolver.Formats := Formats;
    Resolver.InsertDataSet(Values);
  finally
    Resolver.Free;
  end;
end;

class procedure TMARSResolver.InsertDataSet(DataSet: TDataSet; Values :TJSONValue);
begin
  InsertDataSet(DataSet, Values, ResolverFormats);
end;

procedure TMARSResolver.InsertDataSet(Values: TJSONValue);
begin
  if Values is TJSONArray then
    DoInsertArray(Values as TJSONArray)
  else if Values is TJSONObject then
    DoInsertObject(Values as TJSONObject)
  else
    raise Exception.CreateFmt('Cannot insert [%s]', [Values.ClassName]);
end;

class procedure TMARSResolver.UpdateDataSet(DataSet: TDataSet;
  Values: TJSONValue);
begin
  UpdateDataSet(DataSet, Values, ResolverFormats);
end;

procedure TMARSResolver.UpdateDataSet(Values: TJSONValue);
begin
  if Values is TJSONArray then
    DoUpdateArray(Values as TJSONArray)
  else if Values is TJSONObject then
    DoUpdateObject(Values as TJSONObject)
  else
    raise Exception.CreateFmt('Cannot update [%s]', [Values.ClassName]);
end;

{ TMARSResolverFormats }

procedure TMARSResolverFormats.Reset;
begin
  DateFormat := UNIX_DATE;
  DateSeparator := '/';
  TimeSeparator := ':';
end;

initialization
  ResolverFormats.Reset;

end.
