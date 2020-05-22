{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Types;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Rtti,

  WiRL.Core.Singleton, WiRL.Rtti.Utils;

type
  EWiRLConvertError = class(Exception);

  TWiRLFormatSetting = record
    // ISO, UNIX, DMY, MDY
    DateFormat: string;
    // ISO, UNIX, DMY, MDY
    DateTimeFormat: string;

    // Decimal separator: ,.
    FloatFormat: string;

    // Empty
    IntFormat: string;

    class function WithDateFormat(const ADateFormat: string): TWiRLFormatSetting; static;
    class function WithDateTimeFormat(const ADateTimeFormat: string): TWiRLFormatSetting; static;
    class function WithFloatFormat(const AFloatFormat: string): TWiRLFormatSetting; static;
  end;

  TWiRLConverter = class
  protected
    FFormat: TWiRLFormatSetting;
  public
    function ValueFromString(const AValue: string): TValue; virtual; abstract;
    function ValueToString(const AValue: TValue): string; virtual; abstract;
    constructor Create(const AFormat: TWiRLFormatSetting);
  end;

  TWiRLConverterClass = class of TWiRLConverter;

  TWiRLCheckConverter = reference to function (ARttiType: TRttiType; const AFormat: TWiRLFormatSetting): Boolean;

  TWiRLConverterRegistry = class(TObject)
  private type
    TWiRLConverterRegistrySingleton = TWiRLSingleton<TWiRLConverterRegistry>;
    TWiRLConverterInfo = class
      ConverterClass: TWiRLConverterClass;
      CheckConverter: TWiRLCheckConverter;
    end;
  private
    FRegistry: TObjectList<TWiRLConverterInfo>;

  protected
    class function GetInstance: TWiRLConverterRegistry; static; inline;
  public
    constructor Create; virtual;
    function RegisterConverter(AConverterClass: TWiRLConverterClass; const ACheckConverter: TWiRLCheckConverter): TWiRLConverterInfo;
    function GetConverter(const ARttiType: TRttiType; const AFormat: TWiRLFormatSetting): TWiRLConverter;

    class property Instance: TWiRLConverterRegistry read GetInstance;
    destructor Destroy; override;
  end;

  TWiRLConvert = class
  private
    class function Convert<T>(const AValue: string; AFormat: TWiRLFormatSetting): T;
    class function ToString<T>(const AValue: T; AFormat: TWiRLFormatSetting): string; reintroduce;
  public
    // String to type
    class function ToDate(const AValue: string; AFormat: TWiRLFormatSetting): TDate; overload;
    class function ToDate(const AValue: string): TDate; overload;

    class function ToDateTime(const AValue: string; AFormat: TWiRLFormatSetting): TDateTime; overload;
    class function ToDateTime(const AValue: string): TDateTime; overload;

    class function ToFloat(const AValue: string; AFormat: TWiRLFormatSetting): Double; overload;
    class function ToFloat(const AValue: string): Double; overload;

    class function ToInteger(const AValue: string; AFormat: TWiRLFormatSetting): Integer; overload;
    class function ToInteger(const AValue: string): Integer; overload;

    // Type to string
    class function fromDateTime(const AValue: TDateTime; AFormat: TWiRLFormatSetting): string; overload;
    class function fromDateTime(const AValue: TDateTime): string; overload;

    class function fromDate(const AValue: TDate; AFormat: TWiRLFormatSetting): string; overload;
    class function fromDate(const AValue: TDate): string; overload;

    class function FromFloat(const AValue: Double; AFormat: TWiRLFormatSetting): string; overload;
    class function FromFloat(const AValue: Double): string; overload;

    class function fromInteger(const AValue: Integer; AFormat: TWiRLFormatSetting): string; overload;
    class function fromInteger(const AValue: Integer): string; overload;

  end;

  TISODateConverter = class(TWiRLConverter)
  public
    function ValueFromString(const AValue: string): TValue; override;
    function ValueToString(const AValue: TValue): string; override;
  end;

  TISODateTimeConverter = class(TWiRLConverter)
  public
    function ValueFromString(const AValue: string): TValue; override;
    function ValueToString(const AValue: TValue): string; override;
  end;

  TUnixDateTimeConverter = class(TWiRLConverter)
  public
    function ValueFromString(const AValue: string): TValue; override;
    function ValueToString(const AValue: TValue): string; override;
  end;

  TUnixDateConverter = class(TWiRLConverter)
  public
    function ValueFromString(const AValue: string): TValue; override;
    function ValueToString(const AValue: TValue): string; override;
  end;

  TYMDDateConverter = class(TWiRLConverter)
  public
    function ValueFromString(const AValue: string): TValue; override;
    function ValueToString(const AValue: TValue): string; override;
  end;

  TDefaultFloatConverter = class(TWiRLConverter)
  public
    function ValueFromString(const AValue: string): TValue; override;
    function ValueToString(const AValue: TValue): string; override;
  end;

  TDefaultIntegerConverter = class(TWiRLConverter)
  public
    function ValueFromString(const AValue: string): TValue; override;
    function ValueToString(const AValue: TValue): string; override;
  end;

var
  DefaultFormatSetting: TWiRLFormatSetting;

implementation

uses
  System.DateUtils;

{ TWiRLConvert }

const
  UseUTCDate = False;
  SecsInADay = 24 * 60 * 60;

class function TWiRLConvert.ToDateTime(const AValue: string;
  AFormat: TWiRLFormatSetting): TDateTime;
begin
  Result := Convert<TDateTime>(AValue, AFormat);
end;

class function TWiRLConvert.Convert<T>(const AValue: string; AFormat: TWiRLFormatSetting): T;
var
  LConverter: TWiRLConverter;
  LRttiType: TRttiType;
begin
  LRttiType := TRttiHelper.Context.GetType(TypeInfo(T));
  LConverter := TWiRLConverterRegistry.Instance.GetConverter(LRttiType, AFormat);
  try
    Result := LConverter.ValueFromString(AValue).AsType<T>();
  finally
    LConverter.Free;
  end;
end;

class function TWiRLConvert.fromDate(const AValue: TDate;
  AFormat: TWiRLFormatSetting): string;
begin
  Result := ToString<TDate>(AValue, AFormat);
end;

class function TWiRLConvert.fromDate(const AValue: TDate): string;
begin
  Result := fromDate(AValue, DefaultFormatSetting);
end;

class function TWiRLConvert.fromDateTime(const AValue: TDateTime;
  AFormat: TWiRLFormatSetting): string;
begin
  Result := ToString<TDateTime>(AValue, AFormat);
end;

class function TWiRLConvert.fromDateTime(const AValue: TDateTime): string;
begin
  Result := fromDateTime(AValue, DefaultFormatSetting);
end;

class function TWiRLConvert.FromFloat(const AValue: Double): string;
begin
  Result := FromFloat(AValue, DefaultFormatSetting);
end;

class function TWiRLConvert.fromInteger(const AValue: Integer): string;
begin
  Result := fromInteger(AValue, DefaultFormatSetting);
end;

class function TWiRLConvert.fromInteger(const AValue: Integer;
  AFormat: TWiRLFormatSetting): string;
begin
  Result := ToString<Integer>(AValue, AFormat);
end;

class function TWiRLConvert.FromFloat(const AValue: Double;
  AFormat: TWiRLFormatSetting): string;
begin
  Result := ToString<Double>(AValue, AFormat);
end;

class function TWiRLConvert.ToDate(const AValue: string): TDate;
begin
  Result := ToDate(AValue, DefaultFormatSetting);
end;

class function TWiRLConvert.ToDate(const AValue: string;
  AFormat: TWiRLFormatSetting): TDate;
begin
  Result := Convert<TDate>(AValue, AFormat);
end;

class function TWiRLConvert.ToDateTime(const AValue: string): TDateTime;
begin
  Result := ToDateTime(AValue, DefaultFormatSetting);
end;

class function TWiRLConvert.ToFloat(const AValue: string): Double;
begin
  Result := ToFloat(AValue, DefaultFormatSetting);
end;

class function TWiRLConvert.ToInteger(const AValue: string): Integer;
begin
  Result := ToInteger(AValue, DefaultFormatSetting);
end;

class function TWiRLConvert.ToInteger(const AValue: string;
  AFormat: TWiRLFormatSetting): Integer;
begin
  Result := Convert<Integer>(AValue, AFormat);
end;

class function TWiRLConvert.ToFloat(const AValue: string;
  AFormat: TWiRLFormatSetting): Double;
begin
  Result := Convert<Double>(AValue, AFormat);
end;

class function TWiRLConvert.ToString<T>(const AValue: T;
  AFormat: TWiRLFormatSetting): string;
var
  LConverter: TWiRLConverter;
  LRttiType: TRttiType;
begin
  LRttiType := TRttiHelper.Context.GetType(TypeInfo(T));
  LConverter := TWiRLConverterRegistry.Instance.GetConverter(LRttiType, AFormat);
  try
    Result := LConverter.ValueToString(TValue.From(AValue));
  finally
    LConverter.Free;
  end;
end;

{ TWiRLConverterRegistry }

constructor TWiRLConverterRegistry.Create;
begin
  FRegistry := TObjectList<TWiRLConverterInfo>.Create(True);
end;

destructor TWiRLConverterRegistry.Destroy;
begin
  FRegistry.Free;
  inherited;
end;

function TWiRLConverterRegistry.GetConverter(const ARttiType: TRttiType;
  const AFormat: TWiRLFormatSetting): TWiRLConverter;
var
  LConverterInfo: TWiRLConverterInfo;
begin
  for LConverterInfo in FRegistry do
  begin
    if LConverterInfo.CheckConverter(ARttiType, AFormat) then
    begin
      Exit(LConverterInfo.ConverterClass.Create(AFormat));
    end;
  end;

  raise EWiRLConvertError.CreateFmt('Converter non found for [%s]', [ARttiType.QualifiedName]);
end;

class function TWiRLConverterRegistry.GetInstance: TWiRLConverterRegistry;
begin
  Result := TWiRLConverterRegistrySingleton.Instance;
end;

function TWiRLConverterRegistry.RegisterConverter(
  AConverterClass: TWiRLConverterClass;
  const ACheckConverter: TWiRLCheckConverter): TWiRLConverterInfo;
begin
  Result := TWiRLConverterInfo.Create;
  try
    Result.CheckConverter := ACheckConverter;
    Result.ConverterClass := AConverterClass;
    FRegistry.Add(Result);
  except
    Result.Free;
    raise;
  end;
end;

{ TISODateConverter }

function TISODateConverter.ValueFromString(const AValue: string): TValue;
begin
  Result := Trunc(ISO8601ToDate(AValue, True));
end;

function TISODateConverter.ValueToString(const AValue: TValue): string;
begin
  Result := Copy(DateToISO8601(AValue.AsExtended, True), 1, 10);
end;

{ TWiRLConverter }

constructor TWiRLConverter.Create(const AFormat: TWiRLFormatSetting);
begin
  inherited Create;
  FFormat := AFormat;
end;

{ TISODateTimeConverter }

function TISODateTimeConverter.ValueFromString(const AValue: string): TValue;
begin
  Result := ISO8601ToDate(AValue, True);
end;

function TISODateTimeConverter.ValueToString(const AValue: TValue): string;
begin
  Result := DateToISO8601(AValue.AsExtended, True);
end;

procedure RegisterDefaultConverters;
begin
  TWiRLConverterRegistry.Instance.RegisterConverter(TISODateConverter,
    function (ARttiType: TRttiType; const AFormat: TWiRLFormatSetting): Boolean
    begin
      Result := False;
      if (AFormat.DateFormat = 'ISO') and (ARttiType.TypeKind = tkFloat) and
         (ARttiType.Handle = TypeInfo(TDate)) then
        Exit(True);
    end
  );

  TWiRLConverterRegistry.Instance.RegisterConverter(TISODateTimeConverter,
    function (ARttiType: TRttiType; const AFormat: TWiRLFormatSetting): Boolean
    begin
      Result := False;
      if (AFormat.DateFormat = 'ISO') and (ARttiType.TypeKind = tkFloat) and
         (ARttiType.Handle = TypeInfo(TDateTime)) then
        Exit(True);
    end
  );

  TWiRLConverterRegistry.Instance.RegisterConverter(TUnixDateTimeConverter,
    function (ARttiType: TRttiType; const AFormat: TWiRLFormatSetting): Boolean
    begin
      Result := False;
      if (AFormat.DateTimeFormat = 'UNIX') and (ARttiType.TypeKind = tkFloat) and
         (ARttiType.Handle = TypeInfo(TDateTime)) then
        Exit(True);
    end
  );

  TWiRLConverterRegistry.Instance.RegisterConverter(TUnixDateConverter,
    function (ARttiType: TRttiType; const AFormat: TWiRLFormatSetting): Boolean
    begin
      Result := False;
      if (AFormat.DateFormat = 'UNIX') and (ARttiType.TypeKind = tkFloat) and
         (ARttiType.Handle = TypeInfo(TDate)) then
        Exit(True);
    end
  );

  TWiRLConverterRegistry.Instance.RegisterConverter(TYMDDateConverter,
    function (ARttiType: TRttiType; const AFormat: TWiRLFormatSetting): Boolean
    begin
      Result := False;
      if ( (AFormat.DateFormat = 'MDY') or (AFormat.DateFormat = 'DMY') ) and
         (ARttiType.TypeKind = tkFloat) and (ARttiType.Handle = TypeInfo(TDate)) then
        Exit(True);
    end
  );

  TWiRLConverterRegistry.Instance.RegisterConverter(TDefaultFloatConverter,
    function (ARttiType: TRttiType; const AFormat: TWiRLFormatSetting): Boolean
    begin
      Result := False;
      if (ARttiType.TypeKind = tkFloat) and (ARttiType.Handle <> TypeInfo(TDate)) and (Length(AFormat.FloatFormat) = 1) then
      begin
        if (CharInSet(AFormat.FloatFormat[1], [',', '.', #0])) then
          Exit(True);
      end;
    end
  );

  TWiRLConverterRegistry.Instance.RegisterConverter(TDefaultIntegerConverter,
    function (ARttiType: TRttiType; const AFormat: TWiRLFormatSetting): Boolean
    begin
      Result := False;
      if (ARttiType.TypeKind = tkInteger) and (AFormat.IntFormat = '') then
        Exit(True);
    end
  );
end;

{ TUnixDateTimeConverter }

function TUnixDateTimeConverter.ValueFromString(const AValue: string): TValue;
begin
  Result := (StrToInt(AValue) / SecsInADay + UnixDateDelta);
end;

function TUnixDateTimeConverter.ValueToString(const AValue: TValue): string;
begin
  Result := IntToStr(
      Round(
        ((AValue.AsExtended - UnixDateDelta) *  SecsInADay)
      )
  );
end;

{ TUnixDateConverter }

function TUnixDateConverter.ValueFromString(const AValue: string): TValue;
begin
  Result := Trunc((StrToInt(AValue) / SecsInADay + UnixDateDelta));
end;

function TUnixDateConverter.ValueToString(const AValue: TValue): string;
begin
  Result := IntToStr(
      Round(
        ((AValue.AsExtended - UnixDateDelta) *  SecsInADay)
      )
  );
end;

{ TYMDDateConverter }

function TYMDDateConverter.ValueFromString(const AValue: string): TValue;
var
  FS: TFormatSettings;
begin
  FS.DateSeparator := '/';
  if FFormat.DateFormat = 'MDY' then
    FS.ShortDateFormat := 'mm/dd/yyyy'
  else if FFormat.DateFormat = 'MDY' then
    FS.ShortDateFormat := 'dd/mm/yyyy'
  else
    raise EWiRLConvertError.CreateFmt('Date format not supported [%s]', [FFormat.DateFormat]);
  Result := StrToDate(AValue, FS);
end;

function TYMDDateConverter.ValueToString(const AValue: TValue): string;
var
  FS: TFormatSettings;
begin
  FS.DateSeparator := '/';
  if FFormat.DateFormat = 'MDY' then
    FS.ShortDateFormat := 'mm/dd/yyyy'
  else if FFormat.DateFormat = 'MDY' then
    FS.ShortDateFormat := 'dd/mm/yyyy'
  else
    raise EWiRLConvertError.CreateFmt('Date format not supported [%s]', [FFormat.DateFormat]);
  Result := DateToStr(AValue.AsExtended, FS);
end;

{ TWiRLFormatSetting }

class function TWiRLFormatSetting.WithDateFormat(
  const ADateFormat: string): TWiRLFormatSetting;
begin
  Result.DateFormat := ADateFormat;
end;

class function TWiRLFormatSetting.WithDateTimeFormat(
  const ADateTimeFormat: string): TWiRLFormatSetting;
begin
  Result.DateTimeFormat := ADateTimeFormat;
end;

class function TWiRLFormatSetting.WithFloatFormat(
  const AFloatFormat: string): TWiRLFormatSetting;
begin
  Result.FloatFormat := AFloatFormat;
end;

{ TDefaultFloatConverter }

function TDefaultFloatConverter.ValueFromString(
  const AValue: string): TValue;
var
  FS: TFormatSettings;
begin
  if Length(FFormat.FloatFormat) <> 1 then
    raise EWiRLConvertError.CreateFmt('Invalid format for float: [%s]', [FFormat.FloatFormat]);

  FS.DecimalSeparator := FFormat.FloatFormat[1];
  FS.ThousandSeparator := #0;

  Result := StrToFloat(AValue, FS);
end;

function TDefaultFloatConverter.ValueToString(const AValue: TValue): string;
var
  FS: TFormatSettings;
begin
  if Length(FFormat.FloatFormat) <> 1 then
    raise EWiRLConvertError.CreateFmt('Invalid format for float: [%s]', [FFormat.FloatFormat]);

  FS.DecimalSeparator := FFormat.FloatFormat[1];
  FS.ThousandSeparator := #0;

  Result := FloatToStr(AValue.AsExtended, FS);
end;

{ TDefaultIntegerConverter }

function TDefaultIntegerConverter.ValueFromString(const AValue: string): TValue;
begin
  Result := StrToInt(AValue);
end;

function TDefaultIntegerConverter.ValueToString(const AValue: TValue): string;
begin
  Result := IntToStr(AValue.AsInteger);
end;

initialization
  DefaultFormatSetting.DateFormat := 'ISO';
  DefaultFormatSetting.DateTimeFormat := 'ISO';
  DefaultFormatSetting.FloatFormat := '.';
  DefaultFormatSetting.IntFormat := '';

  RegisterDefaultConverters;
end.
