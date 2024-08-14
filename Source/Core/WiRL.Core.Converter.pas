{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2022 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Converter;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.TypInfo,
  System.Rtti,

  WiRL.Core.Singleton,
  WiRL.Core.Declarations,
  WiRL.Rtti.Utils;

type
  EWiRLConvertError = class(Exception);

  TWiRLFormatSetting = record
  private
  const
    KIND_INDEX = 0;
    DECIMAL_SEPARATOR_INDEX = 1;
    THOUSAND_SEPARATOR_INDEX = 2;
    DIGITS_INDEX = 3;

    USE_UTC_DATE_INDEX = 1;
  private
    FParams: TArray<string>;
    procedure SetParam(const Index: Integer; const Value: string);
    function GetParam(const Index: Integer): string;
    function GetKind: string;
    procedure SetKind(const Value: string);
    function GetDecimalSeparator: Char;
    procedure SetDecimalSeparator(const Value: Char);
    function GetThousandSeparator: Char;
    procedure SetThousandSeparator(const Value: Char);
    function GetDigits: Integer;
    procedure SetDigits(const Value: Integer);
    function GetUseUTCDate: Boolean;
    procedure SetUseUTCDate(const Value: Boolean);
  public
  const
    UTC = 'UTC';
    NOUTC = 'NOUTC';
    ISODATE_UTC = 'DEFAULT|' + UTC;
    ISODATE_NOUTC = 'DEFAULT|' + NOUTC;
    DEFAULT = 'DEFAULT';
    UNIX = 'UNIX';
    MDY = 'MDY';
    DMY = 'DMY';
    DOT_SEPARATOR = 'DEFAULT|.';
    COMMA_SEPARATOR = 'DEFAULT|,';
  public
    property Kind: string read GetKind write SetKind;
    property DecimalSeparator: Char read GetDecimalSeparator write SetDecimalSeparator;
    property ThousandSeparator: Char read GetThousandSeparator write SetThousandSeparator;
    property Digits: Integer read GetDigits write SetDigits;

    property UseUTCDate: Boolean read GetUseUTCDate write SetUseUTCDate;

    function IsDefault: Boolean;

    property Params[const Index: Integer]: string read GetParam write SetParam;

    class operator Implicit(const AValue: string): TWiRLFormatSetting;
    class operator Implicit(const AValue: TWiRLFormatSetting): string;
  end;

  TWiRLConverter = class
  protected
    FFormat: TWiRLFormatSetting;
    FRttiType: TRttiType;
    FAttributes: TAttributeArray;
  public
    function ValueFromString(const AValue: string): TValue; virtual; abstract;
    function ValueToString(const AValue: TValue): string; virtual; abstract;
    constructor Create(const AFormat: TWiRLFormatSetting; ARttiType: TRttiType; const AAttributes: TAttributeArray);
  end;

  TWiRLConverterClass = class of TWiRLConverter;

  TWiRLCheckConverter = reference to function (ARttiType: TRttiType; const AAttributes: TAttributeArray; var AAffinity: Integer; const AFormat: TWiRLFormatSetting): Boolean;

  TWiRLConverterRegistry = class(TObject)
  public
    const AFFINITY_VERY_HIGH = 50;
    const AFFINITY_HIGH = 30;
    const AFFINITY_LOW = 10;
    const AFFINITY_VERY_LOW = 1;
    const AFFINITY_ZERO = 0;
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
    function GetConverter(const ARttiType: TRttiType; const AAttributes: TAttributeArray; const AFormat: string): TWiRLConverter;

    class property Instance: TWiRLConverterRegistry read GetInstance;
    destructor Destroy; override;
  end;

  TWiRLConvert = class
  public
    // String to type
    class function AsType<T>(const AValue: string; const AAttributes: TAttributeArray; const AFormat: string = TWiRLFormatSetting.DEFAULT): T; overload;
    class function AsType<T>(const AValue: string; const AFormat: string = TWiRLFormatSetting.DEFAULT): T; overload;
    class function AsType(const AValue: string; const AAttributes: TAttributeArray; ATypeInfo: PTypeInfo; const AFormat: string = TWiRLFormatSetting.DEFAULT): TValue; overload;
    class function AsType(const AValue: string; ATypeInfo: PTypeInfo; const AFormat: string = TWiRLFormatSetting.DEFAULT): TValue; overload;

    // Type to string
    class function From<T>(const AValue: T; const AAttributes: TAttributeArray; const AFormat: string = TWiRLFormatSetting.DEFAULT): string; overload;
    class function From<T>(const AValue: T; const AFormat: string = TWiRLFormatSetting.DEFAULT): string; overload;
    class function From(const AValue: TValue; const AAttributes: TAttributeArray; ATypeInfo: PTypeInfo; const AFormat: string = TWiRLFormatSetting.DEFAULT): string; overload;
    class function From(const AValue: TValue; ATypeInfo: PTypeInfo; const AFormat: string = TWiRLFormatSetting.DEFAULT): string; overload;
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

  TDefaultInt64Converter = class(TWiRLConverter)
  public
    function ValueFromString(const AValue: string): TValue; override;
    function ValueToString(const AValue: TValue): string; override;
  end;

  TDefaultCurrencyConverter = class(TWiRLConverter)
  public
    function ValueFromString(const AValue: string): TValue; override;
    function ValueToString(const AValue: TValue): string; override;
  end;

  TDefaultBooleanConverter = class(TWiRLConverter)
  public
    function ValueFromString(const AValue: string): TValue; override;
    function ValueToString(const AValue: TValue): string; override;
  end;

  TDefaultStringConverter = class(TWiRLConverter)
  public
    function ValueFromString(const AValue: string): TValue; override;
    function ValueToString(const AValue: TValue): string; override;
  end;

  TDefaultEnumConverter = class(TWiRLConverter)
  public
    function ValueFromString(const AValue: string): TValue; override;
    function ValueToString(const AValue: TValue): string; override;
  end;

const
  DefaultArraySeparator = ',';

implementation

uses
  System.DateUtils;

const
  SecsInADay = 24 * 60 * 60;

function IsDate(ARttiType: TRttiType): Boolean;
begin
  Result := (ARttiType.TypeKind = tkFloat) and (ARttiType.Handle = TypeInfo(TDate));
end;

function IsDateTime(ARttiType: TRttiType): Boolean;
begin
  Result := (ARttiType.TypeKind = tkFloat) and (ARttiType.Handle = TypeInfo(TDateTime));
end;

function IsFloat(ARttiType: TRttiType): Boolean;
begin
  Result :=
    (ARttiType.TypeKind = tkFloat) and
    (ARttiType.Handle <> TypeInfo(TDate)) and
    (ARttiType.Handle <> TypeInfo(TDateTime)) and
    (ARttiType.Handle <> TypeInfo(Currency));
end;

function IsCurrency(ARttiType: TRttiType): Boolean;
begin
  Result :=
    (ARttiType.TypeKind = tkFloat) and
    (ARttiType.Handle = TypeInfo(Currency));
end;

function IsBoolean(ARttiType: TRttiType): Boolean;
begin
  Result :=
    (ARttiType.TypeKind = tkEnumeration) and
    (ARttiType.Handle = TypeInfo(Boolean));
end;

function IsEnum(ARttiType: TRttiType): Boolean;
begin
  Result :=
    (ARttiType.TypeKind = tkEnumeration) and
    (ARttiType.Handle <> TypeInfo(Boolean));
end;

function IsString(ARttiType: TRttiType): Boolean;
begin
  Result :=
    (ARttiType.TypeKind in [tkLString, tkUString, tkWString, tkString]);
end;

{ TWiRLConvert }

class function TWiRLConvert.AsType(const AValue: string; const AAttributes: TAttributeArray;
  ATypeInfo: PTypeInfo; const AFormat: string): TValue;
var
  LConverter: TWiRLConverter;
  LRttiType: TRttiType;
begin
  if AValue = '' then
    Exit(TValue.Empty);

  LRttiType := TRttiHelper.Context.GetType(ATypeInfo);
  LConverter := TWiRLConverterRegistry.Instance.GetConverter(LRttiType, AAttributes, AFormat);
  try
    Result := LConverter.ValueFromString(AValue);
  finally
    LConverter.Free;
  end;
end;

class function TWiRLConvert.AsType<T>(const AValue: string;
  const AAttributes: TAttributeArray; const AFormat: string): T;
begin
  Result := AsType(AValue, AAttributes, TypeInfo(T), AFormat).AsType<T>();
end;

class function TWiRLConvert.AsType(const AValue: string; ATypeInfo: PTypeInfo;
  const AFormat: string): TValue;
var
  LAttributes: TAttributeArray;
begin
  SetLength(LAttributes, 0);
  Result := AsType(AValue, LAttributes, ATypeInfo, AFormat);
end;

class function TWiRLConvert.AsType<T>(const AValue, AFormat: string): T;
var
  LAttributes: TAttributeArray;
begin
  SetLength(LAttributes, 0);
  Result := AsType<T>(AValue, LAttributes, AFormat);
end;

class function TWiRLConvert.From(const AValue: TValue;
  const AAttributes: TAttributeArray; ATypeInfo: PTypeInfo;
  const AFormat: string): string;
var
  LConverter: TWiRLConverter;
  LRttiType: TRttiType;
begin
  LRttiType := TRttiHelper.Context.GetType(ATypeInfo);
  LConverter := TWiRLConverterRegistry.Instance.GetConverter(LRttiType, AAttributes, AFormat);
  try
    Result := LConverter.ValueToString(AValue);
  finally
    LConverter.Free;
  end;
end;

class function TWiRLConvert.From(const AValue: TValue; ATypeInfo: PTypeInfo;
  const AFormat: string): string;
var
  LAttributes: TAttributeArray;
begin
  Result := From(AValue, LAttributes, ATypeInfo, AFormat);
end;

class function TWiRLConvert.From<T>(const AValue: T;
  const AFormat: string): string;
var
  LAttributes: TAttributeArray;
begin
  Result := From<T>(AValue, LAttributes, AFormat);
end;

class function TWiRLConvert.From<T>(const AValue: T;
  const AAttributes: TAttributeArray; const AFormat: string): string;
begin
  Result := From(TValue.From(AValue), AAttributes, TypeInfo(T), AFormat);
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
  const AAttributes: TAttributeArray; const AFormat: string): TWiRLConverter;
var
  LConverterInfo: TWiRLConverterInfo;
  LFormatSetting: TWiRLFormatSetting;
  LCandidate: TWiRLConverterClass;
  LCandidateAffinity: Integer;
  LAffinity: Integer;
begin
  LCandidateAffinity := TWiRLConverterRegistry.AFFINITY_ZERO;
  LCandidate := nil;

  LFormatSetting := TWiRLFormatSetting(AFormat);
  for LConverterInfo in FRegistry do
  begin
    LAffinity := TWiRLConverterRegistry.AFFINITY_VERY_LOW;
    if LConverterInfo.CheckConverter(ARttiType, AAttributes, LAffinity, LFormatSetting) then
    begin
      if not Assigned(LCandidate) or (LAffinity > LCandidateAffinity) then
      begin
        LCandidate := LConverterInfo.ConverterClass;
        LCandidateAffinity := LAffinity;
      end;
    end;
  end;

  if Assigned(LCandidate) then
    Exit(LCandidate.Create(LFormatSetting, ARttiType, AAttributes));

  raise EWiRLConvertError.CreateFmt('Converter not found for type [%s] with format [%s]', [ARttiType.QualifiedName, AFormat]);
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
  Result := TValue.From<TDate>(Trunc(ISO8601ToDate(AValue, True)));
end;

function TISODateConverter.ValueToString(const AValue: TValue): string;
begin
  Result := Copy(DateToISO8601(AValue.AsExtended, True), 1, 10);
end;

{ TWiRLConverter }

constructor TWiRLConverter.Create(const AFormat: TWiRLFormatSetting; ARttiType: TRttiType; const AAttributes: TAttributeArray);
begin
  inherited Create;
  FFormat := AFormat;
  FRttiType := ARttiType;
  FAttributes := AAttributes;
end;

{ TISODateTimeConverter }

function TISODateTimeConverter.ValueFromString(const AValue: string): TValue;
begin
  Result := TValue.From<TDateTime>(ISO8601ToDate(AValue, FFormat.UseUTCDate));
end;

function TISODateTimeConverter.ValueToString(const AValue: TValue): string;
begin
  Result := DateToISO8601(AValue.AsExtended, FFormat.UseUTCDate);
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
  if FFormat = 'MDY' then
    FS.ShortDateFormat := 'mm/dd/yyyy'
  else if FFormat = 'DMY' then
    FS.ShortDateFormat := 'dd/mm/yyyy'
  else
    raise EWiRLConvertError.CreateFmt('Date format not supported [%s]', [string(FFormat)]);
  Result := StrToDate(AValue, FS);
end;

function TYMDDateConverter.ValueToString(const AValue: TValue): string;
var
  FS: TFormatSettings;
begin
  FS.DateSeparator := '/';
  if FFormat = 'MDY' then
    FS.ShortDateFormat := 'mm/dd/yyyy'
  else if FFormat = 'DMY' then
    FS.ShortDateFormat := 'dd/mm/yyyy'
  else
    raise EWiRLConvertError.CreateFmt('Date format not supported [%s]', [string(FFormat)]);
  Result := DateToStr(AValue.AsExtended, FS);
end;

{ TDefaultFloatConverter }

function TDefaultFloatConverter.ValueFromString(
  const AValue: string): TValue;
var
  FS: TFormatSettings;
begin
  FS.DecimalSeparator := FFormat.DecimalSeparator;
  FS.ThousandSeparator := #0;

  Result := StrToFloat(AValue, FS);
end;

function TDefaultFloatConverter.ValueToString(const AValue: TValue): string;
var
  FS: TFormatSettings;
begin
  FS.DecimalSeparator := FFormat.DecimalSeparator;
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

{ TWiRLFormatSetting }

class operator TWiRLFormatSetting.Implicit(
  const AValue: string): TWiRLFormatSetting;
begin
  Result.FParams := AValue.Split(['|']);
end;

function TWiRLFormatSetting.GetDecimalSeparator: Char;
begin
  if Params[DECIMAL_SEPARATOR_INDEX] = '' then
    Result := '.'
  else
    Result := Params[DECIMAL_SEPARATOR_INDEX][1];
end;

function TWiRLFormatSetting.GetDigits: Integer;
begin
  if Params[DIGITS_INDEX] = '' then
    Result := 2
  else
    Result := StrToIntDef(Params[DIGITS_INDEX], 2);
end;

function TWiRLFormatSetting.GetKind: string;
begin
  Result := Params[KIND_INDEX];
end;

function TWiRLFormatSetting.GetParam(const Index: Integer): string;
begin
  if Index < Length(FParams) then
    Result := FParams[Index];
end;

function TWiRLFormatSetting.GetThousandSeparator: Char;
begin
  if Params[THOUSAND_SEPARATOR_INDEX] = '' then
    Result := ','
  else
    Result := Params[THOUSAND_SEPARATOR_INDEX][1];
end;

function TWiRLFormatSetting.GetUseUTCDate: Boolean;
begin
  Result := Params[USE_UTC_DATE_INDEX] <> TWiRLFormatSetting.NOUTC;
end;

class operator TWiRLFormatSetting.Implicit(
  const AValue: TWiRLFormatSetting): string;
begin
  Result := String.Join('|', AValue.FParams);
end;

function TWiRLFormatSetting.IsDefault: Boolean;
begin
  Result := Kind = DEFAULT;
end;

procedure TWiRLFormatSetting.SetDecimalSeparator(const Value: Char);
begin
  Params[DECIMAL_SEPARATOR_INDEX] := Value;
end;

procedure TWiRLFormatSetting.SetDigits(const Value: Integer);
begin
  Params[THOUSAND_SEPARATOR_INDEX] := IntToStr(Value);
end;

procedure TWiRLFormatSetting.SetKind(const Value: string);
begin
  Params[KIND_INDEX] := Value;
end;

procedure TWiRLFormatSetting.SetParam(const Index: Integer;
  const Value: string);
begin
  if Index >= Length(FParams) then
    SetLength(FParams, Index + 1);
  FParams[Index] := Value;
end;

procedure TWiRLFormatSetting.SetThousandSeparator(const Value: Char);
begin
  Params[THOUSAND_SEPARATOR_INDEX] := Value;
end;

procedure TWiRLFormatSetting.SetUseUTCDate(const Value: Boolean);
begin
  if Value then
    Params[USE_UTC_DATE_INDEX] := UTC
  else
    Params[USE_UTC_DATE_INDEX] := NOUTC;
end;

{ TDefaultCurrencyConverter }

function TDefaultCurrencyConverter.ValueFromString(
  const AValue: string): TValue;
var
  FS: TFormatSettings;
begin
  FS.DecimalSeparator := FFormat.DecimalSeparator;
  FS.ThousandSeparator := FFormat.ThousandSeparator;

  Result := StrToCurr(StringReplace(AValue, FFormat.ThousandSeparator, '', [rfReplaceAll]), FS);
end;

function TDefaultCurrencyConverter.ValueToString(const AValue: TValue): string;
var
  FS: TFormatSettings;
  FloatFormat: TFloatFormat;
begin
  FS.DecimalSeparator := FFormat.DecimalSeparator;
  FS.ThousandSeparator := FFormat.ThousandSeparator;
  FS.CurrencyDecimals := FFormat.Digits;
  FloatFormat := ffCurrency;

  Result := StringReplace(CurrToStrF(AValue.AsExtended, FloatFormat, FFormat.Digits, FS), ' ', '', [rfReplaceAll]);
end;

{ TDefaultBooleanConverter }

function TDefaultBooleanConverter.ValueFromString(const AValue: string): TValue;
begin
  if AValue.IsEmpty then
    Result := False
  else
    Result := StrToBool(AValue);
end;

function TDefaultBooleanConverter.ValueToString(const AValue: TValue): string;
begin
  Result := BoolToStr(AValue.AsType<Boolean>, True);
end;

{ TDefaultStringConverter }

function TDefaultStringConverter.ValueFromString(const AValue: string): TValue;
begin
  Result := AValue;
end;

function TDefaultStringConverter.ValueToString(const AValue: TValue): string;
begin
  Result := AValue.AsString;
end;

{ RegisterDefaultConverters }

procedure RegisterDefaultConverters;
begin
  TWiRLConverterRegistry.Instance.RegisterConverter(TISODateConverter,
    function (ARttiType: TRttiType; const AAttributes: TAttributeArray; var AAffinity: Integer; const AFormat: TWiRLFormatSetting): Boolean
    begin
      Result := False;
      if AFormat.IsDefault and IsDate(ARttiType) then
        Exit(True);
    end
  );

  TWiRLConverterRegistry.Instance.RegisterConverter(TISODateTimeConverter,
    function (ARttiType: TRttiType; const AAttributes: TAttributeArray; var AAffinity: Integer; const AFormat: TWiRLFormatSetting): Boolean
    begin
      Result := False;
      if AFormat.IsDefault and IsDateTime(ARttiType) then
        Exit(True);
    end
  );

  TWiRLConverterRegistry.Instance.RegisterConverter(TUnixDateTimeConverter,
    function (ARttiType: TRttiType; const AAttributes: TAttributeArray; var AAffinity: Integer; const AFormat: TWiRLFormatSetting): Boolean
    begin
      Result := False;
      if (AFormat = TWiRLFormatSetting.UNIX) and IsDateTime(ARttiType) then
        Exit(True);
    end
  );

  TWiRLConverterRegistry.Instance.RegisterConverter(TUnixDateConverter,
    function (ARttiType: TRttiType; const AAttributes: TAttributeArray; var AAffinity: Integer; const AFormat: TWiRLFormatSetting): Boolean
    begin
      Result := False;
      if (AFormat = TWiRLFormatSetting.UNIX) and IsDate(ARttiType) then
        Exit(True);
    end
  );

  TWiRLConverterRegistry.Instance.RegisterConverter(TYMDDateConverter,
    function (ARttiType: TRttiType; const AAttributes: TAttributeArray; var AAffinity: Integer; const AFormat: TWiRLFormatSetting): Boolean
    begin
      Result := False;
      if ( (AFormat.Kind = TWiRLFormatSetting.MDY) or (AFormat.Kind = TWiRLFormatSetting.DMY) ) and IsDate(ARttiType) then
        Exit(True);
    end
  );

  TWiRLConverterRegistry.Instance.RegisterConverter(TDefaultFloatConverter,
    function (ARttiType: TRttiType; const AAttributes: TAttributeArray; var AAffinity: Integer; const AFormat: TWiRLFormatSetting): Boolean
    begin
      Result := False;
      if AFormat.IsDefault and IsFloat(ARttiType) then
        Exit(True);
    end
  );

  TWiRLConverterRegistry.Instance.RegisterConverter(TDefaultIntegerConverter,
    function (ARttiType: TRttiType; const AAttributes: TAttributeArray; var AAffinity: Integer; const AFormat: TWiRLFormatSetting): Boolean
    begin
      Result := False;
      if (ARttiType.TypeKind = tkInteger) and AFormat.IsDefault then
        Exit(True);
    end
  );

  TWiRLConverterRegistry.Instance.RegisterConverter(TDefaultInt64Converter,
    function (ARttiType: TRttiType; const AAttributes: TAttributeArray; var AAffinity: Integer; const AFormat: TWiRLFormatSetting): Boolean
    begin
      Result := False;
      if (ARttiType.TypeKind = tkInt64) and AFormat.IsDefault then
        Exit(True);
    end
  );

  TWiRLConverterRegistry.Instance.RegisterConverter(TDefaultCurrencyConverter,
    function (ARttiType: TRttiType; const AAttributes: TAttributeArray; var AAffinity: Integer; const AFormat: TWiRLFormatSetting): Boolean
    begin
      Result := False;
      if IsCurrency(ARttiType) and AFormat.IsDefault then
        Exit(True);
    end
  );

  TWiRLConverterRegistry.Instance.RegisterConverter(TDefaultBooleanConverter,
    function (ARttiType: TRttiType; const AAttributes: TAttributeArray; var AAffinity: Integer; const AFormat: TWiRLFormatSetting): Boolean
    begin
      Result := False;
      if IsBoolean(ARttiType) and AFormat.IsDefault then
        Exit(True);
    end
  );

  TWiRLConverterRegistry.Instance.RegisterConverter(TDefaultStringConverter,
    function (ARttiType: TRttiType; const AAttributes: TAttributeArray; var AAffinity: Integer; const AFormat: TWiRLFormatSetting): Boolean
    begin
      Result := False;
      if IsString(ARttiType) and AFormat.IsDefault then
        Exit(True);
    end
  );

  TWiRLConverterRegistry.Instance.RegisterConverter(TDefaultEnumConverter,
    function (ARttiType: TRttiType; const AAttributes: TAttributeArray; var AAffinity: Integer; const AFormat: TWiRLFormatSetting): Boolean
    begin
      Result := False;
      if IsEnum(ARttiType) and AFormat.IsDefault then
        Exit(True);
    end
  );
end;

{ TDefaultEnumConverter }

function TDefaultEnumConverter.ValueFromString(const AValue: string): TValue;
var
  LOrdinal: Integer;
begin
  LOrdinal := GetEnumValue(FRttiType.Handle, AValue);
  TValue.Make(@LOrdinal, FRttiType.Handle, Result);
end;

function TDefaultEnumConverter.ValueToString(const AValue: TValue): string;
begin
  Result := System.TypInfo.GetEnumName(FRttiType.Handle, AValue.AsOrdinal);
end;

{ TDefaultInt64Converter }

function TDefaultInt64Converter.ValueFromString(const AValue: string): TValue;
begin
  Result := StrToInt64(AValue);
end;

function TDefaultInt64Converter.ValueToString(const AValue: TValue): string;
begin
  Result := IntToStr(AValue.AsInt64);
end;

initialization
  RegisterDefaultConverters;

end.
