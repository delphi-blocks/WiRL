{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Tests.Framework.Converter;

interface

uses
  System.SysUtils,
  System.DateUtils,
  DUnitX.TestFramework,

  WiRL.Core.Converter;

type
  Assert = class(DUnitX.TestFramework.Assert)
  public
    class procedure EqualDateTime(const expected, actual : TDateTime; const message : string = '');
  end;

  [TestFixture]
  TTestConvert = class(TObject)
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    [TestCase('Basic', '2020-01-01,2020,1,1')]
    [TestCase('2020-06-14', '2020-06-14,2020,6,14')]
    [TestCase('2020-02-29', '2020-02-29,2020,2,29')]
    procedure Test_ToDateISO(const AStringDate: string; AYear, AMonth, ADay: Integer);

    [Test]
    [TestCase('Epoc', '0,1970,1,1')]
    [TestCase('EpocPlusOneDay', '86400,1970,1,2')]
    [TestCase('2020', '1577836800,2020,1,1')]
    procedure Test_ToDateUnix(const AStringDate: string; AYear, AMonth, ADay: Integer);

    [Test]
    [TestCase('Basic', '01/01/2020,2020,1,1')]
    [TestCase('2020-06-14', '06/14/2020,2020,6,14')]
    [TestCase('2020-02-29', '02/29/2020,2020,2,29')]
    procedure Test_ToDateUS(const AStringDate: string; AYear, AMonth, ADay: Integer);

    [Test]
    [TestCase('Basic', 'DEFAULT,2020-01-01T01:01:01,2020,1,1,1,1,1,0')]
    [TestCase('Midnight', 'DEFAULT,2020-01-01T00:00:00,2020,1,1,0,0,0,0')]
    [TestCase('Twelve', 'DEFAULT,2020-01-01T12:12:12,2020,1,1,12,12,12,0')]
    [TestCase('TwelveFiftyNine', 'DEFAULT,2020-01-01T12:59:59,2020,1,1,12,59,59,0')]

    [TestCase('Basic_UTC', 'DEFAULT|UTC,2020-01-01T01:01:01,2020,1,1,1,1,1,0')]
    [TestCase('Midnight_UTC', 'DEFAULT|UTC,2020-01-01T00:00:00,2020,1,1,0,0,0,0')]
    [TestCase('Twelve_UTC', 'DEFAULT|UC,2020-01-01T12:12:12,2020,1,1,12,12,12,0')]
    [TestCase('TwelveFiftyNine_UTC', 'DEFAULT|UTC,2020-01-01T12:59:59,2020,1,1,12,59,59,0')]

    procedure Test_ToDateTimeISO(const AFormat, AStringDate: string; AYear, AMonth, ADay, AHour, AMin, ASec, AMSec: Integer);

    [TestCase('Basic_NOUTC', 'DEFAULT|NOUTC,2020-01-01T01:01:01,2020,1,1,1,1,1,0')]
    [TestCase('Midnight_NOUTC', 'DEFAULT|NOUTC,2020-01-01T00:00:00,2020,1,1,0,0,0,0')]
    [TestCase('Twelve_NOUTC', 'DEFAULT|NOUTC,2020-01-01T12:12:12,2020,1,1,12,12,12,0')]
    [TestCase('TwelveFiftyNine_NOUTC', 'DEFAULT|NOUTC,2020-01-01T12:59:59,2020,1,1,12,59,59,0')]
    procedure Test_ToDateTimeISO_NOUTC(const AFormat, AStringDate: string; AYear, AMonth, ADay, AHour, AMin, ASec, AMSec: Integer);


    [Test]
    [TestCase('Epoc', '0,1970,1,1,0,0,0,0')]
    [TestCase('EpocPlusOneSec', '1,1970,1,1,0,0,1,0')]
    [TestCase('EpocPlusOneDay', '86400,1970,1,2,0,0,0,0')]
    [TestCase('Midnight', '1577836800,2020,1,1,0,0,0,0')]
    [TestCase('Twelve', '1577880732,2020,1,1,12,12,12,0')]
    [TestCase('TwelveFiftyNine', '1577883599,2020,1,1,12,59,59,0')]
    procedure Test_ToDateTimeUnix(const AStringDate: string; AYear, AMonth, ADay, AHour, AMin, ASec, AMSec: Integer);

    [TestCase('Basic', '2020-01-01,2020,1,1')]
    [TestCase('2020-06-14', '2020-06-14,2020,6,14')]
    [TestCase('2020-02-29', '2020-02-29,2020,2,29')]
    procedure Test_FromDateISO(const AStringDate: string; AYear, AMonth, ADay: Integer);

    [Test]
    [TestCase('Basic', '2020-01-01T01:01:01.000Z,2020,1,1,1,1,1,0')]
    [TestCase('Midnight', '2020-01-01T00:00:00.000Z,2020,1,1,0,0,0,0')]
    [TestCase('Twelve', '2020-01-01T12:12:12.000Z,2020,1,1,12,12,12,0')]
    [TestCase('TwelveFiftyNine', '2020-01-01T12:59:59.000Z,2020,1,1,12,59,59,0')]
    procedure Test_FromDateTimeISO(const AStringDate: string; AYear, AMonth, ADay, AHour, AMin, ASec, AMSec: Integer);

    [Test]
    [TestCase('Epoc', '0,1970,1,1,0,0,0,0')]
    [TestCase('EpocPlusOneSec', '1,1970,1,1,0,0,1,0')]
    [TestCase('EpocPlusOneDay', '86400,1970,1,2,0,0,0,0')]
    [TestCase('Midnight', '1577836800,2020,1,1,0,0,0,0')]
    [TestCase('Twelve', '1577880732,2020,1,1,12,12,12,0')]
    [TestCase('TwelveFiftyNine', '1577883599,2020,1,1,12,59,59,0')]
    procedure Test_FromDateTimeUnix(const AStringDate: string; AYear, AMonth, ADay, AHour, AMin, ASec, AMSec: Integer);

    [Test]
    [TestCase('Epoc', '0,1970,1,1')]
    [TestCase('EpocPlusOneDay', '86400,1970,1,2')]
    [TestCase('2020', '1577836800,2020,1,1')]
    procedure Test_FromDateUnix(const AStringDate: string; AYear, AMonth, ADay: Integer);

    [Test]
    [TestCase('Basic', '01/01/2020,2020,1,1')]
    [TestCase('2020-06-14', '06/14/2020,2020,6,14')]
    [TestCase('2020-02-29', '02/29/2020,2020,2,29')]
    procedure Test_FromDateUS(const AStringDate: string; AYear, AMonth, ADay: Integer);

    [Test]
    [TestCase('Basic', '0,0')]
    [TestCase('DotOne', '0.1,0.1')]
    [TestCase('BigNumber', '1000000.00012,1000000.00012')]
    procedure Test_FromFloatDefault(const AStringFloat: string; AFloat: Double);

    [Test]
    [TestCase('Basic', '0,0')]
    [TestCase('DotOne', '0,1;0.1')]
    [TestCase('BigNumber', '1000000,00012;1000000.00012', ';')]
    procedure Test_FromFloatDefaultComma(const AStringFloat: string; AFloat: Double);

    [Test]
    [TestCase('Basic', '0,0')]
    [TestCase('DotOne', '0.1,0.1')]
    [TestCase('BigNumber', '1000000.00012,1000000.00012')]
    procedure Test_ToFloatDefault(const AStringFloat: string; AFloat: Double);

    [Test]
    [TestCase('Basic', '0,0')]
    [TestCase('DotOne', '0,1;0.1')]
    [TestCase('BigNumber', '1000000,00012;1000000.00012', ';')]
    procedure Test_ToFloatDefaultComma(const AStringFloat: string; AFloat: Double);

    [Test]
    [TestCase('Zero', '0,0')]
    [TestCase('One', '1,1')]
    [TestCase('BigNumber', '1234567890,1234567890')]
    [TestCase('Negative', '-1234567890,-1234567890')]
    procedure Test_ToInteger(const AStringInteger: string; ANumber: Integer);

    [Test]
    [TestCase('Basic', '0,0')]
    [TestCase('DotOne', '0.1,0.1')]
    [TestCase('BigNumber', '1000000.00012,1000000.00012')]
    [TestCase('BigNumberWithSeparator', '1,000,000.00012;1000000.00012', ';')]
    procedure Test_ToCurrencyDefault(const AStringCurrency: string; ACurrency: Currency);

    [Test]
    [TestCase('Basic', '0.00,0')]
    [TestCase('DotOne', '0.10,0.1')]
    [TestCase('BigNumber', '1,000,000.12;1000000.12', ';')]
    [TestCase('BigNumberWithSeparator', '1,000,000.12;1000000.12', ';')]
    procedure Test_FromCurrencyDefault(const AStringCurrency: string; ACurrency: Currency);

    [Test]
    [TestCase('True', 'True,True')]
    [TestCase('False', 'False,False')]
    procedure Test_FromBooleanDefault(const AStringBoolean: string; ABoolean: Boolean);

    [Test]
    [TestCase('True', 'True,True')]
    [TestCase('False', 'False,False')]
    [TestCase('Empty', ',False')]
    procedure Test_ToBooleanDefault(const AStringBoolean: string; ABoolean: Boolean);

  end;

implementation

uses
  DUnitX.ResStrs, DUnitX.Assert, System.TimeSpan;

{ TTestConvert }

procedure TTestConvert.Setup;
begin

end;

procedure TTestConvert.TearDown;
begin

end;

procedure TTestConvert.Test_FromBooleanDefault(const AStringBoolean: string; ABoolean: Boolean);
begin
  Assert.AreEqual(AStringBoolean, TWiRLConvert.From<Boolean>(ABoolean));
end;

procedure TTestConvert.Test_FromCurrencyDefault(const AStringCurrency: string; ACurrency: Currency);
begin
  Assert.AreEqual(AStringCurrency, TWiRLConvert.From<Currency>(ACurrency));
end;

procedure TTestConvert.Test_FromDateISO(const AStringDate: string; AYear,
  AMonth, ADay: Integer);
begin
  Assert.AreEqual(AStringDate, TWiRLConvert.From<TDate>(EncodeDate(AYear, AMonth, ADay)));
end;

procedure TTestConvert.Test_FromDateTimeISO(const AStringDate: string; AYear,
  AMonth, ADay, AHour, AMin, ASec, AMSec: Integer);
begin
  Assert.AreEqual(AStringDate, TWiRLConvert.From<TDateTime>(EncodeDate(AYear, AMonth, ADay) + EncodeTime(AHour, AMin, ASec, AMSec)));
end;

procedure TTestConvert.Test_FromDateTimeUnix(const AStringDate: string; AYear,
  AMonth, ADay, AHour, AMin, ASec, AMSec: Integer);
begin
  Assert.AreEqual(AStringDate,
    TWiRLConvert.From<TDateTime>(EncodeDate(AYear, AMonth, ADay) + EncodeTime(AHour, AMin, ASec, AMSec), TWiRLFormatSetting.UNIX));
end;

procedure TTestConvert.Test_FromDateUnix(const AStringDate: string; AYear, AMonth, ADay: Integer);
begin
  Assert.AreEqual(AStringDate,
    TWiRLConvert.From<TDate>(EncodeDate(AYear, AMonth, ADay), TWiRLFormatSetting.UNIX));
end;

procedure TTestConvert.Test_FromDateUS(const AStringDate: string; AYear, AMonth, ADay: Integer);
begin
  Assert.AreEqual(AStringDate,
    TWiRLConvert.From<TDate>(EncodeDate(AYear, AMonth, ADay), TWiRLFormatSetting.MDY));
end;

procedure TTestConvert.Test_FromFloatDefault(const AStringFloat: string; AFloat: Double);
begin
  Assert.AreEqual(AStringFloat, TWiRLConvert.From<Double>(AFloat));
end;

procedure TTestConvert.Test_FromFloatDefaultComma(const AStringFloat: string; AFloat: Double);
begin
  Assert.AreEqual(AStringFloat, TWiRLConvert.From<Double>(AFloat, TWiRLFormatSetting.COMMA_SEPARATOR));
end;

procedure TTestConvert.Test_ToBooleanDefault(const AStringBoolean: string; ABoolean: Boolean);
begin
  Assert.AreEqual(ABoolean, TWiRLConvert.AsType<Boolean>(AStringBoolean));
end;

procedure TTestConvert.Test_ToCurrencyDefault(const AStringCurrency: string; ACurrency: Currency);
begin
  Assert.AreEqual(ACurrency, TWiRLConvert.AsType<Currency>(AStringCurrency));
end;

procedure TTestConvert.Test_ToDateISO(const AStringDate: string; AYear, AMonth, ADay: Integer);
begin
  Assert.AreEqual(EncodeDate(AYear, AMonth, ADay), TWiRLConvert.AsType<TDate>(AStringDate));
end;

procedure TTestConvert.Test_ToDateTimeISO(const AFormat, AStringDate: string; AYear, AMonth, ADay, AHour, AMin, ASec, AMSec: Integer);
var
  Expected: TDateTime;
begin
  Expected := EncodeDate(AYear, AMonth, ADay) + EncodeTime(AHour, AMin, ASec, AMSec);
  Assert.EqualDateTime(Expected, TWiRLConvert.AsType<TDateTime>(AStringDate, AFormat));
end;

procedure TTestConvert.Test_ToDateTimeISO_NOUTC(const AFormat,
  AStringDate: string; AYear, AMonth, ADay, AHour, AMin, ASec, AMSec: Integer);
var
  Expected: TDateTime;
  Bias: Integer;
  TimeZone: TTimeZone;
begin
  Expected := EncodeDate(AYear, AMonth, ADay) + EncodeTime(AHour, AMin, ASec, AMSec);

  TimeZone := TTimeZone.Local;
  Bias := Trunc(TimeZone.GetUTCOffset(Expected).Negate.TotalMinutes);
  Expected := IncMinute(Expected, -Bias);

  Assert.EqualDateTime(Expected, TWiRLConvert.AsType<TDateTime>(AStringDate, AFormat));
end;

procedure TTestConvert.Test_ToDateTimeUnix(const AStringDate: string; AYear,
  AMonth, ADay, AHour, AMin, ASec, AMSec: Integer);
var
  Expected: TDateTime;
begin
  Expected := EncodeDate(AYear, AMonth, ADay) + EncodeTime(AHour, AMin, ASec, AMSec);

  Assert.EqualDateTime(
    Expected,
    TWiRLConvert.AsType<TDateTime>(AStringDate, TWiRLFormatSetting.UNIX));
end;

procedure TTestConvert.Test_ToDateUnix(const AStringDate: string; AYear, AMonth, ADay: Integer);
begin
  Assert.AreEqual(
    EncodeDate(AYear, AMonth, ADay),
    TWiRLConvert.AsType<TDate>(AStringDate, TWiRLFormatSetting.UNIX));
end;

procedure TTestConvert.Test_ToDateUS(const AStringDate: string; AYear, AMonth, ADay: Integer);
begin
  Assert.AreEqual(
    EncodeDate(AYear, AMonth, ADay),
    TWiRLConvert.AsType<TDate>(AStringDate, TWiRLFormatSetting.MDY));
end;

procedure TTestConvert.Test_ToFloatDefault(const AStringFloat: string; AFloat: Double);
begin
  Assert.AreEqual(AFloat, TWiRLConvert.AsType<Double>(AStringFloat));
end;

procedure TTestConvert.Test_ToFloatDefaultComma(const AStringFloat: string; AFloat: Double);
begin
  Assert.AreEqual(AFloat, TWiRLConvert.AsType<Double>(AStringFloat, TWiRLFormatSetting.COMMA_SEPARATOR));
end;

procedure TTestConvert.Test_ToInteger(const AStringInteger: string; ANumber: Integer);
begin
  Assert.AreEqual(ANumber, TWiRLConvert.AsType<Integer>(AStringInteger));
end;

{ Assert }

class procedure Assert.EqualDateTime(const expected, actual: TDateTime; const message : string);
const
  DateTimeFormat = 'yyyy-mm-dd hh:nn:ss.zzz';
begin
  AreEqual(FormatDateTime(DateTimeFormat, expected), FormatDateTime(DateTimeFormat, actual), message);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestConvert);

end.
