{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Tests.Framework.Types;

interface

uses
  System.SysUtils,
  DUnitX.TestFramework,

  WiRL.Core.Types;

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
    [TestCase('Basic', '2020-01-01T01:01:01,2020,1,1,1,1,1,0')]
    [TestCase('Midnight', '2020-01-01T00:00:00,2020,1,1,0,0,0,0')]
    [TestCase('Twelve', '2020-01-01T12:12:12,2020,1,1,12,12,12,0')]
    [TestCase('TwelveFiftyNine', '2020-01-01T12:59:59,2020,1,1,12,59,59,0')]
    procedure Test_ToDateTimeISO(const AStringDate: string; AYear, AMonth, ADay, AHour, AMin, ASec, AMSec: Integer);

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
    [TestCase('DotOne', '0x1,0.1')]
    [TestCase('BigNumber', '1000000x00012,1000000.00012')]
    procedure Test_FromFloatDefaultComma(const AStringFloat: string; AFloat: Double);

    [Test]
    [TestCase('Basic', '0,0')]
    [TestCase('DotOne', '0.1,0.1')]
    [TestCase('BigNumber', '1000000.00012,1000000.00012')]
    procedure Test_ToFloatDefault(const AStringFloat: string; AFloat: Double);

    [Test]
    [TestCase('Basic', '0,0')]
    [TestCase('DotOne', '0x1,0.1')]
    [TestCase('BigNumber', '1000000x00012,1000000.00012')]
    procedure Test_ToFloatDefaultComma(const AStringFloat: string; AFloat: Double);

    [Test]
    [TestCase('Zero', '0,0')]
    [TestCase('One', '1,1')]
    [TestCase('BigNumber', '1234567890,1234567890')]
    [TestCase('Negative', '-1234567890,-1234567890')]
    procedure Test_ToInteger(const AStringInteger: string; ANumber: Integer);

  end;

implementation

uses
  DUnitX.ResStrs, DUnitX.Assert;

{ TTestConvert }

procedure TTestConvert.Setup;
begin

end;

procedure TTestConvert.TearDown;
begin

end;

procedure TTestConvert.Test_FromDateISO(const AStringDate: string; AYear,
  AMonth, ADay: Integer);
begin
  Assert.AreEqual(AStringDate, TWiRLConvert.fromDate(EncodeDate(AYear, AMonth, ADay)));
end;

procedure TTestConvert.Test_FromDateTimeISO(const AStringDate: string; AYear,
  AMonth, ADay, AHour, AMin, ASec, AMSec: Integer);
begin
  Assert.AreEqual(AStringDate, TWiRLConvert.fromDateTime(EncodeDate(AYear, AMonth, ADay) + EncodeTime(AHour, AMin, ASec, AMSec)));
end;

procedure TTestConvert.Test_FromDateTimeUnix(const AStringDate: string; AYear,
  AMonth, ADay, AHour, AMin, ASec, AMSec: Integer);
begin
  Assert.AreEqual(AStringDate,
    TWiRLConvert.fromDateTime(EncodeDate(AYear, AMonth, ADay) + EncodeTime(AHour, AMin, ASec, AMSec), TWiRLFormatSetting.WithDateTimeFormat('UNIX')));
end;

procedure TTestConvert.Test_FromDateUnix(const AStringDate: string; AYear,
  AMonth, ADay: Integer);
begin
  Assert.AreEqual(AStringDate,
    TWiRLConvert.fromDate(EncodeDate(AYear, AMonth, ADay), TWiRLFormatSetting.WithDateFormat('UNIX')));
end;

procedure TTestConvert.Test_FromDateUS(const AStringDate: string; AYear, AMonth,
  ADay: Integer);
begin
  Assert.AreEqual(AStringDate,
    TWiRLConvert.fromDate(EncodeDate(AYear, AMonth, ADay), TWiRLFormatSetting.WithDateFormat('MDY')));
end;

procedure TTestConvert.Test_FromFloatDefault(const AStringFloat: string;
  AFloat: Double);
begin
  Assert.AreEqual(AStringFloat, TWiRLConvert.FromFloat(AFloat));
end;

procedure TTestConvert.Test_FromFloatDefaultComma(const AStringFloat: string;
  AFloat: Double);
begin
  Assert.AreEqual(StringReplace(AStringFloat, 'x', ',', [rfReplaceAll]), TWiRLConvert.FromFloat(AFloat, TWiRLFormatSetting.WithFloatFormat(',')));
end;

procedure TTestConvert.Test_ToDateISO(const AStringDate: string; AYear, AMonth, ADay: Integer);
begin
  Assert.AreEqual(EncodeDate(AYear, AMonth, ADay), TWiRLConvert.ToDate(AStringDate));
end;

procedure TTestConvert.Test_ToDateTimeISO(const AStringDate: string; AYear, AMonth, ADay, AHour, AMin, ASec, AMSec: Integer);
var
  Expected: TDateTime;
begin
  Expected := EncodeDate(AYear, AMonth, ADay) + EncodeTime(AHour, AMin, ASec, AMSec);
  Assert.EqualDateTime(Expected, TWiRLConvert.ToDateTime(AStringDate));
end;

procedure TTestConvert.Test_ToDateTimeUnix(const AStringDate: string; AYear,
  AMonth, ADay, AHour, AMin, ASec, AMSec: Integer);
var
  Expected: TDateTime;
begin
  Expected := EncodeDate(AYear, AMonth, ADay) + EncodeTime(AHour, AMin, ASec, AMSec);

  Assert.EqualDateTime(
    Expected,
    TWiRLConvert.ToDateTime(AStringDate, TWiRLFormatSetting.WithDateTimeFormat('UNIX')));
end;

procedure TTestConvert.Test_ToDateUnix(const AStringDate: string; AYear, AMonth,
  ADay: Integer);
begin
  Assert.AreEqual(
    EncodeDate(AYear, AMonth, ADay),
    TWiRLConvert.ToDate(AStringDate, TWiRLFormatSetting.WithDateFormat('UNIX')));
end;

procedure TTestConvert.Test_ToDateUS(const AStringDate: string; AYear, AMonth,
  ADay: Integer);
begin
  Assert.AreEqual(
    EncodeDate(AYear, AMonth, ADay),
    TWiRLConvert.ToDate(AStringDate, TWiRLFormatSetting.WithDateFormat('MDY')));
end;

procedure TTestConvert.Test_ToFloatDefault(const AStringFloat: string;
  AFloat: Double);
begin
  Assert.AreEqual(AFloat, TWiRLConvert.ToFloat(AStringFloat));
end;

procedure TTestConvert.Test_ToFloatDefaultComma(const AStringFloat: string;
  AFloat: Double);
begin
  Assert.AreEqual(AFloat, TWiRLConvert.ToFloat(StringReplace(AStringFloat, 'x', ',', []), TWiRLFormatSetting.WithFloatFormat(',')));
end;

procedure TTestConvert.Test_ToInteger(const AStringInteger: string;
  ANumber: Integer);
begin
  Assert.AreEqual(ANumber, TWiRLConvert.ToInteger(AStringInteger));
end;

{ Assert }

class procedure Assert.EqualDateTime(const expected, actual: TDateTime; const message : string);
const
  DateTimeFormat = 'yyyy-mm-dd hh:nn:ss.zzz';
begin
  DoAssert;
  if expected <> actual then
    FailFmt(SNotEqualErrorStr, [FormatDateTime(DateTimeFormat, expected), FormatDateTime(DateTimeFormat, actual), message])

end;

initialization
  TDUnitX.RegisterTestFixture(TTestConvert);

end.
