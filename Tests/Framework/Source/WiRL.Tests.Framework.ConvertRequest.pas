{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Tests.Framework.ConvertRequest;

interface

uses
  System.Classes, System.SysUtils, System.StrUtils, System.JSON,
  System.NetEncoding,

  DUnitX.TestFramework,

  WiRL.http.Server,
  WiRL.Core.Context.Server,
  WiRL.Engine.REST,
  WiRL.Core.Application,
  WiRL.Configuration.Core,
  WiRL.Configuration.Converter,
  WiRL.http.Accept.MediaType,
  WiRL.Tests.Mock.Server;

type
  [TestFixture]
  TTestConvertRequest = class(TObject)
  private
    FContext: TWiRLContext;
    FServer: TWiRLServer;
    FApplication: IWiRLApplication;
    FEngine: TWiRLRESTEngine;
    FRequest: TWiRLTestRequest;
    FResponse: TWiRLTestResponse;
  public

    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestHelloWorld;

    [Test]
    [TestCase('ISO', 'DEFAULT,2020-05-28,2020*5*28')]
    [TestCase('DMY', 'DMY,28/05/2020,2020*5*28')]
    [TestCase('MDY', 'MDY,05/28/2020,2020*5*28')]
    procedure TestDate(const AFormat, ADateParam, AExpectedValue: string);

    [Test]
    [TestCase('DEFAULT', 'DEFAULT:123.12:123120', ':')]
    [TestCase('DOT', 'DEFAULT|.:123.12:123120', ':')]
    [TestCase('COMMA', 'DEFAULT|,:123,12:123120', ':')]
    procedure TestDouble(const AFormat, ADoubleParam, AExpectedValue: string);

    [Test]
    [TestCase('DEFAULT', 'DEFAULT:123.12:123120', ':')]
    [TestCase('DOT', 'DEFAULT|.:123.12:123120', ':')]
    [TestCase('COMMA', 'DEFAULT|,:123,12:123120', ':')]
    procedure TestDoublePathParam(const AFormat, ADoubleParam, AExpectedValue: string);

    [Test]
    [TestCase('DEFAULT', 'DEFAULT:1234:1234', ':')]
    [TestCase('ZERO', 'DEFAULT:0:0', ':')]
    [TestCase('NEGATIVE', 'DEFAULT:-123:-123', ':')]
    procedure TestInteger(const AFormat, AIntegerParam, AExpectedValue: string);

    [Test]
    [TestCase('TRUE', 'DEFAULT:true:1', ':')]
    [TestCase('FALSE', 'DEFAULT:false:0', ':')]
    procedure TestBoolean(const AFormat, ABooleanParam, AExpectedValue: string);

    [Test]
    [TestCase('DATE_ISO', 'DEFAULT,2020-05-28,2020*5*28')]
    [TestCase('DATE_DMY', 'DMY,28/05/2020,2020*5*28')]
    [TestCase('DATE_MDY', 'MDY,05/28/2020,2020*5*28')]
    procedure TestRequest(const AFormat, ADateParam, AExpectedValue: string);

    [Test]
    [TestCase('DEFAULT', 'DEFAULT:123', ':')]
    [TestCase('ZERO', 'DEFAULT:123', ':')]
    [TestCase('NEGATIVE', 'DEFAULT:123', ':')]
    procedure TestReturnInteger(const AFormat, AExpectedValue: string);

    [Test]
    [TestCase('DEFAULT', 'DEFAULT:123.45', ':')]
    [TestCase('DOT', 'DEFAULT|.:123.45', ':')]
    [TestCase('COMMA', 'DEFAULT|,:123,45', ':')]
    procedure TestReturnDouble(const AFormat, AExpectedValue: string);
  end;

implementation

procedure TTestConvertRequest.Setup;
begin
  FServer := TWiRLServer.Create(nil);

  // Engine configuration
  FEngine := FServer.AddEngine<TWiRLRESTEngine>('/rest');
  FEngine.SetEngineName('WiRL Test Demo');

  FApplication := FEngine.AddApplication('/app');

  FApplication
    .SetSystemApp(True)
    .SetAppName('Test Application')
    .SetResources(['*']);

  if not FServer.Active then
    FServer.Active := True;

  FContext := TWiRLContext.Create;

  FRequest := TWiRLTestRequest.Create;
  FContext.AddContainer(FRequest);

  FResponse := TWiRLTestResponse.Create;
  FContext.AddContainer(FResponse);

end;

procedure TTestConvertRequest.TearDown;
begin
  FApplication := nil;
  FServer.Free;
  FContext.Free;
  FRequest.Free;
  FResponse.Free;
end;

procedure TTestConvertRequest.TestBoolean(const AFormat, ABooleanParam,
  AExpectedValue: string);
begin
  FApplication.Plugin.Configure<IWiRLFormatSetting>.AddFormat(TypeInfo(Boolean), AFormat);
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/convert/boolean?value=' + ABooleanParam;
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual(AExpectedValue, FResponse.Content);
end;

procedure TTestConvertRequest.TestDate(const AFormat, ADateParam, AExpectedValue: string);
begin
  FApplication.Plugin.Configure<IWiRLFormatSetting>.AddFormat(TypeInfo(TDate), AFormat);
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/convert/date?value=' + ADateParam;
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual(AExpectedValue, FResponse.Content);
end;

procedure TTestConvertRequest.TestDouble(const AFormat, ADoubleParam,
  AExpectedValue: string);
begin
  FApplication.Plugin.Configure<IWiRLFormatSetting>.AddFormat(TypeInfo(Double), AFormat);
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/convert/double?value=' + ADoubleParam;
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual(AExpectedValue, FResponse.Content);
end;

procedure TTestConvertRequest.TestDoublePathParam(const AFormat, ADoubleParam,
  AExpectedValue: string);
begin
  FApplication.Plugin.Configure<IWiRLFormatSetting>.AddFormat(TypeInfo(Double), AFormat);
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/convert/doublepath/' + ADoubleParam;
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual(AExpectedValue, FResponse.Content);
end;

procedure TTestConvertRequest.TestHelloWorld;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/convert';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('Hello, convert!', FResponse.Content);
end;

procedure TTestConvertRequest.TestInteger(const AFormat, AIntegerParam,
  AExpectedValue: string);
begin
  FApplication.Plugin.Configure<IWiRLFormatSetting>.AddFormat(TypeInfo(Integer), AFormat);
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/convert/intpath/' + AIntegerParam;
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual(AExpectedValue, FResponse.Content);
end;

procedure TTestConvertRequest.TestRequest(const AFormat, ADateParam,
  AExpectedValue: string);
begin
  FApplication.Plugin.Configure<IWiRLFormatSetting>.AddFormat(TypeInfo(TDate), AFormat);
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/convert/request?date=' + ADateParam;
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual(AExpectedValue, FResponse.Content);
end;

procedure TTestConvertRequest.TestReturnDouble(const AFormat,
  AExpectedValue: string);
begin
  FApplication.Plugin.Configure<IWiRLFormatSetting>.AddFormat(TypeInfo(Double), AFormat);
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/convert/returndouble';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual(AExpectedValue, FResponse.Content);
end;

procedure TTestConvertRequest.TestReturnInteger(const AFormat, AExpectedValue: string);
begin
  FApplication.Plugin.Configure<IWiRLFormatSetting>.AddFormat(TypeInfo(Integer), AFormat);
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/convert/returnint';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual(AExpectedValue, FResponse.Content);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestConvertRequest);

end.
