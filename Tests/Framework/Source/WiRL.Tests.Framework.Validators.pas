{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Tests.Framework.Validators;

interface

uses
  System.Classes, System.SysUtils, System.StrUtils, System.JSON,
  System.NetEncoding,

  DUnitX.TestFramework,

  WiRL.http.Server,
  WiRL.Core.Context.Server,
  WiRL.Engine.REST,
  WiRL.http.Accept.MediaType,
  WiRL.Tests.Mock.Server;

type
  [TestFixture]
  TTestValidators = class(TObject)
  private
    FServer: TWiRLServer;
    FContext: TWiRLContext;
    FRequest: TWiRLTestRequest;
    FResponse: TWiRLTestResponse;
    FJson: TJSONValue;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestHelloWorld;

    [Test]
    procedure TestEcho;

    [Test]
    procedure TestEmptyEcho;

    [Test]
    procedure TestDouble;

    [Test]
    procedure TestDoubleMin;

    [Test]
    procedure TestDoubleMax;

    [Test]
    procedure TestEmail;

    [Test]
    procedure TestEmailInvalidMail;

    [Test]
    procedure TestJsonBody;

    [Test]
    procedure TestJsonBodyNoName;

    [Test]
    [TestCase('Empty', ',test')]
    [TestCase('Value', 'ciao,ciao')]
    procedure TestDefaultString(const Value, ResponseString: string);

    [Test]
    [TestCase('Empty', ',10')]
    [TestCase('Value', '123,123')]
    procedure TestDefaultInteger(const Value: string; ResponseString: Integer);

    [Test]
    [TestCase('Empty', ',y:2020 m:1 d:1')]
    [TestCase('Value', '2020-02-20,y:2020 m:2 d:20')]
    procedure TestDefaultDate(const Value, ResponseString: string);
  end;


implementation

{ TTestResource }

procedure TTestValidators.Setup;
begin
  FServer := TWiRLServer.Create(nil);

  // Engine configuration
  FServer.AddEngine<TWiRLRESTEngine>('/rest')
    .SetEngineName('WiRL Test Demo')

    .AddApplication('/app')
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
  FJson := nil;
end;

procedure TTestValidators.TearDown;
begin
  FServer.Free;
  FContext.Free;
  FRequest.Free;
  FResponse.Free;
  FJson.Free;
end;

procedure TTestValidators.TestDefaultDate(const Value, ResponseString: string);
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/validator/defaultdate?value=' + Value;
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual(ResponseString, FResponse.Content);
end;

procedure TTestValidators.TestDefaultInteger(const Value: string; ResponseString: Integer);
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/validator/defaultinteger?value=' + Value;
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual(IntToStr(ResponseString), FResponse.Content);
end;

procedure TTestValidators.TestDefaultString(const Value, ResponseString: string);
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/validator/defaultstring?value=' + Value;
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual(ResponseString, FResponse.Content);
end;

procedure TTestValidators.TestDouble;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/validator/double/12';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('24', FResponse.Content);
end;

procedure TTestValidators.TestDoubleMax;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/validator/double/100';
  FServer.HandleRequest(FContext, FRequest, FResponse);

  FJson := TJSONObject.ParseJSONValue(FResponse.Content);

  Assert.AreEqual(400, FResponse.StatusCode);
  Assert.AreEqual('Constraint [Max] not enforced', FJson.GetValue<string>('message'));
end;

procedure TTestValidators.TestDoubleMin;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/validator/double/0';
  FServer.HandleRequest(FContext, FRequest, FResponse);

  FJson := TJSONObject.ParseJSONValue(FResponse.Content);

  Assert.AreEqual(400, FResponse.StatusCode);
  Assert.AreEqual('Too small', FJson.GetValue<string>('message'));
end;

procedure TTestValidators.TestEcho;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/validator/echostring?value=ciao';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('ciao', FResponse.Content);
end;

procedure TTestValidators.TestEmail;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/validator/buildemail/?email=luca@wirlfoundation.it&name=luca';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('luca <luca@wirlfoundation.it>', FResponse.Content);
end;

procedure TTestValidators.TestEmailInvalidMail;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/validator/buildemail/?email=luca@wirlfoundation_it&name=luca';
  FServer.HandleRequest(FContext, FRequest, FResponse);

  FJson := TJSONObject.ParseJSONValue(FResponse.Content);

  Assert.AreEqual(400, FResponse.StatusCode);
  Assert.AreEqual('E-Mail is not valid', FJson.GetValue<string>('message'));
end;

procedure TTestValidators.TestEmptyEcho;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/validator/echostring/?value=';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual(400, FResponse.StatusCode);
end;

procedure TTestValidators.TestHelloWorld;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/validator';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('Hello, world!', FResponse.Content);
end;

procedure TTestValidators.TestJsonBody;
begin
  FRequest.Method := 'POST';
  FRequest.Url := 'http://localhost:1234/rest/app/validator/json';
  FRequest.ContentType := TMediaType.APPLICATION_JSON;
  FRequest.Content := '{"name": "luca", "project": "WiRL"}';
  FServer.HandleRequest(FContext, FRequest, FResponse);

  FJson := TJSONObject.ParseJSONValue(FResponse.Content);

  Assert.AreEqual('luca', FResponse.Content);
end;

procedure TTestValidators.TestJsonBodyNoName;
begin
  FRequest.Method := 'POST';
  FRequest.Url := 'http://localhost:1234/rest/app/validator/json';
  FRequest.ContentType := TMediaType.APPLICATION_JSON;
  FRequest.Content := '{"project": "WiRL"}';
  FServer.HandleRequest(FContext, FRequest, FResponse);

  Assert.AreEqual(400, FResponse.StatusCode);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestValidators);

end.
