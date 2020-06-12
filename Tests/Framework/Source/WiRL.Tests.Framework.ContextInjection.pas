{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Tests.Framework.ContextInjection;

interface

uses
  System.Classes, System.SysUtils, System.StrUtils, System.JSON,
  System.NetEncoding,

  DUnitX.TestFramework,

  WiRL.http.Server,
  WiRL.Core.Engine,
  WiRL.http.Accept.MediaType,
  WiRL.Tests.Mock.Server;

type
  [TestFixture]
  TTestContextInjection = class(TObject)
  private
    FServer: TWiRLServer;
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
    procedure TestRequestOnClass;

    [Test]
    procedure TestApplicationOnClass;

    [Test]
    procedure TestRequestOnMethod;

    [Test]
    procedure TestPersonInjection;

    [Test]
    procedure TestSingletonCounter;
  end;

implementation

uses
  WiRL.Tests.Mock.Classes;

procedure TTestContextInjection.Setup;
begin
  FServer := TWiRLServer.Create(nil);

  // Engine configuration
  FServer.AddEngine<TWiRLEngine>('/rest')
    .SetEngineName('WiRL Test Demo')

    .AddApplication('/app')
      .SetSystemApp(True)
      .SetAppName('Test Application')
      .SetResources(['*']);

  if not FServer.Active then
    FServer.Active := True;

  FRequest := TWiRLTestRequest.Create;
  FResponse := TWiRLTestResponse.Create;
end;

procedure TTestContextInjection.TearDown;
begin
  FServer.Free;
  FRequest.Free;
  FResponse.Free;
end;

procedure TTestContextInjection.TestApplicationOnClass;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/contextinjection/application';
  FServer.HandleRequest(FRequest, FResponse);
  Assert.AreEqual('/app', FResponse.Content);
end;

procedure TTestContextInjection.TestHelloWorld;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/contextinjection';
  FServer.HandleRequest(FRequest, FResponse);
  Assert.AreEqual('Hello, context injection!', FResponse.Content);
end;

procedure TTestContextInjection.TestPersonInjection;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/contextinjection/person?name=luca&age=25';
  FServer.HandleRequest(FRequest, FResponse);
  Assert.AreEqual('luca:25', FResponse.Content);
end;

procedure TTestContextInjection.TestRequestOnClass;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/contextinjection/request';
  FServer.HandleRequest(FRequest, FResponse);
  Assert.AreEqual('/rest/app/contextinjection/request', FResponse.Content);
end;

procedure TTestContextInjection.TestRequestOnMethod;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/contextinjection/requestmethod?value=123';
  FServer.HandleRequest(FRequest, FResponse);
  Assert.AreEqual('123', FResponse.Content);
end;

procedure TTestContextInjection.TestSingletonCounter;
begin
  GetGlobalCounter.Reset;
  GetGlobalCounter.Inc;

  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/contextinjection/addcounter';
  FServer.HandleRequest(FRequest, FResponse);
  Assert.AreEqual('2', FResponse.Content);

end;

initialization
  TDUnitX.RegisterTestFixture(TTestContextInjection);

end.
