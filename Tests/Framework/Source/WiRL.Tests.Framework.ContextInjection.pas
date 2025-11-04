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
  WiRL.Core.Context.Server,
  WiRL.Engine.REST,
  WiRL.http.Accept.MediaType,
  WiRL.Tests.Mock.Server;

type
  [TestFixture]
  TTestContextInjection = class(TObject)
  private
    FServer: TWiRLServer;
    FRequest: TWiRLTestRequest;
    FResponse: TWiRLTestResponse;
    FContext: TWiRLContext;
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
end;

procedure TTestContextInjection.TearDown;
begin
  FServer.Free;
  FContext.Free;
  FRequest.Free;
  FResponse.Free;
end;

procedure TTestContextInjection.TestApplicationOnClass;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/contextinjection/application';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('/app', FResponse.Content);
end;

procedure TTestContextInjection.TestHelloWorld;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/contextinjection';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('Hello, context injection!', FResponse.Content);
end;

procedure TTestContextInjection.TestPersonInjection;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/contextinjection/person?name=luca&age=25';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('luca:25', FResponse.Content);
end;

procedure TTestContextInjection.TestRequestOnClass;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/contextinjection/request';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('/rest/app/contextinjection/request', FResponse.Content);
end;

procedure TTestContextInjection.TestRequestOnMethod;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/contextinjection/requestmethod?value=123';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('123', FResponse.Content);
end;

procedure TTestContextInjection.TestSingletonCounter;
begin
  GetGlobalCounter.Reset;
  GetGlobalCounter.Inc;

  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/contextinjection/addcounter';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('2', FResponse.Content);

end;

initialization
  TDUnitX.RegisterTestFixture(TTestContextInjection);

end.
