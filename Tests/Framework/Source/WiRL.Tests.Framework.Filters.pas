{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Tests.Framework.Filters;

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
  TTestFilter = class(TObject)
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
    procedure TestResponseFilter;
    [Test]
    procedure TestMatchingBindingResponseFilter;
    [Test]
    procedure TestNonMatchingBindingResponseFilter;
    [Test]
    procedure TestRequestFilter;
    [Test]
    procedure TestMatchingBindingRequestFilter;
    [Test]
    procedure TestNonMatchingBindingRequestFilter;
    [Test]
    procedure TestPerMatchingFilter;
    [Test]
    procedure TestPerMatchingFilterWithInvalidResource;
    [Test]
    procedure TestChangeHeaderOnResponseFilter;
  end;

implementation

{ TTestFilter }

procedure TTestFilter.Setup;
begin
  FServer := TWiRLServer.Create(nil);

  // Engine configuration
  FServer.AddEngine<TWiRLEngine>('/rest')
    .SetEngineName('WiRL Test Demo')

    .AddApplication('/app')
      .SetSystemApp(True)
      .SetAppName('Test Application')
      .SetResources(['*'])
      .SetFilters(['*']);

  if not FServer.Active then
    FServer.Active := True;

  FRequest := TWiRLTestRequest.Create;
  FResponse := TWiRLTestResponse.Create;

end;

procedure TTestFilter.TearDown;
begin
  FServer.Free;
  FRequest.Free;
  FResponse.Free;
end;

procedure TTestFilter.TestChangeHeaderOnResponseFilter;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/helloworld/exception401';
  FServer.HandleRequest(FRequest, FResponse);
  Assert.AreEqual(400, FResponse.StatusCode);
  Assert.IsTrue(FResponse.HeadersSent);
end;

procedure TTestFilter.TestMatchingBindingRequestFilter;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/helloworld/bindingfilter';
  FServer.HandleRequest(FRequest, FResponse);
  Assert.AreEqual('true', FRequest.Headers['x-request-binded-filter']);
end;

procedure TTestFilter.TestMatchingBindingResponseFilter;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/helloworld/bindingfilter';
  FServer.HandleRequest(FRequest, FResponse);
  Assert.AreEqual('true', FResponse.HeaderFields['x-response-binded-filter']);
end;

procedure TTestFilter.TestNonMatchingBindingRequestFilter;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/helloworld';
  FServer.HandleRequest(FRequest, FResponse);
  Assert.AreNotEqual('true', FRequest.Headers['x-request-binded-filter']);
end;

procedure TTestFilter.TestNonMatchingBindingResponseFilter;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/helloworld';
  FServer.HandleRequest(FRequest, FResponse);
  Assert.AreNotEqual('true', FResponse.HeaderFields['x-response-binded-filter']);
end;

procedure TTestFilter.TestPerMatchingFilter;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/helloworld';
  FServer.HandleRequest(FRequest, FResponse);
  Assert.AreEqual('true', FRequest.Headers['x-prematching-filter']);
end;

procedure TTestFilter.TestPerMatchingFilterWithInvalidResource;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/xxx/yyyy/';
  FServer.HandleRequest(FRequest, FResponse);
  Assert.AreEqual('true', FRequest.Headers['x-prematching-filter']);
  Assert.AreEqual(404, FResponse.StatusCode);
end;

procedure TTestFilter.TestRequestFilter;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/helloworld';
  FServer.HandleRequest(FRequest, FResponse);
  Assert.AreEqual('true', FRequest.Headers['x-request-filter']);
end;

procedure TTestFilter.TestResponseFilter;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/helloworld';
  FServer.HandleRequest(FRequest, FResponse);
  Assert.AreEqual('true', FResponse.HeaderFields['x-response-filter']);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestFilter);

end.
