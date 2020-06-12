{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Tests.Framework.ExceptionMapper;

interface

uses
  System.Classes, System.SysUtils, System.StrUtils, System.JSON,
  System.NetEncoding,

  DUnitX.TestFramework,

  WiRL.http.Server,
  WiRL.Core.Application,
  WiRL.Core.Engine,
  WiRL.http.Accept.MediaType,
  WiRL.Tests.Mock.Server;

type
  [TestFixture]
  TTestExceptionMapper = class(TObject)
  private
    FServer: TWiRLServer;
    FRequest: TWiRLTestRequest;
    FResponse: TWiRLTestResponse;
    FJSon: TJSONValue;
    FExceptionHandlerCount: Integer;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestHelloWorld;

    [Test]
    procedure TestBasicException;

    [Test]
    procedure TestCustomException;

    [Test]
    procedure TestExceptionSubScriber;
  end;

  TExceptionListener = class(TInterfacedObject, IWiRLHandleListener, IWiRLHandleExceptionListener)
  private
    FTestExceptionMapper: TTestExceptionMapper;
  public
    procedure HandleException(const ASender: TWiRLEngine; const AApplication: TWiRLApplication; E: Exception);
    constructor Create(ATestExceptionMapper: TTestExceptionMapper);
  end;

implementation

{ TTestExceptionMapper }

procedure TTestExceptionMapper.Setup;
begin
  FExceptionHandlerCount := 0;

  FServer := TWiRLServer.Create(nil);

  // Engine configuration
  FServer.AddEngine<TWiRLEngine>('/rest')
    .SetEngineName('WiRL Test Demo')

    .AddSubscriber(TExceptionListener.Create(Self))

    .AddApplication('/app')
      .SetSystemApp(True)
      .SetAppName('Test Application')
      .SetResources(['*']);

  if not FServer.Active then
    FServer.Active := True;

  FJSon := nil;
  FRequest := TWiRLTestRequest.Create;
  FResponse := TWiRLTestResponse.Create;
end;

procedure TTestExceptionMapper.TearDown;
begin
  FJSon.Free;
  FServer.Free;
  FRequest.Free;
  FResponse.Free;
end;

procedure TTestExceptionMapper.TestBasicException;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/exception/basic';
  FServer.HandleRequest(FRequest, FResponse);
  FJSon := TJSONObject.ParseJSONValue(FResponse.Content);
  Assert.AreEqual(500, FResponse.StatusCode);
  Assert.AreEqual(Exception.ClassName, FJSon.GetValue<string>('exception'));
  Assert.AreEqual('Error Message', FJSon.GetValue<string>('message'));
end;

procedure TTestExceptionMapper.TestCustomException;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/exception/customnotfound';
  FServer.HandleRequest(FRequest, FResponse);
  FJSon := TJSONObject.ParseJSONValue(FResponse.Content);
  Assert.AreEqual(400, FResponse.StatusCode);
  Assert.AreEqual('EMyNotFoundException', FJSon.GetValue<string>('exception'));
  Assert.AreEqual('Test', FJSon.GetValue<string>('message'));
  Assert.AreEqual(123, FJSon.GetValue<Integer>('ErrorCode'));
end;

procedure TTestExceptionMapper.TestExceptionSubScriber;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/exception/basic';
  FServer.HandleRequest(FRequest, FResponse);

  Assert.AreEqual(1, FExceptionHandlerCount);
end;

procedure TTestExceptionMapper.TestHelloWorld;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/exception';
  FServer.HandleRequest(FRequest, FResponse);
  Assert.AreEqual('Hello, exception!', FResponse.Content);
end;

{ TExceptionListener }

constructor TExceptionListener.Create(
  ATestExceptionMapper: TTestExceptionMapper);
begin
  inherited Create;
  FTestExceptionMapper := ATestExceptionMapper;
end;

procedure TExceptionListener.HandleException(const ASender: TWiRLEngine;
  const AApplication: TWiRLApplication; E: Exception);
begin
  Inc(FTestExceptionMapper.FExceptionHandlerCount);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestExceptionMapper);

end.
