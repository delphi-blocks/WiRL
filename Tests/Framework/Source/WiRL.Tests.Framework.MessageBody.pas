{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Tests.Framework.MessageBody;

interface

uses
  System.Classes, System.SysUtils, System.StrUtils, System.JSON,
  System.NetEncoding,

  DUnitX.TestFramework,

  WiRL.http.Server,
  WiRL.core.Context.Server,
  WiRL.Engine.REST,
  WiRL.http.Accept.MediaType,
  WiRL.Tests.Mock.Server;

type
  [TestFixture]
  TTestMessageBody = class(TObject)
  private
    FServer: TWiRLServer;
    FContext: TWiRLContext;
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
    procedure TestJson;

    [Test]
    procedure TestSendJsonObject;

    [Test]
    procedure TestReadJsonObject;

    [Test]
    procedure TestSendJsonRecord;

    [Test]
    procedure TestReadJsonRecord;

    [Test]
    procedure TestCustomMessageBodyReaderObject;

    [Test]
    procedure TestCustomMessageBodyReaderObjectWrongMediaType;

    [Test]
    procedure TestCustomMessageBodyWriterObject;

    [Test]
    procedure TestCustomMessageBodyReaderRecord;

    [Test]
    procedure TestCustomMessageBodyWriterRecord;

    [Test]
    procedure TestQueryParamJsonObject;

    [Test]
    procedure TestReadStream;

    [Test]
    procedure TestReadStreamImage;
  end;

implementation

{ TTestResource }

uses
  WiRL.Tests.Mock.Classes;

procedure TTestMessageBody.Setup;
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

procedure TTestMessageBody.TearDown;
begin
  FServer.Free;
  FContext.Free;
  FRequest.Free;
  FResponse.Free;
end;

procedure TTestMessageBody.TestCustomMessageBodyReaderObject;
begin
  FRequest.Method := 'POST';
  FRequest.Url := 'http://localhost:1234/rest/app/messagebody/testobject';
  FRequest.ContentType := TestPersonMediaType;
  FRequest.Content := 'Name=luca' + sLineBreak + 'Age=25';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('TTestPersonObject/luca/25', FResponse.Content);
end;

procedure TTestMessageBody.TestCustomMessageBodyReaderObjectWrongMediaType;
begin
  FRequest.Method := 'POST';
  FRequest.Url := 'http://localhost:1234/rest/app/messagebody/testobject';
  FRequest.ContentType := TMediaType.IMAGE_PNG;
  FRequest.Content := 'Name=luca' + sLineBreak + 'Age=25';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual(404, FResponse.StatusCode);
end;

procedure TTestMessageBody.TestCustomMessageBodyReaderRecord;
begin
  FRequest.Method := 'POST';
  FRequest.Url := 'http://localhost:1234/rest/app/messagebody/testrecord';
  FRequest.ContentType := TestPersonMediaType;
  FRequest.Content := 'Name=luca' + sLineBreak + 'Age=25';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('TTestPersonRecord/luca/25', FResponse.Content);
end;

procedure TTestMessageBody.TestCustomMessageBodyWriterObject;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/messagebody/testobject?name=luca&age=25';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('Name=luca' + sLineBreak + 'Age=25' + sLineBreak, FResponse.Content);
end;

procedure TTestMessageBody.TestCustomMessageBodyWriterRecord;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/messagebody/testrecord?name=luca&age=25';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('Name=luca' + sLineBreak + 'Age=25' + sLineBreak, FResponse.Content);
end;

procedure TTestMessageBody.TestHelloWorld;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/messagebody';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('Hello, message body!', FResponse.Content);
end;

procedure TTestMessageBody.TestJson;
begin
  FRequest.Method := 'POST';
  FRequest.Url := 'http://localhost:1234/rest/app/messagebody/json';
  FRequest.ContentType := TMediaType.APPLICATION_JSON;
  FRequest.Content := '{"name": "luca"}';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('luca', FResponse.Content);
end;

procedure TTestMessageBody.TestQueryParamJsonObject;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/messagebody/testobjectinurl?person={"Name":"luca","Age":25}';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('TTestPersonObject/luca/25', FResponse.Content);
end;

procedure TTestMessageBody.TestReadJsonObject;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/messagebody/jsonobject?name=luca&age=25';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual(TMediaType.APPLICATION_JSON, FResponse.ContentType);
  Assert.AreEqual('{"Name":"luca","Age":25}', FResponse.Content);
end;

procedure TTestMessageBody.TestReadJsonRecord;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/messagebody/jsonrecord?name=luca&age=25';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual(TMediaType.APPLICATION_JSON, FResponse.ContentType);
  Assert.AreEqual('{"Name":"luca","Age":25}', FResponse.Content);
end;

procedure TTestMessageBody.TestReadStream;
begin
  FRequest.Method := 'POST';
  FRequest.Url := 'http://localhost:1234/rest/app/messagebody/readstream';
  FRequest.ContentType := TMediaType.APPLICATION_OCTET_STREAM;
  FRequest.Content := '1234567890';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('10', FResponse.Content);
end;

procedure TTestMessageBody.TestReadStreamImage;
begin
  FRequest.Method := 'POST';
  FRequest.Url := 'http://localhost:1234/rest/app/messagebody/readstream';
  FRequest.ContentType := TMediaType.IMAGE_PNG;
  FRequest.Content := '1234567890';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('10', FResponse.Content);
end;

procedure TTestMessageBody.TestSendJsonObject;
begin
  FRequest.Method := 'POST';
  FRequest.Url := 'http://localhost:1234/rest/app/messagebody/jsonobject';
  FRequest.ContentType := TMediaType.APPLICATION_JSON;
  FRequest.Content := '{"Name": "luca", "Age": 25}';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('TTestPersonObject/luca/25', FResponse.Content);
end;

procedure TTestMessageBody.TestSendJsonRecord;
begin
  FRequest.Method := 'POST';
  FRequest.Url := 'http://localhost:1234/rest/app/messagebody/jsonrecord';
  FRequest.ContentType := TMediaType.APPLICATION_JSON;
  FRequest.Content := '{"Name": "luca", "Age": 25}';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('TTestPersonRecord/luca/25', FResponse.Content);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestMessageBody);

end.
