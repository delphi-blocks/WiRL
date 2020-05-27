unit WiRL.Tests.Framework.MessageBody;

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
  TTestMessageBody = class(TObject)
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
    procedure TestCustomMessageBodyWriterObject;

    [Test]
    procedure TestCustomMessageBodyReaderRecord;

    [Test]
    procedure TestCustomMessageBodyWriterRecord;
  end;

implementation

{ TTestResource }

uses
  WiRL.Tests.Mock.Classes;

procedure TTestMessageBody.Setup;
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

procedure TTestMessageBody.TearDown;
begin
  FServer.Free;
  FRequest.Free;
  FResponse.Free;
end;

procedure TTestMessageBody.TestCustomMessageBodyReaderObject;
begin
  FRequest.Method := 'POST';
  FRequest.Url := 'http://localhost:1234/rest/app/messagebody/testobject';
  FRequest.ContentType := TestPersonMediaType;
  FRequest.Content := 'Name=luca' + sLineBreak + 'Age=25';
  FServer.HandleRequest(FRequest, FResponse);
  Assert.AreEqual('TTestPersonObject/luca/25', FResponse.Content);
end;

procedure TTestMessageBody.TestCustomMessageBodyReaderRecord;
begin
  FRequest.Method := 'POST';
  FRequest.Url := 'http://localhost:1234/rest/app/messagebody/testrecord';
  FRequest.ContentType := TestPersonMediaType;
  FRequest.Content := 'Name=luca' + sLineBreak + 'Age=25';
  FServer.HandleRequest(FRequest, FResponse);
  Assert.AreEqual('TTestPersonRecord/luca/25', FResponse.Content);
end;

procedure TTestMessageBody.TestCustomMessageBodyWriterObject;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/messagebody/testobject?name=luca&age=25';
  FServer.HandleRequest(FRequest, FResponse);
  Assert.AreEqual('Name=luca' + sLineBreak + 'Age=25' + sLineBreak, FResponse.Content);
end;

procedure TTestMessageBody.TestCustomMessageBodyWriterRecord;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/messagebody/testrecord?name=luca&age=25';
  FServer.HandleRequest(FRequest, FResponse);
  Assert.AreEqual('Name=luca' + sLineBreak + 'Age=25' + sLineBreak, FResponse.Content);
end;

procedure TTestMessageBody.TestHelloWorld;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/messagebody';
  FServer.HandleRequest(FRequest, FResponse);
  Assert.AreEqual('Hello, message body!', FResponse.Content);
end;

procedure TTestMessageBody.TestJson;
begin
  FRequest.Method := 'POST';
  FRequest.Url := 'http://localhost:1234/rest/app/messagebody/json';
  FRequest.ContentType := TMediaType.APPLICATION_JSON;
  FRequest.Content := '{"name": "luca"}';
  FServer.HandleRequest(FRequest, FResponse);
  Assert.AreEqual('luca', FResponse.Content);
end;

procedure TTestMessageBody.TestReadJsonObject;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/messagebody/jsonobject?name=luca&age=25';
  FServer.HandleRequest(FRequest, FResponse);
  Assert.AreEqual(TMediaType.APPLICATION_JSON, FResponse.ContentType);
  Assert.AreEqual('{"Name":"luca","Age":25}', FResponse.Content);
end;

procedure TTestMessageBody.TestReadJsonRecord;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/messagebody/jsonrecord?name=luca&age=25';
  FServer.HandleRequest(FRequest, FResponse);
  Assert.AreEqual(TMediaType.APPLICATION_JSON, FResponse.ContentType);
  Assert.AreEqual('{"Name":"luca","Age":25}', FResponse.Content);
end;

procedure TTestMessageBody.TestSendJsonObject;
begin
  FRequest.Method := 'POST';
  FRequest.Url := 'http://localhost:1234/rest/app/messagebody/jsonobject';
  FRequest.ContentType := TMediaType.APPLICATION_JSON;
  FRequest.Content := '{"Name": "luca", "Age": 25}';
  FServer.HandleRequest(FRequest, FResponse);
  Assert.AreEqual('TTestPersonObject/luca/25', FResponse.Content);
end;

procedure TTestMessageBody.TestSendJsonRecord;
begin
  FRequest.Method := 'POST';
  FRequest.Url := 'http://localhost:1234/rest/app/messagebody/jsonrecord';
  FRequest.ContentType := TMediaType.APPLICATION_JSON;
  FRequest.Content := '{"Name": "luca", "Age": 25}';
  FServer.HandleRequest(FRequest, FResponse);
  Assert.AreEqual('TTestPersonRecord/luca/25', FResponse.Content);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestMessageBody);

end.
