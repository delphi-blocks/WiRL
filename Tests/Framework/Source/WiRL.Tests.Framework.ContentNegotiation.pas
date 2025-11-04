unit WiRL.Tests.Framework.ContentNegotiation;

interface

uses
  System.SysUtils,
  DUnitX.TestFramework,

  WiRL.Core.Application,
  WiRL.Core.Attributes,
  WiRL.Core.Metadata,
  WiRL.Core.Registry,
  WiRL.Core.Context,
  WiRL.Core.Context.Server,
  WiRL.Engine.REST,
  WiRL.http.Server,
  WiRL.http.Response,
  WiRL.http.Request,
  WiRL.http.Accept.MediaType,

  WiRL.Tests.Mock.MessageBody.XML,
  WiRL.Tests.Mock.Server;

type
  [TestFixture]
  TTestContentNegotiation = class(TObject)
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
    procedure TestMainWithoutAccept;
    [Test]
    procedure TestMainWithAccept;
    [Test]
    procedure TestMainWithAcceptFail;
    [Test]
    procedure TestSimpleObjectJson;
    [Test]
    procedure TestSimpleObjectXml;
    [Test]
    procedure TestSimpleObjectFixedXml;
    [Test]
    procedure TestTextPlainWithCharset;
    [Test]
    procedure TestTextPlainWithoutCharset;
  end;

  TSimpleObject = class
    Value: Integer;
  end;

  [Path('/coneg')]
  TContentNegotiationResource = class
  private
    [CONTEXT]
    FResponse: TWiRLResponse;
  public
    [GET]
    [Produces(TMediaType.TEXT_PLAIN + TMediaType.WITH_CHARSET_UTF8)]
    function TextPlainUTF8(): string;
    [GET]
    [Produces(TMediaType.APPLICATION_XML)]
    [Produces(TMediaType.APPLICATION_JSON)]
    function GetObject(): TSimpleObject;
    [GET]
    [Path('/xml')]
    [Produces(TMediaType.APPLICATION_XML)]
    function GetObjectXml(): TSimpleObject;
    [GET]
    [Path('/text-nocharset')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function TextPlain(): string;
  end;

implementation

{ TTestContentNegotiation }

procedure TTestContentNegotiation.Setup;
begin
  FServer := TWiRLServer.Create(nil);

  // Engine configuration
  FServer.AddEngine<TWiRLRESTEngine>('/rest')
    .SetEngineName('WiRL Test Demo')

    .AddApplication('/app')
      .SetSystemApp(True)
      .SetAppName('Test Application')
      .SetResources(['*'])
      .SetFilters(['*']);

  if not FServer.Active then
    FServer.Active := True;

  FContext := TWiRLContext.Create;

  FRequest := TWiRLTestRequest.Create;
  FContext.AddContainer(FRequest);

  FResponse := TWiRLTestResponse.Create;
  FContext.AddContainer(FResponse);

end;

procedure TTestContentNegotiation.TearDown;
begin
  FServer.Free;
  FContext.Free;
  FRequest.Free;
  FResponse.Free;
end;

procedure TTestContentNegotiation.TestMainWithAccept;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/coneg';
  FRequest.Accept := TMediaType.TEXT_PLAIN + TMediaType.WITH_CHARSET_UTF8;
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual(200, FResponse.StatusCode);
  Assert.AreEqual(TMediaType.TEXT_PLAIN + TMediaType.WITH_CHARSET_UTF8, FResponse.ContentType);
end;

procedure TTestContentNegotiation.TestMainWithAcceptFail;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/coneg';
  FRequest.Accept := TMediaType.TEXT_CSV;
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual(404, FResponse.StatusCode);
end;

procedure TTestContentNegotiation.TestMainWithoutAccept;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/coneg';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual(200, FResponse.StatusCode);
  Assert.AreEqual('ciaoà€', FResponse.Content);
  Assert.AreEqual(TMediaType.TEXT_PLAIN + TMediaType.WITH_CHARSET_UTF8, FResponse.ContentType);
  Assert.AreEqual('TextPlainUTF8', FResponse.HeaderFields['X-Resource-Name']);
end;

procedure TTestContentNegotiation.TestSimpleObjectFixedXml;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/coneg/xml';
  //FRequest.Accept := TMediaType.TEXT_XML;
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual(200, FResponse.StatusCode);
  Assert.AreEqual('GetObjectXml', FResponse.HeaderFields['X-Resource-Name']);
  Assert.AreEqual(TMediaType.APPLICATION_XML, FResponse.ContentType);
end;

procedure TTestContentNegotiation.TestSimpleObjectJson;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/coneg';
  FRequest.Accept := TMediaType.APPLICATION_JSON;
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual(200, FResponse.StatusCode);
  Assert.AreEqual(TMediaType.APPLICATION_JSON, FResponse.ContentType);
end;

procedure TTestContentNegotiation.TestSimpleObjectXml;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/coneg';
  FRequest.Accept := TMediaType.APPLICATION_XML;
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual(200, FResponse.StatusCode);
  Assert.AreEqual(TMediaType.APPLICATION_XML, FResponse.ContentType);
end;

procedure TTestContentNegotiation.TestTextPlainWithCharset;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/coneg';
  FRequest.Accept := TMediaType.TEXT_PLAIN;
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual(200, FResponse.StatusCode);
  Assert.AreEqual(TMediaType.TEXT_PLAIN + TMediaType.WITH_CHARSET_UTF8, FResponse.ContentType);
end;

procedure TTestContentNegotiation.TestTextPlainWithoutCharset;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/coneg/text-nocharset';
  FRequest.Accept := TMediaType.TEXT_PLAIN;
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual(200, FResponse.StatusCode);
  Assert.AreEqual(TMediaType.TEXT_PLAIN, FResponse.ContentType);
  Assert.AreEqual('TextPlain', FResponse.HeaderFields['X-Resource-Name']);
  // Warning: if you doesn't specify the content type with the
  // "Produces" attribute the default is ANSI.
  Assert.AreEqual('ciaoù', TEncoding.ANSI.GetString(FResponse.RawContent));
end;

{ TContentNegotiationResource }

function TContentNegotiationResource.GetObject: TSimpleObject;
begin
  FResponse.HeaderFields['X-Resource-Name'] := 'GetObject';
  Result := TSimpleObject.Create;
  Result.Value := 12;
end;

function TContentNegotiationResource.GetObjectXml: TSimpleObject;
begin
  FResponse.HeaderFields['X-Resource-Name'] := 'GetObjectXml';
  Result := TSimpleObject.Create;
  Result.Value := 12;
end;

function TContentNegotiationResource.TextPlain: string;
begin
  FResponse.HeaderFields['X-Resource-Name'] := 'TextPlain';
  Result := 'ciaoù';
end;

function TContentNegotiationResource.TextPlainUTF8: string;
begin
  FResponse.HeaderFields['X-Resource-Name'] := 'TextPlainUTF8';
  Result := 'ciaoà€';
end;

initialization
  TDUnitX.RegisterTestFixture(TTestContentNegotiation);

  TWiRLResourceRegistry.Instance.RegisterResource<TContentNegotiationResource>;


end.
