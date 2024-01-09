unit WiRL.Tests.Framework.GarbageCollector;

interface

uses
  System.SysUtils,
  DUnitX.TestFramework,

  WiRL.Core.GarbageCollector,
  WiRL.Core.Application,
  WiRL.Core.Attributes,
  WiRL.Core.Metadata,
  WiRL.Core.Registry,
  WiRL.Core.Engine,
  WiRL.Core.Context,
  WiRL.http.Server,
  WiRL.http.Response,
  WiRL.http.Request,
  WiRL.http.Accept.MediaType,

  WiRL.Tests.Mock.MessageBody.XML,
  WiRL.Tests.Mock.Server;

type
  [TestFixture]
  TTestGarbageCollector = class(TObject)
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
    procedure TestGarbageCollector;
    [Test]
    procedure TestWithMemoryLeak;
  end;

  TPerson = class
  private
    FName: string;
  public
    property Name: string read FName write FName;
  end;

  [Path('/gc')]
  TCustomGarbageResource = class
  private
    [Context] GC: TWiRLGarbageCollector;
  public
    [GET] [Produces('application/json')]
    function GetObj([QueryParam('name')] const AName: string): TPerson;
  end;


implementation

{ TTestGarbageCollector }

procedure TTestGarbageCollector.Setup;
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

procedure TTestGarbageCollector.TearDown;
begin
  FServer.Free;
  FRequest.Free;
  FResponse.Free;
end;

procedure TTestGarbageCollector.TestGarbageCollector;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/gc';
  FRequest.Accept := TMediaType.APPLICATION_JSON;
  FServer.HandleRequest(FRequest, FResponse);

  Assert.AreEqual(200, FResponse.StatusCode);
  Assert.Pass();
end;

procedure TTestGarbageCollector.TestWithMemoryLeak;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/gc';
  FRequest.Accept := TMediaType.APPLICATION_JSON;
  FServer.HandleRequest(FRequest, FResponse);

  Assert.AreEqual(200, FResponse.StatusCode);
  Assert.Pass();
end;

{ TCustomGarbageResource }

function TCustomGarbageResource.GetObj(const AName: string): TPerson;
var
  LTempObj: TObject;
begin
  LTempObj := TObject.Create;
  GC.AddGarbage(LTempObj);
  Result := TPerson.Create;
  Result.Name := AName;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestGarbageCollector);
  TWiRLResourceRegistry.Instance.RegisterResource<TCustomGarbageResource>;


end.
