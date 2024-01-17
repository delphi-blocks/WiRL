unit WiRL.Tests.Framework.URL;

interface

uses
  System.SysUtils,
  DUnitX.TestFramework,

  WiRL.http.URL;

type
  [TestFixture]
  TTestURL = class(TObject)
  private
    FWiRLURL: TWiRLURL;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestParseUrl();

    [Test]
    [TestCase('Basic', 'http://localhost:8080/rest/|app|resource,http://localhost:8080/rest/app/resource')]
    [TestCase('BasicWithSubResource', 'http://localhost:8080/rest/|app|resource/subresource,http://localhost:8080/rest/app/resource/subresource')]
    [TestCase('SlashAnyWhere', 'http://localhost:8080/rest/|/app/|/resource,http://localhost:8080/rest/app/resource')]
    [TestCase('NoSlashes', 'http://localhost:8080/rest|app|resource,http://localhost:8080/rest/app/resource')]
    [TestCase('AbsoluteApp', 'http://localhost:8080/rest|http://wirl.net/|resource,http://wirl.net/resource')]
    [TestCase('AbsoluteResource', 'http://localhost:8080/rest|https://net.net/|http://wirl.net/resource,http://wirl.net/resource')]
    procedure TestBasicConcat(const APathTokens, AFullPath: string);
  end;

implementation

{ TTestURL }

procedure TTestURL.Setup;
begin
  FWiRLURL := nil;
end;

procedure TTestURL.TearDown;
begin
  if Assigned(FWiRLURL) then
    FWiRLURL.Free;
end;

procedure TTestURL.TestBasicConcat;
var
  LUrl: string;
begin
  LUrl := TWiRLURL.CombinePath(APathTokens.Split(['|']));
  Assert.AreEqual(AFullPath, LUrl);
end;

procedure TTestURL.TestParseUrl;
begin
  FWiRLURL := TWiRLURL.Create('https://localhost:1234/rest/app/resource');
  Assert.AreEqual('https', FWiRLURL.Protocol);
  Assert.AreEqual('localhost', FWiRLURL.HostName);
  Assert.AreEqual(1234, FWiRLURL.PortNumber);
  Assert.AreEqual('/rest/app/resource', FWiRLURL.Path);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestURL);

end.
