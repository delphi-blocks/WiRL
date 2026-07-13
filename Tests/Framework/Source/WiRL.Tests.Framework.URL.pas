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
    [TestCase('SingleSpaceEncoded', 'http://localhost:8080/rest/app/resource%20name,rest|app|resource name')]
    [TestCase('MultipleSpacesEncoded', 'http://localhost:8080/rest/app/resource%20with%20many%20spaces,rest|app|resource with many spaces')]
    [TestCase('SpacesInMultipleSegments', 'http://localhost:8080/rest/app%20one/resource%20two,rest|app one|resource two')]
    [TestCase('SingleSpaceRaw', 'http://localhost:8080/rest/app/resource name,rest|app|resource name')]
    [TestCase('MultipleSpacesRaw', 'http://localhost:8080/rest/app/resource with many spaces,rest|app|resource with many spaces')]
    procedure TestParseUrlWithSpaces(const AURL, AExpectedTokens: string);

    [Test]
    [TestCase('SingleParamEncoded', 'http://localhost:8080/rest/app/resource?name=John%20Doe,name,John Doe')]
    [TestCase('SingleParamRaw', 'http://localhost:8080/rest/app/resource?name=John Doe,name,John Doe')]
    [TestCase('SecondParamEncoded', 'http://localhost:8080/rest/app/resource?id=1&name=John%20Doe,name,John Doe')]
    [TestCase('MultipleSpacesEncoded', 'http://localhost:8080/rest/app/resource?q=New%20York%20City,q,New York City')]
    procedure TestParseQueryWithSpaces(const AURL, AKey, AExpectedValue: string);

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

procedure TTestURL.TestParseUrlWithSpaces(const AURL, AExpectedTokens: string);
begin
  FWiRLURL := TWiRLURL.Create(AURL);
  Assert.AreEqual(AExpectedTokens, string.Join('|', FWiRLURL.PathTokens));
end;

procedure TTestURL.TestParseQueryWithSpaces(const AURL, AKey, AExpectedValue: string);
begin
  FWiRLURL := TWiRLURL.Create(AURL);
  Assert.AreEqual(AExpectedValue, FWiRLURL.QueryTokens[AKey]);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestURL);

end.
