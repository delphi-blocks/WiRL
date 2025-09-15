{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Tests.Framework.Core;

interface

uses
  System.SysUtils,
  DUnitX.TestFramework,

  WiRL.Core.Classes,
  WiRL.http.URL,
  WiRL.http.Accept.MediaType,
  WiRL.Tests.Mock.Server;

type

  [TestFixture]
  TTestMediaType = class(TObject)
  private
    FMediaType: TMediaType;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    [TestCase('Dialect', 'text/html;q=0.5;dialect=extjs')]
    procedure TestDialect(AMediaType: string);

  end;

  [TestFixture]
  TTestEncoding = class(TObject)
  public
    [Test]
    [TestCase('Empty', ',0')]
    [TestCase('Basic', 'ciao,4')]
    [TestCase('Complex', 'èò€,7')]
    procedure TestUTF8NoBOM_GetByteCount(AString: string; ASize: Integer);
    [Test]
    [TestCase('Empty', ',0')]
    [TestCase('Basic', 'ciao,8')]
    [TestCase('Complex', 'èò€,6')]
    procedure TestUnicodeLEEncodingNoBOM_GetByteCount(AString: string; ASize: Integer);
    [Test]
    [TestCase('Empty', ',0')]
    [TestCase('Basic', 'ciao,8')]
    [TestCase('Complex', 'èò€,6')]
    procedure TestUnicodeBEEncodingNoBOM_GetByteCount(AString: string; ASize: Integer);

  end;


  [TestFixture]
  TTestUrl = class(TObject)
  public
    [Test]
    procedure TestResource;

    [Test]
    procedure TestQueryParams;

    //[Test]
    procedure TestPathParamsToString;

    [Test]
    procedure TestPathTokens;

    [Test]
    [TestCase('Basic', 'first,second,third,/first/second/third/')]
    [TestCase('SomeSlash', 'first/,/second,/third,/first/second/third/')]
    [TestCase('SomeMoreSlash', '/first/,/second/,/third/,/first/second/third/')]
    [TestCase('SomeDoubleSlash', '//first/,//second/,/third///,/first/second/third/')]
    procedure TestCombinePath(const Path1, Path2, Path3, ResultPath: string);

    [Test]
    [TestCase('Basic', 'http://localhost:1234/rest/app/hello,hello')]
    [TestCase('TwoPartsMatch', 'http://localhost:1234/rest/app/hello/foo/bar,hello/foo')]
    [TestCase('ThreePartsMatch', 'http://localhost:1234/rest/app/hello/foo/bar,hello/foo/bar')]
    procedure TestMatchResource(const Path1, Path2: string);

    [Test]
    [TestCase('Basic', 'http://localhost:1234/rest/app/hello,ciao')]
    [TestCase('TwoPartsMatch', 'http://localhost:1234/rest/app/hello/foo/bar,hello/bar')]
    [TestCase('ThreePartsMatch', 'http://localhost:1234/rest/app/hello/foo/bar,hello/foo/foo')]
    procedure TestNoMatchResource(const Path1, Path2: string);
  end;


implementation

procedure TTestMediaType.Setup;
begin
  //FMediaType := TMediaType.Create;
end;

procedure TTestMediaType.TearDown;
begin
  FMediaType.Free;
end;

procedure TTestMediaType.TestDialect(AMediaType: string);
begin
  FMediaType := TMediaType.Create(AMediaType);
  Assert.AreEqual('extjs', FMediaType.Dialect);
end;

{ TTestEncoding }

procedure TTestEncoding.TestUnicodeBEEncodingNoBOM_GetByteCount(
  AString: string; ASize: Integer);
var
  LEncoding: TEncoding;
begin
  LEncoding := TUnicodeBEEncodingNoBOM.Create;
  try
    Assert.AreEqual(ASize, LEncoding.GetByteCount(AString));
  finally
    LEncoding.Free;
  end;
end;

procedure TTestEncoding.TestUnicodeLEEncodingNoBOM_GetByteCount(
  AString: string; ASize: Integer);
var
  LEncoding: TEncoding;
begin
  LEncoding := TUnicodeLEEncodingNoBOM.Create;
  try
    Assert.AreEqual(ASize, LEncoding.GetByteCount(AString));
  finally
    LEncoding.Free;
  end;
end;

procedure TTestEncoding.TestUTF8NoBOM_GetByteCount(AString: string; ASize: Integer);
var
  LEncoding: TEncoding;
begin
  LEncoding := TUTF8EncodingNoBOM.Create;
  try
    Assert.AreEqual(ASize, LEncoding.GetByteCount(AString));
  finally
    LEncoding.Free;
  end;
end;

{ TTestUrl }

// Remove?
procedure TTestUrl.TestPathTokens;
var
  LUrl: TWiRLURL;
begin
  LUrl := TWiRLURL.Create('http://localhost:1234/app/resource/subresource/path1/path2/path3');
  try
    Assert.AreEqual('app', LUrl.PathTokens[0]);
    Assert.AreEqual('resource', LUrl.PathTokens[1]);
    Assert.AreEqual('subresource', LUrl.PathTokens[2]);
    Assert.AreEqual('path1', LUrl.PathTokens[3]);
    Assert.AreEqual('path2', LUrl.PathTokens[4]);
    Assert.AreEqual('path3', LUrl.PathTokens[5]);
  finally
    LUrl.Free;
  end;
end;

procedure TTestUrl.TestCombinePath(const Path1, Path2, Path3, ResultPath: string);
begin
  Assert.AreEqual(ResultPath, TWiRLURL.CombinePath([Path1, Path2, Path3], True, True));
end;

procedure TTestUrl.TestMatchResource(const Path1, Path2: string);
var
  LUrl: TWiRLURL;
begin
  LUrl := TWiRLURL.Create(Path1);
  try
    LUrl.BasePath := '/rest/app/';
    Assert.IsTrue(LUrl.MatchResource(Path2));
  finally
    LUrl.Free;
  end;
end;

procedure TTestUrl.TestNoMatchResource(const Path1, Path2: string);
var
  LUrl: TWiRLURL;
begin
  LUrl := TWiRLURL.Create(Path1);
  try
    LUrl.BasePath := '/rest/app/';
    Assert.IsFalse(LUrl.MatchResource(Path2));
  finally
    LUrl.Free;
  end;
end;

procedure TTestUrl.TestPathParamsToString;
var
  LUrl: TWiRLURL;
begin
  LUrl := TWiRLURL.Create('http://localhost:1234/app/first/{v1}/{v2}/{v3}');
  try
    //LUrl.BasePath := '/app/first/second/{v1}/{v2}/{v3}';
    Assert.AreEqual('/second/third', LUrl.PathParams.ToString);
  finally
    LUrl.Free;
  end;
end;

procedure TTestUrl.TestQueryParams;
var
  LUrl: TWiRLURL;
begin
  LUrl := TWiRLURL.Create('http://localhost:1234/first?value1=123&value2=4321');
  try
    Assert.AreEqual('value1=123&value2=4321', LUrl.Query);
  finally
    LUrl.Free;
  end;
end;

procedure TTestUrl.TestResource;
var
  LUrl: TWiRLURL;
begin
  LUrl := TWiRLURL.Create('http://localhost:1234/rest/first/second/1/2/3');
  try
    LUrl.BasePath := '/rest/';
    Assert.AreEqual('http', LUrl.Protocol);
    Assert.AreEqual('localhost', LUrl.HostName);
    Assert.AreEqual(1234, LUrl.PortNumber);
    Assert.AreEqual('/rest/first/second/1/2/3', LUrl.Path);
    Assert.AreEqual('first/second/1/2/3', LUrl.Resource);
  finally
    LUrl.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestMediaType);
  TDUnitX.RegisterTestFixture(TTestEncoding);
  TDUnitX.RegisterTestFixture(TTestUrl);

end.
