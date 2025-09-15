{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Tests.Framework.Response;

interface

uses
  System.Classes, System.SysUtils, System.StrUtils, System.JSON,
  System.NetEncoding,

  DUnitX.TestFramework,

  WiRL.http.Accept.MediaType,
  WiRL.http.Response,
  WiRL.http.Headers,
  WiRL.Tests.Mock.Server;

type
  [TestFixture]
  TTestResponse = class(TObject)
  private
    FResponse: TWiRLTestResponse;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestDate;
    [Test]
    procedure TestEmptyDate;
    [Test]
    procedure TestExpires;
    [Test]
    procedure TestEmptyExpires;
    [Test]
    procedure TestLastModified;
    [Test]
    procedure TestEmptyLastModified;
    [Test]
    procedure TestContent;
    [Test]
    procedure TestContentStream;
    [Test]
    procedure TestEmptyStatusCode;
    [Test]
    procedure TestReasonString;
    [Test]
    procedure TestContentType;
    [Test]
    procedure TestContentEncoding;
    [Test]
    procedure TestContentLength;
    [Test]
    procedure TestEmptyContentLength;
    [Test]
    procedure TestHeaderFields;
    [Test]
    procedure TestContentMediaType;
//    [Test]
//    procedure TestConnection;
    [Test]
    procedure TestAllow;
    [Test]
    procedure TestServer;
    [Test]
    procedure TestWWWAuthenticate;
    [Test]
    procedure TestLocation;
    [Test]
    procedure TestContentLanguage;
  end;

implementation

{ TTestResponse }

const
  OneSecond = 1 {day} / 24 { hours } / 60 { minutes } / 60 { seconds };

procedure TTestResponse.Setup;
begin
  FResponse := TWiRLTestResponse.Create;
end;

procedure TTestResponse.TearDown;
begin
  FResponse.Free;
end;

procedure TTestResponse.TestAllow;
begin
  FResponse.Allow := 'GET, HEAD';
  Assert.AreEqual('GET, HEAD', FResponse.Allow);
end;

//procedure TTestResponse.TestConnection;
//begin
//  FResponse.Connection := 'open';
//  Assert.AreEqual('open', FResponse.Connection);
//end;

procedure TTestResponse.TestContent;
begin
  FResponse.ContentType := 'text/plain; charset=utf-8';
  FResponse.Content := '123';
  Assert.AreEqual('123', FResponse.Content);
end;

procedure TTestResponse.TestContentEncoding;
begin
  FResponse.ContentEncoding := 'gzip';
  Assert.AreEqual('gzip', FResponse.ContentEncoding);
end;

procedure TTestResponse.TestContentLanguage;
begin
  FResponse.ContentLanguage := 'en';
  Assert.AreEqual('en', FResponse.ContentLanguage);
end;

procedure TTestResponse.TestContentLength;
begin
  FResponse.ContentLength := 100;
  Assert.AreEqual(True, FResponse.HasContentLength);
  Assert.AreEqual<Integer>(100, FResponse.ContentLength);
end;

procedure TTestResponse.TestContentMediaType;
begin
  FResponse.ContentType := 'text/plain; charset=utf-8';
  Assert.AreEqual('text', FResponse.ContentMediaType.MainType);
  Assert.AreEqual('plain', FResponse.ContentMediaType.SubType);
  Assert.AreEqual('utf-8', FResponse.ContentMediaType.Charset);
end;

procedure TTestResponse.TestContentStream;
const
  Base64Buffer = 'AAGh/w=='; // 0001A1FF
var
  TestStream: TStream;
  InputBuffer: TBytes;
  OutputBuffer: TBytes;
begin
  InputBuffer := TNetEncoding.Base64.DecodeStringToBytes(Base64Buffer);
  TestStream := TBytesStream.Create(InputBuffer);
  FResponse.ContentStream := TestStream;
  OutputBuffer := FResponse.RawContent;
  Assert.AreEqual(InputBuffer, OutputBuffer);
end;

procedure TTestResponse.TestContentType;
begin
  FResponse.ContentType := 'text/plain';
  Assert.AreEqual('text/plain', FResponse.ContentType);
end;

procedure TTestResponse.TestDate;
var
  LTestDate: TDateTime;
begin
  LTestDate := Now - 10;
  FResponse.Date := LTestDate;
  Assert.AreEqual(LTestDate, FResponse.Date, OneSecond);
end;

procedure TTestResponse.TestEmptyContentLength;
begin
  Assert.IsFalse(FResponse.HasContentLength);
end;

procedure TTestResponse.TestEmptyDate;
begin
  Assert.AreEqual(Now, FResponse.Date, OneSecond);
end;

procedure TTestResponse.TestEmptyExpires;
begin
  Assert.AreEqual(0, FResponse.Expires, OneSecond);
end;

procedure TTestResponse.TestEmptyLastModified;
begin
  Assert.AreEqual(0, FResponse.LastModified, OneSecond);
end;

procedure TTestResponse.TestExpires;
var
  LTestDate: TDateTime;
begin
  LTestDate := Now - 10;
  FResponse.Expires := LTestDate;
  Assert.AreEqual(LTestDate, FResponse.Expires, OneSecond);
end;

procedure TTestResponse.TestHeaderFields;
begin
  FResponse.Headers.Values['x-test-header'] := 'test-value';
  Assert.AreEqual('test-value', FResponse.Headers.Values['x-test-header']);
end;

procedure TTestResponse.TestLastModified;
var
  LTestDate: TDateTime;
begin
  LTestDate := Now - 10;
  FResponse.LastModified := LTestDate;
  Assert.AreEqual(LTestDate, FResponse.LastModified, OneSecond);
end;

procedure TTestResponse.TestLocation;
begin
  FResponse.Location := 'https://github.com/delphi-blocks/WiRL';
  Assert.AreEqual('https://github.com/delphi-blocks/WiRL', FResponse.Location);
end;

procedure TTestResponse.TestReasonString;
begin
  Assert.AreEqual('OK', FResponse.ReasonString);
end;

procedure TTestResponse.TestServer;
begin
  FResponse.Server := 'WiRL WebServer';
  Assert.AreEqual('WiRL WebServer', FResponse.Server);
end;

procedure TTestResponse.TestWWWAuthenticate;
begin
  FResponse.WWWAuthenticate := 'basic';
  Assert.AreEqual('basic', FResponse.WWWAuthenticate);
end;

procedure TTestResponse.TestEmptyStatusCode;
begin
  Assert.AreEqual(200, FResponse.StatusCode);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestResponse);

end.

