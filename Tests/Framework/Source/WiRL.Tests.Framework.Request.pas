{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Tests.Framework.Request;

{$I WiRL.inc}

interface

uses
  System.Classes, System.SysUtils, System.StrUtils, System.JSON,
  System.NetEncoding,

  DUnitX.TestFramework,

  WiRL.http.Headers,
  WiRL.http.Accept.MediaType,
  WiRL.http.Request,
  WiRL.Tests.Mock.Server;

type
  [TestFixture]
  TTestRequest = class(TObject)
  private
    FRequest: TWiRLTestRequest;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    [TestCase('Full query', 'http://wirl.delphiblocks.com/demo/test?param=value,/demo/test')]
    [TestCase('Basic url', 'http://wirl.delphiblocks.com/,/')]
    [TestCase('Basic localhost', 'http://localhost/,/')]
    [TestCase('Basic https localhost', 'https://localhost/,/')]
    [TestCase('Short url', 'https://localhost/test,/test')]
    [TestCase('Short url with slash', 'https://localhost/test/,/test/')]
    [TestCase('File name', 'https://localhost/test/file.txt,/test/file.txt')]
    procedure TestPathInfo(const AUrl, APathInfo: string);
    [Test]
    [TestCase('http://wirl.delphiblocks.com/demo/test?param=value', 'param=value')]
    [TestCase('http://wirl.delphiblocks.com?query', 'query')]
    [TestCase('http://localhost?query', 'query')]
    procedure TestQuery(const AUrl, AQuery: string);
    [Test]
    procedure TestMethod;
    [Test]
    procedure TestHost;
    [Test]
    procedure TestQueryFields;
    [Test]
    [TestCase('One', '1')]
    [TestCase('OneTwoThree', '123')]
    [TestCase('Negative', '-3')]
    procedure TestQueryFieldsInteger(const AValue: string);
    [Test]
    procedure TestQueryFieldsIntegerEmpty;
    //[Test]
    procedure TestContentFields;
    [Test]
    procedure TestHeaderFields;
    //[Test]
    procedure TestCookieFields;
    [Test]
    [TestCase('Simple default charset', 'Ciao,text/plain')]
    [TestCase('Default charset', 'ащти,text/plain')]
    [TestCase('Simple UTF-8', 'Ciao,text/plain; charset=utf-8')]
    [TestCase('UTF-8', 'aати,text/plain; charset=utf-8')]
    [TestCase('Simple Latin1', 'ciao,text/plain; charset=iso-8859-1')]
    [TestCase('Latin1', 'aати,text/plain; charset=iso-8859-1')]
    procedure TestContent(const Content, ContentType: string);
    [Test]
    procedure TestRawContent;
    [Test]
    procedure TestContentStream;
    [Test]
    procedure TestContentType;
    [Test]
    procedure TestContentLength;
    [Test]
    procedure TestContentVersion;
    [Test]
    procedure TestAuthorization;
    [Test]
    procedure TestAccept;
    [Test]
    procedure TestAcceptableMediaTypesFound;
    [Test]
    procedure TestAcceptableMediaTypesNotFound;
    [Test]
    procedure TestAcceptCharSet;
    [Test]
    procedure TestAcceptableCharSetsFound;
    [Test]
    procedure TestAcceptableCharSetsNotFound;
    [Test]
    procedure TestAcceptEncoding;
    [Test]
    procedure TestAcceptableEncodingsFound;
    [Test]
    procedure TestAcceptableEncodingsNotFound;
    [Test]
    procedure TestAcceptLanguage;
    [Test]
    procedure TestAcceptableLanguagesFound;
    [Test]
    procedure TestAcceptableLanguagesNotFound;
    [Test]
    procedure TestContentMediaType;
    [Test]
    procedure TestReadPathInfo;
    [Test]
    procedure TestWritePathInfo;
    [Test]
    procedure TestReadQuery;
    [Test]
    procedure TestWriteQuery;
  end;

implementation

{ TTestRequest }

procedure TTestRequest.Setup;
begin
  FRequest := TWiRLTestRequest.Create;
end;

procedure TTestRequest.TearDown;
begin
  FRequest.Free;
end;

procedure TTestRequest.TestAccept;
begin
  FRequest.Accept := '*/*';
  Assert.AreEqual('*/*', FRequest.Accept);
end;

procedure TTestRequest.TestAcceptableCharSetsFound;
begin
  FRequest.AcceptCharSet := TMediaType.CHARSET_ISO_8859_1 + ',' + TMediaType.CHARSET_UTF8;
  Assert.IsTrue(FRequest.AcceptableCharSets.Contains(TMediaType.CHARSET_UTF8), 'Charset not found');
end;

procedure TTestRequest.TestAcceptableCharSetsNotFound;
begin
  FRequest.AcceptCharSet := TMediaType.CHARSET_ISO_8859_1 + ',' + TMediaType.CHARSET_UTF8;
  Assert.IsFalse(FRequest.AcceptableCharSets.Contains(TMediaType.CHARSET_UTF16), 'Charset found');
end;

procedure TTestRequest.TestAcceptableEncodingsFound;
begin
  FRequest.AcceptEncoding := 'gzip, deflate';
  Assert.IsTrue(FRequest.AcceptableEncodings.Contains('gzip'), 'Encoding not found');
end;

procedure TTestRequest.TestAcceptableEncodingsNotFound;
begin
  FRequest.AcceptEncoding := 'gzip, deflate';
  Assert.IsFalse(FRequest.AcceptableEncodings.Contains('7zip'), 'Encoding found');
end;

procedure TTestRequest.TestAcceptableLanguagesFound;
begin
  FRequest.AcceptLanguage := 'en-US, it-IT';
  Assert.IsTrue(FRequest.AcceptableLanguages.Contains('it-IT'), 'Language not found');
end;

procedure TTestRequest.TestAcceptableLanguagesNotFound;
begin
  FRequest.AcceptLanguage := 'en-US, it-IT';
  Assert.IsFalse(FRequest.AcceptableLanguages.Contains('it-CH'), 'Language found');
end;

procedure TTestRequest.TestAcceptableMediaTypesFound;
begin
  FRequest.Accept := TMediaType.APPLICATION_PDF + ',' + TMediaType.APPLICATION_XML;
  Assert.IsTrue(FRequest.AcceptableMediaTypes.Contains(TMediaType.APPLICATION_PDF), 'Media type not found');
end;

procedure TTestRequest.TestAcceptableMediaTypesNotFound;
begin
  FRequest.Accept := TMediaType.APPLICATION_PDF + ',' + TMediaType.APPLICATION_XML;
  Assert.IsFalse(FRequest.AcceptableMediaTypes.Contains(TMediaType.APPLICATION_OCTET_STREAM), 'Media type not found');
end;

procedure TTestRequest.TestAcceptCharSet;
begin
  FRequest.AcceptCharSet := 'utf-8';
  Assert.AreEqual('utf-8', FRequest.AcceptCharSet);
end;

procedure TTestRequest.TestAcceptEncoding;
begin
  FRequest.AcceptEncoding := 'gzip, deflate';
  Assert.AreEqual('gzip, deflate', FRequest.AcceptEncoding);
end;

procedure TTestRequest.TestAcceptLanguage;
begin
  FRequest.AcceptLanguage := 'en-US';
  Assert.AreEqual('en-US', FRequest.AcceptLanguage);
end;

procedure TTestRequest.TestAuthorization;
begin
  FRequest.Authorization := 'bearer ASD345SDF46HGSD326HY';
  Assert.AreEqual('bearer ASD345SDF46HGSD326HY', FRequest.Authorization);
end;

procedure TTestRequest.TestContent(const Content, ContentType: string);
begin
  FRequest.ContentType := ContentType;
  FRequest.Content := Content;
  Assert.AreEqual(Content, FRequest.Content);
end;

procedure TTestRequest.TestContentFields;
begin
  Assert.NotImplemented;
end;

procedure TTestRequest.TestContentLength;
begin
  FRequest.ContentLength := 12;
  Assert.AreEqual(12, FRequest.ContentLength);
end;

procedure TTestRequest.TestContentMediaType;
begin
  FRequest.ContentType := 'text/plain; charset=utf-8';
  Assert.AreEqual('text', FRequest.ContentMediaType.MainType);
  Assert.AreEqual('plain', FRequest.ContentMediaType.SubType);
  Assert.AreEqual('utf-8', FRequest.ContentMediaType.Charset);
end;

procedure TTestRequest.TestContentStream;
var
  LStream: TStringStream;
begin
  LStream := TStringStream.Create('test');
  FRequest.ContentStream := LStream;
  Assert.AreSame(LStream, FRequest.ContentStream);
end;

procedure TTestRequest.TestContentType;
begin
  FRequest.ContentType := TMediaType.APPLICATION_PDF;
  Assert.AreEqual(TMediaType.APPLICATION_PDF, FRequest.ContentType);
end;

procedure TTestRequest.TestContentVersion;
begin
  FRequest.ContentVersion := 'V1.2.3';
  Assert.AreEqual('V1.2.3', FRequest.ContentVersion);
end;

procedure TTestRequest.TestCookieFields;
begin
  Assert.NotImplemented;
end;



procedure TTestRequest.TestHeaderFields;
begin
  FRequest.Headers.Values['X-Custom'] := 'Value';
  Assert.AreEqual('Value', FRequest.Headers.Values['X-Custom']);
end;

procedure TTestRequest.TestHost;
begin
  FRequest.Host := 'localhost:1234';
  Assert.AreEqual('localhost:1234', FRequest.Host);
end;

procedure TTestRequest.TestMethod;
begin
  FRequest.Method := 'POST';
  Assert.AreEqual('POST', FRequest.Method);
end;

procedure TTestRequest.TestPathInfo;
begin
  FRequest.Url := AUrl;
  Assert.AreEqual(APathInfo, FRequest.PathInfo);
end;

procedure TTestRequest.TestQuery(const AUrl, AQuery: string);
begin
  FRequest.Url := AUrl;
  Assert.AreEqual(AQuery, FRequest.Query);
end;

procedure TTestRequest.TestQueryFields;
begin
  FRequest.Url := 'http://localhost/test?a=first&b=second';
  Assert.AreEqual('first', FRequest.QueryFields['a']);
  Assert.AreEqual('second', FRequest.QueryFields['b']);
end;

procedure TTestRequest.TestQueryFieldsInteger(const AValue: string);
begin
  FRequest.Url := 'http://localhost/test?value=' + AValue;
  Assert.AreEqual(StrToInt(AValue), FRequest.QueryFields.AsType<Integer>('value'));
end;

procedure TTestRequest.TestQueryFieldsIntegerEmpty;
begin
  FRequest.Url := 'http://localhost/test?value=';
  Assert.AreEqual(0, FRequest.QueryFields.AsType<Integer>('value'));
//  Assert.WillRaise(
//    procedure ()
//    begin
//      FRequest.QueryFields.AsType<Integer>('value');
//    end,
//    EConvertError);
end;

procedure TTestRequest.TestRawContent;
var
  LBuffer: TBytes;
  {$IFNDEF HAS_NEW_ARRAY}
  LRawContent: TBytes;
  {$ENDIF}
begin
  {$IFDEF HAS_NEW_ARRAY}
    FRequest.RawContent := [0, 22, 65, 200];
  {$ELSE}
    SetLength(LRawContent, 4);
    LRawContent[0] := 0;
    LRawContent[1] := 22;
    LRawContent[2] := 65;
    LRawContent[3] := 200;
    FRequest.RawContent := LRawContent;
  {$ENDIF}

  Assert.AreEqual(4, Integer(FRequest.ContentStream.Size), 'Some bytes has been lost');
  FRequest.ContentStream.Position := 0;
  SetLength(LBuffer, FRequest.ContentStream.Size);
  FRequest.ContentStream.Read(LBuffer[0], FRequest.ContentStream.Size);
  Assert.AreEqual(Integer(0), Integer(LBuffer[0]));
  Assert.AreEqual(Integer(22), Integer(LBuffer[1]));
  Assert.AreEqual(Integer(65), Integer(LBuffer[2]));
  Assert.AreEqual(Integer(200), Integer(LBuffer[3]));
end;

procedure TTestRequest.TestReadPathInfo;
begin
  FRequest.Url := 'http://localhost:1234/rest/app/helloworld/postecho';
  Assert.AreEqual('/rest/app/helloworld/postecho', FRequest.PathInfo);
end;

procedure TTestRequest.TestWritePathInfo;
begin
  FRequest.Url := 'http://localhost:1234/rest/app/helloworld/postecho';
  FRequest.PathInfo := '/test';
  Assert.AreEqual('/test', FRequest.PathInfo);
end;

procedure TTestRequest.TestReadQuery;
begin
  FRequest.Url := 'http://localhost:1234/rest/app/helloworld/postecho?1234567890';
  Assert.AreEqual('1234567890', FRequest.Query);
end;

procedure TTestRequest.TestWriteQuery;
begin
  FRequest.Url := 'http://localhost:1234/rest/app/helloworld/postecho?1234567890';
  FRequest.Query := '0987654321';
  Assert.AreEqual('0987654321', FRequest.Query);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestRequest);

end.
