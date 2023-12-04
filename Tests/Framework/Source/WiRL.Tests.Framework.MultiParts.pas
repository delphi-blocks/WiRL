{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Tests.Framework.MultiParts;

{$I WiRL.inc}

interface

uses
  System.Classes, System.SysUtils, System.StrUtils, System.JSON,
  System.NetEncoding,

  DUnitX.TestFramework,

  WiRL.Core.Classes,

  WiRL.http.Headers,
  WiRL.http.Accept.MediaType,
  WiRL.http.Request,
  WiRL.Tests.Mock.Server;

type
  [TestFixture]
  TTestMultiParts = class(TObject)
  private
    FRequest: TWiRLTestRequest;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    [TestCase('SimpleString', '123456789012345678901234567890')]
    [TestCase('CRLF', '12345678' + #13#10 + '311')]
    [TestCase('CR', '12345678' + #13 + '311')]
    [TestCase('LF', '12345678' + #10 + '311')]
    procedure TestFormDataBase(const Content: string);
    [Test]
    procedure TestFormDataCharset;
    [Test]
    procedure TestFormDataMultiEncoding;
    [Test]
    procedure TestFormDataBinary;
    [Test]
    procedure TestFormDataBase64;
    [Test]
    procedure TestFormDataHeader;
    [Test]
    procedure TestFormDataPreamble;
    [Test]
    procedure TestFormDataEpilogue;
    [Test]
    procedure TestNotFound;
  end;

implementation

const
  sCRLF = #13#10;

  MultiPartTest =
    '--1234' + sCRLF +
    'Content-Disposition: form-data; name="test1"; filename="test1.txt"' + sCRLF +
    'Content-Type: text/plain' + sCRLF +
    '' + sCRLF +
    '%s' + sCRLF +
    '--1234' + sCRLF +
    'Content-Disposition: form-data; name="test2"' + sCRLF +
    'Content-Type: text/plain; charset=utf-8' + sCRLF +
    'x-header: test' + sCRLF +
    '' + sCRLF +
    'ии' + sCRLF +
    '--1234' + sCRLF +
    'Content-Disposition: form-data; name="test3"' + sCRLF +
    'Content-Type: application/octet-stream' + sCRLF +
    '' + sCRLF +
    #$00#$01#$02#$03#$04#$05#$06#$07#$08#$09#$0a#$0b#$0c#$0d#$0e#$0f +
    #$10#$11#$12#$13#$14#$15#$16#$17#$18#$19#$1a#$1b#$1c#$1d#$1e#$1f + sCRLF +
    '--1234' + sCRLF +
    'Content-Disposition: form-data; name="test4"' + sCRLF +
    'Content-Type: application/octet-stream' + sCRLF +
    'Content-Transfer-Encoding: base64' + sCRLF +
    '' + sCRLF +
    'dGVzdA==' + sCRLF +
    '--1234--';

  MultiPartPrologueTest =
    'Preamble' + sCRLF +
    '--1234' + sCRLF +
    'Content-Disposition: form-data; name="test1"' + sCRLF +
    'Content-Type: text/plain' + sCRLF +
    '' + sCRLF +
    '%s' + sCRLF +
    '--1234' + sCRLF +
    'Content-Disposition: form-data; name="test2"' + sCRLF +
    'Content-Type: text/plain; charset=utf-8' + sCRLF +
    'x-header: test' + sCRLF +
    '' + sCRLF +
    'ии' + sCRLF +
    '--1234' + sCRLF +
    'Content-Disposition: form-data; name="test3"' + sCRLF +
    'Content-Type: application/octet-stream' + sCRLF +
    '' + sCRLF +
    #$00#$01#$02#$03#$04#$05#$06#$07#$08#$09#$0a#$0b#$0c#$0d#$0e#$0f +
    #$10#$11#$12#$13#$14#$15#$16#$17#$18#$19#$1a#$1b#$1c#$1d#$1e#$1f + sCRLF +
    '--1234' + sCRLF +
    'Content-Disposition: form-data; name="test4"' + sCRLF +
    'Content-Type: application/octet-stream' + sCRLF +
    'Content-Transfer-Encoding: base64' + sCRLF +
    '' + sCRLF +
    'dGVzdA==' + sCRLF +
    '--1234--' + sCRLF +
    'Epilogue';

  MultiPartEncTest =
    '--1234' + sCRLF +
    'Content-Disposition: form-data; name="test1"; filename="test1.txt"' + sCRLF +
    'Content-Type: text/plain; charset=utf-8' + sCRLF +
    '' + sCRLF +
    'и' + sCRLF +
    '--1234' + sCRLF +
    'Content-Disposition: form-data; name="test2"' + sCRLF +
    'Content-Type: text/plain; charset=iso-8859-1' + sCRLF +
    'x-header: test' + sCRLF +
    '' + sCRLF +
    '*' + sCRLF +
    '--1234--';



{ TTestRequest }

procedure TTestMultiParts.Setup;
begin
  FRequest := TWiRLTestRequest.Create;
end;

procedure TTestMultiParts.TearDown;
begin
  FRequest.Free;
end;

procedure TTestMultiParts.TestFormDataBase(const Content: string);
begin
  FRequest.ContentType := TMediaType.MULTIPART_FORM_DATA + '; boundary=1234';
  FRequest.ContentStream := TStringStream.Create(Format(MultiPartTest, [Content]), TEncoding.UTF8);
  Assert.AreEqual(Content, FRequest.MultiPartFormData['test1'].Content);
  Assert.AreEqual('text/plain', FRequest.MultiPartFormData['test1'].ContentType);
end;

procedure TTestMultiParts.TestFormDataCharset;
begin
  FRequest.ContentType := TMediaType.MULTIPART_FORM_DATA + '; boundary=1234';
  FRequest.ContentStream := TStringStream.Create(MultiPartTest, TEncoding.UTF8);
  Assert.AreEqual('ии', FRequest.MultiPartFormData['test2'].Content);
  Assert.AreEqual('test', FRequest.MultiPartFormData['test2'].Headers.Values['x-header']);
end;

procedure TTestMultiParts.TestFormDataEpilogue;
begin
  FRequest.ContentType := TMediaType.MULTIPART_FORM_DATA + '; boundary=1234';
  FRequest.ContentStream := TStringStream.Create(MultiPartPrologueTest, TEncoding.UTF8);
  Assert.AreEqual('Epilogue', FRequest.MultiPartFormData.Epilogue);
end;

procedure TTestMultiParts.TestFormDataHeader;
begin
  FRequest.ContentType := TMediaType.MULTIPART_FORM_DATA + '; boundary=1234';
  FRequest.ContentStream := TStringStream.Create(MultiPartTest, TEncoding.UTF8);
  Assert.AreEqual('text/plain', FRequest.MultiPartFormData['test1'].ContentMediaType.MediaType);
  Assert.AreEqual('text/plain', FRequest.MultiPartFormData['test2'].ContentMediaType.MediaType);
  Assert.AreEqual('test1', FRequest.MultiPartFormData['test1'].ContentDisposition.Name);
  Assert.AreEqual('test1.txt', FRequest.MultiPartFormData['test1'].ContentDisposition.FileName);
  Assert.AreEqual('form-data', FRequest.MultiPartFormData['test1'].ContentDisposition.Value);
  Assert.AreEqual('test1', FRequest.MultiPartFormData['test1'].Name);
  Assert.AreEqual('test1.txt', FRequest.MultiPartFormData['test1'].FileName);
  Assert.AreEqual('', FRequest.MultiPartFormData['test1'].Charset);
  Assert.AreEqual('utf-8', FRequest.MultiPartFormData['test2'].Charset);
end;

procedure TTestMultiParts.TestFormDataMultiEncoding;
var
  LSpecialCharPos: Integer;
  LBytes: TBytes;
begin
  LSpecialCharPos := Pos('*', MultiPartEncTest);
  LBytes := TEncoding.UTF8.GetBytes(MultiPartEncTest);
  LBytes[LSpecialCharPos] := Ord(AnsiChar('и'));
  FRequest.ContentType := TMediaType.MULTIPART_FORM_DATA + '; boundary=1234';
  FRequest.ContentStream := TBytesStream.Create(LBytes);
  Assert.AreEqual('и', FRequest.MultiPartFormData['test1'].Content);
  Assert.AreEqual('и', FRequest.MultiPartFormData['test2'].Content);
end;

procedure TTestMultiParts.TestFormDataPreamble;
begin
  FRequest.ContentType := TMediaType.MULTIPART_FORM_DATA + '; boundary=1234';
  FRequest.ContentStream := TStringStream.Create(MultiPartPrologueTest, TEncoding.UTF8);
  Assert.AreEqual('Preamble', FRequest.MultiPartFormData.Preamble);
end;

procedure TTestMultiParts.TestNotFound;
begin
  FRequest.ContentType := TMediaType.MULTIPART_FORM_DATA + '; boundary=1234';
  FRequest.ContentStream := TStringStream.Create(Format(MultiPartTest, ['abc']), TEncoding.UTF8);
  Assert.WillRaise(
    procedure ()
    begin
      FRequest.MultiPartFormData['unknown'].Content;
    end, EWiRLException);
end;

procedure TTestMultiParts.TestFormDataBinary;
var
  LBytes: TBytes;
  LDigit: Byte;
begin
  FRequest.ContentType := TMediaType.MULTIPART_FORM_DATA + '; boundary=1234';
  FRequest.ContentStream := TStringStream.Create(MultiPartTest, TEncoding.UTF8);
  LBytes := FRequest.MultiPartFormData['test3'].RawContent;
  Assert.AreEqual(32, Length(LBytes));
  for LDigit := Low(LBytes) to High(LBytes) do
    Assert.AreEqual(LDigit, LBytes[LDigit]);
end;

procedure TTestMultiParts.TestFormDataBase64;
begin
  FRequest.ContentType := TMediaType.MULTIPART_FORM_DATA + '; boundary=1234';
  FRequest.ContentStream := TStringStream.Create(MultiPartTest, TEncoding.UTF8);
  Assert.AreEqual('test', FRequest.MultiPartFormData['test4'].Content);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestMultiParts);

end.
