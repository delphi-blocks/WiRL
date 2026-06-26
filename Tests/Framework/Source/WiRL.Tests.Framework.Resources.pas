{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Tests.Framework.Resources;

interface

uses
  System.Classes, System.SysUtils, System.StrUtils, System.JSON,
  System.NetEncoding,

  DUnitX.TestFramework,

  WiRL.http.Server,
  WiRL.Core.Context.Server,
  WiRL.Engine.REST,
  WiRL.http.Accept.MediaType,
  WiRL.Tests.Mock.Server;

type
  [TestFixture]
  TTestResource = class(TObject)
  private
    FServer: TWiRLServer;
    FContext: TWiRLContext;
    FRequest: TWiRLTestRequest;
    FResponse: TWiRLTestResponse;
    procedure WriteMultipartBody(const ABoundary, ABody: string);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestHelloWorld;
    [Test]
    procedure TestEcho;
    [Test]
    procedure TestReverse;
    [Test]
    procedure TestParams;
    [Test]
    [TestCase('Low', '1,2')]
    [TestCase('Big', '123456,45678')]
    procedure TestSum(AOne, ATwo: Integer);
    [Test]
    [TestCase('Low', '1,2')]
    [TestCase('Big', '123456,45678')]
    procedure TestQueryParam(AOne, ATwo: Integer);
    [Test]
    procedure TestPostEcho;
    [Test]
    procedure TestPostJSON;
    [Test]
    procedure TestBinary;
    [Test]
    procedure TestInvalidPostMediaType;
    [Test]
    procedure TestInvalidMethod;
    [Test]
    procedure PostIntForm;
    [Test]
    procedure PostStringForm;
    [Test]
    procedure PostBoolForm;
    [Test]
    procedure PostFloatForm;
    [Test]
    procedure PostMultipleParamsForm;
    [Test]
    procedure PostMultipartString;
    [Test]
    procedure PostMultipartInteger;
    [Test]
    procedure PostMultipartBool;
    [Test]
    procedure PostMultipartStream;
    [Test]
    procedure PostMultipartFile;
    [Test]
    procedure PostMultipartMixed;
  end;

implementation

const
  BinaryFile =
    'AAAAAUJ1ZDEAABAAAAAIAAAAEAAAAAQKAAAAAAAAaWRlYmFyXVNob3dTdGF0dXNCYXJbU2hvd1Bh' +
    'dGhiYXJbU2hvd1Rvb2xiYXJcU2lkZWJhcldpZHRoXxAYe3sxMTcsIDE3Nn0sIHs5ODEsIDU4NH19' +
    'CQkICRDACBUiLjxIVGF8fX5/gAAAAAAAAAEBAAAAAAAAAA0AAAAAAAAAAAAAAAAAAACCAAAAAQAu' +
    'bHN2cGJsb2IAAAKAYnBsaXN0MDDYAQIDBAUGBwgJCgsMSEkKS18QEnZpZXdPcHRpb25zVmVyc2lv' +
    'bl8QD3Nob3dJY29uUHJldmlld18QEWNhbGN1bGF0ZUFsbFNpemVzV2NvbHVtbnNYdGV4dFNpemVa' +
    'c29ydENvbHVtbl8QEHVzZVJlbGF0aXZlRGF0ZXNYaWNvblNpemUQAAkI2Q0ODxAREhMUFRYfJCku' +
    'Mzg9Qlhjb21tZW50c1RuYW1lW2RhdGVDcmVhdGVkVHNpemVVbGFiZWxUa2luZFd2ZXJzaW9uXmRh' +
    'dGVMYXN0T3BlbmVkXGRhdGVNb2RpZmllZNQXGBkaCxwKHld2aXNpYmxlVXdpZHRoWWFzY2VuZGlu' +
    'Z1VpbmRleAgRASwJEAfUIBgZGgoiCglXdmlzaWJsZQkRASwJ1BcYGRoLJgsoCBC1CBAC1BcYGRoK' +
    'KwstCRBhCBAD1BcYGRoLMAoyCBBkCRAF1BcYGRoKNQo3CRBzCRAE1BcYGRoLOgo8CBBLCRAG1BcY' +
    'GRoLPwtBCBDICBAI1BcYQxoKRQtHWWFzY2VuZGluZwkQtQgQASNAKAAAAAAAAFxkYXRlTW9kaWZp' +
    'ZWQJAKwAsQC9AMIAyADNANUA5ADxAAoAAAAAAAAAAAA=';


const
  MultipartCRLF = #13#10;

function MultipartTextPart(const ABoundary, AName, AValue: string): string;
begin
  Result :=
    '--' + ABoundary + MultipartCRLF +
    'Content-Disposition: form-data; name="' + AName + '"' + MultipartCRLF +
    MultipartCRLF +
    AValue + MultipartCRLF;
end;

function MultipartFilePart(const ABoundary, AName, AFileName, AContentType, AValue: string): string;
begin
  Result :=
    '--' + ABoundary + MultipartCRLF +
    'Content-Disposition: form-data; name="' + AName + '"; filename="' + AFileName + '"' + MultipartCRLF +
    'Content-Type: ' + AContentType + MultipartCRLF +
    MultipartCRLF +
    AValue + MultipartCRLF;
end;

function MultipartBinaryPart(const ABoundary, AName, AValue: string): string;
begin
  Result :=
    '--' + ABoundary + MultipartCRLF +
    'Content-Disposition: form-data; name="' + AName + '"' + MultipartCRLF +
    'Content-Type: ' + TMediaType.APPLICATION_OCTET_STREAM + MultipartCRLF +
    MultipartCRLF +
    AValue + MultipartCRLF;
end;

function MultipartEnd(const ABoundary: string): string;
begin
  Result := '--' + ABoundary + '--' + MultipartCRLF;
end;

{ TTestResource }

procedure TTestResource.WriteMultipartBody(const ABoundary, ABody: string);
var
  LBytes: TBytes;
begin
  FRequest.ContentType := TMediaType.MULTIPART_FORM_DATA + '; boundary=' + ABoundary;
  LBytes := TEncoding.ANSI.GetBytes(ABody);
  FRequest.ContentStream.WriteBuffer(LBytes[0], Length(LBytes));
  FRequest.ContentStream.Position := 0;
end;

procedure TTestResource.PostIntForm;
begin
  FRequest.Method := 'POST';
  FRequest.Url := 'http://localhost:1234/rest/app/helloworld/postintform';
  FRequest.ContentType := TMediaType.APPLICATION_FORM_URLENCODED_TYPE;
  FRequest.Content := 'id=42';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('42', FResponse.Content);
end;

procedure TTestResource.PostStringForm;
begin
  FRequest.Method := 'POST';
  FRequest.Url := 'http://localhost:1234/rest/app/helloworld/poststringform';
  FRequest.ContentType := TMediaType.APPLICATION_FORM_URLENCODED_TYPE;
  FRequest.Content := 'name=hello';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('hello', FResponse.Content);
end;

procedure TTestResource.PostBoolForm;
begin
  FRequest.Method := 'POST';
  FRequest.Url := 'http://localhost:1234/rest/app/helloworld/postboolform';
  FRequest.ContentType := TMediaType.APPLICATION_FORM_URLENCODED_TYPE;
  FRequest.Content := 'flag=true';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('True', FResponse.Content);
end;

procedure TTestResource.PostFloatForm;
begin
  FRequest.Method := 'POST';
  FRequest.Url := 'http://localhost:1234/rest/app/helloworld/postfloatform';
  FRequest.ContentType := TMediaType.APPLICATION_FORM_URLENCODED_TYPE;
  FRequest.Content := 'value=3.14';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('3.14', FResponse.Content);
end;

procedure TTestResource.PostMultipleParamsForm;
begin
  FRequest.Method := 'POST';
  FRequest.Url := 'http://localhost:1234/rest/app/helloworld/postmultipleparamsform';
  FRequest.ContentType := TMediaType.APPLICATION_FORM_URLENCODED_TYPE;
  FRequest.Content := 'name=hello&id=42';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('hello42', FResponse.Content);
end;

procedure TTestResource.Setup;
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

procedure TTestResource.TearDown;
begin
  FServer.Free;
  FContext.Free;
  FRequest.Free;
  FResponse.Free;
end;

procedure TTestResource.TestBinary;
var
  BinaryRequest, BinaryResponse: TBytes;
begin
  BinaryRequest := TNetEncoding.Base64.DecodeStringToBytes(BinaryFile);
  FRequest.Method := 'POST';
  FRequest.Url := 'http://localhost:1234/rest/app/helloworld/postbinary';
  FRequest.ContentType := TMediaType.APPLICATION_OCTET_STREAM;
  FRequest.ContentStream.Write(BinaryRequest[0], Length(BinaryRequest));
  FRequest.ContentStream.Position := 0;

  FServer.HandleRequest(FContext, FRequest, FResponse);

  SetLength(BinaryResponse, FResponse.ContentStream.Size);
  FResponse.ContentStream.Read(BinaryResponse[0], FResponse.ContentStream.Size);

  Assert.AreEqual<TBytes>(BinaryRequest, BinaryResponse);
end;

procedure TTestResource.TestEcho;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/helloworld/echostring/ciao';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('ciao', FResponse.Content);
end;

procedure TTestResource.TestHelloWorld;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/helloworld';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('Hello, world!', FResponse.Content);
end;

procedure TTestResource.TestInvalidMethod;
begin
  FRequest.Method := 'POST';
  FRequest.Url := 'http://localhost:1234/rest/app/helloworld';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual(404, FResponse.StatusCode);
end;

procedure TTestResource.TestInvalidPostMediaType;
begin
  FRequest.Method := 'POST';
  FRequest.Url := 'http://localhost:1234/rest/app/helloworld/postjson';
  FRequest.ContentType := TMediaType.TEXT_XML;
  FRequest.Content := '<xml><name>test</name></xml>';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual(404, FResponse.StatusCode);
end;

procedure TTestResource.TestParams;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/helloworld/params/ciao/luca';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('ciaoluca', FResponse.Content);
end;

procedure TTestResource.TestPostEcho;
begin
  FRequest.Method := 'POST';
  FRequest.Url := 'http://localhost:1234/rest/app/helloworld/postecho';
  FRequest.Content := 'ciao';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('ciao', FResponse.Content);
end;

procedure TTestResource.TestPostJSON;
begin
  FRequest.Method := 'POST';
  FRequest.Url := 'http://localhost:1234/rest/app/helloworld/postjson';
  FRequest.ContentType := TMediaType.APPLICATION_JSON;
  FRequest.Content := '{"name": "luca"}';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('luca', FResponse.Content);
end;

procedure TTestResource.TestQueryParam(AOne, ATwo: Integer);
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/helloworld/sumwithqueryparam?AOne=' + IntToStr(AOne) + '&ATwo=' + IntToStr(ATwo);
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual(IntToStr(AOne + ATwo), FResponse.Content);
end;

procedure TTestResource.TestReverse;
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/helloworld/reversestring/ciao';
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('oaic', FResponse.Content);
end;

procedure TTestResource.TestSum(AOne, ATwo: Integer);
begin
  FRequest.Method := 'GET';
  FRequest.Url := 'http://localhost:1234/rest/app/helloworld/sum/' + IntToStr(AOne) + '/' + IntToStr(ATwo);
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual(IntToStr(AOne + ATwo), FResponse.Content);
end;

procedure TTestResource.PostMultipartString;
const
  LBoundary = 'TestBnd';
var
  LBody: string;
begin
  FRequest.Method := 'POST';
  FRequest.Url := 'http://localhost:1234/rest/app/helloworld/postmultipartstring';
  LBody := MultipartTextPart(LBoundary, 'text', 'hello') + MultipartEnd(LBoundary);
  WriteMultipartBody(LBoundary, LBody);
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('hello', FResponse.Content);
end;

procedure TTestResource.PostMultipartInteger;
const
  LBoundary = 'TestBnd';
var
  LBody: string;
begin
  FRequest.Method := 'POST';
  FRequest.Url := 'http://localhost:1234/rest/app/helloworld/postmultipartinteger';
  LBody := MultipartTextPart(LBoundary, 'count', '42') + MultipartEnd(LBoundary);
  WriteMultipartBody(LBoundary, LBody);
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('42', FResponse.Content);
end;

procedure TTestResource.PostMultipartBool;
const
  LBoundary = 'TestBnd';
var
  LBody: string;
begin
  FRequest.Method := 'POST';
  FRequest.Url := 'http://localhost:1234/rest/app/helloworld/postmultipartbool';
  LBody := MultipartTextPart(LBoundary, 'active', 'true') + MultipartEnd(LBoundary);
  WriteMultipartBody(LBoundary, LBody);
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('True', FResponse.Content);
end;

procedure TTestResource.PostMultipartStream;
const
  LBoundary = 'TestBnd';
  LExpected = 'StreamData';
var
  LBody: string;
  LResponseBytes: TBytes;
begin
  FRequest.Method := 'POST';
  FRequest.Url := 'http://localhost:1234/rest/app/helloworld/postmultipartstream';
  LBody := MultipartBinaryPart(LBoundary, 'data', LExpected) + MultipartEnd(LBoundary);
  WriteMultipartBody(LBoundary, LBody);
  FServer.HandleRequest(FContext, FRequest, FResponse);
  SetLength(LResponseBytes, FResponse.ContentStream.Size);
  FResponse.ContentStream.Read(LResponseBytes[0], FResponse.ContentStream.Size);
  Assert.AreEqual<TBytes>(TEncoding.ANSI.GetBytes(LExpected), LResponseBytes);
end;

procedure TTestResource.PostMultipartFile;
const
  LBoundary = 'TestBnd';
var
  LBody: string;
begin
  FRequest.Method := 'POST';
  FRequest.Url := 'http://localhost:1234/rest/app/helloworld/postmultipartfile';
  LBody := MultipartFilePart(LBoundary, 'file', 'document.txt', TMediaType.TEXT_PLAIN, 'file content') +
    MultipartEnd(LBoundary);
  WriteMultipartBody(LBoundary, LBody);
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('document.txt', FResponse.Content);
end;

procedure TTestResource.PostMultipartMixed;
const
  LBoundary = 'TestBnd';
var
  LBody: string;
begin
  FRequest.Method := 'POST';
  FRequest.Url := 'http://localhost:1234/rest/app/helloworld/postmultipartmixed';
  LBody := MultipartTextPart(LBoundary, 'name', 'hello') +
    MultipartTextPart(LBoundary, 'qty', '7') +
    MultipartEnd(LBoundary);
  WriteMultipartBody(LBoundary, LBody);
  FServer.HandleRequest(FContext, FRequest, FResponse);
  Assert.AreEqual('hello7', FResponse.Content);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestResource);

end.
