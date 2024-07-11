unit WiRL.Tests.Framework.HeaderParser;

interface

uses
  System.Classes, System.SysUtils, System.StrUtils, System.JSON,
  System.NetEncoding,

  DUnitX.TestFramework,

  WiRL.http.Server,
  WiRL.Engine.REST,
  WiRL.http.Accept.MediaType,
  WiRL.http.Accept.Parser,
  WiRL.http.Accept.Charset,
  WiRL.Tests.Mock.Server;

type
  TMyHeader = class(THeaderItem)

  end;

  [TestFixture]
  TTestHeaderParser = class(TObject)
  public
    [Test]
    procedure TestBasicParsing;
    [Test]
    procedure TestAcceptCharsetHeader;
  end;

implementation


{ TTestHeaderParser }

procedure TTestHeaderParser.TestAcceptCharsetHeader;
var
  AcceptCharset: TAcceptCharset;
begin
  AcceptCharset := TAcceptCharset.Create('Test; q=1; p=2');
  try
    Assert.AreEqual('Test', AcceptCharset.AcceptItemOnly);
    Assert.AreEqual<Double>(1.0, AcceptCharset.QFactor);
  finally
    AcceptCharset.Free;
  end;
end;

procedure TTestHeaderParser.TestBasicParsing;
var
  MyHeader: TMyHeader;
begin
  MyHeader := TMyHeader.Create('Test; key=value');
  try
    Assert.AreEqual('Test', MyHeader.Value);
    Assert.AreEqual(1, MyHeader.Parameters.Count);
  finally
    MyHeader.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestHeaderParser);

end.
