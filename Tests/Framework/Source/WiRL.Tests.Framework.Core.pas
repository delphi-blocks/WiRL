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

initialization
  TDUnitX.RegisterTestFixture(TTestMediaType);
  TDUnitX.RegisterTestFixture(TTestEncoding);

end.
