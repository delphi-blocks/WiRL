{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Tests.Framework.Core;

interface

uses
  DUnitX.TestFramework,

  WiRL.http.Accept.MediaType;

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

initialization
  TDUnitX.RegisterTestFixture(TTestMediaType);
end.
