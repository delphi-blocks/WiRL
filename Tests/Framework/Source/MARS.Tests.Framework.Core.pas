unit MARS.Tests.Framework.Core;

interface

uses
  DUnitX.TestFramework,

  MARS.Core.MediaType;

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
