{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Tests.Framework.Serialization;

interface

uses
  System.SysUtils,
  DUnitX.TestFramework,

  WiRL.Core.JSON,
  Neon.Core.Persistence.JSON;

type
  TBasicObject = class
  private
    FName: string;
    FAge: Integer;
  public
    property Name: string read FName write FName;
    property Age: Integer read FAge write FAge;
  end;

  TCompositeObject = class
  private
    FBasic: TBasicObject;
    FValue: string;
  public
    property Basic: TBasicObject read FBasic write FBasic;
    property Value: string read FValue write FValue;
  end;

  [TestFixture]
  TTestSerialization = class(TObject)
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestBasicObjectSerialization;
    [Test]
    procedure TestBasicObjectDeserialization;
    [Test]
    procedure TestCompositeObjectSerialization;
  end;


implementation

{ TTestSerialization }

procedure TTestSerialization.Setup;
begin
end;

procedure TTestSerialization.TearDown;
begin
end;

procedure TTestSerialization.TestBasicObjectDeserialization;
var
  LBasicObject: TBasicObject;
begin
  LBasicObject := TNeon.JsonToObject<TBasicObject>('{"Name": "Luca", "Age": 42}');
  try
    Assert.AreEqual('Luca', LBasicObject.Name);
    Assert.AreEqual(42, LBasicObject.Age);
  finally
    LBasicObject.Free;
  end;
end;

procedure TTestSerialization.TestBasicObjectSerialization;
var
  LBasicObject: TBasicObject;
  LJson: TJSONValue;
begin
  LBasicObject := TBasicObject.Create;
  try
    LBasicObject.Name := 'Luca';
    LBasicObject.Age := 42;
    LJson := TNeon.ObjectToJSON(LBasicObject);
    try
      Assert.AreEqual('Luca', LJson.GetValue<string>('Name'));
      Assert.AreEqual(42, LJson.GetValue<Integer>('Age'));
    finally
      LJson.Free;
    end;
  finally
    LBasicObject.Free;
  end;
end;

procedure TTestSerialization.TestCompositeObjectSerialization;
var
  LCompositeObject: TCompositeObject;
  LJson, LJsonBasic: TJSONValue;
begin
  LCompositeObject := TCompositeObject.Create;
  try
    LCompositeObject.Basic := TBasicObject.Create;
    try
      LCompositeObject.Basic.Name := 'Luca';
      LCompositeObject.Basic.Age := 42;
      LJson := TNeon.ObjectToJSON(LCompositeObject);
      try
        LJsonBasic := LJson.GetValue<TJSONValue>('Basic');
        Assert.AreEqual('Luca', LJsonBasic.GetValue<string>('Name'));
        Assert.AreEqual(42, LJsonBasic.GetValue<Integer>('Age'));
      finally
        LJson.Free;
      end;
    finally
      LCompositeObject.Basic.Free;
    end;
  finally
    LCompositeObject.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestSerialization);

end.
