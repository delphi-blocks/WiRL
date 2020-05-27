unit WiRL.Tests.Mock.Classes;

interface

uses
  System.Classes;

type
  TTestPersonObject = class
  private
    FName: string;
    FAge: Integer;
  public
    property Name: string read FName write FName;
    property Age: Integer read FAge write FAge;
  end;

  TTestPersonRecord = record
    Name: string;
    Age: Integer;
  end;

const
  TestPersonMediaType = 'application/vnd-wirl-person';

implementation

end.
