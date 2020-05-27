unit WiRL.Tests.Mock.Classes;

interface

uses
  System.SysUtils, System.Classes;

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

  EMyNotFoundException = class(Exception)
  private
    FErrorCode: Integer;
  public
    property ErrorCode: Integer read FErrorCode;
    constructor Create(AErrorCode: Integer; const Msg: string);
  end;

const
  TestPersonMediaType = 'application/vnd-wirl-person';

implementation

{ EMyNotFoundException }

constructor EMyNotFoundException.Create(AErrorCode: Integer; const Msg: string);
begin
  inherited Create(Msg);
  FErrorCode := AErrorCode;
end;

end.
