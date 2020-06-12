{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
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

  TCounter = class
  private
    FValue: Integer;
  public
    property Value: Integer read FValue;
    procedure Reset;
    procedure Inc;
  end;

const
  TestPersonMediaType = 'application/vnd-wirl-person';

function GetGlobalCounter: TCounter;

implementation

var
  GlobalCounter: TCounter;

function GetGlobalCounter: TCounter;
begin
  if not Assigned(GlobalCounter) then
    GlobalCounter := TCounter.Create;
  Result := GlobalCounter;
end;

{ EMyNotFoundException }

constructor EMyNotFoundException.Create(AErrorCode: Integer; const Msg: string);
begin
  inherited Create(Msg);
  FErrorCode := AErrorCode;
end;

{ TCounter }

procedure TCounter.Inc;
begin
  FValue := FValue + 1;
end;

procedure TCounter.Reset;
begin
  FValue := 0;
end;

initialization
  GlobalCounter := nil;

finalization
  GlobalCounter.Free;

end.
