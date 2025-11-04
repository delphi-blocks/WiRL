{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2021 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Demo.Entities;

interface

uses
  System.SysUtils, System.Generics.Collections;

type
  /// <summary>
  ///   Simple Entity to show the automatic conversion to/from JSON
  /// </summary>
  TPerson = class
  private
    FName: string;
    FAge: Integer;
    FDetail: string;
  public
    property Name: string read FName write FName;
    property Age: Integer read FAge write FAge;
    property Detail: string read FDetail write FDetail;
  end;

  TPersonList = class(TObjectList<TPerson>);

  TOrderProposal = class
  private
    FArticle: string;
    FDescription: string;
    FDueDate: TDateTime;
    FQuantity: Double;
  public
    property Article: string read FArticle write FArticle;
    property Description: string read FDescription write FDescription;
    property DueDate: TDateTime read FDueDate write FDueDate;
    property Quantity: Double read FQuantity write FQuantity;
  end;

  TOrder = class(TOrderProposal)
  private
    FID: Integer;
  public
    property ID: Integer read FID write FID;
  end;

  TMyEnum = (First, Second);

  TRecordParam = record
    Name: string;
    City: string;
    Age: Integer;
    Enum: TMyEnum;
  end;

  TArrayParam = TArray<TRecordParam>;
  TArrayInt = TArray<Int64>;

  TSimpleParam = class(TObject)
  private
    FValue: string;
    FPrefix: string;
  public
    property Value: string read FValue write FValue;
    property Prefix: string read FPrefix write FPrefix;

    function ToString: string; override;

    constructor Create(const AValue: string);
  end;

implementation

{ TSimpleParam }

constructor TSimpleParam.Create(const AValue: string);
var
  LValueList: TArray<string>;
begin
  inherited Create;
  LValueList := AValue.Split(['.']);
  if Length(LValueList) >= 2 then
  begin
    FPrefix := LValueList[0];
    FValue := LValueList[1];
  end
  else
    FValue := AValue;
end;

function TSimpleParam.ToString: string;
begin
  Result := Format('Prefix: [%s] - Value [%s]', [FPrefix, FValue]);
end;

end.
