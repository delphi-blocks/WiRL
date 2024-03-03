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
  public
    property Value: string read FValue write FValue;

    constructor Create(const AValue: string);
  end;

implementation

{ TSimpleParam }

constructor TSimpleParam.Create(const AValue: string);
begin
  inherited Create;
  FValue := AValue;
end;

end.
