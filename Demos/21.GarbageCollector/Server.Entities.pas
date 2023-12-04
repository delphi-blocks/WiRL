{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2021 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Entities;

interface

uses
  System.SysUtils, System.Generics.Collections;

type
  /// <summary>
  ///   Simple Entity to show the automatic conversion to/from JSON
  /// </summary>
  TCustomer = class
  private
    FName: string;
    FAge: Integer;
    FDetail: string;
  public
    property Name: string read FName write FName;
    property Age: Integer read FAge write FAge;
    property Detail: string read FDetail write FDetail;
  end;

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
    procedure FinalizeObject;

    property ID: Integer read FID write FID;
  end;

implementation

{ TOrder }

procedure TOrder.FinalizeObject;
begin
  FID := 0;
end;

end.
