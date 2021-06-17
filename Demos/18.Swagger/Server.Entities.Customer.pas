{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2021 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Entities.Customer;

interface

uses
  System.SysUtils, System.Classes, System.Contnrs, System.Generics.Collections,
  System.Math, System.Math.Vectors, System.Types,

  Neon.Core.Types,
  Neon.Core.Attributes;

{$M+}

type
  TOrderItem = class
  private
    FDate: TDateTime;
    FID: Integer;
    FIDArticle: Integer;
    FQuantity: Double;
  public
    property ID: Integer read FID write FID;
    property IDArticle: Integer read FIDArticle write FIDArticle;
    property Date: TDateTime read FDate write FDate;
    property Quantity: Double read FQuantity write FQuantity;
  end;

  TOrderItems = class(TObjectList<TOrderItem>)
  end;

  TOrder = class
  private
    FID: Integer;
    FIDCustomer: Integer;
    FItems: TOrderItems;
    FTotal: Double;
  public
    constructor Create;
    destructor Destroy; override;
    function AddItem(AArticle: Integer; AQuantity: Double; ADate: TDateTime): TOrderItem;

    property ID: Integer read FID write FID;
    property IDCustomer: Integer read FIDCustomer write FIDCustomer;
    property Items: TOrderItems read FItems write FItems;
    property Total: Double read FTotal write FTotal;
  end;

  TOrders = class(TObjectList<TOrder>)
  end;

  /// <summary>
  ///   Customer class
  /// </summary>
  TCustomer = class
  private
    FCompanyName: string;
    FID: Integer;
    FOrders: TOrders;
  public
    constructor Create;
    destructor Destroy; override;
    function AddOrder: TOrder;

    property ID: Integer read FID write FID;
    property CompanyName: string read FCompanyName write FCompanyName;
    property Orders: TOrders read FOrders write FOrders;
  end;

  TCustomers = class(TObjectList<TCustomer>)
  public
    function AddCustomer(const ACompanyName: string): TCustomer;
  end;

implementation

{ TCustomer }

function TCustomer.AddOrder: TOrder;
begin
  Result := TOrder.Create;
  Result.ID := Random(1000);
  Result.IDCustomer := FID;
  FOrders.Add(Result);
end;

constructor TCustomer.Create;
begin
  FOrders := TOrders.Create(True);
end;

destructor TCustomer.Destroy;
begin
  FOrders.Free;
  inherited;
end;

{ TOrder }

function TOrder.AddItem(AArticle: Integer; AQuantity: Double; ADate: TDateTime): TOrderItem;
begin
  Result := TOrderItem.Create;
  Result.ID := Random(10000);
  Result.IDArticle := AArticle;
  Result.Quantity := AQuantity;
  Result.Date := ADate;
  FItems.Add(Result);
end;

constructor TOrder.Create;
begin
  FItems := TOrderItems.Create(True);
end;

destructor TOrder.Destroy;
begin
  FItems.Free;
  inherited;
end;

{ TCustomers }

function TCustomers.AddCustomer(const ACompanyName: string): TCustomer;
begin
  Result := TCustomer.Create;
  Result.ID := Random(1000);
  Result.CompanyName := ACompanyName;
  Self.Add(Result);
end;

end.
