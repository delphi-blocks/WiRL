{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2025 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Entities.Customer;

interface

uses
  System.SysUtils, System.Classes, System.Contnrs, System.Generics.Collections,
  System.Math, System.Math.Vectors, System.Types,

  Neon.Core.Persistence.JSON.Schema,
  Neon.Core.Types,
  Neon.Core.Attributes;

{$M+}

type
  TOrderItem = class
  private
    FDate: TDateTime;
    FId: Integer;
    FIdArticle: Integer;
    FQuantity: Double;
  public
    property Id: Integer read FId write FId;
    property IdArticle: Integer read FIdArticle write FIdArticle;
    property Date: TDateTime read FDate write FDate;
    property Quantity: Double read FQuantity write FQuantity;
  end;

  TOrderItems = class(TObjectList<TOrderItem>)
  end;

  [JsonSchema('title=Order')]
  TOrder = class
  private
    FId: Integer;
    FIdCustomer: Integer;
    FItems: TOrderItems;
    FTotal: Double;
  public
    constructor Create;
    destructor Destroy; override;
    function AddItem(AArticle: Integer; AQuantity: Double; ADate: TDateTime): TOrderItem;

    property Id: Integer read FId write FId;
    property IdCustomer: Integer read FIdCustomer write FIdCustomer;
    property Items: TOrderItems read FItems write FItems;
    property Total: Double read FTotal write FTotal;
  end;

  [JsonSchema('title=Orders')]
  TOrders = class(TObjectList<TOrder>)
  public
    function AddOrder(AID: Integer): TOrder;
  end;

  /// <summary>
  ///   Customer entiry
  /// </summary>
  [JsonSchema('title=Customer')]
  TCustomer = class
  private
    FCity: string;
    FCompanyName: string;
    FId: Integer;
    FOrders: TOrders;
  public
    constructor Create;
    destructor Destroy; override;
    function AddOrder: TOrder;

    [JsonSchema('description=Code for the customer,required')]
    property Id: Integer read FId write FId;
    property CompanyName: string read FCompanyName write FCompanyName;
    property City: string read FCity write FCity;
    [JsonSchema('description=List of orders')]
    property Orders: TOrders read FOrders write FOrders;
  end;

  [JsonSchema('title=Customers')]
  TCustomers = class(TObjectList<TCustomer>)
  public
    function AddCustomer(const ACompanyName: string): TCustomer;
  end;

implementation

{ TCustomer }

function TCustomer.AddOrder: TOrder;
begin
  Result := TOrder.Create;
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
  Result.Id := Random(10000);
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

{ TOrders }

function TOrders.AddOrder(AID: Integer): TOrder;
begin
  Result := TOrder.Create;
  Result.ID := Random(1000);
  Result.IDCustomer := AID;
  Self.Add(Result);
end;

end.

