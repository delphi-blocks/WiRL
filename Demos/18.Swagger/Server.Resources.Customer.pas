{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2021 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Resources.Customer;

interface

uses
  System.Classes, System.SysUtils, System.JSON,

  WiRL.Core.OpenAPI.Resource,
  WiRL.Core.Engine,
  WiRL.Core.Application,
  WiRL.Core.Registry,
  WiRL.Core.Attributes,
  WiRL.Core.Application.Worker,
  WiRL.Core.MessageBody.Default,
  WiRL.Core.Auth.Context,
  WiRL.Core.Auth.Resource,
  WiRL.http.Accept.MediaType,
  WiRL.http.URL,
  WiRL.http.Request,
  WiRL.http.Response,

  Server.Entities.Customer;

type
  [Path('customer')]
  TCustomerResource = class
  public
    [GET][Path('{id}')][Produces(TMediaType.APPLICATION_JSON)]
    function GetOrder([PathParam('id')] ACustomerID: Integer): TCustomer;

    [GET][Path('/list')][Produces(TMediaType.APPLICATION_JSON)]
    function GetCustomers(): TCustomers;
  end;

  [Path('order')]
  TOrderResource = class
  public
    [GET][Path('{id}')][Produces(TMediaType.APPLICATION_JSON)]
    function GetOrder([PathParam('id')] AOrderID: Integer): TOrders;

    [GET][Path('/cust/{custid}')][Produces(TMediaType.APPLICATION_JSON)]
    function GetOrders([PathParam('custid')] ACustomerID: Integer): TOrders;
  end;


implementation

{ TOrderResource }

function TOrderResource.GetOrder(AOrderID: Integer): TOrders;
begin

end;

function TOrderResource.GetOrders(ACustomerID: Integer): TOrders;
var
  LOrder: TOrder;
begin
  Result := TOrders.Create(True);
  LOrder := TOrder.Create;
  LOrder.ID := Random(1000);
  LOrder.IDCustomer := ACustomerID;
  LOrder.AddItem(33, 21.4, Now);
  LOrder.AddItem(77, 112, Now);
  Result.Add(LOrder);
end;

{ TCustomerResource }

function TCustomerResource.GetCustomers: TCustomers;
begin
  Result := TCustomers.Create(True);
  //Result.AddCustomer('Wintech Italia').AddOrder(
end;

function TCustomerResource.GetOrder(ACustomerID: Integer): TCustomer;
begin

end;

initialization
  Randomize;

  //TWiRLResourceRegistry.Instance.RegisterResource<TCustomerResource>;
  //TWiRLResourceRegistry.Instance.RegisterResource<TOrderResource>;

end.
