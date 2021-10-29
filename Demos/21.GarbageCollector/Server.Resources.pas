{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2021 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Resources;

interface

uses
  System.Classes, System.SysUtils, System.JSON, System.NetEncoding, System.Generics.Collections,
  Data.DB, FireDAC.Comp.Client, FireDAC.Comp.DataSet, FireDAC.Stan.Intf,

  WiRL.Core.Registry,
  WiRL.Core.Attributes,
  WiRL.Core.Application,
  WiRL.Core.GarbageCollector,
  WiRL.http.Accept.MediaType,
  WiRL.Core.MessageBody.Default,

  Server.Entities;

type
  [Path('/customer')]
  TCustomerResource = class
  private
    [Context] GC: TWiRLGarbageCollector;
  public
    [GET]
    [Produces(TMediaType.APPLICATION_JSON)]
    function GetCustomer: TCustomer;

    [PUT]
    [Produces(TMediaType.APPLICATION_JSON)]
    function UpdateCustomer(ACustomer: TCustomer): TCustomer;

    [GET, Path('/list1')]
    [Produces(TMediaType.APPLICATION_JSON)]
    function CustomerList1: TDataSet;

    [GET, Path('/list2')]
    [Produces(TMediaType.APPLICATION_JSON)]
    function CustomerList2: TDataSet;

    [GET, Path('/list3')]
    [Produces(TMediaType.APPLICATION_JSON)]
    function CustomerList3: TDataSet;
  end;


implementation

uses
  System.DateUtils, System.StrUtils, System.IOUtils,
  WiRL.http.Accept.Language, Server.Data.Main;

function TCustomerResource.CustomerList1: TDataSet;
begin
  Result := DataMain.GetCustomers1;
end;

function TCustomerResource.CustomerList2: TDataSet;
begin
  Result := DataMain.GetCustomers2(GC);
end;

function TCustomerResource.CustomerList3: TDataSet;
begin
  Result := DataMain.GetCustomers3(GC);
end;

function TCustomerResource.GetCustomer: TCustomer;
var
  LHelperObject1, LHelperObject2: TObject;
begin
  // If you have objects that are needed by the Result object to operate
  // (so you can't free them before the Result's object is serialized)
  // then you can "assign" them to the WiRL own garbage collector

  // You can add your own garbage to the WiRL's Garbage Collector
  LHelperObject1 := TObject.Create;
  GC.AddGarbage(LHelperObject1);

  // In this case TCustomer doesn't need any external object to operate
  Result := TCustomer.Create;
  Result.Name := 'Paolo';
  Result.Age := 50;

  // You can add your own garbage to the WiRL's Garbage Collector
  LHelperObject2 := TObject.Create;
  GC.AddGarbage(LHelperObject2);

end;

function TCustomerResource.UpdateCustomer(ACustomer: TCustomer): TCustomer;
var
  LOrder: TOrder;
begin
  // In this scenario you have an object that needs to be destroyed after the
  // Result is being serialized but, also, you have to call some specific
  // method on the object itself before the destruction

  LOrder := TOrder.Create;

  // Adds LOrder to the garbage collection and registers a method to deal with it
  GC.AddGarbage(LOrder, procedure
    begin
      LOrder.FinalizeObject;
      LOrder.Free;
    end
  );

  Result := TCustomer.Create;
  Result.Name := ACustomer.Name;
  Result.Age := ACustomer.Age;
 end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TCustomerResource>;

end.
