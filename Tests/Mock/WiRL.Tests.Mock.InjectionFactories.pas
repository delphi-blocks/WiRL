{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Tests.Mock.InjectionFactories;

interface

uses
  System.SysUtils, System.Classes, System.Rtti,
  WiRL.Rtti.Utils,
  WiRL.http.Request,
  WiRL.Core.Context,
  WiRL.Core.Injection,
  WiRL.Tests.Mock.Classes;

type
  TPersonFactory = class(TInterfacedObject, IContextObjectFactory)
  public
    function CreateContextObject(const AObject: TRttiObject; AContext: TWiRLContextBase): TValue;
  end;

  TCounterFactory = class(TInterfacedObject, IContextObjectFactory)
  public
    function CreateContextObject(const AObject: TRttiObject; AContext: TWiRLContextBase): TValue;
  end;

implementation

uses
  WiRL.http.Core;

{ TPersonFactory }

function TPersonFactory.CreateContextObject(const AObject: TRttiObject;
  AContext: TWiRLContextBase): TValue;
var
  LPerson: TTestPersonObject;
  LQueryFields: TWiRLParam;
begin
  LQueryFields := AContext.GetContextDataAs<TWiRLRequest>.QueryFields;

  LPerson := TTestPersonObject.Create;
  LPerson.Name := LQueryFields.AsType<string>('name');
  LPerson.Age := LQueryFields.AsType<Integer>('age');

  Result := LPerson;
end;

{ TCounterFactory }

function TCounterFactory.CreateContextObject(const AObject: TRttiObject;
  AContext: TWiRLContextBase): TValue;
begin
  Result := GetGlobalCounter;
end;

initialization
  TWiRLContextInjectionRegistry.Instance.RegisterFactory<TTestPersonObject>(TPersonFactory);
  TWiRLContextInjectionRegistry.Instance.RegisterFactory<TCounter>(TCounterFactory);

end.
