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
  WiRL.Core.Context,
  WiRL.Core.Injection,
  WiRL.Tests.Mock.Classes;

type
  TPersonFactory = class(TInterfacedObject, IContextFactory)
  public
    function CreateContext(const AObject: TRttiObject; AContext: TWiRLContext): TValue;
  end;

  TCounterFactory = class(TInterfacedObject, IContextFactory)
  public
    function CreateContext(const AObject: TRttiObject; AContext: TWiRLContext): TValue;
  end;

implementation

uses
  WiRL.http.Core;

{ TPersonFactory }

function TPersonFactory.CreateContext(const AObject: TRttiObject;
  AContext: TWiRLContext): TValue;
var
  LPerson: TTestPersonObject;
  LQueryFields: TWiRLParam;
begin
  LQueryFields := AContext.Request.QueryFields;

  LPerson := TTestPersonObject.Create;
  LPerson.Name := LQueryFields.AsType<string>('name');
  LPerson.Age := LQueryFields.AsType<Integer>('age');

  Result := LPerson;
end;

{ TCounterFactory }

function TCounterFactory.CreateContext(const AObject: TRttiObject;
  AContext: TWiRLContext): TValue;
begin
  Result := GetGlobalCounter;
end;

initialization
  TWiRLContextInjectionRegistry.Instance.RegisterFactory<TTestPersonObject>(TPersonFactory);
  TWiRLContextInjectionRegistry.Instance.RegisterFactory<TCounter>(TCounterFactory);

end.
