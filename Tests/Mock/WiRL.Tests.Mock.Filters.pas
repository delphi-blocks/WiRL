{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Tests.Mock.Filters;

interface

uses
  System.SysUtils, System.Classes,
  WiRL.Core.Application,
  WiRL.Core.Attributes,
  WiRL.Core.Request,
  WiRL.Core.Response,
  WiRL.http.Filters;

type
  [NameBinding]
  ResponseBindingTestAttribute = class(TCustomAttribute);

  [NameBinding]
  RequestBindingTestAttribute = class(TCustomAttribute);

  [PreMatching]
  TPreFilterTest = class(TInterfacedObject, IWiRLContainerRequestFilter)
  public
    procedure Filter(ARequestContext: TWiRLContainerRequestContext);
  end;

  TRequestFilterTest = class(TInterfacedObject, IWiRLContainerRequestFilter)
  public
    procedure Filter(ARequestContext: TWiRLContainerRequestContext);
  end;

  TResponseFilterTest = class(TInterfacedObject, IWiRLContainerResponseFilter)
  public
    procedure Filter(AResponseContext: TWiRLContainerResponseContext);
  end;

  [ResponseBindingTest]
  TResponseBindingFilterTest = class(TInterfacedObject, IWiRLContainerResponseFilter)
  public
    procedure Filter(AResponseContext: TWiRLContainerResponseContext);
  end;

  [RequestBindingTest]
  TRequestBindingFilterTest = class(TInterfacedObject, IWiRLContainerRequestFilter)
  public
    procedure Filter(ARequestContext: TWiRLContainerRequestContext);
  end;

implementation

{ TResponseFilterTest }

procedure TResponseFilterTest.Filter(AResponseContext: TWiRLContainerResponseContext);
begin
  AResponseContext.Response.HeaderFields['x-response-filter'] := 'true';
end;

{ TPreFilterTest }

procedure TPreFilterTest.Filter(ARequestContext: TWiRLContainerRequestContext);
begin
  ARequestContext.Request.HeaderFields['x-prematching-filter'] := 'true';
end;

{ TRequestFilterTest }

procedure TRequestFilterTest.Filter(ARequestContext: TWiRLContainerRequestContext);
begin
  ARequestContext.Request.HeaderFields['x-request-filter'] := 'true';
end;

{ TResponseBindingFilterTest }

procedure TResponseBindingFilterTest.Filter(AResponseContext: TWiRLContainerResponseContext);
begin
  AResponseContext.Response.HeaderFields['x-response-binded-filter'] := 'true';
end;

{ TRequestBindingFilterTest }

procedure TRequestBindingFilterTest.Filter(ARequestContext: TWiRLContainerRequestContext);
begin
  ARequestContext.Request.HeaderFields['x-request-binded-filter'] := 'true';
end;

initialization
  TWiRLFilterRegistry.Instance.RegisterFilter<TResponseFilterTest>;
  TWiRLFilterRegistry.Instance.RegisterFilter<TPreFilterTest>;
  TWiRLFilterRegistry.Instance.RegisterFilter<TRequestFilterTest>;
  TWiRLFilterRegistry.Instance.RegisterFilter<TResponseBindingFilterTest>;
  TWiRLFilterRegistry.Instance.RegisterFilter<TRequestBindingFilterTest>;

end.
