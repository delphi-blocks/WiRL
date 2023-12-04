{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
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
  WiRL.http.Headers,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.http.Filters;

type
  [NameBinding]
  ResponseBindingTestAttribute = class(TCustomAttribute);

  [NameBinding]
  RequestBindingTestAttribute = class(TCustomAttribute);

  [NameBinding]
  Change401To400Attribute = class(TCustomAttribute);

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

  [Change401To400]
  TResponseChangeFilterTest = class(TInterfacedObject, IWiRLContainerResponseFilter)
  public
    procedure Filter(AResponseContext: TWiRLContainerResponseContext);
  end;

implementation

{ TResponseFilterTest }

procedure TResponseFilterTest.Filter(AResponseContext: TWiRLContainerResponseContext);
begin
  AResponseContext.Response.Headers.Values['x-response-filter'] := 'true';
end;

{ TPreFilterTest }

procedure TPreFilterTest.Filter(ARequestContext: TWiRLContainerRequestContext);
begin
  ARequestContext.Request.Headers.Values['x-prematching-filter'] := 'true';
end;

{ TRequestFilterTest }

procedure TRequestFilterTest.Filter(ARequestContext: TWiRLContainerRequestContext);
begin
  ARequestContext.Request.Headers.Values['x-request-filter'] := 'true';
end;

{ TResponseBindingFilterTest }

procedure TResponseBindingFilterTest.Filter(AResponseContext: TWiRLContainerResponseContext);
begin
  AResponseContext.Response.Headers.Values['x-response-binded-filter'] := 'true';
end;

{ TRequestBindingFilterTest }

procedure TRequestBindingFilterTest.Filter(ARequestContext: TWiRLContainerRequestContext);
begin
  ARequestContext.Request.Headers.Values['x-request-binded-filter'] := 'true';
end;

{ TResponseChangeFilterTest }

procedure TResponseChangeFilterTest.Filter(
  AResponseContext: TWiRLContainerResponseContext);
begin
  if AResponseContext.Response.StatusCode = 401 then
    AResponseContext.Response.StatusCode := 400;
end;

initialization
  TWiRLFilterRegistry.Instance.RegisterFilter<TResponseFilterTest>;
  TWiRLFilterRegistry.Instance.RegisterFilter<TPreFilterTest>;
  TWiRLFilterRegistry.Instance.RegisterFilter<TRequestFilterTest>;
  TWiRLFilterRegistry.Instance.RegisterFilter<TResponseBindingFilterTest>;
  TWiRLFilterRegistry.Instance.RegisterFilter<TRequestBindingFilterTest>;
  TWiRLFilterRegistry.Instance.RegisterFilter<TResponseChangeFilterTest>;

end.
