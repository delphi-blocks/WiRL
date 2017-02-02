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
    procedure Filter(Request: TWiRLRequest);
  end;

  TRequestFilterTest = class(TInterfacedObject, IWiRLContainerRequestFilter)
  public
    procedure Filter(Request: TWiRLRequest);
  end;

  TResponseFilterTest = class(TInterfacedObject, IWiRLContainerResponseFilter)
  public
    procedure Filter(Request: TWiRLRequest; Response: TWiRLResponse);
  end;

  [ResponseBindingTest]
  TResponseBindingFilterTest = class(TInterfacedObject, IWiRLContainerResponseFilter)
  public
    procedure Filter(Request: TWiRLRequest; Response: TWiRLResponse);
  end;

  [RequestBindingTest]
  TRequestBindingFilterTest = class(TInterfacedObject, IWiRLContainerRequestFilter)
  public
    procedure Filter(Request: TWiRLRequest);
  end;

implementation

{ TResponseFilterTest }

procedure TResponseFilterTest.Filter(Request: TWiRLRequest;
  Response: TWiRLResponse);
begin
  Response.HeaderFields['x-response-filter'] := 'true';
end;

{ TPreFilterTest }

procedure TPreFilterTest.Filter(Request: TWiRLRequest);
begin
  Request.HeaderFields['x-prematching-filter'] := 'true';
end;

{ TRequestFilterTest }

procedure TRequestFilterTest.Filter(Request: TWiRLRequest);
begin
  Request.HeaderFields['x-request-filter'] := 'true';
end;

{ TResponseBindingFilterTest }

procedure TResponseBindingFilterTest.Filter(Request: TWiRLRequest;
  Response: TWiRLResponse);
begin
  Response.HeaderFields['x-response-binded-filter'] := 'true';
end;

{ TRequestBindingFilterTest }

procedure TRequestBindingFilterTest.Filter(Request: TWiRLRequest);
begin
  Request.HeaderFields['x-request-binded-filter'] := 'true';
end;

initialization
  TWiRLFilterRegistry.Instance.RegisterFilter<TResponseFilterTest>;
  TWiRLFilterRegistry.Instance.RegisterFilter<TPreFilterTest>;
  TWiRLFilterRegistry.Instance.RegisterFilter<TRequestFilterTest>;
  TWiRLFilterRegistry.Instance.RegisterFilter<TResponseBindingFilterTest>;
  TWiRLFilterRegistry.Instance.RegisterFilter<TRequestBindingFilterTest>;

end.
