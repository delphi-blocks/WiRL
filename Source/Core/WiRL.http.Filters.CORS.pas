{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.Filters.CORS;

interface

uses
  System.SysUtils, System.Classes,

  WiRL.Configuration.CORS,
  WiRL.Core.Registry,
  WiRL.http.Filters,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.Core.Attributes,
  WiRL.Core.Exceptions,
  WiRL.http.Server,
  WiRL.Core.Engine,
  WiRL.Core.Application;

type
  [NameBinding]
  CORSAttribute = class(TCustomAttribute);

  /// <summary>
  ///   CORS (PreMatchingResource) Filter. It adds Headers to the response based on values found in
  ///   the CORS configuration object
  /// </summary>
  /// <remarks>
  ///   It doesn't need to be registerd, it's loaded by the CORS configuration object
  /// </remarks>
  [PreMatchingResource][CORS]
  TCORSFilter = class(TInterfacedObject, IWiRLContainerRequestFilter)
  private
  public
    procedure Filter(ARequestContext: TWiRLContainerRequestContext);
  end;

implementation

uses
  System.IOUtils;

{ TCORSFilter }

procedure TCORSFilter.Filter(ARequestContext: TWiRLContainerRequestContext);
var
  LConf: TWiRLConfigurationCORS;
begin
  LConf := (ARequestContext.Context.Application as TWiRLApplication).GetConfiguration<TWiRLConfigurationCORS>;

  // Insert the Access-Control-Allow-Origin headers in every Response
  ARequestContext.Response.Headers.Values['Access-Control-Allow-Origin'] := LConf.Origin;

  if SameText(ARequestContext.Request.Method, 'OPTIONS') then
  begin
    ARequestContext.Response.Headers.Values['Access-Control-Allow-Methods'] := LConf.Methods;
    ARequestContext.Response.Headers.Values['Access-Control-Allow-Headers'] := LConf.Headers;
    ARequestContext.Abort();
  end;
end;

end.
