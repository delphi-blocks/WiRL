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
  WiRL.http.Filters,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.http.Server,
  WiRL.Core.Registry,
  WiRL.Core.Attributes,
  WiRL.Core.Exceptions,
  WiRL.Core.Application,
  WiRL.Engine.REST;

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
  LOrigin: string;
  LVary: string;
begin
  LConf := (ARequestContext.Context.Application as TWiRLApplication).GetConfiguration<TWiRLConfigurationCORS>;

  // Add CORS headers to all responses
  LOrigin := ARequestContext.Request.Headers['Origin'];
  // Echo back the request Origin (instead of '*') so the response stays valid
  // even when Allow-Credentials is enabled
  if (LOrigin <> '') and (LConf.AllowAllOrigins or LConf.IsOriginAllowed(LOrigin)) then
  begin
    ARequestContext.Response.Headers.Values['Access-Control-Allow-Origin'] := LOrigin;
    // Append (don't overwrite) 'Origin' to Vary so caches vary on it without losing existing values
    LVary := ARequestContext.Response.Headers.Values['Vary'];
    if Pos('origin', LowerCase(LVary)) = 0 then
    begin
      if LVary <> '' then
        LVary := LVary + ', ';
      ARequestContext.Response.Headers.Values['Vary'] := LVary + 'Origin';
    end;
  end;
  if LConf.ExposeHeaders <> '' then
    ARequestContext.Response.Headers.Values['Access-Control-Expose-Headers'] := LConf.ExposeHeaders;
  if LConf.Credentials then
    ARequestContext.Response.Headers.Values['Access-Control-Allow-Credentials'] := 'true';

  // Handle preflight OPTIONS requests
  if SameText(ARequestContext.Request.Method, 'OPTIONS') then
  begin
    if LConf.Methods <> '' then
      ARequestContext.Response.Headers.Values['Access-Control-Allow-Methods'] := LConf.Methods;
    if LConf.Headers <> '' then
      ARequestContext.Response.Headers.Values['Access-Control-Allow-Headers'] := LConf.Headers;
    if LConf.MaxAge > 0 then
      ARequestContext.Response.Headers.Values['Access-Control-Max-Age'] := IntToStr(LConf.MaxAge);
    ARequestContext.Abort();
  end;
end;

end.
