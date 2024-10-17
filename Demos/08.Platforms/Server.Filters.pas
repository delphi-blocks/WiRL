{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Filters;

interface

uses
  System.SysUtils, System.Classes,

  WiRL.Core.Registry,
  WiRL.http.Filters,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.Core.Attributes,
  WiRL.Core.Exceptions,
  WiRL.Core.Auth.Context,
  WiRL.http.URL,
  WiRL.Core.Application,
  WiRL.http.Accept.MediaType;

type
  ILogger = interface
    ['{E7DDF5DE-937F-4248-A278-B97C84F2D957}']
    procedure Log(const AMessage: string);
  end;

  /// <summary>
  ///   Logger Filter (Demo). This filter will log the Request's params before WiRL
  ///   starts to process the http request
  /// </summary>
  [PreMatching]
  TRequestLoggerFilter = class(TInterfacedObject, IWiRLContainerRequestFilter)
  public
    procedure Filter(ARequestContext: TWiRLContainerRequestContext);
  end;

  /// <summary>
  ///   This filter will log the response after WiRL has finished processing the request
  ///   (it's a response filter)
  /// </summary>
  [PreMatching]
  TResponseLoggerFilter = class(TInterfacedObject, IWiRLContainerResponseFilter)
  public
    procedure Filter(ARequestContext: TWiRLContainerResponseContext);
  end;

procedure RegisterLogger(ALogger: ILogger);
procedure UnregisterLogger(ALogger: ILogger);

implementation

var
  Logger: ILogger = nil;


{ TRequestLoggerFilter }

procedure TRequestLoggerFilter.Filter(ARequestContext: TWiRLContainerRequestContext);
var
  LMessage: string;
begin
  if Assigned(Logger) then
  begin
    LMessage := DateTimeToStr(Now) + ' - ' + ARequestContext.Request.Method + ' ' + ARequestContext.Request.PathInfo;
    if ARequestContext.Request.Query <> '' then
      LMessage := LMessage + '?' + ARequestContext.Request.Query;
    Logger.Log('REQ - ' + LMessage);
  end;
end;

{ TResponseLoggerFilter }

procedure TResponseLoggerFilter.Filter(
  ARequestContext: TWiRLContainerResponseContext);
var
  LMessage: string;
begin
  if Assigned(Logger) then
  begin
    LMessage := DateTimeToStr(Now) + ' - ' + TEncoding.ANSI.GetString(ARequestContext.Response.RawContent);
    if ARequestContext.Request.Query <> '' then
      LMessage := LMessage + '?' + ARequestContext.Request.Query;
    Logger.Log('RES - ' + LMessage);
  end;
end;

procedure RegisterLogger(ALogger: ILogger);
begin
  Logger := ALogger;
end;

procedure UnregisterLogger(ALogger: ILogger);
begin
  Logger := nil;
end;

initialization

TWiRLFilterRegistry.Instance.RegisterFilter<TRequestLoggerFilter>();
TWiRLFilterRegistry.Instance.RegisterFilter<TResponseLoggerFilter>();

end.
