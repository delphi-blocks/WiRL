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
  WinApi.Windows, System.SysUtils, System.Classes,

  WiRL.Core.Registry,
  WiRL.http.Filters,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.Core.Attributes,
  WiRL.Core.Exceptions,
  WiRL.Core.Auth.Context,
  WiRL.http.URL,
  WiRL.Core.Application,
  WiRL.http.Accept.MediaType,

  Server.Filters.Attributes,
  Server.Forms.Main;

type
  /// <summary>
  ///   Logger Filter (Demo). This filter will log the Request's params before WiRL
  ///   starts to process the http request
  /// </summary>
  [PreMatching]
  TRequestLoggerFilter = class(TInterfacedObject, IWiRLContainerRequestFilter)
  private
    FMainForm: TMainForm;
  public
    procedure Filter(ARequestContext: TWiRLContainerRequestContext);
    constructor Create(MainForm: TMainForm);
  end;

  /// <summary>
  ///   This filter will log the response after WiRL has finished processing the request
  ///   (it's a response filter)
  /// </summary>
  TResponseLoggerFilter = class(TInterfacedObject, IWiRLContainerResponseFilter)
  private
    FMainForm: TMainForm;
  public
    procedure Filter(ARequestContext: TWiRLContainerResponseContext);
    constructor Create(MainForm: TMainForm);
  end;

  /// <summary>
  ///   This filter shows how to abort a request (WiRL will skip entirely the process)
  /// </summary>
  [PreMatching]
  TAbortTest = class(TInterfacedObject, IWiRLContainerRequestFilter)
  public
    procedure Filter(ARequestContext: TWiRLContainerRequestContext);
  end;

  /// <summary>
  ///   This filter shows how to change the priority
  /// </summary>
  [Priority(TWiRLPriorities.USER)] // Default priority
  TRequestCheckerFilter = class(TInterfacedObject, IWiRLContainerRequestFilter)
  private
    [Context] FApplication: TWiRLApplication;
  public
    procedure Filter(ARequestContext: TWiRLContainerRequestContext);
  end;

  /// <summary>
  ///   This class shows how to create a custom filter that you can apply to a single
  ///   resource
  /// </summary>
  [PoweredByWiRL]
  TResponsePoweredByFilter = class(TInterfacedObject, IWiRLContainerResponseFilter)
  public
    procedure Filter(AResponseContext: TWiRLContainerResponseContext);
  end;


implementation

{ TRequestLoggerFilter }

procedure TRequestCheckerFilter.Filter(ARequestContext: TWiRLContainerRequestContext);
begin
  if Pos('error', ARequestContext.Request.Query) > 0 then
    raise EWiRLWebApplicationException.Create(Format('Filter error test [%s]', [FApplication.AppName]), 400);
end;

{ TRequestLoggerFilter }

constructor TRequestLoggerFilter.Create(MainForm: TMainForm);
begin
  FMainForm := MainForm;
end;

procedure TRequestLoggerFilter.Filter(ARequestContext: TWiRLContainerRequestContext);
var
  LMessage: string;
begin
  LMessage := DateTimeToStr(Now) + ' - ' + ARequestContext.Request.Method + ' ' + ARequestContext.Request.PathInfo;
  if ARequestContext.Request.Query <> '' then
    LMessage := LMessage + '?' + ARequestContext.Request.Query;
  FMainForm.Log('REQ - ' + LMessage);
end;

{ TResponsePoweredByFilter }

procedure TResponsePoweredByFilter.Filter(AResponseContext: TWiRLContainerResponseContext);
begin
  if AResponseContext.Response.StatusCode >= 500 then
    AResponseContext.Response.HeaderFields['X-Powered-By'] := 'DataSnap' // ;-)
  else
    AResponseContext.Response.HeaderFields['X-Powered-By'] := 'WiRL';
end;

{ TAbortTest }

procedure TAbortTest.Filter(ARequestContext: TWiRLContainerRequestContext);
begin
  if not ARequestContext.Request.PathInfo.StartsWith('/rest') then
  begin
    ARequestContext.Response.ContentType := TMediaType.TEXT_PLAIN;
    ARequestContext.Response.Content := Format('[%s] is not a valid API URL',
      [ARequestContext.Request.PathInfo]);
    ARequestContext.Response.StatusCode := 200;
    ARequestContext.Abort;
  end;

end;

{ TResponseLoggerFilter }

constructor TResponseLoggerFilter.Create(MainForm: TMainForm);
begin
  FMainForm := MainForm;
end;

procedure TResponseLoggerFilter.Filter(
  ARequestContext: TWiRLContainerResponseContext);
var
  LMessage: string;
begin
  LMessage := DateTimeToStr(Now) + ' - ' + TEncoding.ANSI.GetString(ARequestContext.Response.RawContent);
  if ARequestContext.Request.Query <> '' then
    LMessage := LMessage + '?' + ARequestContext.Request.Query;
  FMainForm.Log('RES - ' + LMessage);
end;

initialization
  TWiRLFilterRegistry.Instance.RegisterFilter<TRequestLoggerFilter>(
    function (): TObject
    begin
      Result := TRequestLoggerFilter.Create(MainForm);
    end
  );
  TWiRLFilterRegistry.Instance.RegisterFilter<TResponseLoggerFilter>(
    function (): TObject
    begin
      Result := TResponseLoggerFilter.Create(MainForm);
    end
  );
  TWiRLFilterRegistry.Instance.RegisterFilter<TRequestCheckerFilter>;
  TWiRLFilterRegistry.Instance.RegisterFilter<TResponsePoweredByFilter>;
  TWiRLFilterRegistry.Instance.RegisterFilter<TAbortTest>;

end.
