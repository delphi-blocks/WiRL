{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2018 WiRL Team                                      }
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
  WiRL.http.Filters.Compression,

  Server.Filters.Attributes, Server.Forms.Main;

type
  [PreMatching]
  TRequestLoggerFilter = class(TInterfacedObject, IWiRLContainerRequestFilter)
  private
    FMainForm: TMainForm;
  public
    procedure Filter(ARequestContext: TWiRLContainerRequestContext);
    constructor Create(MainForm: TMainForm);
  end;

  [PreMatching]
  TResponseLoggerFilter = class(TInterfacedObject, IWiRLContainerResponseFilter)
  private
    FMainForm: TMainForm;
  public
    procedure Filter(ARequestContext: TWiRLContainerResponseContext);
    constructor Create(MainForm: TMainForm);
  end;

  [PreMatching]
  TAbortTest = class(TInterfacedObject, IWiRLContainerRequestFilter)
  public
    procedure Filter(ARequestContext: TWiRLContainerRequestContext);
  end;

  [Priority(TWiRLPriorities.USER)] // Default priority
  TRequestCheckerFilter = class(TInterfacedObject, IWiRLContainerRequestFilter)
  private
//    [Context] FAuth: TWiRLAuthContext;
    [Context] FApplication: TWiRLApplication;
  public
    procedure Filter(ARequestContext: TWiRLContainerRequestContext);
  end;

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
  LMessage := DateTimeToStr(Now) + ' - ' + ARequestContext.Request.Method + ' ' + ARequestContext.Request.PathInfo;
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
