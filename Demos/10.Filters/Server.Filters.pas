unit Server.Filters;

interface

uses
  WinApi.Windows, System.SysUtils, System.Classes,

  WiRL.Core.Registry,
  WiRL.http.Filters,
  WiRL.Core.Request,
  WiRL.Core.Response,
  WiRL.Core.Attributes,
  WiRL.Core.Exceptions,
  WiRL.Core.URL,
  WiRL.Core.Application,

  Server.Filters.Attributes, Server.Forms.Main;

type
  [PreMatching]
  TRequestLoggerFilter = class(TInterfacedObject, IWiRLContainerRequestFilter)
  private
    FMainForm :TMainForm;
  public
    procedure Filter(Request: TWiRLRequest);
    constructor Create(MainForm :TMainForm);
  end;

  [Priority(TWiRLPriorities.USER)] // Default priority
  TRequestCheckerFilter = class(TInterfacedObject, IWiRLContainerRequestFilter)
  private
    [Context] FApplication: TWiRLApplication;
  public
    procedure Filter(Request: TWiRLRequest);
  end;

  [PoweredByWiRL]
  TResponsePoweredByFilter = class(TInterfacedObject, IWiRLContainerResponseFilter)
  public
    procedure Filter(Request :TWiRLRequest; Response: TWiRLResponse);
  end;


implementation

{ TRequestLoggerFilter }

procedure TRequestCheckerFilter.Filter(Request: TWiRLRequest);
begin
  if Pos('error', Request.Query) > 0 then
    raise EWiRLWebApplicationException.Create(Format('Filter error test [%s]', [FApplication.Name]), 400);
end;

{ TRequestLoggerFilter }

constructor TRequestLoggerFilter.Create(MainForm: TMainForm);
begin
  FMainForm := MainForm;
end;

procedure TRequestLoggerFilter.Filter(Request: TWiRLRequest);
var
  LMessage: string;
begin
  LMessage := DateTimeToStr(Now) + ' - ' + Request.Method + ' ' + Request.RawPathInfo;
  if Request.Query <> '' then
    LMessage := LMessage + '?' + Request.Query;
  FMainForm.Log(LMessage);
end;

{ TResponsePoweredByFilter }

procedure TResponsePoweredByFilter.Filter(Request: TWiRLRequest; Response: TWiRLResponse);
begin
  Response.SetCustomHeader('X-Powered-By', 'WiRL');
end;

initialization
  TWiRLFilterRegistry.Instance.RegisterFilter<TRequestLoggerFilter>(function () :TObject begin
    Result := TRequestLoggerFilter.Create(MainForm);
  end);
  TWiRLFilterRegistry.Instance.RegisterFilter<TRequestCheckerFilter>;
  TWiRLFilterRegistry.Instance.RegisterFilter<TResponsePoweredByFilter>;

end.
