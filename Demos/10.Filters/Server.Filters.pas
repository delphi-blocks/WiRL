unit Server.Filters;

interface

uses
  WinApi.Windows, System.SysUtils, System.Classes,

  MARS.Core.Registry,
  MARS.http.Filters,
  MARS.Core.Request,
  MARS.Core.Response,
  MARS.Core.Attributes,
  MARS.Core.Exceptions,
  MARS.Core.URL,
  MARS.Core.Application,

  Server.Filters.Attributes, Server.Forms.Main;

type
  [PreMatching]
  TRequestLoggerFilter = class(TInterfacedObject, IMARSContainerRequestFilter)
  private
    FMainForm :TMainForm;
  public
    procedure Filter(Request: TMARSRequest);
    constructor Create(MainForm :TMainForm);
  end;

  [Priority(TMARSPriorities.USER)] // Default priority
  TRequestCheckerFilter = class(TInterfacedObject, IMARSContainerRequestFilter)
  private
    [Context] FApplication: TMARSApplication;
  public
    procedure Filter(Request: TMARSRequest);
  end;

  [PoweredByMARS]
  TResponsePoweredByFilter = class(TInterfacedObject, IMARSContainerResponseFilter)
  public
    procedure Filter(Request :TMARSRequest; Response: TMARSResponse);
  end;


implementation

{ TRequestLoggerFilter }

procedure TRequestCheckerFilter.Filter(Request: TMARSRequest);
begin
  if Pos('error', Request.Query) > 0 then
    raise EMARSWebApplicationException.Create(Format('Filter error test [%s]', [FApplication.Name]), 400);
end;

{ TRequestLoggerFilter }

constructor TRequestLoggerFilter.Create(MainForm: TMainForm);
begin
  FMainForm := MainForm;
end;

procedure TRequestLoggerFilter.Filter(Request: TMARSRequest);
var
  LMessage: string;
begin
  LMessage := DateTimeToStr(Now) + ' - ' + Request.Method + ' ' + Request.RawPathInfo;
  if Request.Query <> '' then
    LMessage := LMessage + '?' + Request.Query;
  FMainForm.Log(LMessage);
end;

{ TResponsePoweredByFilter }

procedure TResponsePoweredByFilter.Filter(Request: TMARSRequest; Response: TMARSResponse);
begin
  Response.SetCustomHeader('X-Powered-By', 'MARS');
end;

initialization
  TMARSFilterRegistry.Instance.RegisterFilter<TRequestLoggerFilter>(function () :TObject begin
    Result := TRequestLoggerFilter.Create(MainForm);
  end);
  TMARSFilterRegistry.Instance.RegisterFilter<TRequestCheckerFilter>;
  TMARSFilterRegistry.Instance.RegisterFilter<TResponsePoweredByFilter>;

end.
