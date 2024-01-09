unit Client.Filters;

interface

uses
  System.Classes, System.SysUtils,
  WiRL.http.Accept.MediaType, Vcl.Forms,
  WiRL.Client.Filters;

type
  // This client response filter sends the response body to the main form
  TClientLogger = class(TInterfacedObject, IWiRLClientResponseFilter)
  public
    procedure Filter(AResponseContext: TWiRLClientResponseContext);
  end;

implementation

uses
  WiRL.Core.JSON,
  Client.Form.Main;

{ TClientLogger }

procedure TClientLogger.Filter(AResponseContext: TWiRLClientResponseContext);
begin
  // Probably is not a good idea to use a reference to frmClientMain here,
  // but... it's just a demo!
  frmClientMain.ShowResponse(TJSONHelper.PrettyPrint(AResponseContext.Response.ContentText));
end;

initialization
  TWiRLClientFilterRegistry.Instance.RegisterFilter<TClientLogger>;

end.
