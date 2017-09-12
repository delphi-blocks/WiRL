unit Server.Engines.Demo;

interface

uses
  System.SysUtils,

  WiRL.http.Server,
  WiRL.http.Engines,
  WiRL.http.Accept.MediaType,
  WiRL.Core.Context;

type
  TWiRLDemoEngine = class(TWiRLCustomEngine)
  public
    procedure HandleRequest(AContext: TWiRLContext); override;
  end;

  TWiRLDefaultEngine = class(TWiRLCustomEngine)
  public
    procedure HandleRequest(AContext: TWiRLContext); override;
  end;

implementation

{ TWiRLEngine }

procedure TWiRLDemoEngine.HandleRequest(AContext: TWiRLContext);
begin
  inherited;
  AContext.Response.ContentType := TMediaType.TEXT_PLAIN;
  AContext.Response.Content := ClassName + ': ' + DateTimeToStr(Now);
end;

{ TWiRLDefaultEngine }

procedure TWiRLDefaultEngine.HandleRequest(AContext: TWiRLContext);
begin
  inherited;
  AContext.Response.ContentType := TMediaType.TEXT_PLAIN;
  AContext.Response.Content := ClassName + ': ' + DateTimeToStr(Now);
end;

end.
