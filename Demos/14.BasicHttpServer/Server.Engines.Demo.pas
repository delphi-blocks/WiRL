{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Engines.Demo;

interface

uses
  System.SysUtils,

  WiRL.Engine.Core,
  WiRL.Engine.HTTP,
  WiRL.http.Server,
  WiRL.http.Accept.MediaType,
  WiRL.Core.Context,
  WiRL.Core.Context.Server;

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

{ TWiRLRESTEngine }

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
