{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Resources;

interface

uses
  System.SysUtils, System.Classes,

  WiRL.Core.JSON,
  WiRL.Core.Attributes,
  WiRL.http.Accept.MediaType;

type
  [Path('demo')]
  TFilterDemoResource = class
  private
  protected
  public
    [GET]
    [Produces(TMediaType.TEXT_PLAIN)]
    function WelcomeMessage: string;

    [GET, Path('/echostring/{AString}')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function EchoString([PathParam] AString: string): string;

    [GET, Path('/raise/')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function RaiseTest: string;

  end;

implementation

uses
  WiRL.Core.Registry;

{ TFilterDemoResource }

function TFilterDemoResource.EchoString(AString: string): string;
begin
  Result := AString;
end;

function TFilterDemoResource.WelcomeMessage: string;
begin
  Result := 'Hi! Current time is ' + DateTimeToStr(Now);
end;

function TFilterDemoResource.RaiseTest: string;
begin
  raise Exception.Create('Test error!');
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TFilterDemoResource>;

end.
