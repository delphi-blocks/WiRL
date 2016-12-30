{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Resources;

interface

uses
  System.SysUtils, System.Classes,

  WiRL.Core.Attributes,
  WiRL.http.Accept.MediaType,
  WiRL.Core.JSON,

  Server.Filters.Attributes;

type
  [Path('filterdemo')]
  [PoweredByWiRL]
  [ContentEncoding]
  TFilterDemoResource = class
  private
  protected
  public
    [GET]
    [Produces(TMediaType.TEXT_PLAIN)]
    function SampleText: string;

    [GET, Path('/echostring/{AString}')]
    function EchoString([PathParam] AString: string): string;

    [GET, Path('/raise/')]
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

function TFilterDemoResource.SampleText: string;
begin
  Result := 'Hello World, I am a filter! ';
end;

function TFilterDemoResource.RaiseTest: string;
begin
  raise Exception.Create('Test error!');
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TFilterDemoResource>;

end.
