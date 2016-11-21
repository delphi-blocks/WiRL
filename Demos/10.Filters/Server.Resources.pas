(*
  Copyright 2015-2016, WiRL - REST Library

  Home: https://github.com/WiRL-library

*)
unit Server.Resources;

interface

uses
  System.SysUtils, System.Classes,

  WiRL.Core.Attributes,
  WiRL.Core.MediaType,
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
