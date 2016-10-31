(*
  Copyright 2015-2016, WiRL - REST Library

  Home: https://github.com/WiRL-library

*)
unit Server.Resources;

interface

uses
  SysUtils, Classes,

  WiRL.Core.Attributes,
  WiRL.Core.MediaType,
  WiRL.Core.JSON,

  Server.Filters.Attributes;

type
  [Path('helloworld')]
  [PoweredByWiRL]
  THelloWorldResource = class
  private
  protected
  public
    [GET]
    [Produces(TMediaType.TEXT_PLAIN)]
    function HelloWorld(): string;

    [GET, Path('/echostring/{AString}')]
    function EchoString([PathParam] AString: string): string;

    [GET, Path('/raise/')]
    function RaiseTest: string;
  end;

implementation

uses
  WiRL.Core.Registry;


{ THelloWorldResource }

function THelloWorldResource.EchoString(AString: string): string;
begin
  Result := AString;
end;

function THelloWorldResource.HelloWorld(): string;
begin
  Result := 'Hello World!';
end;

function THelloWorldResource.RaiseTest: string;
begin
  raise Exception.Create('Test error!');
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<THelloWorldResource>;

end.
