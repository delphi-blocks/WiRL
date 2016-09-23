(*
  Copyright 2015-2016, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit Server.Resources;

interface

uses
  SysUtils, Classes,

  MARS.Core.Attributes,
  MARS.Core.MediaType,
  MARS.Core.JSON,

  Server.Filters.Attributes;

type
  [Path('helloworld')]
  THelloWorldResource = class
  private
  protected
  public
    [GET]
    [PoweredByMARS]
    [Produces(TMediaType.TEXT_PLAIN)]
    function HelloWorld(): string;

    [GET, Path('/echostring/{AString}')]
    function EchoString([PathParam] AString: string): string;
  end;

implementation

uses
  MARS.Core.Registry;


{ THelloWorldResource }

function THelloWorldResource.EchoString(AString: string): string;
begin
  Result := AString;
end;

function THelloWorldResource.HelloWorld(): string;
begin
  Result := 'Hello World!';
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<THelloWorldResource>;

end.
