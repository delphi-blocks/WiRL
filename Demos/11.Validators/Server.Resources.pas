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

  Server.Validators,
  Server.Consts;

type
  [Path('valdemo')]
  TValidatorDemoResource = class
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function SampleText: string;

    [GET, Path('/echostring/{AString}')]
    function EchoString([PathParam] AString: string): string;

    [GET, Path('/double/{AValue}')]
    function Double([PathParam][Max(50, sMaxErrorName)] AValue: Integer): Integer;
  end;

implementation

uses
  WiRL.Core.Registry;

resourcestring
  sMaxError = 'Max';

{ TFilterDemoResource }

function TValidatorDemoResource.Double(AValue: Integer): Integer;
begin
  Result := AValue * 2;
end;

function TValidatorDemoResource.EchoString(AString: string): string;
begin
  Result := AString;
end;

function TValidatorDemoResource.SampleText: string;
begin
  Result := 'Hello World, I am a validator! ';
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TValidatorDemoResource>;

end.
