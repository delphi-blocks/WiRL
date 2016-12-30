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
  WiRL.Core.Validators;

type
  [Path('validator')]
  TValidatorDemoResource = class
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function SampleText: string;

    [GET, Path('/echostring/{AString}')]
    function EchoString([PathParam] AString: string): string;

    [GET, Path('/double/{AValue}'), Produces(TMediaType.TEXT_PLAIN)]
    function Double([PathParam][Max(50), Min(1, 'Too small')] AValue: Integer): Integer;

    [GET, Path('/concat?s1={s1}&s2={s2}'), Produces(TMediaType.TEXT_PLAIN)]
    function Concat([QueryParam('email'), Pattern('.+@.+\..+', 'E-Mail is not valid')] EMail: string; [QueryParam('name'), NotNull('Name required')] Name: string): string;
  end;

implementation

uses
  WiRL.Core.Registry;

{ TFilterDemoResource }

function TValidatorDemoResource.Concat(EMail, Name: string): string;
begin
  Result := Name + ' <' + EMail + '>';
end;

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
