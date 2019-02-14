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

  WiRL.Core.Attributes,
  WiRL.http.Accept.MediaType,
  WiRL.Core.MessageBody.Default,
  WiRL.Core.JSON,
  WiRL.Core.Validators,

  Server.Validators;

type
  TForwardedFor = class(TObject)
  private
    FValue: string;
  public
    constructor Create(AValue: string);
    function ToString: string; override;
  end;

  [Path('validator')]
  TValidatorDemoResource = class
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function SampleText: string;

    [GET, Path('/echostring/{AString}'), Produces(TMediaType.TEXT_PLAIN)]
    function EchoString([PathParam] AString: string): string;

    [GET, Path('/double/{AValue}'), Produces(TMediaType.TEXT_PLAIN)]
    function Double([PathParam][Max(50), Min(1, 'Too small')] AValue: Integer): Integer;

    [GET, Path('/concat?s1={s1}&s2={s2}'), Produces(TMediaType.TEXT_PLAIN)]
    function Concat([QueryParam('email'), Pattern('.+@.+\..+', 'E-Mail is not valid')] EMail: string; [QueryParam('name'), NotNull('Name required')] Name: string): string;

    // Crazy test with a lot of attributes
    [GET, Path('/test?i={i}'), Produces(TMediaType.TEXT_PLAIN)]
    function Test([Max(10), NotNull, DefaultValue('0'), Pattern('1'), Size(2, 2), QueryParam('i')] i: Integer): Integer;

    [POST, Path('/body'), Produces(TMediaType.TEXT_PLAIN)]
    function TestBody([Pattern('c'), BodyParam] Body: string): string;

    [POST, Path('/json'), Consumes(TMediaType.APPLICATION_JSON), Produces(TMediaType.TEXT_PLAIN)]
    function TestJson([BodyParam][NotNull, HasName] Json: TJSONObject; [HeaderParam('X-Forwarded-For')] ForwardedFor: TForwardedFor): string;
  end;

implementation

uses
  WiRL.Core.Registry;

{ TValidatorDemoResource }

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

function TValidatorDemoResource.Test(i: Integer): Integer;
begin
  Result := i * 2;
end;

function TValidatorDemoResource.TestBody(Body: string): string;
begin
  Result := 'Hello body:' + Body;
end;

function TValidatorDemoResource.TestJson(Json: TJSONObject; ForwardedFor: TForwardedFor): string;
begin
  Result :=
    ForwardedFor.ToString + sLineBreak +
    sLineBreak +
    Json.ToJSON;
end;

{ TForwardedFor }

constructor TForwardedFor.Create(AValue: string);
begin
  FValue := AValue;
end;

function TForwardedFor.ToString: string;
begin
  Result := FValue;
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TValidatorDemoResource>;

end.
