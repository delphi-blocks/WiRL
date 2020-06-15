{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Tests.Mock.Resources.Validators;

interface

uses
  System.Classes, System.SysUtils, System.StrUtils, System.JSON,

  WiRL.http.Accept.MediaType,
  WiRL.Core.Attributes,
  WiRL.Core.MessageBody.Default,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.Core.Registry,
  WiRL.Core.Exceptions,
  WiRL.Core.Validators,
  WiRL.Tests.Mock.Filters, WiRL.Tests.Mock.Validators, WiRL.Tests.Mock.Classes;

type
  [Path('/validator')]
  TValidatorResource = class
  public
    [GET, Produces(TMediaType.TEXT_PLAIN + TMediaType.WITH_CHARSET_UTF8)]
    function HelloWorld(): string;

    [GET, Path('/echostring/')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function EchoString([NotNull][QueryParam('value')] const AValue: string): string;

    [GET, Path('/double/{AValue}'), Produces(TMediaType.TEXT_PLAIN)]
    function DoubleValue([PathParam][Max(50), Min(1, 'Too small')] AValue: Integer): Integer;

    [GET, Path('/buildemail?s1={s1}&s2={s2}'), Produces(TMediaType.TEXT_PLAIN)]
    function Concat([QueryParam('email'), Pattern('.+@.+\..+', 'E-Mail is not valid')] EMail: string; [QueryParam('name'), NotNull('Name required')] Name: string): string;

    [POST, Path('/json'), Consumes(TMediaType.APPLICATION_JSON), Produces(TMediaType.TEXT_PLAIN)]
    function TestJson([BodyParam][NotNull, HasName] Json: TJSONObject): string;

    [GET]
    [Path('defaultstring')]
    [Produces(TMediaType.TEXT_PLAIN + TMediaType.WITH_CHARSET_UTF8)]
    function DefaultString([DefaultValue('test')][QueryParam('value')] const AValue: string): string;

    [GET]
    [Path('defaultinteger')]
    [Produces(TMediaType.TEXT_PLAIN + TMediaType.WITH_CHARSET_UTF8)]
    function DefaultInteger([DefaultValue('10')][QueryParam('value')] AValue: Integer): Integer;

    [GET]
    [Path('defaultdate')]
    [Produces(TMediaType.TEXT_PLAIN + TMediaType.WITH_CHARSET_UTF8)]
    function DefaultDate([DefaultValue('2020-01-01')][QueryParam('value')] AValue: TDate): string;
  end;

implementation

{ TValidatorResource }

function TValidatorResource.Concat(EMail, Name: string): string;
begin
  Result := Name + ' <' + EMail + '>';
end;

function TValidatorResource.DefaultDate(AValue: TDate): string;
var
  Year, Month, Day: Word;
begin
  DecodeDate(AValue, Year, Month, Day);
  Result := Format('y:%d m:%d d:%d', [Year, Month, Day]);
end;

function TValidatorResource.DefaultInteger(AValue: Integer): Integer;
begin
  Result := AValue;
end;

function TValidatorResource.DefaultString(const AValue: string): string;
begin
  Result := AValue;
end;

function TValidatorResource.DoubleValue(AValue: Integer): Integer;
begin
  Result := AValue * 2;
end;

function TValidatorResource.EchoString(const AValue: string): string;
begin
  Result := AValue;
end;

function TValidatorResource.HelloWorld: string;
begin
  Result := 'Hello, world!';
end;

function TValidatorResource.TestJson(Json: TJSONObject): string;
begin
  Result := Json.GetValue<string>('name');
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TValidatorResource>;

end.
