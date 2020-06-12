{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Tests.Mock.Resources.Core;

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
  [Path('/helloworld')]
  THelloWorldResource = class
  public
    [GET, Produces(TMediaType.TEXT_PLAIN + TMediaType.WITH_CHARSET_UTF8)]
    function HelloWorld(): string;

    [ResponseBindingTest, RequestBindingTest]
    [GET, Path('/bindingfilter'), Produces(TMediaType.TEXT_PLAIN + TMediaType.WITH_CHARSET_UTF8)]
    function BindingFilter(): string;

    [GET, Path('/echostring/{AString}')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function EchoString([PathParam] AString: string): string;

    [GET, Path('/reversestring/{AString}')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function ReverseString([PathParam] AString: string): string;

    [GET, Path('/params/{AOne}/{ATwo}')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function Params([PathParam] AOne: string; [PathParam] ATwo: string): string;

    [GET, Path('/sum/{AOne}/{ATwo}')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function Sum(
      [PathParam] AOne: Integer;
      [PathParam] ATwo: Integer): Integer;

    [GET, Path('/sumwithqueryparam?AOne={AOne}&ATwo={ATwo}')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function SumWithQueryParam(
      [QueryParam] AOne: Integer;
      [QueryParam] ATwo: Integer): Integer;


    [GET, Path('/exception'), Produces(TMediaType.APPLICATION_JSON)]
    function TestException: string;

    [GET, Path('/exception401'), Produces(TMediaType.APPLICATION_JSON)]
    [Change401To400]
    function TestException401: string;

    [POST, Path('/postecho'), Produces(TMediaType.TEXT_PLAIN)]
    function PostEcho([BodyParam] AContent: string): string;

    [POST, Path('/postjson'), Produces(TMediaType.TEXT_PLAIN), Consumes(TMediaType.APPLICATION_JSON)]
    function PostJSONExample([BodyParam] AContent: TJSONObject): string;

    [POST, Path('/postbinary'), Produces(TMediaType.APPLICATION_OCTET_STREAM), Consumes(TMediaType.APPLICATION_OCTET_STREAM)]
    function PostBinary([BodyParam] AContent: TStream): TStream;
  end;

implementation

{ THelloWorldResource }

function THelloWorldResource.BindingFilter: string;
begin
  Result := 'Binding filter';
end;

function THelloWorldResource.EchoString(AString: string): string;
begin
  Result := AString;
end;

function THelloWorldResource.HelloWorld: string;
begin
  Result := 'Hello, world!';
end;

function THelloWorldResource.Params(AOne, ATwo: string): string;
begin
  Result := AOne + ATwo;
end;

function THelloWorldResource.PostBinary(AContent: TStream): TStream;
begin
  Result := TMemoryStream.Create;
  Result.CopyFrom(AContent, AContent.Size);
  Result.Position := 0;
end;

function THelloWorldResource.PostEcho(AContent: string): string;
begin
  Result := AContent;
end;

function THelloWorldResource.PostJSONExample(AContent: TJSONObject): string;
begin
  Result := AContent.GetValue<string>('name');
end;

function THelloWorldResource.ReverseString(AString: string): string;
begin
  Result := System.StrUtils.ReverseString(AString);
end;

function THelloWorldResource.Sum(AOne, ATwo: Integer): Integer;
begin
  Result := AOne + ATwo;
end;

function THelloWorldResource.SumWithQueryParam(AOne, ATwo: Integer): Integer;
begin
  Result := AOne + ATwo;
end;

function THelloWorldResource.TestException: string;
begin
  raise Exception.Create('User Error Message');
end;

function THelloWorldResource.TestException401: string;
begin
  raise EWiRLNotAuthorizedException.Create('NotAuthorizedException');
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<THelloWorldResource>;

end.
