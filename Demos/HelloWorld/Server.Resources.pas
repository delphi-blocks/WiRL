(*
  Copyright 2015-2016, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit Server.Resources;

interface

uses
  System.Classes, System.SysUtils, System.Rtti, Web.HTTPApp, Generics.Collections,
  MARS.Core.JSON,
  MARS.Core.Registry,
  MARS.Core.Attributes,
  MARS.Core.MediaType,
  MARS.Core.URL,
  MARS.Core.MessageBodyWriters,
  MARS.Core.Token,
  MARS.Core.Response;

type
  [Path('/helloworld')]
  THelloWorldResource = class
  private
  protected
    [Context] URL: TMARSURL;
    [Context] Request: TWebRequest;
    [Context] Response: TWebResponse;
    [Context] Token: TMARSAuthContext;
  public
    [GET]
    [Produces(TMediaType.TEXT_PLAIN)]
    function HelloWorld(): string;

    [GET, Path('/echostring/{AString}')]
    function EchoString([PathParam] AString: string): string;

    [GET, Path('/reversestring/{AString}')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function ReverseString([PathParam] AString: string): string;

    [GET, Path('/params/{AOne}/{ATwo}')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function Params([PathParam] AOne: string; [PathParam] ATwo: string): string;

    [GET, Path('/echourl')]
    [Produces(TMediaType.APPLICATION_JSON)]
    function EchoURL: TJSONObject;

    [GET, Path('/sessioninfo')]
    function GetSessionInfo: string;

    [GET, Path('/somma/{Addendo1}/{Addendo2}')]
    function Somma(
      [PathParam] Addendo1: Integer;
      [PathParam] Addendo2: Integer): Integer;

    [GET, Path('/exception'), Produces(TMediaType.APPLICATION_JSON)]
    function TestException: string;

    [POST, Path('/postexample'), Produces(TMediaType.TEXT_PLAIN)]
    function PostExample([BodyParam] AContent: string): string;

    [GET, Path('/image')]
    [Produces('image/png')]
    function GetImage: TStream;

    [GET, Path('/pdf')]
    [Produces('application/pdf')]
    function GetPDF: TStream;
  end;

implementation

uses
  System.DateUtils, System.StrUtils, System.IOUtils;

{ THelloWorldResource }

function THelloWorldResource.EchoString(AString: string): string;
begin
  Result := AString;
end;

function THelloWorldResource.EchoURL: TJSONObject;
begin
  Result := URL.ToJSONObject;
end;

function THelloWorldResource.GetImage: TStream;
var
  LFileName: string;
begin
  LFileName := IncludeTrailingPathDelimiter(
    TDirectory.GetParent(
      TDirectory.GetParent(
        TDirectory.GetParent(WebApplicationDirectory)))) +
    'mars-logo.png';
  Result := TFileStream.Create(LFileName, fmOpenRead or fmShareDenyWrite);
end;

function THelloWorldResource.GetPDF: TStream;
var
  LFileName: string;
begin
  LFileName := IncludeTrailingPathDelimiter(
    TDirectory.GetParent(
      TDirectory.GetParent(
        TDirectory.GetParent(WebApplicationDirectory)))) +
    'mars.pdf';
  Result := TFileStream.Create(LFileName, fmOpenRead or fmShareDenyWrite);
end;

function THelloWorldResource.GetSessionInfo: string;
begin

end;

function THelloWorldResource.HelloWorld(): string;
begin
  Result := 'Hello, ' + Token.Subject.DisplayName;
end;

function THelloWorldResource.Params(AOne, ATwo: string): string;
begin
  Result := 'One: ' + AOne + sLineBreak + 'Two: ' + ATwo;
end;

function THelloWorldResource.PostExample(AContent: string): string;
var
  LArray: TJSONArray;
  LElement: TJSONValue;
begin
  Result := 'PostExample:';
  LArray := TJSONObject.ParseJSONValue(AContent) as TJSONArray;
  try
    for LElement in LArray do
    begin
      if Result <> '' then
        Result := Result + sLineBreak;

      Result := Result + 'Element: ' + LElement.ToJSON;
    end;
  finally
    LArray.Free;
  end;
end;

function THelloWorldResource.ReverseString(AString: string): string;
begin
  Result := System.StrUtils.ReverseString(AString);
end;

function THelloWorldResource.Somma(Addendo1, Addendo2: Integer): Integer;
begin
  Result := Addendo1 + Addendo2;
end;

function THelloWorldResource.TestException: string;
begin
  raise Exception.Create('User Error Message');
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<THelloWorldResource>;

end.
