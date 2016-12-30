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
  System.Classes, System.SysUtils, System.JSON,
  WiRL.Core.Engine,
  WiRL.Core.Application,
  WiRL.Core.Registry,
  WiRL.Core.Attributes,
  WiRL.http.Accept.MediaType,
  WiRL.Core.URL,
  WiRL.Core.MessageBodyWriters,
  WiRL.Core.MessageBodyReaders,
  WiRL.Core.Auth.Context,
  WiRL.Core.Request,
  WiRL.Core.Response;

type
  [Path('/helloworld')]
  THelloWorldResource = class
  private
    [Context] Request: TWiRLRequest;
    [Context] AuthContext: TWiRLAuthContext;
  public
    [GET]
    [Produces(TMediaType.TEXT_PLAIN + TMediaType.WITH_CHARSET_UTF8)]
    function HelloWorld(): string;

    [GET, Path('/time')]
    [Produces(TMediaType.APPLICATION_JSON)]
    function WhatTimeIsIt: TDateTime;

    [GET, Path('/echostring/{AString}')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function EchoString([PathParam] AString: string): string;

    [GET, Path('/reversestring/{AString}')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function ReverseString([PathParam] AString: string): string;

    [GET, Path('/params/{AOne}/{ATwo}')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function Params([PathParam] AOne: string; [PathParam] ATwo: string): string;

    [GET, Path('/authinfo'), Produces(TMediaType.APPLICATION_JSON)]
    function GetAuthInfo: string;

    [GET, Path('/sum/{Addendo1}/{Addendo2}')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function Somma(
      [PathParam] Addendo1: Integer;
      [PathParam] Addendo2: Integer): Integer;

    [GET, Path('/exception'), Produces(TMediaType.APPLICATION_JSON)]
    function TestException: string;

    [POST, Path('/postexample'), Produces(TMediaType.TEXT_PLAIN)]
    function PostExample([BodyParam] AContent: string): string;

    [POST, Path('/postjsonexample'), Produces(TMediaType.TEXT_PLAIN), Consumes(TMediaType.APPLICATION_JSON)]
    function PostJSONExample([BodyParam] AContent: TJSONObject): string;

    [POST, Path('/postjsonarray'), Produces(TMediaType.TEXT_PLAIN), Consumes(TMediaType.APPLICATION_JSON)]
    function PostJSONArrayExample([BodyParam] AContent: TJSONArray): string;

    [POST, Path('/poststream'), Produces(TMediaType.TEXT_PLAIN), Consumes(TMediaType.APPLICATION_OCTET_STREAM)]
    function PostStreamExample([BodyParam] AContent: TStream): string;
  end;

  [Path('/entity')]
  TEntityResource = class
  private
    [Context] URL: TWiRLURL;
  public
    [GET, Path('/url')]
    [Produces(TMediaType.APPLICATION_JSON)]
    function EchoURL: TJSONObject;

    [GET, Path('/image')]
    [Produces('image/png')]
    function GetImage: TStream;

    [GET, Path('/pdf')]
    [Produces('application/pdf')]
    function GetPDF: TStream;
  end;


implementation

uses
  System.DateUtils, System.StrUtils, System.IOUtils,
  WiRL.http.Accept.Language;

{ THelloWorldResource }

function THelloWorldResource.EchoString(AString: string): string;
begin
  Result := AString;
end;

function THelloWorldResource.GetAuthInfo: string;
begin
  Result := AuthContext.Subject.JSON.ToJSON;
end;

function THelloWorldResource.HelloWorld(): string;
var
  LLang: TAcceptLanguage;
begin
  LLang := TAcceptLanguage.Create('it');
  try
    if Request.AcceptableLanguages.Contains(LLang) then
      Result := 'Ciao Mondo! טיטחח'
    else
      Result := 'Hello World! טיטחח';
  finally
    LLang.Free;
  end;
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

function THelloWorldResource.PostJSONArrayExample(AContent: TJSONArray): string;
begin
  Result := 'Array len: ' + IntToStr(AContent.Count);
end;

function THelloWorldResource.PostJSONExample(AContent: TJSONObject): string;
begin
  Result := 'Name=' + AContent.GetValue<string>('name');
end;

function THelloWorldResource.PostStreamExample(AContent: TStream): string;
begin
  Result := 'Stream len: ' + IntToStr(AContent.Size);
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

function THelloWorldResource.WhatTimeIsIt: TDateTime;
begin
  Result := Now;
end;

{ TEntityResource }

function TEntityResource.EchoURL: TJSONObject;
begin
  Result := URL.ToJSONObject;
end;

function TEntityResource.GetImage: TStream;
var
  LFileName: string;
begin
  LFileName := IncludeTrailingPathDelimiter(
    TDirectory.GetParent(
      TDirectory.GetParent(
        TDirectory.GetParent(TWiRLEngine.ServerDirectory)))) +
    'WiRL-logo.png';
  Result := TFileStream.Create(LFileName, fmOpenRead or fmShareDenyWrite);
end;

function TEntityResource.GetPDF: TStream;
var
  LFileName: string;
begin
  LFileName := IncludeTrailingPathDelimiter(
    TDirectory.GetParent(
      TDirectory.GetParent(
        TDirectory.GetParent(TWiRLEngine.ServerDirectory)))) +
    'WiRL-doc.pdf';
  Result := TFileStream.Create(LFileName, fmOpenRead or fmShareDenyWrite);
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<THelloWorldResource>;
  TWiRLResourceRegistry.Instance.RegisterResource<TEntityResource>;

end.
