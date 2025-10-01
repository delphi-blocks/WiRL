{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2021 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Resources.HelloWorld;

interface

uses
  System.Classes, System.SysUtils, System.JSON, System.NetEncoding, System.Generics.Collections,
  Data.DB, FireDAC.Comp.Client, FireDAC.Comp.DataSet, FireDAC.Stan.Intf,

  WiRL.http.URL,
  WiRL.http.MultipartData,
  WiRL.Engine.REST,
  WiRL.Core.Exceptions,
  WiRL.Core.Registry,
  WiRL.Core.Attributes,
  WiRL.Core.Application,
  WiRL.Core.GarbageCollector,
  WiRL.http.Accept.MediaType,
  WiRL.Core.MessageBody.Default,
  //WiRL.Data.FireDAC.MessageBody.Default,

  Demo.Entities;

type
  [Path('/helloworld')]
  THelloWorldResource = class
  public
    [GET]
    [Produces(TMediaType.TEXT_PLAIN + TMediaType.WITH_CHARSET_UTF8)]
    function HelloWorld(): string;

    [GET, Path('/time')]
    [Produces(TMediaType.TEXT_PLAIN)]
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

    [GET, Path('/sum/{Qty1}/{Qty2}')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function Sum([PathParam] Qty1: Integer; [PathParam] Qty2: Integer): Integer;

    [GET, Path('/exception'), Produces(TMediaType.APPLICATION_JSON)]
    function TestException: string;

    [GET, Path('/person'), Produces(TMediaType.APPLICATION_JSON)]
    function GetPerson([QueryParam] Id: Integer): TPerson;

    [POST, Path('/order'), Consumes(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_JSON)]
    function PostOrder([BodyParam] AOrderProposal: TOrderProposal): TOrder;

    /// <summary>
    ///   This method wants to show how to get the body as string, it is not
    ///   the preferred example on how to get data from a post.
    /// </summary>
    [POST, Path('/poststring'), Consumes(TMediaType.TEXT_PLAIN), Produces(TMediaType.TEXT_PLAIN)]
    function PostString([BodyParam] const AContent: string): string;

    [POST, Path('/postobj'), Consumes(TMediaType.APPLICATION_JSON), Produces(TMediaType.TEXT_PLAIN)]
    function PostObj([BodyParam] APerson: TPerson): string;

    [POST, Path('/postlist'), Consumes(TMediaType.APPLICATION_JSON), Produces(TMediaType.TEXT_PLAIN)]
    function PostList([BodyParam] AList: TPersonList): string;

    [POST, Path('/poststream'), Consumes(TMediaType.APPLICATION_OCTET_STREAM), Produces(TMediaType.TEXT_PLAIN)]
    function PostStreamExample([BodyParam] AContent: TStream): string;

    [POST, Path('/multipart'), Consumes(TMediaType.MULTIPART_FORM_DATA), Produces(TMediaType.APPLICATION_JSON)]
    function PostMultiPartExample(
      [FormParam] AValue: string;
      [FormParam] AContent: TWiRLFormDataPart;
      [FormParam] AJSON: TJSONObject
    ): TJSONObject;
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

function DumpStream(AStream: TStream; AMaxSize: Integer = -1): string;
var
  LBuffer: TBytes;
  LByte: Byte;
  LLen: Integer;
begin
  // Exit if the stream is empty
  if AMaxSize.Size = 0 then
    Exit('');

  // If specified read only the first 'AMaxSize' bytes
  if (AMaxSize > 0) and (AStream.Size > AMaxSize) then
    LLen := AMaxSize
  else
    LLen := AStream.Size;

  SetLength(LBuffer, LLen);
  AStream.Read(LBuffer[0], LLen);

  for LByte in LBuffer do
  begin
    Result := Result + IntToHex(LByte, 2) + ' ';
  end;
end;


{ THelloWorldResource }

function THelloWorldResource.EchoString(AString: string): string;
begin
  Result := AString;
end;

function THelloWorldResource.GetPerson(Id: Integer): TPerson;
begin
  if Id >= 100 then
    raise EWiRLNotFoundException.Create('Not found!');

  Result := TPerson.Create;
  Result.Name := 'Paolo Rossi';
  Result.Age := Id;
  Result.Detail := 'Person Detail';
end;

function THelloWorldResource.HelloWorld(): string;
begin
  Result := 'Hello World!';
end;

function THelloWorldResource.Params(AOne, ATwo: string): string;
begin
  Result := 'One: ' + AOne + sLineBreak + 'Two: ' + ATwo;
end;

function THelloWorldResource.PostString(const AContent: string): string;
begin
  Result := 'PostString: ' + AContent;
end;

function THelloWorldResource.PostList(AList: TPersonList): string;
begin
  Result := 'List Count: ' + AList.Count.ToString;
end;

function THelloWorldResource.PostObj(APerson: TPerson): string;
begin
  Result := Format('Name: %s, Age: %d', [APerson.Name, APerson.Age]);
end;

function THelloWorldResource.PostOrder(AOrderProposal: TOrderProposal): TOrder;
begin
  Result := TOrder.Create;

  Result.ID := Random(1000);
  Result.Description := AOrderProposal.Description;
  Result.Article := AOrderProposal.Article;
  Result.Quantity := AOrderProposal.Quantity;
  Result.DueDate := AOrderProposal.DueDate;

end;

function THelloWorldResource.PostMultiPartExample(
  AValue: string;
  AContent: TWiRLFormDataPart;
  AJSON: TJSONObject
): TJSONObject;
var
  LContentBuffer: TBytes;
begin
  LContentBuffer := AContent.RawContent;
  Result := TJSONObject.Create;
  Result
    .AddPair('AValue', AValue)
    .AddPair('JSON', AJSON.ToJSON)
    .AddPair('FileName', AContent.FileName)
    .AddPair('ContentSize', TJSONNumber.Create(Length(LContentBuffer)))
    .AddPair('Content', DumpStream(AContent.ContentStream));
end;

function THelloWorldResource.PostStreamExample(AContent: TStream): string;
begin
  Result :=
    'Stream len: ' + IntToStr(AContent.Size) + sLineBreak +
    DumpStream(AContent) +  sLineBreak;
end;

function THelloWorldResource.ReverseString(AString: string): string;
begin
  Result := System.StrUtils.ReverseString(AString);
end;

function THelloWorldResource.Sum(Qty1, Qty2: Integer): Integer;
begin
  Result := Qty1 + Qty2;
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
        TDirectory.GetParent(TWiRLRESTEngine.ServerDirectory)))) +
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
        TDirectory.GetParent(TWiRLRESTEngine.ServerDirectory)))) +
    'WiRL-doc.pdf';
  Result := TFileStream.Create(LFileName, fmOpenRead or fmShareDenyWrite);
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<THelloWorldResource>;
  TWiRLResourceRegistry.Instance.RegisterResource<TEntityResource>;

end.
