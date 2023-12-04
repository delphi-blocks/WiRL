{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2021 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Resources;

interface

uses
  System.Classes, System.SysUtils, System.JSON, System.NetEncoding, System.Generics.Collections,
  Data.DB, FireDAC.Comp.Client, FireDAC.Comp.DataSet, FireDAC.Stan.Intf,

  WiRL.http.URL,
  WiRL.http.MultipartData,
  WiRL.Core.Engine,
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

    [GET, Path('db')]
    [Produces(TMediaType.APPLICATION_JSON)]
    function GetDBData(): TFDMemTable;

    [POST, Path('db')]
    [Consumes(TMediaType.APPLICATION_JSON)]
    [Produces(TMediaType.TEXT_PLAIN)]
    function PostDBData([BodyParam] APostStream: TStream): Integer;

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

{ THelloWorldResource }

function THelloWorldResource.EchoString(AString: string): string;
begin
  Result := AString;
end;

function THelloWorldResource.GetDBData: TFDMemTable;
var
  LMemTable: TFDMemTable;
begin
  LMemTable := TFDMemTable.Create(nil);
  LMemTable.FieldDefs.Add('id', ftInteger);
  LMemTable.FieldDefs.Add('value', ftString, 100);

  LMemTable.Open;

  LMemTable.AppendRecord([1, 'Luca']);
  LMemTable.AppendRecord([2, 'Paolo']);
  LMemTable.MergeChangeLog;

  Result := LMemTable;
end;

function THelloWorldResource.GetPerson(Id: Integer): TPerson;
begin
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

function THelloWorldResource.PostDBData(APostStream: TStream): Integer;
var
  LMemTable: TFDMemTable;
begin
  LMemTable := TFDMemTable.Create(nil);
  try
    LMemTable.LoadFromStream(APostStream, sfJSON);
    Result := LMemTable.ChangeCount;
  finally
    LMemTable.Free;
  end;
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
      [FormParam] AValue: string;
      [FormParam] AContent: TWiRLFormDataPart;
      [FormParam] AJSON: TJSONObject
    ): TJSONObject;
var
  LContentBuffer: TBytes;
begin
  LContentBuffer := AContent.RawContent;
  Result := TJSONObject.Create;
  Result
    .AddPair('AValue', AValue)
    .AddPair('AJSON', AJSON.ToJSON)
    .AddPair('FileName', AContent.FileName)
    .AddPair('AContent', TNetEncoding.Base64.EncodeBytesToString(LContentBuffer));
end;

function THelloWorldResource.PostStreamExample(AContent: TStream): string;
begin
  Result := 'Stream len: ' + IntToStr(AContent.Size);
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
