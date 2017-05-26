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
  SysUtils, Classes, DB,
  FireDAC.Comp.Client,

  WiRL.Core.Attributes,
  WiRL.http.Accept.MediaType,
  WiRL.Core.MessageBodyWriters,
  WiRL.Data.FireDAC.MessageBodyWriters,
  WiRL.Core.JSON;

type
  [Path('sample')]
  TSampleResource = class
  private
    const XML_AND_JSON = TMediaType.APPLICATION_XML + ',' + TMediaType.APPLICATION_JSON;
  public
    [GET, Produces(TMediaType.TEXT_HTML)]
    function HelloWorld_HTML: string;

    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function HelloWorld_TEXT: string;

    [GET, Produces(TMediaType.APPLICATION_JSON)]
    function HelloWorld_JSON: TJSONObject;

    [GET, Produces(TMediaType.APPLICATION_XML)]
    function HelloWorld_XML: string;

    [GET, Produces('image/jpg')]
    function JpegImage: TStream;

    [GET, Produces('application/pdf')]
    function PdfDocument: TStream;

    [GET, Path('/dataset1')]
    [Produces(XML_AND_JSON)]
    function DataSet1: TDataSet;

    [GET, Path('/dataset2')]
    [Produces(XML_AND_JSON)]
    function DataSet2: TFDMemTable;

    [GET, Path('/dataset3'), Produces(TMediaType.APPLICATION_JSON)]
    function DataSet3: TDataset;
  end;

implementation

uses
  Datasnap.DBClient,
  WiRL.Core.Registry;


{ TSampleResource }

function TSampleResource.DataSet1: TDataSet;
var
  LCDS: TClientDataSet;
begin
  LCDS := TClientDataSet.Create(nil);
  LCDS.FieldDefs.Add('Name', ftString, 100);
  LCDS.FieldDefs.Add('Surname', ftString, 100);
  LCDS.CreateDataSet;
  LCDS.Open;

  Result := LCDS;
  Result.AppendRecord(['Luca', 'Minuti']);
  Result.AppendRecord(['Alberto', 'Dal Dosso']);
  Result.AppendRecord(['Paolo', 'Rossi']);

end;

function TSampleResource.DataSet2: TFDMemTable;
begin
  Result := TFDMemTable.Create(nil);
  Result.FieldDefs.Add('Name', ftString, 100);
  Result.FieldDefs.Add('Surname', ftString, 100);
  Result.CreateDataSet;
  Result.AppendRecord(['Alberto', 'Dal Dosso']);
  Result.AppendRecord(['Paolo', 'Rossi']);
  Result.AppendRecord(['Luca', 'Minuti']);
end;

function TSampleResource.DataSet3: TDataset;
begin
  Result := DataSet2;
end;

function TSampleResource.HelloWorld_HTML: string;
begin
  Result :=
    '<html><body>' +
    '<h2>Hello World!</h2>' +
    '</body></html>';
end;

function TSampleResource.JpegImage: TStream;
begin
  Result := TFileStream.Create('image.jpg', fmOpenRead or fmShareDenyWrite);
end;

function TSampleResource.HelloWorld_JSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('Message', 'Hello World!');
end;

function TSampleResource.PdfDocument: TStream;
begin
  Result := TFileStream.Create('document.pdf', fmOpenRead or fmShareDenyWrite);
end;

function TSampleResource.HelloWorld_TEXT: string;
begin
  Result := 'Hello World!';
end;

function TSampleResource.HelloWorld_XML: string;
begin
  Result := '<?xml version="1.0" encoding="utf-8"?><document><message>Hello World!</message></document>';
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TSampleResource>;

end.
