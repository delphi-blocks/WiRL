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

{$I WiRL.inc}

interface

uses
  System.SysUtils, System.Classes, Data.DB,
  FireDAC.Comp.Client,

  WiRL.Engine.REST,
  WiRL.Core.Attributes,
  WiRL.http.Accept.MediaType,
  WiRL.Core.MessageBody.Default,
  WiRL.Data.MessageBody.Default,
  WiRL.Data.FireDAC.MessageBody.Default,
  WiRL.Core.JSON,

  Server.Entities;

type
  [Path('content')]
  TSampleResource = class
  private
    const XML_AND_JSON = TMediaType.APPLICATION_XML + ',' + TMediaType.APPLICATION_JSON;
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function HelloWorld_TEXT: string;

    [GET, Produces(TMediaType.TEXT_HTML)]
    function HelloWorld_HTML: string;

    [GET, Produces(TMediaType.APPLICATION_JSON)]
    function HelloWorld_JSON: TJSONObject;

    [GET, Produces(TMediaType.APPLICATION_XML)]
    function HelloWorld_XML: string;

    [GET, Produces(TMediaType.IMAGE_JPEG)]
    function JpegImage: TStream;

    [GET, Produces(TMediaType.APPLICATION_PDF)]
    function PdfDocument: TStream;

    [PUT, Consumes(TMediaType.TEXT_PLAIN), Produces(TMediaType.TEXT_PLAIN)]
    function CustomizedHelloWorld_TEXT([BodyParam] const AText: string): string;

    [PUT, Consumes(TMediaType.TEXT_PLAIN), Produces(TMediaType.TEXT_HTML)]
    function CustomizedHelloWorld_HTML([BodyParam] const AText: string): string;

    [GET, Path('/entity')]
    [Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_JAVASCRIPT)]
    function GetEntityJSON: TArray<TSimpleClass>;

    [GET, Path('/array')]
    function SimpleArray: TArray<Integer>;

    [GET, Path('/int')]
    function GetInteger: Integer;

    [GET, Path('/dataset')]
    [Produces(XML_AND_JSON)]
    function DataSet1: TDataSet;

    [GET, Path('/datasets'), Produces(TMediaType.APPLICATION_JSON)]
    function DataSets: TArray<TDataset>;

    [GET, Path('/fddataset')]
    function DataSet2: TFDMemTable;
  end;

implementation

uses
  System.IOUtils, Datasnap.DBClient,
  WiRL.Core.Registry;


{ TSampleResource }

function TSampleResource.CustomizedHelloWorld_HTML(const AText: string): string;
begin
  Result := Format('<html><body><h2>%s</h2></body></html>', [AText]);
end;

function TSampleResource.CustomizedHelloWorld_TEXT(const AText: string): string;
begin
  Result := AText;
end;

function TSampleResource.DataSet1: TDataSet;
var
  LCDS: TClientDataSet;
begin
  LCDS := TClientDataSet.Create(nil);
  LCDS.Name := 'CDS';
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
  Result.Name := 'FDMT';
  Result.FieldDefs.Add('Firstname', ftString, 100);
  Result.FieldDefs.Add('Surname', ftString, 100);
  Result.CreateDataSet;
  Result.AppendRecord(['Alberto', 'Dal Dosso']);
  Result.AppendRecord(['Paolo', 'Rossi']);
  Result.AppendRecord(['Luca', 'Minuti']);
end;

function TSampleResource.DataSets: TArray<TDataset>;
begin
  {$IFDEF HAS_NEW_ARRAY}
  Result := [DataSet1, DataSet2];
  {$ELSE}
  SetLength(Result, 2);
  Result[0] := DataSet1;
  Result[1] := DataSet2;
  {$ENDIF}
end;

function TSampleResource.GetEntityJSON: TArray<TSimpleClass>;
var
  LEntity: TSimpleClass;
  LIndex: Integer;
begin
  Result := [];
  for LIndex := 0 to 100 do
  begin
    LEntity := TSimpleClass.RandomEntity;
    Result := Result + [LEntity];
  end;
end;

function TSampleResource.GetInteger: Integer;
begin
  Result := 42;
end;

function TSampleResource.HelloWorld_HTML: string;
begin
  Result :=
    '<html><body>' +
    '<h2>Hello World!</h2>' +
    '</body></html>';
end;

function TSampleResource.JpegImage: TStream;
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

function TSampleResource.HelloWorld_JSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('Message', 'Hello World!');
end;

function TSampleResource.PdfDocument: TStream;
var
  LFileName: string;
begin
  LFileName := IncludeTrailingPathDelimiter(
    TDirectory.GetParent(
      TDirectory.GetParent(
        TDirectory.GetParent(TWiRLRESTEngine.ServerDirectory)))) +
    'wirl-doc.pdf';
  Result := TFileStream.Create(LFileName, fmOpenRead or fmShareDenyWrite);
end;

function TSampleResource.SimpleArray: TArray<Integer>;
begin
  {$IFDEF HAS_NEW_ARRAY}
  Result := [23, 44, 567];
  {$ELSE}
  SetLength(Result, 3);
  Result[0] := 23;
  Result[1] := 44;
  Result[2] := 567;
  {$ENDIF}
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
