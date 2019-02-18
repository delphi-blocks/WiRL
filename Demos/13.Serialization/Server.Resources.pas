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
  System.SysUtils, System.Classes, Data.DB, System.JSON,
  FireDAC.Comp.Client,

  WiRL.Core.Attributes,
  WiRL.http.Accept.MediaType,
  WiRL.Core.MessageBody.Default,
  WiRL.Data.MessageBody.Default,
  WiRL.Data.FireDAC.MessageBody.Default,
  Server.Entities;

type
  [Path('entity')]
  TEntityResource = class
  public
    [Path('simpleclass')]
    [GET, Produces(TMediaType.APPLICATION_JSON)]
    function SimpleClass: TCaseClass;

    [Path('complexclass')]
    [GET, Produces(TMediaType.APPLICATION_JSON)]
    function ComplexClass: TPerson;

    [Path('record')]
    [GET, Produces(TMediaType.APPLICATION_JSON)]
    function GetRecord: TMyRecord;

    [GET, Produces('image/jpg')]
    function JpegImage: TStream;

    [GET, Produces('application/pdf')]
    function PdfDocument: TStream;

    [GET, Path('/dataset1')]
    [Produces(TMediaType.APPLICATION_JSON)]
    function DataSet1: TDataSet;

    [GET, Path('/dataset2')]
    [Produces(TMediaType.APPLICATION_JSON)]
    function DataSet2: TFDMemTable;

    [GET, Path('/dataset3'), Produces(TMediaType.APPLICATION_JSON)]
    function DataSet3: TDataset;
  end;

implementation

uses
  Datasnap.DBClient,
  WiRL.Core.Registry;


{ TEntityResource }

function TEntityResource.DataSet1: TDataSet;
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

function TEntityResource.DataSet2: TFDMemTable;
begin
  Result := TFDMemTable.Create(nil);
  Result.FieldDefs.Add('Name', ftString, 100);
  Result.FieldDefs.Add('Surname', ftString, 100);
  Result.CreateDataSet;
  Result.AppendRecord(['Alberto', 'Dal Dosso']);
  Result.AppendRecord(['Paolo', 'Rossi']);
  Result.AppendRecord(['Luca', 'Minuti']);
end;

function TEntityResource.DataSet3: TDataset;
begin
  Result := DataSet2;
end;

function TEntityResource.GetRecord: TMyRecord;
begin
  Result.One := 'First Field';
  Result.Two := 4242;
end;

function TEntityResource.ComplexClass: TPerson;
begin
  Result := TPerson.Create;

  Result.Name := 'Paolo';
  Result.Surname := 'Rossi';
  Result.AddAddress('Piacenza', 'Italy');
  Result.AddAddress('Parma', 'Italy');
  Result.Note.Date := Now;
  Result.Note.Text := 'Note Text';
end;

function TEntityResource.JpegImage: TStream;
begin
  Result := TFileStream.Create('image.jpg', fmOpenRead or fmShareDenyWrite);
end;

function TEntityResource.PdfDocument: TStream;
begin
  Result := TFileStream.Create('document.pdf', fmOpenRead or fmShareDenyWrite);
end;

function TEntityResource.SimpleClass: TCaseClass;
begin
  Result := TCaseClass.DefaultValues;
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TEntityResource>;

end.
