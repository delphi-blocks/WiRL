unit Server.Resources.Database;

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
  WiRL.Core.MessageBody.Default;

type
  [Path('/database')]
  TDatabaseResource = class
  public
    [GET, Path('db')]
    [Produces(TMediaType.APPLICATION_JSON)]
    function GetDBData(): TFDMemTable;

    [POST, Path('db')]
    [Consumes(TMediaType.APPLICATION_JSON)]
    [Produces(TMediaType.TEXT_PLAIN)]
    function PostDBData([BodyParam] APostStream: TStream): Integer;

  end;

implementation

function TDatabaseResource.GetDBData: TFDMemTable;
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

function TDatabaseResource.PostDBData(APostStream: TStream): Integer;
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

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TDatabaseResource>;


end.
