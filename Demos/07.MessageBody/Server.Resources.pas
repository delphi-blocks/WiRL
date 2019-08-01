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
  System.SysUtils, System.Classes, Data.DB,
  FireDAC.Comp.Client,

  WiRL.Core.Attributes,
  WiRL.http.Accept.MediaType,
  WiRL.Core.MessageBody.Default,
  {$IFDEF DJSON}
  WiRL.MessageBody.DJSON,
  {$ENDIF}
  {$IFDEF OXML}
  WiRL.MessageBody.OXML,
  {$ENDIF}
  WiRL.Data.MessageBody.Default,
  WiRL.Data.FireDAC.MessageBody.Default,
  WiRL.Core.JSON;

type
  TPerson = class
  private
    FBirthDate: TDateTime;
    FList: TStringList;
    FName: string;
  public
    constructor Create;
    destructor Destroy; override;
    property BirthDate: TDateTime read FBirthDate write FBirthDate;
    property List: TStringList read FList write FList;
    property Name: string read FName write FName;
  end;

  [Path('mb')]
  TMessageBodyResource = class
  private
    const XML_AND_JSON = TMediaType.APPLICATION_XML + ',' + TMediaType.APPLICATION_JSON;
  public
    [GET, Produces(TMediaType.TEXT_HTML)]
    function HtmlDocument: string;

    [Path('/person')]
    [Get, Produces(TMediaType.APPLICATION_JSON)]
    function GetPerson: TPerson;

    [Path('/person')]
    [POST, Consumes(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_JSON)]
    function New([BodyParam] APerson: TPerson): TPerson;

    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function SayHelloWorld: string;

    [GET, Produces(TMediaType.APPLICATION_JSON)]
    function WhoAmI: TPerson;

    [GET, Produces('image/jpg')]
    function JpegImage: TStream;

    [GET, Produces('application/pdf')]
    function PdfDocument: TStream;

    [GET, Path('/dataset')]
    [Produces(XML_AND_JSON)]
    function DataSet1: TDataSet;

    [GET, Path('/fddataset')]
    [Produces(XML_AND_JSON)]
    function DataSet2: TFDMemTable;

  end;

implementation

uses
  Datasnap.DBClient,
  WiRL.Core.Registry;


{ TMessageBodyResource }

function TMessageBodyResource.DataSet1: TDataSet;
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

function TMessageBodyResource.DataSet2: TFDMemTable;
begin
  Result := TFDMemTable.Create(nil);
  Result.FieldDefs.Add('Name', ftString, 100);
  Result.FieldDefs.Add('Surname', ftString, 100);
  Result.CreateDataSet;
  Result.AppendRecord(['Alberto', 'Dal Dosso']);
  Result.AppendRecord(['Paolo', 'Rossi']);
  Result.AppendRecord(['Luca', 'Minuti']);
end;

function TMessageBodyResource.GetPerson: TPerson;
begin
  Result := TPerson.Create;

  Result.Name := 'Paolo';
  Result.BirthDate := EncodeDate(1969, 10, 2);
end;

function TMessageBodyResource.HtmlDocument: string;
begin
  Result :=
    '<html><body>' +
    '<h2>Hello World!</h2>' +
    '</body></html>';
end;

function TMessageBodyResource.JpegImage: TStream;
begin
  Result := TFileStream.Create('image.jpg', fmOpenRead or fmShareDenyWrite);
end;

function TMessageBodyResource.New(APerson: TPerson): TPerson;
begin
  Result := TPerson.Create;
  Result.FName := APerson.Name;
  Result.List.Assign(APerson.List);
end;

function TMessageBodyResource.WhoAmI: TPerson;
begin
  Result := TPerson.Create;
  Result.Name := 'Paolo';
  Result.List.Add('First');
  Result.List.Add('Second');
  Result.List.Add('Third');
  Result.BirthDate := EncodeDate(1969, 10, 02);
end;

function TMessageBodyResource.PdfDocument: TStream;
begin
  Result := TFileStream.Create('document.pdf', fmOpenRead or fmShareDenyWrite);
end;

function TMessageBodyResource.SayHelloWorld: string;
begin
  Result := 'Hello World!';
end;

{ TPerson }

constructor TPerson.Create;
begin
  FList := TStringList.Create;
end;

destructor TPerson.Destroy;
begin
  FList.Free;
  inherited;
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TMessageBodyResource>;

end.
