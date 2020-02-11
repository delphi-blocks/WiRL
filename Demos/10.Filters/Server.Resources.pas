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
  System.SysUtils, System.Classes,

  WiRL.Core.JSON,
  WiRL.Core.Attributes,
  WiRL.http.Accept.MediaType,
  WiRL.http.Filters.Compression,

  Server.Filters.Attributes;

type
  [Path('filterdemo')]
  [PoweredByWiRL]
  [Compression]
  TFilterDemoResource = class
  private
  protected
  public
    [GET]
    [Produces(TMediaType.TEXT_PLAIN)]
    function LongText: string;

    [GET, Path('/echostring/{AString}')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function EchoString([PathParam] AString: string): string;

    [GET, Path('/raise/')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function RaiseTest: string;

    [GET, Path('/bin')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function BinaryTest: TStream;

  end;

implementation

uses
  System.IOUtils,
  WiRL.Core.Registry;

{ TFilterDemoResource }

function TFilterDemoResource.BinaryTest: TStream;
var
  LFileName: string;
begin
  LFileName := TPath.Combine(ExtractFilePath(ParamStr(0)), 'Server.Forms.Main.dcu');
  Result := TFileStream.Create(LFileName, fmOpenRead);
end;

function TFilterDemoResource.EchoString(AString: string): string;
begin
  Result := AString;
end;

function TFilterDemoResource.LongText: string;
begin
  Result := 'Lorem Ipsum is simply dummy text of the printing and typesetting industry. ' +
    'Lorem Ipsum has been the industry standard dummy text ever since the 1500s, ' +
    'when an unknown printer took a galley of type and scrambled it to make a type ' +
    'specimen book. It has survived not only five centuries, but also the leap into ' +
    'electronic typesetting, remaining essentially unchanged. It was popularised in ' +
    'the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, ' +
    'and more recently with desktop publishing software like Aldus PageMaker including ' +
    'versions of Lorem Ipsum.';
end;

function TFilterDemoResource.RaiseTest: string;
begin
  raise Exception.Create('Test error!');
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TFilterDemoResource>;

end.
