{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2026 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Demo.Resources;

interface

uses
  System.Classes, System.SysUtils, System.JSON, System.TypInfo,

  WiRL.Configuration.Neon,
  WiRL.Configuration.Converter,
  Neon.Core.Types,
  WiRL.Core.Converter,
  WiRL.Core.Exceptions,
  WiRL.Core.Registry,
  WiRL.Core.Attributes,
  WiRL.Core.Application,
  WiRL.Core.GarbageCollector,
  WiRL.Core.MessageBody.Default,
  WiRL.http.Server,
  WiRL.http.Accept.MediaType,
  WiRL.Engine.REST,

  Demo.Entities;

type
  TDemoServer = class
    class procedure ConfigureServer(AServer: TWiRLServer);
  end;


  [Path('/demo')]
  TDemoResource = class
  public
    [GET]
    [Produces(TMediaType.APPLICATION_JSON)]
    function HelloWorld(): THelloWorld;

    [GET, Path('/time')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function WhatTimeIsIt: TDateTime;

    [GET, Path('/exception'), Produces(TMediaType.APPLICATION_JSON)]
    function TestException: string;

    [GET, Path('/person'), Produces(TMediaType.APPLICATION_JSON)]
    function GetPerson([QueryParam] Id: Integer): TPerson;

    [POST, Path('/order'), Consumes(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_JSON)]
    function PostOrder([BodyParam] AOrderProposal: TOrderProposal): TOrder;

    [POST, Path('/postobj'), Consumes(TMediaType.APPLICATION_JSON), Produces(TMediaType.TEXT_PLAIN)]
    function PostObj([BodyParam] APerson: TPerson): string;

    [POST, Path('/postlist'), Consumes(TMediaType.APPLICATION_JSON), Produces(TMediaType.TEXT_PLAIN)]
    function PostList([BodyParam] AList: TPersonList): string;

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


function TDemoResource.GetPerson(Id: Integer): TPerson;
begin
  if Id >= 100 then
    raise EWiRLNotFoundException.Create('Not found!');

  Result := TPerson.Create;
  Result.Name := 'Paolo Rossi';
  Result.Age := Id;
  Result.Details := 'Person Detail';
end;

function TDemoResource.HelloWorld(): THelloWorld;
begin
  Result := THelloWorld.Create;
  Result.Id := Random(1000);
  Result.Message := 'Hello World!';
end;

function TDemoResource.PostList(AList: TPersonList): string;
begin
  Result := 'List Count: ' + AList.Count.ToString;
end;

function TDemoResource.PostObj(APerson: TPerson): string;
begin
  Result := Format('Name: %s, Age: %d', [APerson.Name, APerson.Age]);
end;

function TDemoResource.PostOrder(AOrderProposal: TOrderProposal): TOrder;
begin
  Result := TOrder.Create;

  Result.ID := Random(1000);
  Result.Description := AOrderProposal.Description;
  Result.Article := AOrderProposal.Article;
  Result.Quantity := AOrderProposal.Quantity;
  Result.DueDate := AOrderProposal.DueDate;

end;

function TDemoResource.TestException: string;
begin
  raise EWiRLNotFoundException.Create('Resource not found');
end;

function TDemoResource.WhatTimeIsIt: TDateTime;
begin
  Result := Now;
end;

{ TDemoServer }

class procedure TDemoServer.ConfigureServer(AServer: TWiRLServer);
begin
  AServer.AddEngine<TWiRLRESTEngine>('/rest')
    .SetEngineName('RESTEngine')
    .AddApplication('/app')
      .SetResources('*')
      .SetFilters('*')

      .Plugin.Configure<IWiRLFormatSetting>
        .AddFormat(TypeInfo(TDateTime), TWiRLFormatSetting.ISODATE_UTC)
        .ApplyConfig

      .Plugin.Configure<IWiRLConfigurationNeon>
        .SetUseUTCDate(True)
        .SetVisibility([mvPublic, mvPublished])
        .SetMemberCase(TNeonCase.PascalCase)
  ;
end;

initialization
  Randomize();
  TWiRLResourceRegistry.Instance.RegisterResource<TDemoResource>;

end.
