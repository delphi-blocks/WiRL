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
  System.SysUtils, System.Classes,

  WiRL.Engine.REST,
  WiRL.Core.Attributes,
  WiRL.http.Core,
  WiRL.http.Accept.MediaType,
  WiRL.http.Response,
  WiRL.Core.MessageBody.Default,
  WiRL.Data.MessageBody.Default,
  WiRL.Data.FireDAC.MessageBody.Default,
  WiRL.Core.JSON;

type
  [Path('streaming')]
  TChunksResource = class
  private
    [Context]
    FRequest: TWiRLResponse;
    const XML_AND_JSON = TMediaType.APPLICATION_XML + ',' + TMediaType.APPLICATION_JSON;
  public
    [GET]
    [Path('test')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function HelloWorld: string;

    [GET]
    [Path('chunks')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function Chunks([QueryParam('chunks'), DefaultValue('5')] ANumOfChunks: Integer): TWiRLChunkedResponse;

    [GET]
    [Path('sse')]
    [Produces(TMediaType.TEXT_EVENT_STREAM)]
    function ServerSideEvents([QueryParam('events'), DefaultValue('5')] ANumOfEvents: Integer): TWiRLSSEResponse;

  end;

implementation

uses
  System.IOUtils, Datasnap.DBClient,
  WiRL.Core.Registry;

{ TChunksResource }

function TChunksResource.Chunks(ANumOfChunks: Integer): TWiRLChunkedResponse;
begin
  Result := TWiRLChunkedResponse.Create(
    procedure (AWriter: IWiRLResponseWriter)
    var
      LCounter: Integer;
    begin
      for LCounter := 1 to ANumOfChunks do
      begin
        AWriter.Write(DateTimeToStr(Now) + ' - CHUNK #' + IntToStr(LCounter) + sLineBreak);
        Sleep(1000);
      end;
    end
  );
end;

function TChunksResource.HelloWorld: string;
begin
  Result := 'Hello, world!';
end;

function TChunksResource.ServerSideEvents(
  ANumOfEvents: Integer): TWiRLSSEResponse;
begin
  Result := TWiRLSSEResponse.Create(
    procedure (AWriter: IWiRLSSEResponseWriter)
    var
      LCounter: Integer;
    begin
      LCounter := 1;
      while FRequest.Connection.Connected do
      begin
        AWriter.WriteComment('This is a test');
        AWriter.Write(LCounter, 'ping', DateTimeToStr(Now) + ' - PING EVENT');
        AWriter.Write(DateTimeToStr(Now) + ' - DEFAULT EVENT #' + IntToStr(LCounter));
        Inc(LCounter);
        Sleep(1000);
      end;
    end
  );
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TChunksResource>;

end.
