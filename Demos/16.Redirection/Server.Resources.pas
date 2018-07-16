{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2018 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Resources;

{$I WiRL.inc}

interface

uses
  System.SysUtils, System.Classes,

  WiRL.Core.Attributes,
  WiRL.Core.MessageBody.Default,
  WiRL.http.Core,
  WiRL.http.Accept.MediaType;

type
  [Path('demo_resource')]
  TDemoResource = class
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    [ResponseRedirection(TWiRLHttpStatus.MOVED_PERMANENTLY, '/rest/app/demo_resource/destination')]
    function DemoRedirectMethod: string;

    [POST, Produces(TMediaType.TEXT_PLAIN)]
    [ResponseStatus(TWiRLHttpStatus.CREATED)]
    function InsertEntity: string;

    [GET, Path('destination'), Produces(TMediaType.TEXT_PLAIN)]
    function DemoDestinationMethod: string;
  end;

implementation

uses
  System.IOUtils,
  WiRL.Core.Registry;

function TDemoResource.InsertEntity: string;
begin
  Result := 'Entity created (Status 201)';
end;

function TDemoResource.DemoDestinationMethod: string;
begin
  Result := 'New endpoint'
end;

function TDemoResource.DemoRedirectMethod: string;
begin
  Result := 'Old endpoint (redirect to new)';
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TDemoResource>;

end.
