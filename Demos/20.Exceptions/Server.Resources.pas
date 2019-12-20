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

  WiRL.Core.Registry,
  WiRL.Core.Attributes,
  WiRL.http.Accept.MediaType,
  WiRL.Core.MessageBody.Default,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.Core.Exceptions,

  Server.Exceptions;

type
  [Path('demo')]
  TDemoResource = class
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function SampleText: string;
    [Path('error')]
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function GetError: string;
  end;

implementation

{ TDemoResource }


{ TDemoResource }

function TDemoResource.GetError: string;
begin
  raise EMyNotFoundException.Create(102, 'Exception Message');
  Result := 'Hello, Error!';
end;

function TDemoResource.SampleText: string;
begin
  Result := 'Hello, World!'
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TDemoResource>;

end.
