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
  System.SysUtils, System.Classes,

  WiRL.Core.Attributes,
  WiRL.http.Accept.MediaType,
  WiRL.Core.Request,
  WiRL.Core.Response;

type
  [Path('helloworld')]
  THelloWorldResource = class
  protected
    [Context] Req: TWiRLRequest;
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function SayHelloWorld: string;
  end;

implementation

uses
  WiRL.Core.Registry;

{ THelloWorldResource }

function THelloWorldResource.SayHelloWorld: string;
begin
  Result := 'Hello World!';
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<THelloWorldResource>;

end.
