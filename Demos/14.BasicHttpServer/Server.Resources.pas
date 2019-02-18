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
  System.Classes, System.SysUtils, System.JSON,

  WiRL.Core.Registry,
  WiRL.Core.Attributes,
  WiRL.Core.MessageBody.Default,
  WiRL.http.Accept.MediaType;

type
  [Path('/helloworld')]
  THelloWorldResource = class
  public
    [GET]
    [Produces(TMediaType.TEXT_PLAIN + TMediaType.WITH_CHARSET_UTF8)]
    function HelloWorld(): string;
  end;

implementation

{ THelloWorldResource }

function THelloWorldResource.HelloWorld(): string;
begin
  Result := 'Hello World!';
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<THelloWorldResource>;

end.
