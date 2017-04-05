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

  WiRL.Core.Registry,
  WiRL.Core.Attributes,
  WiRL.http.Accept.MediaType,
  WiRL.Core.MessageBodyReaders,
  WiRL.Core.Request,
  WiRL.Core.Validators,

  Server.Context;

type
  [Path('context')]
  TContextDemoResource = class
  private
    [Context] Request: TWiRLRequest;
    [Context][Value(42)] MyClass: TMyClass;
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function SampleText([Context] ParamClass: TMyClass): string;
  end;

implementation

{ TContextDemoResource }

function TContextDemoResource.SampleText(ParamClass: TMyClass): string;
begin
  Result :=
    'Hello context injection!' + sLineBreak + sLineBreak +
    'Request.Host: ' + Request.Host + sLineBreak +
    'MyClass.Info: ' + MyClass.Info + sLineBreak +
    'MyClass.Value: ' + MyClass.Value.ToString;
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TContextDemoResource>;

end.
