unit ServerResources;

interface

uses
  WiRL.Core.Attributes,
  WiRL.http.Accept.MediaType,
  SysUtils,
  Classes;

type
  [Path('helloworld')]
  THelloWorldResource = class
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
