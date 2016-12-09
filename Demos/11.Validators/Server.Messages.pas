unit Server.Messages;

interface

uses
  Generics.Collections;

var
  NamedResources :TDictionary<string, PResStringRec>;

const
  sMaxErrorName = 'resource.sMaxError';

resourcestring
  sMaxError = 'Max valid';

implementation

procedure LoadResources;
begin
  NamedResources.Add(sMaxErrorName, PResStringRec(@sMaxError));
end;

initialization

  NamedResources := TDictionary<string, PResStringRec>.Create;
  LoadResources;

finalization

  NamedResources.Free;

end.
