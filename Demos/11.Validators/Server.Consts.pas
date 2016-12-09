unit Server.Consts;

interface

// This unit is only an example of how you can
// use resource string as message errors inside
// ReST validations

uses
  Generics.Collections;

type
  TResItem = record
    StringRec: PResStringRec;
    Value: string;
  end;

  TStringResourceDictionary = class(TDictionary<string, TResItem>)
  public
    function GetString(const Name: string): string;
    procedure AddString(const Name: string; StringRec: PResStringRec);
  end;

var
  StringResources :TStringResourceDictionary;

const
  sMaxErrorName = 'resource.sMaxError';

resourcestring
  sMaxError = 'Max value exceded';

implementation

procedure LoadResources;
begin
  StringResources.AddString(sMaxErrorName, PResStringRec(@sMaxError));
end;

{ TStringResourceDictionary }

procedure TStringResourceDictionary.AddString(const Name: string;
  StringRec: PResStringRec);
var
  ResItem: TResItem;
begin
  ResItem.StringRec := StringRec;
  Self.Add(Name, ResItem);
end;

function TStringResourceDictionary.GetString(const Name: string): string;
var
  ResItem: TResItem;
begin
  ResItem := Self[Name];
  if ResItem.Value <> '' then
    Exit(ResItem.Value);

  if Assigned(ResItem.StringRec) then
    ResItem.Value := LoadResString(ResItem.StringRec)
  else
    ResItem.Value := Name;
  Result := ResItem.Value;
end;

initialization

  StringResources := TStringResourceDictionary.Create;
  LoadResources;

finalization

  StringResources.Free;

end.
