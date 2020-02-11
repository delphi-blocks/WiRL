unit Server.Entities;

interface

type
  TSimpleClass = class
  private
    FName: string;
    FDevLanguage: string;
    FDevSince: Integer;
    FLastName: string;
  public
    property Name: string read FName write FName;
    property LastName: string read FLastName write FLastName;
    property DevLanguage: string read FDevLanguage write FDevLanguage;
    property DevSince: Integer read FDevSince write FDevSince;
  public
    class function RandomEntity: TSimpleClass;
  end;

implementation

{ TSimpleClass }

class function TSimpleClass.RandomEntity: TSimpleClass;
const
  FirstNames: array of string = ['Paolo', 'Luca', 'Marco', 'Maurizio', 'Fabio', 'Omar', 'Carlo', 'Thomas', 'Antonio'];
  LastNames: array of string  = ['Rossi', 'Minuti', 'Cantù', 'Del Magno', 'Codebue', 'Bossoni', 'Narcisi', 'Ranzetti', 'Polito'];
  DevLangs: array of string  = ['Delphi', 'VB', 'Clipper', 'C#', 'COBOL', 'ASM', 'Pascal', 'Java'];
begin
  Result := Self.Create;
  Result.Name := FirstNames[Random(High(FirstNames)-1)];
  Result.LastName := LastNames[Random(High(LastNames)-1)];
  Result.DevLanguage := DevLangs[Random(High(DevLangs)-1)];
  Result.DevSince := Random(20);
end;

initialization
  Randomize();

end.
