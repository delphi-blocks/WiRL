unit WiRL.Wizards.Modules.Classes;

interface

uses
  System.SysUtils, System.Classes;

type
  TServerConfig = record
    ServerPort: Integer;
    EnginePath: string;
    AppPath: string;
    UseDefaultMessageBody: Boolean;
  end;

implementation

end.
