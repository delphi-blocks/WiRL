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
    CreateTheFirstResource: Boolean;
  end;

  TResourceConfig = record
    Name: string;
    MethodGET: Boolean;
    MethodPOST: Boolean;
    MethodPUT: Boolean;
    MethodDELETE: Boolean;
  end;

implementation

end.
