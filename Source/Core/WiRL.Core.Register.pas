unit WiRL.Core.Register;

interface

uses
  System.SysUtils, System.Classes,
  WiRL.Core.Engine,
  WiRL.http.Engines,
  WiRL.http.FileSystemEngine,
  WiRL.http.Server,
  WiRL.http.Server.Indy;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('WiRL Server', [TWiRLEngine]);
  RegisterComponents('WiRL Server', [TWiRLhttpEngine]);
  RegisterComponents('WiRL Server', [TWiRLServer]);
  RegisterComponents('WiRL Server', [TWiRLFileSystemEngine]);
end;

end.
