program DemoAuthorizationClient;

uses
  Vcl.Forms,
  Client.Form.Main in 'Client.Form.Main.pas' {frmClientMain},
  Client.Filters in 'Client.Filters.pas',
  Common.Entities in 'Common.Entities.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmClientMain, frmClientMain);
  Application.Run;
end.
