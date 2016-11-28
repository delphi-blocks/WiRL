(*
  Copyright 2015, WiRL - REST Library

  Home: https://github.com/WiRL-library

*)
program DemoContentTypes;

uses
  Forms,
  Server.Forms.Main in 'Server.Forms.Main.pas' {MainForm},
  Server.Resources in 'Server.Resources.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := False;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.