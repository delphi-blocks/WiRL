(*
  Copyright 2015, WiRL - REST Library

  Home: https://github.com/WiRL-library

*)
program DemoExtJS;

uses
  Forms,
  Server.Forms.Main in 'Server.Forms.Main.pas' {MainForm},
  Server.Resources.StaticFiles in 'Server.Resources.StaticFiles.pas',
  Server.Resources.Data in 'Server.Resources.Data.pas' {MainModule: TDataModule},
  Server.Database.Builder in 'Server.Database.Builder.pas' {DatabaseBuilder: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
