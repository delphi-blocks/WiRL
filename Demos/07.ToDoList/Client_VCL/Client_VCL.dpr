(*
  Copyright 2015, WiRL - REST Library

  Home: https://github.com/WiRL-library

*)
program Client_VCL;

uses
  Vcl.Forms,
  Forms.Main in 'Forms.Main.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
