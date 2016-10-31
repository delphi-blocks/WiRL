(*
  Copyright 2015, WiRL - REST Library

  Home: https://github.com/WiRL-library

*)
program FireDACBasicServer;

uses
  Vcl.Forms,
  Server.Forms.Main in 'Server.Forms.Main.pas' {MainForm},
  Server.MainData in 'Server.MainData.pas' {MainDataResource: TDataModule},
  WiRL.Data.FireDAC.DataModule in '..\..\Source\Data\FireDAC\WiRL.Data.FireDAC.DataModule.pas' {WiRLFDDataModuleResource: TDataModule};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := False;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
