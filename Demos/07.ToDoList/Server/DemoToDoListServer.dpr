(*
  Copyright 2015, WiRL - REST Library

  Home: https://github.com/WiRL-library

*)
program DemoToDoListServer;

uses
  Forms,
  Server.Forms.Main in 'Server.Forms.Main.pas' {MainForm},
  Server.Resources in 'Server.Resources.pas',
  Model in '..\Common\Model.pas',
  Server.Writers in 'Server.Writers.pas',
  WiRL.Data.FireDAC.DataModule in '..\..\..\Source\Data\FireDAC\WiRL.Data.FireDAC.DataModule.pas' {WiRLFDDataModuleResource: TDataModule},
  Server.Resources.Datamodule in 'Server.Resources.Datamodule.pas' {DataResource: TDataModule},
  Model.Persistence.FDAC in '..\Common\Model.Persistence.FDAC.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := False;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
