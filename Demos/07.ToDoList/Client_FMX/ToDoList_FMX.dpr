(*
  Copyright 2015, WiRL - REST Library

  Home: https://github.com/WiRL-library

*)
program ToDoList_FMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  Forms.Main in 'Forms.Main.pas' {MainForm},
  Data.ToDo in 'Data.ToDo.pas' {TodoDM: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TTodoDM, TodoDM);
  Application.Run;
end.
