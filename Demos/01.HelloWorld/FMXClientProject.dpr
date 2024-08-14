{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
program FMXClientProject;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMXClient.Forms.Main in 'FMXClient.Forms.Main.pas' {MainForm},
  FMXClient.DataModules.Main in 'FMXClient.DataModules.Main.pas' {MainDataModule: TDataModule},
  Demo.Entities in 'Demo.Entities.pas',
  FMXClient.DataModules.Database in 'FMXClient.DataModules.Database.pas' {DatabaseModule: TDataModule},
  FMXClient.DataModules.Params in 'FMXClient.DataModules.Params.pas' {ParamsModule: TDataModule};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TMainDataModule, MainDataModule);
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TDatabaseModule, DatabaseModule);
  Application.CreateForm(TParamsModule, ParamsModule);
  Application.Run;
end.
