{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
program DemoHTMX;

uses
  Forms,
  Server.Forms.Main in 'Server.Forms.Main.pas' {MainForm},
  Server.Resources.Data in 'Server.Resources.Data.pas' {EmployeeModule: TDataModule},
  Server.Database.Builder in 'Server.Database.Builder.pas' {DatabaseBuilder: TDataModule},
  Server.MessageBody.WebStencils in 'Server.MessageBody.WebStencils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
