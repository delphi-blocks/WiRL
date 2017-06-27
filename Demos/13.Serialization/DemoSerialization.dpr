{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
program DemoSerialization;

uses
  Forms,
  Server.Forms.Main in 'Server.Forms.Main.pas' {MainForm},
  Server.Resources in 'Server.Resources.pas',
  Server.Entities in 'Server.Entities.pas',
  WiRL.Core.MessageBody.Classes in '..\..\Source\Core\WiRL.Core.MessageBody.Classes.pas',
  WiRL.Core.Application.Worker in '..\..\Source\Core\WiRL.Core.Application.Worker.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.