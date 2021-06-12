{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
program DemoSwagger;

uses
  Forms,
  Server.Forms.Main in 'Server.Forms.Main.pas' {MainForm},
  Server.Entities in 'Server.Entities.pas',
  Server.Resources.Demo in 'Server.Resources.Demo.pas',
  Server.Resources.Swagger in 'Server.Resources.Swagger.pas',
  WiRL.Core.Metadata in '..\..\Source\Core\WiRL.Core.Metadata.pas',
  WiRL.Core.Application.Worker in '..\..\Source\Core\WiRL.Core.Application.Worker.pas',
  WiRL.Core.Metadata.XMLDoc in '..\..\Source\Core\WiRL.Core.Metadata.XMLDoc.pas',
  WiRL.Core.Application in '..\..\Source\Core\WiRL.Core.Application.pas',
  WiRL.Core.OpenAPI in '..\..\Source\Core\WiRL.Core.OpenAPI.pas',
  Server.Resources.Customer in 'Server.Resources.Customer.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
