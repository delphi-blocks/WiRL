{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2025 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
program PetStoreAPI;

uses
  Forms,
  PetStore.Forms.Main in 'PetStore.Forms.Main.pas' {MainForm},
  PetStore.Entities in 'PetStore.Entities.pas',
  PetStore.Resources in 'PetStore.Resources.pas',
  WiRL.Core.Metadata in '..\..\..\Source\Core\WiRL.Core.Metadata.pas',
  WiRL.Core.Application.Worker in '..\..\..\Source\Core\WiRL.Core.Application.Worker.pas',
  WiRL.Core.Metadata.XMLDoc in '..\..\..\Source\Core\WiRL.Core.Metadata.XMLDoc.pas',
  WiRL.Core.Application in '..\..\..\Source\Core\WiRL.Core.Application.pas',
  WiRL.Core.OpenAPI in '..\..\..\Source\Core\WiRL.Core.OpenAPI.pas',
  WiRL.Core.Metadata.OASDoc in '..\..\..\Source\Core\WiRL.Core.Metadata.OASDoc.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
