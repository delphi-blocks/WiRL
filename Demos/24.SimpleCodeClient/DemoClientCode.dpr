{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2026 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
program DemoClientCode;

uses
  System.StartUpCopy,
  FMX.Forms,
  Demo.Form.Main in 'Demo.Form.Main.pas' {frmMain},
  Demo.Resources in 'Demo.Resources.pas',
  Demo.Entities in 'Demo.Entities.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
