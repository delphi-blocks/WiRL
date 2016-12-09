(*
  Copyright 2015, WiRL - REST Library

  Home: https://github.com/WiRL-library

*)
program DemoValidators;

uses
  Forms,
  Server.Forms.Main in 'Server.Forms.Main.pas' {MainForm},
  Server.Resources in 'Server.Resources.pas',
  Xml.xmldom,
  XML.OmniXMLDom,
  Server.Validators in 'Server.Validators.pas',
  WiRL.Core.Validators in '..\..\Source\Core\WiRL.Core.Validators.pas',
  Server.Consts in 'Server.Consts.pas';

{$R *.res}

begin
  DefaultDOMVendor := sOmniXmlVendor;

  ReportMemoryLeaksOnShutdown := False;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
