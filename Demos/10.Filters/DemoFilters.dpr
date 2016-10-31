(*
  Copyright 2015, WiRL - REST Library

  Home: https://github.com/WiRL-library

*)
program DemoFilters;

uses
  Forms,
  Server.Forms.Main in 'Server.Forms.Main.pas' {MainForm},
  Server.Resources in 'Server.Resources.pas',
  Xml.xmldom,
  XML.OmniXMLDom,
  Server.Filters in 'Server.Filters.pas',
  Server.Filters.Attributes in 'Server.Filters.Attributes.pas';

{$R *.res}

begin
  DefaultDOMVendor := sOmniXmlVendor;

  ReportMemoryLeaksOnShutdown := False;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
