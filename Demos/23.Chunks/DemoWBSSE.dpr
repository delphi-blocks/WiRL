program DemoWBSSE;
{$APPTYPE GUI}

uses
  Vcl.Forms,
  Web.WebReq,
  IdHTTPWebBrokerBridge,
  Server.Forms.WBMain in 'Server.Forms.WBMain.pas' {Form6},
  Server.WebModule in 'Server.WebModule.pas' {WebModule2: TWebModule},
  Server.Resources in 'Server.Resources.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;
  Application.Initialize;
  Application.CreateForm(TForm6, Form6);
  Application.Run;
end.
