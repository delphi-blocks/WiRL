program DemoHelloWorldWebBroker;
{$APPTYPE GUI}

uses
  Vcl.Forms,
  Web.WebReq,
  IdHTTPWebBrokerBridge,
  Server.Forms.WebBrokerMain in 'Server.Forms.WebBrokerMain.pas' {Form1},
  Server.WebModules.Main in 'Server.WebModules.Main.pas' {MainWebModule: TWebModule},
  Demo.Entities in 'Demo.Entities.pas',
  Server.Resources.Database in 'Server.Resources.Database.pas',
  Server.Resources.HelloWorld in 'Server.Resources.HelloWorld.pas',
  Server.Resources.Params in 'Server.Resources.Params.pas';

{$R *.res}

begin
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
