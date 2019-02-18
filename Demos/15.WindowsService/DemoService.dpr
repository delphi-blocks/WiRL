{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
program DemoService;

uses
  Vcl.SvcMgr,
  Server.Module.Service in 'Server.Module.Service.pas' {WiRLService: TService},
  Server.REST.Resources in 'Server.REST.Resources.pas',
  Server.REST.Entities in 'Server.REST.Entities.pas';

{$R *.RES}

begin
  // Windows 2003 Server requires StartServiceCtrlDispatcher to be
  // called before CoRegisterClassObject, which can be called indirectly
  // by Application.Initialize. TServiceApplication.DelayInitialize allows
  // Application.Initialize to be called from TService.Main (after
  // StartServiceCtrlDispatcher has been called).
  //
  // Delayed initialization of the Application object may affect
  // events which then occur prior to initialization, such as
  // TService.OnCreate. It is only recommended if the ServiceApplication
  // registers a class object with OLE and is intended for use with
  // Windows 2003 Server.
  //
  // Application.DelayInitialize := True;
  //

  //ReportMemoryLeaksOnShutdown := True;
  if not Application.DelayInitialize or Application.Installing then
    Application.Initialize;
  Application.CreateForm(TWiRLService, WiRLService);
  Application.Run;

end.
