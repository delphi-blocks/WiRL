program DeamonDemo;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Server.Listener in 'Server.Listener.pas',
  Server.Resources in 'Server.Resources.pas',
  Server.Filters in 'Server.Filters.pas',
  Server.Daemon in 'Server.Daemon.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  TDaemon.Run;
end.
