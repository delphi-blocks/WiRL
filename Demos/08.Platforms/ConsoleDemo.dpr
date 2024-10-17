program ConsoleDemo;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Server.Listener in 'Server.Listener.pas',
  Server.Resources in 'Server.Resources.pas',
  Server.Filters in 'Server.Filters.pas',
  Server.Console in 'Server.Console.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  TConsole.Run;
end.
