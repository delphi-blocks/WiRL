{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2021 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
program DemoHelloWorldConsole;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Types,
  WiRL.Console.Base,
  WiRL.Console.Factory,
  WiRL.Engine.REST,
  WiRL.Engine.WebServer,
  WiRL.http.Server,
  WiRL.http.Server.Indy,
  Server.Resources.HelloWorld in 'Server.Resources.HelloWorld.pas',
  Server.Resources.Database in 'Server.Resources.Database.pas',
  Server.Resources.Params in 'Server.Resources.Params.pas';

var
  WiRLConsole: TWiRLConsoleBase;

begin
  try
    WiRLConsole := TWiRLConsoleFactory.NewConsole(
      procedure (AServer: TWiRLServer)
      begin
        AServer
          .SetPort(8080)
          .SetThreadPoolSize(10)
          .AddEngine<TWiRLRESTEngine>('/rest')
          .SetEngineName('WiRL HelloWorld')

          // Adds and configures an application
          .AddApplication('/app')
            // You can add any single resource (FQCN of the class) or use '*'
            // to add all registered resources
            .SetResources([
              'Server.Resources.HelloWorld.THelloWorldResource',
              'Server.Resources.HelloWorld.TEntityResource',
              'Server.Resources.Database.TDatabaseResource',
              'Server.Resources.Params.TParametersResource'])
        ;

        AServer.AddEngine<TWiRLWebServerEngine>('/')
          .SetEngineName('FileSystemEngine')
          .SetRootFolder('..\..\www');
      end
    );
    try
      WiRLConsole.Start;
    finally
      WiRLConsole.Free;
    end;

    ExitCode := 0;
  except
    on E: Exception do
    begin
      ExitCode := 1;
      TWiRLConsoleLogger.LogError('Exception: ' + E.Message);
    end;
  end;
end.
