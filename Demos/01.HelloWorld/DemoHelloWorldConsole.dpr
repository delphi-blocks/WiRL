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
  WiRL.Core.Engine,
  WiRL.http.Server,
  WiRL.http.Server.Indy,
  Server.Resources in 'Server.Resources.pas';

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
          .AddEngine<TWiRLEngine>('/rest')
          .SetEngineName('WiRL HelloWorld')

          // Adds and configures an application
          .AddApplication('/app')
            .SetResources([
              'Server.Resources.THelloWorldResource',
              'Server.Resources.TEntityResource'])
        ;
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
