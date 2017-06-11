program DemoHelloWorldConsole;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Types,
  WiRL.Console.Base,
  WiRL.Console.Factory,
  WiRL.http.Server.Indy,
  Server.Resources in 'Server.Resources.pas';

var
  WiRLConsole: TWiRLConsoleBase;

begin
  try
    WiRLConsole := TWiRLConsoleFactory.NewConsole(
      procedure (AServer: TWiRLhttpServerIndy)
      begin
        AServer.ConfigureEngine('/rest')
          .SetName('WiRL HelloWorld')
          .SetPort(8080)
          .SetThreadPoolSize(10)

          // Adds and configures an application
          .AddApplication('/app')
          {$IF CompilerVersion >=28} //XE7
            .SetResources([
              'Server.Resources.THelloWorldResource',
              'Server.Resources.TEntityResource'
            ]);
          {$ELSE}
            .SetResources(
              'Server.Resources.THelloWorldResource,'+
              'Server.Resources.TEntityResource'
            );
          {$IFEND}
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
      TWiRLConsoleFactory.LogLn('Exception: ' + E.Message);
    end;
  end;
end.
