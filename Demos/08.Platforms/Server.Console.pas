unit Server.Console;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs,
  Server.Listener, Server.Filters;

type
  IRunnable = interface
    ['{6BC5CA65-8B6B-4042-9F4C-2B0EC7D6BE26}']
    procedure Execute;
  end;

  TConsole = class(TInterfacedObject, ILogger, IRunnable)
  private
    { IRunnable }
    procedure Execute;

    { ILogger }
    procedure Log(const AMessage: string);
  public
    class procedure Run; static;
  end;

implementation

{ TConsole }

procedure TConsole.Execute;
var
  LListener: TListener;
begin
  RegisterLogger(Self);

  LListener := TListener.Create;
  try
    LListener.Start;

    Writeln(LListener.Name);
    Writeln(' _ _ _  _  ___  _     ');
    Writeln('| | | |<_>| . \| |    ');
    Writeln('| | | || ||   /| |_   ');
    Writeln('|__/_/ |_||_\_\|___|  ');
    Writeln(LListener.DisplayName);
    Writeln(Format('http://localhost:%d/rest/app/demo', [LListener.Port]));
    Writeln('Press [ENTER] to exit');
    Writeln('');
    Readln;
    LListener.Stop;
  finally
    LListener.Free;
  end;
  UnregisterLogger(Self);
end;

procedure TConsole.Log(const AMessage: string);
begin
   TMonitor.Enter(Self);
   try
      Writeln(AMessage);
   finally
     TMonitor.Exit(Self);
   end;
end;

class procedure TConsole.Run;
var
  LRunnable: IRunnable;
begin
  try
    LRunnable := TConsole.Create;
    LRunnable.Execute;
  except
    on E: Exception do
      Writeln('Error: ' + E.Message);
  end;
end;

end.
