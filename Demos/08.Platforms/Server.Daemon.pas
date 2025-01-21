unit Server.Daemon;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils,

  WiRL.Console.Posix.Daemon,
  WiRL.Console.Posix.Syslog,

  Server.Filters;

type
  IRunnable = interface
    ['{50C8862B-6918-4D03-B8CE-74D070F628B4}']
    procedure Execute;
  end;

  TDaemon = class(TInterfacedObject, IRunnable, ILogger)
  private
    { IRunnable }
    procedure Execute;

    { ILogger }
    procedure Log(const AMessage: string);
  public
    class procedure Run; static;
  end;

implementation

{ TDaemon }

uses
  Server.Listener;

procedure TDaemon.Execute;
var
  LListener: TListener;
begin
  RegisterLogger(Self);

  LListener := TListener.Create;
  try

    TPosixDaemon.Setup(
      procedure (ASignal: TPosixSignal)
      begin
        case ASignal of
          TPosixSignal.Termination:
          begin
            LListener.Stop;
          end;

          TPosixSignal.Reload:
          begin
            // Reload configuration
            LListener.Port := 8080;
          end;
        end;
      end
    );
    LListener.Start;
    TPosixDaemon.Run(1000);
  finally
    LListener.Free;
  end;
  UnregisterLogger(Self);
end;

procedure TDaemon.Log(const AMessage: string);
var
  LLogPath: string;
begin
  LLogPath := TPath.Combine(ExtractFileDir(ParamStr(0)), 'wirl-demo.log');

  TMonitor.Enter(Self);
  try
    TFile.AppendAllText(LLogPath, AMessage + sLineBreak, TEncoding.UTF8);
  finally
    TMonitor.Exit(Self);
  end;
end;

class procedure TDaemon.Run;
var
  LRunnable: IRunnable;
begin
  LRunnable := TDaemon.Create;
  LRunnable.Execute;
end;

end.
