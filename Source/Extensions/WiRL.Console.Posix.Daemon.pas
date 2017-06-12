{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Console.Posix.Daemon;

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils,
  Posix.Stdlib,
  Posix.SysStat,
  Posix.SysTypes,
  Posix.Unistd,
  Posix.Signal,
  System.IOUtils,
  WiRL.Console.Posix.Syslog;

  /// <summary>
  ///   cdecl procedure to handle signals
  /// </summary>
  procedure HandleSignals(ASigNum: Integer); cdecl;

type
  { TODO -opaolo -c : supports more signals 12/06/2017 11:43:40 }
  TPosixSignal = (Termination, Reload);

  /// <summary>
  ///   Anonymous method signature
  /// </summary>
  TSignalProc = reference to procedure (ASignal: TPosixSignal);

  /// <summary>
  ///   Class to define and run a Posix daemon. For more information please
  ///   read the Linux manpages: man 7 daemon
  /// </summary>
  TPosixDaemon = class
  private const
    LOG_PATH = '/var/tmp/';
  private class var
    FRunning: Boolean;
    FProcessID: pid_t;
    FSignalProc: TSignalProc;
    FLogFile: string;
  public const
    /// <summary>
    ///   Missing from linux/StdlibTypes.inc !!!!
    /// </summary>
    EXIT_SUCCESS = 0;
    EXIT_FAILURE = 1;
  private
    /// <summary>
    ///   Fork a Posix process
    /// </summary>
    /// <remarks>
    ///   Do not create (and run) any thread before forking a process
    /// </remarks>
    class procedure ForkProcess; static;

    /// <summary>
    ///   Close all opened file descriptors
    /// </summary>
    class procedure CloseDescriptors; static;

    /// <summary>
    ///   Catch, ignore and handle signals
    /// </summary>
    class procedure CatchHandleSignals; static;

    /// <summary>
    ///   Kill the first process and leave only the daemon
    /// </summary>
    class procedure KillParent; static;

    /// <summary>
    ///   Detach from any terminal and create an independent session.
    /// </summary>
    class procedure DetachTerminal; static;

    /// <summary>
    ///   Change the current directory to the root directory (/), in order to
    ///   avoid that the daemon involuntarily blocks mount points from being unmounted
    /// </summary>
    class procedure SetRootDirectory; static;

  public
    /// <summary>
    ///   Console setup method, to be called before Run and before starting
    ///   any thread in the application
    /// </summary>
    class procedure Setup(const ALogFile: string; ASignalProc: TSignalProc);

    /// <summary>
    ///   Infinte console loop
    /// </summary>
    class procedure Run(AInterval: Integer); static;

    /// <summary>
    ///   Log method. DateTime and no new line appended
    /// </summary>
    class procedure Log(const AMessage: string); static;

    /// <summary>
    ///   Log method. DateTime and new line appended
    /// </summary>
    class procedure LogLn(const AMessage: string); static;

    /// <summary>
    ///   Log method. Raw content and no new line appended
    /// </summary>
    class procedure LogRaw(const AMessage: string); static;

    /// <summary>
    ///   Posix Process ID (pid)
    /// </summary>
    class property ProcessID: pid_t read FProcessID write FProcessID;
  end;

implementation

procedure HandleSignals(ASigNum: Integer);
begin
  case ASigNum of
    SIGTERM:
    begin
      TPosixDaemon.LogLn('Signal SIGTERM received');
      TPosixDaemon.FSignalProc(TPosixSignal.Termination);
      //Halt(TPosixDaemon.EXIT_SUCCESS);
      TPosixDaemon.FRunning := False;
    end;
    SIGHUP:
    begin
      TPosixDaemon.LogLn('Signal SIGHUP received');
      TPosixDaemon.FSignalProc(TPosixSignal.Reload);
    end;
  end;
end;

{ TPosixDaemon }

class procedure TPosixDaemon.CloseDescriptors;
var
  LIndex: Integer;
begin
  // TODO:   connect /dev/null to standard input, output, and error
  for LIndex := sysconf(_SC_OPEN_MAX) downto 0 do
    __close(LIndex);
end;

class procedure TPosixDaemon.DetachTerminal;
begin
  if setsid() < 0 then
    raise Exception.Create('Impossible to create an independent session');
end;

class procedure TPosixDaemon.ForkProcess;
begin
  FProcessID := fork();
  if FProcessID < 0 then
    raise Exception.Create('Cannot create the fork');
end;

class procedure TPosixDaemon.CatchHandleSignals;
begin
  signal(SIGCHLD, TSignalHandler(SIG_IGN));
  signal(SIGHUP, HandleSignals);
  signal(SIGTERM, HandleSignals);
end;

class procedure TPosixDaemon.KillParent;
begin
  // Call exit() in the first child, so that only the second
  // child (the actual daemon process) stays around
  if FProcessID <> 0 then
    Halt(TPosixDaemon.EXIT_SUCCESS);
end;

class procedure TPosixDaemon.Log(const AMessage: string);
begin
  LogRaw(DateTimeToStr(Now) + ' - ' + AMessage);
end;

class procedure TPosixDaemon.LogLn(const AMessage: string);
begin
  LogRaw(DateTimeToStr(Now) + ' - ' + AMessage + sLineBreak);
end;

class procedure TPosixDaemon.LogRaw(const AMessage: string);
begin
  // Best write on /var/log/ but we haven't privileges
  // Could be using syslog but there is no header (yet) in Delphi
  // you can find some translation here: https://svn.freepascal.org/svn/fpc/trunk/packages/syslog/src/systemlog.pp
  TFile.AppendAllText(TPosixDaemon.FLogFile, AMessage);
end;

class procedure TPosixDaemon.Run(AInterval: Integer);
begin
  FRunning := True;
  while FRunning do
  begin
    Sleep(AInterval);
  end;
  LogLn('The daemon is finally killed!');
end;

class procedure TPosixDaemon.Setup(const ALogFile: string;
  ASignalProc: TSignalProc);
begin
  FLogFile := LOG_PATH + ALogFile + '.log';
  FSignalProc := ASignalProc;

  ForkProcess;
  KillParent;
  DetachTerminal;
  ForkProcess;
  KillParent;
  CatchHandleSignals;
  CloseDescriptors;
  SetRootDirectory;
end;

class procedure TPosixDaemon.SetRootDirectory;
begin
  ChDir('/');
end;

end.
