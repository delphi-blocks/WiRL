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
  private class var
    FRunning: Boolean;
    FProcessID: pid_t;
    FSignalProc: TSignalProc;
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
    class procedure Setup(ASignalProc: TSignalProc);

    /// <summary>
    ///   Infinte console loop
    /// </summary>
    class procedure Run(AInterval: Integer); static;

    /// <summary>
    ///   Log Info message
    /// </summary>
    class procedure LogInfo(const AMessage: string); static;

    /// <summary>
    ///   Log Warning message
    /// </summary>
    class procedure LogWarning(const AMessage: string); static;

    /// <summary>
    ///   Log Error message
    /// </summary>
    class procedure LogError(const AMessage: string); static;

    /// <summary>
    ///   Log Info Raw message
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
      TPosixDaemon.LogInfo('Signal SIGTERM received');
      TPosixDaemon.FSignalProc(TPosixSignal.Termination);
      TPosixDaemon.FRunning := False;
    end;
    SIGHUP:
    begin
      TPosixDaemon.LogInfo('Signal SIGHUP received');
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

class procedure TPosixDaemon.LogError(const AMessage: string);
begin
  syslog(LOG_ERR, AMessage);
end;

class procedure TPosixDaemon.LogInfo(const AMessage: string);
begin
  syslog(LOG_INFO, AMessage);
end;

class procedure TPosixDaemon.LogWarning(const AMessage: string);
begin
  syslog(LOG_WARNING, AMessage);
end;

class procedure TPosixDaemon.LogRaw(const AMessage: string);
begin
  syslog(LOG_INFO, AMessage);
end;

class procedure TPosixDaemon.Run(AInterval: Integer);
begin
  FRunning := True;
  while FRunning do
  begin
    Sleep(AInterval);
  end;
  LogInfo('The daemon is finally killed!');
end;

class procedure TPosixDaemon.Setup(ASignalProc: TSignalProc);
begin
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
