{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
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
  Posix.Fcntl,
  System.IOUtils,
  WiRL.Console.Types,
  WiRL.Console.Posix.Syslog;

  /// <summary>
  ///   global procedure (cdecl) to handle signals
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
    ///   Missing from linux/StdlibTypes.inc !!! <stdlib.h>
    /// </summary>
    EXIT_SUCCESS = 0;
    EXIT_FAILURE = 1;
    ROOT_DIR = '/';
  private
    /// <summary>
    ///   Fork a Posix process
    /// </summary>
    /// <remarks>
    ///   Do not create (and run) any thread before forking a process
    /// </remarks>
    class function ForkProcess: pid_t; static;

    /// <summary>
    ///   Close all opened file descriptors
    /// </summary>
    class procedure CloseDescriptors; static;

    /// <summary>
    ///   Redirect stdin, stdout, stderr to /dev/null
    /// </summary>
    class procedure RouteDescriptors; static;

    /// <summary>
    ///   Catch, ignore and handle signals
    /// </summary>
    class procedure CatchHandleSignals; static;

    /// <summary>
    ///   Kill the first process and leave only the daemon
    /// </summary>
    class procedure KillParent(APID: pid_t); static;

    /// <summary>
    ///   Detach from any terminal and create an independent session.
    /// </summary>
    class procedure DetachTerminal; static;

    /// <summary>
    ///   Set new file permissions: Restrict file creation mode to 750
    /// </summary>
    class procedure SetRootDirectory(const ADirectory: string); static;

    /// <summary>
    ///   Change the current directory to the root directory (/), in order to
    ///   avoid that the daemon involuntarily blocks mount points from being unmounted
    /// </summary>
    class procedure UserFileMask(AMask: Cardinal); static;

  public
    /// <summary>
    ///   Console setup method, to be called before Run and before starting
    ///   any thread in the application
    /// </summary>
    class procedure Setup(ASignalProc: TSignalProc; const ARedirectTo: string = ROOT_DIR);

    /// <summary>
    ///   Infinte console loop
    /// </summary>
    class procedure Run(AInterval: Integer); static;

    /// <summary>
    ///   Stop the daemon
    /// </summary>
    class procedure Stop; static;

    /// <summary>
    ///   Open the log connection
    /// </summary>
    class procedure LogOpen; static;

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
    ///   Close the log connection
    /// </summary>
    class procedure LogClose; static;

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
  LRet: Integer;
begin
  // Closing descriptor stdin (standard input)
  LRet := __close(0);
  if LRet < 0 then
    EPosixException.Create('Error closing stdin');

  // Closing descriptor stdout (standard output)
  LRet := __close(1);
  if LRet < 0 then
    EPosixException.Create('Error closing stdout');

  // Closing descriptor stderr (standard error)
  LRet := __close(2);
  if LRet < 0 then
    EPosixException.Create('Error closing stderr');
end;

class procedure TPosixDaemon.DetachTerminal;
begin
  if setsid() < 0 then
    raise EPosixException.Create('Impossible to create an independent session');
end;

class function TPosixDaemon.ForkProcess: pid_t;
begin
  Result := fork();
  if Result < 0 then
    raise EPosixException.Create('Cannot create the fork');
end;

class procedure TPosixDaemon.CatchHandleSignals;
begin
  signal(SIGCHLD, TSignalHandler(SIG_IGN));
  signal(SIGHUP, HandleSignals);
  signal(SIGTERM, HandleSignals);
end;

class procedure TPosixDaemon.KillParent(APID: pid_t);
begin
  // If the PID returned from fork() is valid, the parent process
  // must terminate gracefully
  if APID > 0 then
    Halt(TPosixDaemon.EXIT_SUCCESS);
end;

class procedure TPosixDaemon.LogClose;
begin
  closelog;
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

class procedure TPosixDaemon.LogOpen;
begin
  // syslog() will call LogOpen() with no arguments if the log is not currently open.
  openlog(nil, LOG_PID or LOG_NDELAY, LOG_DAEMON);
end;

class procedure TPosixDaemon.LogRaw(const AMessage: string);
begin
  syslog(LOG_INFO, AMessage);
end;

class procedure TPosixDaemon.RouteDescriptors;
var
  LFileID: Integer;
begin
  // Open STDIN
  LFileID := __open('/dev/null', O_RDWR);

  // Dup STDOUT
  dup(LFileID);

  // Dup STDERR
  dup(LFileID);
end;

class procedure TPosixDaemon.Run(AInterval: Integer);
begin
  FRunning := True;
  while FRunning do
  begin
    Sleep(AInterval);
  end;
  LogInfo('The daemon is finally killed!');
  LogClose;
end;

class procedure TPosixDaemon.Setup(ASignalProc: TSignalProc; const ARedirectTo: string);
begin
  FSignalProc := ASignalProc;

  LogOpen;

  FProcessID := ForkProcess;
  KillParent(FProcessID);

  DetachTerminal;

  FProcessID := ForkProcess;
  KillParent(FProcessID);

  CatchHandleSignals;

  CloseDescriptors;
  RouteDescriptors;

  UserFileMask(027);
  
  if not ARedirectTo.IsEmpty then
    SetRootDirectory(ARedirectTo);
end;

class procedure TPosixDaemon.Stop;
begin
  FRunning := False;
end;

class procedure TPosixDaemon.UserFileMask(AMask: Cardinal);
begin
  umask(AMask);
end;

class procedure TPosixDaemon.SetRootDirectory(const ADirectory: string);
begin
  ChDir(ADirectory);
end;

end.
