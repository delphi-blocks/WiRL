{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Console.Daemon;

interface

uses
  System.SysUtils,
  WiRl.Console.Base,
  WiRL.Console.Posix.Daemon;

type
  TWiRLConsoleDaemon = class(TWiRLConsoleBase)
  protected
    procedure ConsoleSetup; override;
    procedure ConsoleStart; override;
    procedure ConsoleHelp; override;
  public
    class procedure Log(const AMessage: string); override;
    class procedure LogLn(const AMessage: string); override;
    class procedure LogRaw(const AMessage: string); override;
  end;

implementation

procedure TWiRLConsoleDaemon.ConsoleSetup;
var
  LAppName: string;
begin
  LAppName := ExtractFileName(ParamStr(0));

  TPosixDaemon.Setup(LAppName,
    procedure (ASignal: TPosixSignal)
    begin
      case ASignal of
        TPosixSignal.Termination:
        begin
          ServerStop;
        end;

        TPosixSignal.Reload:
        begin
          ServerStop;
          ServerSetup;
          ServerStart;
        end;
      end;
    end
  );

  ConsoleHelp;
end;

procedure TWiRLConsoleDaemon.ConsoleStart;
begin
  TPosixDaemon.LogLn('WiRL Daemon is running...');
  while True do
    Sleep(1000);
end;

procedure TWiRLConsoleDaemon.ConsoleHelp;
begin
  LogLn(TWiRLConsoleDef.Logo);
  LogLn(TWiRLConsoleDef.OSVer + OSVersion);
end;

class procedure TWiRLConsoleDaemon.Log(const AMessage: string);
begin
  TPosixDaemon.Log(AMessage);
end;

class procedure TWiRLConsoleDaemon.LogLn(const AMessage: string);
begin
  TPosixDaemon.LogLn(AMessage);
end;

class procedure TWiRLConsoleDaemon.LogRaw(const AMessage: string);
begin
  TPosixDaemon.LogRaw(AMessage);
end;

end.
