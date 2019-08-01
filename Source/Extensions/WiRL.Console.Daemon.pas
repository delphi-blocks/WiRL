{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
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
    class procedure LogInfo(const AMessage: string); override;
    class procedure LogWarning(const AMessage: string); override;
    class procedure LogError(const AMessage: string); override;
    class procedure LogRaw(const AMessage: string); override;
  end;

implementation

{ TWiRLConsoleDaemon }

procedure TWiRLConsoleDaemon.ConsoleSetup;
var
  LAppName: string;
begin
  LAppName := ExtractFileName(ParamStr(0));

  TPosixDaemon.Setup(
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
          //ServerSetup;
          ServerStart;
        end;
      end;
    end
  );

  ConsoleHelp;
end;

procedure TWiRLConsoleDaemon.ConsoleStart;
begin
  TPosixDaemon.LogInfo('WiRL Daemon is running...');
  TPosixDaemon.Run(1000);
end;

procedure TWiRLConsoleDaemon.ConsoleHelp;
begin
  LogInfo(TWiRLConsoleDef.Logo);
  LogInfo(TWiRLConsoleDef.OSVer + OSVersion);
end;

class procedure TWiRLConsoleDaemon.LogError(const AMessage: string);
begin
  TPosixDaemon.LogError(AMessage);
end;

class procedure TWiRLConsoleDaemon.LogInfo(const AMessage: string);
begin
  TPosixDaemon.LogInfo(AMessage);
end;

class procedure TWiRLConsoleDaemon.LogRaw(const AMessage: string);
begin
  TPosixDaemon.LogRaw(AMessage);
end;

class procedure TWiRLConsoleDaemon.LogWarning(const AMessage: string);
begin
  TPosixDaemon.LogWarning(AMessage);
end;

end.
