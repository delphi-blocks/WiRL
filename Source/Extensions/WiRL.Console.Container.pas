{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2025 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Console.Container;

interface

uses
  System.SysUtils,
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ELSE}
  Posix.Stdlib,
  Posix.SysStat,
  Posix.SysTypes,
  Posix.Unistd,
  Posix.Signal,
  Posix.Fcntl,
  {$ENDIF}
  WiRl.Console.Base;

type
  TWiRLConsoleContainer = class(TWiRLConsoleBase)
  private
    procedure HandleSigTerm;
  protected
    procedure WriteStatus; override;
    procedure ServerStart; override;
    procedure ServerStop; override;
    procedure ConsoleSetup; override;
    procedure ConsoleStart; override;
    procedure ConsoleHelp; override;
  public
    constructor Create(AConfigProc: TWiRLConfigProc); override;

    class var Running: Boolean;
    class procedure LogInfo(const AMessage: string); override;
    class procedure LogWarning(const AMessage: string); override;
    class procedure LogError(const AMessage: string); override;
    class procedure LogRaw(const AMessage: string); override;
  end;

implementation


{$IFDEF MSWINDOWS}
function ConsoleHandler(dwCtrlType: DWORD): Boolean; cdecl;
begin
  case dwCtrlType of
    CTRL_CLOSE_EVENT, CTRL_C_EVENT, CTRL_BREAK_EVENT:
    begin
      TWiRLConsoleContainer.Running := False;
      TWiRLConsoleContainer.LogInfo('SIGTERM: stop the server');
      Exit(True);
    end
  else
    Exit(false);
  end;
end;
{$ELSE}
procedure HandleSignals(ASigNum: Integer); cdecl;
begin
  case ASigNum of
    SIGTERM:
    begin
      TWiRLConsoleContainer.Running := False;
      TWiRLConsoleContainer.LogInfo('SIGTERM: stop the server');
    end;
  end;
end;
{$ENDIF}

{ TWiRLConsoleContainer }

procedure TWiRLConsoleContainer.ServerStart;
begin
  inherited;
end;

procedure TWiRLConsoleContainer.ServerStop;
begin
  inherited;
end;

procedure TWiRLConsoleContainer.ConsoleSetup;
begin
  ConsoleHelp;
end;

procedure TWiRLConsoleContainer.ConsoleHelp;
begin
end;

procedure TWiRLConsoleContainer.WriteStatus;
begin
  inherited;
end;

class procedure TWiRLConsoleContainer.LogError(const AMessage: string);
begin
  Writeln(AMessage);
end;

class procedure TWiRLConsoleContainer.LogInfo(const AMessage: string);
begin
  Writeln(AMessage);
end;

class procedure TWiRLConsoleContainer.LogRaw(const AMessage: string);
begin
  Write(AMessage);
end;

class procedure TWiRLConsoleContainer.LogWarning(const AMessage: string);
begin
  Writeln(AMessage);
end;

procedure TWiRLConsoleContainer.ConsoleStart;
begin
  Running := True;
  while Running do
    try
      Sleep(1000);
    except
      on EControlC do
      begin
        Running := False;
      end;
    end;
  ServerStop;
end;

constructor TWiRLConsoleContainer.Create(AConfigProc: TWiRLConfigProc);
begin
  inherited;
  HandleSigTerm;
end;

procedure TWiRLConsoleContainer.HandleSigTerm;
begin
  {$IFDEF MSWINDOWS}
  SetConsoleCtrlHandler(@ConsoleHandler, TRUE);
  {$ELSE}
  signal(SIGTERM, @HandleSignals);
  {$ENDIF}
end;

end.
