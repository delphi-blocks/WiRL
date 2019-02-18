{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Console.Standard;

interface

uses
  System.SysUtils,
  WiRl.Console.Base;

type
  TWiRLConsoleStandard = class(TWiRLConsoleBase)
  protected
    procedure WriteStatus; override;
    procedure ServerStart; override;
    procedure ServerStop; override;
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

{ TWiRLConsoleStandard }

procedure TWiRLConsoleStandard.ServerStart;
begin
  inherited;
  LogRaw(TWiRLConsoleDef.CommandPrompt);
end;

procedure TWiRLConsoleStandard.ServerStop;
begin
  inherited;
  LogRaw(TWiRLConsoleDef.CommandPrompt);
end;

procedure TWiRLConsoleStandard.ConsoleSetup;
begin
  ConsoleHelp;
end;

procedure TWiRLConsoleStandard.ConsoleHelp;
begin
  LogInfo(TWiRLConsoleDef.Logo);
  LogInfo(TWiRLConsoleDef.OSVer + OSVersion);
  LogInfo('');
  LogInfo(TWiRLConsoleDef.Help);

  LogRaw(TWiRLConsoleDef.CommandPrompt);
end;

procedure TWiRLConsoleStandard.WriteStatus;
begin
  inherited;
  LogRaw(TWiRLConsoleDef.CommandPrompt);
end;

class procedure TWiRLConsoleStandard.LogError(const AMessage: string);
begin
  Writeln(AMessage);
end;

class procedure TWiRLConsoleStandard.LogInfo(const AMessage: string);
begin
  Writeln(AMessage);
end;

class procedure TWiRLConsoleStandard.LogRaw(const AMessage: string);
begin
  Write(AMessage);
end;

class procedure TWiRLConsoleStandard.LogWarning(const AMessage: string);
begin
  Writeln(AMessage);
end;

procedure TWiRLConsoleStandard.ConsoleStart;
var
  LCommand: string;
begin
  while True do
  begin
    Readln(LCommand);
    LCommand := LowerCase(LCommand);
    if SameText(LCommand, TWiRLConsoleDef.CommandStart) then
      ServerStart
    else if SameText(LCommand, TWiRLConsoleDef.CommandStatus) then
      WriteStatus
    else if SameText(LCommand, TWiRLConsoleDef.CommandStop) then
      ServerStop
    else if LCommand.StartsWith(TWiRLConsoleDef.CommandPort, True) then
      ChangePort(LCommand.Split([' '])[1])
    else if SameText(LCommand, TWiRLConsoleDef.CommandHelp) then
      ConsoleHelp
    else if SameText(LCommand, TWiRLConsoleDef.CommandExit) then
    begin
      ServerStop;
      Break;
    end
    else
    begin
      LogInfo(TWiRLConsoleDef.InvalidCommand);
      LogInfo(TWiRLConsoleDef.CommandPrompt);
    end;
  end;
end;

end.
