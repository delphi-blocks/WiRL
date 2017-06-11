{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Console.Standard;

interface

uses
  System.SysUtils,
  WiRl.Console.Base,
  WiRL.http.Server.Indy;

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
    class procedure Log(const AMessage: string); override;
    class procedure LogLn(const AMessage: string); override;
    class procedure LogRaw(const AMessage: string); override;
  end;

implementation

procedure TWiRLConsoleStandard.ServerStart;
begin
  inherited;
  Log(TWiRLConsoleDef.CommandPrompt);
end;

procedure TWiRLConsoleStandard.ServerStop;
begin
  inherited;
  Log(TWiRLConsoleDef.CommandPrompt);
end;

procedure TWiRLConsoleStandard.ConsoleSetup;
begin
  ConsoleHelp;
end;

procedure TWiRLConsoleStandard.ConsoleHelp;
begin
  LogLn(TWiRLConsoleDef.Logo);
  LogLn(TWiRLConsoleDef.OSVer + OSVersion);
  LogLn('');
  LogLn(TWiRLConsoleDef.Help);

  Log(TWiRLConsoleDef.CommandPrompt);
end;

procedure TWiRLConsoleStandard.WriteStatus;
begin
  inherited;
  Log(TWiRLConsoleDef.CommandPrompt);
end;

class procedure TWiRLConsoleStandard.Log(const AMessage: string);
begin
  Write(AMessage);
end;

class procedure TWiRLConsoleStandard.LogLn(const AMessage: string);
begin
  Writeln(AMessage);
end;

class procedure TWiRLConsoleStandard.LogRaw(const AMessage: string);
begin
  Write(AMessage);
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
      LogLn(TWiRLConsoleDef.InvalidCommand);
      LogLn(TWiRLConsoleDef.CommandPrompt);
    end;
  end;
end;

end.
