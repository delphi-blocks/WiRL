{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Console.Factory;

interface

uses
  System.SysUtils,
  WiRL.Console.Base,
  {$IFDEF LINUX}
  WiRL.Console.Daemon,
  {$ENDIF}
  WiRL.Console.Standard;

type
  TWiRLConsoleFactory = class
  private class var
    FConsoleClass: TWiRLConsoleClass;
  public
    class function NewConsole(AConfigProc: TWiRLConfigProc): TWiRLConsoleBase;
  public
    class procedure Log(const AMessage: string);
    class procedure LogLn(const AMessage: string);
    class procedure LogRaw(const AMessage: string);
  end;

implementation

class function TWiRLConsoleFactory.NewConsole(AConfigProc: TWiRLConfigProc): TWiRLConsoleBase;
begin
  {$IFDEF LINUX}
    {$IFDEF DEBUG}
    Result := TWiRLConsoleStandard.Create(AConfigProc);
    {$ELSE}
      {$IFDEF DAEMON}
      Result := TWiRLConsoleDaemon.Create(AConfigProc);
      {$ELSE}
      Result := TWiRLConsoleStandard.Create(AConfigProc);
      {$ENDIF}
    {$ENDIF}
  {$ELSE}
  Result := TWiRLConsoleStandard.Create(AConfigProc);
  {$ENDIF}
end;

class procedure TWiRLConsoleFactory.Log(const AMessage: string);
begin
  FConsoleClass.Log(AMessage);
end;

class procedure TWiRLConsoleFactory.LogLn(const AMessage: string);
begin
  FConsoleClass.LogLn(AMessage);
end;

class procedure TWiRLConsoleFactory.LogRaw(const AMessage: string);
begin
  FConsoleClass.LogRaw(AMessage);
end;

end.
