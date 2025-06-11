{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
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
  TWiRLConsoleStatic = class
  protected
    class var FConsoleClass: TWiRLConsoleClass;
  protected
    class function GetConsoleClass: TWiRLConsoleClass; static;
  public
    class property ConsoleClass: TWiRLConsoleClass read GetConsoleClass;
  end;

  TWiRLConsoleFactory = class(TWiRLConsoleStatic)
  public
    class function NewConsole(AConsoleClass: TWiRLConsoleClass; AConfigProc: TWiRLConfigProc): TWiRLConsoleBase; overload;
    class function NewConsole(AConfigProc: TWiRLConfigProc): TWiRLConsoleBase; overload;
  end;

  TWiRLConsoleLogger = class(TWiRLConsoleStatic)
  public
    class procedure LogInfo(const AMessage: string);
    class procedure LogWarning(const AMessage: string);
    class procedure LogError(const AMessage: string);
    class procedure LogRaw(const AMessage: string);
  end;

implementation

{ TWiRLConsoleStatic }

class function TWiRLConsoleStatic.GetConsoleClass: TWiRLConsoleClass;
begin
  if not Assigned(FConsoleClass) then
  begin
    {$IFDEF LINUX}
      {$IFDEF DEBUG}
      FConsoleClass := TWiRLConsoleStandard;
      {$ELSE}
        {$IFDEF DAEMON}
        FConsoleClass := TWiRLConsoleDaemon;
        {$ELSE}
        FConsoleClass := TWiRLConsoleStandard;
        {$ENDIF}
      {$ENDIF}
    {$ELSE}
    FConsoleClass := TWiRLConsoleStandard;
    {$ENDIF}
  end;

  Result := FConsoleClass;
end;

{ TWiRLConsoleFactory }

class function TWiRLConsoleFactory.NewConsole(AConfigProc: TWiRLConfigProc): TWiRLConsoleBase;
begin
  Result := ConsoleClass.Create(AConfigProc);
end;

class function TWiRLConsoleFactory.NewConsole(AConsoleClass: TWiRLConsoleClass;
  AConfigProc: TWiRLConfigProc): TWiRLConsoleBase;
begin
  Result := AConsoleClass.Create(AConfigProc);
end;

{ TWiRLConsoleLogger }

class procedure TWiRLConsoleLogger.LogError(const AMessage: string);
begin
  ConsoleClass.LogError(AMessage);
end;

class procedure TWiRLConsoleLogger.LogInfo(const AMessage: string);
begin
  ConsoleClass.LogInfo(AMessage);
end;

class procedure TWiRLConsoleLogger.LogRaw(const AMessage: string);
begin
  ConsoleClass.LogRaw(AMessage);
end;

class procedure TWiRLConsoleLogger.LogWarning(const AMessage: string);
begin
  ConsoleClass.LogWarning(AMessage);
end;

end.
