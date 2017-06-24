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
  end;

  TWiRLConsoleLogger = class
  private class var
    FConsoleClass: TWiRLConsoleClass;
  private
    function GetConsoleClass: TWiRLConsoleClass;
  public
    class procedure LogInfo(const AMessage: string);
    class procedure LogWarning(const AMessage: string);
    class procedure LogError(const AMessage: string);
    class procedure LogRaw(const AMessage: string);

  end;

implementation

{ TWiRLConsoleFactory }

class function TWiRLConsoleFactory.NewConsole(AConfigProc: TWiRLConfigProc): TWiRLConsoleBase;
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

  Result := FConsoleClass.Create(AConfigProc);
end;

{ TWiRLConsoleLogger }

function TWiRLConsoleLogger.GetConsoleClass: TWiRLConsoleClass;
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

class procedure TWiRLConsoleLogger.LogError(const AMessage: string);
begin
  FConsoleClass.LogError(AMessage);
end;

class procedure TWiRLConsoleLogger.LogInfo(const AMessage: string);
begin
  FConsoleClass.LogInfo(AMessage);
end;

class procedure TWiRLConsoleLogger.LogRaw(const AMessage: string);
begin
  FConsoleClass.LogRaw(AMessage);
end;

class procedure TWiRLConsoleLogger.LogWarning(const AMessage: string);
begin
  FConsoleClass.LogWarning(AMessage);
end;

end.
