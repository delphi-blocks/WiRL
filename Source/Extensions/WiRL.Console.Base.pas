{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Console.Base;

interface

uses
  System.SysUtils,
  WiRL.http.Server,
  WiRL.http.Server.Indy;

type
  EConsoleException = class(Exception);

type
  TWiRLConfigProc = reference to procedure (AServer: TWiRLServer);

type
  /// <summary>
  ///   Console const strings definition
  /// </summary>
  TWiRLConsoleDef = class
  public const
    ServerRunning    = 'WiRL service already running';
    ServerNotRunning = 'WiRL service not running';
    ServerStarting   = 'WiRL service starting (port %d)...';
    ServerStarted    = 'WiRL service started';
    ServerStopping   = 'WiRL service stopping...';
    ServerStopped    = 'WiRL service stopped';
    InvalidCommand   = ' Console Error -> Invalid Command';
  public const
    Active = 'Active: ';
    OSVer  = 'Running on: ';
    Port   = 'Port: ';
    Logo   =
     ' ' + sLineBreak +
     ' .--------------. .--------------. .--------------. .--------------. ' + sLineBreak +
     ' | _____  _____ | |     _____    | |  _______     | |   _____      | ' + sLineBreak +
     ' ||_   _||_   _|| |    |_   _|   | | |_   __ \    | |  |_   _|     | ' + sLineBreak +
     ' |  | | /\ | |  | |      | |     | |   | |__) |   | |    | |       | ' + sLineBreak +
     ' |  | |/  \| |  | |      | |     | |   |  __ /    | |    | |   _   | ' + sLineBreak +
     ' |  |   /\   |  | |     _| |_    | |  _| |  \ \_  | |   _| |__/ |  | ' + sLineBreak +
     ' |  |__/  \__|  | |    |_____|   | | |____| |___| | |  |________|  | ' + sLineBreak +
     ' |              | |              | |              | |              | ' + sLineBreak +
     ' .--------------. .--------------. .--------------. .--------------. ' + sLineBreak +
     '';

    Help =
      ' Please enter a command: ' + sLineBreak +
      '  "start"  - service start' + sLineBreak +
      '  "stop"   - service stop' + sLineBreak +
      '  "status" - service status' + sLineBreak +
      '  "help"   - available commands' + sLineBreak +
      '  "port"   - change service port' + sLineBreak +
      '  "exit"   - exit application';


  public const
    /// <summary>
    /// Available commands
    /// </summary>
    CommandPrompt  = '>';
    CommandStart   = 'start';
    CommandStop    = 'stop';
    CommandStatus  = 'status';
    CommandHelp    = 'help';
    CommandPort    = 'port';
    CommandExit    = 'exit';
  end;

  /// <summary>
  ///   Base console class
  /// </summary>
  TWiRLConsoleBase = class
  protected
    FPort: Integer;
    FServer: TWiRLServer;
    FConfigProc: TWiRLConfigProc;
    function OSVersion: string;
    procedure WriteStatus; virtual;
    procedure ServerSetup;
    procedure ServerStart; virtual;
    procedure ServerStop; virtual;
    procedure ChangePort(const APort: string);
    procedure ConsoleSetup; virtual; abstract;
    procedure ConsoleStart; virtual; abstract;
    procedure ConsoleHelp; virtual; abstract;
  public
    constructor Create(AConfigProc: TWiRLConfigProc); virtual;
    destructor Destroy; override;
    procedure Start;
  public
    class procedure LogInfo(const AMessage: string); virtual; abstract;
    class procedure LogWarning(const AMessage: string); virtual; abstract;
    class procedure LogError(const AMessage: string); virtual; abstract;
    class procedure LogRaw(const AMessage: string); virtual; abstract;
  end;

  TWiRLConsoleClass = class of TWiRLConsoleBase;

implementation

{ TWiRLConsoleBase }

procedure TWiRLConsoleBase.ChangePort(const APort: string);
begin
  FPort := StrToIntDef(APort, 8080);
  if Assigned(FConfigProc) then
    FConfigProc(FServer);
end;

procedure TWiRLConsoleBase.ServerSetup;
begin
  if FServer.Active then
    ServerStop;

  if Assigned(FConfigProc) then
    FConfigProc(FServer);
end;

constructor TWiRLConsoleBase.Create(AConfigProc: TWiRLConfigProc);
begin
  FServer := TWiRLServer.Create(nil);
  FConfigProc := AConfigProc;
  FPort := 8080;
end;

destructor TWiRLConsoleBase.Destroy;
begin
  FServer.Free;
  inherited;
end;

function TWiRLConsoleBase.OSVersion: string;
begin
  Result := TOSVersion.ToString;
end;

procedure TWiRLConsoleBase.ServerStart;
begin
  if not FServer.Active then
  begin
    LogInfo(Format(TWiRLConsoleDef.ServerStarting, [FServer.Port]));
    FServer.Active := True;
    LogInfo(TWiRLConsoleDef.ServerStarted);
  end
  else
    LogInfo(TWiRLConsoleDef.ServerRunning);
end;

procedure TWiRLConsoleBase.ServerStop;
begin
  if FServer.Active  then
  begin
    LogInfo(TWiRLConsoleDef.ServerStopping);
    FServer.Active := False;
    LogInfo(TWiRLConsoleDef.ServerStopped);
  end
  else
    LogWarning(TWiRLConsoleDef.ServerNotRunning);
end;

procedure TWiRLConsoleBase.Start;
begin
  ConsoleSetup;
  ServerSetup;
  ServerStart;
  ConsoleStart;
end;

procedure TWiRLConsoleBase.WriteStatus;
begin
  LogInfo(TWiRLConsoleDef.Active + FServer.Active.ToString(TUseBoolStrs.True));
  LogInfo(TWiRLConsoleDef.Port + FPort.ToString);
  LogInfo(TWiRLConsoleDef.OSVer + OSVersion);
end;

end.
