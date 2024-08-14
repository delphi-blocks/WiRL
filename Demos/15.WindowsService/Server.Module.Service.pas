{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Module.Service;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.SvcMgr, System.Win.Registry,

  // WiRL units
  WiRL.Engine.REST,
  WiRL.http.Server,
  WiRL.Console.Base,
  WiRL.Core.Application,
  WiRL.Console.Factory,
  WiRL.http.Server.Indy;

type
  TWiRLService = class(TService)
    procedure ServiceAfterInstall(Sender: TService);
    procedure ServiceAfterUninstall(Sender: TService);
    procedure ServiceExecute(Sender: TService);
  private
    FServer: TWiRLServer;
    procedure Log(const AMessage: string);
    procedure ConfigureServer;
  public
    function GetServiceController: TServiceController; override;
  end;

var
  WiRLService: TWiRLService;

implementation

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  WiRLService.Controller(CtrlCode);
end;

{ TServiceWiRL }

procedure TWiRLService.ConfigureServer;
begin
  FServer
    .SetPort(8080)
    .SetThreadPoolSize(10)
    .AddEngine<TWiRLRESTEngine>('rest')
    .SetEngineName('REST Service')
      .AddApplication('app')
      .SetAppName('Demo App')
      //.SetSecret(TEncoding.UTF8.GetBytes('samplekey...................'))
      .SetFilters('*')
      .SetResources(['*'])
  ;
end;

function TWiRLService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TWiRLService.Log(const AMessage: string);
begin
  OutputDebugString(PChar(DateTimeToStr(Now) + ' ' + AMessage));
end;

procedure TWiRLService.ServiceAfterInstall(Sender: TService);
var
  LReg: TRegistry;
begin
  // To have the Description of your service in the Service Manager

  Log('Registered ' + DisplayName);
  LReg := TRegistry.Create(KEY_READ or KEY_WRITE);
  try
    LReg.RootKey := HKEY_LOCAL_MACHINE;
    if LReg.OpenKey('\SYSTEM\CurrentControlSet\Services\' + Name, False) then
    begin
      LReg.WriteString('Description', 'Demo WiRL Service');
      LReg.CloseKey;
    end;
  finally
    LReg.Free;
  end;
end;

procedure TWiRLService.ServiceAfterUninstall(Sender: TService);
begin
  Log('Unregistered ' + DisplayName);
end;

procedure TWiRLService.ServiceExecute(Sender: TService);
begin
  FServer := TWiRLServer.Create(nil);
  try
    ConfigureServer;

    Log('Service starting...');
    FServer.Active := True;
    Log('Service started...');

    while not Terminated do
      ServiceThread.ProcessRequests(True); // wait for termination

    Log('Service stopping...');
    FServer.Active := False;
    Log('Service stopped...');
  finally
    FreeAndNil(FServer);
  end;
end;

end.
