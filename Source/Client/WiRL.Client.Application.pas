{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Client.Application;

{$I WiRL.inc}

interface

uses
  SysUtils, Classes

  , WiRL.Client.Client
  ;

type
  {$ifdef DelphiXE2_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidAndroid)]
  {$endif}
  TWiRLClientApplication = class(TComponent)
  private
    FAppName: string;
    FDefaultMediaType: string;
    FClient: TWiRLClient;
  protected
    function GetPath: string; virtual;
  public
    constructor Create(AOwner: TComponent); override;

  published
    property DefaultMediaType: string read FDefaultMediaType write FDefaultMediaType;
    property AppName: string read FAppName write FAppName;
    property Client: TWiRLClient read FClient write FClient;
    property Path: string read GetPath;
  end;

procedure Register;

implementation

uses
    WiRL.Client.Utils
  , WiRL.Core.URL
  ;

procedure Register;
begin
  RegisterComponents('WiRL Client', [TWiRLClientApplication]);
end;

{ TWiRLClientApplication }

constructor TWiRLClientApplication.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultMediaType := 'application/json';
  FAppName := 'default';
  if TWiRLComponentHelper.IsDesigning(Self) then
    FClient := TWiRLComponentHelper.FindDefault<TWiRLClient>(Self);

end;

function TWiRLClientApplication.GetPath: string;
var
  LEngine: string;
begin
  LEngine := '';
  if Assigned(FClient) then
    LEngine := FClient.WiRLEngineURL;

  Result := TWiRLURL.CombinePath([LEngine, AppName])
end;

end.
