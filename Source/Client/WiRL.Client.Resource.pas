{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2025 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Client.Resource;

{$I ..\Core\WiRL.inc}

interface

uses
  System.SysUtils, System.Classes,
  WiRL.Client.CustomResource;

type
  {$IF DEFINED(HAS_NEW_ANDROID_PID)}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator32 or pidiOSDevice32 or pidAndroidArm32)]
  {$ELSEIF DEFINED(HAS_NEW_PIDS)}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator32 or pidiOSDevice32 or pidAndroid32Arm)]
  {$ELSE}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidAndroid)]
  {$ENDIF}
  TWiRLClientResource = class(TWiRLClientCustomResource)
  published
    property Accept;
    property ContentType;
    property Application;
    property Client;
    property Resource;
    property Path;
    property PathParams;
    property QueryParams;
    property AfterRequest;
    property BeforeRequest;
    property Headers;
  end;

implementation

end.
