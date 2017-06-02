{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Client.Resource;

{$I WiRL.inc}

interface

uses
  SysUtils, Classes,
  WiRL.Client.CustomResource;

type
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidAndroid)]
  TWiRLClientResource = class(TWiRLClientCustomResource)
  published
    property Accept;
    property ContentType;
    property Application;
    property Client;
    property SpecificAccept;
    property SpecificContentType;
    property SpecificClient;
    property Resource;
    property Path;
    property PathParamsValues;
    property QueryParams;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('WiRL Client', [TWiRLClientResource]);
end;

end.
