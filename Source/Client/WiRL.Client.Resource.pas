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
  {$ifdef DelphiXE2_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidAndroid)]
  {$endif}
  TWiRLClientResource = class(TWiRLClientCustomResource)
  private
  protected
  public
  published
    property Accept;
    property Application;
    property Client;
    property SpecificAccept;
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
