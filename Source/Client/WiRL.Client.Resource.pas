(*
  Copyright 2015-2016, WiRL - REST Library

  Home: https://github.com/WiRL-library

*)
unit WiRL.Client.Resource;

{$I WiRL.inc}

interface

uses
  SysUtils, Classes

  , WiRL.Client.CustomResource
  ;

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
