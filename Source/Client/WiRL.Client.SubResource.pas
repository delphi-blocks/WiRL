{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Client.SubResource;

{$I WiRL.inc}

interface

uses
  SysUtils, Classes

  , WiRL.Client.Resource
  , WiRL.Client.Client
  , WiRL.Client.Application

  ;

type
  {$ifdef DelphiXE2_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidAndroid)]
  {$endif}
  TWiRLClientSubResource = class(TWiRLClientResource)
  private
    FParentResource: TWiRLClientResource;
  protected
    function GetPath: string; override;
    function GetClient: TWiRLClient; override;
    function GetApplication: TWiRLClientApplication; override;
  public
    constructor Create(AOwner: TComponent); override;

  published
    property ParentResource: TWiRLClientResource read FParentResource write FParentResource;
  end;

procedure Register;

implementation

uses
    WiRL.Client.Utils
  , WiRL.Core.URL;

procedure Register;
begin
  RegisterComponents('WiRL Client', [TWiRLClientSubResource]);
end;

{ TWiRLClientSubResource }

constructor TWiRLClientSubResource.Create(AOwner: TComponent);
begin
  inherited;

  if TWiRLComponentHelper.IsDesigning(Self) then
    FParentResource := TWiRLComponentHelper.FindDefault<TWiRLClientResource>(Self);
end;

function TWiRLClientSubResource.GetApplication: TWiRLClientApplication;
begin
  if Assigned(FParentResource) then
    Result := FParentResource.Application
  else
    Result := inherited GetApplication;
end;

function TWiRLClientSubResource.GetClient: TWiRLClient;
begin
  if Assigned(SpecificClient) then
    Result := SpecificClient
  else if Assigned(FParentResource) then
    Result := FParentResource.Client
  else
    Result := inherited GetClient;
end;

function TWiRLClientSubResource.GetPath: string;
begin
  if Assigned(FParentResource) then
    Result := TWiRLURL.CombinePath([FParentResource.Path, Resource])
  else
    Result := inherited GetPath;
end;

end.
