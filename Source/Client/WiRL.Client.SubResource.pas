{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Client.SubResource;

{$I ..\Core\WiRL.inc}

interface

uses
  System.SysUtils, System.Classes, 
  WiRL.Client.Resource, 
  WiRL.http.Client, 
  WiRL.Client.Application;

type
  {$IFDEF HAS_NEW_PIDS}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator32 or pidiOSDevice32 or pidAndroid32Arm)]
  {$ELSE}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidAndroid)]
  {$ENDIF}
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

implementation

uses
  WiRL.Client.Utils,
  WiRL.http.URL;

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
