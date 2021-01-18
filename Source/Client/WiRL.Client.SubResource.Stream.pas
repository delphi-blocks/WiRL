{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Client.SubResource.Stream;

{$I ..\Core\WiRL.inc}

interface

uses
  System.SysUtils, System.Classes, 
  WiRL.Client.SubResource, 
  WiRL.http.Client.Interfaces,
  WiRL.http.Client;

type
  {$IFDEF HAS_NEW_PIDS}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator32 or pidiOSDevice32 or pidAndroid32Arm)]
  {$ELSE}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidAndroid)]
  {$ENDIF}
  TWiRLClientSubResourceStream = class(TWiRLClientSubResource)
  private
    FResponse: TStream;
  protected
    procedure AfterGET(AResponse: IWiRLResponse); override;
    procedure AfterPOST(AResponse: IWiRLResponse); override;
    function GetResponseSize: Int64; virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Response: TStream read FResponse;
    property ResponseSize: Int64 read GetResponseSize;
  end;

implementation

uses
  WiRL.Core.Utils;

{ TWiRLClientResourceJSON }

procedure TWiRLClientSubResourceStream.AfterGET(AResponse: IWiRLResponse);
begin
  inherited;
  CopyStream(AResponse.ContentStream, FResponse);
end;

procedure TWiRLClientSubResourceStream.AfterPOST(AResponse: IWiRLResponse);
begin
  inherited;
  CopyStream(AResponse.ContentStream, FResponse);
end;

constructor TWiRLClientSubResourceStream.Create(AOwner: TComponent);
begin
  inherited;
  FResponse := TMemoryStream.Create;
end;

destructor TWiRLClientSubResourceStream.Destroy;
begin
  FResponse.Free;
  inherited;
end;


function TWiRLClientSubResourceStream.GetResponseSize: Int64;
begin
  Result := FResponse.Size;
end;

end.
