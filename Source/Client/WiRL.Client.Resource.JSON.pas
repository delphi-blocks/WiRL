{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Client.Resource.JSON;

{$I ..\Core\WiRL.inc}

interface

uses
  System.SysUtils, System.Classes, 
  WiRL.Core.JSON, 
  WiRL.Client.Resource, 
  WiRL.http.Client.Interfaces,
  WiRL.http.Client;

type
  {$IFDEF HAS_NEW_PIDS}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator32 or pidiOSDevice32 or pidAndroid32Arm)]
  {$ELSE}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidAndroid)]
  {$ENDIF}
  TWiRLClientResourceJSON = class(TWiRLClientResource)
  private
    FResponse: TJSONValue;
  protected
    procedure AfterGET(AResponse: IWiRLResponse); override;
    procedure AfterPOST(AResponse: IWiRLResponse); override;
    function GetResponseAsString: string; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Response: TJSONValue read FResponse write FResponse;
    property ResponseAsString: string read GetResponseAsString;
  end;

implementation

uses
  WiRL.Core.Utils;

{ TWiRLClientResourceJSON }

procedure TWiRLClientResourceJSON.AfterGET(AResponse: IWiRLResponse);
begin
  inherited;
  if Assigned(FResponse) then
    FResponse.Free;
  FResponse := StreamToJSONValue(AResponse.ContentStream);
end;

procedure TWiRLClientResourceJSON.AfterPOST(AResponse: IWiRLResponse);
begin
  inherited;
  if Assigned(FResponse) then
    FResponse.Free;
  FResponse := StreamToJSONValue(AResponse.ContentStream);
end;

constructor TWiRLClientResourceJSON.Create(AOwner: TComponent);
begin
  inherited;
  FResponse := TJSONObject.Create;
end;

destructor TWiRLClientResourceJSON.Destroy;
begin
  FResponse.Free;
  inherited;
end;

function TWiRLClientResourceJSON.GetResponseAsString: string;
begin
  Result := '';
  if Assigned(FResponse) then
    Result := TJSONHelper.ToJSON(FResponse);
end;

end.
