{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Client.SubResource.JSON;

{$I WiRL.inc}

interface

uses
  SysUtils, Classes
  , WiRL.Core.JSON

  , WiRL.Client.SubResource
  , WiRL.Client.Client
  ;

type
  {$ifdef DelphiXE2_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidAndroid)]
  {$endif}
  TWiRLClientSubResourceJSON = class(TWiRLClientSubResource)
  private
    FResponse: TJSONValue;
  protected
    procedure AfterGET(); override;
    procedure AfterPOST(); override;
    function GetResponseAsString: string; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Response: TJSONValue read FResponse write FResponse;
    property ResponseAsString: string read GetResponseAsString;
  end;

procedure Register;

implementation

uses
  WiRL.Core.Utils;

procedure Register;
begin
  RegisterComponents('WiRL Client', [TWiRLClientSubResourceJSON]);
end;

{ TWiRLClientResourceJSON }

procedure TWiRLClientSubResourceJSON.AfterGET();
begin
  inherited;
  if Assigned(FResponse) then
    FResponse.Free;
  FResponse := StreamToJSONValue(Client.Response.ContentStream);
end;

procedure TWiRLClientSubResourceJSON.AfterPOST;
begin
  inherited;
  if Assigned(FResponse) then
    FResponse.Free;
  FResponse := StreamToJSONValue(Client.Response.ContentStream);
end;

constructor TWiRLClientSubResourceJSON.Create(AOwner: TComponent);
begin
  inherited;
  FResponse := TJSONObject.Create;
end;

destructor TWiRLClientSubResourceJSON.Destroy;
begin
  FResponse.Free;
  inherited;
end;

function TWiRLClientSubResourceJSON.GetResponseAsString: string;
begin
  Result := '';
  if Assigned(FResponse) then
    Result := TJSONHelper.ToJSON(FResponse);
end;

end.
