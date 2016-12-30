{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Client.Resource.JSON;

{$I WiRL.inc}

interface

uses
  SysUtils, Classes
  , WiRL.Core.JSON

  , WiRL.Client.Resource
  , WiRL.Client.Client
  ;

type
  {$ifdef DelphiXE2_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidAndroid)]
  {$endif}
  TWiRLClientResourceJSON = class(TWiRLClientResource)
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
  RegisterComponents('WiRL Client', [TWiRLClientResourceJSON]);
end;

{ TWiRLClientResourceJSON }

procedure TWiRLClientResourceJSON.AfterGET();
begin
  inherited;
  if Assigned(FResponse) then
    FResponse.Free;
  FResponse := StreamToJSONValue(Client.Response.ContentStream);
end;

procedure TWiRLClientResourceJSON.AfterPOST;
begin
  inherited;
  if Assigned(FResponse) then
    FResponse.Free;
  FResponse := StreamToJSONValue(Client.Response.ContentStream);
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
