{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Client.Token;

{$I WiRL.inc}

interface

uses
  System.SysUtils, System.Classes,
  WiRL.Core.JSON,
  WiRL.Client.Resource,
  WiRL.Client.Client;

type
  {$ifdef DelphiXE2_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidAndroid)]
  {$endif}
  TWiRLClientToken = class(TWiRLClientResource)
  private
    FData: TJSONObject;
    FPassword: string;
    FUsername: string;
    FUserRoles: TStringList;
  protected
    procedure AfterGET(); override;

    procedure BeforePOST(AContent: TMemoryStream); override;
    procedure AfterPOST(); override;

    procedure AfterDELETE; override;

    procedure ParseData; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Data: TJSONObject read FData;
    property Password: string read FPassword write FPassword;
    property Username: string read FUsername write FUsername;
  end;

procedure Register;

implementation

uses
  WiRL.Core.Utils,
  WiRL.Rtti.Utils;

procedure Register;
begin
  RegisterComponents('WiRL Client', [TWiRLClientToken]);
end;

{ TWiRLClientToken }

procedure TWiRLClientToken.AfterDELETE();
begin
  inherited;
  if Assigned(FData) then
    FData.Free;
  FData := StreamToJSONValue(Client.Response.ContentStream) as TJSONObject;
  ParseData;
end;

procedure TWiRLClientToken.AfterGET();
begin
  inherited;
  if Assigned(FData) then
    FData.Free;
  FData := StreamToJSONValue(Client.Response.ContentStream) as TJSONObject;
  ParseData;
end;

procedure TWiRLClientToken.AfterPOST();
begin
  inherited;

  if Assigned(FData) then
    FData.Free;
  FData := StreamToJSONValue(Client.Response.ContentStream) as TJSONObject;
  ParseData;
end;

procedure TWiRLClientToken.BeforePOST(AContent: TMemoryStream);
var
  LStreamWriter: TStreamWriter;
begin
  inherited;
  LStreamWriter := TStreamWriter.Create(AContent);
  try
    LStreamWriter.Write('username=' + FUserName + '&password=' + FPassword);
  finally
    LStreamWriter.Free;
  end;
end;

constructor TWiRLClientToken.Create(AOwner: TComponent);
begin
  inherited;
  FData := TJSONObject.Create;
  FUserRoles := TStringList.Create;
  Resource := 'token';
end;

destructor TWiRLClientToken.Destroy;
begin
  FUserRoles.Free;
  FData.Free;

  inherited;
end;

procedure TWiRLClientToken.ParseData;
begin

end;

end.
