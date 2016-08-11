(*
  Copyright 2015-2016, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit MARS.Client.Token;

{$I MARS.inc}

interface

uses
  System.SysUtils, System.Classes,
  MARS.Core.JSON,
  MARS.Client.Resource,
  MARS.Client.Client;

type
  {$ifdef DelphiXE2_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidAndroid)]
  {$endif}
  TMARSClientToken = class(TMARSClientResource)
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
  MARS.Core.Utils,
  MARS.Rtti.Utils;

procedure Register;
begin
  RegisterComponents('MARS Client', [TMARSClientToken]);
end;

{ TMARSClientToken }

procedure TMARSClientToken.AfterDELETE();
begin
  inherited;
  if Assigned(FData) then
    FData.Free;
  FData := StreamToJSONValue(Client.Response.ContentStream) as TJSONObject;
  ParseData;
end;

procedure TMARSClientToken.AfterGET();
begin
  inherited;
  if Assigned(FData) then
    FData.Free;
  FData := StreamToJSONValue(Client.Response.ContentStream) as TJSONObject;
  ParseData;
end;

procedure TMARSClientToken.AfterPOST();
begin
  inherited;

  if Assigned(FData) then
    FData.Free;
  FData := StreamToJSONValue(Client.Response.ContentStream) as TJSONObject;
  ParseData;
end;

procedure TMARSClientToken.BeforePOST(AContent: TMemoryStream);
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

constructor TMARSClientToken.Create(AOwner: TComponent);
begin
  inherited;
  FData := TJSONObject.Create;
  FUserRoles := TStringList.Create;
  Resource := 'token';
end;

destructor TMARSClientToken.Destroy;
begin
  FUserRoles.Free;
  FData.Free;

  inherited;
end;

procedure TMARSClientToken.ParseData;
begin

end;

end.
