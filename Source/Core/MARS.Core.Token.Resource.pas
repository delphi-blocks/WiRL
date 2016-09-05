(*
  Copyright 2015-2016, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit MARS.Core.Token.Resource;

interface

uses
  System.Classes, System.SysUtils,

  MARS.Core.JSON,
  MARS.Core.Registry,
  MARS.Core.Classes,
  MARS.Core.Application,
  MARS.Core.Declarations,
  MARS.Core.Attributes,
  MARS.Core.MediaType,
  MARS.Core.MessageBodyWriter,
  MARS.Core.Token,
  MARS.Core.URL;

type
  TMARSAuthResource = class
  private
    procedure SetAuthContext(AValidated: Boolean; const AUserName: string);
  protected
    [Context] FAuthContext: TMARSAuthContext;
    [Context] FApplication: TMARSApplication;
    function Authenticate(const AUserName, APassword: string): Boolean; virtual; abstract;
    function RolesFromUserName(const AUserName: string): TArray<string>; virtual;
  public
    [GET]
    [Produces(TMediaType.APPLICATION_JSON)]
    function GetCurrent: TJSONObject;

    [POST]
    function DoLogin(
      [FormParam('username')] const AUsername: string;
      [FormParam('password')] const APassword: string): TJSONObject;

    [DELETE]
    function Logout: TJSONObject;
  end;


implementation

uses
  DateUtils;

function TMARSAuthResource.DoLogin(const AUsername, APassword: string): TJSONObject;
begin
  if Authenticate(AUserName, APassword) then
    SetAuthContext(True, AUserName)
  else
    SetAuthContext(False, AUsername);

  FApplication.GenerateToken;

  Result := GetCurrent;
end;

function TMARSAuthResource.GetCurrent: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('token', TJSONString.Create(FAuthContext.CompactToken));
end;

function TMARSAuthResource.Logout: TJSONObject;
begin
  FAuthContext.Authenticated := False;
  FAuthContext.Subject.SetUserAndRoles('', nil);

  Result := GetCurrent;
end;

function TMARSAuthResource.RolesFromUserName(const AUserName: string): TArray<string>;
begin
  Result := ['user'];
  if SameText(AUserName, 'admin') then
    Result := Result + ['admin'];
end;

procedure TMARSAuthResource.SetAuthContext(AValidated: Boolean; const AUserName: string);
begin
  FAuthContext.Authenticated := AValidated;
  if AValidated then
    // User authenticated, set user roles
    FAuthContext.Subject.SetUserAndRoles(AUserName, RolesFromUserName(AUserName))
  else
    // User not authenticated, clear user roles and username
    FAuthContext.Subject.SetUserAndRoles('', nil);
end;

end.
