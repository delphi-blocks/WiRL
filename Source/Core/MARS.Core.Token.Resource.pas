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
  protected
    [Context] FAuthContext: TMARSAuthContext;

    function Authenticate(const AUserName, APassword: string): Boolean; virtual;
    procedure BeforeLogin(const AUserName, APassword: string); virtual;
    procedure AfterLogin(const AUserName, APassword: string); virtual;
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

{ TMARSAuthResource }

procedure TMARSAuthResource.AfterLogin(const AUserName, APassword: string);
begin

end;

function TMARSAuthResource.Authenticate(const AUserName, APassword: string): Boolean;
begin
  Result := SameText(APassword, IntToStr(HourOf(Now)));

  if Result then // authenticated, set user roles
  begin
    if SameText(AUserName, 'admin') then
      FAuthContext.Subject.SetUserAndRoles(AUserName, TArray<string>.Create('standard', 'admin'))
    else
      FAuthContext.Subject.SetUserAndRoles(AUserName, TArray<string>.Create('standard'));
  end
  else // not authenticated, clear user roles and username
    FAuthContext.Subject.SetUserAndRoles('', nil);
end;

procedure TMARSAuthResource.BeforeLogin(const AUserName, APassword: string);
begin

end;

function TMARSAuthResource.DoLogin(const AUsername, APassword: string): TJSONObject;
begin
  Result := nil;
  BeforeLogin(AUserName, APassword);
  try
    if Authenticate(AUserName, APassword) then
    Result := GetCurrent;
  finally
    AfterLogin(AUserName, APassword);
  end;
end;

function TMARSAuthResource.GetCurrent: TJSONObject;
begin
  Result := FAuthContext.Subject.JSON.Clone as TJSONObject;
end;

function TMARSAuthResource.Logout: TJSONObject;
begin
  FAuthContext.Authenticated := False;
  FAuthContext.Subject.SetUserAndRoles('', nil);

  Result := GetCurrent;
end;

end.
