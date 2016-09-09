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
  MARS.Core.URL, System.JSON;

type
  /// <summary>
  /// Base class for the authentication resource
  /// </summary>
  TMARSAuthResource = class
  private
    procedure SetAuthContext(AValidated: Boolean; const AUserName: string);
  protected
    [Context] FAuthContext: TMARSAuthContext;
    [Context] FApplication: TMARSApplication;

    function Authenticate(const AUserName, APassword: string): Boolean; virtual; abstract;
    function RolesFromUserName(const AUserName: string): TArray<string>; virtual;
  public
    [GET, Produces(TMediaType.APPLICATION_JSON)]
    function GetGeneratedToken: TJSONObject;
  end;

  /// <summary>
  /// Custom authentication (www-form-urlencoded) resource
  /// </summary>
  /// <remarks>
  /// The field are named "username" and "password". If you want custom field names you must inherit the base class
  /// </remarks>
  TMARSAuthFormResource = class(TMARSAuthResource)
  public
    [POST, Produces(TMediaType.APPLICATION_JSON)]
    function DoLogin(
      [FormParam('username')] const AUsername: string;
      [FormParam('password')] const APassword: string
    ): TJSONObject;
  end;

  /// <summary>
  /// HTTP basic authentication resource
  /// </summary>
  TMARSAuthBasicResource = class(TMARSAuthResource)
  private
    const AUTH_BASIC = 'Basic ';
  public
    [POST, Produces(TMediaType.APPLICATION_JSON)]
    function DoLogin([HeaderParam('Authorization')] const AAuth: string): TJSONObject;
  end;

implementation

uses
  System.DateUtils,
  JOSE.Encoding.Base64,
  MARS.Core.Exceptions;

function TMARSAuthResource.GetGeneratedToken: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('access_token', TJSONString.Create(FAuthContext.CompactToken));
end;

function TMARSAuthResource.RolesFromUserName(const AUserName: string): TArray<string>;
begin
  SetLength(Result, 1);
  Result[0] := 'user';
  if SameText(AUserName, 'admin') then
  begin
    SetLength(Result, 2);
    Result[1] := 'admin';
  end;
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

function TMARSAuthFormResource.DoLogin(const AUsername, APassword: string): TJSONObject;
begin
  SetAuthContext(False, AUsername);
  if not Authenticate(AUserName, APassword) then
    raise EMARSNotAuthorizedException.Create('Invalid username or password', 'TMARSAuthFormResource', 'DoLogin');

  SetAuthContext(True, AUserName);
  FApplication.GenerateToken;
  Result := GetGeneratedToken;
end;

{ TMARSAuthBasicResource }

function TMARSAuthBasicResource.DoLogin(const AAuth: string): TJSONObject;
var
  LAuth: string;
  LAuthData: TArray<string>;
begin
  if not AAuth.StartsWith(AUTH_BASIC) then
    raise EMARSNotAuthorizedException.Create('Auhtorization header incorrect');

  LAuth := AAuth.Substring(AUTH_BASIC.Length);
  LAuth := TBase64.Decode(LAuth);
  LAuthData := LAuth.Split([':']);

  if Length(LAuthData) < 2 then
    raise EMARSNotAuthorizedException.Create('Invalid (basic) credentials');

  SetAuthContext(False, LAuthData[0]);
  if not Authenticate(LAuthData[0], LAuthData[1]) then
    raise EMARSNotAuthorizedException.Create('Invalid (basic) credentials', 'TMARSAuthFormResource', 'DoLogin');

  SetAuthContext(True, LAuthData[0]);
  FApplication.GenerateToken;

  Result := GetGeneratedToken;
end;

end.
