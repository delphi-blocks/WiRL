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
  TMARSAuthPrincipal = class
  public
    UserName: string;
    Password: string;
  end;

  TMARSAuthResult = record
  public
    Success: Boolean;
    Roles: TArray<string>;
    function RolesAsString: string;
  end;

  /// <summary>
  /// Base class for the authentication resource
  /// </summary>
  TMARSAuthResource = class
  protected
    [Context] FAuthContext: TMARSAuthContext;
    [Context] FApplication: TMARSApplication;

    function Authenticate(const AUserName, APassword: string): TMARSAuthResult; virtual; abstract;
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

{ TMARSAuthFormResource }

function TMARSAuthFormResource.DoLogin(const AUsername, APassword: string): TJSONObject;
var
  LAuthOperation: TMARSAuthResult;
begin
  FAuthContext.Clear;
  LAuthOperation := Authenticate(AUserName, APassword);

  FAuthContext.Authenticated := LAuthOperation.Success;
  //FAuthContext.Subject.Roles := LAuthOperation.Roles;
  FAuthContext.Subject.Roles := string.Join(',', LAuthOperation.Roles);

  FAuthContext.Subject.UserName := AUsername;

  if not LAuthOperation.Success then
    raise EMARSNotAuthorizedException.Create('Invalid username or password', 'TMARSAuthFormResource', 'DoLogin');

  FApplication.GenerateToken;
  Result := GetGeneratedToken;
end;

{ TMARSAuthBasicResource }

function TMARSAuthBasicResource.DoLogin(const AAuth: string): TJSONObject;
var
  LAuthField: string;
  LAuthOperation: TMARSAuthResult;
  LAuthData: TArray<string>;
begin
  if not AAuth.StartsWith(AUTH_BASIC) then
    raise EMARSNotAuthorizedException.Create('Auhtorization header incorrect');

  LAuthField := AAuth.Substring(AUTH_BASIC.Length);
  LAuthField := TBase64.Decode(LAuthField);
  LAuthData := LAuthField.Split([':']);

  if Length(LAuthData) < 2 then
    raise EMARSNotAuthorizedException.Create('Malformed (basic) credentials');

  FAuthContext.Clear;
  LAuthOperation := Authenticate(LAuthData[0], LAuthData[1]);

  FAuthContext.Authenticated := LAuthOperation.Success;
  FAuthContext.Subject.Roles := string.Join(',', LAuthOperation.Roles);
  FAuthContext.Subject.UserName := LAuthData[0];

  if not LAuthOperation.Success then
    raise EMARSNotAuthorizedException.Create('Invalid (basic) credentials', 'TMARSAuthBasicResource', 'DoLogin');

  FApplication.GenerateToken;
  Result := GetGeneratedToken;
end;

{ TMARSAuthResult }

function TMARSAuthResult.RolesAsString: string;
begin

end;

end.
