{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Auth.Resource;

interface

uses
  System.Classes, System.SysUtils, System.JSON,

  WiRL.Core.JSON,
  WiRL.Core.Registry,
  WiRL.Core.Classes,
  WiRL.Core.Application,
  WiRL.Core.Declarations,
  WiRL.Core.Attributes,
  WiRL.http.Accept.MediaType,
  WiRL.Core.MessageBodyWriter,
  WiRL.Core.Auth.Context,
  WiRL.Core.URL;

type
  TWiRLAuthPrincipal = class
  public
    UserName: string;
    Password: string;
  end;

  TWiRLAuthResult = record
  public
    Success: Boolean;
    Roles: TArray<string>;
    function RolesAsString: string;
  end;

  /// <summary>
  /// Base class for the authentication resource
  /// </summary>
  TWiRLAuthResource = class
  protected
    [Context] FAuthContext: TWiRLAuthContext;
    [Context] FApplication: TWiRLApplication;

    function Authenticate(const AUserName, APassword: string): TWiRLAuthResult; virtual; abstract;
  public
    function GetGeneratedToken: TJSONObject;
  end;

  /// <summary>
  /// Custom authentication (www-form-urlencoded) resource
  /// </summary>
  /// <remarks>
  /// The field are named "username" and "password". If you want custom field names you must inherit the base class
  /// </remarks>
  TWiRLAuthFormResource = class(TWiRLAuthResource)
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
  TWiRLAuthBasicResource = class(TWiRLAuthResource)
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
  WiRL.Core.Exceptions;

function TWiRLAuthResource.GetGeneratedToken: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('access_token', TJSONString.Create(FAuthContext.CompactToken));
end;

{ TWiRLAuthFormResource }

function TWiRLAuthFormResource.DoLogin(const AUsername, APassword: string): TJSONObject;
var
  LAuthOperation: TWiRLAuthResult;
begin
  FAuthContext.Clear;
  LAuthOperation := Authenticate(AUserName, APassword);

  FAuthContext.Authenticated := LAuthOperation.Success;
  FAuthContext.Subject.Roles := string.Join(',', LAuthOperation.Roles);

  if not LAuthOperation.Success then
    raise EWiRLNotAuthorizedException.Create('Invalid credentials', 'TWiRLAuthFormResource', 'DoLogin');

  FAuthContext.Generate(FApplication.Secret);
  Result := GetGeneratedToken;
end;

{ TWiRLAuthBasicResource }

function TWiRLAuthBasicResource.DoLogin(const AAuth: string): TJSONObject;
var
  LAuthField: string;
  LAuthOperation: TWiRLAuthResult;
  LAuthData: TArray<string>;
begin
  if not AAuth.StartsWith(AUTH_BASIC) then
    raise EWiRLNotAuthorizedException.Create('Auhtorization header incorrect');

  LAuthField := AAuth.Substring(AUTH_BASIC.Length);
  LAuthField := TBase64.Decode(LAuthField);
  LAuthData := LAuthField.Split([':']);

  if Length(LAuthData) < 2 then
    raise EWiRLNotAuthorizedException.Create('Malformed (basic) credentials');

  FAuthContext.Clear;
  LAuthOperation := Authenticate(LAuthData[0], LAuthData[1]);

  FAuthContext.Authenticated := LAuthOperation.Success;
  FAuthContext.Subject.Roles := string.Join(',', LAuthOperation.Roles);

  if not LAuthOperation.Success then
    raise EWiRLNotAuthorizedException.Create('Invalid credentials', 'TWiRLAuthBasicResource', 'DoLogin');

  FAuthContext.Generate(FApplication.Secret);
  Result := GetGeneratedToken;
end;

{ TWiRLAuthResult }

function TWiRLAuthResult.RolesAsString: string;
begin

end;

end.
