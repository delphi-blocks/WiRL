{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2025 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Auth.Resource;

interface

uses
  System.Classes, System.SysUtils,

  WiRL.Core.Classes,
  WiRL.Core.Registry,
  WiRL.Core.Attributes,
  WiRL.Core.Application,
  WiRL.Core.Declarations,
  WiRL.http.Accept.MediaType,
  WiRL.Core.MessageBodyWriter,
  WiRL.Core.Auth.Context,

  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON.Schema,
  Neon.Core.Attributes;

type
  [JsonSchema('title=LoginResponse')]
  TWiRLLoginResponse = class
  private
    FSuccess: Boolean;
    FAccessToken: string;
  public
    constructor Create(ASuccess: Boolean; const AToken: string); overload;
    [NeonProperty('success')]
    property Success: Boolean read FSuccess write FSuccess;
    [NeonProperty('access_token')]
    property AccessToken: string read FAccessToken write FAccessToken;
  end;

  TWiRLAuthResponse = record
    HttpStatus: Integer;
    HttpReason: string;
    HttpFollowLink: string;
  end;

  TWiRLAuthPrincipal = class
  private
    FPassword: string;
    FUserName: string;
  public
    [NeonProperty('username')]
    property UserName: string read FUserName write FUserName;
    [NeonProperty('password')]
    property Password: string read FPassword write FPassword;
  end;

  TWiRLAuthResult = record
  public
    Success: Boolean;
    Roles: TArray<string>;
    CustomResponse: TWiRLAuthResponse;
    function RolesAsString: string;
    class function DefaultResult: TWiRLAuthResult; static;
  end;

  /// <summary>
  /// Base class for the authentication resource
  /// </summary>
  TWiRLAuthResource = class
  protected
    const ERR_AUTH_INVALID = 'Invalid credentials';
  protected
    [Context] FAuthContext: TWiRLAuthContext;
    [Context] FApplication: TWiRLApplication;
    function Authenticate(const AUserName, APassword: string): TWiRLAuthResult; virtual; abstract;
    function CreateResponse(ASuccess: Boolean; const AToken: string): TWiRLLoginResponse; virtual;
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
      [FormParam('username')] const AUserName: string;
      [FormParam('password')] const APassword: string
    ): TWiRLLoginResponse;
  end;

  /// <summary>
  /// HTTP basic authentication resource
  /// </summary>
  TWiRLAuthBasicResource = class(TWiRLAuthResource)
  private
    const ERR_BASIC_MALFORMED = 'Malformed (basic) credentials';
    const ERR_BASIC_HEADER = 'Auhtorization header incorrect';
    const AUTH_BASIC = 'Basic ';
  public
    [BasicAuth] [POST, Produces(TMediaType.APPLICATION_JSON)]
    function DoLogin([HeaderParam('Authorization')] const AAuth: string): TWiRLLoginResponse;
  end;

  /// <summary>
  ///   HTTP Body (JSON) authentication resource
  /// </summary>
  /// <remarks>
  ///   The JSON properties mus be named "username" and "password". If you want custom
  ///   names you must inherit the base class
  /// </remarks>
  TWiRLAuthBodyResource = class(TWiRLAuthResource)
  public
    [POST, Consumes(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_JSON)]
    function DoLogin([BodyParam] APrincipal: TWiRLAuthPrincipal): TWiRLLoginResponse;
  end;

implementation

uses
  System.DateUtils,
  JOSE.Encoding.Base64,
  WiRL.Configuration.JWT,
  WiRL.Core.Exceptions;

{ TWiRLAuthFormResource }

function TWiRLAuthFormResource.DoLogin(const AUserName, APassword: string): TWiRLLoginResponse;
var
  LAuthOperation: TWiRLAuthResult;
begin
  FAuthContext.Clear;
  LAuthOperation := Authenticate(AUserName, APassword);

  FAuthContext.Subject.Roles := string.Join(',', LAuthOperation.Roles);

  if not LAuthOperation.Success then
    raise EWiRLNotAuthorizedException.Create(ERR_AUTH_INVALID, Self.ClassName, 'DoLogin');

  FAuthContext.Generate(FApplication.GetConfiguration<TWiRLConfigurationJWT>.KeyPair.PrivateKey.Key);

  Result := CreateResponse(LAuthOperation.Success, FAuthContext.CompactToken);
end;

{ TWiRLAuthBasicResource }

function TWiRLAuthBasicResource.DoLogin(const AAuth: string): TWiRLLoginResponse;
var
  LAuthOperation: TWiRLAuthResult;
  LAuthData: TBasicAuth;
begin
  FAuthContext.Clear;

  LAuthData := AAuth;
  LAuthOperation := Authenticate(LAuthData.User, LAuthData.Password);

  FAuthContext.Subject.Roles := string.Join(',', LAuthOperation.Roles);

  if not LAuthOperation.Success then
  begin
    if LAuthOperation.CustomResponse.HttpReason.IsEmpty then
    begin
      raise EWiRLNotAuthorizedException.Create(ERR_AUTH_INVALID, Self.ClassName, 'DoLogin');
    end
    else
      raise EWiRLWebApplicationException.Create(
        LAuthOperation.CustomResponse.HttpReason,
        LAuthOperation.CustomResponse.HttpStatus,
        [Pair.S('follow-link', LAuthOperation.CustomResponse.HttpFollowLink)]
      );
  end;

  FAuthContext.Generate(FApplication.GetConfiguration<TWiRLConfigurationJWT>.KeyPair.PrivateKey.Key);

  Result := CreateResponse(LAuthOperation.Success, FAuthContext.CompactToken);
end;

{ TWiRLAuthBodyResource }

function TWiRLAuthBodyResource.DoLogin(APrincipal: TWiRLAuthPrincipal): TWiRLLoginResponse;
var
  LAuthOperation: TWiRLAuthResult;
begin
  FAuthContext.Clear;
  LAuthOperation := Authenticate(APrincipal.UserName, APrincipal.Password);

  FAuthContext.Subject.Roles := string.Join(',', LAuthOperation.Roles);

  if not LAuthOperation.Success then
    raise EWiRLNotAuthorizedException.Create(ERR_AUTH_INVALID, Self.ClassName, 'DoLogin');

  FAuthContext.Generate(FApplication.GetConfiguration<TWiRLConfigurationJWT>.KeyPair.PrivateKey.Key);

  Result := CreateResponse(LAuthOperation.Success, FAuthContext.CompactToken);
end;

{ TWiRLAuthResult }

class function TWiRLAuthResult.DefaultResult: TWiRLAuthResult;
begin
  Result.Success := False;
  Result.Roles := [];
  Result.CustomResponse.HttpStatus := 0;
  Result.CustomResponse.HttpReason := '';
  Result.CustomResponse.HttpFollowLink := '';
end;

function TWiRLAuthResult.RolesAsString: string;
var
  LRole: string;
begin
  Result := '';
  for LRole in Roles do
    Result := Result + ',' + LRole;
end;

{ TWiRLLoginResponse }

constructor TWiRLLoginResponse.Create(ASuccess: Boolean; const AToken: string);
begin
  FSuccess := ASuccess;
  FAccessToken := AToken;
end;

{ TWiRLAuthResource }

function TWiRLAuthResource.CreateResponse(ASuccess: Boolean;
  const AToken: string): TWiRLLoginResponse;
begin
  Result := TWiRLLoginResponse.Create(ASuccess, AToken);
end;

end.

