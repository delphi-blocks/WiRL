{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
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
  WiRL.Persistence.Core,
  WiRL.Persistence.Attributes,
  WiRL.http.Accept.MediaType,
  WiRL.Core.MessageBodyWriter,
  WiRL.Core.Auth.Context,
  WiRL.http.URL;

type
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
      [FormParam('username')] const AUserName: string;
      [FormParam('password')] const APassword: string
    ): TJSONObject;
  end;

  /// <summary>
  /// HTTP basic authentication resource
  /// </summary>
  TWiRLAuthBasicResource = class(TWiRLAuthResource)
  private
    const ERR_BASIC_MALFORMED = 'Malformed (basic) credentials';
    const ERR_BASIC_HEADER = 'Auhtorization header incorrect';
    const AUTH_BASIC = 'Basic ';
  protected
    function ExtractData(const AAuth: string): TArray<string>;
  public
    [POST, Produces(TMediaType.APPLICATION_JSON)]
    function DoLogin([HeaderParam('Authorization')] const AAuth: string): TJSONObject;
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
    function DoLogin([BodyParam] APrincipal: TWiRLAuthPrincipal): TJSONObject;
  end;

implementation

uses
  System.DateUtils,
  JOSE.Encoding.Base64,
  WiRL.Core.Exceptions;

{ TWiRLAuthResource }

function TWiRLAuthResource.GetGeneratedToken: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('success', TJSONTrue.Create);
  Result.AddPair('access_token', TJSONString.Create(FAuthContext.CompactToken));
end;

{ TWiRLAuthFormResource }

function TWiRLAuthFormResource.DoLogin(const AUserName, APassword: string): TJSONObject;
var
  LAuthOperation: TWiRLAuthResult;
begin
  FAuthContext.Clear;
  LAuthOperation := Authenticate(AUserName, APassword);

  FAuthContext.Subject.Roles := string.Join(',', LAuthOperation.Roles);

  if not LAuthOperation.Success then
    raise EWiRLNotAuthorizedException.Create(ERR_AUTH_INVALID, Self.ClassName, 'DoLogin');

  FAuthContext.Generate(FApplication.Secret);
  Result := GetGeneratedToken;
end;

{ TWiRLAuthBasicResource }

function TWiRLAuthBasicResource.DoLogin(const AAuth: string): TJSONObject;
var
  LAuthOperation: TWiRLAuthResult;
  LAuthData: TArray<string>;
begin
  FAuthContext.Clear;

  LAuthData := ExtractData(AAuth);
  LAuthOperation := Authenticate(LAuthData[0], LAuthData[1]);

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

  FAuthContext.Generate(FApplication.Secret);
  Result := GetGeneratedToken;
end;

function TWiRLAuthBasicResource.ExtractData(const AAuth: string): TArray<string>;
var
  LAuthField: string;
begin
  if not AAuth.StartsWith(AUTH_BASIC) then
    raise EWiRLNotAuthorizedException.Create(ERR_BASIC_HEADER, Self.ClassName, 'DoLogin');

  LAuthField := AAuth.Substring(AUTH_BASIC.Length);
  LAuthField := TBase64.Decode(LAuthField);
  Result := LAuthField.Split([':']);

  if Length(Result) < 2 then
    raise EWiRLNotAuthorizedException.Create(ERR_BASIC_MALFORMED, Self.ClassName, 'DoLogin');
end;

{ TWiRLAuthBodyResource }

function TWiRLAuthBodyResource.DoLogin(APrincipal: TWiRLAuthPrincipal): TJSONObject;
var
  LAuthOperation: TWiRLAuthResult;
begin
  FAuthContext.Clear;
  LAuthOperation := Authenticate(APrincipal.UserName, APrincipal.Password);

  FAuthContext.Subject.Roles := string.Join(',', LAuthOperation.Roles);

  if not LAuthOperation.Success then
    raise EWiRLNotAuthorizedException.Create(ERR_AUTH_INVALID, Self.ClassName, 'DoLogin');

  FAuthContext.Generate(FApplication.Secret);
  Result := GetGeneratedToken;
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

end.
