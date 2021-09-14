{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2021 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Resources;

{$I WiRL.inc}

interface

uses
  System.Classes, System.SysUtils, System.Rtti,

  WiRL.Core.Application,
  WiRL.Core.Registry,
  WiRL.Core.Attributes,
  WiRL.http.Accept.MediaType,
  WiRL.http.URL,
  WiRL.Core.MessageBody.Default,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.Core.Auth.Context,
  WiRL.Core.Auth.Resource,

  Common.Entities,
  Server.Auth.Resource,
  // Only if you want to use a custom (claims) class
  Server.Claims;

type
  TDetailsInfo = class
  private
    FClaims: TServerClaims;
    FMessage: string;
  public
    constructor Create(const AMessage: string; AClaims: TServerClaims);
    property Message: string read FMessage write FMessage;
    property Claims: TServerClaims read FClaims write FClaims;
  end;

  [Path('user')]
  TUserResource = class
  protected
    // Injects the auth context into the "Auth" object
    [Context] Auth: TWiRLAuthContext;

    // Injects the custom claims into "Subject" object
    [Context] Subject: TServerClaims;
  public
    [GET]
    [Produces(TMediaType.APPLICATION_JSON)]
    function PublicInfo: TUserInfo;

    [POST, RolesAllowed('admin,manager')]
    [Produces(TMediaType.APPLICATION_JSON)]
    function InsertUser([BodyParam] AUser: TUserInfo): TUserInfo;

    [GET, Path('/details'), RolesAllowed('admin,manager')]
    [Produces(TMediaType.APPLICATION_JSON)]
    function DetailsInfo: TDetailsInfo;
  end;

  // *********************************************************************
  // Inherit (only one) Auth resource from the base class you want to use:
  // *********************************************************************

  [Path('basic_auth')]
  TBasicAuthResource = class(TWiRLAuthBasicResource)
  private
    // Injects the custom claims into "Subject" field
    [Context] Subject: TServerClaims;
  protected
    function Authenticate(const AUserName, APassword: string): TWiRLAuthResult; override;
  end;

  [Path('form_auth')]
  TFormAuthResource = class(TWiRLAuthFormResource)
  private
    // Injects the custom claims into "Subject" field
    [Context] Subject: TServerClaims;
  protected
    function Authenticate(const AUserName, APassword: string): TWiRLAuthResult; override;
  end;

  [Path('body_auth')]
  TBodyAuthResource = class(TWiRLAuthBodyResource)
  private
    // Injects the custom claims into "Subject" field
    [Context] Subject: TServerClaims;
  protected
    function Authenticate(const AUserName, APassword: string): TWiRLAuthResult; override;
  end;

implementation

uses
  System.DateUtils;

{ TUserResource }

function TUserResource.DetailsInfo: TDetailsInfo;
begin
  Result := TDetailsInfo.Create('Admin-level access informations', Subject);
end;

function TUserResource.InsertUser(AUser: TUserInfo): TUserInfo;
begin
  Result := TUserInfo.Create;

  Result.FullName := AUser.FullName;
  Result.Age := AUser.Age;
  Result.Language := AUser.Language;
  Result.Group := AUser.Group;
end;

function TUserResource.PublicInfo: TUserInfo;
begin
  Result := TUserInfo.Create;

  Result.FullName := 'Paolo Rossi';
  Result.Age := 47;
  Result.Language := Subject.Language;
  Result.Group := 10;

  Result.AddAddress('Via Castello', 'Piacenza', '29121');
  Result.AddAddress('Via Trento', 'Parma', '43122');
end;

{ TBasicAuthResource }

function TBasicAuthResource.Authenticate(const AUserName, APassword: string): TWiRLAuthResult;
begin
  // The line below is only an example, you have to replace with
  // your (server) authentication code (database, another service, etc...)
  Result.Success := SameText(APassword, 'mypassword');

  // The line below is only an example, you have to replace with roles
  // retrieved from the server
  if SameText(AUserName, 'admin') or SameText(AUserName, 'paolo') then
    Result.Roles := 'admin,manager,user'.Split([','])
  else
    Result.Roles := 'user,manager'.Split([',']);

  // Here you can set all claims of the JWT
  Subject.Expiration := IncSecond(Now(), 30);
  Subject.UserID := AUserName;

  // Here you can set all custom claims of the JWT
  Subject.Language := 'it-IT';

end;

{ TFormAuthResource }

function TFormAuthResource.Authenticate(const AUserName, APassword: string): TWiRLAuthResult;
begin
  // The line below is only an example, you have to replace with
  // your (server) authentication code (database, another service, etc...)
  Result.Success := SameText(APassword, 'mypassword');

  // The line below is only an example, you have to replace with roles
  // retrieved from the server
  if SameText(AUserName, 'admin') or SameText(AUserName, 'paolo') then
    Result.Roles := 'user,manager,admin'.Split([','])
  else
    Result.Roles := 'user,manager'.Split([',']);

  // Here you can set all claims of the JWT
  Subject.Expiration := IncSecond(Now(), 30);
  Subject.UserID := AUserName;

  // Here you can set all custom claims of the JWT
  Subject.Language := 'it-IT';
end;

{ TBodyAuthResource }

function TBodyAuthResource.Authenticate(const AUserName, APassword: string): TWiRLAuthResult;
begin
  // The line below is only an example, you have to replace with
  // your (server) authentication code (database, another service, etc...)
  Result.Success := SameText(APassword, 'mypassword');

  // The line below is only an example, you have to replace with roles
  // retrieved from the server
  if SameText(AUserName, 'admin') or SameText(AUserName, 'paolo') then
    Result.Roles := 'user,manager,admin'.Split([','])
  else
    Result.Roles := 'user,manager'.Split([',']);

  // Here you can set all claims of the JWT
  Subject.Expiration := IncSecond(Now(), 30);
  Subject.UserID := AUserName;

  // Here you can set all custom claims of the JWT
  Subject.Language := 'it-IT';
end;

{ TDetailsInfo }

constructor TDetailsInfo.Create(const AMessage: string; AClaims: TServerClaims);
begin
  FMessage := AMessage;
  FClaims := AClaims;
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TUserResource>;

  // Auth resources
  TWiRLResourceRegistry.Instance.RegisterResource<TFormAuthResource>;
  TWiRLResourceRegistry.Instance.RegisterResource<TBasicAuthResource>;
  TWiRLResourceRegistry.Instance.RegisterResource<TBodyAuthResource>;

end.
