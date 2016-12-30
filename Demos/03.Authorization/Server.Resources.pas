{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Resources;

interface

uses
  System.Classes, System.SysUtils, System.Rtti,

  WiRL.Core.JSON,
  WiRL.Core.Registry,
  WiRL.Core.Attributes,
  WiRL.http.Accept.MediaType,
  WiRL.Core.URL,
  WiRL.Core.MessageBodyWriters,
  WiRL.Core.Request,
  WiRL.Core.Response,
  WiRL.Core.Auth.Context,
  WiRL.Core.Auth.Resource,

  // Only if you want to use a custom (claims) class
  Server.Claims;

type
  TAddress = class
  private
    FCity: string;
    FStreet: string;
    FZipCode: string;
  public
    property City: string read FCity write FCity;
    property Street: string read FStreet write FStreet;
    property ZipCode: string read FZipCode write FZipCode;
  end;
  TAddresses = TArray<TAddress>;

  TUserInfo = class
  private
    FAge: Integer;
    FFullName: string;
    FGroup: Integer;
    FLanguage: string;
    FAddresses: TAddresses;
  public
    constructor Create;
    destructor Destroy; override;

    function AddAddress(const AStreet, ACity, AZip: string): TAddress;

    property Age: Integer read FAge write FAge;
    property FullName: string read FFullName write FFullName;
    property Group: Integer read FGroup write FGroup;
    property Language: string read FLanguage write FLanguage;
    property Addresses: TAddresses read FAddresses write FAddresses;
  end;

  [Path('user')]
  TUserResource = class
  private
    // Injects the auth context into the "Auth" object
    [Context] Auth: TWiRLAuthContext;
    // Injects the custom claims into "Subject" object
    [Context] Subject: TServerClaims;
  public
    [GET, PermitAll]
    [Produces(TMediaType.APPLICATION_JSON)]
    function PublicInfo: TUserInfo;

    [POST, PermitAll]
    [Produces(TMediaType.APPLICATION_JSON)]
    function InsertUser([BodyParam] JSON: TJSONObject): TUserInfo;

    [GET, Path('/details'), RolesAllowed('admin,manager')]
    [Produces(TMediaType.APPLICATION_JSON)]
    function DetailsInfo: TJSONObject;
  end;

  // Inherit the Auth resource from the base class you want to use:

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

implementation

uses
  REST.Json;

{ TUserResource }

function TUserResource.DetailsInfo: TJSONObject;
begin
  Result := TJSONObject.Create;

  Result.AddPair('custom', TJSONString.Create('Admin-level access informations here!'));
  Result.AddPair('subject', Auth.Subject.Clone);
end;

function TUserResource.InsertUser(JSON: TJSONObject): TUserInfo;
begin
  Result := TJson.JsonToObject<TUserInfo>(JSON);

  Result.FullName := 'Luca Minuti';
  Result.Age := Result.Age - 10;
  Result.Language := Subject.Language;
  Result.Group := 2;
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

  // Here you can set all field of your custom claims object
  Subject.Language := 'en-US';
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

  // Here you can set all field of your custom claims object
  Subject.Language := 'it-IT';
  Subject.Expiration := Now + 1;

end;

{ TUserInfo }

function TUserInfo.AddAddress(const AStreet, ACity, AZip: string): TAddress;
begin
  Result := TAddress.Create;
  Result.Street := AStreet;
  Result.City := ACity;
  Result.ZipCode := AZip;

  FAddresses := FAddresses + [Result];
end;

constructor TUserInfo.Create;
begin
  SetLength(FAddresses, 0);
end;

destructor TUserInfo.Destroy;
var
  LAddress: TAddress;
begin
  for LAddress in FAddresses do
    LAddress.Free;

  SetLength(FAddresses, 0);
  inherited;
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TUserResource>;

  // Auth resources
  TWiRLResourceRegistry.Instance.RegisterResource<TFormAuthResource>;
  TWiRLResourceRegistry.Instance.RegisterResource<TBasicAuthResource>;

end.
