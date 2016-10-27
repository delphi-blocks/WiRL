(*
  Copyright 2015-2016, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit Server.Resources;

interface

uses
  System.Classes, System.SysUtils, System.Rtti,

  MARS.Core.JSON,
  MARS.Core.Registry,
  MARS.Core.Attributes,
  MARS.Core.MediaType,
  MARS.Core.URL,
  MARS.Core.MessageBodyWriters,
  MARS.Core.Request,
  MARS.Core.Response,
  MARS.Core.Token,
  MARS.Core.Token.Resource,

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
    [Context] Auth: TMARSAuthContext;
    // Injects the custom claims into "Subject" object
    [Context] Subject: TServerClaims;
  public
    [GET, PermitAll]
    [Produces(TMediaType.APPLICATION_JSON)]
    function PublicInfo: TUserInfo;

    [GET, Path('/details'), RolesAllowed('admin')]
    [Produces(TMediaType.APPLICATION_JSON)]
    function DetailsInfo: TJSONObject;
  end;

  // Inherit the Auth resource from the base class you want to use:

  [Path('basic_auth')]
  TBasicAuthResource = class(TMARSAuthBasicResource)
  private
    // Injects the custom claims into "Subject" field
    [Context] Subject: TServerClaims;
  protected
    function Authenticate(const AUserName, APassword: string): TMARSAuthResult; override;
  end;

  [Path('form_auth')]
  TFormAuthResource = class(TMARSAuthFormResource)
  private
    // Injects the custom claims into "Subject" field
    [Context] Subject: TServerClaims;
  protected
    function Authenticate(const AUserName, APassword: string): TMARSAuthResult; override;
  end;

implementation

{ TUserResource }

function TUserResource.DetailsInfo: TJSONObject;
begin
  Result := TJSONObject.Create;

  Result.AddPair('custom', TJSONString.Create('Admin-level access informations here!'));
  Result.AddPair('subject', Auth.Subject.Clone);
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

function TBasicAuthResource.Authenticate(const AUserName, APassword: string): TMARSAuthResult;
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
  Subject.Language := 'en-US';
end;

{ TFormAuthResource }

function TFormAuthResource.Authenticate(const AUserName, APassword: string): TMARSAuthResult;
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
  TMARSResourceRegistry.Instance.RegisterResource<TUserResource>;

  // Auth resources
  TMARSResourceRegistry.Instance.RegisterResource<TFormAuthResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TBasicAuthResource>;

end.
