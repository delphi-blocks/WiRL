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
  MARS.Core.Token.Resource;

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
  TSecuredResource = class
  private
    [Context] Auth: TMARSAuthContext;
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
  protected
    function Authenticate(const AUserName, APassword: string): Boolean; override;
  end;

  [Path('form_auth')]
  TFormAuthResource = class(TMARSAuthFormResource)
  protected
    function Authenticate(const AUserName, APassword: string): Boolean; override;
  end;

implementation

{ TSecuredResource }

function TSecuredResource.DetailsInfo: TJSONObject;
begin
  Result := TJSONObject.Create;

  Result.AddPair('custom', TJSONString.Create('Admin-level access informations here!'));
  Result.AddPair('subject', Auth.Subject.Clone);
end;

function TSecuredResource.PublicInfo: TUserInfo;
begin
  Result := TUserInfo.Create;

  Result.FullName := 'Paolo Rossi';
  Result.Age := 46;
  Result.Language := 'it-IT';
  Result.Group := 10;

  Result.AddAddress('Via Castello', 'Piacenza', '29021');
  Result.AddAddress('Via Trento', 'Parma', '43122');
end;

{ TBasicAuthResource }

function TBasicAuthResource.Authenticate(const AUserName, APassword: string): Boolean;
begin
  Result := SameText(APassword, 'mypassword');
end;

{ TFormAuthResource }

function TFormAuthResource.Authenticate(const AUserName,
  APassword: string): Boolean;
begin
  Result := SameText(APassword, 'mypassword');
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

end;

destructor TUserInfo.Destroy;
var
  LAddress: TAddress;
begin
  for LAddress in FAddresses do
    LAddress.Free;

  FAddresses := [];
  inherited;
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<TSecuredResource>;

  // Auth resource
  TMARSResourceRegistry.Instance.RegisterResource<TFormAuthResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TBasicAuthResource>;

end.
