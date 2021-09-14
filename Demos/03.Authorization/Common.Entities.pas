unit Common.Entities;

interface

uses
  System.SysUtils, System.Classes;

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

implementation

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

end.
