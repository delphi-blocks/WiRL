{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2021 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Entities.Customer;

interface

uses
  System.SysUtils, System.Classes, System.Contnrs, System.Generics.Collections,
  System.Math, System.Math.Vectors, System.Types,

  Neon.Core.Types,
  Neon.Core.Attributes;

{$M+}

type
  TCustomer = class
  private
    FCompanyName: string;
    FID: Integer;
  public
    property ID: Integer read FID write FID;
    property CompanyName: string read FCompanyName write FCompanyName;

  end;

  TAddress = class
  private
    FCity: string;
    FCountry: string;
  published
    property City: string read FCity write FCity;
    property Country: string read FCountry write FCountry;
  end;

  TAddresses = TArray<TAddress>;
  TAddressList = TList<TAddress>;

  TNote = class
  private
    FDate: TDateTime;
    FText: string;
  public
    constructor Create(ADate: TDateTime; const AText: string); overload;
  published
    property Date: TDateTime read FDate write FDate;
    property Text: string read FText write FText;
  end;

  TPerson = class
  private
    FAddresses: TAddresses;
    FBirthdate: TDateTime;
    FName: string;
    FSurname: string;
    FNotes: TObjectDictionary<string, TNote>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddAddress(const ACity, ACountry: string);
  published
    property Name: string read FName write FName;
    [NeonProperty('LastName')]
    property Surname: string read FSurname write FSurname;
    property Birthdate: TDateTime read FBirthdate write FBirthdate;

    property Addresses: TAddresses read FAddresses write FAddresses;
    property Notes: TObjectDictionary<string, TNote> read FNotes write FNotes;
  end;

  TPersons = class(TObjectList<TPerson>);

  TAddressBook = class
  private
    FOwner: string;
    FPurpose: string;
    FContacts: TPersons;
  public
    constructor Create;
    destructor Destroy; override;

    property Owner: string read FOwner write FOwner;
    property Purpose: string read FPurpose write FPurpose;
    property Contacts: TPersons read FContacts write FContacts;
  end;

implementation

{ TPerson }

procedure TPerson.AddAddress(const ACity, ACountry: string);
var
  LAddress: TAddress;
begin
  LAddress := TAddress.Create;
  LAddress.City := ACity;
  LAddress.Country:= ACountry;

  SetLength(FAddresses, Length(FAddresses) + 1);
  FAddresses[Length(FAddresses) - 1] := LAddress;
end;

constructor TPerson.Create;
begin
  FBirthdate := Now;
  FNotes := TObjectDictionary<string, TNote>.Create([doOwnsValues]);
end;

destructor TPerson.Destroy;
var
  LIndex: Integer;
begin
  for LIndex := High(FAddresses) downto Low(FAddresses) do
    FAddresses[LIndex].Free;
  SetLength(FAddresses, 0);
  FNotes.Free;
  inherited;
end;

{ TNote }

constructor TNote.Create(ADate: TDateTime; const AText: string);
begin
  FDate := ADate;
  FText := AText;
end;

{ TAddressBook }

constructor TAddressBook.Create;
begin
  FContacts := TPersons.Create(True);
end;

destructor TAddressBook.Destroy;
begin
  FContacts.Free;
  inherited;
end;

end.
