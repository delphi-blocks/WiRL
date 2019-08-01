{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.REST.Entities;

interface

uses
  System.SysUtils, System.Classes, System.Contnrs, System.Generics.Collections,
  Neon.Core.Attributes;

{$M+}

type
  TAddress = class
  private
    FCity: string;
    FCountry: string;
  published
    property City: string read FCity write FCity;
    property Country: string read FCountry write FCountry;
  end;

  TAddresses = TArray<TAddress>;

  TNotes = class
  private
    FDate: TDateTime;
    FText: string;
  published
    property Date: TDateTime read FDate write FDate;
    property Text: string read FText write FText;
  end;

  TPerson = class
  private
    FAddresses: TAddresses;
    FBirthday: TDateTime;
    FFirstName: string;
    FNotes: TNotes;
    FLastName: string;
    FRandomNumber: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddAddress(const ACity, ACountry: string);
  published
    property FirstName: string read FFirstName write FFirstName;
    property LastName: string read FLastName write FLastName;
    property Birthday: TDateTime read FBirthday write FBirthday;
    property Notes: TNotes read FNotes write FNotes;
    property Addresses: TAddresses read FAddresses write FAddresses;
    property RandomNumber: Integer read FRandomNumber write FRandomNumber;
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
  FNotes := TNotes.Create;
  FBirthday := Now;
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

end.
