unit Server.Entities;

interface

uses
  System.SysUtils, System.Classes, System.Contnrs, System.Generics.Collections;

{$M+}

type

  TIntArray = TArray<Integer>;

  {$SCOPEDENUMS ON}
  TMyEnum = (Primo, Secondo, Terzo, Quarto);

  TMySet = set of TMyEnum;

  TMyRecord = record
  public
    Uno: string;
    Due: Integer;
  end;

  TAddress = class
  private
    FCity: string;
    FCountry: string;
  public
    Rec: TMyRecord;
  published
    property City: string read FCity write FCity;
    property Country: string read FCountry write FCountry;
  end;

  TAddresses = TArray<TAddress>;
  TAddressList = TList<TAddress>;

  TAddressBook = class
  private
    FAddressList: TAddressList;
    FNoteList: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(ACity, ACountry: string): TAddress;
  published
    property AddressList: TAddressList read FAddressList write FAddressList;
    property NoteList: TStringList read FNoteList write FNoteList;
  end;

  TNote = class
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
    FDateProp: TDateTime;
    FDoubleProp: Double;
    FEnum: TMyEnum;
    FName: string;
    FNote: TNote;
    FOptions: TMySet;
    FSurname: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddAddress(const ACity, ACountry: string);
  published

    property Name: string read FName write FName;
    property Surname: string read FSurname write FSurname;

    property Addresses: TAddresses read FAddresses write FAddresses;
    property DateProp: TDateTime read FDateProp write FDateProp;
    property DoubleProp: Double read FDoubleProp write FDoubleProp;
    property Enum: TMyEnum read FEnum write FEnum;
    property Note: TNote read FNote write FNote;
    property Options: TMySet read FOptions write FOptions;
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
  LAddress.Rec.Uno := 'Pippo';
  LAddress.Rec.Due := 12;

  FAddresses := FAddresses + [LAddress];
end;

constructor TPerson.Create;
begin
  FNote := TNote.Create;
  FEnum := TMyEnum.Secondo;
  FDoubleProp := 56.7870988623;
  FDateProp := Now;
  FOptions := [TMyEnum.Primo, TMyEnum.Secondo, TMyEnum.Quarto];
end;

destructor TPerson.Destroy;
var
  LIndex: Integer;
begin
  for LIndex := High(FAddresses) downto Low(FAddresses) do
    FAddresses[LIndex].Free;

  FNote.Free;
  inherited;
end;

{ TAddressBook }

function TAddressBook.Add(ACity, ACountry: string): TAddress;
begin
  Result := TAddress.Create;
  Result.City := ACity;
  Result.Country := ACountry;
  FAddressList.Add(Result);
end;

constructor TAddressBook.Create;
begin
  FAddressList := TAddressList.Create;
  FNoteList := TStringList.Create;
end;

destructor TAddressBook.Destroy;
var
  LAddress: TObject;
begin
  for LAddress in FAddressList do
  begin
    LAddress.Free;
  end;
  FAddressList.Free;
  FNoteList.Free;
  inherited;
end;

end.
