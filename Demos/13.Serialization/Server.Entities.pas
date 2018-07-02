{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2018 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Entities;

interface

uses
  System.SysUtils, System.Classes, System.Contnrs, System.Generics.Collections,

  WiRL.Persistence.Types,
  WiRL.Persistence.Attributes;

{$M+}

type
  TStreamableSample = class
  private
    FPayload: TBytes;
    procedure SetAsString(const Value: string);
  public
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
    function GetAsString: string;
    property AsString: string read GetAsString write SetAsString;
  end;

  TStreamableComposition = class
  private
    FInValue: Integer;
    FStream: TStreamableSample;
  public
    constructor Create;
    destructor Destroy; override;
    property InValue: Integer read FInValue write FInValue;
    property Stream: TStreamableSample read FStream write FStream;
  end;

  TIntArray = TArray<Integer>;

  {$SCOPEDENUMS ON}
  TMyEnum = (First, Second, Third, Fourth);

  TMySet = set of TMyEnum;

  TMyRecord = record
  public
    One: string;
    Two: Integer;

    function ToString: string;
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
    [NeonIgnore]
    property Name: string read FName write FName;
    [NeonProperty('LastName')]
    property Surname: string read FSurname write FSurname;

    property Addresses: TAddresses read FAddresses write FAddresses;
    property DateProp: TDateTime read FDateProp write FDateProp;
    property DoubleProp: Double read FDoubleProp write FDoubleProp;
    property Enum: TMyEnum read FEnum write FEnum;
    property Note: TNote read FNote write FNote;
    property Options: TMySet read FOptions write FOptions;
  end;

  TCaseClass = class
  private
    [NeonInclude]
    FPrivateField: Double;
    FFirstProp: Integer;
    FSecondXProp: string;
    FThirdProp: TDateTime;
  public
    class function DefaultValues: TCaseClass;
  public
    [NeonInclude, NeonMembers(TNeonMembers.Fields)]
    FirstRecord: TMyRecord;
    property FirstProp: Integer read FFirstProp write FFirstProp;
    property SecondXProp: string read FSecondXProp write FSecondXProp;
    property ThirdProp: TDateTime read FThirdProp write FThirdProp;
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
  LAddress.Rec.One := 'Qwerty';
  LAddress.Rec.Two := 12;

  SetLength(FAddresses, Length(FAddresses) + 1);
  FAddresses[Length(FAddresses) - 1] := LAddress;
end;

constructor TPerson.Create;
begin
  FNote := TNote.Create;
  FEnum := TMyEnum.Second;
  FDoubleProp := 56.7870988623;
  FDateProp := Now;
  FOptions := [TMyEnum.First, TMyEnum.Second, TMyEnum.Fourth];
end;

destructor TPerson.Destroy;
var
  LIndex: Integer;
begin
  for LIndex := High(FAddresses) downto Low(FAddresses) do
    FAddresses[LIndex].Free;
  SetLength(FAddresses, 0);
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

{ TMyRecord }

function TMyRecord.ToString: string;
begin
  Result := One + '|' + Two.ToString;
end;

{ TStreamableSample }

function TStreamableSample.GetAsString: string;
begin
  Result := TEncoding.UTF8.GetString(FPayload);
end;

procedure TStreamableSample.LoadFromStream(AStream: TStream);
begin
  AStream.Position := soFromBeginning;
  SetLength(FPayload, AStream.Size);
  AStream.Read(FPayload[0], AStream.Size);
end;

procedure TStreamableSample.SaveToStream(AStream: TStream);
begin
  AStream.Position := soFromBeginning;
  AStream.Write(FPayload[0], Length(FPayload));
end;

procedure TStreamableSample.SetAsString(const Value: string);
begin
  FPayload := TEncoding.UTF8.GetBytes(Value);
end;

{ TStreamableComposition }

constructor TStreamableComposition.Create;
begin
  FStream := TStreamableSample.Create;
end;

destructor TStreamableComposition.Destroy;
begin
  FStream.Free;
  inherited;
end;

{ TCaseClass }

class function TCaseClass.DefaultValues: TCaseClass;
begin
  Result := TCaseClass.Create;
  Result.FPrivateField := 3.1415926535;
  Result.FirstRecord.One := 'Record text field';
  Result.FirstRecord.Two := Random(1000);
  Result.FirstProp := Random(1000);
  Result.SecondXProp := 'ABCDEFG';
  Result.ThirdProp := EncodeDate(2018, Random(11)+1, Random(27)+1);
end;

end.
