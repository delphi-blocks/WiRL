{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2018 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Persistence.Core;

interface

uses
  System.SysUtils, System.Classes, System.Rtti, System.SyncObjs, System.TypInfo,
  System.Generics.Collections,

  WiRL.Persistence.Types,
  WiRL.Persistence.Attributes,
  WiRL.Persistence.DynamicTypes;

type
  TCaseAlgorithm = class
  public
    class function PascalToCamel(const AString: string): string;
    class function CamelToPascal(const AString: string): string;
    class function PascalToSnake(const AString: string): string;
    class function SnakeToPascal(const AString: string): string;
  end;

  INeonConfiguration = interface
  ['{F82AB790-1C65-4501-915C-0289EFD9D8CC}']
    function SetMembers(AValue: TNeonMembers): INeonConfiguration;
    function SetMemberCase(AValue: TNeonCase): INeonConfiguration;
    function SetMemberCustomCase(AValue: TCaseFunc): INeonConfiguration;
    function SetVisibility(AValue: TNeonVisibility): INeonConfiguration;
    function SetIgnoreFieldPrefix(AValue: Boolean): INeonConfiguration;
    function SetUseUTCDate(AValue: Boolean): INeonConfiguration;
  end;

  TNeonConfiguration = class sealed(TInterfacedObject, INeonConfiguration)
  private
    FVisibility: TNeonVisibility;
    FMembers: TNeonMembers;
    FMemberCase: TNeonCase;
    FMemberCustomCase: TCaseFunc;
    FIgnoreFieldPrefix: Boolean;
    FUseUTCDate: Boolean;
  public
    constructor Create;

    class function Default: INeonConfiguration; static;
    class function Snake: INeonConfiguration; static;
    class function Camel: INeonConfiguration; static;

    function SetMembers(AValue: TNeonMembers): INeonConfiguration;
    function SetMemberCase(AValue: TNeonCase): INeonConfiguration;
    function SetMemberCustomCase(AValue: TCaseFunc): INeonConfiguration;
    function SetVisibility(AValue: TNeonVisibility): INeonConfiguration;
    function SetIgnoreFieldPrefix(AValue: Boolean): INeonConfiguration;
    function SetUseUTCDate(AValue: Boolean): INeonConfiguration;

    property Members: TNeonMembers read FMembers write FMembers;
    property MemberCase: TNeonCase read FMemberCase write FMemberCase;
    property MemberCustomCase: TCaseFunc read FMemberCustomCase write FMemberCustomCase;
    property Visibility: TNeonVisibility read FVisibility write FVisibility;
    property IgnoreFieldPrefix: Boolean read FIgnoreFieldPrefix write FIgnoreFieldPrefix;
    property UseUTCDate: Boolean read FUseUTCDate write FUseUTCDate;
  end;

  TNeonRttiObject = class
  protected
    FRttiObject: TRttiObject;
    FNeonInclude: Boolean;
    FAttributes: TArray<TCustomAttribute>;
    FNeonMembers: TNeonMembers;
    FNeonVisibility: TNeonVisibility;
    FNeonIgnore: Boolean;
    FNeonProperty: string;
  protected
    procedure ParseAttributes; virtual;
    function AsRttiType: TRttiType;
  public
    constructor Create(ARttiObject: TRttiObject);
  public
    property Attributes: TArray<TCustomAttribute> read FAttributes write FAttributes;
    // Neon-based properties
    property NeonIgnore: Boolean read FNeonIgnore write FNeonIgnore;
    property NeonInclude: Boolean read FNeonInclude write FNeonInclude;
    property NeonProperty: string read FNeonProperty write FNeonProperty;
    property NeonMembers: TNeonMembers read FNeonMembers write FNeonMembers;
    property NeonVisibility: TNeonVisibility read FNeonVisibility write FNeonVisibility;
  end;

  TNeonRttiMember = class(TNeonRttiObject)
  private
    FMemberType: TNeonMemberType;
    FMember: TRttiMember;
    FInstance: Pointer;
    FSerializable: Boolean;

    function MemberAsProperty: TRttiProperty; inline;
    function MemberAsField: TRttiField; inline;
  public
    constructor Create(AInstance: Pointer; AMember: TRttiMember);

    function Name: string;
    function GetValue: TValue;
    procedure SetValue(const AValue: TValue);
    function RttiType: TRttiType;
    function MemberType: TNeonMemberType;
    function IsWritable: Boolean;
    function IsReadable: Boolean;
    function TypeKind: TTypeKind;
    function Visibility: TMemberVisibility;
    function IsField: Boolean;
    function IsProperty: Boolean;

    property Serializable: Boolean read FSerializable write FSerializable;
  end;

  TNeonRttiMembers = class(TObjectList<TNeonRttiMember>)
  private
    FConfig: TNeonConfiguration;
    FParent: TNeonRttiObject;
  private
    function MatchesVisibility(AVisibility: TMemberVisibility): Boolean;
    function MatchesMemberChoice(AMemberType: TNeonMemberType): Boolean;

  public
    constructor Create(AConfig: TNeonConfiguration; AType: TRttiType);
    destructor Destroy; override;

    procedure FilterSerialize;
    procedure FilterDeserialize;
  end;

  TNeonBase = class
  protected
    FOriginalInstance: TValue;
    FConfig: TNeonConfiguration;
    FErrors: TStrings;
    function IsOriginalInstance(const AValue: TValue): Boolean;
    function GetTypeMembers(AType: TRttiType): TArray<TRttiMember>;
    function GetNeonMembers(AInstance: Pointer; AType: TRttiType): TNeonRttiMembers;
    function GetNameFromMember(AMember: TNeonRttiMember): string; virtual;
  public
    constructor Create(const AConfig: INeonConfiguration);
    destructor Destroy; override;

    procedure LogError(const AMessage: string);
  public
    property Errors: TStrings read FErrors write FErrors;
  end;


implementation

uses
  System.RegularExpressions;

{ TNeonBase }

constructor TNeonBase.Create(const AConfig: INeonConfiguration);
begin
  FConfig := AConfig as TNeonConfiguration;
  FErrors := TStringList.Create;
end;

destructor TNeonBase.Destroy;
begin
  FErrors.Free;
  inherited;
end;

function TNeonBase.GetNameFromMember(AMember: TNeonRttiMember): string;
var
  LMemberName: string;
begin
  if not AMember.NeonProperty.IsEmpty then
    Exit(AMember.NeonProperty);

  if FConfig.IgnoreFieldPrefix and AMember.IsField then
  begin

    if AMember.Name.StartsWith('F', True) and
       (AMember.Visibility in [mvPrivate, mvProtected])
    then
      LMemberName := AMember.Name.Substring(1)
    else
      LMemberName := AMember.Name;
  end
  else
    LMemberName := AMember.Name;

  case FConfig.MemberCase of
    TNeonCase.LowerCase:  Result := LowerCase(LMemberName);
    TNeonCase.UpperCase:  Result := UpperCase(LMemberName);
    TNeonCase.CamelCase:  Result := TCaseAlgorithm.PascalToCamel(LMemberName);
    TNeonCase.SnakeCase:  Result := TCaseAlgorithm.PascalToSnake(LMemberName);
    TNeonCase.PascalCase: Result := LMemberName;
    TNeonCase.CustomCase: Result := FConfig.MemberCustomCase(LMemberName);
  end;
end;

function TNeonBase.GetNeonMembers(AInstance: Pointer; AType: TRttiType): TNeonRttiMembers;
var
  LFields, LProps: TArray<TRttiMember>;
  LMember: TRttiMember;
  LNeonMember: TNeonRttiMember;
begin
  Result := TNeonRttiMembers.Create(FConfig, AType);

  SetLength(LFields, 0);
  SetLength(LProps, 0);

  if AType.IsRecord then
  begin
    LFields := TArray<TRttiMember>(AType.AsRecord.GetFields);
    LProps := TArray<TRttiMember>(AType.AsRecord.GetProperties);
    // GetIndexedProperties
  end
  else if AType.IsInstance then
  begin
    LFields := TArray<TRttiMember>(AType.AsInstance.GetFields);
    LProps := TArray<TRttiMember>(AType.AsInstance.GetProperties);
    // GetIndexedProperties
  end;

  for LMember in LFields do
  begin
    LNeonMember := TNeonRttiMember.Create(AInstance, LMember);
    Result.Add(LNeonMember);
  end;
  for LMember in LProps do
  begin
    LNeonMember := TNeonRttiMember.Create(AInstance, LMember);
    Result.Add(LNeonMember);
  end;
end;

function TNeonBase.GetTypeMembers(AType: TRttiType): TArray<TRttiMember>;
begin
  SetLength(Result, 0);

  case FConfig.Members of
    TNeonMembers.Standard:
    begin
      if AType.IsRecord then
        Result := TArray<TRttiMember>(AType.AsRecord.GetFields)
      else if AType.IsInstance then
        Result := TArray<TRttiMember>(AType.AsInstance.GetProperties);
    end;

    TNeonMembers.Properties:
    begin
      if AType.IsRecord then
        Result := TArray<TRttiMember>(AType.AsRecord.GetProperties)
      else if AType.IsInstance then
        Result := TArray<TRttiMember>(AType.AsInstance.GetProperties);
    end;

    TNeonMembers.Fields:
    begin
      if AType.IsRecord then
        Result := TArray<TRttiMember>(AType.AsRecord.GetFields)
      else if AType.IsInstance then
        Result := TArray<TRttiMember>(AType.AsInstance.GetFields);
    end;
  end;
end;

function TNeonBase.IsOriginalInstance(const AValue: TValue): Boolean;
begin
  if NativeInt(AValue.GetReferenceToRawData^) = NativeInt(FOriginalInstance.GetReferenceToRawData^) then
    Result := True
  else
    Result := False;
end;

procedure TNeonBase.LogError(const AMessage: string);
begin
  FErrors.Add(AMessage);
end;

{ TNeonConfiguration }

constructor TNeonConfiguration.Create;
begin
  SetMemberCase(TNeonCase.PascalCase);
  SetMembers(TNeonMembers.Standard);
  SetIgnoreFieldPrefix(False);
  SetVisibility([mvPublic, mvPublished]);
  SetUseUTCDate(True);
end;

class function TNeonConfiguration.Default: INeonConfiguration;
begin
  Result := TNeonConfiguration.Create;
end;

class function TNeonConfiguration.Camel: INeonConfiguration;
begin
  Result := TNeonConfiguration.Create;

  Result.SetMemberCase(TNeonCase.CamelCase);
end;

class function TNeonConfiguration.Snake: INeonConfiguration;
begin
  Result := TNeonConfiguration.Create;

  Result.SetIgnoreFieldPrefix(True);
  Result.SetMemberCase(TNeonCase.SnakeCase);
end;

function TNeonConfiguration.SetMembers(AValue: TNeonMembers): INeonConfiguration;
begin
  FMembers := AValue;
  Result := Self;
end;

function TNeonConfiguration.SetUseUTCDate(AValue: Boolean): INeonConfiguration;
begin
  FUseUTCDate := AValue;
  Result := Self;
end;

function TNeonConfiguration.SetIgnoreFieldPrefix(AValue: Boolean): INeonConfiguration;
begin
  FIgnoreFieldPrefix := AValue;
  Result := Self;
end;

function TNeonConfiguration.SetMemberCase(AValue: TNeonCase): INeonConfiguration;
begin
  FMemberCase := AValue;
  Result := Self;
end;

function TNeonConfiguration.SetMemberCustomCase(AValue: TCaseFunc): INeonConfiguration;
begin
  FMemberCustomCase := AValue;
  Result := Self;
end;

function TNeonConfiguration.SetVisibility(AValue: TNeonVisibility): INeonConfiguration;
begin
  FVisibility := AValue;
  Result := Self;
end;

{ TNeonRttiMember }

constructor TNeonRttiMember.Create(AInstance: Pointer; AMember: TRttiMember);
begin
  inherited Create(AMember);

  FInstance := AInstance;
  FMember := AMember;

  if FMember is TRttiProperty then
    FMemberType := TNeonMemberType.Prop
  else if FMember is TRttiField then
    FMemberType := TNeonMemberType.Field;
end;

function TNeonRttiMember.GetValue: TValue;
begin
  case FMemberType of
    TNeonMemberType.Unknown: raise ENeonException.Create('Member type must be Field or Property');
    TNeonMemberType.Prop: Result := MemberAsProperty.GetValue(FInstance);
    TNeonMemberType.Field: Result := MemberAsField.GetValue(FInstance);
  end;
end;

function TNeonRttiMember.IsField: Boolean;
begin
  Result := False;
  case FMemberType of
    TNeonMemberType.Field: Result := True;
  end;
end;

function TNeonRttiMember.IsProperty: Boolean;
begin
  Result := False;
  case FMemberType of
    TNeonMemberType.Prop: Result := True;
  end;
end;

function TNeonRttiMember.IsReadable: Boolean;
begin
  Result := False;
  case FMemberType of
    TNeonMemberType.Unknown: raise ENeonException.Create('Member type must be Field or Property');
    TNeonMemberType.Prop: Result := MemberAsProperty.IsReadable;
    TNeonMemberType.Field: Result := True;
  end;
end;

function TNeonRttiMember.IsWritable: Boolean;
begin
  Result := False;
  case FMemberType of
    TNeonMemberType.Unknown: raise ENeonException.Create('Member type must be Field or Property');
    TNeonMemberType.Prop: Result := MemberAsProperty.IsWritable;
    TNeonMemberType.Field: Result := True;
  end;
end;

function TNeonRttiMember.MemberAsField: TRttiField;
begin
  Result := FMember as TRttiField;
end;

function TNeonRttiMember.MemberAsProperty: TRttiProperty;
begin
  Result := FMember as TRttiProperty;
end;

function TNeonRttiMember.MemberType: TNeonMemberType;
begin
  Result := FMemberType;
end;

function TNeonRttiMember.RttiType: TRttiType;
begin
  Result := nil;
  case FMemberType of
    TNeonMemberType.Unknown: raise ENeonException.Create('Member type must be Field or Property');
    TNeonMemberType.Prop: Result := MemberAsProperty.PropertyType;
    TNeonMemberType.Field: Result := MemberAsField.FieldType;
  end;
end;

function TNeonRttiMember.Name: string;
begin
  Result := FMember.Name;
end;

procedure TNeonRttiMember.SetValue(const AValue: TValue);
begin
  case FMemberType of
    TNeonMemberType.Prop: MemberAsProperty.SetValue(FInstance, AValue);
    TNeonMemberType.Field: MemberAsField.SetValue(FInstance, AValue);
  end;
end;

function TNeonRttiMember.TypeKind: TTypeKind;
begin
  Result := tkUnknown;
  case FMemberType of
    TNeonMemberType.Unknown: raise ENeonException.Create('Member type must be Field or Property');
    TNeonMemberType.Prop: Result := MemberAsProperty.PropertyType.TypeKind;
    TNeonMemberType.Field: Result := MemberAsField.FieldType.TypeKind;
  end;
end;

function TNeonRttiMember.Visibility: TMemberVisibility;
begin
  Result := FMember.Visibility
end;

{ TCaseAlgorithm }

class function TCaseAlgorithm.CamelToPascal(const AString: string): string;
var
  LOld, LNew: Char;
begin
  Result := AString;
  if Result.IsEmpty then
    Exit;

  LOld := Result.Chars[0];
  LNew := UpperCase(LOld).Chars[0];

  Result := Result.Replace(LOld, LNew, []);
end;

class function TCaseAlgorithm.PascalToCamel(const AString: string): string;
var
  LOld, LNew: Char;
begin
  Result := AString;
  if Result.IsEmpty then
    Exit;

  LOld := Result.Chars[0];
  LNew := LowerCase(LOld).Chars[0];

  Result := Result.Replace(LOld, LNew, []);
end;

class function TCaseAlgorithm.PascalToSnake(const AString: string): string;
begin
  Result := LowerCase(
    TRegEx.Replace(AString,
    '([A-Z][a-z\d]+)(?=([A-Z][A-Z\a-z\d]+))', '$1_', [])
  );
end;

class function TCaseAlgorithm.SnakeToPascal(const AString: string): string;
var
  LChar: Char;
  LIndex: Integer;
  LSingleWord: string;
  LWords: TArray<string>;
begin
  LWords := AString.Split(['_']);
  for LIndex := 0 to Length(LWords) - 1 do
  begin
    LSingleWord := LWords[LIndex];
    if LSingleWord.IsEmpty then
      Continue;
    LChar := Upcase(LSingleWord.Chars[0]);
    LSingleWord := LSingleWord.Remove(0, 1);
    LSingleWord := LSingleWord.Insert(0, LChar);
    LWords[LIndex] := LSingleWord;
  end;

  Result := string.Join('', LWords);
end;

{ TNeonRttiMembers }

constructor TNeonRttiMembers.Create(AConfig: TNeonConfiguration; AType: TRttiType);
begin
  inherited Create(True);

  FConfig := AConfig;
  FParent := TNeonRttiObject.Create(AType);
end;

destructor TNeonRttiMembers.Destroy;
begin
  FParent.Free;
  inherited;
end;

procedure TNeonRttiMembers.FilterDeserialize;
var
  LMember: TNeonRttiMember;
begin
  for LMember in Self do
  begin
    if LMember.NeonInclude then
    begin
      LMember.Serializable := True;
      Continue;
    end;

    if not LMember.IsWritable then
      Continue;

    if MatchesVisibility(LMember.Visibility) then
    if MatchesMemberChoice(LMember.MemberType) then
      LMember.Serializable := True;
  end;
end;

procedure TNeonRttiMembers.FilterSerialize;
var
  LMember: TNeonRttiMember;
begin
  for LMember in Self do
  begin
    if LMember.NeonInclude then
    begin
      LMember.Serializable := True;
      Continue;
    end;

    { TODO -opaolo -c : Maybe controlled by a config item? 29/06/2018 23:14:17 }
    if SameText(LMember.Name, 'Parent') then
      Continue;

    if SameText(LMember.Name, 'Owner') then
      Continue;

    if not LMember.IsWritable and
       not (LMember.TypeKind in [tkClass, tkInterface]) then
      Continue;

    if LMember.IsReadable then
    if MatchesVisibility(LMember.Visibility) then
    if MatchesMemberChoice(LMember.MemberType) then
      LMember.Serializable := True;
  end;
end;

function TNeonRttiMembers.MatchesMemberChoice(AMemberType: TNeonMemberType): Boolean;
var
  LRttiType: TRttiType;
  LMemberChoice: TNeonMembers;
begin
  Result := False;
  if FParent.NeonMembers = TNeonMembers.Unknown then
    LMemberChoice := FConfig.Members
  else
    LMemberChoice := FParent.NeonMembers;

  if LMemberChoice = TNeonMembers.Standard then
  begin
    LRttiType := FParent.AsRttiType;
    if Assigned(LRttiType) then
    begin
      if LRttiType.IsRecord then
        LMemberChoice := TNeonMembers.Fields;
      if LRttiType.IsInstance then
        LMemberChoice := TNeonMembers.Properties;
    end;
  end;

  case AMemberType of
    //TNeonMemberType.Unknown: Result := False;
    TNeonMemberType.Prop:    Result := TNeonMembers.Properties = LMemberChoice;
    TNeonMemberType.Field:   Result := TNeonMembers.Fields = LMemberChoice;
    //TNeonMemberType.Indexed: Result := False;
  end;
end;

function TNeonRttiMembers.MatchesVisibility(AVisibility: TMemberVisibility): Boolean;
var
  LVisibility: TNeonVisibility;
begin
  Result := False;

  if FParent.NeonVisibility = [] then
    LVisibility := FConfig.Visibility
  else
    LVisibility := FParent.NeonVisibility;

  if AVisibility in LVisibility then
    Result := True;
end;

{ TNeonRttiObject }

function TNeonRttiObject.AsRttiType: TRttiType;
begin
  Result := nil;
  if FRttiObject is TRttiType then
    Result := FRttiObject as TRttiType;
end;

constructor TNeonRttiObject.Create(ARttiObject: TRttiObject);
begin
  FRttiObject := ARttiObject;
  FAttributes := FRttiObject.GetAttributes;

  ParseAttributes;
end;

procedure TNeonRttiObject.ParseAttributes;
var
  LAttribute: TCustomAttribute;
begin
  for LAttribute in FAttributes do
  begin
    if LAttribute is NeonIncludeAttribute then
      FNeonInclude := True
    else if LAttribute is NeonIgnoreAttribute then
      FNeonIgnore := True
    else if LAttribute is NeonPropertyAttribute then
      FNeonProperty := (LAttribute as NeonPropertyAttribute).Value
    else if LAttribute is NeonVisibilityAttribute then
      FNeonVisibility := (LAttribute as NeonVisibilityAttribute).Value
    else if LAttribute is NeonMembersAttribute then
      FNeonMembers := (LAttribute as NeonMembersAttribute).Value
  end;
end;

end.
