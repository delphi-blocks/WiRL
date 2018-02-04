{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Persistence.Core;

interface

uses
  System.SysUtils, System.Classes, System.Rtti, System.SyncObjs, System.TypInfo,
  WiRL.Persistence.Attributes, WiRL.Persistence.DynamicTypes;

{$SCOPEDENUMS ON}

type
  ENeonException = class(Exception);

  TNeonCase = (LowerCase, UpperCase, PascalCase, CamelCase, SnakeCase, CustomCase);
  TNeonMembersType = (Standard, Fields, Properties);
  TNeonVisibility = set of TMemberVisibility;

  TCaseFunc = reference to function (const AString: string): string;

  TCaseAlgorithm = class
  public
    class function PascalToCamel(const AString: string): string;
    class function CamelToPascal(const AString: string): string;
    class function PascalToSnake(const AString: string): string;
    class function SnakeToPascal(const AString: string): string;
  end;

  TNeonRttiMember = class
  private type
    TNeonMemberType = (Unknown, Prop, Field);
  private
    FMemberType: TNeonMemberType;
    FMember: TRttiMember;
    FInstance: Pointer;
    FAttributes: TArray<TCustomAttribute>;
    FIgnore: Boolean;
    FChosedName: string;

    function MemberAsProperty: TRttiProperty; inline;
    function MemberAsField: TRttiField; inline;

    procedure ParseAttributes;
  public
    constructor Create(AInstance: Pointer; AMember: TRttiMember);

    function Name: string;
    function GetValue: TValue;
    procedure SetValue(const AValue: TValue);
    function MemberType: TRttiType;
    function IsWritable: Boolean;
    function IsReadable: Boolean;
    function TypeKind: TTypeKind;
    function Visibility: TMemberVisibility;
    function IsField: Boolean;
    function IsProperty: Boolean;

    property Attributes: TArray<TCustomAttribute> read FAttributes write FAttributes;

    // Neon-based properties
    property Ignore: Boolean read FIgnore write FIgnore;
    property ChosedName: string read FChosedName write FChosedName;
  end;

  INeonConfiguration = interface
  ['{F82AB790-1C65-4501-915C-0289EFD9D8CC}']
    function SetMemberCase(AValue: TNeonCase): INeonConfiguration;
    function SetUseUTCDate(AValue: Boolean): INeonConfiguration;
    function SetMemberCustomCase(AValue: TCaseFunc): INeonConfiguration;
    function SetMembersType(AValue: TNeonMembersType): INeonConfiguration;
    function SetIgnoreFieldPrefix(AValue: Boolean): INeonConfiguration;
    function SetVisibility(AValue: TNeonVisibility): INeonConfiguration;
  end;

  TNeonConfiguration = class sealed(TInterfacedObject, INeonConfiguration)
  private
    FVisibility: TNeonVisibility;
    FMembersType: TNeonMembersType;
    FMemberCase: TNeonCase;
    FMemberCustomCase: TCaseFunc;
    FIgnoreFieldPrefix: Boolean;
    FUseUTCDate: Boolean;
  public
    constructor Create;

    class function Default: INeonConfiguration; static;
    class function Snake: INeonConfiguration; static;
    class function Camel: INeonConfiguration; static;

    function SetMemberCase(AValue: TNeonCase): INeonConfiguration;
    function SetMemberCustomCase(AValue: TCaseFunc): INeonConfiguration;
    function SetMembersType(AValue: TNeonMembersType): INeonConfiguration;
    function SetIgnoreFieldPrefix(AValue: Boolean): INeonConfiguration;
    function SetVisibility(AValue: TNeonVisibility): INeonConfiguration;
    function SetUseUTCDate(AValue: Boolean): INeonConfiguration;

    property MemberCase: TNeonCase read FMemberCase write FMemberCase;
    property MemberCustomCase: TCaseFunc read FMemberCustomCase write FMemberCustomCase;
    property MembersType: TNeonMembersType read FMembersType write FMembersType;
    property IgnoreFieldPrefix: Boolean read FIgnoreFieldPrefix write FIgnoreFieldPrefix;
    property Visibility: TNeonVisibility read FVisibility write FVisibility;
    property UseUTCDate: Boolean read FUseUTCDate write FUseUTCDate;
  end;

  TNeonBase = class
  protected
    FOriginalInstance: TValue;
    FConfig: TNeonConfiguration;
    FErrors: TStrings;
    function IsOriginalInstance(const AValue: TValue): Boolean;
    function GetTypeMembers(AType: TRttiType): TArray<TRttiMember>;

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
  if not AMember.ChosedName.IsEmpty then
    Exit(AMember.ChosedName);

  if FConfig.IgnoreFieldPrefix and AMember.IsField then
  begin
    if AMember.Name.StartsWith('F') then
      LMemberName := AMember.Name.Substring(1);
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

function TNeonBase.GetTypeMembers(AType: TRttiType): TArray<TRttiMember>;
begin
  SetLength(Result, 0);

  case FConfig.MembersType of
    TNeonMembersType.Standard:
    begin
      if AType.IsRecord then
        Result := TArray<TRttiMember>(AType.AsRecord.GetFields)
      else if AType.IsInstance then
        Result := TArray<TRttiMember>(AType.AsInstance.GetProperties);
    end;

    TNeonMembersType.Properties:
    begin
      if AType.IsRecord then
        Result := TArray<TRttiMember>(AType.AsRecord.GetProperties)
      else if AType.IsInstance then
        Result := TArray<TRttiMember>(AType.AsInstance.GetProperties);
    end;

    TNeonMembersType.Fields:
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
  SetMembersType(TNeonMembersType.Standard);
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

function TNeonConfiguration.SetMembersType(AValue: TNeonMembersType): INeonConfiguration;
begin
  FMembersType := AValue;
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
  FInstance := AInstance;
  FMember := AMember;

  if FMember is TRttiProperty then
    FMemberType := TNeonMemberType.Prop
  else if FMember is TRttiField then
    FMemberType := TNeonMemberType.Field;

  FAttributes := AMember.GetAttributes;
  ParseAttributes;
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

function TNeonRttiMember.MemberType: TRttiType;
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

procedure TNeonRttiMember.ParseAttributes;
var
  LAttribute: TCustomAttribute;
begin
  for LAttribute in FAttributes do
  begin
    if LAttribute is NeonIgnoreAttribute then
      FIgnore := True
    else if LAttribute is NeonPropertyAttribute then
      FChosedName := (LAttribute as NeonPropertyAttribute).Value
  end;
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
begin

end;

end.
