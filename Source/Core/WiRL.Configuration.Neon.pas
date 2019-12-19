unit WiRL.Configuration.Neon;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Defaults,

  WiRL.Core.Application,
  WiRL.Configuration.Core,
  Neon.Core.Persistence,
  Neon.Core.Types;

{$SCOPEDENUMS ON}

type
  IWiRLConfigurationNeon = interface(IWiRLConfiguration)
  ['{BD3F569C-5FF0-44E4-A61E-99EF881F0BEA}']
    function SetMembers(AValue: TNeonMembersSet): IWiRLConfigurationNeon;
    function SetMemberCase(AValue: TNeonCase): IWiRLConfigurationNeon;
    function SetMemberCustomCase(AValue: TCaseFunc): IWiRLConfigurationNeon;
    function SetVisibility(AValue: TNeonVisibility): IWiRLConfigurationNeon;
    function SetIgnoreFieldPrefix(AValue: Boolean): IWiRLConfigurationNeon;
    function SetUseUTCDate(AValue: Boolean): IWiRLConfigurationNeon;
    function SetPrettyPrint(AValue: Boolean): IWiRLConfigurationNeon;

    function GetSerializers: TNeonSerializerRegistry;
    function GetNeonConfig: INeonConfiguration;
  end;

  [Implements(IWiRLConfigurationNeon)]
  TWiRLConfigurationNeon = class(TWiRLConfigurationNRef, IWiRLConfigurationNeon)
  private
    FPrettyPrint: Boolean;
    FIgnoreFieldPrefix: Boolean;
    FMemberCustomCase: TCaseFunc;
    FUseUTCDate: Boolean;
    FVisibility: TNeonVisibility;
    FSerializers: TNeonSerializerRegistry;
    FMemberCase: TNeonCase;
    FMembers: TNeonMembersSet;
  public
    constructor Create;
    destructor Destroy; override;

    class function Default: IWiRLConfigurationNeon; static;
    class function Pretty: IWiRLConfigurationNeon; static;
    class function Snake: IWiRLConfigurationNeon; static;
    class function Camel: IWiRLConfigurationNeon; static;
  public
    function SetMembers(AValue: TNeonMembersSet): IWiRLConfigurationNeon;
    function SetMemberCase(AValue: TNeonCase): IWiRLConfigurationNeon;
    function SetMemberCustomCase(AValue: TCaseFunc): IWiRLConfigurationNeon;
    function SetVisibility(AValue: TNeonVisibility): IWiRLConfigurationNeon;
    function SetIgnoreFieldPrefix(AValue: Boolean): IWiRLConfigurationNeon;
    function SetUseUTCDate(AValue: Boolean): IWiRLConfigurationNeon;
    function SetPrettyPrint(AValue: Boolean): IWiRLConfigurationNeon;

    function GetSerializers: TNeonSerializerRegistry;

    function GetNeonConfig: INeonConfiguration;
 published
    property Members: TNeonMembersSet read FMembers write FMembers;
    property MemberCase: TNeonCase read FMemberCase write FMemberCase;
    property MemberCustomCase: TCaseFunc read FMemberCustomCase write FMemberCustomCase;
    property Visibility: TNeonVisibility read FVisibility write FVisibility;
    property IgnoreFieldPrefix: Boolean read FIgnoreFieldPrefix write FIgnoreFieldPrefix;
    property UseUTCDate: Boolean read FUseUTCDate write FUseUTCDate;
    property Serializers: TNeonSerializerRegistry read FSerializers write FSerializers;
  end;

implementation

uses
  System.TypInfo;

{ TWiRLConfigurationNeon }

constructor TWiRLConfigurationNeon.Create;
begin
  inherited;
  FSerializers := TNeonSerializerRegistry.Create;
  SetMemberCase(TNeonCase.PascalCase);
  SetMembers([TNeonMembers.Standard]);
  SetIgnoreFieldPrefix(False);
  SetVisibility([mvPublic, mvPublished]);
  SetUseUTCDate(True);
  SetPrettyPrint(False);
end;

class function TWiRLConfigurationNeon.Default: IWiRLConfigurationNeon;
begin
  Result := TWiRLConfigurationNeon.Create;
end;

class function TWiRLConfigurationNeon.Pretty: IWiRLConfigurationNeon;
begin
  Result := TWiRLConfigurationNeon.Create;
  Result.SetPrettyPrint(True);
end;

class function TWiRLConfigurationNeon.Snake: IWiRLConfigurationNeon;
begin
  Result := TWiRLConfigurationNeon.Create;
  Result.SetIgnoreFieldPrefix(True);
  Result.SetMemberCase(TNeonCase.SnakeCase);
end;

class function TWiRLConfigurationNeon.Camel: IWiRLConfigurationNeon;
begin
  Result := TWiRLConfigurationNeon.Create;
  Result.SetMemberCase(TNeonCase.CamelCase);
end;

destructor TWiRLConfigurationNeon.Destroy;
begin
  FSerializers.Free;
  inherited;
end;

function TWiRLConfigurationNeon.GetNeonConfig: INeonConfiguration;
begin
  Result := TNeonConfiguration.Default;
  Result
   .SetMembers(FMembers)
   .SetMemberCase(FMemberCase)
   .SetMemberCustomCase(FMemberCustomCase)
   .SetVisibility(FVisibility)
   .SetIgnoreFieldPrefix(FIgnoreFieldPrefix)
   .SetUseUTCDate(FUseUTCDate)
   .SetPrettyPrint(FPrettyPrint);

  Result.GetSerializers.Assign(FSerializers);
end;

function TWiRLConfigurationNeon.GetSerializers: TNeonSerializerRegistry;
begin
  Result := FSerializers;
end;


function TWiRLConfigurationNeon.SetIgnoreFieldPrefix(AValue: Boolean): IWiRLConfigurationNeon;
begin
  FIgnoreFieldPrefix := AValue;
  Result := Self;
end;

function TWiRLConfigurationNeon.SetMemberCase(AValue: TNeonCase): IWiRLConfigurationNeon;
begin
  FMemberCase := AValue;
  Result := Self;
end;

function TWiRLConfigurationNeon.SetMemberCustomCase(AValue: TCaseFunc): IWiRLConfigurationNeon;
begin
  FMemberCustomCase := AValue;
  Result := Self;
end;

function TWiRLConfigurationNeon.SetMembers(AValue: TNeonMembersSet): IWiRLConfigurationNeon;
begin
  FMembers := AValue;
  Result := Self;
end;

function TWiRLConfigurationNeon.SetPrettyPrint(AValue: Boolean): IWiRLConfigurationNeon;
begin
  FPrettyPrint := AValue;
  Result := Self;
end;

function TWiRLConfigurationNeon.SetUseUTCDate(AValue: Boolean): IWiRLConfigurationNeon;
begin
  FUseUTCDate := AValue;
  Result := Self;
end;

function TWiRLConfigurationNeon.SetVisibility(AValue: TNeonVisibility): IWiRLConfigurationNeon;
begin
  FVisibility := AValue;
  Result := Self;
end;

initialization

  TWiRLConfigClassRegistry.Instance.RegisterConfigClass(TWiRLConfigurationNeon);

end.