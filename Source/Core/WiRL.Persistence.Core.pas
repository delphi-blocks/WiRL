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
  System.SysUtils, System.Classes, System.Rtti, System.SyncObjs, System.TypInfo;

{$SCOPEDENUMS ON}

type
  ENeonException = class(Exception);

  TNeonCase = (LowerCase, UpperCase, CamelCase, PascalCase, SnakeCase);
  TNeonMembers = (Standard, Fields, Properties);
  TNeonVisibility = set of TMemberVisibility;

  TNeonConfiguration = record
  private
    FVisibility: TNeonVisibility;
    FMembers: TNeonMembers;
    FDeserializedCase: TNeonCase;
    FSerializedCase: TNeonCase;
  public
    class function Default: TNeonConfiguration; static;

    property SerializedCase: TNeonCase read FSerializedCase write FSerializedCase;
    property DeserializedCase: TNeonCase read FDeserializedCase write FDeserializedCase;
    property Members: TNeonMembers read FMembers write FMembers;
    property Visibility: TNeonVisibility read FVisibility write FVisibility;
  end;

  TNeonBase = class
  protected
    FOriginalInstance: TValue;
    FConfig: TNeonConfiguration;
    FErrors: TStrings;
    function IsOriginalInstance(const AValue: TValue): Boolean;
    function GetTypeMembers(AType: TRttiType): TArray<TRttiMember>;

    function GetNameFromMember(AMember: TRttiMember): string; virtual;
    function GetNameFromStream(AName: string): string; virtual;
  public
    constructor Create(AConfig: TNeonConfiguration);
    destructor Destroy; override;

    procedure LogError(const AMessage: string);
  public
    property Errors: TStrings read FErrors write FErrors;
  end;

implementation

{ TNeonBase }

constructor TNeonBase.Create(AConfig: TNeonConfiguration);
begin
  FConfig := AConfig;
  FErrors := TStringList.Create;
end;

destructor TNeonBase.Destroy;
begin
  FErrors.Free;
  inherited;
end;

function TNeonBase.GetNameFromMember(AMember: TRttiMember): string;
begin
  { TODO -opaolo -c : check FConfig.*Case 27/06/2017 10:57:47 }
  Result := AMember.Name;
end;

function TNeonBase.GetNameFromStream(AName: string): string;
begin
  { TODO -opaolo -c : check FConfig.*Case 27/06/2017 10:57:47 }
  Result := AName;
end;

function TNeonBase.GetTypeMembers(AType: TRttiType): TArray<TRttiMember>;
begin
  SetLength(Result, 0);

  case FConfig.Members of
    TNeonMembers.Standard:
    begin
      if AType.IsRecord then
        Result := TArray<TRttiMember>(AType.AsInstance.GetFields)
      else if AType.IsInstance then
        Result := TArray<TRttiMember>(AType.AsInstance.GetProperties);
    end;
    TNeonMembers.Properties:
      Result := TArray<TRttiMember>(AType.AsInstance.GetProperties);
    TNeonMembers.Fields:
      Result := TArray<TRttiMember>(AType.AsInstance.GetFields);
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

class function TNeonConfiguration.Default: TNeonConfiguration;
begin
  Result.Members := TNeonMembers.Standard;
  Result.FVisibility := [mvPublic, mvPublished];
end;

end.
