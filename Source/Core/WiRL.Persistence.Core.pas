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

  TNeonCase = (LowerCase, UpperCase, CamelCase, PascalCase);
  TNeonMembers = (Standard, Fields, Properties);
  TNeonVisibility = set of TMemberVisibility;

  TNeonConfiguration = record
  private
    FVisibility: TNeonVisibility;
    FMembers: TNeonMembers;
  public
    class function Default: TNeonConfiguration; static;

    property Members: TNeonMembers read FMembers write FMembers;
    property Visibility: TNeonVisibility read FVisibility write FVisibility;
  end;

  TNeonBase = class
  protected
    FConfig: TNeonConfiguration;
    FErrors: TStrings;
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
