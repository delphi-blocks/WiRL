{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Tests.Mock.Validators;

interface

uses
  System.SysUtils, System.Classes, System.Rtti, System.JSON,
  WiRL.Core.Context,
  WiRL.Core.Context.Server,
  WiRL.Core.Validators;

type
  HasNameAttribute = class(TCustomConstraintAttribute)
  end;

  THasNameValidator = class(TInterfacedObject, IConstraintValidator<HasNameAttribute>)
  public
    procedure Initialize(AHasNameAttribute: HasNameAttribute);
    function IsValid(AValue: TValue; Context: TWiRLContext): Boolean;
  end;

implementation

{ THasNameValidator }

procedure THasNameValidator.Initialize(AHasNameAttribute: HasNameAttribute);
begin
end;

function THasNameValidator.IsValid(AValue: TValue;
  Context: TWiRLContext): Boolean;
var
  Json :TJSONObject;
begin
  Result := False;
  if AValue.AsObject <> nil then
  begin
    Json := AValue.AsObject as TJSONObject;
    Result := Json.Values['name'] <> nil;
  end;
end;

initialization
  TWiRLValidatorRegistry.Instance.RegisterValidator<THasNameValidator>;

end.
