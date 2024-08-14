{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2023 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.Client.Editor;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  DesignIntf, DesignEditors,

  WiRL.http.Client,
  WiRL.http.Client.Interfaces;

Type
  TClientVendorProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure Register;

implementation

{ TClientVendorProperty }

function TClientVendorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TClientVendorProperty.GetValues(Proc: TGetStrProc);
var
  LClassList: TArray<TPair<string,TClass>>;
  LPair: TPair<string,TClass>;
begin
  inherited;
  LClassList := TWiRLClientRegistry.Instance.ToArray;
  for LPair in LClassList do
    Proc(LPair.Key);
end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(string), TWiRLClient, 'ClientVendor', TClientVendorProperty)
end;

end.
