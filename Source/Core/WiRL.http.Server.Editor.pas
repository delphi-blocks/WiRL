{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.Server.Editor;

interface

uses
  System.Classes, System.SysUtils, DesignIntf, DesignEditors,
  System.Generics.Collections,

  WiRL.http.Engines,
  WiRL.http.Server,
  WiRL.http.Server.Interfaces;

Type
  TServerVendorProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TWiRLhttpEngineSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(string), TWiRLhttpServer, 'ServerVendor', TServerVendorProperty);
  RegisterSelectionEditor(TWiRLhttpEngine, TWiRLhttpEngineSelectionEditor);
end;

{ TServerVendorProperty }

function TServerVendorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TServerVendorProperty.GetValues(Proc: TGetStrProc);
var
  LClassList: TArray<TPair<string,TClass>>;
  LPair: TPair<string,TClass>;
begin
  inherited;
  LClassList := TWiRLServerRegistry.Instance.ToArray;
  for LPair in LClassList do
    Proc(LPair.Key);
end;

{ TWiRLhttpEngineSelectionEditor }

procedure TWiRLhttpEngineSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  Proc('WiRL.http.Request');
  Proc('WiRL.http.Response');
  Proc('WiRL.Core.Context');
end;


end.
