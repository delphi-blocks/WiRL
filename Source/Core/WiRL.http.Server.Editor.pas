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
  System.Generics.Collections, Vcl.Dialogs,

  WiRL.http.Engines,
  WiRL.http.Server,
  WiRL.http.Server.Interfaces,
  WiRL.Core.Application,
  WiRL.Core.Engine;

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

  TWiRLEngineEditor = class (TComponentEditor)
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterClass(TWiRLApplication);
  RegisterNoIcon([TWiRLApplication]);
  RegisterComponentEditor (TWiRLEngine, TWiRLEngineEditor);
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
  LPair: TPair<string,TClass>;
begin
  inherited;
  for LPair in TWiRLServerRegistry.Instance do
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

{ TWiRLEngineEditor }

procedure TWiRLEngineEditor.ExecuteVerb(Index: Integer);
var
  LBasePath: string;
  LApplication: TWiRLApplication;
begin
  inherited;
  LBasePath := '/app' + IntToStr((Component as TWiRLEngine).Applications.Count + 1);
  if InputQuery('New Application', 'BasePath', LBasePath) then
  begin
    LApplication := TWiRLApplication.Create(Component.Owner);
    LApplication.Name := Designer.UniqueName(TWiRLApplication.ClassName);
    LApplication.DisplayName := LApplication.Name;
    LApplication.BasePath := LBasePath;
    LApplication.Engine := Component;
  end;
end;

function TWiRLEngineEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Add application';
end;

function TWiRLEngineEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
