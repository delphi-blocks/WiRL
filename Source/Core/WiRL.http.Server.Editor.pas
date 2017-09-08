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
  System.Generics.Collections, Vcl.Dialogs, ColnEdit,

  WiRL.http.Engines,
  WiRL.http.Server,
  WiRL.http.Server.Interfaces,
  WiRL.Core.Application,
  WiRL.Core.Engine,

  WiRL.Core.Application.Editor;

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

type
  TWiRLApplicationItem = class(TCollectionItem)
  private
    FApplication: TWiRLApplication;
    function GetName: string;
    procedure SetName(const Value: string);
  protected
    property Application: TWiRLApplication read FApplication write FApplication;
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Name: string read GetName write SetName;
  end;

  TWiRLApplicationCollection = class(TOwnedCollection)
  private
    FDesigner: IDesigner;
  public
    property Designer: IDesigner read FDesigner write FDesigner;
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
//  LApplicationCollection: TWiRLApplicationCollection;
//  LApplicationInfo: TWiRLApplicationInfo;
  LApplication: TWiRLApplication;
//  LApplicationItem: TWiRLApplicationItem;
begin
  inherited;
  case Index of
    0: begin
//      LApplicationCollection := TWiRLApplicationCollection.Create(Component, TWiRLApplicationItem);
//      LApplicationCollection.Designer := Designer;
//      for LApplicationInfo in TWiRLEngine(Component).Applications do
//      begin
//        LApplicationItem := TWiRLApplicationItem.Create(nil);
//        LApplicationItem.Application := LApplicationInfo.Application;
//        LApplicationItem.Collection := LApplicationCollection;
//      end;

//      for i := 0 to TWiRLCustomEngine(Component).appChilds.Count - 1 do
//        with TWiRLApplicationItem.Create(nil) do
//        begin
//          LApplication := TWiRLApplication(TWiRLCustomEngine(Component).Childs[i]);
//          Collection := LApplicationCollection;
//        end;
      TWiRLAppEditor.ShowEditor(Designer, Component as TWiRLEngine);
    end;
    1: begin
      LBasePath := '/app' + IntToStr((Component as TWiRLEngine).Applications.Count + 1);
      if InputQuery('New Application', 'BasePath', LBasePath) then
      begin
        LApplication := TWiRLApplication.Create(Component.Owner);
        LApplication.Name := Designer.UniqueName(TWiRLApplication.ClassName);
        LApplication.DisplayName := LApplication.Name;
        LApplication.BasePath := LBasePath;
        LApplication.Engine := Component;
        Designer.Modified;
      end;
    end;
  end;
end;

function TWiRLEngineEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Applications editor...';
  Result := 'Add application';
end;

function TWiRLEngineEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{ TWiRLApplicationItem }

constructor TWiRLApplicationItem.Create(Collection: TCollection);
begin
  inherited;
  if Assigned(Collection) then
  begin
    FApplication := TWiRLApplication.Create(TComponent(TOwnedCollection(Collection).Owner).Owner);
    FApplication.Name := TWiRLApplicationCollection(Collection).Designer.UniqueName(TWiRLApplication.ClassName);
    FApplication.Engine := TWiRLCustomEngine(TComponent(TOwnedCollection(Collection).Owner));
    FApplication.DisplayName := FApplication.Name;
    FApplication.BasePath := 'app' + IntToStr(Collection.Count + 1);
  end;
end;

destructor TWiRLApplicationItem.Destroy;
begin
  FApplication.Free;
  inherited;
end;

function TWiRLApplicationItem.GetDisplayName: string;
begin
  Result := FApplication.DisplayName;
end;

function TWiRLApplicationItem.GetName: string;
begin
  Result := FApplication.Name;
end;

procedure TWiRLApplicationItem.SetName(const Value: string);
begin
  FApplication.Name := Value;
end;

end.
