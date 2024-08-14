{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.Server.Editor;

interface

uses
  System.Classes, System.SysUtils, DesignIntf, DesignEditors, StringsEdit,
  System.Generics.Collections, System.UITypes, Vcl.Dialogs, Vcl.Forms,
  System.TypInfo,

  WiRL.Core.Classes,
  WiRL.Core.Application,
  WiRL.Core.Registry,
  WiRL.http.Server,
  WiRL.http.Server.Interfaces,
  WiRL.http.Filters,
  WiRL.Engine.REST,
  WiRL.Engine.HTTP,
  WiRL.Core.MessageBodyWriter,
  WiRL.Core.MessageBodyReader,

  WiRL.Core.Application.Editor;

Type
  TServerVendorProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TApplicationsProperty = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

  TApplicationRegistryProperty = class(TPropertyEditor)
  protected
    procedure ReadFromRegistry(AKeyList: TStrings); virtual; abstract;
    procedure WriteToRegistry(AKeyList: TStrings); virtual; abstract;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

  TAppResourcesRegistryProperty = class(TApplicationRegistryProperty)
  protected
    procedure ReadFromRegistry(AKeyList: TStrings); override;
    procedure WriteToRegistry(AKeyList: TStrings); override;
  end;

  TAppFiltersRegistryProperty = class(TApplicationRegistryProperty)
  protected
    procedure ReadFromRegistry(AKeyList: TStrings); override;
    procedure WriteToRegistry(AKeyList: TStrings); override;
  end;

  TAppWritersRegistryProperty = class(TApplicationRegistryProperty)
  protected
    procedure ReadFromRegistry(AKeyList: TStrings); override;
    procedure WriteToRegistry(AKeyList: TStrings); override;
  end;

  TAppReadersRegistryProperty = class(TApplicationRegistryProperty)
  protected
    procedure ReadFromRegistry(AKeyList: TStrings); override;
    procedure WriteToRegistry(AKeyList: TStrings); override;
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

uses
  WiRL.Engine.Core;

procedure Register;
begin
  RegisterClass(TWiRLApplication);
  RegisterNoIcon([TWiRLApplication]);
  RegisterComponentEditor (TWiRLRESTEngine, TWiRLEngineEditor);

  RegisterPropertyEditor(TypeInfo(string), TWiRLServer, 'ServerVendor', TServerVendorProperty);
  RegisterPropertyEditor(TypeInfo(TWiRLApplicationList), TWiRLRESTEngine, 'Applications', TApplicationsProperty);

  RegisterPropertyEditor(TypeInfo(TWiRLResourceRegistry), TWiRLApplication, 'Resources', TAppResourcesRegistryProperty);
  RegisterPropertyEditor(TypeInfo(TWiRLFilterRegistry), TWiRLApplication, 'Filters', TAppFiltersRegistryProperty);
  RegisterPropertyEditor(TypeInfo(TWiRLWriterRegistry), TWiRLApplication, 'Writers', TAppWritersRegistryProperty);
  RegisterPropertyEditor(TypeInfo(TWiRLReaderRegistry), TWiRLApplication, 'Readers', TAppReadersRegistryProperty);

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
  case Index of
    0: begin
      TWiRLAppEditor.ShowEditor(Designer, Component as TWiRLRESTEngine);
    end;
    1: begin
      LBasePath := '/app' + IntToStr((Component as TWiRLRESTEngine).Applications.Count + 1);
      if InputQuery('New Application', 'BasePath', LBasePath) then
      begin
        LApplication := TWiRLApplication.Create(Component.Owner);
        LApplication.Name := Designer.UniqueName(TWiRLApplication.ClassName);
        LApplication.AppName := LApplication.Name;
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
  Result := 'Add application...';
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
    FApplication.AppName := FApplication.Name;
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
  Result := FApplication.AppName;
end;

function TWiRLApplicationItem.GetName: string;
begin
  Result := FApplication.Name;
end;

procedure TWiRLApplicationItem.SetName(const Value: string);
begin
  FApplication.Name := Value;
end;

{ TApplicationsProperty }

procedure TApplicationsProperty.Edit;
begin
  inherited;
  if PropCount < 1 then
    Exit;

  TWiRLAppEditor.ShowEditor(Designer, GetComponent(0) as TWiRLRESTEngine);

end;

function TApplicationsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TApplicationsProperty.GetValue: string;
begin
  Result := Format('(%s)', [GetTypeName(GetPropType)]);
end;

{ TApplicationRegistryProperty }

procedure TApplicationRegistryProperty.Edit;
var
  LStringsEditDlg: TStringsEditDlg;
  LLines: TStrings;
begin
  inherited;
  LStringsEditDlg := TStringsEditDlg.Create(nil);
  try
    LLines := TStringList.Create;
    try
      ReadFromRegistry(LLines);
      LStringsEditDlg.Lines := LLines;
      if LStringsEditDlg.ShowModal = mrOk then
      begin
        WriteToRegistry(LStringsEditDlg.Lines);
        Designer.Modified;
      end;
    finally
      LLines.Free;
    end;
  finally
    LStringsEditDlg.Free;
  end;
end;

function TApplicationRegistryProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TApplicationRegistryProperty.GetValue: string;
begin
  Result := Format('(%s)', [GetTypeName(GetPropType)]);
end;

{ TAppResourcesRegistryProperty }

procedure TAppResourcesRegistryProperty.ReadFromRegistry(AKeyList: TStrings);
var
  LApplication: TWiRLApplication;
  LKey: string;
begin
  LApplication := GetComponent(0) as TWiRLApplication;
  for LKey in LApplication.Resources.Keys do
    AKeyList.Add(LKey);
end;

procedure TAppResourcesRegistryProperty.WriteToRegistry(AKeyList: TStrings);
var
  LApplication: TWiRLApplication;
  LKey: string;
begin
  LApplication := GetComponent(0) as TWiRLApplication;
  LApplication.Resources.Clear;
  for LKey in AKeyList do
    LApplication.SetResources(LKey);
end;

{ TAppFiltersRegistryProperty }

procedure TAppFiltersRegistryProperty.ReadFromRegistry(AKeyList: TStrings);
var
  LApplication: TWiRLApplication;
  LKey: TWiRLFilterConstructorProxy;
begin
  LApplication := GetComponent(0) as TWiRLApplication;
  for LKey in LApplication.FilterRegistry do
  begin
    AKeyList.Add(LKey.FilterQualifiedClassName);
  end;
end;

procedure TAppFiltersRegistryProperty.WriteToRegistry(AKeyList: TStrings);
var
  LApplication: TWiRLApplication;
  LKey: string;
begin
  LApplication := GetComponent(0) as TWiRLApplication;
  LApplication.FilterRegistry.Clear;
  for LKey in AKeyList do
  begin
    LApplication.SetFilters(LKey);
  end;
end;

{ TAppWritersRegistryProperty }

procedure TAppWritersRegistryProperty.ReadFromRegistry(AKeyList: TStrings);
var
  LApplication: TWiRLApplication;
  LWriterInfo: TWiRLWriterRegistry.TWriterInfo;
begin
  LApplication := GetComponent(0) as TWiRLApplication;
  for LWriterInfo in LApplication.WriterRegistry do
    AKeyList.Add(LWriterInfo.WriterName);
end;

procedure TAppWritersRegistryProperty.WriteToRegistry(AKeyList: TStrings);
var
  LApplication: TWiRLApplication;
  LKey: string;
begin
  LApplication := GetComponent(0) as TWiRLApplication;
  LApplication.WriterRegistry.Clear;
  for LKey in AKeyList do
    LApplication.SetWriters(LKey);
end;

{ TAppReadersRegistryProperty }

procedure TAppReadersRegistryProperty.ReadFromRegistry(AKeyList: TStrings);
var
  LApplication: TWiRLApplication;
  LReaderInfo: TWiRLReaderRegistry.TReaderInfo;
begin
  LApplication := GetComponent(0) as TWiRLApplication;
  for LReaderInfo in LApplication.ReaderRegistry do
    AKeyList.Add(LReaderInfo.ReaderName);
end;

procedure TAppReadersRegistryProperty.WriteToRegistry(AKeyList: TStrings);
var
  LApplication: TWiRLApplication;
  LKey: string;
begin
  LApplication := GetComponent(0) as TWiRLApplication;
  LApplication.ReaderRegistry.Clear;
  for LKey in AKeyList do
    LApplication.SetReaders(LKey);
end;

end.
