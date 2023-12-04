{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2021 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Client.Application.Editor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, System.TypInfo, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ToolWin,
  System.Actions, Vcl.ActnList, Vcl.Menus, Vcl.PlatformDefaultStyleActnCtrls,
  Vcl.ActnPopup, System.ImageList, Vcl.ImgList, DesignIntf, DesignEditors,

  WiRL.Client.CustomResource,
  WiRL.Client.Application,
  WiRL.Client.Resource;

type
  TWiRLClientAppResourceEditor = class(TForm, IDesignNotification, IDesignWindow)
    ImageList1: TImageList;
    PopupMenu1: TPopupActionBar;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    N2: TMenuItem;
    Toolbar2: TMenuItem;
    ActionList1: TActionList;
    ToolbarCmd: TAction;
    TextLabelsCmd: TAction;
    HelpCmd: TAction;
    AddCmd: TAction;
    DeleteCmd: TAction;
    MoveUpCmd: TAction;
    MoveDownCmd: TAction;
    SelectAllCmd: TAction;
    PopupMenu2: TPopupActionBar;
    TextLabels1: TMenuItem;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ListView1: TListView;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    RunCmd: TAction;
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure AddCmdExecute(Sender: TObject);
    procedure DeleteCmdExecute(Sender: TObject);
    procedure DeleteCmdUpdate(Sender: TObject);
    procedure MoveUpCmdExecute(Sender: TObject);
    procedure MoveDownCmdExecute(Sender: TObject);
    procedure MoveUpCmdUpdate(Sender: TObject);
    procedure MoveDownCmdUpdate(Sender: TObject);
    procedure ListView1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RunCmdUpdate(Sender: TObject);
    procedure RunCmdExecute(Sender: TObject);
  private
    FDesigner: IDesigner;
    FClientApp: TWiRLClientApplication;
    procedure ConfigureListView;
    procedure FillAppList;
    procedure SetSelection;
  public
    class procedure ShowEditor(ADesigner: IDesigner; AClientApp: TWiRLClientApplication);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // IDesignNotification
    procedure ItemDeleted(const ADesigner: IDesigner; Item: TPersistent); virtual;
    procedure ItemInserted(const ADesigner: IDesigner; Item: TPersistent); virtual;
    procedure SelectionChanged(const ADesigner: IDesigner; const ASelection: IDesignerSelections); virtual;
    procedure DesignerOpened(const Designer: IDesigner; AResurrecting: Boolean); virtual;
    procedure DesignerClosed(const Designer: IDesigner; AGoingDormant: Boolean); virtual;
    procedure ItemsModified(const Designer: IDesigner); virtual;

    // IDesignWindow
    procedure WindowHide;
    procedure WindowShow;

    property Designer: IDesigner read FDesigner write FDesigner;
    property ClientApp: TWiRLClientApplication read FClientApp write FClientApp;
  end;

  TWiRLClientAppEditor = class (TComponentEditor)
  public
    procedure Edit; override;
  end;

  TWiRLClientAppClientProperty = class(TComponentProperty)
  private
    function GetClientApp: TWiRLClientApplication;
  public
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    function GetAttributes: TPropertyAttributes; override;
  end;

implementation

{$R *.dfm}

uses
  WiRL.http.Client,
  WiRL.Client.ResourceDebugger;

const
  DefaultClientName = '<default>';

{ TWiRLClientAppResourceEditor }

procedure TWiRLClientAppResourceEditor.AddCmdExecute(Sender: TObject);
var
  LResource: TWiRLClientResource;
begin
  LResource := TWiRLClientResource.Create(FClientApp.Owner);
  LResource.Name := Designer.UniqueName(TWiRLClientResource.ClassName);
  LResource.Resource := 'resource' + IntToStr(FClientApp.Resources.Count + 1);
  LResource.Application := FClientApp;
  FDesigner.Modified;
end;

procedure TWiRLClientAppResourceEditor.ConfigureListView;
var
  LCol: TListColumn;
begin
  LCol := ListView1.Columns.Add;
  LCol.Caption := 'Name';
  //LCol.AutoSize := True;

  LCol := ListView1.Columns.Add;
  LCol.Caption := 'AppPath';
  //LCol.AutoSize := True;

  LCol := ListView1.Columns.Add;
  LCol.Caption := 'ResPath';
  //LCol.AutoSize := True;
end;

constructor TWiRLClientAppResourceEditor.Create(AOwner: TComponent);
begin
  inherited;
  RegisterDesignNotification(Self);
end;

procedure TWiRLClientAppResourceEditor.DeleteCmdExecute(Sender: TObject);
var
  LResource: TWiRLClientResource;
  LIndex: Integer;
begin
  if Assigned(ListView1.Selected) then
  begin
    LIndex := ListView1.Selected.Index;
    LResource := TWiRLClientResource(ListView1.Selected.Data);
    LResource.Free;
    ListView1.DeleteSelected;
    if LIndex > 0 then
      ListView1.Selected := ListView1.Items[LIndex - 1]
    else if ListView1.Items.Count > 0 then
      ListView1.Selected := ListView1.Items[0];
    FDesigner.Modified;
  end;
end;

procedure TWiRLClientAppResourceEditor.DeleteCmdUpdate(Sender: TObject);
begin
  DeleteCmd.Enabled := Assigned(ListView1.Selected);
end;

procedure TWiRLClientAppResourceEditor.DesignerClosed(const Designer: IDesigner;
  AGoingDormant: Boolean);
begin
  if Designer = FDesigner then
    Close;
end;

procedure TWiRLClientAppResourceEditor.DesignerOpened(const Designer: IDesigner;
  AResurrecting: Boolean);
begin

end;

destructor TWiRLClientAppResourceEditor.Destroy;
begin
  UnregisterDesignNotification(Self);
  inherited;
end;

procedure TWiRLClientAppResourceEditor.FillAppList;
var
  LResourceObject :TObject;
  LResource: TWiRLClientResource;
  LListItem: TListItem;
  LItemIndex: Integer;
begin
  LItemIndex := -1;
  if Assigned(ListView1.Selected) then
    LItemIndex := ListView1.Selected.Index;

  ListView1.Clear;
  for LResourceObject in FClientApp.Resources do
  begin
    LResource := LResourceObject as TWiRLClientResource;
    LListItem := ListView1.Items.Add;
    LListItem.Caption := LResource.Name;
    LListItem.SubItems.Add(LResource.Application.AppName);
    LListItem.SubItems.Add(LResource.Resource);
    LListItem.Data := LResource;
  end;

  if LItemIndex >= 0 then
  begin
    if LItemIndex < ListView1.Items.Count then
      ListView1.Selected := ListView1.Items[LItemIndex]
    else if ListView1.Items.Count > 0 then
      ListView1.Selected := ListView1.Items[ListView1.Items.Count - 1]
  end;
end;

procedure TWiRLClientAppResourceEditor.SelectionChanged(const ADesigner: IDesigner;
  const ASelection: IDesignerSelections);
begin

end;

procedure TWiRLClientAppResourceEditor.SetSelection;
var
  I: Integer;
  List: IDesignerSelections;
begin
  try
    if ListView1.SelCount > 0 then
    begin
      List := CreateSelectionList;
      for I := 0 to ListView1.Items.Count - 1 do
        if ListView1.Items[I].Selected then
        begin
          List.Add(TComponent(ListView1.Items[I].Data));
        end;
      Designer.SetSelections(List);
    end
    else
      Designer.SelectComponent(Application);
  except
    Application.HandleException(ExceptObject);
    Close;
  end;
end;

procedure TWiRLClientAppResourceEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TWiRLClientAppResourceEditor.FormResize(Sender: TObject);
var
  I: Integer;
begin
  if ListView1.Columns.Count > 1 then
  begin
    for I := 0 to ListView1.Columns.Count - 1 do
      ListView1.Columns[I].Width := (ListView1.Width - 5) div ListView1.Columns.Count;
  end;
end;

procedure TWiRLClientAppResourceEditor.FormShow(Sender: TObject);
begin
  ConfigureListView;
  FillAppList;
end;

procedure TWiRLClientAppResourceEditor.ItemDeleted(const ADesigner: IDesigner;
  Item: TPersistent);
begin
  FillAppList;
end;

procedure TWiRLClientAppResourceEditor.ItemInserted(const ADesigner: IDesigner;
  Item: TPersistent);
begin
  FillAppList;
end;

procedure TWiRLClientAppResourceEditor.ItemsModified(const Designer: IDesigner);
begin
  FillAppList;
end;

procedure TWiRLClientAppResourceEditor.ListView1Click(Sender: TObject);
begin
  if Assigned(ListView1.Selected) then
  begin
    SetSelection;
  end;
end;

procedure TWiRLClientAppResourceEditor.MoveDownCmdExecute(Sender: TObject);
var
  LSelectedIndex: Integer;
begin
  if Assigned(ListView1.Selected) then
  begin
    LSelectedIndex := ListView1.Selected.Index;
    ClientApp.Resources.Move(ListView1.Selected.Index, ListView1.Selected.Index + 1);
    //FillAppList;
    FDesigner.Modified;
    ListView1.Selected := ListView1.Items[LSelectedIndex + 1];
  end;
end;

procedure TWiRLClientAppResourceEditor.MoveDownCmdUpdate(Sender: TObject);
begin
  MoveDownCmd.Enabled :=
    Assigned(ListView1.Selected) and
    (ListView1.Selected.Index < ListView1.Items.Count - 1);
end;

procedure TWiRLClientAppResourceEditor.MoveUpCmdExecute(Sender: TObject);
var
  LSelectedIndex: Integer;
begin
  if Assigned(ListView1.Selected) then
  begin
    LSelectedIndex := ListView1.Selected.Index;
    ClientApp.Resources.Move(ListView1.Selected.Index, ListView1.Selected.Index - 1);
//    FillAppList;
    FDesigner.Modified;
    ListView1.Selected := ListView1.Items[LSelectedIndex - 1];
  end;
end;

procedure TWiRLClientAppResourceEditor.MoveUpCmdUpdate(Sender: TObject);
begin
  MoveUpCmd.Enabled :=
    Assigned(ListView1.Selected) and
    (ListView1.Selected.Index > 0);
end;

procedure TWiRLClientAppResourceEditor.RunCmdExecute(Sender: TObject);
var
  LSelectedIndex: Integer;
begin
  if Assigned(ListView1.Selected) then
  begin
    LSelectedIndex := ListView1.Selected.Index;
    if (LSelectedIndex >= 0) and (LSelectedIndex < ClientApp.Resources.Count) then
    begin
      ClientApp.SetWriters('*.*');
      ClientApp.SetReaders('*.*');

      TWiRLResourceRunnerForm.Edit(ClientApp.Resources[LSelectedIndex] as TWiRLClientCustomResource);
      FillAppList;
    end;
  end;
end;

procedure TWiRLClientAppResourceEditor.RunCmdUpdate(Sender: TObject);
begin
  MoveUpCmd.Enabled := Assigned(ListView1.Selected);

end;

class procedure TWiRLClientAppResourceEditor.ShowEditor(ADesigner: IDesigner;
  AClientApp: TWiRLClientApplication);
var
  WiRLAppEditor: TWiRLClientAppResourceEditor;
begin
  WiRLAppEditor := TWiRLClientAppResourceEditor.Create(nil);
  WiRLAppEditor.Designer := ADesigner;
  WiRLAppEditor.ClientApp := AClientApp;
  WiRLAppEditor.Show;
end;

procedure TWiRLClientAppResourceEditor.WindowHide;
begin
  if Visible then
    ShowWindow(Handle, SW_HIDE);
end;

procedure TWiRLClientAppResourceEditor.WindowShow;
const
  ShowCommands: array[TWindowState] of Word =
    (SW_SHOWNOACTIVATE, SW_SHOWMINNOACTIVE, SW_SHOWMAXIMIZED);
begin
  if Visible then
    ShowWindow(Handle, ShowCommands[WindowState]);
end;

{ TWiRLClientAppEditor }

procedure TWiRLClientAppEditor.Edit;
begin
  inherited;
  TWiRLClientAppResourceEditor.ShowEditor(Designer, Component as TWiRLClientApplication);
end;

{ TWiRLClientAppClientProperty }

function TWiRLClientAppClientProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  Result := Result - [paMultiSelect];
end;

function TWiRLClientAppClientProperty.GetClientApp: TWiRLClientApplication;
begin
  Result := GetComponent(0) as TWiRLClientApplication;
end;

function TWiRLClientAppClientProperty.GetValue: string;
begin
  if GetClientApp.HasDefaultClient then
    Result := DefaultClientName
  else
    Result := inherited;
end;

procedure TWiRLClientAppClientProperty.GetValues(Proc: TGetStrProc);
begin
  Proc(DefaultClientName);
  inherited;
end;

procedure TWiRLClientAppClientProperty.SetValue(const Value: string);
begin
  if Value = DefaultClientName then
    GetClientApp.Client := nil
  else
    inherited SetValue(Value);
end;

end.
