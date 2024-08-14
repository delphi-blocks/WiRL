{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Application.Editor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ToolWin,
  System.Actions, Vcl.ActnList, Vcl.Menus, Vcl.PlatformDefaultStyleActnCtrls,
  Vcl.ActnPopup, System.ImageList, Vcl.ImgList, DesignIntf,

  WiRL.Engine.REST,
  WiRL.Core.Application;

type
  TWiRLAppEditor = class(TForm, IDesignNotification, IDesignWindow)
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
  private
    FDesigner: IDesigner;
    FEngine: TWiRLRESTEngine;
    procedure ConfigureListView;
    procedure FillAppList;
    procedure SetSelection;
  public
    class procedure ShowEditor(ADesigner: IDesigner; AEngine: TWiRLRESTEngine);
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
    property Engine: TWiRLRESTEngine read FEngine write FEngine;
  end;

implementation

{$R *.dfm}

{ TWiRLAppEditor }

procedure TWiRLAppEditor.AddCmdExecute(Sender: TObject);
var
  LApplication: TWiRLApplication;
begin
  LApplication := TWiRLApplication.Create(FEngine.Owner);
  LApplication.Name := Designer.UniqueName(TWiRLApplication.ClassName);
  LApplication.AppName := LApplication.Name;
  LApplication.BasePath := '/app' + IntToStr(FEngine.Applications.Count + 1);
  LApplication.Engine := FEngine;
  FDesigner.Modified;
//  FillAppList;
end;

procedure TWiRLAppEditor.ConfigureListView;
var
  LCol: TListColumn;
begin
  LCol := ListView1.Columns.Add;
  LCol.Caption := 'AppName';
  //LCol.AutoSize := True;

  LCol := ListView1.Columns.Add;
  LCol.Caption := 'BasePath';
  //LCol.AutoSize := True;
end;

constructor TWiRLAppEditor.Create(AOwner: TComponent);
begin
  inherited;
  RegisterDesignNotification(Self);
end;

procedure TWiRLAppEditor.DeleteCmdExecute(Sender: TObject);
var
  LApp: TWiRLApplication;
  LIndex: Integer;
begin
  if Assigned(ListView1.Selected) then
  begin
    LIndex := ListView1.Selected.Index;
    LApp := TWiRLApplication(ListView1.Selected.Data);
    LApp.Free;
    ListView1.DeleteSelected;
    if LIndex > 0 then
      ListView1.Selected := ListView1.Items[LIndex - 1]
    else if ListView1.Items.Count > 0 then
      ListView1.Selected := ListView1.Items[0];
    FDesigner.Modified;
  end;
end;

procedure TWiRLAppEditor.DeleteCmdUpdate(Sender: TObject);
begin
  DeleteCmd.Enabled := Assigned(ListView1.Selected);
end;

procedure TWiRLAppEditor.DesignerClosed(const Designer: IDesigner;
  AGoingDormant: Boolean);
begin
  if Designer = FDesigner then
    Close;
end;

procedure TWiRLAppEditor.DesignerOpened(const Designer: IDesigner;
  AResurrecting: Boolean);
begin

end;

destructor TWiRLAppEditor.Destroy;
begin
  UnregisterDesignNotification(Self);
  inherited;
end;

procedure TWiRLAppEditor.FillAppList;
var
  LAppInfo :TWiRLApplicationInfo;
  LListItem: TListItem;
  LItemIndex: Integer;
begin
  LItemIndex := -1;
  if Assigned(ListView1.Selected) then
    LItemIndex := ListView1.Selected.Index;

  ListView1.Clear;
  for LAppInfo in FEngine.Applications do
  begin
    LListItem := ListView1.Items.Add;
    LListItem.Caption := LAppInfo.Application.AppName;
    LListItem.SubItems.Add(LAppInfo.Application.BasePath);
    LListItem.Data := LAppInfo.Application;
  end;

  if LItemIndex >= 0 then
  begin
    if LItemIndex < ListView1.Items.Count then
      ListView1.Selected := ListView1.Items[LItemIndex]
    else if ListView1.Items.Count > 0 then
      ListView1.Selected := ListView1.Items[ListView1.Items.Count - 1]
  end;
end;

procedure TWiRLAppEditor.SelectionChanged(const ADesigner: IDesigner;
  const ASelection: IDesignerSelections);
begin

end;

procedure TWiRLAppEditor.SetSelection;
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
      Designer.SelectComponent(Engine);
  except
    Application.HandleException(ExceptObject);
    Close;
  end;
end;

procedure TWiRLAppEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TWiRLAppEditor.FormResize(Sender: TObject);
begin
  if ListView1.Columns.Count > 1 then
  begin
    ListView1.Columns[0].Width := (ListView1.Width - 5) div 2;
    ListView1.Columns[1].Width := (ListView1.Width - 5) div 2;
  end;
end;

procedure TWiRLAppEditor.FormShow(Sender: TObject);
begin
  ConfigureListView;
  FillAppList;
end;

procedure TWiRLAppEditor.ItemDeleted(const ADesigner: IDesigner;
  Item: TPersistent);
begin
  FillAppList;
end;

procedure TWiRLAppEditor.ItemInserted(const ADesigner: IDesigner;
  Item: TPersistent);
begin
  FillAppList;
end;

procedure TWiRLAppEditor.ItemsModified(const Designer: IDesigner);
begin
  FillAppList;
end;

procedure TWiRLAppEditor.ListView1Click(Sender: TObject);
begin
  if Assigned(ListView1.Selected) then
  begin
    SetSelection;
  end;
end;

procedure TWiRLAppEditor.MoveDownCmdExecute(Sender: TObject);
var
  LSelectedIndex: Integer;
begin
  if Assigned(ListView1.Selected) then
  begin
    LSelectedIndex := ListView1.Selected.Index;
    FEngine.Applications.Move(ListView1.Selected.Index, ListView1.Selected.Index + 1);
    //FillAppList;
    FDesigner.Modified;
    ListView1.Selected := ListView1.Items[LSelectedIndex + 1];
  end;
end;

procedure TWiRLAppEditor.MoveDownCmdUpdate(Sender: TObject);
begin
  MoveDownCmd.Enabled :=
    Assigned(ListView1.Selected) and
    (ListView1.Selected.Index < ListView1.Items.Count - 1);
end;

procedure TWiRLAppEditor.MoveUpCmdExecute(Sender: TObject);
var
  LSelectedIndex: Integer;
begin
  if Assigned(ListView1.Selected) then
  begin
    LSelectedIndex := ListView1.Selected.Index;
    FEngine.Applications.Move(ListView1.Selected.Index, ListView1.Selected.Index - 1);
//    FillAppList;
    FDesigner.Modified;
    ListView1.Selected := ListView1.Items[LSelectedIndex - 1];
  end;
end;

procedure TWiRLAppEditor.MoveUpCmdUpdate(Sender: TObject);
begin
  MoveUpCmd.Enabled :=
    Assigned(ListView1.Selected) and
    (ListView1.Selected.Index > 0);
end;

class procedure TWiRLAppEditor.ShowEditor(ADesigner: IDesigner; AEngine: TWiRLRESTEngine);
var
  WiRLAppEditor: TWiRLAppEditor;
begin
  WiRLAppEditor := TWiRLAppEditor.Create(nil);
  WiRLAppEditor.Designer := ADesigner;
  WiRLAppEditor.Engine := AEngine;
  WiRLAppEditor.Show;
end;

procedure TWiRLAppEditor.WindowHide;
begin
  if Visible then
    ShowWindow(Handle, SW_HIDE);
end;

procedure TWiRLAppEditor.WindowShow;
const
  ShowCommands: array[TWindowState] of Word =
    (SW_SHOWNOACTIVATE, SW_SHOWMINNOACTIVE, SW_SHOWMAXIMIZED);
begin
  if Visible then
    ShowWindow(Handle, ShowCommands[WindowState]);
end;

end.
