{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2023 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Client.ResourceHeaderEditor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.ToolWin, System.ImageList, Vcl.ImgList,
  DesignIntf,

  WiRL.http.Headers,
  WiRL.Client.CustomResource;

type
  TFormHeadersEditor = class(TForm)
    ListViewHeader: TListView;
    ImageList1: TImageList;
    ToolBar1: TToolBar;
    ButtonAddHeader: TToolButton;
    ButtonDeleteHeader: TToolButton;
    procedure ButtonAddHeaderClick(Sender: TObject);
    procedure ButtonEditHeaderClick(Sender: TObject);
    procedure ButtonDeleteHeaderClick(Sender: TObject);
  private
    FHeaders: IWiRLHeaders;
    FDesigner: IDesigner;
    procedure SetHeaders(const AValue: IWiRLHeaders);
    procedure AddHeader(const AName, AValue: string);
  public
    property Headers: IWiRLHeaders read FHeaders write SetHeaders;
    property Designer: IDesigner read FDesigner write FDesigner;
  public
    class procedure Execute(ADesigner: IDesigner; AHeaders: IWiRLHeaders);
  end;


implementation

uses
  WiRL.Client.ResourceDebuggerHeader;

{$R *.dfm}

procedure TFormHeadersEditor.AddHeader(const AName, AValue: string);
var
  LItem: TListItem;
begin
  LItem := ListViewHeader.FindCaption(0, AName, False, True, False);
  if not Assigned(LItem) then
    LItem := ListViewHeader.Items.Add;

  LItem.Caption := AName;
  LItem.SubItems.Clear;
  LItem.SubItems.Add(AValue);
  FHeaders[AName] := AValue;
end;

procedure TFormHeadersEditor.ButtonAddHeaderClick(Sender: TObject);
var
  LName, LValue: string;
begin
  if TFormEditHeader.Execute(LName, LValue) then
  begin
    AddHeader(LName, LValue);
    FDesigner.Modified;
  end;
end;

procedure TFormHeadersEditor.ButtonDeleteHeaderClick(Sender: TObject);
var
  LItem: TListItem;
begin
  LItem := ListViewHeader.Selected;
  if Assigned(LItem) then
  begin
    FHeaders[LItem.Caption] := '';
    ListViewHeader.DeleteSelected;
  end;
end;

procedure TFormHeadersEditor.ButtonEditHeaderClick(Sender: TObject);
var
  LName, LValue: string;
  LItem: TListItem;
begin
  LName := '';
  LValue := '';
  LItem := ListViewHeader.Selected;
  if Assigned(LItem) then
  begin
    LName := LItem.Caption;
    LValue := FHeaders[LName];
  end;

  if TFormEditHeader.Execute(LName, LValue) then
  begin
    AddHeader(LName, LValue);
    FDesigner.Modified;
  end;
end;

class procedure TFormHeadersEditor.Execute(ADesigner: IDesigner; AHeaders: IWiRLHeaders);
var
  FormHeadersEditor: TFormHeadersEditor;
begin
  FormHeadersEditor := TFormHeadersEditor.Create(nil);
  try
    FormHeadersEditor.Designer := ADesigner;
    FormHeadersEditor.Headers := AHeaders;
    FormHeadersEditor.ShowModal;
  finally
    FormHeadersEditor.Free;
  end;
end;

procedure TFormHeadersEditor.SetHeaders(const AValue: IWiRLHeaders);
var
  LHeader: TWiRLHeader;
begin
  FHeaders := AValue;
  for LHeader in AValue do
  begin
    AddHeader(LHeader.Name, LHeader.Value);
  end;
end;

end.
