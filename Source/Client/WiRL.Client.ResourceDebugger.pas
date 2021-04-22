unit WiRL.Client.ResourceDebugger;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Imaging.pngimage, Winapi.ShellAPI, Vcl.Grids,

  WiRL.Client.CustomResource,
  WiRL.Client.Resource,
  WiRL.http.Client.Interfaces,
  WiRL.http.Headers, WiRL.Client.ResourceDebuggerHeader;

type
  TWiRLResourceRunnerForm = class(TForm)
    PanelHeader: TPanel;
    GroupBoxRequest: TGroupBox;
    ResponseGroupBox: TGroupBox;
    PageControlRequest: TPageControl;
    RequestTab: TTabSheet;
    HeadersTab: TTabSheet;
    ProxyTab: TTabSheet;
    Label1: TLabel;
    EditBaseUrl: TEdit;
    FooterPanel: TPanel;
    ImageLogo: TImage;
    Label2: TLabel;
    WiRLUrlLabel: TLabel;
    Label3: TLabel;
    ComboBoxMethod: TComboBox;
    EditResourcePath: TEdit;
    ButtonClose: TButton;
    ButtonSendRequest: TButton;
    ResponseLabel: TLabel;
    Label4: TLabel;
    ComboBoxComponents: TComboBox;
    MemoResponse: TMemo;
    HeaderCommandPanel: TPanel;
    ListViewHeader: TListView;
    ButtonAddHeader: TButton;
    ButtonDeleteHeader: TButton;
    ButtonEditHeader: TButton;
    MemoRequest: TMemo;
    Label5: TLabel;
    procedure WiRLUrlLabelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonSendRequestClick(Sender: TObject);
    procedure ButtonDeleteHeaderClick(Sender: TObject);
    procedure ButtonAddHeaderClick(Sender: TObject);
    procedure ButtonEditHeaderClick(Sender: TObject);
  private
    FResource: TWiRLClientCustomResource;
    FContainer: TComponent;
    procedure AfterRequestHandler(Sender: TObject; const AHttpMethod: string; ARequestStream: TStream; AResponse: IWiRLResponse);
    procedure SetResource(const Value: TWiRLClientCustomResource);
    procedure ConfigComponent;
    procedure AddHeader(const AName, AValue: string);
    procedure DeleteHeader;
    procedure SetContainer(const Value: TComponent);
  public
    class procedure Edit(AResource: TWiRLClientCustomResource);
  public
    property Resource: TWiRLClientCustomResource read FResource write SetResource;
    property Container: TComponent read FContainer write SetContainer;
  end;

implementation

{$R *.dfm}

procedure TWiRLResourceRunnerForm.AddHeader(const AName, AValue: string);
var
  LItem: TListItem;
begin
  LItem := ListViewHeader.FindCaption(0, AName, False, True, False);
  if not Assigned(LItem) then
    LItem := ListViewHeader.Items.Add;

  LItem.Caption := AName;
  LItem.SubItems.Clear;
  LItem.SubItems.Add(AValue);
  FResource.Headers[AName] := AValue;
end;

procedure TWiRLResourceRunnerForm.AfterRequestHandler(Sender: TObject;
  const AHttpMethod: string; ARequestStream: TStream; AResponse: IWiRLResponse);
begin
  if AResponse.StatusCode >= 500 then
    ResponseLabel.Font.Color := clMaroon
  else if AResponse.StatusCode >= 400 then
    ResponseLabel.Font.Color := clOlive
  else if AResponse.StatusCode >= 300 then
    ResponseLabel.Font.Color := clBlue
  else
    ResponseLabel.Font.Color := clBlack;

  ResponseLabel.Caption := IntToStr(AResponse.StatusCode) + ' - ' + AResponse.StatusText;
end;

procedure TWiRLResourceRunnerForm.ButtonAddHeaderClick(Sender: TObject);
var
  LName, LValue: string;
begin
  if TFormEditHeader.Execute(LName, LValue) then
  begin
    AddHeader(LName, LValue);
  end;
end;

procedure TWiRLResourceRunnerForm.ButtonDeleteHeaderClick(Sender: TObject);
begin
  DeleteHeader;
end;

procedure TWiRLResourceRunnerForm.ButtonEditHeaderClick(Sender: TObject);
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
    LValue := FResource.Headers[LName];
  end;

  if TFormEditHeader.Execute(LName, LValue) then
  begin
    AddHeader(LName, LValue);
  end;
end;

procedure TWiRLResourceRunnerForm.ButtonCloseClick(Sender: TObject);
begin
  ConfigComponent;
  Close;
end;

procedure TWiRLResourceRunnerForm.ConfigComponent;
//var
//  LIndex: Integer;
//  LName, LValue: string;
begin
  FResource.Resource := EditResourcePath.Text;

//  FResource.Headers.Clear;
//  for LIndex := 0 to HeaderGrid.RowCount - 1 do
//  begin
//    LName := HeaderGrid.Cells[0, LIndex];
//    LValue := HeaderGrid.Cells[1, LIndex];
//    if LName.Trim = '' then
//      Continue;
//
//    if SameText(LName, 'Accept') and SameText(LValue, FResource.Application.DefaultMediaType)  then
//      Continue;
//
//    if SameText(LName, 'Content-Type') and SameText(LValue, FResource.Application.DefaultMediaType)  then
//      Continue;
//
//    FResource.Headers.AddHeader(TWiRLHeader.Create(LName, LValue));
//  end;
end;

procedure TWiRLResourceRunnerForm.DeleteHeader;
var
  LItem: TListItem;
begin
  LItem := ListViewHeader.Selected;
  if Assigned(LItem) then
  begin
    FResource.Headers[LItem.Caption] := '';
    ListViewHeader.DeleteSelected;
  end;
end;

class procedure TWiRLResourceRunnerForm.Edit(AResource: TWiRLClientCustomResource);
var
  LEditorForm: TWiRLResourceRunnerForm;
begin
  if not Assigned(AResource) then
    raise Exception.Create('WiRLResource needed');

  LEditorForm := TWiRLResourceRunnerForm.Create(nil);
  try
    LEditorForm.Resource := AResource;
    LEditorForm.Container := AResource.Owner;
    LEditorForm.ShowModal;
  finally
    LEditorForm.Free;
  end;
end;

procedure TWiRLResourceRunnerForm.FormCreate(Sender: TObject);
begin
  PageControlRequest.ActivePageIndex := 0;
  ComboBoxMethod.ItemIndex := 0;
end;

procedure TWiRLResourceRunnerForm.ButtonSendRequestClick(Sender: TObject);
var
  LOriginalAfterRequestEvent: TAfterRequestEvent;
  LResponseText: string;
  LObject: TObject;
begin
  LOriginalAfterRequestEvent := FResource.AfterRequest;
  try
    LObject := nil;
    if not Assigned(FResource.Application) then
      raise Exception.Create('Application non defined');

    if not Assigned(FResource.Client) then
      raise Exception.Create('HttpClient non defined');

    try
      ConfigComponent;
      FResource.AfterRequest := AfterRequestHandler;

      if (ComboBoxComponents.ItemIndex >= 0) then
        LObject := ComboBoxComponents.Items.Objects[ComboBoxComponents.ItemIndex];

      if Assigned(LObject) then
      begin
        FResource.GenericHttpRequest<string>(ComboBoxMethod.Text, MemoRequest.Text, LObject);
        MemoResponse.Text := LResponseText;
      end
      else
      begin
        LResponseText := FResource.GenericHttpRequest<string, string>(ComboBoxMethod.Text, MemoRequest.Text);
        MemoResponse.Text := LResponseText;
      end;
    except
      on E: EWiRLClientResourceException do
      begin
        if Assigned(E.JsonResponse) then
          MemoResponse.Text := E.JsonResponse.ToJSON
        else
          MemoResponse.Text := E.Message;
      end;
    end;
  finally
    FResource.AfterRequest := LOriginalAfterRequestEvent;
  end;
end;

procedure TWiRLResourceRunnerForm.SetContainer(const Value: TComponent);
var
  I: Integer;
begin
  FContainer := Value;
  if Assigned(FContainer) then
  begin
    ComboBoxComponents.Clear;
    ComboBoxComponents.Items.Add('<string>');
    for I := 0 to FContainer.ComponentCount - 1 do
    begin
      ComboBoxComponents.Items.AddObject(FContainer.Components[I].Name + ': ' + FContainer.Components[I].ClassName, FContainer.Components[I]);
    end;
  end;
end;

procedure TWiRLResourceRunnerForm.SetResource(const Value: TWiRLClientCustomResource);
var
  LHeader: TWiRLHeader;
begin
  FResource := Value;
  if Assigned(FResource.Client) then
    EditBaseUrl.Text := FResource.Client.WiRLEngineURL;
  EditResourcePath.Text := FResource.Resource;

  if Assigned(FResource.Application) then
  begin
    AddHeader('Accept', FResource.Application.DefaultMediaType);
    AddHeader('Content-Type', FResource.Application.DefaultMediaType);
  end;

  for LHeader in FResource.Headers do
  begin
    AddHeader(LHeader.Name, LHeader.Value);
  end;
end;

procedure TWiRLResourceRunnerForm.WiRLUrlLabelClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PChar(WiRLUrlLabel.Caption), '', '', SW_NORMAL);
end;

end.
