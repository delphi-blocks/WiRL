unit WiRL.Client.ResourceRunner;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Imaging.pngimage, Winapi.ShellAPI, Vcl.Grids,

  WiRL.Client.CustomResource,
  WiRL.Client.Resource,
  WiRL.http.Client.Interfaces,
  WiRL.http.Headers;

type
  TWiRLResourceRunnerForm = class(TForm)
    HeaderPanel: TPanel;
    RequestGroupBox: TGroupBox;
    ResponseGroupBox: TGroupBox;
    RequestPageControl: TPageControl;
    RequestTab: TTabSheet;
    HeadersTab: TTabSheet;
    ProxyTab: TTabSheet;
    Label1: TLabel;
    BaseUrlEdit: TEdit;
    FooterPanel: TPanel;
    Image1: TImage;
    Label2: TLabel;
    WiRLUrlLabel: TLabel;
    Label3: TLabel;
    MethodComboBox: TComboBox;
    ResourcePathEdit: TEdit;
    CloseButton: TButton;
    SendRequestButton: TButton;
    ResponsePageControl: TPageControl;
    ErrorTab: TTabSheet;
    ComponentTab: TTabSheet;
    StringTab: TTabSheet;
    BinaryTab: TTabSheet;
    ResponseLabel: TLabel;
    ErrorResponseMemo: TMemo;
    Label4: TLabel;
    ComponentsComboBox: TComboBox;
    ResponseMemo: TMemo;
    Memo2: TMemo;
    HeaderGrid: TStringGrid;
    procedure WiRLUrlLabelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure SendRequestButtonClick(Sender: TObject);
  private
    FResource: TWiRLClientCustomResource;
    FContainer: TComponent;
    procedure SetResource(const Value: TWiRLClientCustomResource);
    procedure ConfigComponent;
    procedure AddHeader(const AName, AValue: string);
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
  LIndex: Integer;
  LFirstEmpty: Integer;
begin
  LFirstEmpty := -1;
  for LIndex := 0 to HeaderGrid.RowCount - 1 do
  begin
    if SameText(HeaderGrid.Cells[0, LIndex], AName) then
    begin
      HeaderGrid.Cells[1, LIndex] := AValue;
      Exit;
    end;
    if (HeaderGrid.Cells[0, LIndex] = '') and (LFirstEmpty < 0) then
      LFirstEmpty := LIndex;
  end;

  if LFirstEmpty = -1 then
  begin
    LFirstEmpty := HeaderGrid.RowCount;
    HeaderGrid.RowCount := LFirstEmpty + 1;
  end;

  HeaderGrid.Cells[0, LFirstEmpty] := AName;
  HeaderGrid.Cells[1, LFirstEmpty] := AValue;
end;

procedure TWiRLResourceRunnerForm.CloseButtonClick(Sender: TObject);
begin
  ConfigComponent;
  Close;
end;

procedure TWiRLResourceRunnerForm.ConfigComponent;
var
  LIndex: Integer;
  LName, LValue: string;
begin
  FResource.Resource := ResourcePathEdit.Text;

  FResource.Headers.Clear;
  for LIndex := 0 to HeaderGrid.RowCount - 1 do
  begin
    LName := HeaderGrid.Cells[0, LIndex];
    LValue := HeaderGrid.Cells[1, LIndex];
    if LName.Trim = '' then
      Continue;

    if SameText(LName, 'Accept') and SameText(LValue, FResource.Application.DefaultMediaType)  then
      Continue;

    if SameText(LName, 'Content-Type') and SameText(LValue, FResource.Application.DefaultMediaType)  then
      Continue;

    FResource.Headers.AddHeader(TWiRLHeader.Create(LName, LValue));
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
  BinaryTab.TabVisible := False; // Not yet implemented

  ErrorTab.TabVisible := False;
  RequestPageControl.ActivePageIndex := 0;
  ResponsePageControl.ActivePage := StringTab;
  MethodComboBox.ItemIndex := 0;

  HeaderGrid.Cells[0,0] := 'Name';
  HeaderGrid.Cells[1,0] := 'Value';
end;

procedure TWiRLResourceRunnerForm.SendRequestButtonClick(Sender: TObject);
var
  LResponseText: string;
begin
  if not Assigned(FResource.Application) then
    raise Exception.Create('Application non defined');

  if not Assigned(FResource.Client) then
    raise Exception.Create('HttpClient non defined');

  try
    ConfigComponent;

    if ResponsePageControl.ActivePage = StringTab then
    begin
      LResponseText := FResource.GenericHttpRequest<TObject, string>(MethodComboBox.Text, nil);
      ResponseMemo.Text := LResponseText;
    end
    else if ResponsePageControl.ActivePage = ComponentTab then
    begin
      if ComponentsComboBox.ItemIndex < 0 then
        raise Exception.Create('Select a valid component');
      FResource.GenericHttpRequest<TObject>(MethodComboBox.Text, nil, FContainer.Components[ComponentsComboBox.ItemIndex]);
      ResponseMemo.Text := LResponseText;
    end
    else
      raise Exception.Create('Select a valid response');
    ErrorTab.TabVisible := False;
  except
    on E: EWiRLClientResourceException do
    begin
      ErrorTab.TabVisible := True;
      ResponsePageControl.ActivePage := ErrorTab;
      ResponseLabel.Caption := IntToStr(E.StatusCode) + ' - ' + E.ReasonString;
      if Assigned(E.JsonResponse) then
        ErrorResponseMemo.Text := E.JsonResponse.ToJSON
      else
        ErrorResponseMemo.Text := E.Message;
    end;
  end;
end;

procedure TWiRLResourceRunnerForm.SetContainer(const Value: TComponent);
var
  I: Integer;
begin
  FContainer := Value;
  if Assigned(FContainer) then
  begin
    ComponentsComboBox.Clear;
    for I := 0 to FContainer.ComponentCount - 1 do
    begin
      ComponentsComboBox.Items.Add(FContainer.Components[I].Name + ': ' + FContainer.Components[I].ClassName);
    end;
    end;
end;

procedure TWiRLResourceRunnerForm.SetResource(const Value: TWiRLClientCustomResource);
var
  LHeader: TWiRLHeader;
begin
  FResource := Value;
  if Assigned(FResource.Client) then
    BaseUrlEdit.Text := FResource.Client.WiRLEngineURL;
  ResourcePathEdit.Text := FResource.Resource;

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
