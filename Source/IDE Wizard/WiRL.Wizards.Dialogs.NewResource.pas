unit WiRL.Wizards.Dialogs.NewResource;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.pngimage, Vcl.ExtCtrls,
  Vcl.StdCtrls, WiRL.Wizards.Modules.Classes;

type
  TformNewResourceDialog = class(TForm)
    Panel1: TPanel;
    Image1: TImage;
    Button1: TButton;
    Button2: TButton;
    Label3: TLabel;
    CheckMethodGET: TCheckBox;
    EditResourceName: TEdit;
    LabelMethods: TLabel;
    CheckMethodPOST: TCheckBox;
    CheckMethodPUT: TCheckBox;
    CheckMethodDELETE: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    class function FindConfig(var AResourceConfig: TResourceConfig): Boolean;
  public
    { Public declarations }
  end;

var
  formNewResourceDialog: TformNewResourceDialog;

implementation

{$R *.dfm}

{ TformNewResourceDialog }

procedure TformNewResourceDialog.Button1Click(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TformNewResourceDialog.Button2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

class function TformNewResourceDialog.FindConfig(var AResourceConfig: TResourceConfig): Boolean;
var
  LDialog: TformNewResourceDialog;
begin
  LDialog := TformNewResourceDialog.Create(nil);
  try
    Result := LDialog.ShowModal = mrOk;
    if Result then
    begin
      AResourceConfig.Name := LDialog.EditResourceName.Text;
      AResourceConfig.MethodGET := LDialog.CheckMethodGET.Checked;
      AResourceConfig.MethodPOST := LDialog.CheckMethodPOST.Checked;
      AResourceConfig.MethodPUT := LDialog.CheckMethodPUT.Checked;
      AResourceConfig.MethodDELETE := LDialog.CheckMethodDELETE.Checked;
    end;
  finally
    LDialog.Free;
  end;
end;

end.
