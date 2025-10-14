unit WiRL.Wizards.Dialogs.ServerProject;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Imaging.pngimage,
  Vcl.ExtCtrls, WiRL.Wizards.Modules.Classes;

type
  TformServerProjectDialog = class(TForm)
    Panel1: TPanel;
    Image1: TImage;
    CheckMessageBodyWriter: TCheckBox;
    Label1: TLabel;
    EditEnginePath: TEdit;
    Label2: TLabel;
    EditMainAppPath: TEdit;
    Label3: TLabel;
    EditServerPort: TEdit;
    Button1: TButton;
    Button2: TButton;
    CheckCreateTheFirstResource: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    class function FindConfig(var AServerConfig: TServerConfig): Boolean;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

{ TformServerProjectDialog }

procedure TformServerProjectDialog.Button1Click(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TformServerProjectDialog.Button2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

class function TformServerProjectDialog.FindConfig(var AServerConfig: TServerConfig): Boolean;
var
  LDialog: TformServerProjectDialog;
begin
  LDialog := TformServerProjectDialog.Create(nil);
  try
    Result := LDialog.ShowModal = mrOk;
    if Result then
    begin
      AServerConfig.ServerPort := StrToIntDef(LDialog.EditServerPort.Text, 8080);
      AServerConfig.EnginePath := LDialog.EditEnginePath.Text;
      AServerConfig.AppPath := LDialog.EditMainAppPath.Text;
      AServerConfig.UseDefaultMessageBody := LDialog.CheckMessageBodyWriter.Checked;
      AServerConfig.CreateTheFirstResource := LDialog.CheckCreateTheFirstResource.Checked;
    end;
  finally
    LDialog.Free;
  end;
end;

end.
