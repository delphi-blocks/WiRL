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
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    class function FindConfig(var LServerConfig: TServerConfig): Boolean;
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

class function TformServerProjectDialog.FindConfig(var LServerConfig: TServerConfig): Boolean;
var
  Dialog: TformServerProjectDialog;
begin
  Dialog := TformServerProjectDialog.Create(nil);
  try
    Result := Dialog.ShowModal = mrOk;
    if Result then
    begin
      LServerConfig.ServerPort := StrToIntDef(Dialog.EditServerPort.Text, 8080);
      LServerConfig.EnginePath := Dialog.EditEnginePath.Text;
      LServerConfig.AppPath := Dialog.EditMainAppPath.Text;
      LServerConfig.UseDefaultMessageBody := Dialog.CheckMessageBodyWriter.Checked;
    end;
  finally
    Dialog.Free;
  end;
end;

end.
