{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2023 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Client.ResourceDebuggerHeader;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  DesignIntf;

type
  TFormEditHeader = class(TForm)
    PanelFooter: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    ComboBoxName: TComboBox;
    ComboBoxValue: TComboBox;
    ButtonOk: TButton;
    ButtonCancel: TButton;
    procedure ButtonOkClick(Sender: TObject);
  private
    function GetHeaderName: string;
    function GetHeaderValue: string;
    procedure SetHeaderName(const Value: string);
    procedure SetHeaderValue(const Value: string);
  public
    property HeaderName: string read GetHeaderName write SetHeaderName;
    property HeaderValue: string read GetHeaderValue write SetHeaderValue;
  public
    class function Execute(var AName, AValue: string): Boolean;
  end;


implementation

{$R *.dfm}

{ TForm5 }

procedure TFormEditHeader.ButtonOkClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

class function TFormEditHeader.Execute(var AName, AValue: string): Boolean;
var
  FormEditHeader: TFormEditHeader;
begin
  Result := False;
  FormEditHeader := TFormEditHeader.Create(nil);
  try
    FormEditHeader.HeaderName := AName;
    FormEditHeader.HeaderValue := AValue;
    if FormEditHeader.ShowModal = mrOk then
    begin
      AName := FormEditHeader.HeaderName;
      AValue := FormEditHeader.HeaderValue;
      Result := True;
    end;
  finally
    FormEditHeader.Free;
  end;
end;

function TFormEditHeader.GetHeaderName: string;
begin
  Result := ComboBoxName.Text;
end;

function TFormEditHeader.GetHeaderValue: string;
begin
  Result := ComboBoxValue.Text;
end;

procedure TFormEditHeader.SetHeaderName(const Value: string);
begin
  ComboBoxName.Text := Value;
end;

procedure TFormEditHeader.SetHeaderValue(const Value: string);
begin
  ComboBoxValue.Text := Value;
end;

end.
