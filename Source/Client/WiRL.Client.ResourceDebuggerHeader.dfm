object FormEditHeader: TFormEditHeader
  Left = 0
  Top = 0
  Caption = 'Add header'
  ClientHeight = 147
  ClientWidth = 468
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  DesignSize = (
    468
    147)
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object Label2: TLabel
    Left = 16
    Top = 64
    Width = 30
    Height = 13
    Caption = 'Value:'
  end
  object PanelFooter: TPanel
    Left = 0
    Top = 117
    Width = 468
    Height = 30
    Align = alBottom
    BevelOuter = bvNone
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 0
    object ButtonOk: TButton
      AlignWithMargins = True
      Left = 309
      Top = 3
      Width = 75
      Height = 24
      Align = alRight
      Caption = 'Ok'
      TabOrder = 0
      OnClick = ButtonOkClick
    end
    object ButtonCancel: TButton
      AlignWithMargins = True
      Left = 390
      Top = 3
      Width = 75
      Height = 24
      Align = alRight
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object ComboBoxName: TComboBox
    Left = 16
    Top = 35
    Width = 439
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Items.Strings = (
      'Authorization'
      'Accept'
      'Accept-Charset'
      'Accept-Encoding'
      'Accept-Language'
      'Cookie'
      'Content-Length'
      'Content-Type'
      'Content-Encoding'
      'Content-Language'
      'Content-Location')
  end
  object ComboBoxValue: TComboBox
    Left = 16
    Top = 83
    Width = 439
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    Items.Strings = (
      'application/json'
      'application/octet-stream'
      'application/pdf'
      'application/xml'
      'image/gif'
      'image/jpeg'
      'image/png'
      'text/css'
      'text/html'
      'text/plain'
      'text/xml')
  end
end
