object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'WiRL Chunks Server'
  ClientHeight = 201
  ClientWidth = 464
  Color = clBtnFace
  Constraints.MinHeight = 240
  Constraints.MinWidth = 480
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 464
    Height = 73
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 28
      Top = 17
      Width = 63
      Height = 13
      Caption = 'Port number:'
    end
    object StartButton: TButton
      Left = 16
      Top = 41
      Width = 75
      Height = 25
      Action = StartServerAction
      TabOrder = 0
    end
    object StopButton: TButton
      Left = 104
      Top = 41
      Width = 75
      Height = 25
      Action = StopServerAction
      TabOrder = 1
    end
    object PortNumberEdit: TEdit
      Left = 97
      Top = 14
      Width = 82
      Height = 21
      TabOrder = 2
      Text = '8080'
    end
  end
  object ButtonWebDemo: TButton
    Left = 128
    Top = 104
    Width = 169
    Height = 33
    Caption = 'Web Demo'
    TabOrder = 1
    OnClick = ButtonWebDemoClick
  end
  object Button1: TButton
    Left = 128
    Top = 143
    Width = 169
    Height = 33
    Caption = 'Desktop Demo'
    TabOrder = 2
    OnClick = Button1Click
  end
  object MainActionList: TActionList
    Left = 384
    Top = 24
    object StartServerAction: TAction
      Caption = 'Start Server'
      OnExecute = StartServerActionExecute
      OnUpdate = StartServerActionUpdate
    end
    object StopServerAction: TAction
      Caption = 'Stop Server'
      OnExecute = StopServerActionExecute
      OnUpdate = StopServerActionUpdate
    end
  end
end
