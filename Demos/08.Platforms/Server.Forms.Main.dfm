object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'WiRL Server'
  ClientHeight = 340
  ClientWidth = 554
  Color = clBtnFace
  Constraints.MinHeight = 240
  Constraints.MinWidth = 480
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  TextHeight = 13
  object TopPanel: TPanel
    Left = 0
    Top = 57
    Width = 554
    Height = 50
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
      Left = 200
      Top = 14
      Width = 75
      Height = 25
      Action = StartServerAction
      TabOrder = 0
    end
    object StopButton: TButton
      Left = 288
      Top = 14
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
      NumbersOnly = True
      TabOrder = 2
      Text = '8080'
      OnChange = PortNumberEditChange
    end
  end
  object lstLog: TListBox
    Left = 0
    Top = 107
    Width = 554
    Height = 233
    Align = alClient
    ItemHeight = 13
    TabOrder = 1
  end
  object TitlePanel: TPanel
    Left = 0
    Top = 0
    Width = 554
    Height = 57
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Title'
    Color = clWhitesmoke
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 2
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
