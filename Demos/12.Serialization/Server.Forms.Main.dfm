object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'WiRL ContentTypes Server'
  ClientHeight = 486
  ClientWidth = 1037
  Color = clBtnFace
  Constraints.MinHeight = 240
  Constraints.MinWidth = 480
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    1037
    486)
  PixelsPerInch = 96
  TextHeight = 13
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 1037
    Height = 73
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 919
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
  object btnSerComplexObject: TButton
    Left = 16
    Top = 86
    Width = 131
    Height = 25
    Caption = 'ComplexObject'
    TabOrder = 1
    OnClick = btnSerComplexObjectClick
  end
  object memoSerialize: TMemo
    Left = 153
    Top = 88
    Width = 373
    Height = 390
    Anchors = [akLeft, akTop, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 2
    WordWrap = False
  end
  object btnSimpleTypes: TButton
    Left = 16
    Top = 125
    Width = 131
    Height = 25
    Caption = 'SimpleTypes'
    TabOrder = 3
    OnClick = btnSimpleTypesClick
  end
  object btnDesComplexObject: TButton
    Left = 532
    Top = 88
    Width = 131
    Height = 25
    Caption = 'ComplexObject'
    TabOrder = 4
    OnClick = btnDesComplexObjectClick
  end
  object memoDeserialize: TMemo
    Left = 669
    Top = 88
    Width = 360
    Height = 390
    Anchors = [akLeft, akTop, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 5
    WordWrap = False
  end
  object btnGenericList: TButton
    Left = 16
    Top = 168
    Width = 131
    Height = 25
    Caption = 'GenericList'
    TabOrder = 6
    OnClick = btnGenericListClick
  end
  object btnGenericObjectList: TButton
    Left = 16
    Top = 207
    Width = 131
    Height = 25
    Caption = 'GenericObjectList'
    TabOrder = 7
    OnClick = btnGenericObjectListClick
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
