object frmClientMain: TfrmClientMain
  Left = 0
  Top = 0
  Caption = 'Authorization Client'
  ClientHeight = 608
  ClientWidth = 552
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object Label8: TLabel
    Left = 8
    Top = 54
    Width = 80
    Height = 13
    Caption = 'Token Received:'
  end
  object Label13: TLabel
    Left = 8
    Top = 212
    Width = 74
    Height = 13
    Caption = 'Response Body'
  end
  object edtHost: TEdit
    Left = 8
    Top = 8
    Width = 525
    Height = 21
    TabOrder = 0
    Text = 'http://localhost:8080/rest/app/'
    OnChange = edtHostChange
  end
  object pgcAuthApp: TPageControl
    Left = 8
    Top = 78
    Width = 529
    Height = 131
    ActivePage = tsPrivateResource
    TabHeight = 30
    TabOrder = 1
    object tsPublicResource: TTabSheet
      Caption = 'Public Resource'
      object Label2: TLabel
        Left = 31
        Top = 8
        Width = 45
        Height = 13
        Caption = 'Resource'
      end
      object Label10: TLabel
        Left = 3
        Top = 29
        Width = 24
        Height = 16
        Caption = 'GET'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object btnPublicResource: TButton
        Left = 397
        Top = 8
        Width = 121
        Height = 25
        Caption = 'Get Public Resource'
        TabOrder = 0
        OnClick = btnPublicResourceClick
      end
      object edtResourcePublic: TEdit
        Left = 33
        Top = 27
        Width = 121
        Height = 21
        TabOrder = 1
        Text = 'user'
      end
    end
    object tsLogin: TTabSheet
      Caption = 'Login (Basic Auth)'
      ImageIndex = 1
      object Label1: TLabel
        Left = 43
        Top = 8
        Width = 106
        Height = 13
        Caption = 'Resource (Basic Auth)'
      end
      object Label9: TLabel
        Left = 5
        Top = 27
        Width = 32
        Height = 16
        Caption = 'POST'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label14: TLabel
        Left = 170
        Top = 8
        Width = 48
        Height = 13
        Caption = 'Username'
      end
      object Label15: TLabel
        Left = 265
        Top = 8
        Width = 46
        Height = 13
        Caption = 'Password'
      end
      object btnLoginBasic: TButton
        Left = 397
        Top = 3
        Width = 121
        Height = 25
        Caption = 'Login (Basic Auth)'
        TabOrder = 0
        OnClick = btnLoginBasicClick
      end
      object edtResourceLogin: TEdit
        Left = 43
        Top = 24
        Width = 121
        Height = 21
        TabOrder = 1
        Text = 'basic_auth'
      end
      object edtUsername: TEdit
        Left = 170
        Top = 24
        Width = 89
        Height = 21
        TabOrder = 2
        Text = 'me'
      end
      object edtPassword: TEdit
        Left = 265
        Top = 24
        Width = 90
        Height = 21
        TabOrder = 3
        Text = 'mypassword'
      end
    end
    object tsPrivateResource: TTabSheet
      Caption = 'Private Resource'
      ImageIndex = 3
      object Label5: TLabel
        Left = 43
        Top = 8
        Width = 106
        Height = 13
        Caption = 'Resource (Basic Auth)'
      end
      object Label11: TLabel
        Left = 13
        Top = 25
        Width = 24
        Height = 16
        Caption = 'GET'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object btnPrivateResource: TButton
        Left = 397
        Top = 8
        Width = 121
        Height = 25
        Caption = 'Get Private Resource'
        TabOrder = 0
        OnClick = btnPrivateResourceClick
      end
      object edtResourcePrivate: TEdit
        Left = 43
        Top = 24
        Width = 121
        Height = 21
        TabOrder = 1
        Text = 'user/details'
      end
    end
  end
  object memoLog: TMemo
    Left = 8
    Top = 492
    Width = 525
    Height = 89
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
  end
  object edtToken: TEdit
    Left = 94
    Top = 51
    Width = 439
    Height = 21
    TabOrder = 3
  end
  object memoResponse: TMemo
    Left = 8
    Top = 231
    Width = 525
    Height = 255
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 4
    WordWrap = False
  end
end
