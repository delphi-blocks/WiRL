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
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label8: TLabel
    Left = 8
    Top = 54
    Width = 80
    Height = 13
    Caption = 'Token Received:'
  end
  object edtHost: TEdit
    Left = 8
    Top = 8
    Width = 525
    Height = 21
    TabOrder = 0
    Text = 'http://localhost:8080/rest/app/'
  end
  object pgcAuthApp: TPageControl
    Left = 8
    Top = 78
    Width = 529
    Height = 408
    ActivePage = tsPublicResource
    TabOrder = 1
    object tsPublicResource: TTabSheet
      Caption = 'Public Resource'
      ExplicitLeft = 0
      ExplicitTop = 32
      ExplicitHeight = 264
      object Label2: TLabel
        Left = 3
        Top = 8
        Width = 45
        Height = 17
        Caption = 'Resource'
      end
      object Label3: TLabel
        Left = 3
        Top = 51
        Width = 40
        Height = 13
        Caption = 'Headers'
      end
      object Label4: TLabel
        Left = 3
        Top = 130
        Width = 74
        Height = 13
        Caption = 'Response Body'
      end
      object Label10: TLabel
        Left = 494
        Top = 65
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
        Top = 34
        Width = 121
        Height = 25
        Caption = 'Get Public Resource'
        TabOrder = 0
        OnClick = btnPublicResourceClick
      end
      object edtResourcePublic: TEdit
        Left = 3
        Top = 24
        Width = 121
        Height = 21
        TabOrder = 1
        Text = 'user'
      end
      object memoHeadersPublic: TMemo
        Left = 3
        Top = 67
        Width = 312
        Height = 53
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'Accept=application/json')
        ParentFont = False
        TabOrder = 2
      end
      object memoResponsePublic: TMemo
        Left = 3
        Top = 146
        Width = 515
        Height = 231
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 3
        WordWrap = False
      end
      object btnCompilePublic: TButton
        Left = 397
        Top = 3
        Width = 121
        Height = 25
        Caption = 'Compile Headers'
        Enabled = False
        TabOrder = 4
      end
    end
    object tsLogin: TTabSheet
      Caption = 'Login (Basic Auth)'
      ImageIndex = 1
      ExplicitLeft = 12
      ExplicitHeight = 264
      object Label1: TLabel
        Left = 3
        Top = 8
        Width = 106
        Height = 13
        Caption = 'Resource (Basic Auth)'
      end
      object Label6: TLabel
        Left = 3
        Top = 51
        Width = 40
        Height = 13
        Caption = 'Headers'
      end
      object Label7: TLabel
        Left = 3
        Top = 130
        Width = 74
        Height = 13
        Caption = 'Response Body'
      end
      object Label9: TLabel
        Left = 486
        Top = 65
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
        Left = 130
        Top = 8
        Width = 48
        Height = 13
        Caption = 'Username'
      end
      object Label15: TLabel
        Left = 225
        Top = 8
        Width = 46
        Height = 13
        Caption = 'Password'
      end
      object btnLoginBasic: TButton
        Left = 397
        Top = 34
        Width = 121
        Height = 25
        Caption = 'Login (Basic Auth)'
        TabOrder = 0
        OnClick = btnLoginBasicClick
      end
      object edtResourceLogin: TEdit
        Left = 3
        Top = 24
        Width = 121
        Height = 21
        TabOrder = 1
        Text = 'basic_auth'
      end
      object memoHeadersLogin: TMemo
        Left = 3
        Top = 67
        Width = 312
        Height = 53
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'Accept=application/json')
        ParentFont = False
        TabOrder = 2
        WordWrap = False
      end
      object memoResponseLogin: TMemo
        Left = 3
        Top = 146
        Width = 515
        Height = 231
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 3
        WordWrap = False
      end
      object btnCompileLogin: TButton
        Left = 397
        Top = 3
        Width = 121
        Height = 25
        Caption = 'Compile Headers'
        TabOrder = 4
        OnClick = btnCompileLoginClick
      end
      object edtUsername: TEdit
        Left = 130
        Top = 24
        Width = 89
        Height = 21
        TabOrder = 5
        Text = 'me'
      end
      object edtPassword: TEdit
        Left = 225
        Top = 24
        Width = 90
        Height = 21
        TabOrder = 6
        Text = 'mypassword'
      end
    end
    object tsPrivateResource: TTabSheet
      Caption = 'Private Resource'
      ImageIndex = 3
      ExplicitWidth = 281
      ExplicitHeight = 165
      object Label5: TLabel
        Left = 3
        Top = 8
        Width = 106
        Height = 13
        Caption = 'Resource (Basic Auth)'
      end
      object Label11: TLabel
        Left = 494
        Top = 65
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
      object Label12: TLabel
        Left = 3
        Top = 51
        Width = 40
        Height = 13
        Caption = 'Headers'
      end
      object Label13: TLabel
        Left = 3
        Top = 130
        Width = 74
        Height = 13
        Caption = 'Response Body'
      end
      object btnPrivateResource: TButton
        Left = 397
        Top = 34
        Width = 121
        Height = 25
        Caption = 'Get Private Resource'
        TabOrder = 0
        OnClick = btnPrivateResourceClick
      end
      object edtResourcePrivate: TEdit
        Left = 3
        Top = 24
        Width = 121
        Height = 21
        TabOrder = 1
        Text = 'user/details'
      end
      object memoHeadersPrivate: TMemo
        Left = 3
        Top = 67
        Width = 312
        Height = 53
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'Accept=application/json')
        ParentFont = False
        TabOrder = 2
        WordWrap = False
      end
      object memoResponsePrivate: TMemo
        Left = 3
        Top = 146
        Width = 515
        Height = 231
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 3
        WordWrap = False
      end
      object btnCompilePrivate: TButton
        Left = 397
        Top = 3
        Width = 121
        Height = 25
        Caption = 'Compile Headers'
        TabOrder = 4
        OnClick = btnCompilePrivateClick
      end
    end
  end
  object memoLog: TMemo
    Left = 3
    Top = 492
    Width = 527
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
  object httpRestClient: TNetHTTPClient
    Asynchronous = False
    ConnectionTimeout = 60000
    ResponseTimeout = 60000
    AllowCookies = True
    HandleRedirects = True
    UserAgent = 'Embarcadero URI Client/1.0'
    Left = 472
    Top = 104
  end
  object httpRequest: TNetHTTPRequest
    Asynchronous = False
    ConnectionTimeout = 60000
    ResponseTimeout = 60000
    Client = httpRestClient
    Left = 472
    Top = 160
  end
end
