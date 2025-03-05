object Form29: TForm29
  Left = 0
  Top = 0
  Caption = 'ChunksClient Demo'
  ClientHeight = 346
  ClientWidth = 651
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Segoe UI'
  Font.Style = []
  DesignSize = (
    651
    346)
  TextHeight = 17
  object ButtonIndySync: TButton
    Left = 483
    Top = 8
    Width = 162
    Height = 30
    Anchors = [akTop, akRight]
    Caption = 'Test sync (Indy)'
    TabOrder = 0
    OnClick = ButtonIndySyncClick
  end
  object MemoLog: TMemo
    Left = 8
    Top = 8
    Width = 469
    Height = 332
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 1
    ExplicitWidth = 289
    ExplicitHeight = 281
  end
  object ButtonIndyAsync: TButton
    Left = 483
    Top = 44
    Width = 162
    Height = 30
    Anchors = [akTop, akRight]
    Caption = 'Test chunked (Indy)'
    TabOrder = 2
    OnClick = ButtonIndyAsyncClick
  end
  object ButtonHttpSync: TButton
    Left = 483
    Top = 101
    Width = 162
    Height = 30
    Anchors = [akTop, akRight]
    Caption = 'Test sync (HttpClient)'
    TabOrder = 3
    OnClick = ButtonHttpSyncClick
  end
  object ButtonHttpAsync: TButton
    Left = 483
    Top = 137
    Width = 160
    Height = 30
    Anchors = [akTop, akRight]
    Caption = 'Test chunked (HttpClient)'
    TabOrder = 4
    OnClick = ButtonHttpAsyncClick
  end
  object IdHTTPSync: TIdHTTP
    ProxyParams.BasicAuthentication = False
    ProxyParams.ProxyPort = 0
    Request.ContentLength = -1
    Request.ContentRangeEnd = -1
    Request.ContentRangeStart = -1
    Request.ContentRangeInstanceLength = -1
    Request.Accept = 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8'
    Request.BasicAuthentication = False
    Request.UserAgent = 'Mozilla/3.0 (compatible; Indy Library)'
    Request.Ranges.Units = 'bytes'
    Request.Ranges = <>
    HTTPOptions = [hoForceEncodeParams]
    Left = 64
    Top = 224
  end
  object IdHTTPAsync: TIdHTTP
    ProxyParams.BasicAuthentication = False
    ProxyParams.ProxyPort = 0
    Request.ContentLength = -1
    Request.ContentRangeEnd = -1
    Request.ContentRangeStart = -1
    Request.ContentRangeInstanceLength = -1
    Request.Accept = 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8'
    Request.BasicAuthentication = False
    Request.UserAgent = 'Mozilla/3.0 (compatible; Indy Library)'
    Request.Ranges.Units = 'bytes'
    Request.Ranges = <>
    HTTPOptions = [hoForceEncodeParams]
    OnChunkReceived = IdHTTPAsyncChunkReceived
    Left = 184
    Top = 224
  end
  object NetHTTPClientSync: TNetHTTPClient
    UserAgent = 'Embarcadero URI Client/1.0'
    Left = 336
    Top = 160
  end
  object NetHTTPClientAsync: TNetHTTPClient
    UserAgent = 'Embarcadero URI Client/1.0'
    OnReceiveDataEx = NetHTTPClientAsyncReceiveDataEx
    Left = 336
    Top = 224
  end
end
