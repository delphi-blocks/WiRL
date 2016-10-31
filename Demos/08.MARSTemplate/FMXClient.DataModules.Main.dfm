object MainDataModule: TMainDataModule
  OldCreateOrder = False
  Height = 411
  Width = 518
  object WiRLClient: TWiRLClient
    WiRLEngineURL = 'http://localhost:8080/rest'
    ConnectTimeout = 0
    ReadTimeout = -1
    Left = 88
    Top = 24
  end
  object WiRLApplication: TWiRLClientApplication
    DefaultMediaType = 'application/json'
    AppName = 'default'
    Client = WiRLClient
    Left = 88
    Top = 80
  end
end
