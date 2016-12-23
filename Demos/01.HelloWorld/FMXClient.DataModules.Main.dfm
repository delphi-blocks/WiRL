object MainDataModule: TMainDataModule
  OldCreateOrder = False
  Height = 394
  Width = 605
  object WiRLClient1: TWiRLClient
    WiRLEngineURL = 'http://localhost:8080/rest'
    ConnectTimeout = 0
    ReadTimeout = -1
    Left = 280
    Top = 16
  end
  object WiRLClientApplication1: TWiRLClientApplication
    DefaultMediaType = 'text/plain'
    AppName = 'app'
    Client = WiRLClient1
    Left = 280
    Top = 72
  end
  object HelloWorldResource: TWiRLClientResource
    Application = WiRLClientApplication1
    SpecificAccept = 'text/plain'
    Resource = 'helloworld'
    Left = 280
    Top = 128
  end
  object EchoStringResource: TWiRLClientSubResource
    Application = WiRLClientApplication1
    Resource = 'echostring'
    ParentResource = HelloWorldResource
    Left = 224
    Top = 208
  end
  object ReverseStringResource: TWiRLClientSubResource
    Application = WiRLClientApplication1
    Resource = 'reversestring'
    ParentResource = HelloWorldResource
    Left = 328
    Top = 192
  end
  object PostExampleResource: TWiRLClientSubResourceJSON
    Application = WiRLClientApplication1
    Resource = 'postexample'
    ParentResource = HelloWorldResource
    Left = 72
    Top = 288
  end
end
