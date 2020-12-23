object MainDataModule: TMainDataModule
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 394
  Width = 604
  object WiRLClient1: TWiRLClient
    WiRLEngineURL = 'http://localhost:8080/rest'
    ConnectTimeout = 0
    ReadTimeout = -1
    ProxyParams.BasicAuthentication = False
    ProxyParams.ProxyPort = 0
    OnBeforeCommand = WiRLClient1BeforeCommand
    NoProtocolErrorException = False
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
  object PersonResource: TWiRLClientSubResource
    Application = WiRLClientApplication1
    SpecificAccept = 'application/json'
    Resource = 'person'
    ParentResource = HelloWorldResource
    Left = 328
    Top = 280
  end
  object WiRLClientResource1: TWiRLClientResource
    Application = WiRLClientApplication1
    Resource = 'helloworld/person'
    Left = 456
    Top = 240
  end
  object NetHTTPClient1: TNetHTTPClient
    Asynchronous = False
    ConnectionTimeout = 60000
    ResponseTimeout = 60000
    HandleRedirects = True
    AllowCookies = True
    UserAgent = 'Embarcadero URI Client/1.0'
    Left = 464
    Top = 96
  end
  object Timer1: TTimer
    Left = 480
    Top = 184
  end
  object IdHTTP1: TIdHTTP
    AllowCookies = True
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
    Left = 488
    Top = 328
  end
end
