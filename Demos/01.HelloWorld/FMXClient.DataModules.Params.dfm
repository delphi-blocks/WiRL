object ParamsModule: TParamsModule
  Height = 480
  Width = 640
  object WiRLClient1: TWiRLClient
    WiRLEngineURL = 'http://localhost:8080/rest'
    ConnectTimeout = 0
    ReadTimeout = -1
    ProxyParams.BasicAuthentication = False
    ProxyParams.ProxyPort = 0
    Left = 280
    Top = 16
  end
  object WiRLClientApplication1: TWiRLClientApplication
    DefaultMediaType = 'text/plain'
    Client = WiRLClient1
    Filters.Strings = (
      '*')
    Readers.Strings = (
      '*')
    Writers.Strings = (
      '*')
    Left = 280
    Top = 72
    object HelloWorldResource: TWiRLClientResource
      Application = WiRLClientApplication1
      Resource = 'helloworld'
      Left = 280
      Top = 128
    end
    object EchoStringResource: TWiRLClientResource
      Application = WiRLClientApplication1
      Resource = 'helloworld/echostring/{AString}'
    end
    object ReverseStringResource: TWiRLClientResource
      Application = WiRLClientApplication1
      Resource = 'helloworld/reversestring/{AString}'
    end
    object PostStreamResource: TWiRLClientResource
      Application = WiRLClientApplication1
      Resource = 'helloworld/poststream'
      CustomHeaders = (
        'Content-Type=application/octet-stream')
    end
  end
end
