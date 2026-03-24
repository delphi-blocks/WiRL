object WebModule2: TWebModule2
  OnCreate = WebModuleCreate
  OnDestroy = WebModuleDestroy
  Actions = <
    item
      Name = 'itmSSE'
      PathInfo = '/streaming/sse'
      OnAction = WebModule2WebActionItem1Action
    end
    item
      Name = 'itmChunks'
      PathInfo = '/streaming/chunks'
      OnAction = WebModule2WebActionItem2Action
    end
    item
      Default = True
      Name = 'DefaultHandler'
      PathInfo = '/'
      OnAction = WebModule2DefaultHandlerAction
    end>
  Height = 230
  Width = 415
end
