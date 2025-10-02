object MainWebModule: TMainWebModule
  Actions = <
    item
      Default = True
      Name = 'DefaultHandler'
      PathInfo = '/webbroker'
      OnAction = WebModule1DefaultHandlerAction
    end>
  Height = 230
  Width = 415
end
