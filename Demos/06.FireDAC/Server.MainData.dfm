inherited MainDataResource: TMainDataResource
  OldCreateOrder = True
  Width = 412
  object FDConnection1: TFDConnection
    Params.Strings = (
      'ConnectionDef=SQLite_Demo')
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    Connected = True
    LoginPrompt = False
    Left = 48
    Top = 24
  end
  object employee: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'select * from Employees ')
    Left = 48
    Top = 88
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 304
    Top = 24
  end
end
