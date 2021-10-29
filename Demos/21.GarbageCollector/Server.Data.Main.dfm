object DataMain: TDataMain
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 408
  Width = 594
  PixelsPerInch = 96
  object qryTestSQL: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'select * from EMPLOYEE')
    Left = 384
    Top = 208
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'ConnectionDef=SQLite_Demo_Pooled')
    Connected = True
    Left = 384
    Top = 144
  end
  object DataBaseConnection: TFDConnection
    Left = 264
    Top = 56
  end
end
