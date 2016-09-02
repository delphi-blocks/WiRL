object MainModule: TMainModule
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 284
  Width = 341
  object FDConnection: TFDConnection
    Params.Strings = (
      'DriverID=SQLite')
    ConnectedStoredUsage = []
    LoginPrompt = False
    Left = 40
    Top = 32
  end
  object qryEmployee: TFDQuery
    Connection = FDConnection
    SQL.Strings = (
      'select * from EMPLOYEE &filter')
    Left = 48
    Top = 88
    MacroData = <
      item
        Value = ''
        Name = 'FILTER'
      end>
    object qryEmployeeEMP_NO: TSmallintField
      FieldName = 'EMP_NO'
      Origin = 'EMP_NO'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object qryEmployeeFIRST_NAME: TStringField
      FieldName = 'FIRST_NAME'
      Origin = 'FIRST_NAME'
      Required = True
      Size = 15
    end
    object qryEmployeeLAST_NAME: TStringField
      FieldName = 'LAST_NAME'
      Origin = 'LAST_NAME'
      Required = True
    end
    object qryEmployeePHONE_EXT: TStringField
      FieldName = 'PHONE_EXT'
      Origin = 'PHONE_EXT'
      Size = 4
    end
    object qryEmployeeHIRE_DATE: TSQLTimeStampField
      FieldName = 'HIRE_DATE'
      Origin = 'HIRE_DATE'
      Required = True
    end
    object qryEmployeeDEPT_NO: TStringField
      FieldName = 'DEPT_NO'
      Origin = 'DEPT_NO'
      Required = True
      FixedChar = True
      Size = 3
    end
    object qryEmployeeJOB_CODE: TStringField
      FieldName = 'JOB_CODE'
      Origin = 'JOB_CODE'
      Required = True
      Size = 5
    end
    object qryEmployeeJOB_GRADE: TSmallintField
      FieldName = 'JOB_GRADE'
      Origin = 'JOB_GRADE'
      Required = True
    end
    object qryEmployeeJOB_COUNTRY: TStringField
      FieldName = 'JOB_COUNTRY'
      Origin = 'JOB_COUNTRY'
      Required = True
      Size = 15
    end
    object qryEmployeeSALARY: TBCDField
      FieldName = 'SALARY'
      Origin = 'SALARY'
      Required = True
      Precision = 18
      Size = 2
    end
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 183
    Top = 24
  end
  object qryEmpNoGen: TFDQuery
    Connection = FDConnection
    SQL.Strings = (
      'SELECT GEN_ID(&name, 1) FROM RDB$DATABASE')
    Left = 184
    Top = 88
    MacroData = <
      item
        Value = 'EMP_NO_GEN'
        Name = 'NAME'
      end>
  end
end
