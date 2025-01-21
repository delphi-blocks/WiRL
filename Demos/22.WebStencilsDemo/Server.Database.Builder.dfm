object DatabaseBuilder: TDatabaseBuilder
  OldCreateOrder = False
  Height = 323
  Width = 463
  object FDConnection: TFDConnection
    Params.Strings = (
      'DriverID=SQLite')
    ConnectedStoredUsage = []
    LoginPrompt = False
    Left = 64
    Top = 56
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 183
    Top = 24
  end
  object FDScript1: TFDScript
    SQLScripts = <
      item
        Name = 'CREATE_EMPLOYEE'
        SQL.Strings = (
          'CREATE TABLE IF NOT EXISTS EMPLOYEE ('
          '    EMP_NO       INTEGER NOT NULL,'
          '    FIRST_NAME   VARCHAR(50) NOT NULL,'
          '    LAST_NAME    VARCHAR(50) NOT NULL,'
          '    PHONE_EXT    VARCHAR(4),'
          '    HIRE_DATE    TIMESTAMP,'
          '    DEPT_NO      VARCHAR(3),'
          '    JOB_CODE    VARCHAR(5),'
          '    JOB_GRADE    INTEGER,'
          '    JOB_COUNTRY  VARCHAR(15),'
          '    SALARY       NUMERIC(10,2)'
          ');'
          ''
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (2, '#39'Robert'#39', '#39'Nelson'#39', '#39'250'#39', '#39'1988-12-28 ' +
            '00:00:00'#39', '#39'600'#39', '#39'VP'#39', 2, '#39'USA'#39', 105900);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (4, '#39'Bruce'#39', '#39'Dug'#39', '#39'233'#39', '#39'1988-12-28 00:0' +
            '0:00'#39', '#39'621'#39', '#39'Eng'#39', 2, '#39'USA'#39', 97500);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (5, '#39'Kim'#39', '#39'Lambert'#39', '#39'22'#39', '#39'2016-05-26 00:' +
            '00:00'#39', '#39'130'#39', '#39'Eng'#39', 2, '#39'USA'#39', 102750);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (8, '#39'Leslie'#39', '#39'Johnson'#39', '#39'410'#39', '#39'2016-05-26' +
            ' 00:00:00'#39', '#39'180'#39', '#39'Mktg'#39', 3, '#39'USA'#39', 64635);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (9, '#39'Phil'#39', '#39'Forest'#39', '#39'229'#39', '#39'1989-04-17 00' +
            ':00:00'#39', '#39'622'#39', '#39'Mngr'#39', 3, '#39'USA'#39', 75060);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (11, '#39'K. J.'#39', '#39'Weston'#39', '#39'348'#39', '#39'1990-01-17 ' +
            '00:00:00'#39', '#39'130'#39', '#39'SRep'#39', 4, '#39'USA'#39', 86292.94);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (12, '#39'Terri'#39', '#39'Lee'#39', '#39'256'#39', '#39'2016-05-26 00:' +
            '00:00'#39', '#39'000'#39', '#39'Admin'#39', 4, '#39'USA'#39', 53793);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (14, '#39'Stewart'#39', '#39'Hall'#39', '#39'227'#39', '#39'1990-06-04 ' +
            '00:00:00'#39', '#39'900'#39', '#39'Finan'#39', 3, '#39'USA'#39', 69482.63);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (15, '#39'Katherine'#39', '#39'Young'#39', '#39'231'#39', '#39'1990-06-' +
            '14 00:00:00'#39', '#39'623'#39', '#39'Mngr'#39', 3, '#39'USA'#39', 67241.25);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (20, '#39'Chris'#39', '#39'Papadopoulos'#39', '#39'887'#39', '#39'1990-' +
            '01-01 00:00:00'#39', '#39'671'#39', '#39'Mngr'#39', 3, '#39'USA'#39', 89655);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (24, '#39'Pete'#39', '#39'Fisher'#39', '#39'888'#39', '#39'1990-09-12 0' +
            '0:00:00'#39', '#39'671'#39', '#39'Eng'#39', 3, '#39'USA'#39', 81810.19);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (28, '#39'Ann'#39', '#39'Bennet'#39', '#39'5'#39', '#39'1991-02-01 00:0' +
            '0:00'#39', '#39'120'#39', '#39'Admin'#39', 5, '#39'England'#39', 22935);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (29, '#39'Roger'#39', '#39'De Souza'#39', '#39'290'#39', '#39'1991-02-1' +
            '8 00:00:00'#39', '#39'623'#39', '#39'Eng'#39', 3, '#39'USA'#39', 69482.63);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (34, '#39'Janeth'#39', '#39'Baldwin'#39', '#39'2'#39', '#39'1991-03-21 ' +
            '00:00:00'#39', '#39'110'#39', '#39'Sales'#39', 3, '#39'USA'#39', 61637.81);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (36, '#39'Roger'#39', '#39'Reeves'#39', '#39'6'#39', '#39'1991-04-25 00' +
            ':00:00'#39', '#39'120'#39', '#39'Sales'#39', 3, '#39'England'#39', 33620.63);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (37, '#39'Willie'#39', '#39'Stansbury'#39', '#39'7'#39', '#39'1991-04-2' +
            '5 00:00:00'#39', '#39'120'#39', '#39'Eng'#39', 4, '#39'England'#39', 39224.06);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (44, '#39'Leslie'#39', '#39'Phong'#39', '#39'216'#39', '#39'1991-06-03 ' +
            '00:00:00'#39', '#39'623'#39', '#39'Eng'#39', 4, '#39'USA'#39', 56034.38);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (45, '#39'Ashok'#39', '#39'Ramanathan'#39', '#39'209'#39', '#39'1991-08' +
            '-01 00:00:00'#39', '#39'621'#39', '#39'Eng'#39', 3, '#39'USA'#39', 80689.5);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (46, '#39'Walter'#39', '#39'Steadman'#39', '#39'210'#39', '#39'1991-08-' +
            '09 00:00:00'#39', '#39'900'#39', '#39'CFO'#39', 1, '#39'USA'#39', 116100);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (52, '#39'Carol'#39', '#39'Nordstrom'#39', '#39'420'#39', '#39'1991-10-' +
            '02 00:00:00'#39', '#39'180'#39', '#39'PRel'#39', 4, '#39'USA'#39', 42742.5);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (61, '#39'Luke'#39', '#39'Leung'#39', '#39'3'#39', '#39'1992-02-18 00:0' +
            '0:00'#39', '#39'110'#39', '#39'SRep'#39', 4, '#39'USA'#39', 68805);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (65, '#39'Sue Anne'#39', '#39'O'#39#39'Brien'#39', '#39'877'#39', '#39'1992-0' +
            '3-23 00:00:00'#39', '#39'670'#39', '#39'Admin'#39', 5, '#39'USA'#39', 31275);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (71, '#39'Jennifer M.'#39', '#39'Burbank'#39', '#39'289'#39', '#39'1992' +
            '-04-15 00:00:00'#39', '#39'622'#39', '#39'Eng'#39', 3, '#39'USA'#39', 53167.5);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (72, '#39'Claudia'#39', '#39'Sutherland'#39', NULL, '#39'1992-0' +
            '4-20 00:00:00'#39', '#39'140'#39', '#39'SRep'#39', 4, '#39'Canada'#39', 100914);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (83, '#39'Dana'#39', '#39'Bishop'#39', '#39'290'#39', '#39'1992-06-01 0' +
            '0:00:00'#39', '#39'621'#39', '#39'Eng'#39', 3, '#39'USA'#39', 62550);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (85, '#39'Mary S.'#39', '#39'MacDonald'#39', '#39'477'#39', '#39'1992-0' +
            '6-01 00:00:00'#39', '#39'100'#39', '#39'VP'#39', 2, '#39'USA'#39', 111262.5);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (94, '#39'Randy'#39', '#39'Williams'#39', '#39'892'#39', '#39'1992-08-0' +
            '8 00:00:00'#39', '#39'672'#39', '#39'Mngr'#39', 4, '#39'USA'#39', 56295);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (105, '#39'Oliver H.'#39', '#39'Bender'#39', '#39'255'#39', '#39'1992-1' +
            '0-08 00:00:00'#39', '#39'000'#39', '#39'CEO'#39', 1, '#39'USA'#39', 212850);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (107, '#39'Kevin'#39', '#39'Cook'#39', '#39'894'#39', '#39'1993-02-01 0' +
            '0:00:00'#39', '#39'670'#39', '#39'Dir'#39', 2, '#39'USA'#39', 111262.5);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (109, '#39'Kelly'#39', '#39'Brown'#39', '#39'202'#39', '#39'1993-02-04 ' +
            '00:00:00'#39', '#39'600'#39', '#39'Admin'#39', 5, '#39'USA'#39', 27000);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (110, '#39'Yuki'#39', '#39'Ichida'#39', '#39'22'#39', '#39'1993-02-04 0' +
            '0:00:00'#39', '#39'115'#39', '#39'Eng'#39', 3, '#39'Japan'#39', 6000000);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (113, '#39'Mary'#39', '#39'Page'#39', '#39'845'#39', '#39'1993-04-12 00' +
            ':00:00'#39', '#39'671'#39', '#39'Eng'#39', 4, '#39'USA'#39', 48000);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (114, '#39'Bill'#39', '#39'Parker'#39', '#39'247'#39', '#39'1993-06-01 ' +
            '00:00:00'#39', '#39'623'#39', '#39'Eng'#39', 5, '#39'USA'#39', 35000);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (118, '#39'Takashi'#39', '#39'Yamamoto'#39', '#39'23'#39', '#39'1993-07' +
            '-01 00:00:00'#39', '#39'115'#39', '#39'SRep'#39', 4, '#39'Japan'#39', 7480000);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (121, '#39'Roberto'#39', '#39'Ferrari'#39', '#39'1'#39', '#39'1993-07-1' +
            '2 00:00:00'#39', '#39'125'#39', '#39'SRep'#39', 4, '#39'Italy'#39', 99000000);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (127, '#39'Michael'#39', '#39'Yanowski'#39', '#39'492'#39', '#39'1993-0' +
            '8-09 00:00:00'#39', '#39'100'#39', '#39'SRep'#39', 4, '#39'USA'#39', 44000);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (134, '#39'Jacques'#39', '#39'Glon'#39', NULL, '#39'1993-08-23 ' +
            '00:00:00'#39', '#39'123'#39', '#39'SRep'#39', 4, '#39'France'#39', 390500);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (136, '#39'Scott'#39', '#39'Johnson'#39', '#39'265'#39', '#39'1993-09-1' +
            '3 00:00:00'#39', '#39'623'#39', '#39'Doc'#39', 3, '#39'USA'#39', 60000);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (138, '#39'T.J.'#39', '#39'Green'#39', '#39'218'#39', '#39'1993-11-01 0' +
            '0:00:00'#39', '#39'621'#39', '#39'Eng'#39', 4, '#39'USA'#39', 36000);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (141, '#39'Pierre'#39', '#39'Osborne'#39', NULL, '#39'1994-01-0' +
            '3 00:00:00'#39', '#39'121'#39', '#39'SRep'#39', 4, '#39'Switzerland'#39', 110000);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (144, '#39'John'#39', '#39'Montgomery'#39', '#39'820'#39', '#39'1994-03' +
            '-30 00:00:00'#39', '#39'672'#39', '#39'Eng'#39', 5, '#39'USA'#39', 35000);'
          
            'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, ' +
            'HIRE_DATE, DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY)'
          
            '              VALUES (145, '#39'Mark'#39', '#39'Guckenheimer'#39', '#39'221'#39', '#39'1994-' +
            '05-02 00:00:00'#39', '#39'622'#39', '#39'Eng'#39', 5, '#39'USA'#39', 32000);')
      end>
    Connection = FDConnection
    ScriptOptions.BreakOnError = True
    Params = <>
    Macros = <>
    FetchOptions.AssignedValues = [evItems, evAutoClose, evAutoFetchAll]
    FetchOptions.AutoClose = False
    FetchOptions.Items = [fiBlobs, fiDetails]
    ResourceOptions.AssignedValues = [rvMacroCreate, rvMacroExpand, rvDirectExecute, rvPersistent]
    ResourceOptions.MacroCreate = False
    ResourceOptions.DirectExecute = True
    Left = 160
    Top = 144
  end
end
