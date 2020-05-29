@ECHO OFF

@SET ORI_PATH=%PATH%

REG QUERY HKCU\Software\Embarcadero\BDS\18.0 >nul 2>&1
IF %ERRORLEVEL% NEQ 0 goto :delphi102 

	ECHO ++++++++++++++++++++++++++++++++++++++++++++++++++
	ECHO Delphi 10.1 Berlin
	@SET PATH=%ORI_PATH%
	CALL WiRL.Tests.Build.101.bat


:delphi102

REG QUERY HKCU\Software\Embarcadero\BDS\19.0 >nul 2>&1
IF %ERRORLEVEL% NEQ 0 goto :delphi103

	ECHO.
	ECHO ++++++++++++++++++++++++++++++++++++++++++++++++++
	ECHO Delphi 10.2 Tokyo
	@SET PATH=%ORI_PATH%
	CALL WiRL.Tests.Build.102.bat

)

:delphi103

REG QUERY HKCU\Software\Embarcadero\BDS\20.0 >nul 2>&1
IF %ERRORLEVEL% NEQ 0 goto :delphi104 

	ECHO.
	ECHO ++++++++++++++++++++++++++++++++++++++++++++++++++
	ECHO Delphi 10.3 Rio
	@SET PATH=%ORI_PATH%
	CALL WiRL.Tests.Build.103.bat

:delphi104

REG QUERY HKCU\Software\Embarcadero\BDS\21.0 >nul 2>&1
IF %ERRORLEVEL% NEQ 0 goto :end

	ECHO.
	ECHO ++++++++++++++++++++++++++++++++++++++++++++++++++
	ECHO Delphi 10.4 Sydney
	@SET PATH=%ORI_PATH%
	CALL WiRL.Tests.Build.104.bat

:end

pause