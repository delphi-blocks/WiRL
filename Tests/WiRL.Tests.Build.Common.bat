SET _TARGET=%1
IF [%1] == [] (SET _TARGET="Make")

SET _CONFIG=%2
IF [%2] == [] (SET _CONFIG="Release")

SET _PLATFORM=%3
IF [%3] == [] (SET _PLATFORM="Win32")

SET BUILDTARGET="/t:%_TARGET%"
SET BUILDCONFIG="/p:config=%_CONFIG%"
SET BUILDPLATFORM="/p:platform=%_PLATFORM%"

ECHO Compile...
@ECHO OFF
msbuild Framework\WiRL.Tests.Framework.dproj %BUILDTARGET% %BUILDCONFIG% %BUILDPLATFORM% /property:DCC_Define=CI >nul

IF %ERRORLEVEL% NEQ 0 (
  
  ECHO ===========================================
  ECHO ===    WiRL Tests Failed to Compile     ===
  ECHO ===========================================  
  EXIT /B 1
  
) ELSE ( 
  Framework\%_PLATFORM%\%_CONFIG%\WiRL.Tests.Framework.exe 
)    
