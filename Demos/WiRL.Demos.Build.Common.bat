SET _TARGET=%1
IF [%1] == [] (SET _TARGET="Make")

SET _CONFIG=%2
IF [%2] == [] (SET _CONFIG="Debug")

SET _PLATFORM=%3
IF [%3] == [] (SET _PLATFORM="Win32")

SET BUILDTARGET="/t:%_TARGET%"
SET BUILDCONFIG="/p:config=%_CONFIG%"
SET BUILDPLATFORM="/p:platform=%_PLATFORM%"

SET "ERRORCOUNT=0"

@ECHO OFF
msbuild 01.HelloWorld\DemoHelloWorld.dproj %BUILDTARGET% %BUILDCONFIG% %BUILDPLATFORM% 
IF %ERRORLEVEL% NEQ 0 set /a ERRORCOUNT+=1
msbuild 01.HelloWorld\DemoHelloWorldConsole.dproj %BUILDTARGET% %BUILDCONFIG% %BUILDPLATFORM% 
IF %ERRORLEVEL% NEQ 0 set /a ERRORCOUNT+=1
msbuild 01.HelloWorld\FMXClientProject.dproj %BUILDTARGET% %BUILDCONFIG% %BUILDPLATFORM% 
IF %ERRORLEVEL% NEQ 0 set /a ERRORCOUNT+=1

msbuild 02.ContentTypes\DemoContentTypes.dproj %BUILDTARGET% %BUILDCONFIG% %BUILDPLATFORM% 
IF %ERRORLEVEL% NEQ 0 set /a ERRORCOUNT+=1

msbuild 03.Authorization\DemoAuthorization.dproj %BUILDTARGET% %BUILDCONFIG% %BUILDPLATFORM% 
IF %ERRORLEVEL% NEQ 0 set /a ERRORCOUNT+=1
msbuild 03.Authorization\DemoAuthorizationClient.dproj %BUILDTARGET% %BUILDCONFIG% %BUILDPLATFORM% 
IF %ERRORLEVEL% NEQ 0 set /a ERRORCOUNT+=1

msbuild 04.WebServer\DemoWebServer.dproj %BUILDTARGET% %BUILDCONFIG% %BUILDPLATFORM% 
IF %ERRORLEVEL% NEQ 0 set /a ERRORCOUNT+=1

msbuild 05.SimpleVCLClient\ClientProject.dproj %BUILDTARGET% %BUILDCONFIG% %BUILDPLATFORM% 
IF %ERRORLEVEL% NEQ 0 set /a ERRORCOUNT+=1

msbuild 06.FireDAC\FireDACBasicServer.dproj %BUILDTARGET% %BUILDCONFIG% %BUILDPLATFORM% 
IF %ERRORLEVEL% NEQ 0 set /a ERRORCOUNT+=1
msbuild 06.FireDAC\FireDACBasicClient.dproj %BUILDTARGET% %BUILDCONFIG% %BUILDPLATFORM% 
IF %ERRORLEVEL% NEQ 0 set /a ERRORCOUNT+=1

msbuild 07.MessageBody\DemoMessageBody.dproj %BUILDTARGET% %BUILDCONFIG% %BUILDPLATFORM% 
IF %ERRORLEVEL% NEQ 0 set /a ERRORCOUNT+=1

msbuild 08.Platforms\ConsoleDemo.dproj %BUILDTARGET% %BUILDCONFIG% %BUILDPLATFORM% 
IF %ERRORLEVEL% NEQ 0 set /a ERRORCOUNT+=1
msbuild 08.Platforms\VclDemo.dproj %BUILDTARGET% %BUILDCONFIG% %BUILDPLATFORM% 
IF %ERRORLEVEL% NEQ 0 set /a ERRORCOUNT+=1

msbuild 09.ExtJS\DemoExtJS.dproj %BUILDTARGET% %BUILDCONFIG% %BUILDPLATFORM% 
IF %ERRORLEVEL% NEQ 0 set /a ERRORCOUNT+=1

msbuild 10.Filters\DemoFilters.dproj %BUILDTARGET% %BUILDCONFIG% %BUILDPLATFORM% 
IF %ERRORLEVEL% NEQ 0 set /a ERRORCOUNT+=1

msbuild 11.Validators\DemoValidators.dproj %BUILDTARGET% %BUILDCONFIG% %BUILDPLATFORM% 
IF %ERRORLEVEL% NEQ 0 set /a ERRORCOUNT+=1

msbuild 12.Context\DemoContext.dproj %BUILDTARGET% %BUILDCONFIG% %BUILDPLATFORM% 
IF %ERRORLEVEL% NEQ 0 set /a ERRORCOUNT+=1

msbuild 13.Serialization\DemoSerialization.dproj %BUILDTARGET% %BUILDCONFIG% %BUILDPLATFORM% 
IF %ERRORLEVEL% NEQ 0 set /a ERRORCOUNT+=1

msbuild 14.BasicHttpServer\DemoWebServer.dproj %BUILDTARGET% %BUILDCONFIG% %BUILDPLATFORM% 
IF %ERRORLEVEL% NEQ 0 set /a ERRORCOUNT+=1

msbuild 15.WindowsService\DemoService.dproj %BUILDTARGET% %BUILDCONFIG% %BUILDPLATFORM% 
IF %ERRORLEVEL% NEQ 0 set /a ERRORCOUNT+=1

msbuild 16.Redirection\DemoRedirection.dproj %BUILDTARGET% %BUILDCONFIG% %BUILDPLATFORM% 
IF %ERRORLEVEL% NEQ 0 set /a ERRORCOUNT+=1

msbuild 17.Parameters\DemoParameters.dproj %BUILDTARGET% %BUILDCONFIG% %BUILDPLATFORM% 
IF %ERRORLEVEL% NEQ 0 set /a ERRORCOUNT+=1

msbuild 18.OpenAPI\DemoOpenAPI.dproj %BUILDTARGET% %BUILDCONFIG% %BUILDPLATFORM% 
IF %ERRORLEVEL% NEQ 0 set /a ERRORCOUNT+=1

msbuild 19.CustomConfig\DemoCustomConfig.dproj %BUILDTARGET% %BUILDCONFIG% %BUILDPLATFORM% 
IF %ERRORLEVEL% NEQ 0 set /a ERRORCOUNT+=1

msbuild 20.Exceptions\DemoException.dproj %BUILDTARGET% %BUILDCONFIG% %BUILDPLATFORM% 
IF %ERRORLEVEL% NEQ 0 set /a ERRORCOUNT+=1


IF %ERRORCOUNT% NEQ 0 (
  
  ECHO ===========================================
  ECHO ===    %ERRORCOUNT% WiRL Demos Failed to Compile   ===
  ECHO ===========================================  
  EXIT /B 1
  
) ELSE ( 

  ECHO ===========================================
  ECHO ===    WiRL Demos Compiled Successful   ===
  ECHO ===========================================
  
)    

