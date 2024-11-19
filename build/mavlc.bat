@rem
@rem Copyright 2015 the original author or authors.
@rem
@rem Licensed under the Apache License, Version 2.0 (the "License");
@rem you may not use this file except in compliance with the License.
@rem You may obtain a copy of the License at
@rem
@rem      https://www.apache.org/licenses/LICENSE-2.0
@rem
@rem Unless required by applicable law or agreed to in writing, software
@rem distributed under the License is distributed on an "AS IS" BASIS,
@rem WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
@rem See the License for the specific language governing permissions and
@rem limitations under the License.
@rem

@if "%DEBUG%"=="" @echo off
@rem ##########################################################################
@rem
@rem  mavlc startup script for Windows
@rem
@rem ##########################################################################

@rem Set local scope for the variables with windows NT shell
if "%OS%"=="Windows_NT" setlocal

set DIRNAME=%~dp0
if "%DIRNAME%"=="" set DIRNAME=.
set APP_BASE_NAME=%~n0
set APP_HOME=%DIRNAME%..

@rem Resolve any "." and ".." in APP_HOME to make it shorter.
for %%i in ("%APP_HOME%") do set APP_HOME=%%~fi

@rem Add default JVM options here. You can also use JAVA_OPTS and MAVLC_OPTS to pass JVM options to this script.
set DEFAULT_JVM_OPTS=

@rem Find java.exe
if defined JAVA_HOME goto findJavaFromJavaHome

set JAVA_EXE=java.exe
%JAVA_EXE% -version >NUL 2>&1
if %ERRORLEVEL% equ 0 goto execute

echo.
echo ERROR: JAVA_HOME is not set and no 'java' command could be found in your PATH.
echo.
echo Please set the JAVA_HOME variable in your environment to match the
echo location of your Java installation.

goto fail

:findJavaFromJavaHome
set JAVA_HOME=%JAVA_HOME:"=%
set JAVA_EXE=%JAVA_HOME%/bin/java.exe

if exist "%JAVA_EXE%" goto execute

echo.
echo ERROR: JAVA_HOME is set to an invalid directory: %JAVA_HOME%
echo.
echo Please set the JAVA_HOME variable in your environment to match the
echo location of your Java installation.

goto fail

:execute
@rem Setup the command line

set CLASSPATH=\Users\sterben\Desktop\Studium\3rd-Semester\CB\eicb-p1\bin;\Users\sterben\Desktop\Studium\3rd-Semester\CB\eicb-p1\build\classes\java\main;\Users\sterben\.gradle\caches\modules-2\files-2.1\commons-io\commons-io\2.4\b1b6ea3b7e4aa4f492509a4952029cd8e48019ad\commons-io-2.4.jar;\Users\sterben\.gradle\caches\modules-2\files-2.1\commons-cli\commons-cli\1.3.1\1303efbc4b181e5a58bf2e967dc156a3132b97c0\commons-cli-1.3.1.jar;\Users\sterben\.gradle\caches\modules-2\files-2.1\com.thoughtworks.xstream\xstream\1.4.20\e2315b8b2e95e9f21697833c8e56cdd9c98a5ee\xstream-1.4.20.jar;\Users\sterben\.gradle\caches\modules-2\files-2.1\org.xmlunit\xmlunit-matchers\2.2.1\26c2afc03381d88ffa32ef2351c69e8ae90c3a8c\xmlunit-matchers-2.2.1.jar;\Users\sterben\.gradle\caches\modules-2\files-2.1\org.xmlunit\xmlunit-core\2.2.1\1f3c89bdf2800ab830a0b556c883ecf99083460c\xmlunit-core-2.2.1.jar;\Users\sterben\.gradle\caches\modules-2\files-2.1\io.github.x-stream\mxparser\1.2.2\476fb3b3bb3716cad797cd054ce45f89445794e9\mxparser-1.2.2.jar;\Users\sterben\.gradle\caches\modules-2\files-2.1\org.hamcrest\hamcrest-core\1.3\42a25dc3219429f0e5d060061f71acb49bf010a0\hamcrest-core-1.3.jar;\Users\sterben\.gradle\caches\modules-2\files-2.1\xmlpull\xmlpull\1.1.3.1\2b8e230d2ab644e4ecaa94db7cdedbc40c805dfa\xmlpull-1.1.3.1.jar


@rem Execute mavlc
"%JAVA_EXE%" %DEFAULT_JVM_OPTS% %JAVA_OPTS% %MAVLC_OPTS%  -classpath "%CLASSPATH%" mavlc.Driver %*

:end
@rem End local scope for the variables with windows NT shell
if %ERRORLEVEL% equ 0 goto mainEnd

:fail
rem Set variable MAVLC_EXIT_CONSOLE if you need the _script_ return code instead of
rem the _cmd.exe /c_ return code!
set EXIT_CODE=%ERRORLEVEL%
if %EXIT_CODE% equ 0 set EXIT_CODE=1
if not ""=="%MAVLC_EXIT_CONSOLE%" exit %EXIT_CODE%
exit /b %EXIT_CODE%

:mainEnd
if "%OS%"=="Windows_NT" endlocal

:omega
