@echo off
rem Compile and run the Carik unit tests with instantfpc.
rem Usage: run_tests.bat [runner options]   (default: -a)
rem
rem FastPlaz location: set FASTPLAZ_DIR to the FastPlaz folder (the one that
rem contains src\ and test\). Default: ..\..\..\vendors\fastplaz\FastPlaz (as in extra.cfg)
rem SimpleAI location: SIMPLEAI_DIR, default ..\..\..\vendors\SimpleAI

setlocal
cd /d "%~dp0"

where instantfpc >nul 2>nul
if errorlevel 1 set "PATH=C:\lazarus\fpc\3.2.2\bin\x86_64-win64;%PATH%"

if not defined FASTPLAZ_DIR set "FASTPLAZ_DIR=%~dp0..\..\..\vendors\fastplaz\FastPlaz"
if not defined SIMPLEAI_DIR set "SIMPLEAI_DIR=%~dp0..\..\..\vendors\SimpleAI"
if not exist "%FASTPLAZ_DIR%\src\library\common.pas" (
  echo FastPlaz not found: %FASTPLAZ_DIR%
  echo Set FASTPLAZ_DIR to the FastPlaz folder.
  exit /b 2
)
set "FP=%FASTPLAZ_DIR%"
set "SA=%SIMPLEAI_DIR%\source\package"
set "CR=%~dp0.."

rem instantfpc does not accept -Fu paths on its command line, pass them here.
rem -B: always rebuild, the cache only tracks the main program source.
rem Unit paths mirror source\carik\extra.cfg.
set "INSTANTFPCOPTIONS=-B -Fi%FP% -Fi%CR%\carik\config -Fu%FP%\test -Fu%FP% -Fu%FP%\src\helper -Fu%FP%\src\systems -Fu%FP%\src\library -Fu%FP%\src\util -Fu%FP%\src\model -Fu%FP%\src\integration -Fu%FP%\src\cms\controller -Fu%FP%\src\cms\model -Fu%FP%\src\cms\util -Fu%FP%\src\vendor -Fu%FP%\src\vendor\dcrypt -Fu%SA%\src -Fu%SA%\src\controller -Fu%CR%\common"

echo Compiling (instantfpc)...
if "%~1"=="" (
  instantfpc carik_test.lpr -a
) else (
  instantfpc carik_test.lpr %*
)
exit /b %errorlevel%
