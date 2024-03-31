@echo off
setlocal

:: Determine OS and Architecture
set OS=windows
if "%PROCESSOR_ARCHITECTURE%"=="AMD64" (set ARCH=x64) else (set ARCH=x86)

:: Set the URL for the latest version
set LATEST_VERSION_URL=https://s3.eu-central-1.amazonaws.com/eywa.public/eywa_cli/latest

:: Default to fetching the latest version
for /f %%i in ('curl -s %LATEST_VERSION_URL%') do set VERSION=%%i

:: Check command-line arguments for --version
set VERSION_ARG=
:arg_loop
if "%~1"=="" goto after_arg_loop
if "%~1"=="--version" (
    set VERSION_ARG=%~2
    shift
)
shift
goto arg_loop
:after_arg_loop

if not "%VERSION_ARG%"=="" set VERSION=%VERSION_ARG%

echo Using version: %VERSION%

set LOCATION=eywa_cli/%OS%/%ARCH%/%VERSION%/eywa
set S3URL=https://s3.eu-central-1.amazonaws.com/eywa.public/%LOCATION%

:: Define install location
set INSTALL_DIR=%USERPROFILE%\.eywa\bin\

:: Create the installation directory if it doesn't exist
if not exist "%INSTALL_DIR%" mkdir "%INSTALL_DIR%"

:: Check if Eywa is already installed
if exist "%INSTALL_DIR%\eywa" (
    echo Eywa is already installed at %INSTALL_DIR%eywa.
    goto end
)

:: Use curl to check if the file is accessible
curl -I "%S3URL%" 2>nul | findstr "200 OK" >nul
if errorlevel 1 (
    echo The file "%S3URL%" does not exist or is not accessible.
    goto end
)

:: Download the file
curl -o "%INSTALL_DIR%\eywa" "%S3URL%"

:: Unfortunately, setting execute permissions and modifying the PATH permanently are not straightforward in batch scripts
:: and typically require manual steps or more advanced scripting with PowerShell or similar.

echo %LOCATION% installed successfully. Please add %INSTALL_DIR% to your PATH manually.

:end
endlocal

