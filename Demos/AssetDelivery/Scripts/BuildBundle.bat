@echo off

:: Example call -
:: BuildBundle.bat AssetDeliveryDemo Z:\Kastri\Demos\AssetDelivery\Android64\Release Z:\Config\PlayStore.keystore playstore XXXXX XXXXX^ 
::   Z:\Kastri\Demos\AssetDelivery\AssetPacks C:\Android\SDK android-30

:: Requires a path to the JDK so that "jar" and "java.exe" can be found

set CURRENT_DIR=%cd%

if "%BDS%" == "" (call rsvars.bat)

if "%BDS%" == "" (
  echo RADStudio batch file is missing or failed to set vars
  goto finish
)

where /Q java.exe
if ERRORLEVEL 1 (
  echo Unable to find java.exe. Please ensure that it is in the system's PATH
  goto finish
)

set PROJECT=%1
set TARGET_DIR=%~2\%PROJECT%
set KEYSTORE_FILE=%3
set ALIAS=%4
set KEY_PASS=%5
set STORE_PASS=%6
set ASSET_PACKS_DIR=%7
set SDK_DIR=%8
set API_LEVEL=%9

set BASE_FILE=base.zip
set MANIFEST_FILE=AndroidManifest.xml
set BASE_DIR=%TARGET_DIR%\base
set OUTPUT_DIR=%TARGET_DIR%\bin
set BUNDLE_FILE=%OUTPUT_DIR%\%PROJECT%.aab
set MODULE_FILES="%TARGET_DIR%\%BASE_FILE%"

if not exist "%BASE_DIR%" (
  echo %BASE_DIR:"=% does not exist - please ensure that you supply the correct folder path, and you use Project, Deploy in Delphi first
  goto finish
)

:: Stripping quotes to test existence
set PACKS_DIR=%ASSET_PACKS_DIR:"=%
if not "%PACKS_DIR%" == "" (
  if not exist "%PACKS_DIR%" (
    echo Asset packs root folder %PACKS_DIR% does not exist
    goto finish
  )
)

:: Set variables
set ANDROID_JAR=%SDK_DIR%\platforms\%API_LEVEL%\android.jar
set AAPT2_PATH=%BDS%\bin\android\aapt2.exe
set BUNDLETOOL_JAR=%BDS%\bin\android\bundletool-all-1.2.0.jar

if not exist "%SDK_DIR%\platforms\%API_LEVEL%" (
  echo SDK API level %SDK_DIR%\platforms\%API_LEVEL% does not exist
  goto finish
)

:: Build asset pack modules
:: **** Inside the for loop, all variables being SET in the loop must be expanded using the ! character
setlocal enabledelayedexpansion
for /D %%d in (%ASSET_PACKS_DIR%\*) do (
  set ROOT_DIR=%%d
  set PACK_MANIFEST_DIR=!ROOT_DIR!\pack\manifest
  set WORKING_FILE=!ROOT_DIR!\working.zip
  for %%f in ("!ROOT_DIR!") do set ASSET_FILE=%%~nxf

  :: Change to the root dir
  cd /D !ROOT_DIR!

  :: Create linked archive
  "%AAPT2_PATH%" link --proto-format --auto-add-overlay -o "working.zip" -I "%ANDROID_JAR%" --manifest "%MANIFEST_FILE%" -A "pack\assets"

  :: Extract manifest
  if not exist !PACK_MANIFEST_DIR! mkdir !PACK_MANIFEST_DIR!
  cd !PACK_MANIFEST_DIR!
  jar xf !WORKING_FILE! %MANIFEST_FILE%

  :: Delete the working file
  del !WORKING_FILE!

  :: Archive the asset module
  cd /D !ROOT_DIR!\pack
  jar cfM "!ASSET_PACKS_DIR!\!ASSET_FILE!.zip" *
)
endlocal

:: Build modules list by iterating asset packs
if defined ASSET_PACKS_DIR (
  for %%f in (%ASSET_PACKS_DIR%\*.zip) do call set "MODULE_FILES=%%MODULE_FILES%%,"%%~nxf""
) 

:: Archive base dir
cd /D %BASE_DIR%
jar cfM "..\%BASE_FILE%" *

cd /D %ASSET_PACKS_DIR%
if exist "%BUNDLE_FILE%" del "%BUNDLE_FILE%"  
:: Build the bundle...  removed:  --config=%TARGET_DIR%\buildconfig.json
java.exe -jar "%BUNDLETOOL_JAR%" build-bundle --modules=%MODULE_FILES% --output="%BUNDLE_FILE%" 

:: Sign it
jarsigner.exe -keystore "%KEYSTORE_FILE%" -keypass %KEY_PASS% -storepass %STORE_PASS% "%BUNDLE_FILE%" %ALIAS%

:: Delete the base.zip (no longer required)
del "%TARGET_DIR%\%BASE_FILE%"

:finish

:: Go back to original folder
cd /D %CURRENT_DIR%