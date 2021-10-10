@echo off

:: Example call -
:: InstallBundle.bat AssetDeliveryDemo Z:\Kastri\Demos\AssetDelivery\Android64\Release Z:\Config\PlayStore.keystore playstore XXXXX XXXXX^ 
::   Z:\Kastri\Demos\AssetDelivery\AssetPacks C:\Android\SDK android-30

:: InstallBundle.bat AssetDeliveryDemo Z:\Kastri\Demos\AssetDelivery\Android64\Release 99AAY19MGL

:: Requires a path to the JDK so that "java.exe" can be found

if "%BDS%" == "" (call rsvars.bat)

set PROJECT=%1
set TARGET_DIR=%~2\%PROJECT%
set KEYSTORE_FILE=%3
set ALIAS=%4
set KEY_PASS=%5
set STORE_PASS=%6
set SERIAL=%~7

set BUNDLE_FILE=%TARGET_DIR%\bin\%PROJECT%.aab
set APKS_FILE=%TARGET_DIR%\bin\%PROJECT%.apks

set BUNDLETOOL_JAR=%BDS%\bin\android\bundletool-all-1.2.0.jar

:: Delete any existing apks file
if exist "%APKS_FILE%" del "%APKS_FILE%"

:: Build apks - note: --local-testing flag is required to test asset packs "locally", otherwise it will error when attempting to fetch the packs
java.exe -jar "%BUNDLETOOL_JAR%" build-apks --bundle="%BUNDLE_FILE%" --output="%APKS_FILE%" --ks "%KEYSTORE_FILE%" --key-pass=pass:%KEY_PASS% --ks-pass=pass:%STORE_PASS% --ks-key-alias=%ALIAS% --local-testing

:: Install apks
if not "%SERIAL%" == "" set DEVICE_ID=--device-id=%SERIAL%
java.exe -jar "%BUNDLETOOL_JAR%" install-apks --apks="%APKS_FILE%" %DEVICE_ID%
