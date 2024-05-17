@echo off
call rsvars.bat

set patchexe=%ProgramW6432%\Git\usr\bin\patch.exe
if not exist "%patchexe%" goto nopatch 

copy /V /Y "%BDS%\source\fmx\FMX.PushNotification.FCM.iOS.pas" . >nul
if not ERRORLEVEL 0 goto end

"%patchexe%" --binary FMX.PushNotification.FCM.iOS.pas FMX.PushNotification.FCM.iOS.patch
if ERRORLEVEL 0 goto end
echo Patch failed
goto end

:nopatch
echo You do not appear to have Git installed, or the patch command is missing

:end

