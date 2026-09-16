@echo off
setlocal enabledelayedexpansion

rem Startet die C-Test-Auswertung aus dem Installationsordner.
rem Zuerst wird das mitgelieferte R verwendet, danach ein installiertes R.

set "RootDir=%~dp0"
set "RPathFound=false"

if exist "%RootDir%R\bin\x64\R.exe" (
    set "R=%RootDir%R\bin\x64\R.exe"
    set "RPathFound=true"
)

if not "!RPathFound!"=="true" (
    for %%d in ("C:\Program Files\R" "C:\Program Files (x86)\R") do (
        for /d %%i in ("%%~d\R-*") do (
            if exist "%%~i\bin\x64\R.exe" (
                set "R=%%~i\bin\x64\R.exe"
                set "RPathFound=true"
            )
        )
    )
)

if not "!RPathFound!"=="true" (
    echo Es wurde kein R gefunden. Bitte den vollstaendigen Pfad zu R.exe angeben.
    set /p "R=Pfad zu R.exe: "
)

if not exist "!R!" (
    echo Fehler: R wurde nicht gefunden: "!R!"
    pause
    exit /b 1
)

rem Der App-Ordner heisst "app"; sonst der einzige Ordner ausser R, chrome und pandoc
set "AppDir="
if exist "%RootDir%app\" set "AppDir=%RootDir%app"
if not defined AppDir (
    for /d %%i in ("%RootDir%*") do (
        if /i not "%%~nxi"=="R" if /i not "%%~nxi"=="chrome" if /i not "%%~nxi"=="pandoc" (
            set "AppDir=%RootDir%%%~nxi"
        )
    )
)

if not defined AppDir (
    echo Fehler: Im Ordner %RootDir% wurde kein App-Ordner gefunden.
    pause
    exit /b 1
)

rem pandoc liegt mit im Setup; rmarkdown findet es ueber RSTUDIO_PANDOC
if exist "%RootDir%pandoc\pandoc.exe" set "RSTUDIO_PANDOC=%RootDir%pandoc"

echo C-Test Auswertung wird gestartet...
echo.

"!R!" --no-save --slave -f "%RootDir%run.R" --args "%AppDir%"

if errorlevel 1 (
    echo.
    echo Die Anwendung wurde mit einem Fehler beendet. Die Meldung steht oben.
    pause
)

exit /b 0
