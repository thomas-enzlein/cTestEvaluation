<#
.SYNOPSIS
    Baut den Auslieferungsordner und das Windows-Setup der C-Test-Auswertung.

.DESCRIPTION
    Ablauf:
      1. Version bestimmen (Parameter -Version oder APP_VERSION aus app.R)
      2. Laufzeit aufbauen (nur wenn noetig): portables R, R-Pakete, Chrome, pandoc
      3. Quellcode in den Auslieferungsordner spiegeln (ohne .git, tests, Auswertungen)
      4. Inno Setup aufrufen  ->  <FactoryDir>\Output\ctest_auswertung_<Version>.exe
      5. optional (-Release): GitHub-Release mit dem Setup als Anhang anlegen

    Die Laufzeit (R + Pakete + Chrome + pandoc) wird nur einmal aufgebaut. Mit
    -SkipRuntime werden nur gespiegelt und der Installer neu erzeugt - das dauert
    Sekunden statt Minuten und ist der Normalfall fuer Code-Aenderungen. Mit
    -ForceChrome wird nur der mitgelieferte Browser getauscht.

    Als Browser wird eine normale Chrome-Installation mitgeliefert (Details in
    chrome_holen.ps1) - ausdruecklich NICHT "Chrome for Testing", das beim Start
    einen nicht abschaltbaren Test-Hinweis anzeigt.

    Voraussetzungen:
      - Inno Setup 6 (ISCC.exe)
      - Internet fuer den ersten Lauf (R, Pakete, pandoc)
      - ein installiertes Chrome oder -ChromeQuelle auf eine Chrome-Ablage
      - PowerShell als Administrator, wenn R neu installiert werden muss
      - fuer -Release: die GitHub-CLI (gh) im PATH

.EXAMPLE
    .\build\build_factory.ps1 -Version 1.6

.EXAMPLE
    .\build\build_factory.ps1 -SkipRuntime             # nur Installer neu bauen

.EXAMPLE
    .\build\build_factory.ps1 -ForceChrome             # nur den Browser tauschen

.EXAMPLE
    .\build\build_factory.ps1 -ForceRuntime -Release    # Laufzeit neu + Release
#>
[CmdletBinding()]
param(
    # Ordner, in dem der Auslieferungsordner aufgebaut wird (nicht im Repo!)
    [string]$FactoryDir = "D:\R\cTest_build",
    # Version; leer = aus APP_VERSION in app.R
    [string]$Version = "",
    # R-Version, die mitgeliefert wird
    [string]$RVersion = "4.5.3",
    # CRAN-Snapshot (Posit Public Package Manager), z. B. 2026-08-01.
    # Leer = aktuelles CRAN (nicht reproduzierbar).
    [string]$Snapshot = "2026-08-01",
    # Chrome-Quelle; leer = automatisch suchen (installiertes Chrome, dann
    # PortableApps unter D:\R\GoogleChromePortable\App)
    [string]$ChromeQuelle = "",
    # pandoc-Version; leer = neueste Version von GitHub
    [string]$PandocVersion = "",
    # Pfad zu ISCC.exe; leer = automatisch suchen
    [string]$Iscc = "",
    # Laufzeit nicht anfassen (nur spiegeln und Installer bauen)
    [switch]$SkipRuntime,
    # Laufzeit komplett neu aufbauen (R, Pakete, Chrome, pandoc)
    [switch]$ForceRuntime,
    # nur den mitgelieferten Browser neu holen
    [switch]$ForceChrome,
    # GitHub-Release anlegen und das Setup hochladen
    [switch]$Release
)

$ErrorActionPreference = "Stop"
$ProgressPreference = "SilentlyContinue"
# Windows PowerShell 5.1 verhandelt sonst TLS 1.0 - GitHub und Google lehnen das ab
[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12

function Schritt { param([string]$Text) Write-Host "`n=== $Text ===" -ForegroundColor Cyan }
function Info { param([string]$Text) Write-Host "    $Text" -ForegroundColor Gray }
function Ok { param([string]$Text) Write-Host "    OK: $Text" -ForegroundColor Green }
function Warnung { param([string]$Text) Write-Host "    Achtung: $Text" -ForegroundColor Yellow }
function Abbruch { param([string]$Text) throw $Text }
function Ohne-Zitat { param([string]$Pfad) $Pfad -replace "\\", "/" }

# Textdatei ohne BOM schreiben. Wichtig: Set-Content -Encoding UTF8 haengt in
# Windows PowerShell 5.1 ein BOM an den Anfang, und R bricht daran ab
# ("Fehler: unerwartete Eingabe").
function Set-TextOhneBom {
    param([string]$Pfad, [string]$Text)
    [System.IO.File]::WriteAllText($Pfad, $Text, (New-Object System.Text.UTF8Encoding($false)))
}

# Browser besorgen (Finde-ChromeQuelle, Installiere-Chrome, Test-ChromeOrdner,
# Get-ChromeVersion) - eigene Datei, damit der Schritt einzeln testbar ist
. (Join-Path $PSScriptRoot "chrome_holen.ps1")

function Get-Datei {
    param([string]$Url, [string]$Ziel)
    Info "lade $Url"
    try {
        $wc = New-Object System.Net.WebClient
        $wc.Headers.Add("User-Agent", "ctest-build")
        $wc.DownloadFile($Url, $Ziel)
    } catch {
        Invoke-WebRequest -Uri $Url -OutFile $Ziel
    }
    if (!(Test-Path $Ziel)) { Abbruch "Download fehlgeschlagen: $Url" }
}

function Get-WebJson {
    param([string]$Url)
    return Invoke-RestMethod -Uri $Url -Headers @{ "User-Agent" = "ctest-build" }
}

function Finde-Iscc {
    if ($Iscc -ne "" -and (Test-Path $Iscc)) { return $Iscc }
    $kandidaten = @(
        "C:\Program Files (x86)\Inno Setup 6\ISCC.exe",
        "C:\Program Files\Inno Setup 6\ISCC.exe",
        (Join-Path $env:LOCALAPPDATA "Programs\Inno Setup 6\ISCC.exe")
    )
    foreach ($k in $kandidaten) { if (Test-Path $k) { return $k } }
    $cmd = Get-Command "ISCC.exe" -ErrorAction SilentlyContinue
    if ($cmd) { return $cmd.Source }
    return $null
}

# ----------------------------------------------------------------------------
# 0. Vorbereitung
# ----------------------------------------------------------------------------
$RepoRoot = (Resolve-Path (Join-Path $PSScriptRoot "..")).Path

# Pfad normalisieren (loest ".." auf), damit der Vergleich unten und alle
# Dateioperationen mit einer eindeutigen Form arbeiten
if (![System.IO.Path]::IsPathRooted($FactoryDir)) {
    $FactoryDir = Join-Path (Get-Location).Path $FactoryDir
}
$FactoryDir = [System.IO.Path]::GetFullPath($FactoryDir).TrimEnd([char]'\')

$AppQuelle = Join-Path $FactoryDir "app"
$RZiel = Join-Path $FactoryDir "R"
$Rscript = Join-Path $RZiel "bin\x64\Rscript.exe"
$RLib = Join-Path $RZiel "library"
$AusgabeOrdner = Join-Path $FactoryDir "Output"
$TempOrdner = Join-Path $FactoryDir "_tmp"

Write-Host "C-Test Auswertung - Build" -ForegroundColor Cyan
Info "Repository:          $RepoRoot"
Info "Auslieferungsordner: $FactoryDir"

# Schutz gegen eine boese Falle: liegt der Auslieferungsordner IM Repository,
# spiegelt robocopy /MIR das Repository in einen Unterordner von sich selbst
# und laeuft sich dabei voll.
$repoPraefix = $RepoRoot.TrimEnd([char]'\') + '\'
if ($FactoryDir.StartsWith($repoPraefix, [System.StringComparison]::OrdinalIgnoreCase)) {
    Abbruch ("Der Auslieferungsordner darf nicht im Repository liegen (robocopy wuerde " +
             "in sich selbst spiegeln). Bitte -FactoryDir ausserhalb waehlen, z. B. " +
             "'D:\R\cTest_build'.")
}

if (!(Test-Path $FactoryDir)) { New-Item -ItemType Directory -Path $FactoryDir | Out-Null }

# Version aus app.R, falls nicht uebergeben
if ($Version -eq "") {
    $appR = Get-Content (Join-Path $RepoRoot "app.R") -Raw
    if ($appR -match 'APP_VERSION\s*<-\s*"([^"]+)"') {
        $Version = $Matches[1]
    } else {
        Abbruch "Keine Version gefunden: APP_VERSION in app.R setzen oder -Version uebergeben."
    }
}
Info "Version:             $Version"

$Commit = ""
try {
    Push-Location $RepoRoot
    $Commit = (& git rev-parse --short HEAD 2>$null)
    Pop-Location
} catch { if ((Get-Location).Path -ne $RepoRoot) { Pop-Location } }
if ($Commit) { Info "Git-Commit:          $Commit" }

$ChromeGebaut = ""
$ChromeQuelleVerwendet = ""
$PandocGebaut = ""
$RepoVerwendet = ""

# ----------------------------------------------------------------------------
# 1. Laufzeit: portables R, Pakete, Chrome, pandoc
# ----------------------------------------------------------------------------
if (!$SkipRuntime) {

    Schritt "Portables R $RVersion"
    if ($ForceRuntime -and (Test-Path $RZiel)) {
        Info "vorhandenes R wird entfernt"
        Remove-Item -Recurse -Force $RZiel
    }

    if (Test-Path $Rscript) {
        $istVersion = (& $Rscript -e "cat(as.character(getRversion()))")
        Ok "R ist vorhanden: $istVersion"
        if ($istVersion -ne $RVersion) {
            Warnung "gewuenscht ist $RVersion - mit -ForceRuntime neu aufbauen"
        }
    } else {
        $installer = Join-Path $TempOrdner "R-$RVersion-win.exe"
        if (!(Test-Path $TempOrdner)) { New-Item -ItemType Directory -Path $TempOrdner | Out-Null }

        $quellen = @(
            "https://cloud.r-project.org/bin/windows/base/R-$RVersion-win.exe",
            "https://cloud.r-project.org/bin/windows/base/old/$RVersion/R-$RVersion-win.exe"
        )
        $geladen = $false
        foreach ($q in $quellen) {
            try { Get-Datei -Url $q -Ziel $installer; $geladen = $true; break } catch { }
        }
        if (!$geladen) { Abbruch "R $RVersion konnte nicht geladen werden. Gibt es diese Version?" }

        Info "installiere R still nach $RZiel (ggf. Adminrechte noetig)"
        $argumente = @("/VERYSILENT", "/SUPPRESSMSGBOXES", "/NORESTART", "/SP-",
                       ('/DIR="' + $RZiel + '"'))
        $p = Start-Process -FilePath $installer -ArgumentList $argumente -Wait -PassThru
        Remove-Item -Force $installer -ErrorAction SilentlyContinue
        if (!(Test-Path $Rscript)) { Abbruch "R-Installation fehlgeschlagen (Exitcode $($p.ExitCode))." }
        # Deinstallations-Reste entfernen - die gehoeren nicht in das Setup
        Get-ChildItem $RZiel -Filter "unins000.*" -ErrorAction SilentlyContinue |
            Remove-Item -Force -ErrorAction SilentlyContinue
        Ok "R installiert: $(& $Rscript -e 'cat(as.character(getRversion()))')"
    }

    Schritt "R-Pakete aus req.txt"
    $pakete = Get-Content (Join-Path $RepoRoot "req.txt") |
        Where-Object { $_.Trim() -ne "" } | ForEach-Object { $_.Trim() }
    Info "$($pakete.Count) Pakete in req.txt"

    $paketDatei = Join-Path $TempOrdner "_req.txt"
    $rDatei = Join-Path $TempOrdner "_pakete.R"
    if (!(Test-Path $TempOrdner)) { New-Item -ItemType Directory -Path $TempOrdner | Out-Null }
    $pakete | Set-Content -Path $paketDatei -Encoding ASCII

    function Installiere-Pakete {
        param([string]$Repo)
        $code = @"
options(Ncpus = max(1L, parallel::detectCores() - 1L))
pkgs <- readLines("$(Ohne-Zitat $paketDatei)", warn = FALSE)
pkgs <- trimws(pkgs[nzchar(trimws(pkgs))])
lib  <- "$(Ohne-Zitat $RLib)"
cat("Repository: ", "$Repo", "\n", sep = "")
fehlend <- pkgs[!(pkgs %in% rownames(installed.packages()))]
cat("zu installieren: ", if (length(fehlend) == 0) "nichts" else paste(fehlend, collapse = ", "),
    "\n", sep = "")
if (length(fehlend) > 0) {
  install.packages(fehlend, lib = lib, repos = "$Repo")
}
noch <- pkgs[!(pkgs %in% rownames(installed.packages()))]
if (length(noch) > 0) {
  cat("NOCH-FEHLEND: ", paste(noch, collapse = ", "), "\n", sep = "")
  quit(status = 1)
}
cat("alle Pakete vorhanden\n")
"@
        Set-TextOhneBom -Pfad $rDatei -Text $code
        # Ausgabe direkt auf die Konsole: die Funktion liefert nur das Ergebnis
        & $Rscript $rDatei | Out-Host
        return ($LASTEXITCODE -eq 0)
    }

    if ($Snapshot -ne "") {
        $RepoVerwendet = "https://packagemanager.posit.co/cran/$Snapshot"
        $paketeOk = Installiere-Pakete -Repo $RepoVerwendet
        if (!$paketeOk) {
            Warnung "Snapshot $Snapshot hat nicht gereicht - neuer Versuch mit dem aktuellen CRAN"
            $RepoVerwendet = "https://cloud.r-project.org"
            $paketeOk = Installiere-Pakete -Repo $RepoVerwendet
        }
    } else {
        $RepoVerwendet = "https://cloud.r-project.org"
        $paketeOk = Installiere-Pakete -Repo $RepoVerwendet
    }
    if (!$paketeOk) { Abbruch "Es fehlen Pakete in der Bibliothek - siehe Ausgabe oben." }
    Ok "Pakete vollstaendig ($RepoVerwendet)"

    Schritt "Chrome (normale Installation)"
    $chromeDir = Join-Path $FactoryDir "chrome"
    try {
        $chrome = Installiere-Chrome -Ziel $chromeDir -Quelle $ChromeQuelle `
                                     -Erzwingen:($ForceChrome -or $ForceRuntime)
    } catch {
        Abbruch $_.Exception.Message
    }
    $ChromeGebaut = $chrome.Version
    $ChromeQuelleVerwendet = $chrome.Quelle
    if ($chrome.Kopiert) { Ok "Chrome $($chrome.Version) kopiert aus $($chrome.Quelle)" }
    else { Ok "Chrome ist vorhanden: $($chrome.Version)" }

    Schritt "pandoc"
    $pandocDir = Join-Path $FactoryDir "pandoc"
    if ($ForceRuntime -and (Test-Path $pandocDir)) { Remove-Item -Recurse -Force $pandocDir }
    if ((Test-Path (Join-Path $pandocDir "pandoc.exe")) -and !$ForceRuntime) {
        $PandocGebaut = (& (Join-Path $pandocDir "pandoc.exe") --version | Select-Object -First 1)
        Ok "pandoc ist vorhanden: $PandocGebaut"
    } else {
        if ($PandocVersion -eq "") {
            $rel = Get-WebJson "https://api.github.com/repos/jgm/pandoc/releases/latest"
            $PandocVersion = $rel.tag_name
        }
        $PandocGebaut = $PandocVersion
        $url = "https://github.com/jgm/pandoc/releases/download/$PandocVersion/pandoc-$PandocVersion-windows-x86_64.zip"
        $zip = Join-Path $TempOrdner "pandoc.zip"
        $entpackt = Join-Path $TempOrdner "pandoc"
        Get-Datei -Url $url -Ziel $zip
        if (Test-Path $entpackt) { Remove-Item -Recurse -Force $entpackt }
        Expand-Archive -Path $zip -DestinationPath $entpackt -Force
        if (Test-Path $pandocDir) { Remove-Item -Recurse -Force $pandocDir }
        New-Item -ItemType Directory -Path $pandocDir | Out-Null
        $inner = Get-ChildItem $entpackt -Directory | Select-Object -First 1
        Get-ChildItem $inner.FullName | ForEach-Object { Move-Item $_.FullName $pandocDir }
        Remove-Item -Recurse -Force $entpackt, $zip
        Ok "pandoc $PandocGebaut entpackt"
    }
} else {
    Info "Laufzeit bleibt unveraendert (-SkipRuntime)"
    if (Test-Path (Join-Path $FactoryDir "chrome\chrome.exe")) {
        $ChromeGebaut = Get-ChromeVersion -Ziel (Join-Path $FactoryDir "chrome")
    }
    if (Test-Path (Join-Path $FactoryDir "pandoc\pandoc.exe")) {
        $PandocGebaut = (& (Join-Path $FactoryDir "pandoc\pandoc.exe") --version | Select-Object -First 1)
    }
    if (Test-Path $Rscript) { Ok "R im Auslieferungsordner: $(& $Rscript -e 'cat(as.character(getRversion()))')" }
}

# ----------------------------------------------------------------------------
# 2. Quellcode spiegeln
# ----------------------------------------------------------------------------
Schritt "Quellcode nach app\ spiegeln"
Info "$RepoRoot  ->  $AppQuelle"
robocopy $RepoRoot $AppQuelle /MIR `
    /XD ".git" ".github" ".Rproj.user" "tests" "build" "Auswertungen" "_factory" ".rtmp" `
    /XF ".RData" ".Rhistory" "req_dev.txt" "*.log" ".gitignore" ".Rbuildignore" `
        "template_1.docx" "~`$*" | Out-Null
if ($LASTEXITCODE -ge 8) { Abbruch "robocopy meldete Fehler (Exitcode $LASTEXITCODE)." }
$global:LASTEXITCODE = 0

foreach ($f in @("run.bat", "run.R", "icon.ico")) {
    Copy-Item (Join-Path $PSScriptRoot $f) $FactoryDir -Force
}
Copy-Item (Join-Path $RepoRoot "README.md") $FactoryDir -Force
Copy-Item (Join-Path $RepoRoot "LICENSE.md") $FactoryDir -Force

foreach ($muss in @("app\app.R", "app\ui.R", "app\server.R", "app\global.R",
                    "app\functions\functions.R", "app\infobrief\abschluss.Rmd")) {
    if (!(Test-Path (Join-Path $FactoryDir $muss))) { Abbruch "Im Auslieferungsordner fehlt: $muss" }
}
if (Test-Path (Join-Path $AppQuelle "Auswertungen")) {
    Warnung "app\Auswertungen existiert - dort liegen Ergebnisdateien, die nicht ins Setup gehoeren"
}
Ok "Quellcode gespiegelt"

# ----------------------------------------------------------------------------
# 3. Build-Informationen
# ----------------------------------------------------------------------------
Schritt "Build-Informationen"
$paketListe = Join-Path $FactoryDir "build-info-pakete.csv"
if (Test-Path $Rscript) {
    $code = @"
pkgs <- trimws(readLines("$(Ohne-Zitat (Join-Path $RepoRoot 'req.txt'))", warn = FALSE))
ip <- installed.packages()[, c("Package", "Version")]
ip <- ip[ip[, "Package"] %in% pkgs, , drop = FALSE]
ip <- ip[order(ip[, "Package"]), , drop = FALSE]
write.csv(ip, "$(Ohne-Zitat $paketListe)", row.names = FALSE, quote = FALSE)
cat(nrow(ip), "Pakete notiert\n")
"@
    $rDatei = Join-Path $TempOrdner "_info.R"
    if (!(Test-Path $TempOrdner)) { New-Item -ItemType Directory -Path $TempOrdner | Out-Null }
    Set-TextOhneBom -Pfad $rDatei -Text $code
    & $Rscript $rDatei | Out-Host
}

$info = @()
$info += "C-Test Auswertung - Build-Informationen"
$info += "Version:        $Version"
$info += "Gebaut am:      $(Get-Date -Format 'yyyy-MM-dd HH:mm')"
if ($Commit) { $info += "Git-Commit:     $Commit" }
if (Test-Path $Rscript) { $info += "R:              $(& $Rscript -e 'cat(as.character(getRversion()))')" }
if ($RepoVerwendet) { $info += "CRAN-Quelle:    $RepoVerwendet" }
if ($ChromeGebaut) { $info += "Chrome:         $ChromeGebaut" }
if ($ChromeQuelleVerwendet) { $info += "Chrome-Quelle:  $ChromeQuelleVerwendet" }
if ($PandocGebaut) { $info += "pandoc:         $PandocGebaut" }
$info += "Paketversionen: build-info-pakete.csv"
Set-TextOhneBom -Pfad (Join-Path $FactoryDir "build-info.txt") -Text ($info -join "`r`n")
$info | ForEach-Object { Info $_ }

# ----------------------------------------------------------------------------
# 4. Installer bauen
# ----------------------------------------------------------------------------
Schritt "Installer bauen"
$isccExe = Finde-Iscc
if (!$isccExe) { Abbruch "ISCC.exe (Inno Setup 6) wurde nicht gefunden - bitte -Iscc <Pfad> angeben." }
Info "ISCC: $isccExe"

if (!(Test-Path $AusgabeOrdner)) { New-Item -ItemType Directory -Path $AusgabeOrdner | Out-Null }

$isccArgumente = @(
    "/DMyAppVersion=$Version",
    ('/DSrcDir="' + $FactoryDir + '"'),
    ('/O"' + $AusgabeOrdner + '"'),
    (Join-Path $PSScriptRoot "installer.iss")
)
& $isccExe @isccArgumente
if ($LASTEXITCODE -ne 0) { Abbruch "Inno Setup brach mit Exitcode $LASTEXITCODE ab." }

$setup = Join-Path $AusgabeOrdner "ctest_auswertung_$Version.exe"
if (!(Test-Path $setup)) { Abbruch "Das Setup wurde nicht erzeugt: $setup" }
$groesse = [math]::Round((Get-Item $setup).Length / 1MB, 1)
Ok "Setup gebaut: $setup ($groesse MB)"

if (Test-Path $TempOrdner) { Remove-Item -Recurse -Force $TempOrdner }

# ----------------------------------------------------------------------------
# 5. optional: GitHub-Release
# ----------------------------------------------------------------------------
if ($Release) {
    Schritt "GitHub-Release v$Version"
    if (!(Get-Command gh -ErrorAction SilentlyContinue)) {
        Abbruch "gh (GitHub-CLI) ist nicht im PATH - Setup liegt bereit unter $setup"
    }
    Push-Location $RepoRoot
    try {
        # cmd /c, damit die Fehlermeldung von gh nicht als PowerShell-Fehler gilt
        cmd /c "gh release view `"v$Version`" >nul 2>&1"
        if ($LASTEXITCODE -eq 0) {
            Info "Release v$Version existiert - Setup wird angehaengt"
            gh release upload "v$Version" $setup --clobber
        } else {
            gh release create "v$Version" $setup --title "Release $Version" --generate-notes
        }
        if ($LASTEXITCODE -ne 0) { Warnung "gh meldete einen Fehler - Setup liegt unter $setup" }
        else { Ok "Release v$Version enthaelt $([System.IO.Path]::GetFileName($setup))" }
    } finally {
        Pop-Location
    }
}

Write-Host "`nFertig." -ForegroundColor Cyan
Write-Host "Setup: $setup" -ForegroundColor Green
