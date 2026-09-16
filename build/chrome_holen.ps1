# Holt das Chrome, das mit ausgeliefert wird.
#
# Bewusst KEIN "Chrome for Testing": dieser Build zeigt beim Start den Hinweis
# "Chrome for Testing ist nur fuer automatisierte Tests vorgesehen". Der Hinweis
# steckt fest in den Sprachdateien des Builds und laesst sich nicht abschalten -
# fuer eine Anwender-App ist das unbrauchbar. Mitgeliefert wird deshalb eine
# normale Chrome-Installation (wie bei den bisherigen Versionen ueber
# GoogleChromePortable), die diesen Hinweis gar nicht enthaelt.
#
# Quelle, wenn -Quelle nicht angegeben ist:
#   1. installiertes Chrome: Program Files, Program Files (x86), LocalAppData
#   2. Chrome-bin-Ablage von PortableApps (siehe -PortableZusatz)
#
# Als -Quelle akzeptiert: der "Application"-Ordner von Chrome, ein Ordner mit
# chrome.exe, oder ein Ordner mit "Chrome-bin" (dann die hoechste Version).

function Test-ChromeOrdner {
    param([string]$Pfad)
    if (!$Pfad) { return $false }
    return (Test-Path (Join-Path $Pfad "chrome.exe"))
}

function Get-ChromeVersion {
    param([string]$Ziel)
    $exe = Join-Path $Ziel "chrome.exe"
    if (Test-Path $exe) {
        $v = (Get-Item $exe).VersionInfo.ProductVersion
        if (!$v) { $v = (Get-Item $exe).VersionInfo.FileVersion }
        if ($v) { return $v }
    }
    $unter = Get-ChildItem $Ziel -Directory -ErrorAction SilentlyContinue |
        Where-Object { $_.Name -match '^\d+\.\d+\.\d+\.\d+$' } |
        Sort-Object { [version]$_.Name } -Descending | Select-Object -First 1
    if ($unter) { return $unter.Name }
    return "unbekannt"
}

function Finde-ChromeQuelle {
    param([string]$Quelle = "", [string]$PortableZusatz = "")

    # Chrome-bin-Ablage (PortableApps): hoechste Version mit chrome.exe
    $ausBin = {
        param([string]$Pfad)
        # PortableApps legt das Chrome unter "App\Chrome-bin\<Version>" ab -
        # beide Schreibweisen werden versucht
        $moeglich = @((Join-Path $Pfad "Chrome-bin"), (Join-Path $Pfad "App\Chrome-bin"))
        foreach ($bin in $moeglich) {
            if (!(Test-Path $bin)) { continue }
            $v = Get-ChildItem $bin -Directory -ErrorAction SilentlyContinue |
                Where-Object { Test-ChromeOrdner $_.FullName } |
                Sort-Object { [version]$_.Name } -Descending | Select-Object -First 1
            if ($v) { return $v.FullName }
        }
        return $null
    }

    if ($Quelle) {
        if (Test-ChromeOrdner $Quelle) { return $Quelle }
        $ausBinTreffer = & $ausBin $Quelle
        if ($ausBinTreffer) { return $ausBinTreffer }
        throw "In '$Quelle' wurde kein Chrome gefunden (erwartet wird ein Ordner mit chrome.exe)."
    }

    $kandidaten = @()
    if ($env:ProgramFiles) { $kandidaten += (Join-Path $env:ProgramFiles "Google\Chrome\Application") }
    if (${env:ProgramFiles(x86)}) { $kandidaten += (Join-Path ${env:ProgramFiles(x86)} "Google\Chrome\Application") }
    if ($env:LOCALAPPDATA) { $kandidaten += (Join-Path $env:LOCALAPPDATA "Google\Chrome\Application") }
    if ($PortableZusatz) { $kandidaten += $PortableZusatz }

    foreach ($k in $kandidaten) {
        if (Test-ChromeOrdner $k) { return $k }
        $ausBinTreffer = & $ausBin $k
        if ($ausBinTreffer) { return $ausBinTreffer }
    }

    throw ("Es wurde kein Chrome gefunden. Bitte Chrome installieren oder den Pfad " +
           "mit -ChromeQuelle angeben (z. B. 'C:\Program Files (x86)\Google\Chrome\Application').")
}

function Installiere-Chrome {
    param(
        [string]$Ziel,
        [string]$Quelle = "",
        [string]$PortableZusatz = "D:\R\GoogleChromePortable\App",
        [switch]$Erzwingen
    )

    $exe = Join-Path $Ziel "chrome.exe"
    if ((Test-Path $exe) -and !$Erzwingen) {
        return [pscustomobject]@{
            Version = (Get-ChromeVersion -Ziel $Ziel)
            Quelle  = "(bereits im Auslieferungsordner)"
            Kopiert = $false
        }
    }

    $src = Finde-ChromeQuelle -Quelle $Quelle -PortableZusatz $PortableZusatz
    if (Test-Path $Ziel) { Remove-Item -Recurse -Force $Ziel }
    New-Item -ItemType Directory -Force -Path $Ziel | Out-Null

    # /E kopiert Unterordner mit (die Versionsmappe gehoert dazu!), SetupMetrics
    # ist nur Installer-Ballast
    robocopy $src $Ziel /E /XD "SetupMetrics" /NFL /NDL /NJH /NJS /NP | Out-Null
    if ($LASTEXITCODE -ge 8) { throw "Chrome konnte nicht kopiert werden (robocopy $LASTEXITCODE)." }
    if (!(Test-Path $exe)) { throw "Nach dem Kopieren fehlt chrome.exe im Zielordner (Quelle: $src)." }

    return [pscustomobject]@{
        Version = (Get-ChromeVersion -Ziel $Ziel)
        Quelle  = $src
        Kopiert = $true
    }
}
