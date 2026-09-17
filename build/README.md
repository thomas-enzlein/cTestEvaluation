# Build: Windows-Setup der C-Test-Auswertung

Dieser Ordner enthält alles, was für das Auslieferungs-Setup gebraucht wird. Auf den
Zielrechnern muss danach **nichts** installiert werden: R, Chrome und pandoc liegen mit
im Setup.

| Datei | Zweck |
|---|---|
| `build_factory.ps1` | Baut den Auslieferungsordner, installiert R/Pakete/Chrome/pandoc und ruft Inno Setup auf |
| `chrome_holen.ps1` | Besorgt den mitgelieferten Browser (normale Chrome-Installation, **kein** „Chrome for Testing") |
| `installer.iss` | Inno-Setup-Skript (was ins Setup kommt, Icons, Zielordner, Rechte) |
| `run.bat` | Launcher: sucht R und den App-Ordner, startet `run.R` |
| `run.R` | Setzt Bibliothek, `RSTUDIO_PANDOC`, Arbeitsverzeichnis, Browser und startet Shiny |
| `icon.ico` | Icon der Verknüpfungen und des Setups |

## Bauen

```powershell
# in einer PowerShell-Konsole im Projektordner
.\build\build_factory.ps1 -Version 1.7        # erster Lauf: baut die Laufzeit auf (R, Pakete, Chrome, pandoc)
.\build\build_factory.ps1 -SkipRuntime        # nach Code-Änderungen: nur spiegeln + Setup (Sekunden)
.\build\build_factory.ps1 -ForceChrome        # nur den mitgelieferten Browser austauschen
.\build\build_factory.ps1 -ForceRuntime       # Laufzeit komplett neu aufbauen
.\build\build_factory.ps1 -Release            # zusätzlich GitHub-Release mit dem Setup (braucht gh)

# aus cmd.exe heraus (oder wenn die Ausführungsrichtlinie Skripte blockt)
powershell -ExecutionPolicy Bypass -File build\build_factory.ps1 -Version 1.7
```

Läuft mit Windows PowerShell 5.1 (kein PowerShell 7 nötig).

Voraussetzungen: Windows, [Inno Setup 6](https://jrsoftware.org/isdl.php), Internet
(erster Lauf: R, R-Pakete, Chrome und pandoc), ein installiertes Chrome, PowerShell als
Administrator, wenn R neu installiert werden muss. Die Version kommt aus `APP_VERSION` in
`app.R`, wenn `-Version` nicht angegeben ist.

Ergebnis: `<FactoryDir>\Output\ctest_auswertung_<Version>.exe` (Vorgabe für `FactoryDir`
ist `D:\R\cTest_build` – bewusst außerhalb des Repos, damit Dropbox/Git die großen
Laufzeitdateien nicht mitschleppen).

## Schalter

| Schalter | Wirkung |
|---|---|
| `-FactoryDir <Pfad>` | Auslieferungsordner (enthält `app\`, `R\`, `chrome\`, `pandoc\`, `Output\`) |
| `-Version <x.y>` | Version; leer = `APP_VERSION` aus `app.R` |
| `-RVersion <x.y.z>` | R-Version, die mitgeliefert wird (Vorgabe: 4.5.3) |
| `-Snapshot <yyyy-mm-dd>` | CRAN-Snapshot von Posit PPM (Vorgabe: 2026-08-01). Leer = aktuelles CRAN. Passt der Snapshot nicht, versucht das Skript automatisch das aktuelle CRAN und sagt es |
| `-ChromeQuelle <Pfad>` | Woher das Chrome kopiert wird; leer = automatisch (installiertes Chrome, dann `D:\R\GoogleChromePortable\App`) |
| `-PandocVersion <Version>` | pandoc-Version; leer = neueste von GitHub |
| `-Iscc <Pfad>` | Pfad zu `ISCC.exe`, wenn die automatische Suche nicht greift |
| `-SkipRuntime` | Laufzeit unverändert lassen (nur spiegeln und Setup bauen) |
| `-ForceChrome` | nur den mitgelieferten Browser neu kopieren |
| `-ForceRuntime` | Laufzeit neu aufbauen (R, Pakete, Chrome, pandoc) |
| `-Release` | GitHub-Release `v<Version>` anlegen bzw. das Setup anhängen |

## Was der Build macht

1. **Version** bestimmen (`-Version` oder `APP_VERSION` in `app.R`).
2. **Laufzeit** aufbauen, falls nötig:
   - R 4.5.3 mit dem offiziellen Installer still installieren (`/VERYSILENT /DIR=…`) –
     eine kopierte Installation ist portabel, R findet sein Zuhause über den eigenen Pfad.
   - Pakete aus `req.txt` in `R\library` installieren (Windows-Binärpakete, gepinnter Snapshot).
     Geprüft wird dabei **gegen die Auslieferungsbibliothek**, nicht gegen den ganzen Suchpfad:
     sonst meldet der Schritt „nichts zu tun", wenn auf dem Rechner schon eine gefüllte
     Benutzerbibliothek liegt, und das Setup enthielte kein einziges App-Paket.
   - eine **normale Chrome-Installation** nach `chrome\` kopieren (`chrome_holen.ps1`).
     Ausdrücklich **kein „Chrome for Testing"**: dieser Build zeigt beim Start einen
     Test-Hinweis, der sich nicht abschalten lässt.
   - pandoc (Windows-Zip) nach `pandoc\` entpacken.
   - **Endkontrolle:** Der Build prüft unabhängig davon, ob alle Pakete aus `req.txt` in
     `R\library` liegen und sich mit **ausgeblendeter Benutzerbibliothek** laden lassen
     (so wie auf einem fremden Rechner). Fehlt etwas, bricht er ab – ein Setup ohne
     App-Pakete kann so nicht mehr entstehen.
3. **Quellcode spiegeln** nach `app\` – ohne `.git`, `.github`, `tests`, `build`,
   `Auswertungen`, `req_dev.txt`, Word-Sperrdateien.
4. **Build-Informationen** schreiben: `build-info.txt` (Version, Datum, Commit, R, Quelle,
   Chrome samt Herkunft, pandoc) und `build-info-pakete.csv` (Paketversionen). Damit ist
   nachvollziehbar, womit ein Setup gebaut wurde – beide Dateien werden mit ausgeliefert
   (nur die txt).
5. **Inno Setup** aufrufen und das Setup nach `Output\` schreiben.

## Was im Setup landet

```
<Installationsordner>            Vorgabe: C:\ProgramData\C-Test Auswertung
  run.bat, run.R, icon.ico       Launcher
  README.md, LICENSE.md, build-info.txt
  app\                           Quellcode der App (inkl. elternbrief\, infobrief\, helpfiles\)
  R\                             portables R 4.5.3 mit allen Paketen
  chrome\                        normales Chrome (Fenster ohne Adressleiste via --app)
  pandoc\                        pandoc für die Word-Briefe
```

Rechte: Der Installationsordner wird auf `users-full` gesetzt, damit die App dort
`app\Auswertungen` anlegen kann. Klappt das trotzdem nicht (z. B. fremder Rechner, andere
Rechte), weicht die App auf `C-Test Auswertung` in den eigenen Dokumenten aus und nennt
den Ordner in der Meldung.

## Automatischer Build (GitHub Actions)

`.github/workflows/release.yml` ruft genau dieses Skript auf – lokal und in der CI läuft
also derselbe Bau.

| Auslöser | Ergebnis |
|---|---|
| Tag `v*` pushen | Testsuite läuft, Setup wird gebaut, **Release** wird angelegt und das Setup angehängt |
| Actions → „Windows-Setup" → Run workflow | Testsuite läuft, Setup wird gebaut, Setup liegt als **Artefakt** bereit (kein Release) – zum Testen des Ablaufs |

Der Workflow prüft vor dem Bauen, dass der Tag zur Version passt: bei Tag `v1.7` muss in
`app.R` `APP_VERSION <- "1.7"` stehen, sonst bricht er ab. Die R-Paketbibliothek wird über
`actions/cache` gecacht (Schlüssel aus R-Version, CRAN-Snapshot und `req.txt`) – der erste
Lauf dauert daher deutlich länger als die folgenden.

Der **Release-Text** wird automatisch gebaut: zuerst der Abschnitt der Version aus
[CHANGELOG.md](../CHANGELOG.md), darunter in einem aufklappbaren Block die Commits seit dem
letzten Tag und die Vergleichs-URL. Vor einem Release also in `CHANGELOG.md` einen
Abschnitt `## <Version>` anlegen – fehlt er, wird nur gewarnt und die Änderungsliste
verwendet (der Build läuft weiter). Die GitHub-Automatik `--generate-notes` wird bewusst
nicht genutzt, weil sie nur gemergte Pull Requests auflistet.

Damit der Ablauf reproduzierbar bleibt, sind R-Version (`4.5.3`) und CRAN-Snapshot
(`2026-08-01`) im Workflow fest eingetragen; pandoc kommt jeweils als aktuelles Release
dazu. Der Auslieferungsordner liegt in der CI **außerhalb** des Repos (abgeleitet aus
`github.workspace`, ohne `..` im Pfad – `upload-artifact` und `cache` verbieten relative
Pfadangaben), sonst würde `robocopy /MIR` das Repository in sich selbst spiegeln; das
Skript lehnt einen solchen Pfad zusätzlich ausdrücklich ab.

## Erster Test nach dem Bauen

Am besten auf einem Windows **ohne** installiertes R: Setup ausführen, App starten, einen
Schüler eintragen, speichern, einen Elternbrief erzeugen (prüft pandoc) und den Infobrief
mit zwei geladenen Jahrgängen erzeugen (prüft die zweite Stufe). Wenn das läuft, ist das
Setup in sich geschlossen.

Den Paketbund kann man vorher in einer Zeile prüfen – `True` heißt, die App-Pakete liegen
im Installationsordner und nicht nur in einer Benutzerbibliothek:

```powershell
Test-Path "C:\ProgramData\C-Test Auswertung\R\library\shiny"
```

Beim Start zeigt das Konsolenfenster, welche Bausteine benutzt werden – dort muss das
**mitgelieferte** R stehen (Pfad im Installationsordner), nicht ein installiertes:

```
App-Ordner: C:\ProgramData\C-Test Auswertung\app
R: R version 4.5.3 (…) (C:/ProgramData/C-Test Auswertung/R)
pandoc: C:\ProgramData\C-Test Auswertung\pandoc\pandoc.exe
Browser: C:\ProgramData\C-Test Auswertung\chrome\chrome.exe
Profil:  C:/Users/<Name>/AppData/Local/C-Test Auswertung/chrome_profile
```
