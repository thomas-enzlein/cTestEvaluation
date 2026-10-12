# Architektur und Konventionen

Für alle, die die App warten oder erweitern. Kurz gehalten; die Anleitung für
Lehrkräfte steht in `helpfiles/anleitung.md`, die fachlichen Hintergründe
(Normwerte, Kategorien, Vergleichswerte) in `analyse/` (nicht im Repository).

## Schichten

| Ebene | Dateien | Aufgabe |
|---|---|---|
| Einstieg | `app.R` | `APP_VERSION`, UTF-8-Option, lädt Oberfläche und Server |
| Bootstrap | `global.R` | Pakete aus `req.txt`, Farben/Stufen (`cols`, `lvls`), lädt `functions/*.R` |
| Oberfläche | `ui.R`, `components/` (header, sidebar, body) | Dashboard, Tabs, Eingabefelder; eigener Look in `style.css` |
| Server | `server.R` | ein Server: `reactiveValues` (`rv`), 13 `reactive`, 20 `observeEvent`, 20 Ausgaben |
| Fachlogik | `functions/utils.R` | technische Helfer: Prüfen, Pfade, Dateien, Zustand, UI-Bausteine |
| | `functions/formatierer.R` | Darstellung: `de_zahl`, `de_vz`, Namen, Kategorie-Farben |
| | `functions/functions.R` | Laden/Prüfen/Vorbereiten, Kategorien, Export, Vorlagen, Sicherung, Briefauswahl, Vergleichswerte |
| | `functions/cohort.R` | Zuordnung 5 → 6 (`build_cohort`, `cohort_gematcht`), Kennzahlen, Tabellen |
| | `functions/infobrief.R` | Abschnitte und Rmd-Bau beider Lehrkräfte-Briefe |
| Ausgabe | `functions/plot_functions.R`, `table2doc_.R`, `table2spreadsheet_.R` | Diagramme, Word- und Excel-Ausgabe |
| Texte | `elternbrief/*.Rmd`, `infobrief/*.Rmd` | Fließtext der Briefe (ohne Code änderbar) |
| Daten | `vorlagen/*.xlsx`, `vorlagen/template.docx` | Referenzwerte, Kategorietexte, Word-Vorlage |
| Werkzeuge | `werkzeuge/vergleich_erzeugen.R` | erzeugt die Referenzdatei; **nicht** im Setup |
| Tests | `tests/run_tests.R`, `tests/testthat/` | 200 Tests mit Fixtures (`fixtures/`) |

## Datenfluss

1. **Laden:** eine tsv-Datei aus der App (`C-Test_Auswertung_<Datum>_<Klasse>.tsv`) → `loadData()` in
   `functions.R`: Spalten ordnen, Zahlen konvertieren (deutsches Komma), Itemzahl ergänzen,
   Kategorie ableiten, Plausibilität prüfen. Jede Auffälligkeit wird als Hinweis gemeldet, aber
   geladen. Ergebnis liegt in `rv$df`; jede Zeile führt in der internen Spalte `Quelle` ihre Herkunft
   (Schlüssel der geladenen Datei oder `"manuell"`), damit sich einzelne Dateien wieder schließen
   lassen (`.quelle_spalte`, `.ohne_quelle()` in `functions.R`).
2. **Ansehen:** Statistik-Tab (Diagramme, `createStatsText` → `.kennzahlen`), Auswertungstabelle,
   Übersichtstabelle (mit Vergleichsspalten, wenn die Referenzdatei vorliegt).
3. **Zuordnen:** `cohort.R` bildet Paare 5 → 6 (gleiche Klasse, nächstes Jahr) und lässt
   Vorschläge im Tab „Zuordnung zum Vorjahr" bestätigen.
4. **Briefe:** Entwicklungsbrief (Kohorten) und Stand-Brief (Klassen) über `functions/infobrief.R`
   + `infobrief/*.Rmd`; Elternbriefe über `compose_letter()` + `elternbrief/elternbrief.Rmd`.
   Gerendert wird mit `officedown`/`pandoc`, zusammengefügt mit `officer`.
5. **Exporte:** Ergebnistabelle als Word-Tabelle (`table2doc_`) und Excel (`table2spreadsheet_`).

**Referenzwerte werden nie in der App gerechnet.** Sie kommen ausschließlich aus
`vorlagen/Vergleichswerte_C-Test.xlsx` (liegt eine persönliche Kopie im Benutzerordner, hat sie
Vorrang). Erzeugt wird die Datei mit `werkzeuge/vergleich_erzeugen.R`. Der Schalter
`vergleich_anzeigen` in `einstellungen.txt` schaltet die Innenansicht ab oder an.

## Ordner zur Laufzeit

| Ort | Inhalt |
|---|---|
| Programmordner | `Auswertungen/` mit den erzeugten Briefen, Tabellen, tsv-Dateien |
| `Dokumente\C-Test Auswertung\` | `einstellungen.txt`, `vorlagen/` (persönliche Vorlagen), Ausweichziel für Ausgaben |
| Option `ctest.outdir` | erzwingt einen anderen Ausgabeordner (`ctest.outdir.fallback` nutzen die Tests) |

Beim ersten Start werden fehlende Vorlagen aus `vorlagen/` in den persönlichen Ordner kopiert –
**vorhandene Dateien werden nie überschrieben**, die Referenzdatei wird bewusst nicht kopiert
(sonst würde sie spätere Updates verdecken).

## Konventionen

- **„Fehlt oder ist leer"** gibt es einmal: `.ist_leer()` in `utils.R` (vektorisiert;
  wer einen einzelnen Wert erwartet, prueft die Laenge davor).
- **Eine Verantwortlichkeit je Datei.** Technisches nach `utils.R`, Darstellung nach
  `formatierer.R`, Fachliches nach `functions.R`. Fließtext gehört in `.Rmd`, nicht in R.
- **Eine Stufe darf auf mehrere Dateien verteilt sein** (5a–5b in einer, 5c–5e in einer anderen):
  die Auswertung arbeitet mit den Daten, nicht mit den Dateien. Doppelt sein darf nur eine **Klasse** –
  `doppelte_klassen()` warnt dann, gesperrt wird nicht (die tsv führt kein Jahr, die App kann „5b 2024"
  und „5b 2025" nicht unterscheiden). Namen werden über `.name_aehnlichkeiten()` (Matrix, vektorisiert)
  verglichen, nicht Paar für Paar in einer R-Schleife – das ist bei ganzen Jahrgängen der Unterschied
  zwischen Minuten und Sekunden.
- **Namen:** neuer Code `snake_case`; ein führender Punkt heißt „intern" (nicht von außen gedacht);
  der Altbestand in `camelCase` (`loadData`, `saveData`, `addEntry`, `getRFlevel`) bleibt so.
- **Feste Normgrenzen** (`.rf_stufen`, `.we_normgrenze`) sind absichtlich konstant – eine Änderung
  in `einstellungen.txt` darf die Einteilung der Kinder nie verschieben. Die *einstellbare* Linie
  ist `rf_norm_unten` (Sprache in den Briefen).
- **Keine globalen Zuweisungen:** Zustand im Server über `reactiveValues`/`reactiveVal`, kein `<<-`.
- **Zahlen und Rundungen an einer Stelle:** `.kennzahlen()` (Mittel/Median/SD), `de_zahl()`/
  `de_vz()` (deutsches Format) – damit Statistik-Tab, Stand-Brief und Entwicklungsbrief dieselben
  Zahlen zeigen.
- **Fehlerbehandlung an den Rändern** (Laden, Schreiben, Einstellungen): `tryCatch` und als
  `list(ok = …, meldung = …)` zurückgeben; Meldungen laufen über die Statuszeile
  (`zeige_meldung()` → `output$text`).
- **Briefe bauen** über die gemeinsamen Helfer in `infobrief.R` (`.brief_vorlage_bereit`,
  `.brief_fragment`, `.brief_setze_d`, `.brief_rmd_bauen`, `.brief_rendern`) – nicht daneben
  eigene Rmd-Läufe bauen.

## Rezepte

- **Neuer Tab:** `menuItem` in `components/sidebar.R`, `tabItem` in `components/body.R`, Ausgaben in
  `server.R`; die neue ID in `tests/testthat/test-ui.R` ergänzen.
- **Neuer Satz / neue Zahl im Brief:** Text in `infobrief/*.Rmd` (bzw. `elternbrief/elternbrief.Rmd`),
  die Daten im jeweiligen Abschnitts-Eintrag (`infobrief_abschnitt`/`stand_abschnitt`) bereitstellen;
  Test in `test-infobrief.R`/`test-standbrief.R` erweitern.
- **Neue Einstellung:** `.einstellungen_default()` ergänzen, Feld in `components/body.R`, Wert in
  `einstellungen_werte()` aufnehmen. Unbekannte und veraltete Schlüssel behandelt die Datei selbst.
- **Neue Vergleichskennzahl:** Spalte in der Referenzdatei + Funktion in `functions.R`
  (`vergleich_*`), Erzeugung in `werkzeuge/vergleich_erzeugen.R` ergänzen.
- **Neuer Export:** Muster von `table2doc_.R`/`table2spreadsheet_.R` übernehmen; Kategorie-Farben
  über `.kat_spalte()`/`.kat_zeilen()` holen.

## Tests und Release

- **Tests:** `Rscript tests/run_tests.R` (optional ein Filter-Regex, z. B.
  `Rscript tests/run_tests.R vergleich`). Die Tests laden `global.R` und arbeiten mit Fixtures.
  Lokal unter Windows: `"C:\Program Files\R\R-4.5.2\bin\x64\Rscript.exe"`, `TMPDIR` auf einen
  Ordner im Projekt setzen, pandoc über `RSTUDIO_PANDOC`.
- **Vor dem Release einmal wirklich starten:** `Rscript tests/smoke_start.R` startet die App in
  einem Kindprozess und bricht ab, wenn sie nicht hochkommt (im Release-Workflow läuft es vor dem
  Setup-Bau). Grund: `testServer()` wertet den Server in einem Reaktiv-Kontext aus. Ein Reaktiv-Wert,
  der *oben in der Server-Funktion* gelesen wird (z. B. `rv$df[[...]] <- ...` beim Start), fällt der
  Testsuite deshalb nicht auf – `shiny` bricht dann aber beim Start ab („Can't access reactive value
  outside of reactive consumer"). Zustand gehört in die `reactiveValues(...)`-Definition, nicht daneben.
- **Release:** `APP_VERSION` in `app.R` erhöhen und in `CHANGELOG.md` einen Abschnitt
  `## <Version>` schreiben. Tag `v<Version>` pushen → GitHub Actions prüft die Testsuite (R-Version
  und CRAN-Snapshot sind im Workflow gepinnt), baut das Setup und legt Release samt Setup an; der
  CHANGELOG-Abschnitt wird der Release-Text. Tag und `APP_VERSION` müssen übereinstimmen.
  Lokal baut `.\build\build_factory.ps1` dasselbe Setup.

## Bekannte Baustellen

- `server.R` ist eine große Funktion ohne Shiny-Module; beim nächsten Feature den betroffenen Tab
  als Modul herauslösen.
- `functions.R` bleibt die größte Datei; nächste Schnitte: `functions/vergleich.R` (Vergleichswerte)
  und `functions/daten.R` (Laden/Prüfen/Vorbereiten).
- Gemischte Namensstile aus dem Altbestand (siehe Konventionen).
- Keine Versionssperre im Repository; abgefedert durch den gepinnten CI-Snapshot und
  `build-info-pakete.csv` aus dem Build.

## Datenschutz

Echte Kinderdaten und Arbeitsdateien gehören **nicht** ins Repository: `analyse/` (interne
Auswertung, Skripte, Plots) und `literatur/` sind per `.gitignore` ausgeschlossen, ebenso
persönliche Vorlagen (`elternbrief/template_1.docx`, `vorlagen/logo.png`) und Nebenprodukte
(`helpfiles/anleitung.html`, `.ctmp/`). Erzeugte Briefe und tsv-Dateien landen im Programm- bzw.
Benutzerordner, nie im Repository.
