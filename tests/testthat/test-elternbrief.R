# Elternbrief-Pipeline: rendert echte docx-Dateien (langsamster Test).
# Laeuft in einem temporaeren Ordner mit einer Kopie von elternbrief/.

brief_test_umgebung <- function(code) {
  withr::with_tempdir({
    # Inhalt von elternbrief/ kopieren (ohne file.copy-Warnung)
    dir.create("elternbrief")
    dateien <- list.files(file.path(projekt_root, "elternbrief"), full.names = TRUE)
    file.copy(dateien, "elternbrief", recursive = TRUE)
    dir.create("Auswertungen")
    force(code)
  })
}

# Briefe erzeugen; die Ausgabe von knitr/pandoc wird dabei unterdrueckt.
# Rueckgabe: Liste aus create_letters() (datei, erstellt, fehler)
erzeuge_briefe <- function(df, lehrername, signatur = "", qrLink = NULL) {
  ergebnis <- NULL
  utils::capture.output(
    suppressMessages(
      ergebnis <- create_letters(df, lehrername = lehrername,
                                 signatur = signatur, qrLink = qrLink)
    )
  )
  ergebnis
}

# compose_letter voruebergehend ersetzen (Testdouble) und danach zurueckstellen.
# bau_ersatz bekommt die echte Funktion und liefert den Ersatz.
mit_compose_letter <- function(bau_ersatz, code) {
  echt <- compose_letter
  assign("compose_letter", bau_ersatz(echt), envir = globalenv())
  on.exit(assign("compose_letter", echt, envir = globalenv()), add = TRUE)
  force(code)
}

brief_text <- function(datei) {
  paste(officer::docx_summary(officer::read_docx(datei))$text, collapse = "\n")
}

erwartete_datei <- function() {
  list.files("Auswertungen", pattern = "^Elternbriefe_.*\\.docx$", full.names = TRUE)
}

anzahl_anreden <- function(text) {
  length(regmatches(text, gregexpr("Liebe Erziehungsberechtigte", text)))
}

# officer::body_add_docx() schreibt weitere Briefe nicht in document.xml,
# sondern legt sie als eigenes Dokument im Paket ab (word/file*.docx) und
# verweist per altChunk darauf.
anzahl_altchunks <- function(datei) {
  xml <- paste(readLines(unz(datei, "word/document.xml"), warn = FALSE), collapse = "")
  treffer <- gregexpr("<w:altChunk", xml, fixed = TRUE)[[1]]
  if (treffer[1] == -1) 0L else length(treffer)
}

eingebettete_dokumente <- function(datei) {
  eintraege <- utils::unzip(datei, list = TRUE)$Name
  teile <- grep("^word/file.*\\.docx$", eintraege, value = TRUE)
  if (length(teile) == 0) return(character(0))
  ziel <- tempfile("eingebettet")
  dir.create(ziel)
  utils::unzip(datei, files = teile, exdir = ziel)
  file.path(ziel, teile)
}

test_that("fuer ein einzelnes Kind entsteht ein Elternbrief", {
  skip_if_not(rmarkdown::pandoc_available(), "pandoc nicht gefunden")

  brief_test_umgebung({
    df <- lade_fixture("klasse_5c.tsv")[1, ]   # Testmann, Anna (2B)
    erzeuge_briefe(df, lehrername = "Test, Tina")

    dateien <- erwartete_datei()
    expect_length(dateien, 1)
    expect_match(basename(dateien), paste0("Elternbriefe_", Sys.Date(), "_5c\\.docx"))

    text <- brief_text(dateien)
    expect_match(text, "Testmann, Anna", fixed = TRUE)
    expect_match(text, "Diagnostik im Fach Deutsch im Jahrgang 5", fixed = TRUE)
    expect_match(text, "B2: ", fixed = TRUE)   # Kategorie 2B -> Elterntext B2
    expect_match(text, "Test, Tina", fixed = TRUE)
    expect_equal(anzahl_anreden(text), 1)
    # ein einzelner Brief wird nicht als altChunk angehaengt
    expect_equal(anzahl_altchunks(dateien), 0)
  })
})

test_that("fuer mehrere Kinder entsteht eine Datei mit einem Brief je Kind", {
  skip_if_not(rmarkdown::pandoc_available(), "pandoc nicht gefunden")

  brief_test_umgebung({
    df <- lade_fixture("klasse_5c.tsv")[1:2, ]  # Anna (2B) und Ben (4C)

    # Zustand der Briefvorlagen im Programmverzeichnis vorher merken
    vorher <- sort(list.files("elternbrief", recursive = TRUE, all.files = TRUE, no.. = TRUE))

    erzeuge_briefe(df, lehrername = "Test, Tina", signatur = "Abteilungsleitung I")

    dateien <- erwartete_datei()
    expect_length(dateien, 1)

    # erster Brief steht direkt im Dokument
    text <- brief_text(dateien)
    expect_match(text, "Testmann, Anna", fixed = TRUE)
    expect_match(text, "B2: ", fixed = TRUE)
    expect_match(text, "Abteilungsleitung I", fixed = TRUE)
    expect_equal(anzahl_anreden(text), 1)

    # zweiter Brief ist als eingebettetes Dokument angehaengt
    expect_equal(anzahl_altchunks(dateien), 1)
    eingebettet <- eingebettete_dokumente(dateien)
    expect_length(eingebettet, 1)
    text2 <- brief_text(eingebettet)
    expect_match(text2, "Beispiel, Ben", fixed = TRUE)
    expect_match(text2, "C4: ", fixed = TRUE)
    expect_match(text2, "Abteilungsleitung I", fixed = TRUE)

    # Paket B: das Programmverzeichnis bleibt unberuehrt (kein knit.md usw.)
    nachher <- sort(list.files("elternbrief", recursive = TRUE, all.files = TRUE, no.. = TRUE))
    expect_equal(nachher, vorher)
    expect_false(file.exists("elternbrief/elternbrief.knit.md"))
    # die Arbeitskopie im Temp-Verzeichnis ist wieder aufgeraeumt. Windows gibt
    # gesperrte Dateien manchmal erst verzoegert frei (Virenscanner, Cloud-Sync),
    # deshalb bis zu einer Sekunde nachfassen, bevor der Test fehlschlaegt.
    kopie <- file.path(tempdir(), paste0("elternbrief_", Sys.getpid()))
    for (i in 1:10) {
      if (!dir.exists(kopie)) break
      Sys.sleep(0.1)
    }
    expect_false(dir.exists(kopie))
  })
})

test_that("elternbrief_vorbereiten kopiert die Vorlagen in ein Temp-Verzeichnis", {
  vorlage <- elternbrief_vorbereiten(file.path(projekt_root, "elternbrief"))
  on.exit(unlink(vorlage, recursive = TRUE), add = TRUE)

  expect_true(dir.exists(vorlage))
  expect_setequal(list.files(vorlage), list.files(file.path(projekt_root, "elternbrief")))
  for (datei in c("elternbrief.Rmd", "ergebnisse.xlsx", "table.png", "_output.yml")) {
    expect_true(file.exists(file.path(vorlage, datei)))
  }
  # Arbeitskopie liegt im Temp-Verzeichnis und damit ausserhalb des
  # Projektordners, damit dort nichts geschrieben wird. Bei umgeleitetem TMPDIR
  # (z. B. in einer Sandbox) kann tempdir() selbst im Projekt liegen - dann
  # entfaellt die zweite Pruefung, weil sie die Umgebung statt den Code testet.
  expect_true(startsWith(normalizePath(vorlage), normalizePath(tempdir())))
  if (!startsWith(normalizePath(tempdir()), normalizePath(projekt_root))) {
    expect_false(startsWith(normalizePath(vorlage), normalizePath(projekt_root)))
  }
})

test_that("vorlagen_aufraeumen loescht die Arbeitskopie und meldet Erfolg", {
  ziel <- file.path(tempdir(), paste0("aufraeumen_", Sys.getpid()))
  dir.create(file.path(ziel, "unterordner"), recursive = TRUE, showWarnings = FALSE)
  writeLines("rest", file.path(ziel, "rest.txt"))

  expect_true(vorlagen_aufraeumen(ziel))
  expect_false(dir.exists(ziel))
  # nichts zu loeschen ist kein Fehler (on.exit laeuft immer)
  expect_true(vorlagen_aufraeumen(ziel))
})

test_that("eine liegen gebliebene Arbeitskopie wird ersetzt", {
  # Reste aus einem abgebrochenen Lauf duerfen den naechsten nicht blockieren
  ziel <- file.path(tempdir(), paste0("elternbrief_", Sys.getpid()))
  dir.create(ziel, recursive = TRUE, showWarnings = FALSE)
  writeLines("alt", file.path(ziel, "alt.txt"))

  vorlage <- elternbrief_vorbereiten(file.path(projekt_root, "elternbrief"))
  on.exit(vorlagen_aufraeumen(ziel), add = TRUE)

  expect_equal(normalizePath(vorlage), normalizePath(ziel))
  expect_false(file.exists(file.path(ziel, "alt.txt")))
  expect_true(file.exists(file.path(ziel, "elternbrief.Rmd")))
})

test_that("elternbrief_vorbereiten meldet fehlende Vorlagen", {
  expect_error(elternbrief_vorbereiten(file.path(tempdir(), "gibt_es_nicht")), "nicht gefunden")
})

test_that("ohne Link entsteht kein QR-Code und der Brief bleibt fehlerfrei", {
  skip_if_not(rmarkdown::pandoc_available(), "pandoc nicht gefunden")

  brief_test_umgebung({
    df <- lade_fixture("klasse_5c.tsv")[1, ]
    ergebnis <- erzeuge_briefe(df, lehrername = "Test, Tina", qrLink = NULL)

    expect_equal(ergebnis$erstellt, 1)
    expect_length(ergebnis$fehler, 0)

    dateien <- erwartete_datei()
    expect_length(dateien, 1)
    text <- brief_text(dateien)
    expect_false(grepl("tinyurl", text))
    expect_false(grepl("Sie möchten Ihr Kind unterstützen", text))
  })
})

test_that("ein fehlerhafter Brief verhindert die uebrigen nicht", {
  skip_if_not(rmarkdown::pandoc_available(), "pandoc nicht gefunden")

  brief_test_umgebung({
    df <- lade_fixture("klasse_5c.tsv")[1:2, ]   # Anna und Ben

    ergebnis <- mit_compose_letter(
      function(echt) {
        function(name, ...) {
          if (identical(name, "Beispiel, Ben")) stop("Testfehler beim Rendern")
          echt(name, ...)
        }
      },
      erzeuge_briefe(df, lehrername = "Test, Tina")
    )

    # Anna wurde erstellt, Ben nicht - die Datei existiert trotzdem und ist nutzbar
    expect_equal(ergebnis$erstellt, 1)
    expect_length(ergebnis$fehler, 1)
    expect_match(ergebnis$fehler, "Beispiel, Ben", fixed = TRUE)
    expect_match(ergebnis$fehler, "Testfehler beim Rendern", fixed = TRUE)

    expect_true(file.exists(ergebnis$datei))
    text <- brief_text(ergebnis$datei)
    expect_match(text, "Testmann, Anna", fixed = TRUE)
    expect_false(grepl("Beispiel, Ben", text))
  })
})

test_that("scheitern alle Briefe, gibt es einen klaren Fehler", {
  brief_test_umgebung({
    df <- lade_fixture("klasse_5c.tsv")[1:2, ]
    fehlermeldung <- mit_compose_letter(
      function(echt) function(...) stop("kein Rendern moeglich"),
      tryCatch(erzeuge_briefe(df, lehrername = "Test, Tina"),
               error = function(e) conditionMessage(e))
    )
    expect_match(fehlermeldung, "kein Elternbrief erstellt werden")
    expect_match(fehlermeldung, "kein Rendern moeglich")
    expect_length(erwartete_datei(), 0)
  })
})
