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

# Briefe erzeugen; die Ausgabe von knitr/pandoc wird dabei unterdrueckt
erzeuge_briefe <- function(df, lehrername, signatur = "", qrLink = NULL) {
  invisible(utils::capture.output(
    suppressMessages(create_letters(df, lehrername = lehrername,
                                    signatur = signatur, qrLink = qrLink))
  ))
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
  })
})

test_that("ohne Link entsteht kein QR-Code und der Brief bleibt fehlerfrei", {
  skip_if_not(rmarkdown::pandoc_available(), "pandoc nicht gefunden")

  brief_test_umgebung({
    df <- lade_fixture("klasse_5c.tsv")[1, ]
    expect_no_error(erzeuge_briefe(df, lehrername = "Test, Tina", qrLink = NULL))

    dateien <- erwartete_datei()
    expect_length(dateien, 1)
    text <- brief_text(dateien)
    expect_false(grepl("tinyurl", text))
    expect_false(grepl("Sie möchten Ihr Kind unterstützen", text))
    # Hinweis: pandoc meldet fuer die leere Bildreferenz des QR-Codes
    # "[WARNING] Could not fetch resource" - kosmetisch, kein Fehler.
  })
})
