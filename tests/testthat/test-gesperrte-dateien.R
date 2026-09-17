# Gesperrte Zieldateien: Word und Excel halten geoeffnete Dokumente exklusiv.
# Ohne Vorpruefung scheitert das Anlegen der Datei erst nach dem Rendern, und die
# Meldung des Renderers ("Permission denied", "failed to copy rendered pandoc
# artefact") laesst nicht erkennen, was zu tun ist.

# .datei_gesperrt() mit einem Oeffnen, das fehlschlaegt (so verhaelt sich Word)
mit_gesperrter_datei <- function(code) {
  echt <- .datei_gesperrt
  assign(".datei_gesperrt",
         function(pfad, oeffnen = NULL) TRUE,
         envir = globalenv())
  on.exit(assign(".datei_gesperrt", echt, envir = globalenv()), add = TRUE)
  force(code)
}

test_that("die Sperrpruefung erkennt freie und fehlende Dateien", {
  withr::with_tempdir({
    frei <- file.path(getwd(), "frei.txt")
    writeLines("hallo", frei)

    expect_false(.datei_gesperrt(frei))
    expect_false(.datei_gesperrt(file.path(getwd(), "gibt_es_nicht.txt")))
    expect_false(.datei_gesperrt(""))
    expect_false(.datei_gesperrt(NULL))
    expect_no_error(.pruefe_datei_frei(frei))
    expect_no_error(.pruefe_datei_frei(file.path(getwd(), "gibt_es_nicht.txt")))
  })
})

test_that("eine gesperrte Datei wird mit klarer Meldung gemeldet", {
  withr::with_tempdir({
    ziel <- file.path(getwd(), "Infobrief_5-6_5c_6c.docx")
    writeLines("belegt", ziel)

    # Oeffnen schlaegt fehl -> gesperrt
    scheitert <- function(p) stop("Permission denied")
    expect_true(.datei_gesperrt(ziel, oeffnen = scheitert))

    fehler <- tryCatch(.pruefe_datei_frei(ziel, oeffnen = scheitert),
                       error = function(e) conditionMessage(e))
    expect_match(fehler, "Infobrief_5-6_5c_6c.docx", fixed = TRUE)
    expect_match(fehler, "geöffnet oder schreibgeschützt", fixed = TRUE)
    expect_match(fehler, "Bitte schließen", fixed = TRUE)

    # eine freie Datei wird nicht gemeldet
    expect_no_error(.pruefe_datei_frei(ziel, oeffnen = function(p) file(p, "r+b")))
  })
})

# Kohorte aus den Fixtures (5c und 6c), wie in den Infobrief-Tests
kohorte_aus_fixtures <- function() {
  df <- dplyr::bind_rows(lade_fixture("klasse_5c.tsv"), lade_fixture("klasse_6c.tsv"))
  build_cohort(df, 5, 6)
}

test_that("der Infobrief prueft die Zieldatei vor dem Rendern", {
  withr::with_tempdir({
    dir.create("infobrief")
    dateien <- list.files(file.path(projekt_root, "infobrief"), full.names = TRUE)
    file.copy(dateien, "infobrief", recursive = TRUE)
    dir.create("Auswertungen")
    withr::local_options(ctest.outdir.fallback = file.path(getwd(), "benutzer"))

    ziel <- file.path(getwd(), "Auswertungen", "Infobrief_5-6_5c_6c.docx")

    fehler <- mit_gesperrter_datei(
      tryCatch({
        utils::capture.output(suppressMessages(
          create_infobrief(kohorte_aus_fixtures(), klassenleitung = "6c",
                           absender = "Test, Tina")))
        NULL
      }, error = function(e) conditionMessage(e)))

    expect_false(is.null(fehler))
    expect_match(fehler, "Infobrief_5-6_5c_6c.docx", fixed = TRUE)
    expect_match(fehler, "geöffnet oder schreibgeschützt", fixed = TRUE)
    # es wurde nichts gerendert
    expect_false(file.exists(ziel))
  })
})

test_that("die Elternbriefe pruefen die Zieldatei vor dem Rendern", {
  withr::with_tempdir({
    dir.create("elternbrief")
    dateien <- list.files(file.path(projekt_root, "elternbrief"), full.names = TRUE)
    file.copy(dateien, "elternbrief", recursive = TRUE)
    dir.create("Auswertungen")
    withr::local_options(ctest.outdir.fallback = file.path(getwd(), "benutzer"))

    df <- lade_fixture("klasse_5c.tsv")[1, ]

    meldungen <- list()
    fehler <- mit_gesperrter_datei(
      tryCatch({
        utils::capture.output(suppressMessages(
          create_letters(df, lehrername = "Test, Tina",
                         fortschritt = function(anteil, text) {
                           meldungen[[length(meldungen) + 1]] <<- text
                         })))
        NULL
      }, error = function(e) conditionMessage(e)))

    expect_false(is.null(fehler))
    expect_match(fehler, "geöffnet oder schreibgeschützt", fixed = TRUE)
    expect_match(fehler, "Elternbriefe_", fixed = TRUE)
    # kein Brief wurde gerendert: nur die Vorbereitung wurde gemeldet
    expect_false(any(grepl("^Brief ", unlist(meldungen))))
    expect_length(list.files("Auswertungen", pattern = "^Elternbriefe_.*\\.docx$"), 0)
  })
})

test_that("das Speichern prueft seine Zieldateien", {
  withr::with_tempdir({
    file.copy(file.path(projekt_root, "template.docx"), "template.docx")
    dir.create("Auswertungen")
    df <- lade_fixture("klasse_5c.tsv")

    fehler <- mit_gesperrter_datei(
      tryCatch({ saveData(df); NULL }, error = function(e) conditionMessage(e)))

    expect_false(is.null(fehler))
    expect_match(fehler, "geöffnet oder schreibgeschützt", fixed = TRUE)
    # nichts wurde geschrieben
    expect_length(list.files("Auswertungen"), 0)
  })
})
