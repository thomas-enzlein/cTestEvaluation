# Anpassbare Vorlagen: persoenliche Kopie bei den Dokumenten des Benutzers und
# Overlay beim Rendern.
#
# Hintergrund: Die mitgelieferten Vorlagen liegen im Programmordner und werden
# bei einem Update ueberschrieben. Anpassungen (Briefkopf, Schrift,
# Ergebnistabelle, Kategorie-Texte) muessen das ueberleben - und fuer
# Elternbrief UND Infobrief gelten.

# Persoenlicher Vorlagenordner im Test: ueber dieselbe Option, die auch der
# Ausgabeordner-Fallback nutzt (benutzer_ausgabeordner()). Dazu wird der zentrale
# Vorlagenordner in das Testverzeichnis kopiert - dort sucht die App ihn relativ
# zum Arbeitsverzeichnis.
mit_benutzerordner <- function(code, zentral = TRUE) {
  withr::with_tempdir({
    if (zentral) {
      dir.create("vorlagen")
      file.copy(list.files(file.path(projekt_root, "vorlagen"), full.names = TRUE),
                "vorlagen", recursive = TRUE)
    }
    withr::with_options(list(ctest.outdir.fallback = file.path(getwd(), "benutzer")),
                        force(code))
  })
}

# Pfad des persoenlichen Ordners, unabhaengig von vorlagen_ordner()
test_ordner <- function() file.path("benutzer", "C-Test Auswertung", "vorlagen")

md5 <- function(pfad) unname(tools::md5sum(pfad))

test_that("der Vorlagenordner liegt bei den Dokumenten des Benutzers", {
  mit_benutzerordner({
    # Anzeigeform: unter Windows mit Backslashes (siehe .pfad_nativ)
    erwartet <- .pfad_nativ(file.path(getwd(), test_ordner()))
    expect_equal(vorlagen_ordner(), erwartet)
    # ohne anlegen wird nichts erzeugt
    expect_false(dir.exists(erwartet))

    expect_equal(vorlagen_ordner(anlegen = TRUE), erwartet)
    expect_true(dir.exists(erwartet))
  })
})

test_that("Pfade werden in der Schreibweise des Systems ausgegeben", {
  mit_benutzerordner({
    withr::with_dir(projekt_root, {
      ordner <- vorlagen_ordner()
      ziel <- vorlage_bereitstellen("template.docx")

      if (.Platform$OS.type == "windows") {
        # kein Mischmasch aus "C:\Users\.../Documents": genau der Fall, den
        # file.path() mit USERPROFILE erzeugt
        expect_false(grepl("/", ordner, fixed = TRUE))
        expect_false(grepl("/", ziel, fixed = TRUE))
        # und genau ein Backslash je Trenner (kein doppelter)
        expect_equal(.pfad_nativ("C:\\Users\\Thomas/Documents/x"),
                     "C:\\Users\\Thomas\\Documents\\x")
      }
      # und die Pfade zeigen trotzdem auf die richtigen Dateien
      expect_true(dir.exists(ordner))
      expect_true(file.exists(ziel))
    })
  })
})

test_that("vorlage_bereitstellen kopiert die mitgelieferte Vorlage einmalig", {
  mit_benutzerordner({
    withr::with_dir(projekt_root, {
      ziel <- vorlage_bereitstellen("template.docx")
      expect_true(file.exists(ziel))
      expect_equal(md5(ziel), md5(file.path(projekt_root, "vorlagen", "template.docx")))

      # eine vorhandene Anpassung wird nie ueberschrieben
      writeLines("angepasst", ziel, useBytes = TRUE)
      expect_equal(vorlage_bereitstellen("template.docx"), ziel)
      expect_equal(readLines(ziel, warn = FALSE), "angepasst")
    })
  })
})

test_that("vorlagen_bereitstellen legt alle anpassbaren Dateien an", {
  mit_benutzerordner({
    withr::with_dir(projekt_root, {
      ordner <- vorlagen_bereitstellen()
      expect_setequal(list.files(ordner), .vorlagen_dateien)
      # logo.png gehoert nicht dazu: das Brieflogo steckt im Kopf der Vorlage
      expect_false("logo.png" %in% list.files(ordner))
    })
  })
})

test_that("eine angepasste Vorlage ueberlagert die mitgelieferte", {
  mit_benutzerordner({
    withr::with_dir(projekt_root, {
      ordner <- vorlagen_ordner(anlegen = TRUE)
      writeLines("meine vorlage", file.path(ordner, "template.docx"), useBytes = TRUE)

      arbeitsordner <- elternbrief_vorbereiten(file.path(projekt_root, "elternbrief"))
      on.exit(vorlagen_aufraeumen(arbeitsordner), add = TRUE)

      expect_equal(readLines(file.path(arbeitsordner, "template.docx"), warn = FALSE),
                   "meine vorlage")
      # ohne persoenliche Kopie gilt die zentral mitgelieferte Kategorie-Tabelle
      expect_equal(md5(file.path(arbeitsordner, "ergebnisse.xlsx")),
                   md5(file.path(projekt_root, "vorlagen", "ergebnisse.xlsx")))
      # die Logik bleibt unberuehrt: das Rmd ist das mitgelieferte
      expect_equal(md5(file.path(arbeitsordner, "elternbrief.Rmd")),
                   md5(file.path(projekt_root, "elternbrief", "elternbrief.Rmd")))
    })
  })
})

test_that("ohne persoenliche Kopie bleibt die mitgelieferte Vorlage", {
  mit_benutzerordner({
    arbeitsordner <- elternbrief_vorbereiten(file.path(projekt_root, "elternbrief"))
    on.exit(vorlagen_aufraeumen(arbeitsordner), add = TRUE)

    expect_equal(md5(file.path(arbeitsordner, "template.docx")),
                 md5(file.path(projekt_root, "vorlagen", "template.docx")))
    expect_equal(md5(file.path(arbeitsordner, "ergebnisse.xlsx")),
                 md5(file.path(projekt_root, "vorlagen", "ergebnisse.xlsx")))
  })
})

test_that("auch der Infobrief nutzt die angepasste Vorlage", {
  mit_benutzerordner({
    ordner <- vorlagen_ordner(anlegen = TRUE)
    writeLines("meine vorlage", file.path(ordner, "template.docx"), useBytes = TRUE)
    # die Kategorie-Texte gehoeren nicht in die Infobrief-Mappe: eine Anpassung
    # ersetzt Vorhandenes, sie erfindet nichts dazu
    writeLines("kein xlsx", file.path(ordner, "ergebnisse.xlsx"), useBytes = TRUE)

    arbeitsordner <- infobrief_vorbereiten(file.path(projekt_root, "infobrief"))
    on.exit(vorlagen_aufraeumen(arbeitsordner), add = TRUE)

    expect_equal(readLines(file.path(arbeitsordner, "template.docx"), warn = FALSE),
                 "meine vorlage")
    expect_false(file.exists(file.path(arbeitsordner, "ergebnisse.xlsx")))
  })
})

test_that("eine unlesbare Kategorie-Tabelle wird nicht uebernommen", {
  mit_benutzerordner({
    ordner <- vorlagen_ordner(anlegen = TRUE)
    writeLines("kein xlsx", file.path(ordner, "ergebnisse.xlsx"), useBytes = TRUE)

    arbeitsordner <- elternbrief_vorbereiten(file.path(projekt_root, "elternbrief"))
    on.exit(vorlagen_aufraeumen(arbeitsordner), add = TRUE)

    # die mitgelieferte Tabelle bleibt erhalten - ein kaputter Tabellenedit darf
    # die Briefe nicht unbrauchbar machen
    expect_equal(md5(file.path(arbeitsordner, "ergebnisse.xlsx")),
                 md5(file.path(projekt_root, "vorlagen", "ergebnisse.xlsx")))
    expect_true(.kategorie_tabelle_ok(file.path(projekt_root, "vorlagen", "ergebnisse.xlsx")))
    expect_false(.kategorie_tabelle_ok(file.path(ordner, "ergebnisse.xlsx")))
  })
})

test_that("fehlende mitgelieferte Vorlagen werden klar gemeldet", {
  withr::with_tempdir({
    # Arbeitskopie ohne den zentralen Vorlagenordner: das darf nicht still zu
    # einem Brief ohne Vorlage fuehren
    dir.create("elternbrief")
    file.copy(list.files(file.path(projekt_root, "elternbrief"), full.names = TRUE),
              "elternbrief", recursive = TRUE)
    dir.create("infobrief")
    file.copy(list.files(file.path(projekt_root, "infobrief"), full.names = TRUE),
              "infobrief", recursive = TRUE)

    expect_false(dir.exists("vorlagen"))
    expect_error(elternbrief_vorbereiten(file.path(getwd(), "elternbrief")),
                 "mitgelieferten Vorlagen fehlen")
    expect_error(infobrief_vorbereiten(file.path(getwd(), "infobrief")),
                 "mitgelieferten Vorlagen fehlen")
  })
})

test_that("vorlage_status nennt die aktive Vorlage, ohne etwas anzulegen", {
  mit_benutzerordner({
    withr::with_dir(projekt_root, {
      # ohne persoenliche Kopie: die mitgelieferte Vorlage, nichts wird erzeugt
      status <- vorlage_status()
      expect_false(status$eigen)
      expect_equal(status$pfad, .pfad_nativ(file.path(projekt_root, "vorlagen", "template.docx")))
      expect_true(is.na(status$zeit))
      expect_false(dir.exists(vorlagen_ordner()))
      expect_match(vorlage_status_text(status), "mitgelieferte Vorlage", fixed = TRUE)

      # mit persoenlicher Kopie: Pfad und Datum
      ziel <- vorlage_bereitstellen("template.docx")
      status <- vorlage_status()
      expect_true(status$eigen)
      expect_equal(status$pfad, .pfad_nativ(ziel))
      expect_match(status$zeit, "^[0-9]{2}\\.[0-9]{2}\\.[0-9]{4} [0-9]{2}:[0-9]{2}$")
      expect_match(vorlage_status_text(status), "Persönliche Vorlage", fixed = TRUE)
      expect_match(vorlage_status_text(status), status$pfad, fixed = TRUE)
    })
  })
})

test_that("ein unbrauchbarer Vorlagenordner wird gemeldet statt still zu scheitern", {
  mit_benutzerordner({
    # "vorlagen" ist eine Datei: der Ordner kann nicht angelegt werden. Bewusst
    # der absolute Pfad aus vorlagen_ordner(), damit der Blocker unabhaengig vom
    # Arbeitsverzeichnis dort liegt, wo die App sucht.
    ziel <- vorlagen_ordner()
    dir.create(dirname(ziel), recursive = TRUE)
    writeLines("blockiert", ziel)

    shiny::testServer(server, {
      session$setInputs(btVorlageOeffnen = 1)
      expect_match(als_text(output$text), "Öffnen fehlgeschlagen", fixed = TRUE)
      expect_match(als_text(output$text), "Vorlagenordner konnte nicht angelegt werden",
                   fixed = TRUE)
    })
  })
})

test_that("ein Rest im Temp-Verzeichnis verhindert die Briefe nicht", {
  withr::with_tempdir({
    dir.create("elternbrief")
    file.copy(list.files(file.path(projekt_root, "elternbrief"), full.names = TRUE),
              "elternbrief", recursive = TRUE)
    dir.create("vorlagen")
    file.copy(list.files(file.path(projekt_root, "vorlagen"), full.names = TRUE),
              "vorlagen", recursive = TRUE)
    withr::local_options(ctest.outdir.fallback = file.path(getwd(), "benutzer"))

    # Windows gibt gesperrte Dateien manchmal erst verzoegert frei, dann bleibt
    # unter dem ueblichen Namen ein Rest liegen und das Anlegen scheitert.
    rest <- file.path(tempdir(), paste0("elternbrief_", Sys.getpid()))
    unlink(rest, recursive = TRUE)
    writeLines("Rest", rest)
    on.exit(unlink(rest), add = TRUE)

    # Beleg fuer die Ursache: ein Rest blockiert dir.create()
    expect_false(dir.create(rest, recursive = TRUE, showWarnings = FALSE))

    # Die Vorbereitung weicht deshalb auf einen eindeutigen Namen aus
    arbeitsordner <- elternbrief_vorbereiten()
    on.exit(vorlagen_aufraeumen(arbeitsordner), add = TRUE)

    expect_true(dir.exists(arbeitsordner))
    expect_true(file.exists(file.path(arbeitsordner, "elternbrief.Rmd")))
    expect_true(file.exists(file.path(arbeitsordner, "template.docx")))
  })
})
