# Anpassbare Vorlagen: persoenliche Kopie bei den Dokumenten des Benutzers und
# Overlay beim Rendern.
#
# Hintergrund: Die mitgelieferten Vorlagen liegen im Programmordner und werden
# bei einem Update ueberschrieben. Anpassungen (Briefkopf, Schrift,
# Ergebnistabelle, Kategorie-Texte) muessen das ueberleben - und fuer
# Elternbrief UND Infobrief gelten.

# Persoenlicher Vorlagenordner im Test: ueber dieselbe Option, die auch der
# Ausgabeordner-Fallback nutzt (benutzer_ausgabeordner()).
mit_benutzerordner <- function(code) {
  withr::with_tempdir({
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
      expect_equal(md5(ziel), md5(file.path(projekt_root, "elternbrief", "template.docx")))

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
      writeLines("mein bild", file.path(ordner, "table.png"), useBytes = TRUE)

      arbeitsordner <- vorlagen_vorbereiten(file.path(projekt_root, "elternbrief"), "test")
      on.exit(vorlagen_aufraeumen(arbeitsordner), add = TRUE)

      expect_equal(readLines(file.path(arbeitsordner, "template.docx"), warn = FALSE),
                   "meine vorlage")
      expect_equal(readLines(file.path(arbeitsordner, "table.png"), warn = FALSE),
                   "mein bild")
      # die Logik bleibt unberuehrt: das Rmd ist das mitgelieferte
      expect_equal(md5(file.path(arbeitsordner, "elternbrief.Rmd")),
                   md5(file.path(projekt_root, "elternbrief", "elternbrief.Rmd")))
    })
  })
})

test_that("ohne persoenliche Kopie bleibt die mitgelieferte Vorlage", {
  mit_benutzerordner({
    arbeitsordner <- vorlagen_vorbereiten(file.path(projekt_root, "elternbrief"), "test")
    on.exit(vorlagen_aufraeumen(arbeitsordner), add = TRUE)

    expect_equal(md5(file.path(arbeitsordner, "template.docx")),
                 md5(file.path(projekt_root, "elternbrief", "template.docx")))
  })
})

test_that("auch der Infobrief nutzt die angepasste Vorlage", {
  mit_benutzerordner({
    ordner <- vorlagen_ordner(anlegen = TRUE)
    writeLines("meine vorlage", file.path(ordner, "template.docx"), useBytes = TRUE)
    # table.png gehoert nicht in die Infobrief-Mappe: eine Anpassung ersetzt
    # Vorhandenes, sie erfindet nichts dazu
    writeLines("mein bild", file.path(ordner, "table.png"), useBytes = TRUE)

    arbeitsordner <- vorlagen_vorbereiten(file.path(projekt_root, "infobrief"), "test")
    on.exit(vorlagen_aufraeumen(arbeitsordner), add = TRUE)

    expect_equal(readLines(file.path(arbeitsordner, "template.docx"), warn = FALSE),
                 "meine vorlage")
    expect_false(file.exists(file.path(arbeitsordner, "table.png")))
  })
})

test_that("eine unlesbare Kategorie-Tabelle wird nicht uebernommen", {
  mit_benutzerordner({
    ordner <- vorlagen_ordner(anlegen = TRUE)
    writeLines("kein xlsx", file.path(ordner, "ergebnisse.xlsx"), useBytes = TRUE)

    arbeitsordner <- vorlagen_vorbereiten(file.path(projekt_root, "elternbrief"), "test")
    on.exit(vorlagen_aufraeumen(arbeitsordner), add = TRUE)

    # die mitgelieferte Tabelle bleibt erhalten - ein kaputter Tabellenedit darf
    # die Briefe nicht unbrauchbar machen
    expect_equal(md5(file.path(arbeitsordner, "ergebnisse.xlsx")),
                 md5(file.path(projekt_root, "elternbrief", "ergebnisse.xlsx")))
    expect_true(.kategorie_tabelle_ok(file.path(projekt_root, "elternbrief", "ergebnisse.xlsx")))
    expect_false(.kategorie_tabelle_ok(file.path(ordner, "ergebnisse.xlsx")))
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
