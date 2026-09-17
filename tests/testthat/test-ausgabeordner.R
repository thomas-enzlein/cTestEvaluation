# Ausgabeordner und Schreibrechte (Paket B).
#
# Die installierte App liegt unter C:/ProgramData. Ein normaler Benutzer
# (Lehrkraft) darf dort nicht in jedem Fall schreiben. Deshalb gilt:
# Programmordner nur benutzen, wenn er wirklich beschreibbar ist - sonst in
# den Benutzerordner ausweichen. Beides wird hier ohne echte Rechteaenderung
# geprueft, indem ein unbrauchbarer Pfad erzeugt wird ("Auswertungen" als Datei).

test_that("verzeichnis_sicherstellen legt Ordner an und prueft die Schreibbarkeit", {
  withr::with_tempdir({
    tief <- file.path("neu", "tiefer", "Auswertungen")
    expect_true(verzeichnis_sicherstellen(tief))
    expect_true(dir.exists(tief))
    # vom Schreibtest bleibt nichts liegen
    expect_length(list.files(tief, all.files = TRUE, no.. = TRUE), 0)
  })
})

test_that("verzeichnis_sicherstellen erkennt unbrauchbare Pfade", {
  withr::with_tempdir({
    writeLines("kein Ordner", "blocker")
    expect_false(verzeichnis_sicherstellen(file.path("blocker", "Auswertungen")))
    expect_false(verzeichnis_sicherstellen(""))
    expect_false(verzeichnis_sicherstellen(NULL))
  })
})

test_that("ein vorhandener, aber nicht beschreibbarer Ordner wird erkannt", {
  withr::with_tempdir({
    dir.create("Auswertungen")
    # Der Schreibtest legt eine Datei .schreibtest_<pid> an. Existiert an dieser
    # Stelle bereits ein Verzeichnis, schlaegt das Schreiben fehl - genau das
    # entspricht einem Ordner ohne Schreibrecht (z. B. unter C:/ProgramData).
    dir.create(file.path("Auswertungen", paste0(".schreibtest_", Sys.getpid())))
    expect_false(verzeichnis_sicherstellen("Auswertungen"))
  })
})

test_that("createFilePath legt den Ausgabeordner an", {
  withr::with_tempdir({
    ausgabeordner_zuruecksetzen()
    ordner <- createFilePath(NULL, "")
    # Schreibweise des Systems: .pfad_nativ() (unter Windows Backslashes)
    expect_equal(ordner, .pfad_nativ(file.path(getwd(), "Auswertungen")))
    expect_true(dir.exists(ordner))
    expect_equal(createFilePath("Test", "tsv"), .pfad_nativ(file.path(ordner, "Test.tsv")))
    ausgabeordner_zuruecksetzen()
  })
})

test_that("ohne Schreibmoeglichkeit im Programmordner wird auf den Benutzerordner ausgewichen", {
  withr::with_tempdir({
    # "Auswertungen" ist hier eine Datei -> Programmordner nicht nutzbar
    writeLines("blockiert", "Auswertungen")
    fallback <- file.path(getwd(), "Benutzerordner")
    withr::with_options(list(ctest.outdir.fallback = fallback), {
      ausgabeordner_zuruecksetzen()
      ordner <- createFilePath(NULL, "")
      expect_equal(ordner, .pfad_nativ(file.path(fallback, "C-Test Auswertung")))
      expect_true(dir.exists(ordner))
      # dort laesst sich wirklich schreiben
      probe <- file.path(ordner, "probe.txt")
      expect_true(file.create(probe))
      unlink(probe)
      ausgabeordner_zuruecksetzen()
    })
  })
})

test_that("der Benutzerordner wird im Dokumente-Verzeichnis gesucht", {
  # ohne Vorgabe (die Testumgebung setzt eine, siehe helper-app.R)
  withr::with_options(list(ctest.outdir.fallback = NULL), {
    expect_match(benutzer_ausgabeordner(), "C-Test Auswertung$")
    expect_false(grepl(tempdir(), benutzer_ausgabeordner(), fixed = TRUE))
  })
})

test_that("die Option ctest.outdir hat Vorrang", {
  withr::with_tempdir({
    ziel <- file.path(getwd(), "vorgabe")
    withr::with_options(list(ctest.outdir = ziel), {
      expect_equal(createFilePath(NULL, ""), .pfad_nativ(ziel))
      expect_true(dir.exists(ziel))
    })
  })
})

test_that("ein unbrauchbarer vorgegebener Ordner meldet einen klaren Fehler", {
  withr::with_tempdir({
    writeLines("blockiert", "blocker")
    withr::with_options(list(ctest.outdir = file.path(getwd(), "blocker", "x")), {
      expect_error(createFilePath(NULL, ""), "kann nicht angelegt werden")
    })
  })
})

test_that("saveData schreibt in den aufgeloesten Ausgabeordner", {
  withr::with_tempdir({
    file.copy(file.path(projekt_root, "template.docx"), "template.docx")
    ziel <- file.path(getwd(), "meinZiel")
    withr::with_options(list(ctest.outdir = ziel), {
      df <- leere_tabelle()
      df <- addEntry(df, name = "Testmann, Anna", klasse = "5a", rf = 28, we = 34, numItems = 40)

      msgs <- saveData(df)

      expect_match(msgs, .pfad_nativ(ziel), fixed = TRUE)
      dateien <- list.files(ziel)
      expect_true(any(grepl("\\.tsv$", dateien)))
      expect_true(any(grepl("\\.docx$", dateien)))
      expect_true(any(grepl("\\.xlsx$", dateien)))
      # im Programmordner entsteht dabei nichts
      expect_false(dir.exists(file.path(getwd(), "Auswertungen")))
    })
  })
})

test_that("saveData meldet einen unbrauchbaren Ausgabeordner als Fehler", {
  withr::with_tempdir({
    file.copy(file.path(projekt_root, "template.docx"), "template.docx")
    writeLines("blockiert", "blocker")
    withr::with_options(list(ctest.outdir = file.path(getwd(), "blocker", "x")), {
      df <- leere_tabelle()
      df <- addEntry(df, name = "Testmann, Anna", klasse = "5a", rf = 28, we = 34, numItems = 40)
      expect_error(saveData(df), "kann nicht angelegt werden")
    })
  })
})

test_that("create_letters prueft den Ausgabeordner vor dem Rendern", {
  withr::with_tempdir({
    dir.create("elternbrief")
    writeLines("blockiert", "blocker")
    withr::with_options(list(ctest.outdir = file.path(getwd(), "blocker", "x")), {
      df <- lade_fixture("klasse_5c.tsv")[1, ]
      # schlaegt sofort fehl, ohne Briefe zu rendern
      expect_error(create_letters(df, lehrername = "Test, Tina"), "kann nicht angelegt werden")
    })
  })
})
