# Datensicherung vor dem Briefversand.
#
# Hintergrund: "speichern" ist ein eigener Knopf. Werte von Hand eintippen und
# nur die Briefe erstellen hiess bisher: Der Datensatz existierte danach nur noch
# als Word-Datei - kein Vorjahresvergleich, keine Itemzahl, keine
# Nachauswertung. Deshalb sichert die App den Stand vor dem Rendern als tsv.

sicherungsdateien <- function() {
  list.files("Auswertungen", pattern = "_sicherung_.*\\.tsv$", full.names = TRUE)
}

test_that("der Fingerabdruck haengt nicht an der Zeilenreihenfolge", {
  df <- lade_fixture("klasse_5c.tsv")
  umgedreht <- df[rev(seq_len(nrow(df))), , drop = FALSE]

  expect_equal(daten_fingerabdruck(df), daten_fingerabdruck(umgedreht))

  geaendert <- df
  geaendert$`WE-%`[1] <- geaendert$`WE-%`[1] + 1
  expect_false(identical(daten_fingerabdruck(df), daten_fingerabdruck(geaendert)))

  ohne_wert <- df
  ohne_wert$`WE-%`[1] <- NA
  expect_false(identical(daten_fingerabdruck(df), daten_fingerabdruck(ohne_wert)))

  expect_true(is.na(daten_fingerabdruck(leere_tabelle())))
  expect_true(is.na(daten_fingerabdruck(NULL)))
})

test_that("sichere_daten schreibt nur die tsv - keine Berichte", {
  withr::with_tempdir({
    dir.create("Auswertungen")
    sicherungsstand_zuruecksetzen()
    df <- lade_fixture("klasse_5c.tsv")

    ergebnis <- sichere_daten(df)
    expect_true(ergebnis$geschrieben)
    expect_match(ergebnis$meldung, "zusaetzlich gesichert", fixed = TRUE)

    dateien <- sicherungsdateien()
    expect_length(dateien, 1)
    # nur Daten: im Ordner liegt nichts ausser dieser einen Datei
    expect_length(list.files("Auswertungen"), 1)
    # der Name nennt Datum und Klassen
    expect_match(basename(dateien),
                 "^C-Test_Auswertung_\\d{4}-\\d{2}-\\d{2}_5c_sicherung_\\d{6}\\.tsv$")

    # Itemzahl ist mitgesichert (sonst waeren Rohwerte spaeter nicht nachrechenbar)
    kopf <- strsplit(readLines(dateien, n = 1), "\t")[[1]]
    expect_true("Items" %in% kopf)

    # die Sicherung ist mit dem normalen Ladeweg wieder lesbar
    gelesen <- loadData(list(datapath = dateien, name = basename(dateien),
                             size = file.size(dateien),
                             type = "text/tab-separated-values"))
    expect_equal(nrow(gelesen), nrow(df))
    expect_equal(gelesen$Name, df$Name)
  })
})

test_that("sichere_daten sichert denselben Stand nicht zweimal", {
  withr::with_tempdir({
    dir.create("Auswertungen")
    sicherungsstand_zuruecksetzen()
    df <- lade_fixture("klasse_5c.tsv")

    expect_true(sichere_daten(df)$geschrieben)

    zweite <- sichere_daten(df)
    expect_false(zweite$geschrieben)
    expect_match(zweite$meldung, "bereits gesichert", fixed = TRUE)
    expect_length(sicherungsdateien(), 1)

    # geaenderte Daten werden erneut gesichert
    df$`WE-%`[1] <- df$`WE-%`[1] + 1
    expect_true(sichere_daten(df)$geschrieben)
    expect_length(sicherungsdateien(), 2)
  })
})

test_that("ohne Daten und ohne Schreibrechte gibt es eine Meldung statt eines Fehlers", {
  withr::with_tempdir({
    dir.create("Auswertungen")
    sicherungsstand_zuruecksetzen()

    leer <- sichere_daten(leere_tabelle())
    expect_false(leer$geschrieben)
    expect_match(leer$meldung, "Keine Daten", fixed = TRUE)
    expect_false(sichere_daten(NULL)$geschrieben)
    expect_length(sicherungsdateien(), 0)
  })

  withr::with_tempdir({
    # Ausgabeordner unbrauchbar machen: "blocker" ist eine Datei
    writeLines("blockiert", "blocker")
    withr::with_options(list(ctest.outdir = file.path(getwd(), "blocker", "x")), {
      sicherungsstand_zuruecksetzen()
      ergebnis <- sichere_daten(lade_fixture("klasse_5c.tsv"))
      expect_false(ergebnis$geschrieben)
      expect_match(ergebnis$meldung, "nicht zusaetzlich gesichert", fixed = TRUE)
    })
  })
})

test_that("nach dem Speichern legt der Briefversand keine zweite Datei an", {
  withr::with_tempdir({
    file.copy(file.path(projekt_root, "template.docx"), "template.docx")
    dir.create("Auswertungen")
    sicherungsstand_zuruecksetzen()
    df <- lade_fixture("klasse_5c.tsv")

    saveData(df)
    expect_true(any(grepl("\\.tsv$", list.files("Auswertungen"))))

    # der Stand liegt schon als Datei vor
    expect_false(sichere_daten(df)$geschrieben)
    expect_length(sicherungsdateien(), 0)
  })
})

test_that("nach dem Laden gilt der Stand als gesichert", {
  withr::with_tempdir({
    dir.create("Auswertungen")
    sicherungsstand_zuruecksetzen()
    shiny::testServer(server, {
      session$setInputs(numItems = "40", klassenstufe = "5", klBuchstabe = "c",
                        cbWEDiff = FALSE, cbAllCombined = TRUE, siPlotType = "Histogramm")
      ohne_bekannte_warnungen(session$setInputs(input_tsv = tsv_input("klasse_5c.tsv")))

      # die geladene Datei liegt bereits im Auswertungsordner
      expect_false(sichere_daten(rv$df)$geschrieben)

      # nach einer Aenderung wird wieder gesichert
      ohne_bekannte_warnungen(
        session$setInputs(schuelerName = "Neumann, Tim", weWert = 30, rfWert = 25,
                          btHinzufuegen = 1))
      expect_true(sichere_daten(rv$df)$geschrieben)
    })
  })
})

test_that("der Briefversand sichert die Daten, bevor gerendert wird", {
  withr::with_tempdir({
    # Auswertungen ist beschreibbar, die Briefvorlage fehlt: das Rendern
    # scheitert sofort - gesichert sein muss der Stand trotzdem
    dir.create("Auswertungen")
    sicherungsstand_zuruecksetzen()
    shiny::testServer(server, {
      session$setInputs(numItems = "40", klassenstufe = "5", klBuchstabe = "c",
                        cbWEDiff = FALSE, cbAllCombined = TRUE, siPlotType = "Histogramm")
      session$setInputs(schuelerName = "Testmann, Anna", weWert = 34, rfWert = 28,
                        btHinzufuegen = 1)
      session$setInputs(lehrername = "Test, Tina", btBrief = 1)

      expect_match(als_text(output$text), "Fehler beim Erstellen der Elternbriefe",
                   fixed = TRUE)
      expect_length(sicherungsdateien(), 1)
    })
  })
})

test_that("auch der Infobrief sichert die Daten vor dem Rendern", {
  withr::with_tempdir({
    dir.create("Auswertungen")
    sicherungsstand_zuruecksetzen()
    shiny::testServer(server, {
      session$setInputs(numItems = "40", klassenstufe = "5", klBuchstabe = "c",
                        cbWEDiff = FALSE, cbAllCombined = TRUE, siPlotType = "Histogramm",
                        infoKlassenleitung = "6c", infoAbsender = "Test, Tina")
      ohne_bekannte_warnungen(session$setInputs(input_tsv = tsv_input("klasse_5c.tsv")))
      ohne_bekannte_warnungen(session$setInputs(input_tsv = tsv_input("klasse_6c.tsv")))
      ohne_bekannte_warnungen(session$setInputs(siStufeAlt = 5, siStufeNeu = 6))

      # zwei geladene Dateien: der zusammengefuehrte Stand liegt nirgends auf der
      # Platte - er wird vor dem Rendern gesichert
      ohne_bekannte_warnungen(session$setInputs(btInfobrief = 1))

      expect_match(als_text(output$text), "Fehler beim Erstellen des Infobriefs",
                   fixed = TRUE)
      expect_length(sicherungsdateien(), 1)
    })
  })
})
