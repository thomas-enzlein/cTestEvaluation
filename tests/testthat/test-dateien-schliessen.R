# Geladene Dateien schliessen: Kuerzel in der Sidebar, x je Datei,
# "Alle schliessen" mit Bestaetigung.

test_that("das Kuerzel einer geladenen Datei passt in die Sidebar", {
  name <- "C-Test_Auswertung_2024-10-29_5c.tsv"

  expect_equal(datei_kuerzel("5c", name), "29.10.24 - 5c")
  # gleiche Stufe: nur das erste Kuerzel traegt die Stufenzahl
  expect_equal(datei_kuerzel(c("5c", "5d", "5e"), name), "29.10.24 - 5c,d,e")
  expect_equal(datei_kuerzel(c("5e", "5c", "5d"), name), "29.10.24 - 5c,d,e")
  expect_equal(datei_kuerzel(c("5c", "5d", "5e", "5f"), name), "29.10.24 - 5c, +3")
  # zwei Stufen: beide mit Stufenzahl, mehr passt nicht hinein
  expect_equal(datei_kuerzel(c("5c", "6c"), name), "29.10.24 - 5c, 6c")
  expect_equal(datei_kuerzel(c("5c", "6c", "7c"), name), "29.10.24 - 5c, +2")

  # Sonderfaelle: ohne Klassenangabe, ohne Datum, Datum in deutscher Form
  expect_equal(datei_kuerzel(character(0), name), "29.10.24 - ohne Kl.")
  expect_equal(datei_kuerzel("5c", "meine_datei.tsv"), "5c")
  expect_equal(datei_kuerzel("5c", "C-Test_Auswertung_29.10.2024_5c.tsv"),
               "29.10.24 - 5c")

  # Kennzahlen der Briefe tragen die Herkunftsspalte nicht
  df <- lade_fixture("klasse_5c.tsv")
  df[[.quelle_spalte]] <- "d1"
  expect_false(.quelle_spalte %in% colnames(stand_statistik(df)))

  # vollstaendige Beschriftung fuer Tooltip und Statusmeldung
  expect_equal(beschriftung_datei(c("6c", "5c"), "datei.tsv"), "datei.tsv (5c, 6c)")
  expect_equal(beschriftung_datei(character(0), "datei.tsv"), "datei.tsv (ohne Klassenangabe)")
})

test_that("der Bestaetigungsdialog bietet Sichern nur bei Aenderungen", {
  mit <- paste(as.character(.schliessen_dialog(TRUE)), collapse = "")
  ohne <- paste(as.character(.schliessen_dialog(FALSE)), collapse = "")

  expect_match(mit, "btSchliessenSichern", fixed = TRUE)
  expect_false(grepl("btSchliessenSichern", ohne, fixed = TRUE))
  for (html in list(mit, ohne)) {
    expect_match(html, "btSchliessenNur", fixed = TRUE)
    expect_match(html, "Abbrechen", fixed = TRUE)
    expect_match(html, "Daten schließen?", fixed = TRUE)
  }
  expect_match(mit, "noch nicht gesichert", fixed = TRUE)
  expect_match(ohne, "bereits gesichert", fixed = TRUE)
})

test_that("eine geladene Datei laesst sich schliessen", {
  shiny::testServer(server, {
    session$setInputs(numItems = "40", klassenstufe = "5", klBuchstabe = "c",
                      cbWEDiff = FALSE, cbAllCombined = TRUE, siPlotType = "Histogramm")
    ohne_bekannte_warnungen(session$setInputs(input_tsv = tsv_input("klasse_5c.tsv")))
    ohne_bekannte_warnungen(session$setInputs(input_tsv = tsv_input("klasse_6c.tsv")))

    # jede Ladung bekommt einen eigenen Schluessel
    expect_length(rv$dateien, 2)
    expect_setequal(vapply(rv$dateien, function(d) d$schluessel, ""), c("d1", "d2"))
    expect_setequal(unique(rv$df[[.quelle_spalte]]), c("d1", "d2"))
    expect_equal(rv$dateien[[1]]$klassen, "5c")
    # die Herkunft ist in der Uebersichtstabelle nicht sichtbar
    expect_false(grepl(.quelle_spalte, als_text(output$tabUebersicht), fixed = TRUE))

    # x der ersten Datei: nur ihre Zeilen verschwinden
    session$setInputs(btDateiSchliessen_d1 = 1)

    expect_length(rv$dateien, 1)
    expect_equal(unique(rv$df$Klasse), "6c")
    expect_false("d1" %in% rv$df[[.quelle_spalte]])
    expect_match(als_text(output$text), "geschlossen", fixed = TRUE)

    # die letzte Datei schliessen: Ausgangszustand wie nach dem Start
    session$setInputs(btDateiSchliessen_d2 = 1)
    expect_equal(nrow(rv$df), 0)
    expect_true(rv$inital)
    expect_length(rv$dateien, 0)
  })
})

test_that("Handeingaben bleiben beim Schliessen stehen", {
  shiny::testServer(server, {
    session$setInputs(numItems = "40", klassenstufe = "5", klBuchstabe = "c",
                      cbWEDiff = FALSE, cbAllCombined = TRUE, siPlotType = "Histogramm")
    ohne_bekannte_warnungen(
      session$setInputs(schuelerName = "Testmann, Anna", weWert = 34, rfWert = 28,
                        btHinzufuegen = 1))
    expect_equal(rv$df[[.quelle_spalte]], "manuell")

    # Datei dazu, dann wieder schliessen: die Handeingabe bleibt
    ohne_bekannte_warnungen(session$setInputs(input_tsv = tsv_input("klasse_5c.tsv")))
    expect_equal(nrow(rv$df), 10)
    session$setInputs(btDateiSchliessen_d1 = 1)

    expect_equal(nrow(rv$df), 1)
    expect_equal(rv$df$Name, "Testmann, Anna")
    expect_false(rv$inital)
    expect_length(rv$dateien, 0)
  })
})

test_that("Alle schliessen setzt die App in den Ausgangszustand", {
  shiny::testServer(server, {
    session$setInputs(numItems = "40", klassenstufe = "5", klBuchstabe = "c",
                      cbWEDiff = FALSE, cbAllCombined = TRUE, siPlotType = "Histogramm")
    ohne_bekannte_warnungen(session$setInputs(input_tsv = tsv_input("klasse_5c.tsv")))
    ohne_bekannte_warnungen(session$setInputs(input_tsv = tsv_input("klasse_6c.tsv")))

    expect_no_error(session$setInputs(btAlleSchliessen = 1))
    session$setInputs(btSchliessenNur = 1)

    expect_equal(nrow(rv$df), 0)
    expect_true(rv$inital)
    expect_length(rv$dateien, 0)
    expect_match(als_text(output$text), "Alle Daten geschlossen", fixed = TRUE)
  })
})

test_that("die Herkunftsspalte landet nicht in den Dateien", {
  withr::with_tempdir({
    file.copy(file.path(projekt_root, "template.docx"), "template.docx")
    dir.create("Auswertungen")
    sicherungsstand_zuruecksetzen()

    df <- lade_fixture("klasse_5c.tsv")
    df[[.quelle_spalte]] <- "d1"
    saveData(df)

    # die tsv ist das Datenformat: Itemzahl ja, Herkunft nein
    tsv <- list.files("Auswertungen", pattern = "\\.tsv$", full.names = TRUE)[1]
    spalten <- strsplit(readLines(tsv, n = 1, warn = FALSE), "\t")[[1]]
    expect_false(.quelle_spalte %in% spalten)
    expect_true(.items_spalte %in% spalten)

    # Excel-Bericht ebenso
    xlsx <- list.files("Auswertungen", pattern = "\\.xlsx$", full.names = TRUE)[1]
    expect_false(is.na(xlsx))
    expect_false(.quelle_spalte %in% colnames(readxl::read_excel(xlsx)))

    # und die Sicherung (tsv) ebenfalls nicht
    datei <- sichere_daten(df)$datei
    if (!is.na(datei)) {
      spalten2 <- strsplit(readLines(datei, n = 1, warn = FALSE), "\t")[[1]]
      expect_false(.quelle_spalte %in% spalten2)
    }
  })
})

test_that("stand_geaendert erkennt Aenderungen gegenueber der Sicherung", {
  df <- lade_fixture("klasse_5c.tsv")
  sicherungsstand_zuruecksetzen()
  merke_gesicherten_stand(df)

  expect_false(stand_geaendert(df))
  expect_false(stand_geaendert(df[0, , drop = FALSE]))
  expect_false(stand_geaendert(NULL))

  df$`R/F-%`[1] <- df$`R/F-%`[1] + 5
  expect_true(stand_geaendert(df))
})
