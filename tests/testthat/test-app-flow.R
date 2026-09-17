# App-Ablaeufe ohne Browser (shiny::testServer): Schueler anlegen, loeschen,
# tsv laden, Statistik zeichnen. Deckt die Observer in server.R ab.
#
# Wichtig: die Eingaben der UI (Checkboxen, Diagrammtyp) werden vor dem Laden
# der Daten gesetzt, weil sie in der echten App ebenfalls von Anfang an
# existieren. In testServer sind nicht gesetzte Eingaben NULL.

test_that("Schueler hinzufuegen legt einen Eintrag in der Tabelle an", {
  shiny::testServer(server, {
    session$setInputs(numItems = "40", klassenstufe = "5", klBuchstabe = "c",
                      cbWEDiff = FALSE, cbAllCombined = TRUE, siPlotType = "Histogramm")
    session$setInputs(schuelerName = "Testmann, Anna", weWert = 34, rfWert = 28)
    session$setInputs(btHinzufuegen = 1)

    expect_equal(nrow(rv$df), 1)
    expect_equal(rv$df$Name, "Testmann, Anna")
    expect_equal(rv$df$Klasse, "5c")
    expect_equal(rv$df$`WE-%`, 85)
    expect_equal(rv$df$`R/F-%`, 70)
    expect_equal(as.character(rv$df$`Kat.`), "2B")
    expect_equal(rv$numItems, 40)
    expect_false(rv$inital)
    expect_false(is.null(output$tabUebersicht))
    # nach erfolgreicher Eingabe keine Fehlermeldung
    expect_equal(als_text(output$text), "")
    # Hinweis: das Zuruecksetzen der Felder (updateTextInput) ist mit testServer
    # nicht pruefbar - dabei wird nur eine Nachricht an den Browser geschickt,
    # input$... bleibt im Mock unveraendert.
  })
})

test_that("fehlerhafte Eingaben werden gemeldet und nichts eingetragen", {
  shiny::testServer(server, {
    session$setInputs(numItems = "40", klassenstufe = "5", klBuchstabe = "c",
                      cbWEDiff = FALSE, cbAllCombined = TRUE, siPlotType = "Histogramm")

    # ohne Namen
    session$setInputs(schuelerName = "", weWert = 30, rfWert = 25, btHinzufuegen = 1)
    expect_equal(nrow(rv$df), 0)
    expect_match(als_text(output$text), "Bitte Namen eingeben.", fixed = TRUE)

    # R/F-Wert groesser als WE-Wert
    session$setInputs(schuelerName = "Test, Tina", weWert = 20, rfWert = 25, btHinzufuegen = 2)
    expect_equal(nrow(rv$df), 0)
    expect_match(als_text(output$text), "R/F-Wert kann nicht größer als WE-Wert sein.", fixed = TRUE)

    # Wert oberhalb der Anzahl Test-Items
    session$setInputs(weWert = 45, rfWert = 41, btHinzufuegen = 3)
    expect_equal(nrow(rv$df), 0)
    expect_match(als_text(output$text), "kann nicht höher als Anzahl Test-Items sein.", fixed = TRUE)

    # ohne Klassenangabe
    session$setInputs(klassenstufe = "", klBuchstabe = "", weWert = 30, rfWert = 25, btHinzufuegen = 4)
    expect_equal(nrow(rv$df), 0)
    expect_match(als_text(output$text), "Bitte Klassenstufe und Klasse angeben.", fixed = TRUE)
  })
})

test_that("markierte Schueler lassen sich wieder entfernen", {
  shiny::testServer(server, {
    session$setInputs(numItems = "40", klassenstufe = "5", klBuchstabe = "c",
                      cbWEDiff = FALSE, cbAllCombined = TRUE, siPlotType = "Histogramm")
    session$setInputs(schuelerName = "Testmann, Anna", weWert = 34, rfWert = 28, btHinzufuegen = 1)
    session$setInputs(schuelerName = "Beispiel, Ben", weWert = 28, rfWert = 22, btHinzufuegen = 2)
    expect_equal(nrow(rv$df), 2)

    session$setInputs(tabUebersicht_rows_selected = 1L)
    session$setInputs(btEntfernen = 1)
    expect_equal(nrow(rv$df), 1)
    expect_equal(rv$df$Name, "Beispiel, Ben")
  })
})

test_that("tsv-Dateien lassen sich laden und ergaenzen (Jahresvergleich)", {
  shiny::testServer(server, {
    session$setInputs(numItems = "40", klassenstufe = "5", klBuchstabe = "c",
                      cbWEDiff = FALSE, cbAllCombined = TRUE, siPlotType = "Histogramm")

    ohne_bekannte_warnungen(session$setInputs(input_tsv = tsv_input("klasse_5c.tsv")))
    expect_equal(nrow(rv$df), 9)
    expect_false(rv$inital)

    ohne_bekannte_warnungen(session$setInputs(input_tsv = tsv_input("klasse_6c.tsv")))
    expect_equal(nrow(rv$df), 18)
    expect_setequal(unique(rv$df$Klasse), c("5c", "6c"))
  })
})

test_that("Speichern und Elternbriefe brechen bei leerer Tabelle sauber ab", {
  shiny::testServer(server, {
    session$setInputs(numItems = "40", klassenstufe = "5", klBuchstabe = "c",
                      cbWEDiff = FALSE, cbAllCombined = TRUE, siPlotType = "Histogramm")

    expect_no_error(session$setInputs(btSpeichern = 1))
    expect_no_error(session$setInputs(btBrief = 1))
    expect_equal(nrow(rv$df), 0)

    # Daten vorhanden, aber kein Lehrername: Abbruch vor dem Rendern
    ohne_bekannte_warnungen(session$setInputs(input_tsv = tsv_input("klasse_5c.tsv")))
    expect_no_error(session$setInputs(lehrername = "", btBrief = 1))
    expect_equal(nrow(rv$df), 9)
  })
})

test_that("Statistik zeichnet Verteilungen und Kennzahlen", {
  shiny::testServer(server, {
    session$setInputs(numItems = "40", klassenstufe = "5", klBuchstabe = "c",
                      cbWEDiff = FALSE, cbAllCombined = TRUE, siPlotType = "Histogramm")
    for (datei in c("klasse_5c.tsv", "klasse_6c.tsv")) {
      ohne_bekannte_warnungen(session$setInputs(input_tsv = tsv_input(datei)))
    }
    expect_equal(nrow(rv$df), 18)

    expect_false(is.null(output$histRF))
    expect_false(is.null(output$histWE))
    expect_gt(nchar(als_text(output$histRF)), 50)
    expect_match(als_text(output$statsRF), "Mittelwert R/F:", fixed = TRUE)
    expect_match(als_text(output$statsWE), "Mittelwert WE:", fixed = TRUE)
    expect_match(als_text(output$dynamicText), "Gestrichelte Linie", fixed = TRUE)

    # Diagrammtyp "Dichte"
    ohne_bekannte_warnungen(session$setInputs(siPlotType = "Dichte"))
    expect_gt(nchar(als_text(output$histRF)), 50)

    # Checkbox "Differenz": linkes Diagramm zeigt WE - R/F
    ohne_bekannte_warnungen(session$setInputs(siPlotType = "Histogramm", cbWEDiff = TRUE))
    expect_gt(nchar(als_text(output$histWE)), 50)

    # Gesamtuebersicht aus: je Klasse zeichnen
    ohne_bekannte_warnungen(session$setInputs(cbWEDiff = FALSE, cbAllCombined = FALSE))
    expect_gt(nchar(als_text(output$histRF)), 50)
  })
})

test_that("Diagrammtyp Entwicklung zeichnet beide Diagramme", {
  shiny::testServer(server, {
    session$setInputs(numItems = "40", klassenstufe = "5", klBuchstabe = "c",
                      cbWEDiff = FALSE, cbAllCombined = TRUE, siPlotType = "Histogramm")
    for (datei in c("klasse_5c.tsv", "klasse_6c.tsv")) {
      ohne_bekannte_warnungen(session$setInputs(input_tsv = tsv_input(datei)))
    }

    ohne_bekannte_warnungen(session$setInputs(siPlotType = "Entwicklung"))
    expect_gt(nchar(als_text(output$histRF)), 50)
    expect_gt(nchar(als_text(output$histWE)), 50)
    # ohne Referenzlinien im Entwicklungsdiagramm
    expect_false(isTRUE(grepl("Gestrichelte", als_text(output$dynamicText))))
  })
})

test_that("Schreibfehler werden in der App gemeldet (kein stiller Abbruch)", {
  withr::with_tempdir({
    # Ausgabeordner unbrauchbar machen: "blocker" ist eine Datei
    writeLines("blockiert", "blocker")
    withr::with_options(list(ctest.outdir = file.path(getwd(), "blocker", "x")), {
      shiny::testServer(server, {
        session$setInputs(numItems = "40", klassenstufe = "5", klBuchstabe = "c",
                          cbWEDiff = FALSE, cbAllCombined = TRUE, siPlotType = "Histogramm")
        session$setInputs(schuelerName = "Testmann, Anna", weWert = 34, rfWert = 28,
                          btHinzufuegen = 1)
        expect_equal(nrow(rv$df), 1)

        # Speichern
        expect_no_error(session$setInputs(btSpeichern = 1))
        expect_match(als_text(output$text), "Fehler beim Speichern", fixed = TRUE)
        expect_match(als_text(output$text), "kann nicht angelegt werden", fixed = TRUE)

        # Elternbriefe
        session$setInputs(lehrername = "Test, Tina", btBrief = 1)
        expect_match(als_text(output$text), "Fehler beim Erstellen der Elternbriefe", fixed = TRUE)
        expect_match(als_text(output$text), "kann nicht angelegt werden", fixed = TRUE)
      })
    })
  })
})

test_that("der Stufenvergleich fuellt Auswahl, Tabelle und Status", {
  withr::with_tempdir({
    # Ausgabeordner im Temp-Verzeichnis, damit nichts im Projekt landet
    shiny::testServer(server, {
      session$setInputs(numItems = "40", klassenstufe = "5", klBuchstabe = "c",
                        cbWEDiff = FALSE, cbAllCombined = TRUE, siPlotType = "Histogramm",
                        infoKlassenleitung = "6c", infoAbsender = "Test, Tina")

      # ohne zweite Stufe ist der Vergleich gesperrt
      ohne_bekannte_warnungen(session$setInputs(input_tsv = tsv_input("klasse_5c.tsv")))
      expect_equal(nrow(rv$df), 9)
      expect_false(auswahl_moeglich())
      expect_match(als_text(output$vergleichHinweis), "zweite tsv-Datei", fixed = TRUE)

      # zweite Stufe laden -> Vergleich moeglich
      ohne_bekannte_warnungen(session$setInputs(input_tsv = tsv_input("klasse_6c.tsv")))
      expect_equal(nrow(rv$df), 18)
      expect_true(auswahl_moeglich())
      expect_equal(stufen_verfuegbar(), c(5, 6))

      # Stufenpaar setzen (in testServer loest updateSelectInput die Eingabe nicht aus)
      session$setInputs(siStufeAlt = 5, siStufeNeu = 6)
      k <- cohort_daten()
      expect_equal(k$stufe_alt, 5)
      expect_equal(k$stufe_neu, 6)
      expect_gt(nrow(cohort_gematcht(k)), 5)

      # Tabelle und Status werden gefuellt
      expect_false(is.null(output$tabZuordnung))
      status <- als_text(output$zuordnungStatus)
      expect_match(status, "Kinder sind zugeordnet", fixed = TRUE)
      # im Normalfall steht kein Hinweis mehr - das Stufenpaar zeigen die Felder,
      # das Diagramm und der Infobrief selbst
      expect_equal(trimws(als_text(output$vergleichHinweis)), "")
      expect_match(als_text(output$infobriefHinweis), "bestätigten Zuordnungen",
                   fixed = TRUE)

      # eine Zuordnung bestaetigen: Entscheidung wird gespeichert
      vorschlaege <- which(k$paare$Status == "vorschlag")
      if (length(vorschlaege) > 0) {
        session$setInputs(tabZuordnung_rows_selected = vorschlaege[1])
        session$setInputs(btZuordnungJa = 1)
        expect_true(any(rv$entscheidungen$Aktion == "ja"))
        erwartete_datei <- entscheidungen_pfad(k)
        expect_true(file.exists(erwartete_datei))
        expect_match(basename(erwartete_datei), "^zuordnung_5c-6c\\.tsv$")
        # Herkunft und Pruefsumme stehen in der Datei
        gespeichert <- readr::read_tsv(erwartete_datei, show_col_types = FALSE)
        expect_true(all(c("Quelle_Alt", "Quelle_Neu", "Kohorte") %in%
                          colnames(gespeichert)))
        expect_equal(unique(gespeichert$Kohorte), kohorte_pruefsumme(k))

        # zuruecksetzen loescht die Datei
        session$setInputs(btZuordnungResetBestaetigt = 1)
        expect_false(file.exists(erwartete_datei))
        expect_null(rv$entscheidungen)
      }

      # Vergleichstabelle im Statistik-Tab
      expect_false(is.null(output$tabVergleich))
      hinweis <- als_text(output$vergleichTabHinweis)
      expect_match(hinweis, "Kinder in der Liste", fixed = TRUE)
      expect_match(hinweis, "in beiden Jahrgängen zugeordnet", fixed = TRUE)

      # Entwicklungsdiagramm nutzt dieselbe Zuordnung
      ohne_bekannte_warnungen(session$setInputs(siPlotType = "Entwicklung"))
      expect_gt(nchar(als_text(output$histRF)), 50)
      expect_gt(nchar(als_text(output$histWE)), 50)
    })
  })
})
