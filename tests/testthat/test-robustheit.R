# Paket K: robustes Laden und robuste Anzeige.
#
# Die App darf an einer unerwarteten tsv-Datei nicht scheitern: fehlende oder
# anders benannte Spalten werden ergaenzt und gemeldet, fehlende Kategorien aus
# den Prozentwerten berechnet. Unbekannte Spalten werden ignoriert und genannt.

test_that("tsv ohne Kategorie-Spalte laedt und berechnet sie aus den Werten", {
  df <- lade_fixture("ohne_kategorie.tsv")

  expect_equal(colnames(df),
               c("Name", "Klasse", "WE-Wert", "WE-%", "R/F-Wert", "R/F-%",
                 "Kat.", "Empfehlung", "Items"))
  expect_equal(nrow(df), 4)
  # Anna: WE 85 %, R/F 70 % -> Stufe 2, Unterschied 15 -> B
  expect_equal(df$`Kat.`[df$Name == "Testmann, Anna"], "2B")
  expect_match(df$Empfehlung[df$Name == "Testmann, Anna"],
               "oberhalb des Normbereichs", fixed = TRUE)
  # Clara: WE 60 %, R/F 35 % -> Stufe 5, Unterschied 25, WE unter 65 -> E
  expect_equal(df$`Kat.`[df$Name == "Muster, Clara"], "5E")
  # nicht teilgenommen bleibt "0"
  expect_equal(df$`Kat.`[df$Name == "Probst, Ella"], "0")
  expect_equal(df$Empfehlung[df$Name == "Probst, Ella"], "Hat nicht teilgenommen.")
  # die fehlenden Spalten werden gemeldet
  hinweise <- lade_hinweise(df)
  expect_true(any(grepl("'Kat.' fehlt", hinweise)))
  expect_true(any(grepl("Kategorie fuer 4", hinweise)))
})

test_that("andere Spaltenreihenfolge aendert die Werte und Typen nicht", {
  # Frueher wurden die Spaltentypen nach Position zugeordnet - bei anderer
  # Reihenfolge waren sie still falsch.
  df <- lade_fixture("spalten_anders.tsv")

  expect_equal(df$`WE-%`, c(85, 70, NA))
  expect_equal(df$`R/F-%`, c(70, 55, NA))
  expect_equal(df$`WE-Wert`, c(34, 28, NA))
  expect_type(df$`WE-%`, "double")
  expect_equal(df$`Kat.`, c("2B", "4C", "0"))
  # einziger Hinweis: die Itemzahl wurde aus Wert und Prozentwert ermittelt
  hinweise <- lade_hinweise(df)
  expect_length(hinweise, 1)
  expect_match(hinweise, "Itemzahl fuer 2 Kind(er) aus Wert und Prozentwert ermittelt",
               fixed = TRUE)
})

test_that("gewoehnliche Schreibweisen der Spaltennamen werden erkannt", {
  df <- lade_fixture("andere_schreibweise.tsv")

  expect_equal(df$Name, c("Testmann, Anna", "Beispiel, Ben"))
  expect_equal(df$`WE-%`, c(85, 70))
  expect_equal(df$`R/F-%`, c(70, 55))
  expect_equal(df$`Kat.`, c("2B", "4C"))
  # die Umbenennungen stehen in den Hinweisen
  hinweise <- paste(lade_hinweise(df), collapse = " ")
  expect_match(hinweise, "'Sch\u00fcler' als 'Name' gelesen", fixed = TRUE)
  expect_match(hinweise, "'WE %' als 'WE-%' gelesen", fixed = TRUE)
})

test_that("tsv mit nur einer Spalte laedt mit Hinweisen", {
  df <- lade_fixture("nur_name.tsv")

  expect_equal(df$Name, c("Testmann, Anna", "Beispiel, Ben"))
  expect_true(all(is.na(df$`WE-%`)))
  expect_true(all(is.na(df$`Kat.`)))
  # Klasse bleibt leer (wie im Altformat), nicht NA
  expect_equal(df$Klasse, c("", ""))
  hinweise <- paste(lade_hinweise(df), collapse = " ")
  expect_match(hinweise, "'WE-%' fehlt", fixed = TRUE)
  expect_match(hinweise, "'Empfehlung' fehlt", fixed = TRUE)
})

test_that("unbekannte Spalten werden gemeldet, aber nicht verwendet", {
  df <- lade_fixture("falsche_spalten.tsv")

  expect_equal(df$Name, "Test, Tina")
  expect_equal(df$Klasse, "5c")
  expect_false("Quatsch" %in% colnames(df))
  expect_true(any(grepl("Nicht verwendet: Quatsch", lade_hinweise(df))))
})

test_that("ohne Spalte Name gibt es eine klare Meldung", {
  fehler <- tryCatch(lade_fixture("ohne_name.tsv"), error = function(e) conditionMessage(e))
  expect_match(fehler, "fehlt die Spalte 'Name'", fixed = TRUE)
  expect_match(fehler, "Gefundene Spalten", fixed = TRUE)
})

test_that("unlesbare Werte werden NA statt Fehler", {
  pfad <- tempfile(fileext = ".tsv")
  writeLines(c("Name\tKlasse\tWE-Wert\tWE-%\tR/F-Wert\tR/F-%\tKat.\tEmpfehlung",
               "Test, Tina\t5c\tabc\t85,5\t28\t70\t2B\tText"), pfad)
  df <- loadData(list(datapath = pfad, name = basename(pfad), size = file.size(pfad),
                      type = "text/tab-separated-values"))

  # "abc" ist keine Zahl: der Wert bleibt leer, das Laden bricht nicht ab.
  # Aus dem R/F-Paar (28/70) kommt die Itemzahl 40, damit wird der WE-Wert
  # wieder ergaenzt - 85,5 % von 40 sind 34.
  # deutsches Komma wird verstanden
  expect_equal(df$`WE-%`[1], 85.5)
  expect_equal(df$Items[1], 40)
  expect_equal(df$`WE-Wert`[1], 34)
  unlink(pfad)
})

test_that("die Anzeige bricht bei fehlenden Spalten nicht ab", {
  df <- lade_fixture("nur_name.tsv")

  # Kennzahlen: Hinweis statt Fehler, auch je Klasse
  expect_no_error(createStatsText(df, "WE-%", "WE"))
  expect_no_error(createStatsText(df, "WE-%", "WE", multiple = TRUE))
  # Diagramm: Hinweis statt Fehler (auch ohne Kategorie-Spalte)
  p <- createPlot(df, x = "WE-%", fill = "Kat.", xlab = "WE-Wert in %")
  expect_s3_class(p, "ggplot")
  p2 <- createPlot(df, x = "R/F-%", fill = "Kat.", xlab = "R/F-Wert in %")
  expect_s3_class(p2, "ggplot")
  # Tabelle ohne Kategorie-Spalte bleibt unveraendert
  einfach <- data.frame(Name = "Test, Tina")
  expect_identical(styleTable(einfach), einfach)
})

test_that("eine Datei ohne Werte laesst sich laden und anzeigen", {
  # Regression: frueher brach das Laden ab, weil read_tsv die Spalten suchte
  df <- lade_fixture("falsche_spalten.tsv")
  expect_no_error(createPlot(df, x = "WE-%", fill = "Kat.", xlab = "WE-Wert in %"))
  expect_no_error(createStatsText(df, "WE-%", "WE"))
  # "keine Werte vorhanden" statt NaN
  expect_match(as.character(createStatsText(df, "WE-%", "WE")), "keine Werte vorhanden",
               fixed = TRUE)
})

test_that("die App laedt eine Datei mit fehlenden Spalten ohne Fehler", {
  withr::with_tempdir({
    shiny::testServer(server, {
      session$setInputs(numItems = "40", klassenstufe = "5", klBuchstabe = "c",
                        cbWEDiff = FALSE, cbAllCombined = TRUE, siPlotType = "Histogramm")
      ohne_bekannte_warnungen(session$setInputs(input_tsv = tsv_input("ohne_kategorie.tsv")))

      expect_equal(nrow(rv$df), 4)
      expect_true(all(c("Kat.", "Empfehlung") %in% colnames(rv$df)))
      # die Oberflaeche meldet, was ergaenzt wurde
      expect_match(als_text(output$text), "Kategorie", fixed = TRUE)
    })
  })
})

test_that("die App meldet eine Datei ohne Namen als Fehler und laeuft weiter", {
  withr::with_tempdir({
    shiny::testServer(server, {
      session$setInputs(numItems = "40", klassenstufe = "5", klBuchstabe = "c",
                        cbWEDiff = FALSE, cbAllCombined = TRUE, siPlotType = "Histogramm")
      expect_no_error(
        ohne_bekannte_warnungen(session$setInputs(input_tsv = tsv_input("ohne_name.tsv"))))
      # nichts geladen, aber die Sitzung lebt
      expect_true(rv$inital)
      expect_match(als_text(output$text), "fehlt die Spalte 'Name'", fixed = TRUE)
    })
  })
})

#### Paket B: Spaltenzuordnung ueber Merkmale ####

test_that("das snake_case-Schema von 2023 wird vollstaendig zugeordnet", {
  # klasse, name, r_f_wert, r_f_percent, we_wert, we_percent, kat, empfehlung
  df <- lade_fixture("snake_percent.tsv")

  expect_equal(df$Name, c("Testmann, Anna", "Beispiel, Ben", "Muster, Clara",
                          "Probst, Ella"))
  expect_equal(df$Klasse, rep("5c", 4))
  expect_equal(df$`WE-%`, c(85, 70, 60, NA))
  expect_equal(df$`R/F-%`, c(70, 55, 35, NA))
  expect_equal(df$`WE-Wert`, c(34, 28, 24, NA))
  expect_equal(df$`Kat.`, c("2B", "4C", "5E", "0"))
  # keine Spalte bleibt liegen, nichts wird vermisst
  hinweise <- paste(lade_hinweise(df), collapse = " ")
  expect_false(grepl("Nicht verwendet", hinweise))
  expect_false(grepl("fehlt in der Datei", hinweise))
  expect_match(hinweise, "'we_percent' als 'WE-%' gelesen", fixed = TRUE)
  expect_match(hinweise, "'r_f_percent' als 'R/F-%' gelesen", fixed = TRUE)
})

test_that("englische Schreibweisen mit percentage werden erkannt", {
  df <- lade_fixture("variante_englisch.tsv")

  expect_equal(df$`WE-Wert`, c(34, 28))
  expect_equal(df$`WE-%`, c(85, 70))
  expect_equal(df$`R/F-%`, c(70, 55))
  expect_equal(df$`Kat.`, c("2B", "4C"))
  # keine Spalte bleibt liegen (Kat./Empfehlung fehlen in der Datei und werden
  # berechnet - das ist kein Zuordnungsproblem)
  hinweise <- paste(lade_hinweise(df), collapse = " ")
  expect_false(grepl("Nicht verwendet", hinweise))
  # die Zuordnung wird benannt - eine Umbenennung ist kein Fehler
  expect_match(hinweise, "'vocabulary_percentage' als 'WE-%' gelesen", fixed = TRUE)
})

test_that("ein nichtssagender Spaltenname wird nicht als Wertspalte verwendet", {
  # "Bewertung" enthaelt die Buchstaben "we", aber nicht das Wort
  df <- lade_fixture("falle_bewertung.tsv")

  expect_equal(df$`WE-%`[1], 85)
  expect_equal(df$`R/F-Wert`[1], 28)
  # der WE-Wert kommt aus dem Prozentwert (85 % von 40 Items), nicht aus
  # "Bewertung" (das "sehr gut" enthaelt)
  expect_equal(df$`WE-Wert`[1], 34)
  hinweise <- paste(lade_hinweise(df), collapse = " ")
  expect_match(hinweise, "Nicht verwendet: Bewertung", fixed = TRUE)
  expect_match(hinweise, "Spalte 'WE-Wert' fehlt", fixed = TRUE)
})

test_that("bei mehreren Kandidaten gewinnt der exakte Name", {
  df <- lade_fixture("konflikt.tsv")

  expect_equal(df$`WE-%`, c(85, 70))      # nicht die 99 aus we_percent
  hinweise <- paste(lade_hinweise(df), collapse = " ")
  expect_match(hinweise, "Nicht verwendet: we_percent", fixed = TRUE)
})

test_that("mehrdeutige Kandidaten in derselben Stufe werden gemeldet", {
  # zwei Spalten, die beide nur ueber Merkmale als Prozent + WE erkennbar sind
  # (WE_Prozent waere dagegen ein exakter Treffer und damit eindeutig)
  roh <- data.frame(Name = "Test, Tina", Klasse = "5c", we_percent = "85",
                    we_percentage = "70", check.names = FALSE)
  df <- pruefe_schuelerdaten(roh)

  hinweise <- paste(lade_hinweise(df), collapse = " ")
  expect_match(hinweise, "Mehrere Kandidaten fuer 'WE-%'", fixed = TRUE)
  expect_equal(df$`WE-%`, 85)              # die erste Spalte gewinnt
  expect_match(hinweise, "Nicht verwendet: we_percentage", fixed = TRUE)
})

#### Paket B: Itemzahl, Nachrechnen, Plausibilitaet ####

test_that("fehlende Prozentwerte werden aus Wert und Itemzahl berechnet", {
  df <- lade_fixture("items_ohne_prozente.tsv")

  expect_equal(df$`WE-%`, c(85, 70, NA))
  expect_equal(df$`R/F-%`, c(70, 55, NA))
  expect_equal(df$`WE-Wert`, c(34, 28, NA))     # Rohwerte bleiben unveraendert
  expect_equal(df$Items, rep(40, 3))
  hinweise <- paste(lade_hinweise(df), collapse = " ")
  expect_match(hinweise, "'WE-%' fuer 2 Kind(er) aus Wert und Itemzahl berechnet",
               fixed = TRUE)
  expect_match(hinweise, "'R/F-%' fuer 2 Kind(er) aus Wert und Itemzahl berechnet",
               fixed = TRUE)
})

test_that("fehlende Rohwerte werden aus Prozentwert und Itemzahl ergaenzt", {
  df <- lade_fixture("items_ohne_rohwerte.tsv")

  # 85 % von 40 = 34, 70 % von 40 = 28, 60 % von 40 = 24
  expect_equal(df$`WE-Wert`, c(34, 28, 24))
  expect_equal(df$`R/F-Wert`, c(28, 22, 14))
  hinweise <- paste(lade_hinweise(df), collapse = " ")
  expect_match(hinweise, "aus Prozentwert und Itemzahl ergaenzt", fixed = TRUE)
  # und daraus entsteht die Kategorie
  expect_equal(df$`Kat.`, c("2B", "4C", "5E"))
})

test_that("passt die Itemzahl nicht zu den Werten, wird nicht nachgerechnet", {
  df <- lade_fixture("items_falsch.tsv")

  # 34 von 20 Items waeren 170 % - die Prozente passen zu 40 Items
  hinweise <- paste(lade_hinweise(df), collapse = " ")
  expect_match(hinweise, "Itemzahl passt nicht zu den Werten", fixed = TRUE)
  expect_match(hinweise, "'WE-%' passt bei 2 Kind(ern) nicht zu Wert und Itemzahl",
               fixed = TRUE)
  # die Werte der Datei bleiben, wie sie sind
  expect_equal(df$`WE-%`, c(85, 70))
})

test_that("ohne pruefbare Itemzahl werden keine Rohwerte erfunden", {
  # Prozente liegen nicht auf dem Raster der Itemzahl (20 Items -> 5er-Schritte)
  df <- lade_fixture("items_raster_falsch.tsv")

  expect_true(all(is.na(df$`WE-Wert`)))
  expect_true(all(is.na(df$`R/F-Wert`)))
  hinweise <- paste(lade_hinweise(df), collapse = " ")
  expect_false(grepl("aus Prozentwert und Itemzahl ergaenzt", hinweise))
})

test_that("Prozentwerte ausserhalb 0 bis 100 werden verworfen und nachgerechnet", {
  df <- lade_fixture("prozent_unplausibel.tsv")

  # Die WE-%-Spalte enthaelt 850 - die ganze Spalte wird verworfen. Aus den
  # Rohwerten und der Itemzahl (aus dem R/F-Paar: 28/70 -> 40) wird sie korrekt
  # neu berechnet: 34 von 40 Items sind 85 %, 28 von 40 sind 70 %.
  expect_equal(df$`WE-%`, c(85, 70))
  expect_equal(df$`R/F-%`, c(70, 55))
  expect_equal(df$Items, c(40, 40))
  # die Kategorie entsteht daraus wieder (und kein Unsinn wie "2NA")
  # Ben: R/F 55 %, WE 70 % -> Stufe 4, Unterschied 15, WE ueber 65 -> C
  expect_equal(df$`Kat.`, c("2B", "4C"))
  hinweise <- paste(lade_hinweise(df), collapse = " ")
  expect_match(hinweise, "ausserhalb 0 bis 100", fixed = TRUE)
  expect_match(hinweise, "aus Wert und Itemzahl berechnet", fixed = TRUE)
})

test_that("WE kleiner als R/F wird gemeldet", {
  df <- lade_fixture("we_kleiner_rf.tsv")

  hinweise <- paste(lade_hinweise(df), collapse = " ")
  expect_match(hinweise, "WE-% kleiner als R/F-%", fixed = TRUE)
  # gemeldet, aber nicht veraendert
  expect_equal(df$`WE-%`[1], 60)
  expect_equal(df$`R/F-%`[1], 85)
})

test_that("die Itemzahl wird je Zeile aus Wert und Prozentwert zurueckgerechnet", {
  # Eine Datei mit zwei Klassen und unterschiedlicher Textlaenge: eine
  # dateiweite Itemzahl waere hier falsch.
  df <- lade_fixture("items_rueckrechnung.tsv")

  # 5c rechnet mit 40 Items, 6c mit 26 - das Nicht-Teilnehmerkind bleibt leer
  expect_equal(df$Items, c(40, 40, 26, 26, NA))
  # die Werte der Datei bleiben unveraendert
  expect_equal(df$`WE-Wert`, c(34, 28, 20, 22, NA))
  expect_equal(df$`WE-%`, c(85, 70, 76.9, 84.6, NA))
  expect_equal(df$`Kat.`, c("2B", "4C", "2A", "3C*", "0"))
  hinweise <- paste(lade_hinweise(df), collapse = " ")
  expect_match(hinweise, "Itemzahl fuer 4 Kind(er) aus Wert und Prozentwert ermittelt",
               fixed = TRUE)
})

test_that("ein vollstaendiges Wertepaar genuegt, halbe Angaben bleiben leer", {
  df <- lade_fixture("items_zerlegt.tsv")

  # Zeile 1: beide Paare -> 40; Zeile 2: nur das R/F-Paar -> 40;
  # Zeile 3: nur ein Rohwert ohne Prozentwert -> keine Itemzahl
  expect_equal(df$Items, c(40, 40, NA))
  expect_equal(df$`WE-%`, c(85, NA, NA))
  expect_equal(df$`R/F-%`, c(70, 55, NA))
  # ohne Prozentwerte gibt es keine Kategorie - und "nicht teilgenommen" waere
  # falsch, weil ein Rohwert vorliegt
  expect_true(is.na(df$`Kat.`[3]))
  expect_false(identical(df$`Kat.`[3], "0"))
})

test_that("widerspruechliche Itemzahlen werden nicht uebernommen", {
  df <- lade_fixture("items_widerspruch.tsv")

  # Zeile 1: WE sagt 40, R/F sagt 20 -> nichts uebernehmen
  # Zeile 2: beide sagen 40
  expect_equal(df$Items, c(NA, 40))
  hinweise <- paste(lade_hinweise(df), collapse = " ")
  expect_match(hinweise, "nicht uebernommen (WE und R/F ergeben verschiedene Werte)",
               fixed = TRUE)
})

test_that("laden und speichern fuehrt eine Altdatei ins neue Format", {
  withr::with_tempdir({
    file.copy(file.path(projekt_root, "template.docx"), "template.docx")
    dir.create("Auswertungen")

    # Altdatei ohne Items-Spalte laden (Itemzahl wird ermittelt) und speichern
    df <- lade_fixture("items_rueckrechnung.tsv")
    saveData(df)

    tsv <- list.files("Auswertungen", pattern = "\\.tsv$", full.names = TRUE)
    gespeichert <- readr::read_tsv(tsv, show_col_types = FALSE)
    expect_true("Items" %in% colnames(gespeichert))
    expect_equal(gespeichert$Items, df$Items)

    # erneut laden: gleiche Zahlen, Itemzahl da, keine Hinweise mehr
    wieder <- loadData(list(datapath = tsv, name = basename(tsv),
                            size = file.size(tsv),
                            type = "text/tab-separated-values"))
    expect_length(lade_hinweise(wieder), 0)
    expect_equal(wieder$Items, df$Items)
    expect_equal(wieder$`WE-%`, df$`WE-%`)
    expect_equal(wieder$`Kat.`, df$`Kat.`)
  })
})

test_that("die Itemzahl steht in der tsv, aber nicht in den Berichten", {
  withr::with_tempdir({
    file.copy(file.path(projekt_root, "template.docx"), "template.docx")
    dir.create("Auswertungen")
    df <- leere_tabelle()
    df <- addEntry(df, name = "Testmann, Anna", klasse = "5c", rf = 28, we = 34,
                   numItems = 40)
    expect_equal(df$Items, 40)

    saveData(df)
    tsv <- list.files("Auswertungen", pattern = "\\.tsv$", full.names = TRUE)
    gespeichert <- readr::read_tsv(tsv, show_col_types = FALSE)
    expect_true("Items" %in% colnames(gespeichert))
    expect_equal(gespeichert$Items, 40)

    # wieder laden: Itemzahl bleibt, keine Hinweise
    wieder <- loadData(list(datapath = tsv, name = basename(tsv),
                            size = file.size(tsv),
                            type = "text/tab-separated-values"))
    expect_equal(wieder$Items, 40)
    expect_equal(wieder$`WE-%`, 85)
    expect_length(lade_hinweise(wieder), 0)
  })
})
