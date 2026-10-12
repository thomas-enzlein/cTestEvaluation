# Mehrere Dateien je Stufe: erlaubt, solange keine KLASSE doppelt geladen ist.
# Dazu die neue vektorisierte Aehnlichkeit (gleiche Werte, viel schneller).

# kleine Hilfe: eine erfundene tsv-Datei im Eingabeformat der App.
# praefix macht die Kinder einer Kohorte zuordenbar (gleicher Name in beiden
# Jahrgaengen), ohne dass Namen innerhalb einer Stufe mehrdeutig werden.
schreibe_klasse <- function(klasse, praefix = "K", n = 12, rf = 62, we = 80,
                            datei = NULL) {
  df <- data.frame(Name = paste0(praefix, seq_len(n)),
                   Klasse = klasse,
                   `WE-Wert` = round(we * 0.4), `WE-%` = rep(we, n),
                   `R/F-Wert` = round(rf * 0.4), `R/F-%` = rep(rf, n),
                   Kat. = "3C", Empfehlung = "Test", Items = 40,
                   check.names = FALSE, stringsAsFactors = FALSE)
  pfad <- file.path(getwd(), if (is.null(datei)) paste0("klasse_", klasse, ".tsv") else datei)
  readr::write_tsv(df, pfad)
  list(datapath = pfad, name = basename(pfad), size = file.size(pfad),
       type = "text/tab-separated-values")
}

test_that("doppelte_klassen findet nur doppelte KLASSEN, nicht Stufen", {
  dateien <- list(
    list(schluessel = "d1", name = "a.tsv", klassen = c("5a", "5b"), stufen = 5),
    list(schluessel = "d2", name = "b.tsv", klassen = c("5c", "5d"), stufen = 5),
    list(schluessel = "d3", name = "c.tsv", klassen = c("6c"), stufen = 6))
  # zwei Dateien mit Stufe 5, aber verschiedenen Klassen: kein Problem
  expect_equal(doppelte_klassen(dateien), character(0))

  # dieselbe Klasse in zwei Dateien: das ist der Fehlerfall
  dateien[[3]]$klassen <- c("5c", "6c")
  expect_equal(doppelte_klassen(dateien), "5c")
  expect_equal(doppelte_klassen(list()), character(0))
})

test_that("kinder_doppelt zaehlt mehrfach vorhandene Kinder", {
  df <- data.frame(Name = c("A, A", "B, B", "A, A", "A, A", "A, A"),
                   Klasse = c("5c", "5c", "5c", "5d", "5c"),
                   check.names = FALSE, stringsAsFactors = FALSE)
  # "A, A" in 5c steht dreimal (Zeilen 1, 3, 5) -> zwei Ueberzaehlige;
  # "A, A" in 5d ist ein anderes Kind (andere Klasse) und zaehlt nicht mit
  expect_equal(kinder_doppelt(df), 2L)
  expect_equal(kinder_doppelt(NULL), 0L)
})

test_that("die vektorisierte Aehnlichkeit liefert dieselben Werte", {
  a <- c("Albers, Anna", "Beispiel, Ben", "Dahlbruch, Colin", "", "Meier, Mia")
  b <- c("Albers, Anna", "Beispel, Ben", "Dahlbruch, Collin", "Mueller, Max", "")

  m <- .name_aehnlichkeiten(a, b)
  expect_equal(dim(m), c(length(a), length(b)))
  for (i in seq_along(a)) {
    for (j in seq_along(b)) {
      expect_equal(m[i, j], name_aehnlichkeit(a[i], b[j]), tolerance = 1e-12)
    }
  }
  # leere Namen haben keine Aehnlichkeit
  expect_equal(m[4, ], rep(0, length(b)))
  expect_equal(m[, 5], rep(0, length(a)))
})

test_that("zwei Dateien mit getrennten Klassen derselben Stufe sind erlaubt", {
  withr::with_tempdir({
    dir.create("Auswertungen")
    sicherungsstand_zuruecksetzen()
    # zwei Kohorten mit eigenen Namen: C-Kinder in 5c/6c, D-Kinder in 5d/6d
    dateien <- list(schreibe_klasse("5c", praefix = "C"),
                    schreibe_klasse("5d", praefix = "D"),
                    schreibe_klasse("6c", praefix = "C"),
                    schreibe_klasse("6d", praefix = "D"))

    shiny::testServer(server, {
      session$setInputs(numItems = "40", klassenstufe = "5", klBuchstabe = "c",
                        cbWEDiff = FALSE, cbAllCombined = TRUE, siPlotType = "Histogramm")
      for (f in dateien) {
        ohne_bekannte_warnungen(session$setInputs(input_tsv = f))
      }

      expect_setequal(unique(rv$df$Klasse), c("5c", "5d", "6c", "6d"))
      expect_equal(stufen_verfuegbar(), c(5, 6))
      # Stufe 5 liegt in ZWEI Dateien, aber die Klassen sind verschieden
      expect_equal(doppelte_klassen(rv$dateien), character(0))
      expect_true(auswahl_moeglich())
      expect_equal(kinder_doppelt(rv$df), 0L)

      # die Zuordnung vergleicht beide Klassen der Stufe 5 mit der Stufe 6
      session$setInputs(siStufeAlt = 5, siStufeNeu = 6)
      k <- cohort_oder_null()
      expect_false(is.null(k))
      paare <- cohort_gematcht(k)
      expect_setequal(unique(paare$Klasse_Alt), c("5c", "5d"))
      expect_setequal(unique(paare$Klasse_Neu), c("6c", "6d"))
      expect_equal(nrow(paare), 24)
    })
  })
})

test_that("eine doppelt geladene Klasse wird gewarnt, aber nicht gesperrt", {
  withr::with_tempdir({
    dir.create("Auswertungen")
    sicherungsstand_zuruecksetzen()
    f5c <- schreibe_klasse("5c")
    f5c_neu <- schreibe_klasse("5c", datei = "klasse_5c_nachtrag.tsv")
    f6c <- schreibe_klasse("6c")

    shiny::testServer(server, {
      session$setInputs(numItems = "40", klassenstufe = "5", klBuchstabe = "c",
                        cbWEDiff = FALSE, cbAllCombined = TRUE, siPlotType = "Histogramm")
      for (f in list(f5c, f6c)) {
        ohne_bekannte_warnungen(session$setInputs(input_tsv = f))
      }
      expect_equal(klassen_doppelt(), character(0))
      expect_equal(als_text(output$vergleichHinweis), "")

      # dieselbe Klasse ein zweites Mal: Warnung, aber die Auswahl bleibt offen
      ohne_bekannte_warnungen(session$setInputs(input_tsv = f5c_neu))
      expect_equal(klassen_doppelt(), "5c")
      expect_gt(kinder_doppelt(rv$df), 0L)
      expect_true(auswahl_moeglich())
      hinweis <- als_text(output$vergleichHinweis)
      expect_match(hinweis, "5c", fixed = TRUE)
      expect_match(hinweis, "doppelt", fixed = TRUE)
      expect_match(hinweis, "×", fixed = TRUE)
    })
  })
})

test_that("klassen_mit_doppelten_kindern prueft die Kinder, nicht den Klassennamen", {
  mit_dopplung <- data.frame(Name = c("A", "B", "A", "C"),
                             Klasse = rep("6a", 4), stringsAsFactors = FALSE)
  expect_equal(klassen_mit_doppelten_kindern(mit_dopplung, "6a"), "6a")

  andere_kinder <- data.frame(Name = c("A", "B", "C", "D"),
                              Klasse = rep("6a", 4), stringsAsFactors = FALSE)
  expect_equal(klassen_mit_doppelten_kindern(andere_kinder, "6a"), character(0))
  expect_equal(klassen_mit_doppelten_kindern(andere_kinder, character(0)), character(0))
  expect_equal(klassen_mit_doppelten_kindern(NULL, "6a"), character(0))
})

test_that("gleichnamige Klasse aus zwei Jahren ist kein Fehler", {
  withr::with_tempdir({
    dir.create("Auswertungen")
    sicherungsstand_zuruecksetzen()
    # 6a aus zwei Jahren: gleicher Klassenname, aber andere Kinder
    f5c <- schreibe_klasse("5c", praefix = "C")
    f6a_2025 <- schreibe_klasse("6a", praefix = "A",
                                datei = "C-Test_Auswertung_2025-10-01_6a.tsv")
    f6a_2026 <- schreibe_klasse("6a", praefix = "B",
                                datei = "C-Test_Auswertung_2026-10-01_6a.tsv")

    shiny::testServer(server, {
      session$setInputs(numItems = "40", klassenstufe = "5", klBuchstabe = "c",
                        cbWEDiff = FALSE, cbAllCombined = TRUE, siPlotType = "Histogramm")
      for (f in list(f5c, f6a_2025, f6a_2026)) {
        ohne_bekannte_warnungen(session$setInputs(input_tsv = f))
      }

      # der Klassenname steht zweimal in der Dateiliste ...
      expect_equal(doppelte_klassen(rv$dateien), "6a")
      # ... die Kinder sind aber verschieden: kein Fehler, keine Warnung
      expect_equal(klassen_doppelt(), character(0))
      expect_equal(als_text(output$vergleichHinweis), "")
      expect_equal(kinder_doppelt(rv$df), 0L)

      # dieselbe Datei ein zweites Mal: jetzt stehen dieselben Kinder doppelt
      ohne_bekannte_warnungen(session$setInputs(input_tsv = f6a_2025))
      expect_equal(klassen_doppelt(), "6a")
      expect_gt(kinder_doppelt(rv$df), 0L)
      hinweis <- als_text(output$vergleichHinweis)
      expect_match(hinweis, "6a", fixed = TRUE)
      expect_match(hinweis, "doppelt", fixed = TRUE)
    })
  })
})

test_that("bei vielen Klassen zeigt die y-Achse nur zwei Werte", {
  viele <- do.call(rbind, lapply(c("5b", "6b", "6d", "6e"), function(k) {
    data.frame(Name = paste0("Kind", 1:6, k), Klasse = k,
               `WE-%` = c(40, 50, 60, 70, 80, 90), `R/F-%` = c(30, 40, 50, 60, 70, 80),
               Kat. = "3C", Empfehlung = "Test", Items = 40,
               check.names = FALSE, stringsAsFactors = FALSE)
  }))
  zwei <- viele[viele$Klasse %in% c("5b", "6b"), , drop = FALSE]

  brueche <- function(p) {
    gebaut <- ggplot2::ggplot_build(p)
    laengen <- vapply(gebaut$layout$panel_params, function(x) length(x$y$breaks), 0L)
    unique(laengen)
  }
  # vier Klassen: genau zwei Marken je Panel (Null und oberes Ende)
  expect_equal(brueche(createPlot(viele, "WE-%", "Kat.", "WE-Wert in %", allCombined = FALSE)),
               c(2L))
  # zwei Klassen: unveraendert die normale Achse
  expect_gt(max(brueche(createPlot(zwei, "WE-%", "Kat.", "WE-Wert in %", allCombined = FALSE))), 2L)
  # ohne Facetten ebenfalls unveraendert
  expect_gt(max(brueche(createPlot(viele, "WE-%", "Kat.", "WE-Wert in %", allCombined = TRUE))), 2L)
})
