# Einstellungen: persoenliche Datei neben den Ausgaben des Benutzers.
#
# Inhalt: Werte, die eine Lehrkraft nicht bei jedem Start neu eintippen soll,
# plus zwei Marken des R/F-Werts (rf_referenz und rf_norm_unten), die dort auch
# von Hand geaendert werden koennen.
#
# Wichtig: Die Kategorien der Kinder haengen NICHT an diesen Marken - sie
# ergeben sich aus den festen Normgrenzen des Verfahrens (.rf_stufen,
# .we_normgrenze). Das wird hier ausdruecklich geprueft.

# Einstellungen im Test immer in einem isolierten Benutzerordner
mit_einstellungen <- function(code) {
  withr::with_tempdir({
    withr::with_options(list(ctest.outdir.fallback = file.path(getwd(), "benutzer")),
                        {
                          referenzwerte_zuruecksetzen()
                          force(code)
                          referenzwerte_zuruecksetzen()
                        })
  })
}

einstellungen_datei <- function() {
  file.path("benutzer", "C-Test Auswertung", "einstellungen.txt")
}

# Werte setzen und die Datei schreiben (wie die App es beim Tippen tut)
setze <- function(...) {
  werte <- .einstellungen_default()
  neu <- list(...)
  for (name in names(neu)) werte[[name]] <- as.character(neu[[name]])
  einstellungen_schreiben(werte)
  referenzwerte_zuruecksetzen()
  invisible(referenzwerte())
}

# oeffne_datei() voruebergehend ersetzen (gleiches Vorgehen wie
# mit_compose_letter in den Elternbrief-Tests): im Test soll kein Editor
# aufgehen. Rueckgabe des Ausdrucks: der Pfad, der geoeffnet worden waere.
mit_oeffnen_gemerkt <- function(code) {
  echt <- oeffne_datei
  gemerkt <- NULL
  assign("oeffne_datei",
         function(pfad) {
           gemerkt <<- pfad
           invisible(TRUE)
         },
         envir = globalenv())
  on.exit(assign("oeffne_datei", echt, envir = globalenv()), add = TRUE)

  force(code)
  gemerkt
}

# Kinderkohorte fuer die Diagramm-Tests (5c und 6c, wie in der App)
kohorte <- function() {
  df <- dplyr::bind_rows(lade_fixture("klasse_5c.tsv"), lade_fixture("klasse_6c.tsv"))
  build_cohort(df, 5, 6)
}

test_that("ohne Datei gelten die Standardwerte", {
  mit_einstellungen({
    werte <- einstellungen_lesen()
    expect_equal(werte, .einstellungen_default())
    expect_equal(werte$numitems, "40")
    expect_equal(werte$rf_referenz, "71.3")
    expect_equal(werte$rf_norm_unten, "65")
    expect_equal(referenzwerte(), list(referenz = 71.3, norm_unten = 65))
  })
})

test_that("Schreiben und Lesen ist ein Rundlauf - auch mit Umlauten", {
  mit_einstellungen({
    werte <- .einstellungen_default()
    werte$lehrername <- "Müller-Lüdenscheidt, Özkan"
    werte$qrlink <- "https://example.org/übungen?x=1"
    werte$numitems <- "20"

    expect_true(isTRUE(einstellungen_schreiben(werte)))
    expect_true(file.exists(einstellungen_datei()))

    gelesen <- einstellungen_lesen()
    expect_equal(gelesen$lehrername, "Müller-Lüdenscheidt, Özkan")
    expect_equal(gelesen$qrlink, "https://example.org/übungen?x=1")
    expect_equal(gelesen$numitems, "20")

    # UTF-8 ohne BOM: sonst scheitert R beim Wiedereinlesen bzw. Notepad zeigt
    # Sonderzeichen falsch
    bytes <- readBin(einstellungen_datei(), "raw", n = 3)
    expect_false(identical(bytes, as.raw(c(0xEF, 0xBB, 0xBF))))
    expect_true(any(readBin(einstellungen_datei(), "raw", n = 2000) > as.raw(0x7F)))
  })
})

test_that("die Datei enthaelt alle bekannten Schluessel und einen Kopfkommentar", {
  mit_einstellungen({
    werte <- .einstellungen_default()
    werte$lehrername <- "Test, Tina"
    einstellungen_schreiben(werte)

    zeilen <- readLines(einstellungen_datei(), encoding = "UTF-8")
    expect_true(any(startsWith(zeilen, "#")))
    schluessel <- sub("=.*$", "", zeilen[!startsWith(zeilen, "#") & nzchar(zeilen)])
    expect_setequal(schluessel, names(.einstellungen_default()))
    expect_true(all(c("rf_referenz", "rf_norm_unten") %in% schluessel))
    # der Kommentar erklaert, dass die Kategorien unabhaengig sind
    expect_match(paste(zeilen, collapse = "\n"), "Kategorien der Kinder sind fest",
                 fixed = TRUE)
  })
})

test_that("unbekannte Schluessel und Kommentare in der Datei bleiben erhalten", {
  mit_einstellungen({
    einstellungen_schreiben(.einstellungen_default())
    cat("zukunftswert=42\n", file = einstellungen_datei(), append = TRUE)

    werte <- einstellungen_lesen()
    expect_equal(werte$zukunftswert, "42")

    einstellungen_schreiben(werte)
    expect_match(paste(readLines(einstellungen_datei(), encoding = "UTF-8"), collapse = "\n"),
                 "zukunftswert=42", fixed = TRUE)
  })
})

test_that("kaputte Zeilen und fehlende Dateien stoeren nicht", {
  mit_einstellungen({
    dir.create(dirname(einstellungen_datei()), recursive = TRUE)
    writeLines(c("nur ein wort ohne gleichheitszeichen",
                 "=wert ohne schluessel",
                 "lehrername=Test, Tina",
                 "",
                 "# Kommentar"), einstellungen_datei())

    werte <- einstellungen_lesen()
    expect_equal(werte$lehrername, "Test, Tina")
    expect_equal(werte$numitems, "40")
  })
})

test_that("die R/F-Marken aendern keine Kategorie", {
  mit_einstellungen({
    raster <- expand.grid(rf = 0:100, we = 0:100)
    kategorien <- function() {
      paste0(getRFlevel(raster$rf), getWElevel(raster$rf, raster$we))
    }

    standard <- kategorien()
    expect_gt(sum(!is.na(standard)), 0)

    # deutlich andere Marken: die Einteilung muss gleich bleiben
    setze(rf_referenz = 95, rf_norm_unten = 50)
    expect_equal(referenzwerte(), list(referenz = 95, norm_unten = 50))
    expect_identical(kategorien(), standard)

    setze(rf_referenz = 60, rf_norm_unten = 20)
    expect_identical(kategorien(), standard)

    # und der Wortschatz-Wert ist ebenfalls unabhaengig
    expect_equal(.we_normgrenze, 65)
    expect_equal(getWElevel(60, 70), "C")
    setze(rf_norm_unten = 90)
    expect_equal(getWElevel(60, 70), "C")
  })
})

test_that("die Grenze des unteren Normbereichs haengt an der Kennzahl", {
  mit_einstellungen({
    # Voreinstellung: beide 65
    expect_equal(.normgrenze("R/F"), 65)
    expect_equal(.normgrenze("WE"), 65)

    setze(rf_norm_unten = 50)
    # nur der R/F-Wert folgt der Einstellung
    expect_equal(.normgrenze("R/F"), 50)
    expect_equal(.normgrenze("WE"), 65)

    expect_error(.normgrenze("diff"))
  })
})

test_that("unbrauchbare R/F-Marken fallen auf die Standardwerte zurueck", {
  mit_einstellungen({
    pruefe <- function(referenz, norm_unten) {
      setze(rf_referenz = referenz, rf_norm_unten = norm_unten)
      expect_equal(referenzwerte(), list(referenz = 71.3, norm_unten = 65))
    }

    pruefe("abc", "65")        # Text statt Zahl
    pruefe("71.3", "80")       # unterer Normbereich ueber dem Referenzwert
    pruefe("71.3", "71.3")     # gleich gross ist keine Ordnung
    pruefe("120", "65")        # ausserhalb 0..100
    pruefe("71.3", "0")        # unterer Normbereich bei null

    # eine niedrige Marke ist dagegen erlaubt - sie beeinflusst nur die Linien
    setze(rf_referenz = 60, rf_norm_unten = 40)
    expect_equal(referenzwerte(), list(referenz = 60, norm_unten = 40))
  })
})

test_that("ein Handedit in der Datei wird beim Speichern nicht ueberschrieben", {
  mit_einstellungen({
    setze(rf_referenz = 80, rf_norm_unten = 70)

    neu <- list(lehrername = "Test, Tina")
    zusammengefasst <- einstellungen_mit_referenz(neu)
    expect_equal(zusammengefasst$rf_referenz, "80")
    expect_equal(zusammengefasst$rf_norm_unten, "70")

    # auch ein unbrauchbarer Handeintrag bleibt stehen (die App rechnet dann
    # mit den Standardwerten, ueberschreibt aber nichts)
    setze(rf_referenz = "50", rf_norm_unten = "80")
    expect_equal(einstellungen_mit_referenz(neu)$rf_norm_unten, "80")
    expect_equal(referenzwerte()$norm_unten, 65)
  })
})

test_that("ohne Datei werden die R/F-Marken beim ersten Speichern ergaenzt", {
  mit_einstellungen({
    expect_false(file.exists(einstellungen_datei()))

    pfad <- einstellungen_bereitstellen(list(lehrername = "Test, Tina"))
    expect_true(file.exists(pfad))

    werte <- einstellungen_lesen()
    expect_equal(werte$rf_referenz, "71.3")
    expect_equal(werte$rf_norm_unten, "65")
  })
})

test_that("die Referenzlinien im R/F-Diagramm nutzen die Einstellungen", {
  mit_einstellungen({
    setze(rf_referenz = 80, rf_norm_unten = 60)

    p <- referenz_linien(ggplot2::ggplot(data.frame(x = 1, y = 1),
                                         ggplot2::aes(x = x, y = y)))
    bau <- ggplot2::ggplot_build(p)
    expect_equal(length(p$layers), 2)
    expect_setequal(c(bau$data[[1]]$xintercept[1], bau$data[[2]]$xintercept[1]),
                    c(80, 60))
  })
})

test_that("im Verlaufsdiagramm folgt nur die R/F-Linie den Einstellungen", {
  mit_einstellungen({
    k <- kohorte()
    setze(rf_norm_unten = 50)

    p <- plot_verlauf(k)
    expect_no_error(ggplot2::ggplot_build(p))

    # die Linien liegen als eigene Ebene vor: je Facette eine Grenze
    linien <- p$layers[[3]]$data
    expect_equal(linien$Grenze[linien$Kennzahl == "R/F-Wert in %"], 50)
    expect_equal(linien$Grenze[linien$Kennzahl == "WE-Wert in %"], 65)
  })
})

test_that("im Entwicklungsdiagramm faerbt der R/F-Wert nach der Einstellung", {
  mit_einstellungen({
    k <- kohorte()
    anzahl_rot <- function() {
      p <- plot_veraenderung(k, variable = "R/F-%")
      d <- ggplot2::ggplot_build(p)$data[[4]]
      sum(d$colour == "darkred")
    }

    standard <- anzahl_rot()

    # ein hoher unterer Normbereich stellt mehr Kinder als unterstuetzungsbeduerftig dar
    # (der Referenzwert muss dabei darueber bleiben)
    setze(rf_referenz = 95, rf_norm_unten = 90)
    expect_gt(anzahl_rot(), standard)

    # der Wortschatz-Fall bleibt davon unberuehrt
    p_we <- plot_veraenderung(k, variable = "WE-%")
    expect_no_error(ggplot2::ggplot_build(p_we))
  })
})

test_that("ein unbrauchbarer Ordner verhindert das Speichern nicht hart", {
  withr::with_tempdir({
    # "benutzer" ist eine Datei: der Ordner kann nicht angelegt werden
    writeLines("blockiert", "benutzer")
    withr::with_options(list(ctest.outdir.fallback = file.path(getwd(), "benutzer")), {
      expect_false(isTRUE(einstellungen_schreiben(.einstellungen_default())))
      expect_equal(einstellungen_lesen()$numitems, "40")
    })
  })
})

test_that("der Knopf legt die Einstellungen an und oeffnet sie", {
  # Der persoenliche Ordner kommt aus der Testumgebung (siehe helper-app.R),
  # es wird also nichts in den echten Dokumenten angelegt.
  pfad <- einstellungen_pfad()
  unlink(pfad)

  geoeffnet <- mit_oeffnen_gemerkt({
    shiny::testServer(server, {
      session$setInputs(numItems = "40", klassenstufe = "5", klBuchstabe = "c",
                        cbWEDiff = FALSE, cbAllCombined = TRUE,
                        siPlotType = "Histogramm",
                        lehrername = "Test, Tina",
                        qrLink = "https://example.org/uebungen")
      session$setInputs(btEinstellungen = 1)
      expect_match(als_text(output$text), "Einstellungen geöffnet", fixed = TRUE)
    })
  })

  expect_equal(geoeffnet, pfad)
  expect_true(file.exists(pfad))

  werte <- einstellungen_lesen(pfad)
  expect_equal(werte$lehrername, "Test, Tina")
  expect_equal(werte$qrlink, "https://example.org/uebungen")
  # die R/F-Marken stehen von Anfang an mit in der Datei
  expect_equal(werte$rf_referenz, "71.3")
  expect_equal(werte$rf_norm_unten, "65")

  unlink(pfad)
})

test_that("Aenderungen werden automatisch gespeichert (verzoegert)", {
  pfad <- einstellungen_pfad()
  unlink(pfad)

  shiny::testServer(server, {
    session$setInputs(lehrername = "Neu, Nina", signatur = "Fachleitung")
    # Die Speicherung ist um eine Sekunde verzoegert, damit nicht jeder
    # Tastendruck auf die Platte geht - hier die faelligen Timer ausfuehren
    Sys.sleep(1.3)
    later::run_now()
    session$flushReact()
    Sys.sleep(0.2)
    later::run_now()
    session$flushReact()
  })

  expect_true(file.exists(pfad))
  werte <- einstellungen_lesen(pfad)
  expect_equal(werte$lehrername, "Neu, Nina")
  expect_equal(werte$signatur, "Fachleitung")
  expect_equal(werte$rf_referenz, "71.3")

  unlink(pfad)
})
