# Aufbau der Oberflaeche: prueft ohne Browser, dass die UI fehlerfrei
# entsteht und die zentralen Ein- und Ausgaben vorhanden sind.

test_that("die UI baut sich auf und enthaelt alle zentralen Elemente", {
  withr::with_dir(projekt_root, {
    suppressPackageStartupMessages(source("ui.R", local = FALSE))
  })
  html <- as.character(ui)

  # Eingaben und Ausgaben im Tab "Auswertung"
  for (id in c("schuelerName", "weWert", "rfWert", "btHinzufuegen", "btEntfernen",
               "numItems", "klassenstufe", "klBuchstabe", "tabUebersicht")) {
    expect_match(html, paste0('id="', id, '"'), fixed = TRUE)
  }
  # Eingaben und Ausgaben in Statistik und Elternbrief
  for (id in c("histWE", "histRF", "statsWE", "statsRF", "cbWEDiff", "cbAllCombined",
               "siPlotType", "lehrername", "signatur", "qrLink", "btBrief",
               # Infobrief und Stand-Brief: Briefart, Absender, Knopf
               "siBrieftyp", "infoAbsender", "btInfobrief", "infobriefHinweis",
               # Vorlagen und Einstellungen (Elternbrief und Infobrief)
               "btVorlageOeffnen", "btVorlagenOrdner", "btEinstellungen",
               "vorlagenHinweis")) {
    expect_match(html, paste0('id="', id, '"'), fixed = TRUE)
  }
  # die Anrede wird automatisch gebildet - ein Feld fuer die Klassenleitung
  # gibt es nicht mehr
  expect_false(grepl('id="infoKlassenleitung"', html, fixed = TRUE))
  # Menueband in der Seitenleiste
  expect_match(html, "btSpeichern", fixed = TRUE)
  expect_match(html, "input_tsv", fixed = TRUE)
  for (tab in c("Auswertung", "Statistik", "Elternbrief", "Anleitung")) {
    expect_match(html, tab, fixed = TRUE)
  }
  expect_match(html, "C-Test Auswertung", fixed = TRUE)
})

test_that("die Anleitung wird in die UI eingebunden", {
  html <- as.character(ui)
  # includeMarkdown("helpfiles/anleitung.md") rendert die Anleitung als HTML
  expect_match(html, "Schritt-für-Schritt", fixed = TRUE)
  expect_match(html, "C-Test-Auswertungstool", fixed = TRUE)
})
