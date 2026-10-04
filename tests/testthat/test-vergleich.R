# Vergleichswerte der Schule: Baender, Rangintervalle, Mindestgroessen und das
# Lesen der Referenzdatei. Die Werte hier sind erfunden - geprueft wird die
# Logik, nicht unsere eigene Verteilung.

erfundene_referenz <- function() {
  kinder <- data.frame(
    klassenstufe = c("5", "5", "6", "6", "5"),
    kennzahl = c("R/F", "WE", "R/F", "WE", "R/F"),
    n = c(481, 481, 371, 371, 50),
    p10 = c(27.5, 45, 37.5, 55, NA),
    p25 = c(45, 65, 56.2, 72.5, 50),
    p50 = c(62.5, 77.5, 70, 85, 65),
    p75 = c(75, 85, 84.6, 92.5, 80),
    p90 = c(82.5, 92.5, 92.3, 100, NA),
    stringsAsFactors = FALSE)

  klassen <- data.frame(
    klassenstufe = c("5", "5", "5", "5", "6", "6"),
    kennzahl = c("R/F", "R/F", "WE", "R/F", "R/F", "WE"),
    lagemass = c("Mittel", "Median", "Median", "Mittel", "Median", "Median"),
    n_klassen = c(18, 18, 18, 6, 14, 14),
    p10 = c(55, 55.5, 72.5, 55, 58.75, 73.25),
    p25 = c(55.7, 60, 74.06, 56, 61.25, 83.13),
    p50 = c(58.6, 62, 76.9, 59, 73.13, 87.38),
    p75 = c(61.6, 64.6, 79.69, 62, 84.6, 93.76),
    p90 = c(64.1, 67.1, 80, 65, 87.92, 94.25),
    stringsAsFactors = FALSE)
  # Zeile 4: kleine Schule mit nur 6 Klassengruppen

  entwicklung <- data.frame(
    klassenstufe = "5->6", kennzahl = c("R/F", "WE"), n = c(291, 291),
    p10 = c(-14.9, -12.5), p25 = c(-5.2, -3.8), p50 = c(3.9, 5),
    p75 = c(16.7, 15), p90 = c(27.5, 26.2),
    Erwartung_a = c(29.6, 42.7), Erwartung_b = c(-0.355, -0.477),
    Rest_SD = c(15.1, 13.2), Kohorten_SD = c(8.31, 6.64), n_Kohorten = c(13, 13),
    stringsAsFactors = FALSE)

  list(kinder = kinder, klassen = klassen, entwicklung = entwicklung,
       info = NULL, datei = "erfunden")
}

# eine Klasse mit genug Kindern fuer einen Klassenvergleich (ab 10)
grosse_klasse <- function(klasse = "5c", rf = 62, we = 80, n = 36) {
  data.frame(Name = paste0("Kind", seq_len(n)), Klasse = klasse,
             `R/F-%` = rep(rf, n), `WE-%` = rep(we, n), Kat. = "3C",
             check.names = FALSE, stringsAsFactors = FALSE)
}

test_that("Kinder werden in weite Baender mit Rangintervall eingeordnet", {
  ref <- erfundene_referenz()

  oben <- vergleich_urteil(ref, 95, "R/F", "kind", stufe = "6")
  expect_equal(oben$band, "obere 10 %")
  expect_true(oben$rang[1] > 70 && oben$rang[2] >= oben$rang[1])

  mitte <- vergleich_urteil(ref, 70, "R/F", "kind", stufe = "6")
  expect_equal(mitte$band, "im Jahrgangsbereich")

  unten <- vergleich_urteil(ref, 30, "R/F", "kind", stufe = "6")
  expect_equal(unten$band, "untere 10 %")

  # das Rangintervall ist breit: der Messfehler eines Einzelwerts steckt darin
  expect_gt(mitte$rang[2] - mitte$rang[1], 20)
  # und bleibt im gueltigen Bereich
  expect_gte(mitte$rang[1], 0)
  expect_lte(mitte$rang[2], 100)
})

test_that("eine Decke im oberen Band (WE = 100 %) verhindert die Aussage", {
  ref <- erfundene_referenz()
  # Stufe 6 WE hat p90 = 100: niemand kann darueber liegen, also gibt es dort
  # keine "obere 10 %" - der Wert bleibt im Jahrgangsbereich
  oben <- vergleich_urteil(ref, 100, "WE", "kind", stufe = "6")
  expect_equal(oben$band, "im Jahrgangsbereich")
  expect_equal(unname(oben$rang[2]), 90)   # oberster Schnittpunkt der Referenz
  expect_lte(oben$rang[2], 100)
  # Stufe 5 WE hat p90 = 92,5 -> normale Aussage
  klar <- vergleich_urteil(ref, 98, "WE", "kind", stufe = "5")
  expect_equal(klar$band, "obere 10 %")
})

test_that("ohne Dezile gilt die Viertel-Einteilung", {
  # kleine Schule: nur Viertel-Schnittpunkte in der Datei
  klein <- list(kinder = data.frame(
    klassenstufe = "5", kennzahl = "R/F", n = 50,
    p10 = NA_real_, p25 = 50, p50 = 65, p75 = 80, p90 = NA_real_,
    stringsAsFactors = FALSE))
  urteil <- vergleich_urteil(klein, 90, "R/F", "kind", stufe = "5")
  expect_false(urteil$dezile)
  expect_equal(urteil$band, "oberes Viertel")
})

test_that("Mindestgroessen verhindern Aussagen auf zu kleiner Basis", {
  ref <- erfundene_referenz()

  # Kinder: unter 30 keine Aussage
  wenig <- erfundene_referenz()
  wenig$kinder$n[1] <- 20
  expect_null(vergleich_urteil(wenig, 60, "R/F", "kind", stufe = "5"))

  # Klassen: unter 8 Klassengruppen keine Aussage
  kleine_schule <- list(klassen = data.frame(
    klassenstufe = "5", kennzahl = "R/F", lagemass = "Mittel", n_klassen = 6,
    p10 = 55, p25 = 56, p50 = 59, p75 = 62, p90 = 65, stringsAsFactors = FALSE))
  expect_null(vergleich_urteil(kleine_schule, 60, "R/F", "klasse", stufe = "5",
                               lagemass = "Mittel", n_kinder = 25))

  # mit 18 Gruppen schon
  urteil <- vergleich_urteil(ref, 62, "R/F", "klasse", stufe = "5",
                             lagemass = "Median", n_kinder = 25)
  expect_equal(urteil$band, "Mittelfeld")

  # fehlende Bezugsgruppe (Stufe 7) ergibt keine Aussage
  expect_null(vergleich_urteil(ref, 60, "R/F", "kind", stufe = "7"))
})

test_that("Klassen werden gegen ihre eigene Verteilung gehalten", {
  ref <- erfundene_referenz()

  tief <- vergleich_urteil(ref, 54, "R/F", "klasse", stufe = "5",
                           lagemass = "Mittel", n_kinder = 25)
  expect_equal(tief$band, "unteres Viertel")

  hoch <- vergleich_urteil(ref, 66, "R/F", "klasse", stufe = "5",
                           lagemass = "Mittel", n_kinder = 25)
  expect_equal(hoch$band, "oberes Viertel")
})

test_that("die Entwicklung wird an der Erwartung des Startniveaus gemessen", {
  ref <- erfundene_referenz()
  erwartet_rf <- function(start) 29.6 - 0.355 * start

  # 1) klar ueber der Erwartung (Start 63,8 -> erwartet ~7,0, erreicht 20,2)
  stark <- vergleich_entwicklung_urteil(ref, 63.8, 20.2, "R/F", n_kinder = 19)
  expect_equal(round(stark$erwartet, 1), round(erwartet_rf(63.8), 1))
  expect_equal(round(stark$abweichung, 1), round(20.2 - erwartet_rf(63.8), 1))
  expect_equal(stark$bewertung, "deutlich über dem Üblichen")

  # 2) im ueblichen Bereich
  ueblich <- vergleich_entwicklung_urteil(ref, 61.4, 5.0, "R/F", n_kinder = 22)
  expect_equal(ueblich$bewertung, "im üblichen Bereich")

  # 3) leicht unter dem Ueblichen (Rangintervall beruehrt die Grenze)
  leicht <- vergleich_entwicklung_urteil(ref, 61.5, -0.2, "R/F", n_kinder = 23)
  expect_equal(leicht$bewertung, "leicht unter dem Üblichen")

  # 4) dieselbe Abweichung, groesseres n -> Rangintervall enger -> deutlicher
  eng <- vergleich_entwicklung_urteil(ref, 46.5, 22.5, "R/F", n_kinder = 21)
  expect_equal(eng$bewertung, "leicht über dem Üblichen")
  weit <- vergleich_entwicklung_urteil(ref, 46.5, 22.5, "R/F", n_kinder = 200)
  expect_equal(weit$bewertung, "deutlich über dem Üblichen")

  # dasselbe erreichte Ergebnis wird bei hoeherem Startniveau besser beurteilt
  hoch <- vergleich_entwicklung_urteil(ref, 80, 5.0, "R/F", n_kinder = 20)
  tief <- vergleich_entwicklung_urteil(ref, 50, 5.0, "R/F", n_kinder = 20)
  expect_gt(hoch$abweichung, tief$abweichung)

  # Erwartung ist auf den verbleibenden Raum gedeckelt (Schutz gegen sinnlose Werte)
  steil <- erfundene_referenz()
  steil$entwicklung$Erwartung_a[1] <- 0
  steil$entwicklung$Erwartung_b[1] <- 0.5
  gedeckelt <- vergleich_entwicklung_urteil(steil, 99, 0, "R/F", n_kinder = 20)
  expect_equal(gedeckelt$erwartet, 1)

  # ohne Erwartung in der Datei (aeltere Referenz) gibt es keine Aussage
  alt <- erfundene_referenz()
  alt$entwicklung$Erwartung_a <- NULL
  expect_null(vergleich_entwicklung_urteil(alt, 63.8, 20.2, "R/F", n_kinder = 19))
  expect_null(vergleich_entwicklung_urteil(ref, NA_real_, 20.2, "R/F", n_kinder = 19))
})

test_that("Entwicklung: Satz fuer den Brief und Zeile fuer den Statistik-Tab", {
  ref <- erfundene_referenz()

  stark <- vergleich_entwicklung_urteil(ref, 63.8, 20.2, "R/F", n_kinder = 19)
  satz <- vergleich_entwicklung_satz(stark)
  expect_match(satz, "Die Kohorte startete bei 63,8 % (R/F)", fixed = TRUE)
  expect_match(satz, "mittleren Zuwachs von +20,2 Punkten", fixed = TRUE)
  expect_match(satz, "üblich für dieses Niveau sind etwa +7,0", fixed = TRUE)
  expect_match(satz, "deutlich über dem Üblichen", fixed = TRUE)

  # die Kennzahl laesst sich ueberschreiben (z. B. fuer den Wortschatz)
  expect_match(vergleich_entwicklung_satz(stark, "WE"), "(WE)", fixed = TRUE)

  ueblich <- vergleich_entwicklung_urteil(ref, 61.4, 5.0, "R/F", n_kinder = 22)
  zeile <- vergleich_entwicklung_zeile(stats::setNames(list(ueblich), ""), "R/F")
  expect_match(zeile, "Mittlere Entwicklung R/F (5 \u2192 6)", fixed = TRUE)
  expect_match(zeile, "+5,0 (erwartet +7,8)", fixed = TRUE)
  expect_match(zeile, "im üblichen Bereich", fixed = TRUE)

  # je Kohorte mit Beschriftung
  zwei <- vergleich_entwicklung_zeile(list("5a\u21926a" = ueblich, "5c\u21926c" = stark), "R/F")
  expect_match(zwei, "; 5a\u21926a", fixed = TRUE)
  expect_match(zwei, "; 5c\u21926c", fixed = TRUE)
  # mehrere Bewertungen -> keine Sammelbewertung am Ende
  expect_false(grepl("üblich", zwei, fixed = TRUE) && grepl("üblich.", zwei, fixed = TRUE))

  expect_null(vergleich_entwicklung_satz(NULL))
  expect_null(vergleich_entwicklung_zeile(list(), "R/F"))
})

test_that("Kurzform und Satz nennen Band, Rang und Bezugsgruppe", {
  ref <- erfundene_referenz()

  kind <- vergleich_urteil(ref, 95, "R/F", "kind", stufe = "6")
  expect_match(vergleich_kurz(kind), "obere 10 %", fixed = TRUE)
  expect_match(vergleich_kurz(kind), "Rang", fixed = TRUE)
  expect_match(vergleich_satz(kind), "Vergleichswert R/F", fixed = TRUE)

  klasse <- vergleich_urteil(ref, 62, "R/F", "klasse", stufe = "5",
                             lagemass = "Median", n_kinder = 25)
  satz <- vergleich_satz(klasse)
  expect_match(satz, "Klassen-Median R/F", fixed = TRUE)
  expect_match(satz, "den 5. Klassen dieser Schule", fixed = TRUE)
  expect_match(satz, "n = 18", fixed = TRUE)

  expect_null(vergleich_satz(NULL))
})

test_that("die Referenzdatei wird gelesen, fehlende oder kaputte Dateien nicht", {
  withr::with_tempdir({
    # wie die App sucht: persoenlicher Vorlagenordner
    withr::local_options(list(ctest.outdir.fallback = file.path(getwd(), "benutzer")))
    ordner <- vorlagen_ordner(anlegen = TRUE)

    # ohne Datei gibt es keine Referenz
    expect_null(vergleich_referenz())

    ref <- erfundene_referenz()
    pfad <- file.path(ordner, .vergleich_dateiname)
    # ueber die Workbook-Schnittstelle: write.xlsx(append = TRUE) ersetzt die
    # Datei, wenn overwrite nicht ausdruecklich FALSE ist
    wb <- openxlsx::createWorkbook()
    for (blatt in c("Kinder", "Klassen", "Entwicklung")) {
      openxlsx::addWorksheet(wb, blatt)
      openxlsx::writeData(wb, blatt, ref[[tolower(blatt)]])
    }
    openxlsx::saveWorkbook(wb, pfad, overwrite = TRUE)

    gelesen <- vergleich_referenz()
    expect_false(is.null(gelesen))
    expect_equal(nrow(gelesen$kinder), nrow(ref$kinder))
    expect_equal(names(gelesen$kinder),
                 c("klassenstufe", "kennzahl", "n", "p10", "p25", "p50", "p75", "p90"))
    # und damit laesst sich rechnen
    expect_equal(vergleich_urteil(gelesen, 95, "R/F", "kind", stufe = "6")$band,
                 "obere 10 %")

    # eine kaputte Datei fuehrt nicht zum Absturz, sondern zu keiner Aussage
    writeLines("kein xlsx", pfad, useBytes = TRUE)
    expect_null(vergleich_referenz())
  })
})

test_that("die mitgelieferte Referenz enthaelt keine Namen", {
  withr::with_dir(projekt_root, {
    ref <- vergleich_referenz()
  })
  skip_if(is.null(ref), "keine Referenzdatei vorhanden")

  for (blatt in c("kinder", "klassen", "entwicklung")) {
    if (is.null(ref[[blatt]])) next
    expect_false(any(grepl("name", names(ref[[blatt]]), ignore.case = TRUE)))
  }
})

test_that("Klassenzeilen und Kindspalten fuer die Anzeige", {
  ref <- erfundene_referenz()
  gross <- dplyr::bind_rows(grosse_klasse("5c", rf = 62, we = 80),
                            grosse_klasse("6c", rf = 58, we = 70))

  zeilen <- vergleich_klassen_zeilen(gross, ref)
  expect_equal(length(zeilen), 4)                       # 2 Klassen x 2 Kennzahlen
  expect_true(any(grepl("5c", zeilen, fixed = TRUE)))
  expect_true(any(grepl("R/F-Median", zeilen, fixed = TRUE)))
  expect_true(any(grepl("Bezug: 5. Klassen dieser Schule", zeilen, fixed = TRUE)))
  # 5c liegt mit R/F 62 im Mittelfeld, 6c mit 58 im unteren Viertel
  expect_true(any(grepl("Mittelfeld", zeilen, fixed = TRUE)))
  expect_true(any(grepl("unteres Viertel", zeilen, fixed = TRUE)))

  # ohne Referenz gibt es keine Zeilen
  expect_length(vergleich_klassen_zeilen(gross, NULL), 0)
  # sehr kleine Klassen werden nicht verglichen
  expect_length(vergleich_klassen_zeilen(grosse_klasse(n = 5), ref), 0)

  spalten <- vergleich_spalten(gross, ref)
  expect_equal(names(spalten), c("Vergleich R/F", "Vergleich WE"))
  expect_equal(nrow(spalten), nrow(gross))
  # mindestens ein Kind bekommt ein Band, leere Werte bleiben leer
  expect_true(any(nzchar(spalten[["Vergleich R/F"]])))
  ohne <- gross
  ohne[["R/F-%"]][1] <- NA
  expect_equal(vergleich_spalten(ohne, ref)[["Vergleich R/F"]][1], "")
  expect_null(vergleich_spalten(gross, NULL))
})

test_that("der Stand-Brief nimmt den Klassenvergleich auf", {
  ref <- erfundene_referenz()
  df <- grosse_klasse("5c", rf = 62, we = 80)

  ohne <- stand_bericht(df)
  expect_null(ohne$abschnitte[[1]]$vergleich)

  mit <- stand_bericht(df, vergleich = ref)
  text <- mit$abschnitte[[1]]$vergleich
  expect_false(is.null(text))
  expect_match(text, "Klassen-Median R/F", fixed = TRUE)
  expect_match(text, "den 5. Klassen dieser Schule", fixed = TRUE)

  # eine zu kleine Klasse bleibt ohne Vergleich
  klein <- stand_bericht(grosse_klasse("5c", n = 6), vergleich = ref)
  expect_null(klein$abschnitte[[1]]$vergleich)
})

test_that("der Entwicklungsbrief nimmt den Entwicklungsvergleich auf", {
  ref <- erfundene_referenz()
  df <- dplyr::bind_rows(lade_fixture("klasse_5c.tsv"), lade_fixture("klasse_6c.tsv"))

  ohne <- infobrief_bericht(build_cohort(df, 5, 6))
  expect_true(all(vapply(ohne$abschnitte, function(a) is.null(a$vergleich), logical(1))))

  mit <- infobrief_bericht(build_cohort(df, 5, 6), vergleich = ref)
  texte <- lapply(mit$abschnitte, function(a) a$vergleich)
  zusammen <- paste(unlist(texte), collapse = " ")
  expect_true(any(vapply(texte, function(t) !is.null(t), logical(1))))
  expect_match(zusammen, "Die Kohorte startete bei", fixed = TRUE)
  expect_match(zusammen, "mittleren Zuwachs von", fixed = TRUE)
  expect_match(zusammen, "üblich für dieses Niveau sind etwa", fixed = TRUE)
  expect_match(zusammen, "(R/F)", fixed = TRUE)
  expect_match(zusammen, "(WE)", fixed = TRUE)

  # je Kennzahl ein eigener Satz, Wortschatz zuerst (wie in den Tabellen)
  erster <- Filter(Negate(is.null), texte)[[1]]
  expect_length(erster, 2)
  expect_match(erster[1], "(WE)", fixed = TRUE)
  expect_match(erster[2], "(R/F)", fixed = TRUE)
})

test_that("die Entwicklungszeile im Statistik-Tab kommt aus der Zuordnung", {
  ref <- erfundene_referenz()
  # eine erfundene Zuordnung: zwei Kohorten mit je 12 Kindern
  gematcht <- data.frame(
    Name_Alt = paste0("Kind", 1:24), Klasse_Alt = rep(c("5a", "5c"), each = 12),
    Name_Neu = paste0("Kind", 1:24), Klasse_Neu = rep(c("6a", "6c"), each = 12),
    RF_Alt = rep(c(60, 46.5), each = 12),
    RF_Neu = rep(c(66, 64), each = 12),
    WE_Alt = rep(c(75, 62), each = 12),
    WE_Neu = rep(c(80, 74), each = 12),
    stringsAsFactors = FALSE)

  ohne <- vergleich_entwicklung_zeilen(gematcht, NULL, "R/F")
  expect_length(ohne, 0)
  expect_length(vergleich_entwicklung_zeilen(NULL, ref, "R/F"), 0)
  expect_length(vergleich_entwicklung_zeilen("kein data.frame", ref, "R/F"), 0)

  gesamt <- vergleich_entwicklung_zeilen(gematcht, ref, "R/F")
  expect_length(gesamt, 1)
  expect_match(gesamt, "Mittlere Entwicklung R/F (5 \u2192 6):", fixed = TRUE)
  expect_match(gesamt, "(erwartet", fixed = TRUE)

  pro <- vergleich_entwicklung_zeilen(gematcht, ref, "R/F", pro_kohorte = TRUE)
  expect_length(pro, 1)
  expect_match(pro, "5a\u21926a", fixed = TRUE)
  expect_match(pro, "5c\u21926c", fixed = TRUE)

  # sehr kleine Kohorten werden nicht einzeln ausgewiesen
  klein <- gematcht[1:6, , drop = FALSE]
  expect_length(vergleich_entwicklung_zeilen(klein, ref, "R/F", pro_kohorte = TRUE), 0)
  # die Gesamtuebersicht gibt es trotzdem
  expect_length(vergleich_entwicklung_zeilen(klein, ref, "R/F"), 1)
})

test_that("die App zeigt die Innenansicht nur mit Schalter und Datei", {
  mit_referenz <- function(code) {
    withr::with_tempdir({
      withr::local_options(list(ctest.outdir.fallback = file.path(getwd(), "benutzer")))
      ref <- erfundene_referenz()
      wb <- openxlsx::createWorkbook()
      for (blatt in c("Kinder", "Klassen", "Entwicklung")) {
        openxlsx::addWorksheet(wb, blatt)
        openxlsx::writeData(wb, blatt, ref[[tolower(blatt)]])
      }
      openxlsx::saveWorkbook(wb, file.path(vorlagen_ordner(anlegen = TRUE),
                                           .vergleich_dateiname), overwrite = TRUE)
      force(code)
    })
  }

  # eine Klasse, die gross genug fuer einen Klassenvergleich ist (>= 10 Kinder)
  grosse_klasse <- function() {
    basis <- lade_fixture("klasse_5c.tsv")
    basis <- basis[!is.na(basis$`R/F-%`), , drop = FALSE]
    gross <- dplyr::bind_rows(lapply(seq_len(5), function(i) {
      b <- basis
      b$Name <- paste0(b$Name, " ", i)
      b
    }))
    pfad <- file.path(getwd(), "klasse_5c_gross.tsv")
    readr::write_tsv(gross, pfad)
    list(datapath = pfad, name = basename(pfad), size = file.size(pfad),
         type = "text/tab-separated-values")
  }

  mit_referenz({
    shiny::testServer(server, {
      session$setInputs(numItems = "40", klassenstufe = "5", klBuchstabe = "c",
                        cbWEDiff = FALSE, cbAllCombined = TRUE,
                        siPlotType = "Histogramm")
      ohne_bekannte_warnungen(session$setInputs(input_tsv = grosse_klasse()))

      # Vorgabe: Schalter aus -> nichts zu sehen
      expect_null(output$vergleichwerteHinweis)
      expect_null(output$vergleichKlasse)
      expect_null(output$vergleichKlasseWE)
      expect_false(grepl("Vergleich R/F", als_text(output$tabUebersicht), fixed = TRUE))

      # Schalter an -> Hinweis, Klassenvergleich und Spalten in der Tabelle
      ohne_bekannte_warnungen(session$setInputs(cbVergleich = TRUE))
      expect_match(als_text(output$vergleichwerteHinweis), "Vergleichswerte", fixed = TRUE)
      # R/F-Zeilen im R/F-Kasten, WE-Zeilen im WE-Kasten
      expect_match(als_text(output$vergleichKlasse), "R/F-Werten der Klassen", fixed = TRUE)
      expect_match(als_text(output$vergleichKlasse), "5c", fixed = TRUE)
      expect_match(als_text(output$vergleichKlasseWE), "WE-Werten der Klassen", fixed = TRUE)
      expect_false(grepl("WE-Median", als_text(output$vergleichKlasse), fixed = TRUE))
      expect_false(grepl("R/F-Median", als_text(output$vergleichKlasseWE), fixed = TRUE))

      tabelle <- als_text(output$tabUebersicht)
      expect_match(tabelle, "Vergleich R/F", fixed = TRUE)
      expect_match(tabelle, "Vergleich WE", fixed = TRUE)
    })
  })

  # ein Paar 5c/6c mit 12 zugeordneten Kindern (ueber der 10er-Grenze)
  klassenpaar <- function() {
    n <- 12
    rf5 <- seq(40, 80, length.out = n)
    rf6 <- rf5 + rep(c(3, 12), length.out = n)
    we5 <- rf5 + 15
    we6 <- rf6 + 5
    bauen <- function(klasse, rf, we) {
      data.frame(Name = paste0("Paar", seq_len(n), ", Test"), Klasse = klasse,
                 `WE-Wert` = round(we / 2.5), `WE-%` = we,
                 `R/F-Wert` = round(rf / 2.5), `R/F-%` = rf,
                 Kat. = "3C", Empfehlung = "Test",
                 check.names = FALSE, stringsAsFactors = FALSE)
    }
    eingaben <- list()
    for (i in seq_along(c("5c", "6c"))) {
      klasse <- c("5c", "6c")[i]
      d <- if (i == 1) bauen(klasse, rf5, we5) else bauen(klasse, rf6, we6)
      pfad <- file.path(getwd(), paste0("paar_", klasse, ".tsv"))
      readr::write_tsv(d, pfad)
      eingaben[[i]] <- list(datapath = pfad, name = basename(pfad),
                            size = file.size(pfad), type = "text/tab-separated-values")
    }
    eingaben
  }

  # zweiter Durchgang: zwei Jahrgaenge -> die mittlere Entwicklung erscheint
  mit_referenz({
    shiny::testServer(server, {
      session$setInputs(numItems = "40", klassenstufe = "5", klBuchstabe = "c",
                        cbWEDiff = FALSE, cbAllCombined = TRUE,
                        siPlotType = "Histogramm", siStufeAlt = "5", siStufeNeu = "6")
      for (eingabe in klassenpaar()) {
        ohne_bekannte_warnungen(session$setInputs(input_tsv = eingabe))
      }

      ohne_bekannte_warnungen(session$setInputs(cbVergleich = TRUE))
      rf <- als_text(output$vergleichKlasse)
      we <- als_text(output$vergleichKlasseWE)
      expect_match(rf, "R/F-Werten der Klassen", fixed = TRUE)
      expect_match(rf, "Mittlere Entwicklung R/F (5 \u2192 6)", fixed = TRUE)
      expect_match(rf, "(erwartet", fixed = TRUE)
      expect_match(we, "Mittlere Entwicklung WE (5 \u2192 6)", fixed = TRUE)
      expect_false(grepl("Mittlere Entwicklung WE", rf, fixed = TRUE))

      # Gesamtuebersicht aus -> eine Zeile je Kohorte
      ohne_bekannte_warnungen(session$setInputs(cbAllCombined = FALSE))
      expect_match(als_text(output$vergleichKlasse), "5c\u21926c", fixed = TRUE)
    })
  })
})
