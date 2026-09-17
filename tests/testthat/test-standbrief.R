# Paket M: Stand-Brief - eine Seite je Klasse in EINEM Dokument.
#
# Anders als der Entwicklungsbrief braucht er keine Zuordnung: er beschreibt den
# aktuellen Stand einer Klasse (nach dem Ersttest in der 5 genauso wie nach dem
# Re-Test in der 6). Der Fliesstext steht in infobrief/stand.Rmd; hier werden die
# Zulieferungen geprueft (Kennzahlen, Kategorien, Normbereich, Listen) plus ein
# echtes docx-Rendering.

# Schueler-Tabelle in der Struktur von rv$df. Ohne eigene Kategorien werden sie
# wie in der App aus den Werten berechnet (fehlende Werte -> "0").
stand_df <- function(Name, Klasse, rf, we = rf, kat = NULL, items = 40) {
  if (is.null(kat)) {
    kat <- ifelse(is.na(rf) | is.na(we), "0",
                  paste0(getRFlevel(rf), getWElevel(rf, we)))
  }
  tibble::tibble(Name = Name, Klasse = Klasse,
                 `WE-Wert` = we, `WE-%` = we,
                 `R/F-Wert` = rf, `R/F-%` = rf,
                 Kat. = kat, Empfehlung = "", Items = items)
}

# Text und XML eines docx (inklusive per altChunk eingebetteter Teile)
stand_docx_xml <- function(datei) {
  z <- tempfile("docx"); dir.create(z)
  utils::unzip(datei, exdir = z)
  xmls <- character(0)
  for (teil in list.files(z, recursive = TRUE, full.names = TRUE,
                          pattern = "\\.docx$")) {
    zz <- tempfile("docx_teil"); dir.create(zz)
    utils::unzip(teil, exdir = zz)
    pfad <- file.path(zz, "word", "document.xml")
    if (file.exists(pfad)) {
      xmls <- c(xmls, paste(readLines(pfad, warn = FALSE, encoding = "UTF-8"),
                            collapse = ""))
    }
  }
  haupt <- file.path(z, "word", "document.xml")
  if (file.exists(haupt)) {
    xmls <- c(paste(readLines(haupt, warn = FALSE, encoding = "UTF-8"),
                    collapse = ""), xmls)
  }
  paste(xmls, collapse = "")
}

stand_docx_text <- function(datei) {
  gsub("\\s+", " ", gsub("<[^>]+>", " ", stand_docx_xml(datei)))
}

# .datei_gesperrt() mit einem Oeffnen, das fehlschlaegt (so verhaelt sich Word)
stand_mit_gesperrter_datei <- function(code) {
  echt <- .datei_gesperrt
  assign(".datei_gesperrt", function(pfad, oeffnen = NULL) TRUE, envir = globalenv())
  on.exit(assign(".datei_gesperrt", echt, envir = globalenv()), add = TRUE)
  force(code)
}

test_that("die Kennzahlen je Klasse stimmen (ohne Vorjahresbezug)", {
  df <- stand_df(Name = c("A", "B", "C", "D"),
                 Klasse = c("5a", "5a", "5a", "5b"),
                 rf = c(80, 60, NA, 50),
                 we = c(90, 70, NA, 50))
  s <- stand_statistik(df)

  expect_equal(s$Klasse, c("5a", "5b"))
  expect_equal(s$Stufe, c(5, 5))
  expect_equal(s$n, c(3, 1))                 # alle Kinder der Klasse
  expect_equal(s$n_werte, c(2, 1))           # Kinder mit mindestens einem Wert
  expect_equal(s$mittel_RF, c(70, 50))
  expect_equal(s$median_RF, c(70, 50))
  expect_equal(s$mittel_WE, c(80, 50))
  expect_equal(s$sd_RF, c(round(stats::sd(c(80, 60)), 1), NA))
  # ein Kind allein hat keine Streuung -> "-" in der Tabelle, kein "NaN"
  expect_true(is.na(s$sd_RF[2]))

  # eine Klasse ohne Werte bleibt in der Statistik, faellt aber im Brief weg
  ohne <- stand_df(Name = c("E", "F"), Klasse = c("6a", "6a"),
                   rf = c(NA, NA), we = c(NA, NA))
  s2 <- stand_statistik(ohne)
  expect_equal(s2$n_werte, 0)
  expect_true(is.na(s2$mittel_RF))
  expect_true(is.na(s2$median_WE))

  expect_equal(nrow(stand_statistik(leere_tabelle())), 0)
})

test_that("die Kategorien werden in vier Gruppen plus Teilnahme eingeordnet", {
  expect_equal(stand_gruppe(c("1A", "2B", "3C", "4C*", "5E", "0", NA, "")),
               c(1L, 1L, 2L, 3L, 4L, 5L, 5L, 5L))
  # unbekannte Werte (z. B. aus einer aelteren Datei) gehen in die letzte Gruppe
  expect_equal(stand_gruppe("9Z"), 6L)
  expect_equal(kat_text(c("1A", "0", NA, "")), c("1A", "-", "-", "-"))

  kats <- c("1A", "1B", "2A", "2B", "1A",          # kein Handlungsbedarf
            "3C", "4C", "5C", "3C", "4C",          # Rechtschreibung ausbaufaehig
            "3C*", "4C*", "5C*", "3C*", "4C*",     # grosser Unterschied WE zu R/F
            "3D", "4D", "5D", "5E",                # Handlungsbedarf
            "0")                                   # nicht teilgenommen
  u <- kategorien_uebersicht(kats)

  expect_equal(colnames(u),
               c("Gruppe", "Kategorien (Kat.)", "Anzahl", "Anteil"))
  expect_equal(u$Gruppe, .stand_gruppen[1:5])
  expect_equal(u[["Kategorien (Kat.)"]],
               c("1A, 1B, 2A, 2B", "3C, 4C, 5C", "3C*, 4C*, 5C*",
                 "3D, 4D, 5D, 4E, 5E", "ohne Werte"))
  expect_equal(u$Anzahl, c("5", "5", "5", "4", "1"))
  expect_equal(u$Anteil, c("25 %", "25 %", "25 %", "20 %", "5 %"))
  # die Anzahlen ergeben zusammen die Klasse
  expect_equal(sum(as.integer(u$Anzahl)), length(kats))
  # "ohne Zuordnung" erscheint nur, wenn es solche Werte wirklich gibt
  expect_false("ohne Zuordnung" %in% u$Gruppe)

  # der Querverweis nennt jede Kategorie genau einmal
  alle <- kategorien_je_gruppe()
  aus_gruppen <- trimws(unlist(strsplit(paste(alle[1:4], collapse = ","), ",")))
  expect_setequal(aus_gruppen, lvls)
  expect_length(aus_gruppen, length(lvls))

  # unbekannte Werte bekommen eine eigene Zeile - die vier Gruppen und
  # "nicht teilgenommen" stehen immer da, damit der Ueberblick vollstaendig ist
  u2 <- kategorien_uebersicht(c("1A", "9Z", "0"))
  expect_equal(u2$Gruppe, .stand_gruppen)
  expect_equal(u2$Anzahl, c("1", "0", "0", "0", "1", "1"))
  expect_equal(u2[["Kategorien (Kat.)"]][6], "unbekannte Werte")

  expect_null(kategorien_uebersicht(character(0)))
})

test_that("ein Abschnitt enthaelt Kennzahlen, Kategorien, Normbereich und Listen", {
  df <- stand_df(Name = c("Zimmer", "Ahrend", "Meyer", "Kraus", "Bode", "Voss"),
                 Klasse = "5a",
                 rf = c(80, 70, 60, 50, 40, NA),
                 we = c(85, 75, 65, 55, 45, NA))
  a <- stand_abschnitt(df, "5a", grenze = 65)

  expect_equal(a$klasse, "5a")
  expect_equal(a$stufe, 5)
  expect_equal(a$anrede, "Liebe Klassenleitung der 5a,")
  expect_equal(a$n, 6)
  expect_equal(a$n_werte, 5)

  # Kennzahlentabelle: ohne Klassenspalte (die Ueberschrift nennt die Klasse)
  expect_equal(colnames(a$tabelle),
               c("n (mit Werten)", "WE % (Mittel \u00b1 SD)",
                 "R/F % (Mittel \u00b1 SD)", "Median WE %", "Median R/F %"))
  expect_equal(a$tabelle[["n (mit Werten)"]], "6 (5)")
  expect_equal(a$tabelle[["Median R/F %"]], "60,0")

  # Kategorien: 1A, 2A (gruen), 3D, 4D, 4D (rot), "0" (nicht teilgenommen)
  expect_equal(a$kategorien$Anzahl, c("2", "0", "0", "3", "1"))
  expect_equal(a$kategorien$Anteil, c("33 %", "0 %", "0 %", "50 %", "17 %"))

  # unterer Normbereich: Grenze kommt von aussen (in der App aus den Einstellungen)
  expect_equal(a$referenz$grenze, "65")
  expect_equal(a$referenz$n, 5)
  expect_equal(a$referenz$unter, 3)
  # keine Namensaufzaehlung im Text - markiert ist die Tabellenzeile
  expect_null(a$referenz$namens_text)

  # hoechste Werte: nach R/F absteigend, ohne Kinder ohne Werte
  expect_equal(nrow(a$top), 5)
  expect_equal(a$top$Name[1], "Zimmer")
  expect_equal(a$top$Name[2], "Ahrend")
  expect_false("Voss" %in% a$top$Name)

  # Markierung "unter dem unteren Normbereich" (Grenze 65)
  expect_equal(a$top_unten, c(FALSE, FALSE, TRUE, TRUE, TRUE))
  erwartet_unten <- (!is.na(df[["R/F-%"]]) & df[["R/F-%"]] < 65)[order(df$Name)]
  expect_equal(a$lese_unten, erwartet_unten)

  # Anhang: alle Kinder der Klasse, alphabetisch, Nicht-Teilnehmer mit "-"
  expect_equal(a$lesetabelle$Name, df$Name[order(df$Name)])
  expect_equal(nrow(a$lesetabelle), 6)
  expect_equal(a$lesetabelle[["Kat."]][a$lesetabelle$Name == "Voss"], "-")
  expect_true(is.na(a$lesetabelle[["R/F %"]][a$lesetabelle$Name == "Voss"]))

  # ohne Werte gibt es keine Liste der hoechsten Werte
  ohne <- stand_df(Name = "E", Klasse = "5a", rf = NA, we = NA)
  expect_null(stand_abschnitt(ohne, "5a")$top)
  expect_null(stand_abschnitt(ohne, "5a")$top_unten)
})

test_that("Kategorien werden wie in App, Excel und Word farbig hinterlegt", {
  # dieselbe Farbskala wie cols/lvls aus global.R
  expect_equal(unname(.kat_farbe(c("1A", "2B", "3C", "3C*", "4D", "5E"))),
               unname(cols[c(1, 4, 5, 6, 10, 15)]))
  # "nicht teilgenommen" und unbekannte Werte bleiben ohne Farbe
  expect_true(all(is.na(.kat_farbe(c("0", "9Z", NA)))))

  tab <- data.frame(Name = c("A", "B"), "WE %" = c(80, 60), "R/F %" = c(70, 50),
                    "Kat." = c("2B", "4D"), check.names = FALSE)
  ft <- tabelle_werte(tab, c(FALSE, TRUE))
  expect_s3_class(ft, "flextable")
  # Kategorie-Zelle gefuellt, die markierte Zeile ist komplett fett (inkl. Name)
  expect_equal(unname(ft$body$styles$cells$background$data[, "Kat."]),
               unname(.kat_farbe(tab$Kat.)))
  expect_equal(unname(ft$body$styles$text$bold$data[1, ]),
               rep(FALSE, ncol(tab)))
  expect_true(all(ft$body$styles$text$bold$data[2, ]))
  expect_true(all(ft$body$styles$cells$background$data[, "Name"] == "transparent"))

  # die Gruppenzeilen der Uebersicht tragen die Farben der Skala
  u <- kategorien_uebersicht(c("1A", "3C", "3C*", "4E", "0"))
  ft2 <- tabelle_kategorien(u)
  expect_equal(unname(ft2$body$styles$cells$background$data[, "Gruppe"]),
               c("darkgreen", "#FFA500", "#CD8500", "orangered", "transparent"))
  # der Querverweis auf die Kategorien bleibt ungefaerbt
  expect_true(all(ft2$body$styles$cells$background$data[, "Kategorien (Kat.)"] ==
                    "transparent"))
})

test_that("Klassen ohne Werte werden uebersprungen", {
  df <- stand_df(Name = c("A", "B"), Klasse = c("5a", "5b"),
                 rf = c(70, NA), we = c(75, NA))
  b <- stand_bericht(df, grenze = 65)

  expect_equal(b$klassen, "5a")
  expect_equal(b$uebersprungen, "5b")
  expect_length(b$abschnitte, 1)
  expect_equal(b$abschnitte[[1]]$klasse, "5a")

  ohne <- stand_df(Name = "E", Klasse = "6a", rf = NA, we = NA)
  expect_length(stand_bericht(ohne)$abschnitte, 0)
  expect_equal(stand_bericht(ohne)$uebersprungen, "6a")
  expect_error(create_standbrief(ohne, absender = "Test, Tina"),
               "Keine Kinder mit Werten", fixed = TRUE)
})

test_that("die Anrede wird aus den Klassen gebildet", {
  expect_equal(infobrief_anrede("6c"), "Liebe Klassenleitung der 6c,")
  expect_equal(infobrief_anrede(c("6b", "6c")),
               "Liebe Klassenleitungen der 6b und 6c,")
  expect_equal(infobrief_anrede(c("6c", "6b")),
               "Liebe Klassenleitungen der 6b und 6c,")   # sortiert
  expect_equal(infobrief_anrede(c("6c", "6a", "6b")),
               "Liebe Klassenleitungen der 6a, 6b und 6c,")
  expect_equal(infobrief_anrede(character(0)), "Liebe Kollegin, lieber Kollege,")
  expect_equal(infobrief_anrede(NULL), "Liebe Kollegin, lieber Kollege,")
  expect_equal(infobrief_anrede(""), "Liebe Kollegin, lieber Kollege,")

  # der Entwicklungsbrief nennt die Klasse, in der die Kinder heute sitzen
  df <- dplyr::bind_rows(lade_fixture("klasse_5c.tsv"), lade_fixture("klasse_6c.tsv"))
  k <- build_cohort(df, 5, 6)
  expect_equal(klassen_fuer_anrede(k), "6c")
})

test_that("der Stand-Brief entsteht als EINE docx mit einer Seite je Klasse", {
  skip_if_not(rmarkdown::pandoc_available(), "pandoc nicht gefunden")

  withr::with_tempdir({
    dir.create("infobrief")
    dateien <- list.files(file.path(projekt_root, "infobrief"), full.names = TRUE)
    file.copy(dateien, "infobrief", recursive = TRUE)
    withr::local_options(ctest.outdir.fallback = file.path(getwd(), "benutzer"))

    df <- dplyr::bind_rows(lade_fixture("klasse_5c.tsv"), lade_fixture("klasse_6c.tsv"))

    meldungen <- list()
    ergebnis <- NULL
    utils::capture.output(
      suppressMessages(
        ergebnis <- create_standbrief(df, absender = "Test, Tina",
                                      fortschritt = function(anteil, text) {
                                        meldungen[[length(meldungen) + 1]] <<-
                                          list(anteil = anteil, text = text)
                                      })
      )
    )

    anteile <- vapply(meldungen, function(m) m$anteil, numeric(1))
    expect_gt(length(anteile), 1)
    expect_true(all(diff(anteile) >= 0))        # nur vorwaerts
    expect_equal(anteile[length(anteile)], 1)    # endet bei 100 %

    expect_true(file.exists(ergebnis$datei))
    expect_match(basename(ergebnis$datei), "^Infobrief_Stand_5c_6c\\.docx$")
    # ein Dokument, nicht mehrere
    expect_length(list.files(dirname(ergebnis$datei), pattern = "^Infobrief_Stand_.*\\.docx$"), 1)
    expect_equal(ergebnis$bericht$klassen, c("5c", "6c"))

    text <- stand_docx_text(ergebnis$datei)
    xml <- stand_docx_xml(ergebnis$datei)
    # beide Klassen stehen im selben Dokument, jede mit eigener Anrede
    expect_match(text, "Lernstand im Fach Deutsch", fixed = TRUE)
    expect_match(text, "Klasse 5c", fixed = TRUE)
    expect_match(text, "Klasse 6c", fixed = TRUE)
    expect_match(text, "Liebe Klassenleitung der 5c,", fixed = TRUE)
    expect_match(text, "Liebe Klassenleitung der 6c,", fixed = TRUE)
    expect_match(text, "Test, Tina", fixed = TRUE)
    # Kennzahlen, Kategorien-Uebersicht mit den vier Gruppen, Normbereich
    expect_match(text, "n (mit Werten)", fixed = TRUE)
    expect_match(text, "Median R/F %", fixed = TRUE)
    expect_match(text, "Ergebnisse im Überblick", fixed = TRUE)
    expect_match(text, "kein Handlungsbedarf", fixed = TRUE)
    expect_match(text, "Rechtschreibung ausbaufähig", fixed = TRUE)
    expect_match(text, "großer Unterschied WE zu R/F", fixed = TRUE)
    expect_match(text, "Handlungsbedarf (Rechtschreibung und Wortschatz)", fixed = TRUE)
    expect_match(text, "nicht teilgenommen", fixed = TRUE)
    # Querverweis: welche Kategorien in welcher Gruppe landen
    expect_match(text, "Kategorien (Kat.)", fixed = TRUE)
    expect_match(text, "1A, 1B, 2A, 2B", fixed = TRUE)
    expect_match(text, "3C, 4C, 5C", fixed = TRUE)
    expect_match(text, "3C*, 4C*, 5C*", fixed = TRUE)
    expect_match(text, "3D, 4D, 5D, 4E, 5E", fixed = TRUE)
    expect_match(text, "unter dem unteren Normbereich", fixed = TRUE)
    # keine Namensaufzaehlung im Text: die Zeilen in der Tabelle sind fett
    expect_false(grepl("Betroffen sind", text, fixed = TRUE))
    expect_match(text, "liegen mit ihrem R/F-Wert unter dem unteren Normbereich",
                 fixed = TRUE)
    expect_match(text, "Die höchsten Werte", fixed = TRUE)
    # Anhang je Klasse mit Einzelwerten, aber ohne Vergleichsspalten
    expect_match(text, "Anhang: Übersicht aller Kinder der Klasse 5c", fixed = TRUE)
    expect_match(text, "Anhang: Übersicht aller Kinder der Klasse 6c", fixed = TRUE)
    expect_false(grepl("\u0394 R/F", text, fixed = TRUE))
    expect_false(grepl("Kohorte", text, fixed = TRUE))
    expect_false(grepl("Vorjahr", text, fixed = TRUE))

    # Kategorien sind farbig hinterlegt (dieselben Farben wie im Excel-Export)
    for (farbe in c("006400", "90EE90", "FFA500", "CD8500", "FF4500", "FF0000")) {
      expect_match(xml, paste0('w:fill="', farbe, '"'), fixed = TRUE)
    }
    # Kinder unter dem unteren Normbereich sind fett und werden erklaert
    expect_match(text, "Fett gedruckt sind Kinder", fixed = TRUE)
    expect_match(xml, '<w:b w:val="true"/>', fixed = TRUE)
    # Tabellen haben Luft: hoehere Zeilen und Vor-/Nachlauf
    expect_false(grepl('w:trHeight w:val="360"', xml, fixed = TRUE))
    expect_match(xml, 'w:trHeight w:val="4[0-9][0-9]"')
    expect_match(xml, "w:before=", fixed = TRUE)
    expect_match(xml, "w:after=", fixed = TRUE)
  })
})

test_that("die Briefart steuert, welcher Brief erstellt wird", {
  withr::with_tempdir({
    dir.create("Auswertungen")
    shiny::testServer(server, {
      session$setInputs(numItems = "40", klassenstufe = "5", klBuchstabe = "c",
                        cbWEDiff = FALSE, cbAllCombined = TRUE,
                        siPlotType = "Histogramm", siBrieftyp = "stand")
      ohne_bekannte_warnungen(session$setInputs(input_tsv = tsv_input("klasse_5c.tsv")))

      # ohne Werte wird nicht gerendert, sondern sauber gemeldet - und zwar
      # OHNE die Forderung nach zwei Jahrgaengen (die gilt nur fuer die Entwicklung)
      rv$df[["WE-%"]] <- NA_real_
      rv$df[["R/F-%"]] <- NA_real_
      ohne_bekannte_warnungen(session$setInputs(btInfobrief = 1))

      meldung <- als_text(output$text)
      expect_match(meldung, "keine Kinder mit Werten", fixed = TRUE)
      expect_false(grepl("zwei Stufen", meldung, fixed = TRUE))
    })
  })
})

test_that("auch der Stand-Brief prueft die Zieldatei vor dem Rendern", {
  withr::with_tempdir({
    dir.create("infobrief")
    dateien <- list.files(file.path(projekt_root, "infobrief"), full.names = TRUE)
    file.copy(dateien, "infobrief", recursive = TRUE)
    dir.create("Auswertungen")
    withr::local_options(ctest.outdir.fallback = file.path(getwd(), "benutzer"))

    df <- dplyr::bind_rows(lade_fixture("klasse_5c.tsv"), lade_fixture("klasse_6c.tsv"))
    ziel <- file.path(getwd(), "Auswertungen", "Infobrief_Stand_5c_6c.docx")

    fehler <- stand_mit_gesperrter_datei(
      tryCatch({
        utils::capture.output(suppressMessages(
          create_standbrief(df, absender = "Test, Tina")))
        NULL
      }, error = function(e) conditionMessage(e)))

    expect_false(is.null(fehler))
    expect_match(fehler, "Infobrief_Stand_5c_6c.docx", fixed = TRUE)
    expect_match(fehler, "geöffnet oder schreibgeschützt", fixed = TRUE)
    expect_false(file.exists(ziel))
  })
})
