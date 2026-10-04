# Auswahl der Klassen fuer die Elternbriefe: die Logik laeuft ohne Oberflaeche
# und ist deshalb hier direkt pruefbar. Der Knopf im Tab "Elternbrief" erzeugt
# die Briefe fuer eine Klasse, einen Jahrgang oder alle geladenen Klassen;
# vorausgewaehlt ist die hoechste Klasse.

test_that("brief_klassen liefert die geladenen Klassen sortiert und eindeutig", {
  df <- dplyr::bind_rows(lade_fixture("klasse_5c.tsv"), lade_fixture("klasse_6c.tsv"))
  expect_equal(brief_klassen(df), c("5c", "6c"))

  # leere Tabelle und fehlende Spalte: keine Klassen
  expect_equal(brief_klassen(leere_tabelle()), character(0))
  expect_equal(brief_klassen(data.frame(Name = "Test")), character(0))

  # leere und fehlende Klassennamen fallen heraus
  df$Klasse <- as.character(df$Klasse)
  df$Klasse[1:2] <- c("", NA)
  expect_equal(brief_klassen(df), c("5c", "6c"))
})

test_that("brief_jahrgang liest die Stufe aus dem Klassennamen", {
  expect_equal(brief_jahrgang(c("5c", "6a", "10b")), c(5, 6, 10))
  expect_true(is.na(brief_jahrgang("ohne Ziffer")))
})

test_that("brief_auswahl_liste bietet alle Klassen, je Jahrgang und einzeln an", {
  auswahl <- brief_auswahl_liste(c("6c", "5a", "5b"))

  # Reihenfolge: erst "alle", dann die Jahrgaenge, dann die Klassen
  expect_equal(unname(auswahl[1]), "alle")
  expect_equal(names(auswahl)[1], "Alle geladenen Klassen")
  expect_equal(unname(auswahl["Jahrgang 5 (5a, 5b)"]), "jahrgang:5")
  expect_equal(unname(auswahl["Jahrgang 6 (6c)"]), "jahrgang:6")
  expect_equal(unname(auswahl[c("5a", "5b", "6c")]), c("5a", "5b", "6c"), ignore_attr = TRUE)
  expect_equal(sort(unname(auswahl)),
               c("5a", "5b", "6c", "alle", "jahrgang:5", "jahrgang:6"))

  # ohne Klassen gibt es nichts anzubieten
  expect_length(brief_auswahl_liste(character(0)), 0)
})

test_that("brief_vorauswahl nimmt die hoechste Klasse, bei Gleichstand die erste", {
  # Beispiel aus der Rueckmeldung: 5a und 6a geladen -> 6a
  expect_equal(brief_vorauswahl(c("5a", "6a")), "6a")
  # gleicher Jahrgang: alphanumerisch der erste
  expect_equal(brief_vorauswahl(c("5b", "5a", "6a")), "6a")
  expect_equal(brief_vorauswahl(c("5c", "5b", "5a")), "5a")
  expect_equal(brief_vorauswahl(c("6b", "6a")), "6a")
  # Klassennamen ohne Ziffer: trotzdem eine Auswahl
  expect_equal(brief_vorauswahl(c("b", "a")), "a")
  expect_null(brief_vorauswahl(character(0)))
})

test_that("brief_daten schraenkt auf Klasse, Jahrgang oder alles ein", {
  df <- dplyr::bind_rows(lade_fixture("klasse_5c.tsv"), lade_fixture("klasse_6c.tsv"))
  anzahl_5c <- sum(as.character(df$Klasse) == "5c")
  anzahl_6c <- sum(as.character(df$Klasse) == "6c")
  expect_gt(anzahl_5c, 0)
  expect_gt(anzahl_6c, 0)

  # alle und ohne Auswahl: alles
  expect_equal(nrow(brief_daten(df, "alle")), nrow(df))
  expect_equal(nrow(brief_daten(df, NULL)), nrow(df))

  # ein Jahrgang
  nur5 <- brief_daten(df, "jahrgang:5")
  expect_equal(nrow(nur5), anzahl_5c)
  expect_setequal(unique(as.character(nur5$Klasse)), "5c")
  expect_equal(nrow(brief_daten(df, "jahrgang:6")), anzahl_6c)

  # eine Klasse
  eins <- brief_daten(df, "6c")
  expect_equal(nrow(eins), anzahl_6c)
  expect_setequal(unique(as.character(eins$Klasse)), "6c")

  # unbekannte Auswahl: keine Zeilen (die App meldet das)
  expect_equal(nrow(brief_daten(df, "9z")), 0)
  expect_equal(nrow(brief_daten(df, "jahrgang:9")), 0)

  # die Eingabetabelle bleibt unveraendert
  expect_equal(nrow(df), anzahl_5c + anzahl_6c)
})

test_that("brief_buchstaben waehlt fuer den Entwicklungsbrief ganze Kohorten", {
  df <- dplyr::bind_rows(lade_fixture("klasse_5c.tsv"), lade_fixture("klasse_6c.tsv"))
  klassen <- brief_klassen(df)

  # ohne Einschraenkung bleibt alles (leerer Vektor)
  expect_equal(brief_buchstaben(klassen, "alle"), character(0))
  expect_equal(brief_buchstaben(klassen, NULL), character(0))

  # eine Klasse steht fuer ihren Buchstaben - in beiden Jahrgaengen
  expect_equal(brief_buchstaben(klassen, "6c"), "c")

  # ein Jahrgang sind die Buchstaben seiner Klassen
  expect_equal(brief_buchstaben(c("5a", "5b", "6b"), "jahrgang:5"), c("a", "b"))
  expect_equal(brief_buchstaben(c("5a", "5b", "6b"), "jahrgang:6"), "b")
})

test_that("brief_daten_buchstaben behaelt beide Jahrgaenge der Auswahl", {
  df <- dplyr::bind_rows(lade_fixture("klasse_5c.tsv"), lade_fixture("klasse_6c.tsv"))

  # ohne Buchstaben unveraendert
  expect_equal(nrow(brief_daten_buchstaben(df, character(0))), nrow(df))

  nur_c <- brief_daten_buchstaben(df, "c")
  expect_setequal(unique(as.character(nur_c$Klasse)), c("5c", "6c"))

  # ein Buchstabe, den es nicht gibt: keine Zeilen (die App meldet das)
  expect_equal(nrow(brief_daten_buchstaben(df, "z")), 0)
})

test_that("die Auswahl im Infobrief richtet sich nach der Briefart", {
  shiny::testServer(server, {
    session$setInputs(numItems = "40", klassenstufe = "5", klBuchstabe = "c",
                      cbWEDiff = FALSE, cbAllCombined = TRUE,
                      siPlotType = "Histogramm")
    for (datei in c("klasse_5c.tsv", "klasse_6c.tsv")) {
      ohne_bekannte_warnungen(session$setInputs(input_tsv = tsv_input(datei)))
    }

    # Elternbrief: Klassen, hoechste vorausgewaehlt (6c)
    html <- als_text(output$briefAuswahlUI)
    expect_match(html, "Alle geladenen Klassen", fixed = TRUE)
    expect_match(html, "Jahrgang 5 (5c)", fixed = TRUE)
    expect_match(html, "Jahrgang 6 (6c)", fixed = TRUE)
    expect_match(html, 'value="6c" selected', fixed = TRUE)
    expect_false(grepl('value="5c" selected', html, fixed = TRUE))

    # Stand-Brief: dieselben Eintraege wie im Elternbrief
    ohne_bekannte_warnungen(session$setInputs(siBrieftyp = "stand"))
    html <- als_text(output$infoAuswahlUI)
    expect_match(html, "Klassen für den Brief", fixed = TRUE)
    expect_match(html, "Alle geladenen Klassen", fixed = TRUE)
    expect_match(html, "Jahrgang 6 (6c)", fixed = TRUE)
    expect_match(html, 'value="6c" selected', fixed = TRUE)

    # Entwicklungsbrief: Kohorten statt Klassen, Paar aus der Stufenauswahl
    ohne_bekannte_warnungen(
      session$setInputs(siBrieftyp = "entwicklung", siStufeAlt = 5, siStufeNeu = 6))
    html <- als_text(output$infoAuswahlUI)
    expect_match(html, "Kohorten für den Brief", fixed = TRUE)
    expect_match(html, "Alle Kohorten", fixed = TRUE)
    expect_match(html, "c: 5c", fixed = TRUE)          # Label der Kohorte
    expect_match(html, "6c", fixed = TRUE)
    expect_match(html, 'value="c" selected', fixed = TRUE)
    # keine Klassen- und keine Jahrgangseintraege mehr
    expect_false(grepl('value="6c"', html, fixed = TRUE))
    expect_false(grepl("Jahrgang", html, fixed = TRUE))

    # eine Kohorte, die es nur in einem Jahr gibt, wird gekennzeichnet
    ohne_bekannte_warnungen(session$setInputs(siStufeAlt = 5, siStufeNeu = 7))
    html <- als_text(output$infoAuswahlUI)
    expect_match(html, "c (nur 5c)", fixed = TRUE)
  })
})
