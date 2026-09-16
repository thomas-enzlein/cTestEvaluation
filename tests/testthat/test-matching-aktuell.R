# Matching in der aktuellen Fassung (functions/fuzzyMatch.R).
# Diese Tests halten den Ist-Zustand fest: der Umbau in Paket E (Matching-
# Schicht functions/cohort.R) darf daran nichts unbeabsichtigt aendern.

test_that("normalize_name vereinheitlicht Schreibweisen", {
  # normalize_name() liefert einen benannten Vektor (sapply) - hier uninteressant
  expect_equal(unname(normalize_name("Muster, Max")), "max muster")
  expect_equal(unname(normalize_name("MUSTER, max")), "max muster")
})

test_that("normalize_name behandelt Satzzeichen und Leerzeichen robust", {
  skip(paste("Bekannter Fehler, wird in Paket E durch die neue Matching-Schicht",
             "ersetzt: normalize_name() entfernt das Komma erst nach dem",
             "Zusammenfassen der Leerzeichen. Bei 'Muster , Max' bleibt ein leerer",
             "Namensbestandteil uebrig (' max muster'), bei 'Muster,Max' verschmelzen",
             "Vor- und Nachname ('mustermax') - beides verschiebt die Distanz."))
  expect_equal(unname(normalize_name("  Muster ,   Max ")), "max muster")
  expect_equal(unname(normalize_name("Muster,Max")), "max muster")
})

test_that("aktuelles Matching findet exakte, vertauschte und Sonderzeichen-Varianten", {
  df <- dplyr::bind_rows(lade_fixture("klasse_5c.tsv"), lade_fixture("klasse_6c.tsv"))
  m <- fuzzy_match_names(df, var_name = "WE-%", max_dist = 3)

  # 7 von 9 Kindern werden zugeordnet:
  # "Probst, Ella" ist nicht mehr in der Klasse, "Neu, Nino" ist neu.
  expect_equal(nrow(m), 7)
  expect_setequal(m$Name_Alt,
                  c("Testmann, Anna", "Beispiel, Ben", "Muster, Clara", "Winter, Jonas",
                    "Meyer-Lange, Ben-Elias", "Aydin, Sara", "Krüger, Tim"))
  expect_true(is.numeric(m$Wert_Alt))
  expect_true(is.numeric(m$Wert_Neu))

  # exakter Treffer: keine Distanz
  anna <- m[m$Name_Alt == "Testmann, Anna", ]
  expect_equal(anna$Distanz, 0)
  expect_equal(anna$Wert_Alt, 85)
  expect_equal(anna$Wert_Neu, 92.5)

  # Verschlechterung erscheint als negative Differenz
  sara <- m[m$Name_Alt == "Aydin, Sara", ]
  expect_equal(sara$Wert_Neu - sara$Wert_Alt, -10)

  # bekannte Grenze: ein fehlender Zweitname bei langem Namen wird nicht erkannt
  expect_false("van der Berg Kessler, Diego Armando" %in% m$Name_Alt)
})

test_that("plot_veraenderung zeichnet den Zwei-Klassen-Fall (mit Fuzzy-Matching)", {
  df <- dplyr::bind_rows(lade_fixture("klasse_5c.tsv"), lade_fixture("klasse_6c.tsv"))
  p <- plot_veraenderung(df, "WE-%")
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("plot_veraenderung zeichnet den Verlauf bei drei Stufen", {
  df <- dplyr::bind_rows(lade_fixture("klasse_5c.tsv"),
                         lade_fixture("klasse_6c.tsv"),
                         dplyr::mutate(lade_fixture("klasse_6c.tsv"), Klasse = "7c"))
  expect_warning(p <- plot_veraenderung(df, "WE-%"), "Fuzzy-Matching")
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("Entwicklung vergleicht 9a (aelter) mit 10a (neuer)", {
  skip(paste("Bekannter Fehler, Behebung in Paket D/E: fuzzy_match_names() sortiert",
             "Klassen alphabetisch, dadurch gilt '10a' als aelter als '9a' und die",
             "Richtung des Vergleichs wird vertauscht."))
  df <- dplyr::bind_rows(dplyr::mutate(lade_fixture("klasse_5c.tsv"), Klasse = "9a"),
                         dplyr::mutate(lade_fixture("klasse_6c.tsv"), Klasse = "10a"))
  m <- fuzzy_match_names(df, var_name = "WE-%", max_dist = 3)
  expect_true(all(m$Klasse_Alt == "9a"))
  expect_true(all(m$Klasse_Neu == "10a"))
})

test_that("Entwicklung laeuft auch ohne Fuzzy-Matching", {
  skip(paste("Bekannter Fehler, Behebung in Paket D: der Zweig use_fuzzy = FALSE",
             "plottet Name_Display, das nur im Fuzzy-Zweig erzeugt wird."))
  df <- dplyr::bind_rows(lade_fixture("klasse_5c.tsv"), lade_fixture("klasse_6c.tsv"))
  p <- plot_veraenderung(df, "WE-%", use_fuzzy = FALSE)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})
