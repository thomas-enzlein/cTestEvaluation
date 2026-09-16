# Rechenkern: Kategorien, Empfehlungen, Eingabepruefung, Hilfsfunktionen.
# Diese Tests dokumentieren das aktuelle Verhalten und schlagen an, wenn
# sich Schwellenwerte oder Texte unbeabsichtigt aendern.

test_that("getRFlevel trifft die Grenzwerte", {
  expect_equal(getRFlevel(100), 1)
  expect_equal(getRFlevel(71.3), 1)
  expect_equal(getRFlevel(71.2), 2)
  expect_equal(getRFlevel(66.3), 2)
  expect_equal(getRFlevel(66.2), 3)
  expect_equal(getRFlevel(56.3), 3)
  expect_equal(getRFlevel(56.2), 4)
  expect_equal(getRFlevel(36.2), 4)
  expect_equal(getRFlevel(36.1), 5)
  expect_equal(getRFlevel(0), 5)
  expect_equal(getRFlevel(NA_real_), 0)
})

test_that("getWElevel liefert die Buchstaben A bis E", {
  expect_equal(getWElevel(70, 85), "B")   # hoher R/F-Wert, grosse Differenz
  expect_equal(getWElevel(70, 75), "A")   # hoher R/F-Wert, kleine Differenz
  expect_equal(getWElevel(65, 75), "C")   # Normbereich
  expect_equal(getWElevel(50, 70), "C*")  # Verdacht auf LRS
  expect_equal(getWElevel(50, 60), "E")   # tiefe R/F-Stufe, grosse Differenz
  expect_equal(getWElevel(50, 58), "D")   # tiefe R/F-Stufe, kleine Differenz
  expect_equal(getWElevel(20, 40), "E")
  expect_equal(getWElevel(NA_real_, NA_real_), "")
})

test_that("jede erreichbare Kategorie ist definiert und hat eine Empfehlung", {
  raster <- expand.grid(rf = seq(0, 100, by = 0.5), we = seq(0, 100, by = 0.5))
  raster <- raster[raster$we >= raster$rf, ]
  kats <- unique(paste0(getRFlevel(raster$rf), getWElevel(raster$rf, raster$we)))
  expect_true(all(kats %in% lvls))
  expect_false(any(is.na(getRecommendation(kats))))
  expect_true(all(nzchar(getRecommendation(kats))))
})

test_that("checkInputErrors meldet genau die erwarteten Fehler", {
  expect_equal(checkInputErrors("", 10, 10, 40, "5c"), "Bitte Namen eingeben.")
  expect_equal(checkInputErrors("   ", 10, 10, 40, "5c"), "Bitte Namen eingeben.")
  expect_equal(checkInputErrors("Test, T", 10, NA, 40, "5c"),
               "Bitte beide Werte angeben oder keinen (Schüler hat nicht teilgenommen).")
  expect_equal(checkInputErrors("Test, T", 30, 20, 40, "5c"),
               "R/F-Wert kann nicht größer als WE-Wert sein.")
  expect_equal(checkInputErrors("Test, T", 41, 45, 40, "5c"),
               "R/F- bzw WE-Wert kann nicht höher als Anzahl Test-Items sein.")
  expect_equal(checkInputErrors("Test, T", 10, 10, 40, NULL),
               "Bitte Klassenstufe und Klasse angeben.")
  expect_null(checkInputErrors("Test, T", 10, 12, 40, "5c"))
  # Schueler hat nicht teilgenommen (beide Werte leer) ist erlaubt
  expect_null(checkInputErrors("Test, T", NA, NA, 40, "5c"))
})

test_that("composeClass baut die Klasse zusammen", {
  expect_equal(composeClass("5", "c"), "5c")
  expect_equal(composeClass("13", "h"), "13h")
  expect_null(composeClass("5", ""))
  expect_null(composeClass(NA, "c"))
})

test_that("ohne gewaehlte Klassenstufe wird keine Klasse gebildet", {
  # sonst entstuende "c" ohne Jahrgang - im Elternbrief "der NA. Klasse"
  expect_null(composeClass("", "c"))
  expect_null(composeClass("   ", "c"))
  expect_equal(checkInputErrors("Test, T", 10, 12, 40, composeClass("", "c")),
               "Bitte Klassenstufe und Klasse angeben.")
})

test_that("addEntry rechnet Prozentwerte, Kategorie und Empfehlung", {
  df <- addEntry(leere_tabelle(), name = "Testmann, Anna", klasse = "5c",
                 rf = 28, we = 34, numItems = 40)
  expect_equal(nrow(df), 1)
  expect_equal(df$`WE-%`, 85)
  expect_equal(df$`R/F-%`, 70)
  expect_equal(as.character(df$`Kat.`), "2B")
  # Hinweis: die Empfehlung traegt aktuell den Namen der Kategorie als
  # names-Attribut (getRecommendation() indiziert einen benannten Vektor).
  # Kosmetisch, Aufraeumen als Kleinigkeit in Paket D vorgesehen.
  expect_equal(unname(df$Empfehlung),
               "Ergebnis oberhalb des Normbereichs. Kein Handlungsbedarf im Bereich Rechtschreibung und Wortschatz.")

  # Rundung auf eine Nachkommastelle
  df2 <- addEntry(leere_tabelle(), name = "Aydin, Sara", klasse = "5c",
                  rf = 17, we = 20, numItems = 40)
  expect_equal(df2$`R/F-%`, 42.5)
  expect_equal(df2$`WE-%`, 50)

  # Schueler hat nicht teilgenommen
  df3 <- addEntry(leere_tabelle(), name = "Probst, Ella", klasse = "5c",
                  rf = NA, we = NA, numItems = 40)
  expect_equal(as.character(df3$`Kat.`), "0")
  expect_equal(unname(df3$Empfehlung), "Hat nicht teilgenommen.")
  expect_true(is.na(df3$`WE-%`) && is.na(df3$`R/F-%`))
})

test_that("convert_kat_meaning liefert je Kategorie genau einen Text", {
  # liest elternbrief/ergebnisse.xlsx, also relativ zum Projektordner
  withr::with_dir(projekt_root, {
    for (kat in lvls) {
      txt <- convert_kat_meaning(kat)
      expect_length(txt, 1)
      expect_true(nzchar(txt))
    }
    expect_match(convert_kat_meaning("1A"), "^A1: ")
    expect_match(convert_kat_meaning("4D"), "^D4: ")
    # Sternchen-Kategorien ergeben bewusst denselben Elternbrief-Text
    expect_identical(convert_kat_meaning("3C"), convert_kat_meaning("3C*"))
    expect_match(convert_kat_meaning("0"), "nicht teilgenommen")
  })
})

test_that("createStatsText gibt Mittelwert, Median und Anzahl aus", {
  df <- dplyr::bind_rows(lade_fixture("klasse_5c.tsv"), lade_fixture("klasse_6c.tsv"))

  einzeln <- as.character(createStatsText(df, "WE-%", "WE", multiple = FALSE))
  expect_match(einzeln, "Mittelwert WE:")
  expect_match(einzeln, "Median WE:")
  expect_match(einzeln, "Anzahl: 18")

  je_klasse <- as.character(createStatsText(df, "R/F-%", "R/F", multiple = TRUE))
  expect_match(je_klasse, "Mittelwert R/F:")
  expect_match(je_klasse, "5c")
  expect_match(je_klasse, "6c")
})

test_that("checkColumnNames prueft die Spalten der Uebersichtstabelle", {
  df <- lade_fixture("klasse_5c.tsv")
  expect_true(checkColumnNames(df, df))
  expect_false(checkColumnNames(df, df[, 1:3]))
})
