# Ergebnistabelle des Elternbriefes: Blatt "Tabelle2" der Kategorie-Datei ist
# die Vorlage (Kopfzeile, Zeilen, verbundene Zellen). Fehlt das Blatt - wie in
# den persoenlichen Kopien, die vorher angelegt wurden - wird dieselbe
# Darstellung aus der Zuordnungstabelle abgeleitet.

# Verbundene Bereiche eines flextable: data.frame(von, bis, spalte)
verbunde_von <- function(ft) {
  spannen <- ft$body$spans$columns
  treffer <- which(spannen > 1, arr.ind = TRUE)
  if (nrow(treffer) == 0) {
    return(data.frame(von = integer(0), bis = integer(0), spalte = integer(0)))
  }
  data.frame(von = treffer[, 1], bis = treffer[, 1] + spannen[treffer] - 1,
             spalte = treffer[, 2], row.names = NULL)
}

test_that("die Ergebnistabelle kommt aus dem Blatt Tabelle2", {
  withr::with_dir(projekt_root, {
    ft <- ergebnis_tabelle(file.path(projekt_root, "vorlagen", "ergebnisse.xlsx"))
  })

  # Ueberschrift "Kategorie" statt der internen Spalte, ohne die Zeile "0"
  expect_equal(names(ft$body$dataset), c("Kategorie", "Bedeutung", "Handlungsempfehlung"))
  expect_equal(nrow(ft$body$dataset), 12)
  expect_equal(as.character(ft$body$dataset$Kategorie[1]), "A1")
  expect_false(any(grepl("nicht teilgenommen", ft$body$dataset$Bedeutung, fixed = TRUE)))

  # die drei Verbünde aus der Datei
  expect_equal(verbunde_von(ft),
               data.frame(von = c(1L, 6L, 9L), bis = c(4L, 7L, 12L), spalte = 3L))
})

test_that("ohne Blatt Tabelle2 entsteht dieselbe Tabelle aus der Zuordnung", {
  withr::with_tempdir({
    # nur die Zuordnungstabelle: der Fall der persoenlichen Kopien
    alt <- as.data.frame(readxl::read_xlsx(file.path(projekt_root, "vorlagen",
                                                     "ergebnisse.xlsx"),
                                           sheet = "Tabelle1"))
    pfad <- file.path(getwd(), "ergebnisse.xlsx")
    openxlsx::write.xlsx(alt, pfad, sheetName = "Tabelle1")

    ft <- ergebnis_tabelle(pfad)
    expect_equal(names(ft$body$dataset), c("Kategorie", "Bedeutung", "Handlungsempfehlung"))
    expect_equal(nrow(ft$body$dataset), 12)
    # gleiche Verbünde wie im Blatt Tabelle2
    expect_equal(verbunde_von(ft),
                 data.frame(von = c(1L, 6L, 9L), bis = c(4L, 7L, 12L), spalte = 3L))
  })
})

test_that("eine unbrauchbare Kategorie-Datei wird gemeldet", {
  withr::with_tempdir({
    pfad <- file.path(getwd(), "ergebnisse.xlsx")
    writeLines("kein xlsx", pfad, useBytes = TRUE)
    expect_error(ergebnis_tabelle(pfad), "nicht lesbar")
  })
})
