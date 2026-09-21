# Paket G: Verlaufsdiagramm und Export der Vergleichstabelle.

cohort_fixture_g <- function() {
  df <- dplyr::bind_rows(lade_fixture("klasse_5c.tsv"), lade_fixture("klasse_6c.tsv"))
  build_cohort(df, 5, 6)
}

test_that("das Verlaufsdiagramm zeigt je Kind eine Linie ueber beide Stufen", {
  k <- cohort_fixture_g()
  p <- plot_verlauf(k)

  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))

  daten <- ggplot2::ggplot_build(p)$data[[1]]
  # nur zugeordnete Kinder mit zwei Messwerten
  g <- cohort_gematcht(k)
  erwartet <- sum(!is.na(g$WE_Alt) & !is.na(g$WE_Neu)) +
    sum(!is.na(g$RF_Alt) & !is.na(g$RF_Neu))
  expect_equal(length(unique(daten$group)), nrow(g))
  expect_gte(nrow(daten), erwartet)
  # beide Stufen sind Achsenwerte, beide Kennzahlen sind Facetten
  expect_true(all(c(k$stufe_alt, k$stufe_neu) %in% daten$x))
  expect_length(unique(daten$PANEL), 2)
})

test_that("ohne Vergleich liefert der Verlauf einen Hinweis statt eines Fehlers", {
  expect_s3_class(plot_verlauf(NULL), "ggplot")
  expect_no_error(ggplot2::ggplot_build(plot_verlauf(NULL)))
  # leere Kohorte (keine Paare) ebenso
  k <- cohort_fixture_g()
  k$paare$Status <- "kein_partner"
  expect_no_error(ggplot2::ggplot_build(plot_verlauf(k)))
})

test_that("createPlot reicht die Diagrammart Verlauf durch", {
  k <- cohort_fixture_g()
  df <- dplyr::bind_rows(lade_fixture("klasse_5c.tsv"), lade_fixture("klasse_6c.tsv"))
  p <- createPlot(df, x = "R/F-%", fill = "Kat.", xlab = "R/F-Wert in %",
                  type = "Verlauf", cohort = k)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("der Speichern-Weg legt das Vergleichsblatt und den Word-Anhang an", {
  withr::with_tempdir({
    file.copy(file.path(projekt_root, "template.docx"), "template.docx")
    dir.create("Auswertungen")
    df <- dplyr::bind_rows(lade_fixture("klasse_5c.tsv"), lade_fixture("klasse_6c.tsv"))
    tab <- vergleich_tabelle(cohort_fixture_g())
    expect_true(!is.null(tab))

    msgs <- saveData(df, vergleich = tab)
    expect_match(msgs, "Vergleichstabelle mit", fixed = TRUE)

    xlsx <- list.files("Auswertungen", pattern = "\\.xlsx$", full.names = TRUE)
    expect_length(xlsx, 1)
    blaetter <- openxlsx::getSheetNames(xlsx)
    expect_true(all(c("C-Test", "Vergleich") %in% blaetter))
    gelesen <- openxlsx::read.xlsx(xlsx, sheet = "Vergleich")
    expect_equal(nrow(gelesen), nrow(tab))
    expect_true("Klasse" %in% colnames(gelesen))
    # das Excel-Blatt behaelt den Hinweis
    expect_true("Hinweis" %in% colnames(gelesen))

    docx <- list.files("Auswertungen", pattern = "\\.docx$", full.names = TRUE)
    expect_length(docx, 1)
    z <- tempfile("d"); dir.create(z)
    utils::unzip(docx, exdir = z)
    xml <- paste(readLines(file.path(z, "word", "document.xml"), warn = FALSE,
                           encoding = "UTF-8"), collapse = "")
    expect_match(xml, "Vergleich je Kind", fixed = TRUE)
    expect_true(grepl("<w:tbl", xml))
    # im Word-Anhang fehlt die Hinweisspalte (Platz auf der Seite)
    expect_false(grepl("<w:t>Hinweis</w:t>", xml, fixed = TRUE))
  })
})

test_that("ohne Vergleich bleibt der Export unveraendert", {
  withr::with_tempdir({
    file.copy(file.path(projekt_root, "template.docx"), "template.docx")
    dir.create("Auswertungen")
    df <- lade_fixture("klasse_5c.tsv")

    msgs <- saveData(df, vergleich = NULL)
    expect_false(grepl("Vergleichstabelle", msgs, fixed = TRUE))
    xlsx <- list.files("Auswertungen", pattern = "\\.xlsx$", full.names = TRUE)
    expect_false("Vergleich" %in% openxlsx::getSheetNames(xlsx))
  })
})

test_that("Word und Excel sind erst nach Klasse, dann nach Namen sortiert", {
  withr::with_tempdir({
    file.copy(file.path(projekt_root, "template.docx"), "template.docx")
    dir.create("Auswertungen")

    # absichtlich unsortiert eingegeben (zwei Klassen)
    df <- leere_tabelle()
    df <- addEntry(df, name = "Zimmer, Zoe",  klasse = "5c", rf = 28, we = 34, numItems = 40)
    df <- addEntry(df, name = "Aydin, Sara",  klasse = "6c", rf = 22, we = 28, numItems = 40)
    df <- addEntry(df, name = "Beispiel, Ben", klasse = "5c", rf = 17, we = 20, numItems = 40)

    erwartet_namen <- c("Beispiel, Ben", "Zimmer, Zoe", "Aydin, Sara")

    saveData(df)

    # Excel
    xlsx <- list.files("Auswertungen", pattern = "\\.xlsx$", full.names = TRUE)
    gelesen <- openxlsx::read.xlsx(xlsx, sheet = "C-Test")
    expect_equal(as.character(gelesen$Name), erwartet_namen)
    expect_equal(as.character(gelesen$Klasse), c("5c", "5c", "6c"))

    # Word: Reihenfolge der Namen im Dokument (Position im XML vergleichen)
    docx <- list.files("Auswertungen", pattern = "\\.docx$", full.names = TRUE)
    z <- tempfile("d"); dir.create(z)
    utils::unzip(docx, exdir = z)
    xml <- paste(readLines(file.path(z, "word", "document.xml"), warn = FALSE,
                           encoding = "UTF-8"), collapse = "")
    positionen <- vapply(erwartet_namen, function(n) regexpr(n, xml, fixed = TRUE)[1],
                         numeric(1))
    expect_true(all(positionen > 0))          # alle Namen stehen im Dokument
    expect_true(all(diff(positionen) > 0))    # und zwar in dieser Reihenfolge

    # tsv behaelt die Eingabereihenfolge
    tsv <- list.files("Auswertungen", pattern = "\\.tsv$", full.names = TRUE)
    roh <- readr::read_tsv(tsv, show_col_types = FALSE)
    expect_equal(as.character(roh$Name), as.character(df$Name))
  })
})
