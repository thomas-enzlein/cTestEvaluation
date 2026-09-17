# Datenrundlauf: tsv/Docx/Xlsx speichern und wieder laden.
# Laeuft komplett in einem temporaeren Ordner - der Ordner "Auswertungen"
# des Projekts wird dabei nicht beruehrt.

test_that("saveData schreibt tsv, docx und xlsx in den Ausgabeordner", {
  withr::with_tempdir({
    file.copy(file.path(projekt_root, "template.docx"), "template.docx")
    dir.create("Auswertungen")
    df <- lade_fixture("klasse_5c.tsv")

    msgs <- saveData(df)

    dateien <- list.files("Auswertungen")
    erwartet <- paste0("C-Test_Auswertung_", Sys.Date(), "_5c")
    expect_true(paste0(erwartet, ".tsv") %in% dateien)
    expect_true(paste0(erwartet, ".docx") %in% dateien)
    expect_true(paste0(erwartet, ".xlsx") %in% dateien)
    expect_match(msgs, "Daten gespeichert unter")
  })
})

test_that("gespeicherte tsv laesst sich unveraendert wieder laden", {
  withr::with_tempdir({
    file.copy(file.path(projekt_root, "template.docx"), "template.docx")
    dir.create("Auswertungen")

    # frisch eingegebene Schueler (so wie in der App eingegeben)
    df <- leere_tabelle()
    df <- addEntry(df, name = "Testmann, Anna", klasse = "5c", rf = 28, we = 34, numItems = 40)
    df <- addEntry(df, name = "Beispiel, Ben", klasse = "5c", rf = 22, we = 28, numItems = 40)
    df <- addEntry(df, name = "Aydin, Sara", klasse = "5c", rf = 17, we = 20, numItems = 40)

    saveData(df)
    tsv <- list.files("Auswertungen", pattern = "\\.tsv$", full.names = TRUE)

    geladen <- loadData(list(datapath = tsv, name = basename(tsv),
                             size = file.size(tsv),
                             type = "text/tab-separated-values"))

    expect_equal(colnames(geladen), colnames(df))
    expect_equal(geladen$Name, c("Testmann, Anna", "Beispiel, Ben", "Aydin, Sara"))
    expect_equal(geladen$Klasse, rep("5c", 3))
    expect_equal(geladen$`WE-Wert`, c(34, 28, 20))
    expect_equal(geladen$`WE-%`, c(85, 70, 50))
    expect_equal(geladen$`R/F-Wert`, c(28, 22, 17))
    expect_equal(geladen$`R/F-%`, c(70, 55, 42.5))
    expect_equal(geladen$`Kat.`, c("2B", "4C", "4D"))
    expect_equal(geladen$Empfehlung, as.character(df$Empfehlung))
    expect_true(checkColumnNames(df, geladen))
  })
})

test_that("Nicht-Teilnehmer behalten ihre Kategorie beim Neuladen", {
  # frueher wurde Kat. als Faktor der 15 Kategorien gelesen; "0" faellt daraus
  # heraus und wurde zu NA (Kategorie weg, im Elternbrief nur ":").
  # Seit Paket D wird Kat. als Text gelesen - der Wert bleibt erhalten.
  df <- lade_fixture("klasse_5c.tsv")
  expect_equal(df$`Kat.`[df$Name == "Probst, Ella"], "0")
  # die Zuordnung zum Elterntext funktioniert auch fuer "0"
  # (convert_kat_meaning liest die Tabelle relativ zum Projektordner)
  withr::with_dir(projekt_root, {
    expect_equal(convert_kat_meaning(df$`Kat.`[df$Name == "Probst, Ella"]),
                 "0: Ihr Kind hat leider nicht teilgenommen.")
  })

  # und der Wert ueberlebt den naechsten Speichervorgang
  withr::with_tempdir({
    file.copy(file.path(projekt_root, "template.docx"), "template.docx")
    dir.create("Auswertungen")
    saveData(df)
    tsv <- list.files("Auswertungen", pattern = "\\.tsv$", full.names = TRUE)
    wieder <- loadData(list(datapath = tsv, name = basename(tsv),
                            size = file.size(tsv),
                            type = "text/tab-separated-values"))
    expect_equal(wieder$`Kat.`[wieder$Name == "Probst, Ella"], "0")
  })
})

test_that("alte tsv-Dateien ohne Spalte Klasse werden konvertiert", {
  df <- lade_fixture("altformat_5d.tsv")

  expect_equal(colnames(df),
               c("Name", "Klasse", "WE-Wert", "WE-%", "R/F-Wert", "R/F-%", "Kat.",
                 "Empfehlung", "Items"))
  expect_equal(nrow(df), 3)
  expect_true(all(df$Klasse == ""))
  # auch im Altformat bleibt "0" (nicht teilgenommen) erhalten
  expect_equal(df$`Kat.`, c("3D", "4C", "0"))
  expect_equal(df$`WE-%`, c(60, 65, NA))
  expect_equal(df$`R/F-%`, c(60, 50, NA))
  expect_equal(df$Name[3], "Nichtmit, Carl")
})

test_that("zwei tsv-Dateien lassen sich nacheinander laden (Jahresvergleich)", {
  df <- dplyr::bind_rows(lade_fixture("klasse_5c.tsv"), lade_fixture("klasse_6c.tsv"))
  expect_equal(nrow(df), 18)
  expect_setequal(unique(df$Klasse), c("5c", "6c"))
})

# Hinweis: tsv-Dateien mit fehlenden Spalten werden seit Paket K geladen und
# gemeldet statt abzubrechen (siehe test-robustheit.R).

test_that("createFilePath baut die Pfade im Ausgabeordner", {
  withr::with_tempdir({
    expect_equal(createFilePath(NULL, ""), .pfad_nativ(file.path(getwd(), "Auswertungen")))
    expect_equal(createFilePath("C-Test_Auswertung_test", "tsv"),
                 .pfad_nativ(file.path(getwd(), "Auswertungen", "C-Test_Auswertung_test.tsv")))
  })
})
