# Elternbrief-Pipeline: rendert echte docx-Dateien (langsamster Test).
# Laeuft in einem temporaeren Ordner mit einer Kopie von elternbrief/.

brief_test_umgebung <- function(code) {
  withr::with_tempdir({
    # Inhalt von elternbrief/ kopieren (ohne file.copy-Warnung). Die Word-Vorlage
    # und die Kategorie-Texte kommen aus dem zentralen Ordner "vorlagen".
    dir.create("elternbrief")
    dateien <- list.files(file.path(projekt_root, "elternbrief"), full.names = TRUE)
    file.copy(dateien, "elternbrief", recursive = TRUE)
    dir.create("vorlagen")
    file.copy(list.files(file.path(projekt_root, "vorlagen"), full.names = TRUE),
              "vorlagen", recursive = TRUE)
    dir.create("Auswertungen")
    # Persoenlicher Vorlagenordner: sonst wuerde eine vom Benutzer angepasste
    # Vorlage unter "Dokumente" die Testergebnisse veraendern
    withr::local_options(ctest.outdir.fallback = file.path(getwd(), "benutzer"))
    force(code)
  })
}

# Briefe erzeugen; die Ausgabe von knitr/pandoc wird dabei unterdrueckt.
# Rueckgabe: Liste aus create_letters() (datei, erstellt, fehler)
erzeuge_briefe <- function(df, lehrername, signatur = "", qrLink = NULL) {
  ergebnis <- NULL
  utils::capture.output(
    suppressMessages(
      ergebnis <- create_letters(df, lehrername = lehrername,
                                 signatur = signatur, qrLink = qrLink)
    )
  )
  ergebnis
}

# compose_letter voruebergehend ersetzen (Testdouble) und danach zurueckstellen.
# bau_ersatz bekommt die echte Funktion und liefert den Ersatz.
mit_compose_letter <- function(bau_ersatz, code) {
  echt <- compose_letter
  assign("compose_letter", bau_ersatz(echt), envir = globalenv())
  on.exit(assign("compose_letter", echt, envir = globalenv()), add = TRUE)
  force(code)
}

brief_text <- function(datei) {
  paste(officer::docx_summary(officer::read_docx(datei))$text, collapse = "\n")
}

erwartete_datei <- function() {
  list.files("Auswertungen", pattern = "^Elternbriefe_.*\\.docx$", full.names = TRUE)
}

anzahl_anreden <- function(text) {
  length(regmatches(text, gregexpr("Liebe Erziehungsberechtigte", text)))
}

# officer::body_add_docx() schreibt weitere Briefe nicht in document.xml,
# sondern legt sie als eigenes Dokument im Paket ab (word/file*.docx) und
# verweist per altChunk darauf.
anzahl_altchunks <- function(datei) {
  xml <- paste(readLines(unz(datei, "word/document.xml"), warn = FALSE), collapse = "")
  treffer <- gregexpr("<w:altChunk", xml, fixed = TRUE)[[1]]
  if (treffer[1] == -1) 0L else length(treffer)
}

eingebettete_dokumente <- function(datei) {
  eintraege <- utils::unzip(datei, list = TRUE)$Name
  teile <- grep("^word/file.*\\.docx$", eintraege, value = TRUE)
  if (length(teile) == 0) return(character(0))
  ziel <- tempfile("eingebettet")
  dir.create(ziel)
  utils::unzip(datei, files = teile, exdir = ziel)
  file.path(ziel, teile)
}

test_that("fuer ein einzelnes Kind entsteht ein Elternbrief", {
  skip_if_not(rmarkdown::pandoc_available(), "pandoc nicht gefunden")

  brief_test_umgebung({
    df <- lade_fixture("klasse_5c.tsv")[1, ]   # Testmann, Anna (2B)
    erzeuge_briefe(df, lehrername = "Test, Tina")

    dateien <- erwartete_datei()
    expect_length(dateien, 1)
    expect_match(basename(dateien), paste0("Elternbriefe_", Sys.Date(), "_5c\\.docx"))

    text <- brief_text(dateien)
    expect_match(text, "Testmann, Anna", fixed = TRUE)
    expect_match(text, "Diagnostik im Fach Deutsch im Jahrgang 5", fixed = TRUE)
    expect_match(text, "B2: ", fixed = TRUE)   # Kategorie 2B -> Elterntext B2
    expect_match(text, "Test, Tina", fixed = TRUE)
    expect_equal(anzahl_anreden(text), 1)
    # ein einzelner Brief wird nicht als altChunk angehaengt
    expect_equal(anzahl_altchunks(dateien), 0)
  })
})

# Der QR-Code mit dem Link zur Uebungssammlung ist einer der Hauptgruende fuer
# den Elternbrief. Dieser Test deckt den Pfad ab, der bisher ungetestet war:
# Das Bild entsteht im Temp-Verzeichnis und muss in den Brief hinein.
test_that("mit Link entsteht ein Brief mit QR-Code und Linktext", {
  skip_if_not(rmarkdown::pandoc_available(), "pandoc nicht gefunden")

  brief_test_umgebung({
    # ohne Internet im Test: sonst wuerde der Link ueber tinyurl gekuerzt
    kurzlink_zuruecksetzen()
    testthat::local_mocked_bindings(
      req_perform = function(...) stop("kein Netz im Test"),
      .package = "httr2"
    )

    df <- lade_fixture("klasse_5c.tsv")[1, ]

    # Vergleichslauf ohne Link: nur die mitgelieferten Bilder
    erzeuge_briefe(df, lehrername = "Test, Tina")
    ohne <- erwartete_datei()
    expect_length(ohne, 1)
    bilder_ohne <- sum(grepl("^word/media/", utils::unzip(ohne, list = TRUE)$Name))

    erzeuge_briefe(df, lehrername = "Test, Tina",
                   qrLink = "https://example.org/uebungen")
    dateien <- erwartete_datei()
    expect_length(dateien, 1)

    # der Link steht als Text im Brief
    text <- brief_text(dateien)
    expect_match(text, "https://example.org/uebungen", fixed = TRUE)
    expect_match(text, "Sammlung", fixed = TRUE)

    # und der QR-Code liegt als zusaetzliches Bild im Dokument
    bilder_mit <- sum(grepl("^word/media/", utils::unzip(dateien, list = TRUE)$Name))
    expect_gt(bilder_mit, bilder_ohne)
  })
})

test_that("fuer mehrere Kinder entsteht eine Datei mit einem Brief je Kind", {
  skip_if_not(rmarkdown::pandoc_available(), "pandoc nicht gefunden")

  brief_test_umgebung({
    df <- lade_fixture("klasse_5c.tsv")[1:2, ]  # Anna (2B) und Ben (4C)

    # Zustand der Briefvorlagen im Programmverzeichnis vorher merken
    vorher <- sort(list.files("elternbrief", recursive = TRUE, all.files = TRUE, no.. = TRUE))

    erzeuge_briefe(df, lehrername = "Test, Tina", signatur = "Abteilungsleitung I")

    dateien <- erwartete_datei()
    expect_length(dateien, 1)

    # Die Briefe stehen nach Klasse und Name sortiert in der Datei: in der 5c
    # kommt "Beispiel, Ben" vor "Testmann, Anna", der erste Brief steht direkt
    # im Dokument.
    text <- brief_text(dateien)
    expect_match(text, "Beispiel, Ben", fixed = TRUE)
    expect_match(text, "C4: ", fixed = TRUE)
    expect_match(text, "Abteilungsleitung I", fixed = TRUE)
    expect_equal(anzahl_anreden(text), 1)

    # der zweite Brief ist als eingebettetes Dokument angehaengt
    expect_equal(anzahl_altchunks(dateien), 1)
    eingebettet <- eingebettete_dokumente(dateien)
    expect_length(eingebettet, 1)
    text2 <- brief_text(eingebettet)
    expect_match(text2, "Testmann, Anna", fixed = TRUE)
    expect_match(text2, "B2: ", fixed = TRUE)
    expect_match(text2, "Abteilungsleitung I", fixed = TRUE)

    # Paket B: das Programmverzeichnis bleibt unberuehrt (kein knit.md usw.)
    nachher <- sort(list.files("elternbrief", recursive = TRUE, all.files = TRUE, no.. = TRUE))
    expect_equal(nachher, vorher)
    expect_false(file.exists("elternbrief/elternbrief.knit.md"))
    # die Arbeitskopie im Temp-Verzeichnis ist wieder aufgeraeumt. Windows gibt
    # gesperrte Dateien manchmal erst verzoegert frei (Virenscanner, Cloud-Sync),
    # deshalb bis zu einer Sekunde nachfassen, bevor der Test fehlschlaegt.
    kopie <- file.path(tempdir(), paste0("elternbrief_", Sys.getpid()))
    for (i in 1:10) {
      if (!dir.exists(kopie)) break
      Sys.sleep(0.1)
    }
    expect_false(dir.exists(kopie))
  })
})

test_that("elternbrief_vorbereiten kopiert die Vorlagen in ein Temp-Verzeichnis", {
  vorlage <- elternbrief_vorbereiten(file.path(projekt_root, "elternbrief"))
  on.exit(unlink(vorlage, recursive = TRUE), add = TRUE)

  expect_true(dir.exists(vorlage))
  # die Arbeitskopie enthaelt die Dateien des Briefes UND die zentral
  # mitgelieferten (Word-Vorlage, Kategorie-Texte)
  erwartet <- c(list.files(file.path(projekt_root, "elternbrief")),
                "template.docx", "ergebnisse.xlsx")
  expect_setequal(list.files(vorlage), erwartet)
  for (datei in c("elternbrief.Rmd", "ergebnisse.xlsx", "_output.yml", "template.docx")) {
    expect_true(file.exists(file.path(vorlage, datei)))
  }
  # Arbeitskopie liegt im Temp-Verzeichnis und damit ausserhalb des
  # Projektordners, damit dort nichts geschrieben wird. Bei umgeleitetem TMPDIR
  # (z. B. in einer Sandbox) kann tempdir() selbst im Projekt liegen - dann
  # entfaellt die zweite Pruefung, weil sie die Umgebung statt den Code testet.
  expect_true(startsWith(normalizePath(vorlage), normalizePath(tempdir())))
  if (!startsWith(normalizePath(tempdir()), normalizePath(projekt_root))) {
    expect_false(startsWith(normalizePath(vorlage), normalizePath(projekt_root)))
  }
})

test_that("vorlagen_aufraeumen loescht die Arbeitskopie und meldet Erfolg", {
  ziel <- file.path(tempdir(), paste0("aufraeumen_", Sys.getpid()))
  dir.create(file.path(ziel, "unterordner"), recursive = TRUE, showWarnings = FALSE)
  writeLines("rest", file.path(ziel, "rest.txt"))

  expect_true(vorlagen_aufraeumen(ziel))
  expect_false(dir.exists(ziel))
  # nichts zu loeschen ist kein Fehler (on.exit laeuft immer)
  expect_true(vorlagen_aufraeumen(ziel))
})

test_that("eine liegen gebliebene Arbeitskopie wird ersetzt", {
  # Reste aus einem abgebrochenen Lauf duerfen den naechsten nicht blockieren
  ziel <- file.path(tempdir(), paste0("elternbrief_", Sys.getpid()))
  dir.create(ziel, recursive = TRUE, showWarnings = FALSE)
  writeLines("alt", file.path(ziel, "alt.txt"))

  vorlage <- elternbrief_vorbereiten(file.path(projekt_root, "elternbrief"))
  on.exit(vorlagen_aufraeumen(ziel), add = TRUE)

  expect_equal(normalizePath(vorlage), normalizePath(ziel))
  expect_false(file.exists(file.path(ziel, "alt.txt")))
  expect_true(file.exists(file.path(ziel, "elternbrief.Rmd")))
})

test_that("elternbrief_vorbereiten meldet fehlende Vorlagen", {
  expect_error(elternbrief_vorbereiten(file.path(tempdir(), "gibt_es_nicht")), "nicht gefunden")
})

test_that("ohne Link entsteht kein QR-Code und der Brief bleibt fehlerfrei", {
  skip_if_not(rmarkdown::pandoc_available(), "pandoc nicht gefunden")

  brief_test_umgebung({
    df <- lade_fixture("klasse_5c.tsv")[1, ]
    ergebnis <- erzeuge_briefe(df, lehrername = "Test, Tina", qrLink = NULL)

    expect_equal(ergebnis$erstellt, 1)
    expect_length(ergebnis$fehler, 0)

    dateien <- erwartete_datei()
    expect_length(dateien, 1)
    text <- brief_text(dateien)
    expect_false(grepl("tinyurl", text))
    expect_false(grepl("Sie möchten Ihr Kind unterstützen", text))
  })
})

test_that("ein fehlerhafter Brief verhindert die uebrigen nicht", {
  skip_if_not(rmarkdown::pandoc_available(), "pandoc nicht gefunden")

  brief_test_umgebung({
    df <- lade_fixture("klasse_5c.tsv")[1:2, ]   # Anna und Ben

    ergebnis <- mit_compose_letter(
      function(echt) {
        function(name, ...) {
          if (identical(name, "Beispiel, Ben")) stop("Testfehler beim Rendern")
          echt(name, ...)
        }
      },
      erzeuge_briefe(df, lehrername = "Test, Tina")
    )

    # Anna wurde erstellt, Ben nicht - die Datei existiert trotzdem und ist nutzbar
    expect_equal(ergebnis$erstellt, 1)
    expect_length(ergebnis$fehler, 1)
    expect_match(ergebnis$fehler, "Beispiel, Ben", fixed = TRUE)
    expect_match(ergebnis$fehler, "Testfehler beim Rendern", fixed = TRUE)

    expect_true(file.exists(ergebnis$datei))
    text <- brief_text(ergebnis$datei)
    expect_match(text, "Testmann, Anna", fixed = TRUE)
    expect_false(grepl("Beispiel, Ben", text))
  })
})

test_that("scheitern alle Briefe, gibt es einen klaren Fehler", {
  brief_test_umgebung({
    df <- lade_fixture("klasse_5c.tsv")[1:2, ]
    fehlermeldung <- mit_compose_letter(
      function(echt) function(...) stop("kein Rendern moeglich"),
      tryCatch(erzeuge_briefe(df, lehrername = "Test, Tina"),
               error = function(e) conditionMessage(e))
    )
    expect_match(fehlermeldung, "kein Elternbrief erstellt werden")
    expect_match(fehlermeldung, "kein Rendern moeglich")
    expect_length(erwartete_datei(), 0)
  })
})

test_that("die Brief-Erzeugung meldet ihren Fortschritt", {
  # Paket L: die App zeigt damit einen Fortschrittsbalken. Getestet wird die
  # Rueckmeldung selbst - ohne echtes Rendern (compose_letter wirft sofort).
  brief_test_umgebung({
    df <- lade_fixture("klasse_5c.tsv")[1:4, ]
    meldungen <- list()

    tryCatch(
      mit_compose_letter(
        function(echt) function(...) stop("kein Rendern im Test"),
        {
          utils::capture.output(suppressMessages(
            create_letters(df, lehrername = "Test, Tina",
                           fortschritt = function(anteil, text) {
                             meldungen[[length(meldungen) + 1]] <<-
                               list(anteil = anteil, text = text)
                           })))
          NULL
        }
      ),
      error = function(e) NULL)

    anteile <- vapply(meldungen, function(m) m$anteil, numeric(1))
    texte <- vapply(meldungen, function(m) m$text, character(1))
    alle <- paste(texte, collapse = " | ")

    # mindestens: Vorbereitung + vier Kinder
    expect_gte(length(anteile), 5)
    expect_equal(anteile[1], 0)
    expect_true(all(diff(anteile) >= 0))              # nur vorwaerts
    expect_true(all(anteile >= 0 & anteile <= 1))      # immer im gueltigen Bereich
    expect_match(alle, "Vorlagen werden vorbereitet", fixed = TRUE)
    expect_match(alle, "Brief 1 von 4", fixed = TRUE)
    expect_match(alle, "Brief 4 von 4", fixed = TRUE)
    expect_match(alle, "Testmann, Anna", fixed = TRUE)
  })
})

# XML eines Dokumentteils aus dem docx lesen (ohne das Paket zu entpacken)
docx_teil <- function(datei, teil) {
  con <- unz(datei, teil)
  on.exit(close(con), add = TRUE)
  paste(readLines(con, warn = FALSE), collapse = "")
}

zaehle <- function(xml, muster) {
  treffer <- gregexpr(muster, xml, fixed = TRUE)[[1]]
  if (treffer[1] == -1) 0L else length(treffer)
}

# wie zaehle(), aber als regulaerer Ausdruck (z. B. "<w:tr[ >]" - sonst zaehlt
# "<w:trPr>" mit)
zaehle_regex <- function(xml, muster) {
  treffer <- gregexpr(muster, xml)[[1]]
  if (treffer[1] == -1) 0L else length(treffer)
}

# Groessen aller Bilder im Hauptdokument (EMU), z. B. fuer den QR-Code
bild_groessen <- function(datei) {
  xml <- docx_teil(datei, "word/document.xml")
  treffer <- regmatches(xml, gregexpr('<wp:extent cx="[0-9]+" cy="[0-9]+"', xml))[[1]]
  if (length(treffer) == 0) return(data.frame(cx = numeric(0), cy = numeric(0)))
  zahlen <- regmatches(treffer, gregexpr("[0-9]+", treffer))
  data.frame(cx = as.numeric(vapply(zahlen, `[`, character(1), 1)),
             cy = as.numeric(vapply(zahlen, `[`, character(1), 2)))
}

# Eingebettete Briefe in der Reihenfolge, in der sie im Dokument stehen.
# officer::body_add_docx() haengt sie als altChunk an; die Reihenfolge der
# Dateien im Paket ist nicht die Reihenfolge im Dokument, deshalb wird ueber die
# Beziehungs-Ids (document.xml -> .rels -> file*.docx) aufgeloest.
eingebettete_der_reihe_nach <- function(datei) {
  xml <- docx_teil(datei, "word/document.xml")
  rels <- docx_teil(datei, "word/_rels/document.xml.rels")
  ids <- regmatches(xml, gregexpr('<w:altChunk r:id="[^"]+"', xml))[[1]]
  if (length(ids) == 0) return(character(0))
  ids <- sub('^.*r:id="', "", sub('"$', "", ids))
  ziele <- vapply(ids, function(id) {
    treffer <- regmatches(rels, regexpr(paste0('Id="', id, '"[^>]*Target="[^"]+"'), rels))
    basename(sub('"$', "", sub('^.*Target="', "", treffer)))
  }, character(1))
  alle <- eingebettete_dokumente(datei)
  alle[match(ziele, basename(alle))]
}

test_that("die Ergebnistabelle ist eine echte Word-Tabelle", {
  skip_if_not(rmarkdown::pandoc_available(), "pandoc nicht gefunden")

  brief_test_umgebung({
    df <- lade_fixture("klasse_5c.tsv")[1, ]
    erzeuge_briefe(df, lehrername = "Test, Tina")

    dateien <- erwartete_datei()
    expect_length(dateien, 1)
    xml <- docx_teil(dateien, "word/document.xml")

    # Tabelle im Dokument, kein Bild im Textkoerper (ohne Link gibt es keins)
    expect_gte(zaehle(xml, "<w:tbl"), 1)
    expect_equal(zaehle(xml, "<w:drawing"), 0)

    # Zellentrenner: jede Zelle hat einen Rahmen, waagerecht und senkrecht
    expect_gte(zaehle(xml, "<w:tcBorders"), 13)
    expect_gte(zaehle(xml, "<w:left"), 1)
    expect_gte(zaehle(xml, "<w:right"), 1)

    # der Tabelleninhalt ist Text (kein Bild) - Ueberschrift und Zelltexte
    text <- brief_text(dateien)
    expect_match(text, "Handlungsempfehlung", fixed = TRUE)
    expect_match(text, "Kategorie", fixed = TRUE)
    # 12 Zeilen aus dem Blatt Tabelle2 plus Kopfzeile, ohne die Zeile "0"
    expect_equal(zaehle_regex(xml, "<w:tr[ >]"), 13)
    expect_false(grepl("nicht teilgenommen", text, fixed = TRUE))
    # die Empfehlung fuer A1-B2 steht einmal, nicht vier Mal (verbundene Zelle)
    satz <- "Das Ergebnis liegt oberhalb des Normbereichs. Es besteht kein Handlungsbedarf"
    treffer <- gregexpr(satz, text, fixed = TRUE)[[1]]
    expect_equal(sum(treffer > 0), 1)
    expect_gte(zaehle(xml, "<w:vMerge"), 1)

    # Rechtschreibung im Brief: Dativ nach "bei allen"
    expect_match(text, "Schülerinnen und Schülern der 5. Klasse", fixed = TRUE)
    expect_false(grepl("Schülerinnen und Schüler der", text, fixed = TRUE))
  })
})

test_that("der Linktext trennt die Saetze hart und der QR-Code ist quadratisch", {
  skip_if_not(rmarkdown::pandoc_available(), "pandoc nicht gefunden")

  # Harter Umbruch (zwei Leerzeichen vor dem Zeilenwechsel) im Textbaustein
  kurzlink_zuruecksetzen()
  testthat::local_mocked_bindings(
    req_perform = function(...) stop("kein Netz im Test"),
    .package = "httr2"
  )
  qr <- suppressMessages(generate_qrcode("https://example.org/uebungen",
                                        zielordner = tempdir()))
  expect_equal(qr$txt,
               paste0("Sie möchten Ihr Kind unterstützen?  \n",
                      "Dann schauen Sie hier in unsere Sammlung:\n",
                      "https://example.org/uebungen"))

  brief_test_umgebung({
    df <- lade_fixture("klasse_5c.tsv")[1, ]
    erzeuge_briefe(df, lehrername = "Test, Tina",
                   qrLink = "https://example.org/uebungen")

    dateien <- erwartete_datei()
    expect_length(dateien, 1)

    # im Dokument steht genau ein Bild (der QR-Code) - quadratisch und 1 Zoll
    groessen <- bild_groessen(dateien)
    expect_equal(nrow(groessen), 1)
    expect_equal(groessen$cx, groessen$cy)
    expect_equal(groessen$cx / 914400, 1, tolerance = 0.01)

    # die Tabelle bleibt zusaetzlich im Dokument
    expect_gte(zaehle(docx_teil(dateien, "word/document.xml"), "<w:tbl"), 1)
  })
})

test_that("die Briefe stehen nach Klasse und Name sortiert im Dokument", {
  skip_if_not(rmarkdown::pandoc_available(), "pandoc nicht gefunden")

  brief_test_umgebung({
    # Absichtlich in verkehrter Reihenfolge: erst 6c, dann 5c (Anna vor Ben)
    df <- dplyr::bind_rows(lade_fixture("klasse_6c.tsv")[6, ],
                           lade_fixture("klasse_5c.tsv")[1:2, ])
    erzeuge_briefe(df, lehrername = "Test, Tina")

    dateien <- erwartete_datei()
    expect_length(dateien, 1)

    # erster Brief: 5c, "Beispiel, Ben" (junge Stufe zuerst, dann Name)
    text <- brief_text(dateien)
    expect_match(text, "Beispiel, Ben", fixed = TRUE)
    expect_false(grepl("Testmann, Anna", text, fixed = TRUE))
    expect_false(grepl("Aydin, Sara", text, fixed = TRUE))

    # danach Anna (5c) und Sara (6c) als eingebettete Dokumente - in dieser
    # Reihenfolge steht der Brief auch im Dokument
    eingebettet <- eingebettete_der_reihe_nach(dateien)
    expect_length(eingebettet, 2)
    expect_match(brief_text(eingebettet[1]), "Testmann, Anna", fixed = TRUE)
    expect_match(brief_text(eingebettet[2]), "Aydin, Sara", fixed = TRUE)
  })
})

test_that("fehlende Spalten fuer die Briefe werden klar gemeldet", {
  brief_test_umgebung({
    df <- lade_fixture("klasse_5c.tsv")[1:2, ]
    ohne_kat <- df[, setdiff(colnames(df), "Kat.")]
    expect_error(create_letters(ohne_kat, lehrername = "Test, Tina"),
                 "fehlen Spalten: Kat\\.")
  })
})
