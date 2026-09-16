# Laedt die App-Bausteine fuer die Tests - ohne die App zu starten.
#
# Die Tests laufen bewusst gegen die echten Funktionen aus functions/*.R
# sowie gegen echte Fixtures im tsv-Format der App (tests/testthat/fixtures).
# Alle Namen in den Fixtures sind erfunden (keine echten Schuelerdaten).

projekt_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)

# Pfad zu einer Fixture-Datei (absolut, damit Tests das Arbeitsverzeichnis
# wechseln koennen - test_path() liefert sonst einen relativen Pfad)
fixture <- function(...) {
  normalizePath(file.path(projekt_root, "tests", "testthat", "fixtures", ...), mustWork = TRUE)
}

# server.R beendet die App beim Sitzungsende (session$onSessionEnded(stopApp)).
# Im Testprozess laeuft keine App - deshalb hier ein Platzhalter.
if (!exists("stopApp", envir = globalenv(), inherits = FALSE)) {
  assign("stopApp", function(...) invisible(NULL), envir = globalenv())
}

# Hinweis: frueher stand hier ein Platzhalter fuer das Objekt `js` aus
# extendShinyjs(). Seit Paket F nutzt die App shinyjs::runjs() und braucht
# kein `js` mehr - der Platzhalter hatte genau den Fehler verdeckt, den er
# verhindern sollte (xfun exportiert ebenfalls ein `js`).

# App-Code laden (idempotent): Pakete aus req.txt, Funktionen und Server.
lade_app <- function() {
  if (isTRUE(getOption("ctest.tests.geladen"))) return(invisible(TRUE))
  alt <- setwd(projekt_root)
  on.exit(setwd(alt), add = TRUE)
  suppressPackageStartupMessages(source("global.R", local = FALSE))
  suppressPackageStartupMessages(source("server.R", local = FALSE))
  options(ctest.tests.geladen = TRUE)
  invisible(TRUE)
}

# Leere Schueler-Tabelle in der Struktur von rv$df (siehe server.R)
leere_tabelle <- function() {
  tibble::tibble(Name = character(0),
                 Klasse = character(0),
                 `WE-Wert` = numeric(0),
                 `WE-%` = numeric(0),
                 `R/F-Wert` = numeric(0),
                 `R/F-%` = numeric(0),
                 Kat. = character(0),
                 Empfehlung = character(0))
}

# Eine tsv-Fixture so laden, wie es die App tut (ueber loadData())
#
# Warnungen werden unterdrueckt, weil die App beim Zeichnen meldet, dass
# Nicht-Teilnehmer (NA-Werte) aus Histogrammen/Dichten entfernt werden
# (ggplot: "Removed ... non-finite").
lade_fixture <- function(datei, klasse = NULL) {
  pfad <- fixture(datei)
  df <- suppressWarnings(
    loadData(list(datapath = pfad,
                  name = datei,
                  size = file.size(pfad),
                  type = "text/tab-separated-values"))
  )
  if (!is.null(klasse)) df <- df[df$Klasse %in% klasse, , drop = FALSE]
  df
}

# Wendet einen Ausdruck an und unterdrueckt die beiden bekannten Warnungen
# (siehe lade_fixture). Fuer App-Ablaeufe, in denen Daten geladen oder
# Diagramme gezeichnet werden.
ohne_bekannte_warnungen <- function(code) suppressWarnings(force(code))

# Eingabe einer tsv-Datei in der Form, die fileInput() liefert
tsv_input <- function(datei) {
  pfad <- fixture(datei)
  list(datapath = pfad, name = datei, size = file.size(pfad),
       type = "text/tab-separated-values")
}

# Ausgabe eines renderUI/htmlOutput fuer Textpruefungen
als_text <- function(x) paste(as.character(x), collapse = "\n")

lade_app()
