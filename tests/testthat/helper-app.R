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

# Das Objekt `js` legt normalerweise extendShinyjs() beim Aufbau der UI an.
# Fuer die Server-Tests ohne UI hier ein Platzhalter mit derselben Schnittstelle.
if (!exists("js", envir = globalenv(), inherits = FALSE)) {
  assign("js", list(refocus = function(...) invisible(NULL)), envir = globalenv())
}

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
                 Kat. = factor(character(0), levels = lvls),
                 Empfehlung = character(0))
}

# Eine tsv-Fixture so laden, wie es die App tut (ueber loadData())
#
# Warnungen werden unterdrueckt, weil die App dort zwei bekannte Faelle meldet:
#  - readr "value in level set" fuer Kat. '0' (Nicht-Teilnehmer): bekannter
#    Fehler, dokumentiert in test-datenrundlauf.R
#  - ggplot "Removed ... non-finite": Nicht-Teilnehmer haben NA-Werte
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
