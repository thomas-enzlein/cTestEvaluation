# functions/utils.R
#
# Technische Helfer: Pruefen, Pfade, Dateien, UI-Bausteine.
#
# Ausgelagert aus functions.R (nur verschoben, Inhalt unveraendert), damit
# die Fachlogik in functions.R uebersichtlich bleibt.

# Fehlt der Wert oder ist er leer? Fuer jeden Wert TRUE/FALSE (auch fuer
# Vektoren, zum Filtern). Bei character(0)/NULL kommt logical(0) - Aufrufer,
# die einen einzelnen Wert erwarten, pruefen die Laenge davor (siehe
# composeClass). Die Vertragspruefung in name_tokens() bleibt bewusst eigen.
.ist_leer <- function(x) is.na(x) | !nzchar(trimws(as.character(x)))

composeClass <- function(klassenstufe, klassenBuchstabe) {
  # ohne Stufe UND ohne Buchstabe gibt es keine Klasse: sonst entstuende z. B.
  # nur "c" (ohne Jahrgang) - im Elternbrief waere das "der NA. Klasse"
  # die Laengenpruefung faengt character(0) ab (.ist_leer liefert dort nichts)
  fehlt <- function(x) length(x) == 0 || .ist_leer(x)
  if(fehlt(klassenstufe) || fehlt(klassenBuchstabe)) {
    return(NULL)
  }
  
  return(paste0(klassenstufe, klassenBuchstabe))
}

checkCompleteInput <- function(rf, we) {
  # genau eine Angabe leer
  if(sum(is.na(c(rf, we))) == 1) {
    return(TRUE)
  }
  return(FALSE)
}

checkInputFile <- function(inputFile) {
  req(inputFile)
  ext <- tools::file_ext(inputFile$datapath)
  
  validate(need(ext == "tsv", "Bitte tsv Datei auswählen"))
  return(inputFile$datapath)
}

# Spaltennamen in tsv pruefen
checkColumnNames <- function(df1, df2) {
  # nur die fachlichen Spalten vergleichen: die Itemzahl kann in der einen
  # Tabelle stehen und in der anderen fehlen (Altdateien)
  spalten <- intersect(colnames(df1), .schueler_spalten)
  if (length(spalten) == 0) spalten <- colnames(df1)
  return(all(spalten %in% colnames(df2)))
}

# Prueft, ob ein Verzeichnis existiert (legt es an) und beschreibbar ist
verzeichnis_sicherstellen <- function(pfad) {
  if (is.null(pfad) || !nzchar(pfad)) return(FALSE)
  if (!dir.exists(pfad)) {
    dir.create(pfad, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(pfad)) return(FALSE)

  # echter Schreibtest, weil Verzeichnisrechte unter Windows sonst taeuschen
  probe <- file.path(pfad, paste0(".schreibtest_", Sys.getpid()))
  ok <- suppressWarnings(file.create(probe))
  if (isTRUE(ok)) unlink(probe)
  isTRUE(ok)
}

# Benutzerordner als Ausweichziel (Dokumente des angemeldeten Benutzers)
benutzer_ausgabeordner <- function() {
  basis <- getOption("ctest.outdir.fallback")
  if (is.null(basis)) {
    basis <- ""
    if (.Platform$OS.type == "windows") {
      profil <- Sys.getenv("USERPROFILE")
      kandidaten <- file.path(profil, c("Documents", "Dokumente"))
      vorhanden <- kandidaten[dir.exists(kandidaten)]
      if (length(vorhanden) > 0) basis <- vorhanden[1]
      else if (nzchar(profil)) basis <- profil
    }
    if (!nzchar(basis)) basis <- path.expand("~")
  }
  file.path(basis, "C-Test Auswertung")
}

# Pfad in der Schreibweise des Systems. file.path() mischt die Trennzeichen,
# weil Umgebungsvariablen wie USERPROFILE Backslashes enthalten und file.path()
# mit "/" anhaengt ("C:\Users\.../Documents"). Fuer Hinweise, Meldungen und das
# Oeffnen im Explorer oder in Word ist die native Form lesbarer.
.pfad_nativ <- function(pfad) {
  if (is.null(pfad)) return(pfad)
  if (.Platform$OS.type == "windows") chartr("/", "\\", pfad) else pfad
}

# Ausgabeordner bestimmen: Vorgabe (Option) > Programmordner > Benutzerordner
ctest_ausgabeordner <- function(neu = FALSE) {
  vorgabe <- getOption("ctest.outdir")
  if (!is.null(vorgabe)) {
    if (!verzeichnis_sicherstellen(vorgabe)) {
      stop("Ausgabeordner '", vorgabe, "' kann nicht angelegt werden. ",
           "Bitte Option ctest.outdir pruefen.", call. = FALSE)
    }
    return(.pfad_nativ(vorgabe))
  }

  programmordner <- file.path(getwd(), "Auswertungen")

  # Zwischenspeicher nur nutzen, wenn er zum aktuellen Programmordner passt
  if (!neu && !is.null(.ctest_env$outdir) &&
      identical(.ctest_env$programmordner, programmordner)) {
    return(.ctest_env$outdir)
  }

  .ctest_env$programmordner <- programmordner

  if (verzeichnis_sicherstellen(programmordner)) {
    .ctest_env$outdir <- .pfad_nativ(programmordner)
    return(.ctest_env$outdir)
  }

  benutzerordner <- benutzer_ausgabeordner()
  if (verzeichnis_sicherstellen(benutzerordner)) {
    message("Programmordner ist nicht beschreibbar - Ausgaben gehen nach: ",
            .pfad_nativ(benutzerordner))
    .ctest_env$outdir <- .pfad_nativ(benutzerordner)
    return(.ctest_env$outdir)
  }

  stop("Es wurde kein beschreibbarer Ausgabeordner gefunden. Bitte pruefen, ",
       "ob Schreibrechte fuer '", programmordner, "' oder '", benutzerordner,
       "' bestehen.", call. = FALSE)
}

# Dateipfad erstellen (Ordner wird angelegt und auf Schreibbarkeit geprueft)
createFilePath <- function(filename, extension) {
  outpath <- ctest_ausgabeordner()
  if(is.null(filename)) {
    return(outpath)
  }
  
  fullpath <- .pfad_nativ(file.path(outpath, paste0(filename, ".", extension)))
  return(fullpath)
}

# Ist eine Datei gerade gesperrt? Word und Excel halten geoeffnete Dokumente
# exklusiv; das Anlegen der fertigen Datei scheitert dann mit "Permission denied"
# - bei den Briefen erst nach dem Rendern, das Minuten dauert. Deshalb wird
# vorher geprueft.
#
# 'oeffnen' ist fuer Tests da: damit laesst sich der gesperrte Fall ohne Word
# ausloesen.
.datei_gesperrt <- function(pfad, oeffnen = NULL) {
  if (is.null(pfad) || length(pfad) == 0 || !nzchar(pfad)) return(FALSE)
  if (!file.exists(pfad)) return(FALSE)

  if (is.null(oeffnen)) oeffnen <- function(p) file(p, open = "r+b")
  # das Scheitern ist hier der erwartete Fall - keine Warnung auf der Konsole
  verbindung <- tryCatch(suppressWarnings(oeffnen(pfad)), error = function(e) NULL)
  if (is.null(verbindung)) return(TRUE)

  try(close(verbindung), silent = TRUE)
  FALSE
}

# Klare Meldung statt der Meldung des Renderers, wenn die Zieldatei gesperrt ist
.pruefe_datei_frei <- function(pfad, oeffnen = NULL) {
  if (!.datei_gesperrt(pfad, oeffnen = oeffnen)) return(invisible(TRUE))
  stop("Die Datei '", basename(pfad), "' ist gerade geöffnet oder ",
       "schreibgeschützt (z. B. in Word). Bitte schließen und erneut starten.",
       call. = FALSE)
}

# Datei oder Ordner mit dem Standardprogramm oeffnen (fuer Tests ersetzbar)
oeffne_datei <- function(pfad) {
  utils::browseURL(pfad)
  invisible(TRUE)
}

# Hinweise einer geladenen Tabelle (leer, wenn keine)
lade_hinweise <- function(df) {
  hinweise <- attr(df, "hinweise")
  if (is.null(hinweise)) return(character(0))
  as.character(hinweise)
}

# Zwischendateien von knitr aus der Arbeitskopie entfernen
knit_reste_entfernen <- function(vorlage) {
  rest <- file.path(vorlage, "elternbrief.knit.md")
  if (fs::file_exists(rest)) fs::file_delete(rest)
  invisible(TRUE)
}

# Link ueber tinyurl kuerzen. Ohne Internet (oder bei Fehlern) wird NULL
# zurueckgegeben - der Aufrufer arbeitet dann mit dem Originallink weiter.
# Erfolgreiche Kuerzungen werden zwischengespeichert, weil tinyurl die Anzahl
# der Anfragen begrenzt.
shorten_url <- function(long_url, timeout_sek = 5) {
  if (is.null(long_url) || !nzchar(long_url)) return(NULL)
  if (!grepl("^https?://", long_url)) {
    long_url <- paste0("https://", long_url)
  }

  zwischengespeichert <- .ctest_env$tinyLink
  if (!is.null(zwischengespeichert) && identical(zwischengespeichert$raw, long_url)) {
    return(zwischengespeichert$link)
  }

  kurz <- tryCatch({
    req <- httr2::request("https://tinyurl.com/api-create.php") |>
      httr2::req_url_query(url = long_url) |>
      httr2::req_timeout(timeout_sek) |>
      httr2::req_perform()
    httr2::resp_body_string(req)
  }, error = function(e) NULL)

  # tinyurl antwortet bei Problemen mit einer Fehlermeldung statt einer URL
  if (is.null(kurz) || !is.character(kurz) || !grepl("^https?://", kurz)) {
    message("Link konnte nicht gekuerzt werden (kein Internet?). ",
            "Es wird der Originallink verwendet.")
    return(NULL)
  }

  .ctest_env$tinyLink <- list(link = kurz, raw = long_url)
  return(kurz)
}

.als_wahr <- function(x) {
  if (is.logical(x)) return(isTRUE(x))
  tolower(trimws(as.character(x))) %in% c("ja", "true", "wahr", "1", "yes")
}

# stat output vorbereiten
createStatsOutput <- function(outputId) {
  htmlOutput(outputId = outputId)
}

# input felder/button erstellen
createInputField <- function(inputId, label, value = NA_integer_, min = 0, max = 100) {
  numericInput(inputId = inputId, 
               label = label, 
               value = value, 
               min = min, 
               max = max) %>%
    helper(content = inputId)
}

createActionButton <- function(inputId, label, icon) {
  actionButton(inputId = inputId, 
               label = label, 
               style = 'margin-top:25px;
                        margin-left:15px;
                        margin-bottom:6px',
               icon = icon)
}

# Bestaetigung fuer "Alle schliessen". Der Sichern-Knopf erscheint nur, wenn
# sich der Datenstand seit der letzten Sicherung geaendert hat (stand_geaendert
# in functions.R) - sonst gaebe es nichts zu sichern.
.schliessen_dialog <- function(geaendert) {
  shiny::modalDialog(
    title = "Daten schließen?",
    "Die geladene Tabelle und die Zuordnung werden geleert. Die Dateien auf der ",
    "Platte bleiben erhalten.",
    if (isTRUE(geaendert)) {
      shiny::helpText("Der Datenstand ist noch nicht gesichert.")
    } else {
      shiny::helpText("Der Datenstand ist bereits gesichert.")
    },
    footer = shiny::tagList(
      if (isTRUE(geaendert)) {
        shiny::actionButton("btSchliessenSichern", "Vorher sichern und schließen")
      },
      shiny::actionButton("btSchliessenNur",
                          if (isTRUE(geaendert)) "Nur schließen" else "Schließen"),
      shiny::modalButton("Abbrechen")
    ),
    easyClose = FALSE
  )
}
