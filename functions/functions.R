composeClass <- function(klassenstufe, klassenBuchstabe) {
  # ohne Stufe UND ohne Buchstabe gibt es keine Klasse: sonst entstuende z. B.
  # nur "c" (ohne Jahrgang) - im Elternbrief waere das "der NA. Klasse"
  leer <- function(x) length(x) == 0 || is.na(x) || !nzchar(trimws(as.character(x)))
  if(leer(klassenstufe) || leer(klassenBuchstabe)) {
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

checkRfWe <- function(rf, we) {
  # kein check nötig -> hat nicht teilgenommen
  if(all(is.na(c(rf, we)))) {
    return(FALSE)
  }
  
  # Ueberpruefe ob rf groeßer als we ist
  if(rf > we) {
    return(TRUE)
  }
  # alles ok
  return(FALSE)
}

checkMorePointsThenNumItems <- function(rf, we, numItems) {
  # kein check nötig -> hat nicht teilgenommen
  if(all(is.na(c(rf, we)))) {
    return(FALSE)
  }
  
  # Ueberpruefe ob rf oder we groeßer als die Anzahl an Test-Items ist
  if(rf > numItems | we > numItems) {
    return(TRUE)
  }
  return(FALSE)
}

checkInputErrors <- function(inputName, inputRf, inputWe, numItems, klasse) {
  if(nchar(str_trim(inputName)) == 0) {
    return("Bitte Namen eingeben.")
  }
  
  if(checkCompleteInput(rf = inputRf, we = inputWe)) {
    return("Bitte beide Werte angeben oder keinen (Schüler hat nicht teilgenommen).")
  }
  
  if(checkRfWe(rf = inputRf, we = inputWe)) {
    return("R/F-Wert kann nicht größer als WE-Wert sein.")
  }
  
  if(checkMorePointsThenNumItems(rf = inputRf, we = inputWe, numItems = numItems)) {
    return("R/F- bzw WE-Wert kann nicht höher als Anzahl Test-Items sein.")
  }
  
  if(is.null(klasse)) {
    return("Bitte Klassenstufe und Klasse angeben.")
  }
  
  return(NULL)
}

# Normgrenzen des Verfahrens: sie definieren die Kategorien und sind deshalb
# FEST. Kein Einstellungswert fasst sie an - eine Aenderung in
# einstellungen.txt darf die Einteilung der Kinder nie verschieben.
.rf_stufen <- c(stufe1 = 71.3, stufe2 = 66.3, stufe3 = 56.3, stufe4 = 36.2)
# Untere Grenze des Wortschatz-Werts in der C/C*-Regel. Achtung: derselbe Betrag
# wie der untere Normbereich des R/F-Werts (65), aber inhaltlich unabhaengig -
# der Wortschatz-Wert ist nicht einstellbar.
.we_normgrenze <- 65

getRFlevel <- function(rfPerc) {
  # Wende Grenzwerte an um rf Kategorie zu erhalten (fest, siehe .rf_stufen)
  res <- case_when (rfPerc >= .rf_stufen[["stufe1"]] ~ 1,
                    rfPerc >= .rf_stufen[["stufe2"]] ~ 2,
                    rfPerc >= .rf_stufen[["stufe3"]] ~ 3,
                    rfPerc >= .rf_stufen[["stufe4"]] ~ 4,
                    rfPerc < .rf_stufen[["stufe4"]] ~ 5,
                    is.na(rfPerc)  ~ 0)
  return(res)
}

getWElevel <- function(rfPerc, wePerc) {
  diff <- wePerc - rfPerc
  rflvl <- getRFlevel(rfPerc)
  # Wortschatz-Grenze: fester Wert des Verfahrens, bewusst nicht einstellbar
  we_grenze <- .we_normgrenze
  
  res <- case_when(rflvl <= 2 & diff <= 10 ~ "A", 
                   rflvl <= 2 & diff > 10 ~ "B",  
                   rflvl > 2 & between(diff, 10, 19.9) & wePerc > we_grenze ~ "C",
                   rflvl > 2 & diff >= 20 & wePerc > we_grenze ~ "C*",
                   rflvl > 2 & diff < 10 ~ "D",
                   rflvl > 3 & diff >= 10 ~ "E",
                   rflvl == 0 ~ "")
  return(res)
}

getRecommendation <- function(kat) {
  recommendations <- c("1A" = "Ergebnis oberhalb des Normbereichs. Kein Handlungsbedarf im Bereich Rechtschreibung und Wortschatz.",
                       "2A" = "Ergebnis oberhalb des Normbereichs. Kein Handlungsbedarf im Bereich Rechtschreibung und Wortschatz.",
                       "1B" = "Ergebnis oberhalb des Normbereichs. Kein Handlungsbedarf im Bereich Rechtschreibung und Wortschatz.",
                       "2B" = "Ergebnis oberhalb des Normbereichs. Kein Handlungsbedarf im Bereich Rechtschreibung und Wortschatz.",
                       "3C" = "Das Ergebnis liegt im Normbereich. Der Lernende würde von Rechtschreib-Übungen profitieren.",
                       "3C*" = "Der Lernende sollte die Rechtschreibung verbessern (mögl. LRS).",
                       "3D" = "Der Lernende sollte seinen Wortschatz verbessern (z.B. durch Lesen).",
                       "4C" = "Der Lernende sollte die Rechtschreibung verbessern.",
                       "4C*" = "Der Lernende sollte die Rechtschreibung verbessern (mögl. LRS).",
                       "4D" = "Der Lernende sollte die Rechtschreibung verbessern und mehr lesen.",
                       "4E" = "Der Lernende sollte die Rechtschreibung verbessern und mehr lesen.",
                       "5C" = "Der Lernende sollte die Rechtschreibung verbessern.",
                       "5C*" = "Der Lernende sollte die Rechtschreibung verbessern (mögl. LRS).",
                       "5D" = "Der Lernende sollte die Rechtschreibung verbessern und mehr lesen.",
                       "5E" = "Der Lernende sollte die Rechtschreibung verbessern und mehr lesen.",
                       "0"  =  "Hat nicht teilgenommen.")
  
  return(recommendations[kat])
}

styleTable <- function(dt) {
  # ohne Kategorie-Spalte gibt es nichts zu faerben (kein Abbruch)
  if (!"Kat." %in% colnames(dt)) return(dt)

  # Faerbe Zellen in der Tabelle basierend auf der Kategorie
  dt <-
    dt %>%
    formatStyle(
      'Kat.',
      color = 'black',
      backgroundColor = styleEqual(levels = lvls, 
                                   values = cols
      )
    )
  
  return(dt)
}

#### Ausgabeordner ####
# Die App kann als installierte Anwendung unter C:/ProgramData liegen. Dort
# darf ein normaler Benutzer (Lehrkraft) nicht in jedem Fall schreiben.
# Deshalb: Ausgabeordner im Programmverzeichnis anlegen und nur benutzen,
# wenn er wirklich beschreibbar ist - sonst auf den Benutzerordner ausweichen.

# Zwischenspeicher fuer den einmal ermittelten Ausgabeordner
.ctest_env <- new.env(parent = emptyenv())

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

# Fuer Tests: zwischengespeicherten Ordner vergessen
ausgabeordner_zuruecksetzen <- function() {
  for (eintrag in c("outdir", "programmordner")) {
    if (exists(eintrag, envir = .ctest_env, inherits = FALSE)) {
      rm(list = eintrag, envir = .ctest_env)
    }
  }
  invisible(TRUE)
}

# Fuer Tests: zwischengespeicherten Kurzlink vergessen
kurzlink_zuruecksetzen <- function() {
  if (exists("tinyLink", envir = .ctest_env, inherits = FALSE)) {
    rm("tinyLink", envir = .ctest_env)
  }
  invisible(TRUE)
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

#### Einstellungen des Benutzers ####
#
# Was eine Lehrkraft nicht bei jedem Start neu eintippen soll (Name, Signatur,
# Link zur Uebungssammlung, Absender, Itemzahl, Ansicht) steht in einer
# Textdatei neben den Ausgaben des Benutzers. Bewusst kein JSON (kein
# zusaetzliches Paket) und kein Excel-Format (Umlaute) - die Datei ist zum
# Anschauen und Bearbeiten gedacht.

.einstellungen_default <- function() {
  list(lehrername = "",
       signatur = "",
       qrlink = "",
       info_absender = "",
       numitems = "40",
       plot_diff = "nein",
       plot_gesamt = "ja",
       plot_typ = "Histogramm",
       # Zwei Marken des R/F-Werts in Prozent. Sie haben (noch) kein Eingabefeld
       # in der Oberflaeche, sind aber hier einstellbar. Die Kategorien der
       # Kinder haengen NICHT daran (siehe .rf_stufen).
       rf_referenz = "71.3",
       rf_norm_unten = "65")
}

# Schluessel, die es nicht mehr gibt. Die Anrede der Briefe wird aus den Klassen
# gebildet (siehe infobrief_anrede), ein Feld "Klassenleitung" gibt es nicht
# mehr. Alte Zeilen werden beim naechsten Speichern entfernt.
.einstellungen_veraltet <- c("info_klassenleitung")

einstellungen_pfad <- function() {
  .pfad_nativ(file.path(benutzer_ausgabeordner(), "einstellungen.txt"))
}

.als_wahr <- function(x) {
  if (is.logical(x)) return(isTRUE(x))
  tolower(trimws(as.character(x))) %in% c("ja", "true", "wahr", "1", "yes")
}

# Datei einlesen: fehlende Werte kommen aus den Standardwerten, unbekannte
# Schluessel bleiben erhalten (siehe einstellungen_schreiben).
einstellungen_lesen <- function(pfad = einstellungen_pfad()) {
  werte <- .einstellungen_default()
  if (!file.exists(pfad)) return(werte)

  zeilen <- tryCatch(readLines(pfad, warn = FALSE, encoding = "UTF-8"),
                     error = function(e) character(0))
  for (zeile in zeilen) {
    zeile <- trimws(zeile)
    if (!nzchar(zeile) || startsWith(zeile, "#")) next
    teile <- strsplit(zeile, "=", fixed = TRUE)[[1]]
    if (length(teile) < 2) next
    schluessel <- tolower(trimws(teile[1]))
    wert <- trimws(paste(teile[-1], collapse = "="))
    if (nzchar(schluessel)) werte[[schluessel]] <- wert
  }
  werte
}

# Schreiben: bekannte Schluessel in fester Reihenfolge; unbekannte Schluessel
# aus einer vorhandenen Datei bleiben erhalten (Handeintraege, spaetere
# Versionen). Schreibfehler werden gemeldet, aber nicht als Fehler geworfen -
# beim Tippen darf kein Fenster aufgehen.
einstellungen_schreiben <- function(werte, pfad = einstellungen_pfad()) {
  # Schluessel ohne Feld in der Oberflaeche gehoeren nicht mehr in die Datei
  for (name in .einstellungen_veraltet) werte[[name]] <- NULL

  if (!verzeichnis_sicherstellen(dirname(pfad))) {
    message("Einstellungen konnten nicht gespeichert werden - Ordner nicht ",
            "beschreibbar: ", dirname(pfad))
    return(invisible(FALSE))
  }

  bekannt <- names(.einstellungen_default())
  if (file.exists(pfad)) {
    vorhanden <- einstellungen_lesen(pfad)
    for (name in setdiff(names(vorhanden), c(bekannt, .einstellungen_veraltet))) {
      if (is.null(werte[[name]])) werte[[name]] <- vorhanden[[name]]
    }
  }

  als_text <- function(name) {
    wert <- werte[[name]]
    if (is.null(wert) || length(wert) == 0) "" else as.character(wert)[1]
  }
  reihenfolge <- unique(c(bekannt, setdiff(names(werte), bekannt)))
  zeilen <- c(
    "# C-Test Auswertung - Einstellungen",
    "# Diese Datei pflegt die App selbst: Werte in der App aendern, sie werden",
    "# hier gespeichert und beim naechsten Start wieder eingesetzt.",
    "# Zeilen mit '#' sind Kommentar und werden nicht ausgewertet.",
    "#",
    "# rf_referenz / rf_norm_unten: zwei Marken des R/F-Werts in Prozent.",
    "#   rf_referenz   = Referenzwert (gestrichelte Linie im Diagramm).",
    "#   rf_norm_unten = unterer Normbereich (gepunktete Linie und die",
    "#                   Markierungen im Lehrkraefte-Infobrief).",
    "#   Gueltig: beide zwischen 0 und 100, rf_norm_unten < rf_referenz.",
    "#   Ungueltige Angaben werden beim Start ignoriert, dann gelten 71.3 und 65.",
    "#   Die Kategorien der Kinder sind fest und unabhaengig von diesen Werten.",
    "#",
    sprintf("%s=%s", reihenfolge, vapply(reihenfolge, als_text, character(1)))
  )

  con <- file(pfad, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(zeilen, con, useBytes = FALSE)
  invisible(TRUE)
}

# Werte aus der Oberflaeche um die beiden R/F-Marken ergaenzen. Steht in der
# Datei schon ein Wert (auch ein ungueltiger), bleibt er erhalten - ein Handedit
# darf nicht ueberschrieben werden. Bekommt spaeter einmal ein Eingabefeld in
# der Oberflaeche Vorrang, muss hier die Prioritaet gedreht werden (dann gewinnt
# der uebergebene Wert).
einstellungen_mit_referenz <- function(werte, pfad = einstellungen_pfad()) {
  roh <- if (file.exists(pfad)) einstellungen_lesen(pfad) else list()
  marken <- referenzwerte()
  standard <- c(rf_referenz = as.character(marken$referenz),
                rf_norm_unten = as.character(marken$norm_unten))
  for (name in names(standard)) {
    vorhanden <- roh[[name]]
    werte[[name]] <- if (is.null(vorhanden) || !nzchar(vorhanden)) standard[[name]] else vorhanden
  }
  werte
}

# Einstellungsdatei anlegen (falls noetig) und ihren Pfad zurueckgeben
einstellungen_bereitstellen <- function(werte = NULL, pfad = einstellungen_pfad()) {
  if (!file.exists(pfad)) {
    if (is.null(werte)) werte <- list()
    einstellungen_schreiben(einstellungen_mit_referenz(werte, pfad), pfad)
  }
  if (!file.exists(pfad)) {
    stop("Die Einstellungen konnten nicht angelegt werden: ", pfad, call. = FALSE)
  }
  pfad
}

# Die beiden R/F-Marken aus den Einstellungen:
#   referenz   - Referenzwert (gestrichelte Linie)
#   norm_unten - unterer Normbereich (gepunktete Linie, Infobrief)
# Sie beeinflussen die Kategorien nicht. Unbrauchbare Angaben werden gemeldet
# und durch die Standardwerte ersetzt - ein Tippfehler in der Datei darf keine
# falschen Markierungen erzeugen.
referenzwerte <- function(neu = FALSE) {
  if (!neu && !is.null(.ctest_env$referenzwerte)) return(.ctest_env$referenzwerte)

  werte <- einstellungen_lesen()
  referenz <- suppressWarnings(as.numeric(werte$rf_referenz))
  norm_unten <- suppressWarnings(as.numeric(werte$rf_norm_unten))
  standard <- list(referenz = 71.3, norm_unten = 65)
  unbrauchbar <- is.na(referenz) || is.na(norm_unten)

  if (!unbrauchbar && (norm_unten <= 0 || referenz > 100 ||
                       norm_unten >= referenz)) {
    message("Die R/F-Marken in den Einstellungen sind unbrauchbar (rf_referenz=",
            werte$rf_referenz, ", rf_norm_unten=", werte$rf_norm_unten,
            "). Erwartet: beide zwischen 0 und 100 und rf_norm_unten < ",
            "rf_referenz - es gelten 71.3 und 65.")
    unbrauchbar <- TRUE
  }

  ergebnis <- if (unbrauchbar) standard else
    list(referenz = referenz, norm_unten = norm_unten)
  .ctest_env$referenzwerte <- ergebnis
  ergebnis
}

# Grenze des unteren Normbereichs je Kennzahl:
#   R/F - der eingestellte Wert (rf_norm_unten)
#   WE  - der feste Wert des Verfahrens (.we_normgrenze)
.normgrenze <- function(kennzahl = c("R/F", "WE")) {
  kennzahl <- match.arg(kennzahl)
  if (kennzahl == "R/F") referenzwerte()$norm_unten else .we_normgrenze
}

# Fuer Tests: gemerkte Referenzwerte vergessen
referenzwerte_zuruecksetzen <- function() {
  if (exists("referenzwerte", envir = .ctest_env, inherits = FALSE)) {
    rm("referenzwerte", envir = .ctest_env)
  }
  invisible(TRUE)
}

# Datei oder Ordner mit dem Standardprogramm oeffnen (fuer Tests ersetzbar)
oeffne_datei <- function(pfad) {
  utils::browseURL(pfad)
  invisible(TRUE)
}

# Spaltennamen in tsv pruefen
checkColumnNames <- function(df1, df2) {
  # nur die fachlichen Spalten vergleichen: die Itemzahl kann in der einen
  # Tabelle stehen und in der anderen fehlen (Altdateien)
  spalten <- intersect(colnames(df1), .schueler_spalten)
  if (length(spalten) == 0) spalten <- colnames(df1)
  return(all(spalten %in% colnames(df2)))
}

# Median und Mittelwert berechnen und aufbereiten
createStatsText <- function(df, column, label, multiple = FALSE) {
  kennzahlen <- function(text) {
    div(HTML(text),
        style = "margin-left:15px;
             margin-right:15px;
             font-size: 20px;
             font-style: bold")
  }

  # fehlende Spalte oder keine Werte duerfen die Ansicht nicht sprengen
  if (!column %in% names(df)) {
    return(kennzahlen(paste0("Spalte '", column, "' fehlt in den Daten.")))
  }
  if (all(is.na(df[[column]]))) {
    return(kennzahlen(paste0(label, ": keine Werte vorhanden. Anzahl: ", dim(df)[1])))
  }
  # je Klasse nur, wenn es die Spalte gibt - sonst zusammenfassend
  if (multiple && !"Klasse" %in% names(df)) multiple <- FALSE

  if(!multiple) {
    l1 <- paste0("Mittelwert ", label, ": ", round(mean(df[[column]], na.rm = TRUE), 1), "±", round(sd(df[[column]], na.rm = TRUE), 1), "%")
    l2 <- paste0("Median ", label, ": ", round(median(df[[column]], na.rm = TRUE), 1), "%")
    l3 <- paste0("Anzahl: ", dim(df)[1])
  } else {
    stats <- 
      df %>% 
      summarise(mean = round(mean(.data[[column]], na.rm = TRUE), 1),
                sd = round(sd(.data[[column]], na.rm = TRUE), 1), 
                median = round(median(.data[[column]], na.rm = TRUE), 1), 
                n = n(),
                .by = "Klasse")
    l1 <- paste0("Mittelwert ", label, ": ")
    l2 <- paste0("Median ", label, ": ")
    l3 <- paste0("Anzahl: ")
    for(i in seq.int(dim(stats)[1])) {
      l1 <- paste0(l1, stats$mean[i], "±", stats$sd[i], " (", stats$Klasse[i])
      l2 <- paste0(l2, stats$median[i], " (", stats$Klasse[i])
      l3 <- paste0(l3, stats$n[i], " (", stats$Klasse[i])
      if(i != dim(stats)[1]) {
        l1 <- paste0(l1, "), ")
        l2 <- paste0(l2, "), ")
        l3 <- paste0(l3, "), ")
      } else {
        l1 <- paste0(l1, ")")
        l2 <- paste0(l2, ")")
        l3 <- paste0(l3, ")")
      }
    }
  }
                
  
  return(kennzahlen(paste(l1, l2, l3, sep = "<br/>")))
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

addEntry <- function(df, name, klasse, rf, we, numItems) {
  rfPerc <- round(rf/numItems*100,1)
  wePerc <- round(we/numItems*100,1)
  
  kat <- paste0(getRFlevel(rfPerc), 
                getWElevel(rfPerc, wePerc))
  new <- tibble("Name" = name,
                "Klasse" = klasse,
                "WE-Wert" = we,
                "WE-%" = wePerc,
                "R/F-Wert" = rf,
                "R/F-%" = rfPerc,
                "Kat." = kat,
                "Empfehlung" = getRecommendation(kat),
                # Itemzahl je Kind mitschreiben: damit lassen sich fehlende
                # Prozent- oder Rohwerte spaeter nachrechnen. Sie wird nur in der
                # tsv gefuehrt (nicht angezeigt, nicht in den Berichten).
                "Items" = as.numeric(numItems))
  
  df <- bind_rows(df, new)
  
  return(df)
}

#### Datensicherung vor dem Briefversand ####

# Fingerabdruck des Datenstands fuer die Frage "schon gesichert?".
# Spalten kanonisch, Zeilen sortiert (radix = unabhaengig von der Sprache),
# Zahlen mit fester Stellenzahl: damit spielt die Zeilenreihenfolge keine Rolle
# und 70.3 ist derselbe Wert wie 70.30.
daten_fingerabdruck <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NA_character_)

  spalten <- intersect(c(.schueler_spalten, .items_spalte), colnames(df))
  if (length(spalten) == 0) spalten <- colnames(df)

  als_text <- function(x) {
    if (is.numeric(x)) {
      ifelse(is.na(x), "NA", formatC(round(x, 6), format = "f", digits = 6))
    } else {
      ifelse(is.na(x), "NA", as.character(x))
    }
  }

  zeilen <- do.call(paste, c(lapply(df[spalten], als_text), list(sep = "\u001f")))
  paste(sort(zeilen, method = "radix"), collapse = "\u001e")
}

# Dateiname der Sicherung: Datum, Klassen, Uhrzeit. Aufgebaut wie bei
# "speichern", damit im Auswertungsordner erkennbar bleibt, wozu die Datei
# gehoert.
dateiname_sicherung <- function(df) {
  fn <- paste0("C-Test_Auswertung_", Sys.Date())

  if ("Klasse" %in% colnames(df)) {
    klassen <- sort(unique(as.character(df$Klasse)))
    klassen <- klassen[!is.na(klassen) & nzchar(klassen)]
    if (length(klassen) > 0) fn <- paste0(fn, "_", paste0(klassen, collapse = "_"))
  }

  paste0(fn, "_sicherung_", format(Sys.time(), "%H%M%S"))
}

# Merken, welcher Datenstand bereits als Datei vorliegt (nach "speichern" oder
# nach dem Laden einer tsv).
merke_gesicherten_stand <- function(df) {
  .ctest_env$gesicherter_stand <- daten_fingerabdruck(df)
  invisible(TRUE)
}

# Fuer Tests: gemerkten Stand vergessen
sicherungsstand_zuruecksetzen <- function() {
  if (exists("gesicherter_stand", envir = .ctest_env, inherits = FALSE)) {
    rm("gesicherter_stand", envir = .ctest_env)
  }
  invisible(TRUE)
}

# Datenstand als tsv sichern, falls er noch nicht gesichert ist.
#
# Wird vor dem Erstellen der Briefe aufgerufen: "speichern" ist ein eigener
# Knopf, und ohne ihn existierte der Datensatz nach dem Briefversand nur noch
# als Word-Datei - kein Vorjahresvergleich, keine Itemzahl, keine
# Nachauswertung. Geschrieben wird nur die tsv (keine Berichte), nichts wird
# ueberschrieben und es wird nicht gefragt.
#
# Rueckgabe: list(geschrieben, datei, meldung). Fehler kommen als Meldung
# zurueck, damit sie den Briefversand nie blockieren.
sichere_daten <- function(df) {
  abbruch <- function(meldung) {
    list(geschrieben = FALSE, datei = NA_character_, meldung = meldung)
  }

  if (is.null(df) || nrow(df) == 0) {
    return(abbruch("Keine Daten zum Sichern vorhanden."))
  }

  stand <- daten_fingerabdruck(df)
  if (!is.na(stand) &&
      exists("gesicherter_stand", envir = .ctest_env, inherits = FALSE) &&
      identical(.ctest_env$gesicherter_stand, stand)) {
    return(abbruch("Der Datenstand ist bereits gesichert."))
  }

  pfad <- tryCatch({
    # Uhrzeit im Namen: mehrere Staende am selben Tag stehen nebeneinander,
    # vorhandene Dateien werden nie ueberschrieben
    fn <- dateiname_sicherung(df)
    ziel <- createFilePath(fn, "tsv")
    zaehler <- 1
    while (file.exists(ziel)) {
      zaehler <- zaehler + 1
      ziel <- createFilePath(paste0(fn, "_", zaehler), "tsv")
    }
    readr::write_tsv(df, file = ziel)
    ziel
  }, error = function(e) {
    message("Sicherung des Datenstands fehlgeschlagen: ", conditionMessage(e))
    NULL
  })

  if (is.null(pfad)) {
    return(abbruch("Daten konnten nicht zusaetzlich gesichert werden."))
  }

  .ctest_env$gesicherter_stand <- stand
  list(geschrieben = TRUE, datei = pfad,
       meldung = paste0("Datenstand zusaetzlich gesichert: ", basename(pfad)))
}

saveData <- function(df, vergleich = NULL) {
  fn <- paste0("C-Test_Auswertung_", Sys.Date())
  
  if("Klasse" %in% colnames(df)) {
    kl <- paste0(unique(df$Klasse), collapse = "_")
    fn <- paste0(fn, "_", kl)
  }

  # Vor dem Schreiben pruefen: eine in Word geoeffnete Datei kann nicht ersetzt
  # werden, und der Fehler des Renderers ist schwer zu deuten
  basis <- .pfad_nativ(file.path(createFilePath(NULL, ""), fn))
  .pruefe_datei_frei(createFilePath(fn, "tsv"))
  .pruefe_datei_frei(paste0(basis, ".docx"))
  .pruefe_datei_frei(paste0(basis, ".xlsx"))
  
  # Die tsv ist das Datenformat und fuehrt die Itemzahl mit (damit fehlende Werte
  # spaeter nachrechenbar sind). Word und Excel sind Berichte fuer die Lehrkraft -
  # dort steht die Itemzahl nicht.
  write_tsv(df, 
            file = createFilePath(fn, "tsv"))
  # die tsv liegt jetzt auf der Platte: eine Sicherung vor dem Briefversand
  # waere doppelt
  merke_gesicherten_stand(df)
  bericht <- df[, setdiff(colnames(df), .items_spalte), drop = FALSE]
  table2doc_(bericht, 
             file = createFilePath(fn, ""), 
             digits = 1, 
             width = 8.3,
             height = 11.7,
             pointsize = 7)
  
  table2spreadsheet_(bericht, 
                     file = createFilePath(fn, ""), 
                     sheetName = "C-Test", 
                     digits = 1)
  
  msgs <- paste0("Daten gespeichert unter ", createFilePath(NULL, ""))
  
  # Vergleichstabelle (zwei Stufen, mindestens ein zugeordnetes Kind) anhaengen
  if(!is.null(vergleich) && nrow(vergleich) > 0) {
    msgs <- paste0(msgs, vergleich_anhaengen(createFilePath(fn, "xlsx"),
                                             createFilePath(fn, "docx"),
                                             vergleich))
  }
  
  return(msgs)
}

# Vergleichstabelle als zweites Blatt im Excel und als Anhang im Word ablegen.
# Fehler dabei duerfen das Speichern nicht scheitern lassen (Hauptdaten sind
# bereits geschrieben) - deshalb nur Meldungen.
vergleich_anhaengen <- function(xlsx_datei, docx_datei, vergleich) {
  hinweis <- paste0(" (inkl. Vergleichstabelle mit ", nrow(vergleich), " Kindern)")

  tryCatch({
    wb <- openxlsx::loadWorkbook(xlsx_datei)
    openxlsx::addWorksheet(wb, "Vergleich")
    openxlsx::writeData(wb, sheet = "Vergleich", x = vergleich, colNames = TRUE)
    openxlsx::addStyle(wb, sheet = "Vergleich",
                       style = openxlsx::createStyle(textDecoration = "bold"),
                       rows = 1, cols = seq_len(ncol(vergleich)), stack = TRUE)
    openxlsx::saveWorkbook(wb, xlsx_datei, overwrite = TRUE)
  }, error = function(e) {
    message("Vergleichsblatt im Excel konnte nicht ergaenzt werden: ",
            conditionMessage(e))
  })

  tryCatch({
    doc <- officer::read_docx(docx_datei)
    doc <- officer::body_add_break(doc)
    # ohne Word-Stil (die Vorlage kennt "heading 1" nicht) - stattdessen fett
    doc <- officer::body_add_fpar(doc, officer::fpar(
      officer::ftext("Vergleich je Kind (zwei Stufen)",
                     officer::fp_text(bold = TRUE, font.size = 14))))
    # im Word-Anhang ohne Hinweisspalte (Platz auf der Seite)
    ft <- tabelle_infobrief(
      vergleich[, setdiff(colnames(vergleich), "Hinweis"), drop = FALSE],
      farb_spalten = c("\u0394 WE", "\u0394 R/F"),
      vorzeichen_spalten = c("\u0394 WE", "\u0394 R/F"))
    if(!is.null(ft)) doc <- flextable::body_add_flextable(doc, value = ft)
    print(doc, target = docx_datei)
  }, error = function(e) {
    message("Anhang im Word-Dokument konnte nicht ergaenzt werden: ",
            conditionMessage(e))
  })

  return(hinweis)
}

checkInputFile <- function(inputFile) {
  req(inputFile)
  ext <- tools::file_ext(inputFile$datapath)
  
  validate(need(ext == "tsv", "Bitte tsv Datei auswählen"))
  return(inputFile$datapath)
}

#### Robuste Eingabe: geladene Tabellen in die erwartete Form bringen ####
# Die App erwartet acht Spalten (Reihenfolge wie in rv$df, siehe server.R).
# "Items" (Anzahl der Test-Items je Kind) kommt als neunte Spalte dazu: sie wird
# nur in der tsv gefuehrt (nicht in der Uebersichtstabelle und nicht in den
# Berichten) und macht fehlende Prozentwerte nachrechenbar.
.schueler_spalten <- c("Name", "Klasse", "WE-Wert", "WE-%", "R/F-Wert", "R/F-%",
                       "Kat.", "Empfehlung")
.items_spalte <- "Items"
.alle_spalten <- c(.schueler_spalten, .items_spalte)

# Namen vergleichbar machen: Kleinschreibung, Umlaute aufloesen
.name_falten <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("\u00e4", "ae", x, fixed = TRUE)
  x <- gsub("\u00f6", "oe", x, fixed = TRUE)
  x <- gsub("\u00fc", "ue", x, fixed = TRUE)
  x <- gsub("\u00df", "ss", x, fixed = TRUE)
  x
}

# Schluessel fuer den Namensvergleich: zusaetzlich Trennzeichen weg und "%" als
# "prozent" - so passen "WE %", "we-%", "WE-Prozent" zusammen.
.schluessel <- function(x) {
  x <- gsub("%", "prozent", .name_falten(x), fixed = TRUE)
  gsub("[^a-z0-9]", "", x)
}

# Bestandteile eines Spaltennamens (getrennt an allem ausser Buchstaben/Ziffern)
.spalten_tokens <- function(x) {
  strsplit(.name_falten(x), "[^a-z0-9]+")
}

# Gebraeuchliche Schreibweisen je Spalte (zusaetzlich zum Namen selbst).
# Hier stehen vor allem Kuerzel, die kein erkennbares Wort enthalten (wep, rfp) -
# alles andere deckt die Merkmalserkennung unten ab.
.schueler_alias <- list(
  "Name"       = c("schueler", "schuelerin", "schuelername", "nachname"),
  "Klasse"     = c("klassen", "klassenname", "klassenstufe"),
  "WE-Wert"    = c("we", "wepunkte", "wepunktzahl", "wepwert"),
  "WE-%"       = c("weprozent", "weproz", "weinprozent", "wep"),
  "R/F-Wert"   = c("rf", "rfwert", "rfpunkte", "rfwertpunkte", "rfpwert"),
  "R/F-%"      = c("rfprozent", "rfproz", "rfinprozent", "rfp"),
  "Kat."       = c("kat", "kategorie"),
  "Empfehlung" = c("empfehlungen", "hinweis", "empfehlungstext"),
  "Items"      = c("item", "itemzahl", "anzahl", "testitems", "numitems", "nitems")
)

# Merkmale eines Spaltennamens: sagt, was die Spalte inhaltlich ist - unabhaengig
# von der genauen Schreibweise ("we_percent", "WE-Anteil (%)", "percentage WE").
.spalten_merkmale <- function(namen) {
  tokens <- .spalten_tokens(namen)
  hat <- function(t, woerter) any(t %in% woerter)
  prozent <- grepl("%", as.character(namen), fixed = TRUE) |
    vapply(tokens, function(t) hat(t, c("prozent", "prozentual", "proz", "percent",
                                        "percentage", "pct")), logical(1)) |
    vapply(tokens, function(t) hat(t, "per") && hat(t, "cent"), logical(1))
  data.frame(
    prozent = prozent,
    wert = vapply(tokens, function(t) hat(t, c("wert", "punkte", "score", "raw")), logical(1)),
    we = vapply(tokens, function(t) hat(t, c("we", "wep", "worterkennung", "wortschatz",
                                             "vocabulary", "recognition")), logical(1)),
    rf = vapply(tokens, function(t) hat(t, c("rf", "rfp", "rechtschreibung", "richtig",
                                             "spelling", "orthography")) ||
                  (hat(t, "r") && hat(t, "f")), logical(1)),
    name = vapply(tokens, function(t) hat(t, c("name", "schueler", "schuelerin",
                                               "schuelername", "nachname")), logical(1)),
    klasse = vapply(tokens, function(t) hat(t, c("klasse", "klassen", "klassenname",
                                                 "klassenstufe")), logical(1)),
    kat = vapply(tokens, function(t) hat(t, c("kat", "kategorie")), logical(1)),
    empfehlung = vapply(tokens, function(t) hat(t, c("empfehlung", "empfehlungen", "hinweis")), logical(1)),
    items = vapply(tokens, function(t) hat(t, c("items", "item", "itemzahl", "anzahl",
                                                "testitems", "numitems", "nitems")), logical(1)),
    stringsAsFactors = FALSE
  )
}

# Spalten der Datei den erwarteten Spalten zuordnen. Geschichtet, damit nichts
# geraten wird, was eindeutig ist:
#   1. exakter Name        ("WE-%" -> "WE-%")
#   2. bekannte Schreibweise (Alias-Liste, z. B. Kuerzel)
#   3. Merkmale            (Tokens; Prozentspalten zuerst)
# Jede Spalte der Datei wird nur einmal vergeben. Mehrdeutigkeiten werden
# gemeldet, fehlende Spalten ebenfalls.
.ordne_spalten <- function(namen) {
  schluessel <- vapply(namen, .schluessel, character(1), USE.NAMES = FALSE)
  merkmale <- .spalten_merkmale(namen)
  frei <- rep(TRUE, length(namen))
  hinweise <- character(0)

  regel <- list(
    "Name"       = list(exakt = "Name",       merkmale = merkmale$name),
    "Klasse"     = list(exakt = "Klasse",     merkmale = merkmale$klasse),
    "WE-%"       = list(exakt = "WE-%",       merkmale = merkmale$prozent & merkmale$we),
    "R/F-%"      = list(exakt = "R/F-%",      merkmale = merkmale$prozent & merkmale$rf),
    "WE-Wert"    = list(exakt = "WE-Wert",    merkmale = merkmale$we & !merkmale$prozent),
    "R/F-Wert"   = list(exakt = "R/F-Wert",   merkmale = merkmale$rf & !merkmale$prozent),
    "Kat."       = list(exakt = "Kat.",       merkmale = merkmale$kat),
    "Empfehlung" = list(exakt = "Empfehlung", merkmale = merkmale$empfehlung)
  )
  # Name und Klasse zuerst, dann Prozentspalten, dann Werte, dann der Rest:
  # sonst wuerde z. B. "we_percent" bei WE-Wert landen
  reihenfolge <- c("Name", "Klasse", "WE-%", "R/F-%", "WE-Wert", "R/F-Wert",
                   "Kat.", "Empfehlung", "Items")

  zuordnung <- list()
  for (spalte in reihenfolge) {
    kandidaten <- integer(0)
    if (spalte == "Items") {
      kandidaten <- which(frei & merkmale$items)
    } else {
      # 1. exakter Name
      kandidaten <- which(frei & schluessel == .schluessel(regel[[spalte]]$exakt))
      # 2. bekannte Schreibweise
      aliase <- .schueler_alias[[spalte]]
      if (length(kandidaten) == 0 && length(aliase) > 0) {
        kandidaten <- which(frei & schluessel %in% aliase)
      }
      # 3. Merkmale
      if (length(kandidaten) == 0) {
        kandidaten <- which(frei & regel[[spalte]]$merkmale)
      }
    }
    if (length(kandidaten) == 0) next
    zuordnung[[spalte]] <- namen[kandidaten[1]]
    frei[kandidaten[1]] <- FALSE
    if (length(kandidaten) > 1) {
      hinweise <- c(hinweise, paste0("Mehrere Kandidaten fuer '", spalte, "': ",
                                     paste(namen[kandidaten], collapse = ", "),
                                     " - verwendet wurde '", namen[kandidaten[1]], "'."))
    }
  }

  if (any(frei)) {
    hinweise <- c(hinweise, paste0("Nicht verwendet: ",
                                   paste(namen[frei], collapse = ", "), "."))
  }

  list(zuordnung = zuordnung, hinweise = hinweise)
}

# Geladene Tabelle in die erwartete Form bringen. Gibt die Tabelle zurueck; die
# Hinweise haengen als Attribut "hinweise" daran (siehe lade_hinweise).
pruefe_schuelerdaten <- function(df, quelle = "") {
  if (is.null(df)) df <- tibble::tibble()
  namen <- names(df)
  if (length(namen) == 0) {
    stop(paste0("Die Datei enthaelt keine Spalten",
                if (nzchar(quelle)) paste0(" (", quelle, ")") else "", "."),
         call. = FALSE)
  }

  ordnung <- .ordne_spalten(namen)
  zuordnung <- ordnung$zuordnung
  hinweise <- ordnung$hinweise

  if (is.null(zuordnung[["Name"]])) {
    stop(paste0("In der Datei fehlt die Spalte 'Name'",
                if (nzchar(quelle)) paste0(" (", quelle, ")") else "",
                ". Ohne Namen lassen sich die Ergebnisse nicht zuordnen. ",
                "Gefundene Spalten: ", paste(namen, collapse = ", "), "."),
         call. = FALSE)
  }

  # leere Huelle mit der richtigen Zeilenzahl
  ausgabe <- tibble::tibble(.rows = nrow(df))
  for (spalte in .alle_spalten) {
    gefunden <- zuordnung[[spalte]]
    if (is.null(gefunden)) {
      ausgabe[[spalte]] <- rep(NA_character_, nrow(df))
      if (spalte != .items_spalte) {
        hinweise <- c(hinweise, paste0("Spalte '", spalte, "' fehlt in der Datei."))
      }
    } else {
      ausgabe[[spalte]] <- as.character(df[[gefunden]])
      if (!identical(gefunden, spalte)) {
        hinweise <- c(hinweise, paste0("Spalte '", gefunden, "' als '", spalte,
                                       "' gelesen."))
      }
    }
  }

  ausgabe <- .zahlen_konvertieren(ausgabe)

  # Prozentwerte, die keine sein koennen, kommen nicht in die Auswertung
  ausgabe <- .prozentbereich_pruefen(ausgabe, zuordnung, hinweise)
  hinweise <- attr(ausgabe, "hinweise")

  # Itemzahl je Zeile aus Wert und Prozentwert zurueckrechnen (nur wo das
  # zweifelsfrei geht) - danach pruefen und ergaenzen die Schritte unten
  ausgabe <- .itemzahl_ergaenzen(ausgabe, hinweise)
  hinweise <- attr(ausgabe, "hinweise")

  # Itemzahl belastbar? (Zeilen mit Wert+Prozent+Items bzw. Rasterprobe)
  itemzahl <- .itemzahl_pruefen(ausgabe)
  if (!is.null(itemzahl$hinweis)) hinweise <- c(hinweise, itemzahl$hinweis)

  # fehlende Prozentwerte berechnen, fehlende Rohwerte nur mit bestandener Probe
  ausgabe <- .ergaenze_werte(ausgabe, hinweise, itemzahl)
  hinweise <- attr(ausgabe, "hinweise")

  # Plausibilitaet der Werte
  ausgabe <- .pruefe_plausibilitaet(ausgabe, hinweise)
  hinweise <- attr(ausgabe, "hinweise")

  # Kategorie und Empfehlung ergaenzen (nach der Nachrechnung)
  ausgabe <- .ergaenze_kategorie(ausgabe, zuordnung, hinweise)
  hinweise <- attr(ausgabe, "hinweise")

  attr(ausgabe, "hinweise") <- unique(hinweise)
  ausgabe
}

# Text in Zahlen wandeln; deutsches Komma erlaubt, Unlesbares wird NA
.zahlen_konvertieren <- function(ausgabe) {
  als_zahl <- function(x) {
    suppressWarnings(as.numeric(gsub(",", ".", gsub("[^0-9,.-]", "", as.character(x)),
                                     fixed = TRUE)))
  }
  for (spalte in c("WE-Wert", "WE-%", "R/F-Wert", "R/F-%", .items_spalte)) {
    ausgabe[[spalte]] <- als_zahl(ausgabe[[spalte]])
  }
  ausgabe[["Klasse"]][is.na(ausgabe[["Klasse"]])] <- ""
  ausgabe[["Name"]] <- trimws(ausgabe[["Name"]])
  ausgabe
}

# Prozentwerte muessen zwischen 0 und 100 liegen. Passt eine Spalte nicht, ist
# sie keine Prozentspalte - dann wird sie verworfen (spaeter ggf. aus Wert und
# Itemzahl nachgerechnet) statt falsche Zahlen zu uebernehmen.
.prozentbereich_pruefen <- function(ausgabe, zuordnung, hinweise) {
  for (spalte in c("WE-%", "R/F-%")) {
    if (is.null(zuordnung[[spalte]])) next
    werte <- ausgabe[[spalte]]
    if (all(is.na(werte))) next
    if (any(werte < 0 | werte > 100, na.rm = TRUE)) {
      ausgabe[[spalte]] <- rep(NA_real_, nrow(ausgabe))
      hinweise <- c(hinweise, paste0("Spalte '", zuordnung[[spalte]],
                                     "' enthaelt Werte ausserhalb 0 bis 100 % und ",
                                     "wird nicht verwendet."))
    }
  }
  attr(ausgabe, "hinweise") <- hinweise
  ausgabe
}

# Itemzahl je Zeile aus Wert und Prozentwert zurueckrechnen:
#   Items = Wert * 100 / Prozent
# Nur wo das zweifelsfrei geht, wird geschrieben:
#   - Wert UND Prozentwert muessen in derselben Zeile stehen (eines der beiden
#     Paare WE oder R/F genuegt)
#   - die Itemzahl muss (nahezu) ganzzahlig sein
#   - mit ihr muss round(Wert / Items * 100, 1) den Prozentwert der Datei exakt
#     reproduzieren (sonst waere der Wert nur geraten)
#   - sind beide Paare vollstaendig, muessen sie dieselbe Itemzahl ergeben
# Eine vorhandene Itemzahl wird nie ueberschrieben. Zeilen ohne Werte bleiben
# leer - dort ist die Itemzahl nicht berechenbar.
.itemzahl_ergaenzen <- function(ausgabe, hinweise, toleranz = 0.05,
                                max_items = 500) {
  if (is.null(ausgabe[[.items_spalte]])) return(ausgabe)
  items <- ausgabe[[.items_spalte]]

  # Kandidat ist die gerundete Itemzahl; entscheidend ist nicht, wie genau sie
  # aus dem Prozentwert faellt (bei kleinen Werten streut das), sondern ob sie
  # den Prozentwert der Datei reproduziert.
  kandidat <- function(wert, prozent) {
    moeglich <- !is.na(wert) & !is.na(prozent) & wert > 0 & prozent > 0
    k <- rep(NA_real_, length(wert))
    k[moeglich] <- round(wert[moeglich] * 100 / prozent[moeglich])
    gueltig <- moeglich & !is.na(k) & k >= 1 & k <= max_items
    nach <- gueltig
    nach[gueltig] <- abs(round(wert[gueltig] / k[gueltig] * 100, 1) -
                           prozent[gueltig]) <= toleranz
    k[!nach] <- NA_real_
    k
  }

  we <- kandidat(ausgabe[["WE-Wert"]], ausgabe[["WE-%"]])
  rf <- kandidat(ausgabe[["R/F-Wert"]], ausgabe[["R/F-%"]])

  beide <- !is.na(we) & !is.na(rf)
  sicher <- (!is.na(we) & is.na(rf)) | (!is.na(rf) & is.na(we)) | (beide & we == rf)
  neu <- ifelse(!is.na(we), we, rf)

  leer <- is.na(items)
  fuellbar <- leer & sicher & !is.na(neu)
  if (any(fuellbar)) {
    items[fuellbar] <- neu[fuellbar]
    hinweise <- c(hinweise, paste0("Itemzahl fuer ", sum(fuellbar),
                                   " Kind(er) aus Wert und Prozentwert ermittelt."))
  }
  widerspruch <- leer & beide & we != rf
  if (any(widerspruch)) {
    hinweise <- c(hinweise, paste0("Itemzahl fuer ", sum(widerspruch),
                                   " Kind(er) nicht uebernommen (WE und R/F ",
                                   "ergeben verschiedene Werte) - bitte pruefen."))
  }

  ausgabe[[.items_spalte]] <- items
  attr(ausgabe, "hinweise") <- hinweise
  ausgabe
}

# Itemzahl pruefen. Zwei Wege, weil die Richtungen unterschiedlich belastbar
# sind:
#   ok_wert   - Zeilen mit Wert, Prozentwert UND Itemzahl: passt Wert/Items*100
#               zum Prozentwert? (starke Probe)
#   ok_raster - liegen die Prozentwerte auf dem Raster, das die Itemzahl
#               erzeugt (0, 1/Items, 2/Items ...)? (mittelstarke Probe)
# Nur mit einer bestandenen Probe werden fehlende Werte nachgerechnet.
.itemzahl_pruefen <- function(ausgabe, toleranz = 0.05) {
  if (is.null(ausgabe[[.items_spalte]]) || all(is.na(ausgabe[[.items_spalte]]))) {
    return(list(ok_wert = FALSE, ok_raster = FALSE, hinweis = NULL))
  }
  passt <- 0
  gesamt <- 0
  for (kennzahl in list(c("WE-Wert", "WE-%"), c("R/F-Wert", "R/F-%"))) {
    wert <- ausgabe[[kennzahl[1]]]
    prozent <- ausgabe[[kennzahl[2]]]
    items <- ausgabe[[.items_spalte]]
    vollstaendig <- !is.na(wert) & !is.na(prozent) & !is.na(items) & items > 0
    if (!any(vollstaendig)) next
    erwartet <- round(wert[vollstaendig] / items[vollstaendig] * 100, 1)
    passt <- passt + sum(abs(erwartet - prozent[vollstaendig]) <= toleranz)
    gesamt <- gesamt + sum(vollstaendig)
  }
  ok_wert <- gesamt > 0 && passt == gesamt

  # Rasterprobe: jeder Prozentwert muss nahe an k/Items*100 liegen
  auf_raster <- function(prozent, items) {
    vollstaendig <- !is.na(prozent) & !is.na(items) & items > 0
    if (!any(vollstaendig)) return(NA)
    k <- prozent[vollstaendig] / 100 * items[vollstaendig]
    all(abs(k - round(k)) <= toleranz)
  }
  raster <- c(auf_raster(ausgabe[["WE-%"]], ausgabe[[.items_spalte]]),
              auf_raster(ausgabe[["R/F-%"]], ausgabe[[.items_spalte]]))
  ok_raster <- length(raster) > 0 && !any(is.na(raster)) && all(raster)

  hinweis <- NULL
  if (!ok_wert && gesamt > 0) {
    hinweis <- paste0("Itemzahl passt nicht zu den Werten (", gesamt - passt,
                      " von ", gesamt, " Zeilen weichen ab).")
  }
  list(ok_wert = ok_wert, ok_raster = ok_raster, hinweis = hinweis)
}

# Fehlende Prozentwerte aus Wert und Itemzahl berechnen und - wenn die Itemzahl
# geprueft ist - fehlende Rohwerte aus Prozentwert und Itemzahl zurueckrechnen.
# Die Richtungen sind bewusst unterschiedlich streng: der Prozentwert ist die
# Groesse, die die App selbst immer aus Wert und Itemzahl bildet (nachrechnen ist
# also die natuerliche Richtung), der Rohwert ist die Quelldatenangabe - ihn
# zurueckzurechnen ist eine Rekonstruktion und braucht eine bestandene Probe.
.ergaenze_werte <- function(ausgabe, hinweise, itemzahl) {
  items <- ausgabe[[.items_spalte]]
  for (paar in list(c("WE-Wert", "WE-%"), c("R/F-Wert", "R/F-%"))) {
    wert <- ausgabe[[paar[1]]]
    prozent <- ausgabe[[paar[2]]]
    moeglich <- !is.na(items) & items > 0

    fehlt_prozent <- is.na(prozent) & moeglich & !is.na(wert)
    if (any(fehlt_prozent)) {
      ausgabe[[paar[2]]][fehlt_prozent] <-
        round(wert[fehlt_prozent] / items[fehlt_prozent] * 100, 1)
      hinweise <- c(hinweise, paste0("'", paar[2], "' fuer ", sum(fehlt_prozent),
                                     " Kind(er) aus Wert und Itemzahl berechnet."))
    }

    if (!isTRUE(itemzahl$ok_wert) && !isTRUE(itemzahl$ok_raster)) next
    wert <- ausgabe[[paar[1]]]
    prozent <- ausgabe[[paar[2]]]
    fehlt_wert <- is.na(wert) & moeglich & !is.na(prozent)
    if (any(fehlt_wert)) {
      ausgabe[[paar[1]]][fehlt_wert] <-
        round(prozent[fehlt_wert] / 100 * items[fehlt_wert])
      hinweise <- c(hinweise, paste0("'", paar[1], "' fuer ", sum(fehlt_wert),
                                     " Kind(er) aus Prozentwert und Itemzahl ergaenzt."))
    }
  }
  attr(ausgabe, "hinweise") <- hinweise
  ausgabe
}

# Plausibilitaet der (ggf. nachgerechneten) Werte pruefen. Es wird nie etwas
# blockiert - nur gemeldet.
.pruefe_plausibilitaet <- function(ausgabe, hinweise, toleranz = 0.05) {
  we <- ausgabe[["WE-%"]]
  rf <- ausgabe[["R/F-%"]]
  we_w <- ausgabe[["WE-Wert"]]
  rf_w <- ausgabe[["R/F-Wert"]]
  items <- ausgabe[[.items_spalte]]
  beide <- !is.na(we) & !is.na(rf)
  beide_w <- !is.na(we_w) & !is.na(rf_w)

  # WE >= R/F gilt fachlich immer (nur erkannte Woerter koennen richtig
  # geschrieben sein) - fuer Prozentwerte und fuer Rohwerte
  verletzt <- beide & (we < rf - 1e-9)
  if (any(verletzt)) {
    hinweise <- c(hinweise, paste0("Bei ", sum(verletzt), " Kind(ern) ist WE-% ",
                                   "kleiner als R/F-% - das ist nicht moeglich, ",
                                   "bitte pruefen."))
    message("WE-% kleiner als R/F-%: ",
            paste(utils::head(ausgabe[["Name"]][verletzt], 10), collapse = ", "))
  }
  verletzt_w <- beide_w & (we_w < rf_w - 1e-9)
  if (any(verletzt_w)) {
    hinweise <- c(hinweise, paste0("Bei ", sum(verletzt_w), " Kind(ern) ist der ",
                                   "WE-Wert kleiner als der R/F-Wert - bitte pruefen."))
  }

  if (!all(is.na(items))) {
    # Werte koennen nicht groesser als die Itemzahl sein
    zu_gross <- (!is.na(we_w) & !is.na(items) & we_w > items) |
      (!is.na(rf_w) & !is.na(items) & rf_w > items)
    if (any(zu_gross)) {
      hinweise <- c(hinweise, paste0("Bei ", sum(zu_gross), " Kind(ern) ist ein Wert ",
                                     "groesser als die Anzahl der Items - bitte pruefen."))
    }

    # alle Items richtig geschrieben erzwingt WE = alle Items
    voll <- !is.na(rf_w) & !is.na(we_w) & !is.na(items) &
      abs(rf_w - items) < 1e-9 & we_w < items - 1e-9
    if (any(voll)) {
      hinweise <- c(hinweise, paste0("Bei ", sum(voll), " Kind(ern) sind alle ",
                                     "Items richtig geschrieben (R/F), aber der ",
                                     "WE-Wert ist niedriger - nicht moeglich."))
    }

    # Prozentwert gegen Wert und Itemzahl
    for (paar in list(c("WE-Wert", "WE-%"), c("R/F-Wert", "R/F-%"))) {
      wert <- ausgabe[[paar[1]]]
      prozent <- ausgabe[[paar[2]]]
      vollstaendig <- !is.na(wert) & !is.na(prozent) & !is.na(items) & items > 0
      if (!any(vollstaendig)) next
      abweichung <- abs(round(wert[vollstaendig] / items[vollstaendig] * 100, 1) -
                          prozent[vollstaendig]) > toleranz
      if (any(abweichung)) {
        hinweise <- c(hinweise, paste0("'", paar[2], "' passt bei ", sum(abweichung),
                                       " Kind(ern) nicht zu Wert und Itemzahl."))
      }
    }
  }

  attr(ausgabe, "hinweise") <- hinweise
  ausgabe
}

# Kategorie und Empfehlung ergaenzen, wo sie fehlen - und pruefen, ob eine
# vorhandene Kategorie zu den Prozentwerten passt.
.ergaenze_kategorie <- function(ausgabe, zuordnung, hinweise) {
  we <- ausgabe[["WE-%"]]
  rf <- ausgabe[["R/F-%"]]
  we_w <- ausgabe[["WE-Wert"]]
  rf_w <- ausgabe[["R/F-Wert"]]
  # Sind beide Prozentspalten in der Datei vorhanden UND fehlt jede Angabe
  # (auch der Rohwert), hat das Kind nicht teilgenommen - dann gehoert dort die
  # Kategorie "0" hin. Fehlt eine der Spalten ganz oder steht nur ein Rohwert
  # ohne Prozentwert da, wird nichts erfunden.
  spalten_da <- !is.null(zuordnung[["WE-%"]]) && !is.null(zuordnung[["R/F-%"]])
  beide_werte <- !is.na(we) & !is.na(rf)
  keine_angaben <- is.na(we) & is.na(rf) & is.na(we_w) & is.na(rf_w)
  # Nur wenn die Datei ueberhaupt Angaben mitbringt (oder nachgerechnet wurde),
  # ist eine fehlende Kategorie erklaerungsbeduerftig - bei einer Datei mit nur
  # Namen waere der Hinweis nur Rauschen.
  if (!any(beide_werte) && !any(keine_angaben) && !spalten_da) {
    attr(ausgabe, "hinweise") <- hinweise
    return(ausgabe)
  }

  # Kategorie nur dort bilden, wo beide Werte da sind UND die Stufen bestimmbar
  # sind (getWElevel liefert nicht fuer jede Kombination eine Stufe)
  neu <- rep(NA_character_, nrow(ausgabe))
  rflvl <- getRFlevel(rf)
  welvl <- getWElevel(rf, we)
  bestimmbar <- beide_werte & !is.na(rflvl) & !is.na(welvl) & nzchar(welvl)
  neu[bestimmbar] <- paste0(rflvl[bestimmbar], welvl[bestimmbar])
  # Sind beide Prozentspalten vorhanden und fehlt jede Angabe, hat das Kind
  # nicht teilgenommen -> Kategorie "0"
  if (spalten_da) neu[keine_angaben] <- "0"

  leer_kat <- is.na(ausgabe[["Kat."]]) | !nzchar(trimws(ausgabe[["Kat."]]))
  fuellbar <- leer_kat & !is.na(neu)
  if (any(fuellbar)) {
    ausgabe[["Kat."]][fuellbar] <- neu[fuellbar]
    hinweise <- c(hinweise, paste0("Kategorie fuer ", sum(fuellbar),
                                   " Kind(er) aus den Prozentwerten berechnet."))
  }
  unbestimmbar <- leer_kat & is.na(neu)
  if (any(unbestimmbar)) {
    hinweise <- c(hinweise, paste0("Kategorie fuer ", sum(unbestimmbar),
                                   " Kind(er) nicht bestimmbar (Werte unvollstaendig ",
                                   "oder unplausibel) - bitte pruefen."))
  }

  # passt eine Kategorie der Datei nicht zu den Werten?
  werte_vorhanden <- beide_werte
  if (!is.null(zuordnung[["Kat."]]) && any(werte_vorhanden & !leer_kat)) {
    vergleichbar <- werte_vorhanden & !leer_kat & !is.na(neu)
    abweichung <- sum(ausgabe[["Kat."]][vergleichbar] != neu[vergleichbar])
    if (sum(vergleichbar) > 0 && abweichung / sum(vergleichbar) > 0.1) {
      hinweise <- c(hinweise, paste0("Die Kategorie-Spalte der Datei passt bei ",
                                     abweichung, " von ", sum(vergleichbar),
                                     " Kindern nicht zu den Prozentwerten - bitte ",
                                     "pruefen. Es bleibt die Kategorie der Datei."))
    }
  }

  leer_empfehlung <- is.na(ausgabe[["Empfehlung"]]) |
    !nzchar(trimws(ausgabe[["Empfehlung"]]))
  if (any(leer_empfehlung)) {
    empfehlung <- as.character(getRecommendation(ausgabe[["Kat."]]))
    fuellbar <- leer_empfehlung & !is.na(empfehlung)
    if (any(fuellbar)) {
      ausgabe[["Empfehlung"]][fuellbar] <- empfehlung[fuellbar]
      hinweise <- c(hinweise, paste0("Empfehlung fuer ", sum(fuellbar),
                                     " Kind(er) aus der Kategorie ergaenzt."))
    }
  }

  attr(ausgabe, "hinweise") <- hinweise
  ausgabe
}

# Hinweise einer geladenen Tabelle (leer, wenn keine)
lade_hinweise <- function(df) {
  hinweise <- attr(df, "hinweise")
  if (is.null(hinweise)) return(character(0))
  as.character(hinweise)
}

loadData <- function(inputFile) {
  pfad <- checkInputFile(inputFile)

  # Bewusst alles als Text einlesen: die frueheren positionsabhaengigen
  # Spaltentypen haben bei anderer Spaltenreihenfolge still die falschen Typen
  # ergeben, und ein fehlender Spaltenname liess read_tsv abbrechen. Jetzt
  # entscheidet pruefe_schuelerdaten, was fehlt - mit Hinweis statt Absturz.
  raw <- suppressWarnings(readr::read_tsv(
    pfad,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE,
    progress = FALSE))

  pruefe_schuelerdaten(raw, quelle = if (is.null(inputFile$name)) "" else inputFile$name)
}


convert_kat_meaning <- function(kat, table_path = "elternbrief/ergebnisse.xlsx") {
  # ohne Kategorie (z. B. Kind ohne Werte) gibt es keinen Text - nicht abbrechen
  if (length(kat) == 0 || is.na(kat) || !nzchar(trimws(as.character(kat)))) {
    return("")
  }

  df <- readxl::read_xlsx(table_path) %>%
    janitor::clean_names()
  
  idx  <- which(df$kategorie == kat)
  
  return(paste0(df$kat_ext[idx], ": ", df$bedeutung[idx]))
}

# Arbeitskopie der Vorlagen in einem temporaeren Verzeichnis.
# Beim Rendern schreibt rmarkdown die Datei <name>.knit.md neben das Rmd.
# Damit dabei nichts im Programmverzeichnis geschrieben wird (installierte App,
# normaler Benutzer), wird aus der Kopie heraus gerendert.

# Arbeitskopie loeschen. Windows gibt gesperrte Dateien manchmal erst kurz
# verzoegert frei (Virenscanner, Cloud-Sync, Indizierung); unlink() scheitert
# dann still und der Ordner bleibt liegen. Deshalb ein paar Versuche mit kurzer
# Pause. Ein Rest im Temp-Verzeichnis ist harmlos - nur das Anlegen einer neuen
# Kopie unter derselben Prozessnummer wuerde daran scheitern.
vorlagen_aufraeumen <- function(ziel, versuche = 3, pause = 0.2) {
  for (versuch in seq_len(versuche)) {
    unlink(ziel, recursive = TRUE)
    if (!dir.exists(ziel)) return(invisible(TRUE))
    if (versuch < versuche) Sys.sleep(pause)
  }
  invisible(FALSE)
}

#### Anpassbare Vorlagen des Benutzers ####
#
# Die mitgelieferten Vorlagen liegen im Programmordner und werden bei einem
# Update ueberschrieben. Anpassungen (Briefkopf, Logo, Schrift, Ergebnistabelle,
# Kategorie-Texte) liegen deshalb in einer persoenlichen Kopie bei den
# Dokumenten des Benutzers und werden beim Rendern ueber die mitgelieferten
# Dateien kopiert.

# Dateien, die angepasst werden duerfen. Der Briefkopf (Logo) steckt im Kopf der
# template.docx - die Datei logo.png wird von keiner Vorlage verwendet.
.vorlagen_dateien <- c("template.docx", "table.png", "ergebnisse.xlsx")

# Pfad in der Schreibweise des Systems: .pfad_nativ() ist oben bei den
# Pfadfunktionen definiert (siehe createFilePath).
#
# Persoenlicher Vorlagenordner (wird bei Bedarf angelegt).
vorlagen_ordner <- function(anlegen = FALSE) {
  ordner <- .pfad_nativ(file.path(benutzer_ausgabeordner(), "vorlagen"))
  if (anlegen && !verzeichnis_sicherstellen(ordner)) return(NULL)
  ordner
}

# Ist die Tabelle mit den Kategorie-Texten brauchbar? Eine unlesbare oder falsch
# aufgebaute Anpassung darf die Briefe nicht unbrauchbar machen.
.kategorie_tabelle_ok <- function(pfad) {
  tryCatch({
    df <- janitor::clean_names(readxl::read_xlsx(pfad))
    all(c("kategorie", "kat_ext", "bedeutung") %in% colnames(df)) && nrow(df) > 0
  }, error = function(e) FALSE)
}

# Eine Vorlagendatei in den persoenlichen Ordner legen, falls sie dort fehlt.
# Eine vorhandene Anpassung wird nie ueberschrieben.
vorlage_bereitstellen <- function(datei = "template.docx") {
  ziel_ordner <- vorlagen_ordner(anlegen = TRUE)
  if (is.null(ziel_ordner)) {
    stop("Der Vorlagenordner konnte nicht angelegt werden. Bitte Schreibrechte ",
         "im Ordner 'Dokumente' pruefen.", call. = FALSE)
  }

  ziel <- .pfad_nativ(file.path(ziel_ordner, datei))
  if (!file.exists(ziel)) {
    quelle <- file.path(getwd(), "elternbrief", datei)
    if (!file.exists(quelle)) {
      stop("Die mitgelieferte Vorlage '", datei, "' wurde nicht gefunden.",
           call. = FALSE)
    }
    if (!file.copy(quelle, ziel, overwrite = FALSE)) {
      stop("Die Vorlage konnte nicht nach '", ziel, "' kopiert werden.",
           call. = FALSE)
    }
  }

  ziel
}

# Alle anpassbaren Dateien bereitstellen und den Ordner zurueckgeben
vorlagen_bereitstellen <- function() {
  ziel_ordner <- vorlagen_ordner(anlegen = TRUE)
  if (is.null(ziel_ordner)) {
    stop("Der Vorlagenordner konnte nicht angelegt werden. Bitte Schreibrechte ",
         "im Ordner 'Dokumente' pruefen.", call. = FALSE)
  }
  for (datei in .vorlagen_dateien) vorlage_bereitstellen(datei)
  ziel_ordner
}

# Angepasste Vorlagen ueber die mitgelieferten kopieren. Nur Layout-Dateien:
# die Rmd-Dateien mit der Brief-Logik bleiben unberuehrt, und uebernommen wird
# nur, was in dieser Vorlagenmappe auch mitgeliefert wird.
vorlagen_ueberlagern <- function(ziel) {
  ordner <- vorlagen_ordner()
  if (is.null(ordner) || !dir.exists(ordner)) return(invisible(character(0)))

  uebernommen <- character(0)
  for (datei in .vorlagen_dateien) {
    quelle <- file.path(ordner, datei)
    if (!file.exists(quelle) || !file.exists(file.path(ziel, datei))) next

    if (identical(datei, "ergebnisse.xlsx") && !.kategorie_tabelle_ok(quelle)) {
      message("Die angepasste 'ergebnisse.xlsx' ist nicht lesbar - es wird die ",
              "mitgelieferte Tabelle verwendet.")
      next
    }

    if (file.copy(quelle, file.path(ziel, datei), overwrite = TRUE)) {
      uebernommen <- c(uebernommen, datei)
    }
  }

  if (length(uebernommen) > 0) {
    message("Angepasste Vorlage verwendet: ", paste(uebernommen, collapse = ", "))
  }
  invisible(uebernommen)
}

vorlagen_vorbereiten <- function(quelle, praefix) {
  if (!dir.exists(quelle)) {
    stop("Vorlagen nicht gefunden: '", quelle, "'", call. = FALSE)
  }
  ziel <- file.path(tempdir(), paste0(praefix, "_", Sys.getpid()))
  if (dir.exists(ziel)) vorlagen_aufraeumen(ziel)
  if (!dir.create(ziel, recursive = TRUE, showWarnings = FALSE)) {
    # Liegt unter dem ueblichen Namen noch ein Rest (Windows gibt gesperrte
    # Dateien manchmal erst verzoegert frei, siehe vorlagen_aufraeumen), dann auf
    # einen eindeutigen Namen ausweichen. Ein Rest im Temp-Verzeichnis darf das
    # Erstellen der Briefe nicht verhindern.
    ziel <- file.path(tempdir(), paste0(praefix, "_", Sys.getpid(), "_",
                                        basename(tempfile(""))))
    if (!dir.create(ziel, recursive = TRUE, showWarnings = FALSE)) {
      stop("Temporaeres Arbeitsverzeichnis konnte nicht angelegt werden. ",
           "Bitte pruefen, ob im Ordner '", tempdir(), "' geschrieben werden darf.",
           call. = FALSE)
    }
  }
  dateien <- list.files(quelle, full.names = TRUE)
  kopiert <- file.copy(dateien, ziel, recursive = TRUE)
  if (!all(kopiert)) {
    stop("Vorlagen konnten nicht kopiert werden.", call. = FALSE)
  }
  # persoenliche Vorlagen ueberlagern (Briefkopf, Schrift, Ergebnistabelle ...)
  vorlagen_ueberlagern(ziel)
  return(ziel)
}

elternbrief_vorbereiten <- function(quelle = file.path(getwd(), "elternbrief")) {
  vorlagen_vorbereiten(quelle, "elternbrief")
}

# Zwischendateien von knitr aus der Arbeitskopie entfernen
knit_reste_entfernen <- function(vorlage) {
  rest <- file.path(vorlage, "elternbrief.knit.md")
  if (fs::file_exists(rest)) fs::file_delete(rest)
  invisible(TRUE)
}

compose_letter <- function(name, klasse, kat, lehrername, signatur = "",
                           qrLink = NULL,
                           output = NULL, vorlage = elternbrief_vorbereiten()) {
  # all variables are processed in elternbrief.Rmd!
  # (name, klasse, kat, lehrername, signatur, qrLink liegen in diesem Frame;
  #  rmarkdown::render() wertet das Rmd darin aus)
  kat <- convert_kat_meaning(kat, table_path = file.path(vorlage, "ergebnisse.xlsx"))

  rmarkdown::render(file.path(vorlage, "elternbrief.Rmd"), output_file = output)
}

# Weitere Briefe/Texte an ein Dokument anhaengen. umbruch = FALSE laesst den
# Seitenumbruch weg (fuer den Infobrief, damit Seite 1 nicht halb leer bleibt).
combine_letters <- function(rdocx, temp_path, out_path = NULL, umbruch = TRUE) {
  if (isTRUE(umbruch)) rdocx <- officer::body_add_break(rdocx)
  rdocx <- officer::body_add_docx(rdocx, temp_path)
  return(rdocx)
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

generate_qrcode <- function(qrLink, zielordner = tempdir()) {
  # ohne Link: zwei NA-Felder statt eines nackten NA - so liefert auch dieser
  # Fall ueber qr$img einen Wert (und nicht nur qr[[1]])
  if (is.null(qrLink) || !isTruthy(qrLink)) {
    return(list(img = NA_character_, txt = NA_character_))
  }

  # QR-Code immer aus dem Originallink erzeugen - das braucht kein Internet
  qr <- qr_code(qrLink, ecl = "M")
  # Das Bild muss NEBEN dem Dokument liegen und als reiner Dateiname
  # zurueckgegeben werden: knitr::include_graphics() rechnet absolute Pfade
  # relativ zum Ausgabeordner um (xfun::relative_path). Ein Pfad aus tempdir()
  # zeigt danach ins Leere, und officer lehnt das Bild ab ("src must be a
  # string starting with 'rId' or an existing image filename").
  name <- paste0("qrcode_", Sys.getpid(), "_", basename(tempfile("")), ".png")
  qr_tmp <- file.path(zielordner, name)
  png(filename = qr_tmp)
  plot(qr)
  dev.off()

  # Verweis auf die Uebungssammlung: mit Kurzlink, wenn das moeglich ist
  kurz <- shorten_url(qrLink)
  ziel <- if (is.null(kurz)) qrLink else kurz
  linkText <- paste("Sie möchten Ihr Kind unterstützen?",
                    "Dann schauen Sie hier in unsere Sammlung:", ziel,
                    sep = "\n")

  return(list(img = name,
              txt = linkText))
}

create_letters <- function(df, lehrername, signatur = "", qrLink = NULL,
                           fortschritt = NULL) {
  # fortschritt(anteil, text) meldet den Stand an die Oberflaeche (0 bis 1).
  # Ohne Funktion passiert nichts - die Konsole bekommt weiterhin message().
  melde <- function(anteil, text) {
    if (is.function(fortschritt)) {
      fortschritt(max(0, min(1, anteil)), text)
    }
    invisible(NULL)
  }

  # klare Meldung statt kryptischem Fehler, wenn Spalten fehlen
  fehlend <- setdiff(c("Name", "Klasse", "Kat."), colnames(df))
  if (length(fehlend) > 0) {
    stop("Fuer die Briefe fehlen Spalten: ", paste(fehlend, collapse = ", "),
         ". Bitte die Daten neu laden.", call. = FALSE)
  }

  df <- janitor::clean_names(df) %>%
    mutate(kat = str_remove(kat, pattern = "\\*"))

  melde(0, "Vorlagen werden vorbereitet ...")

  # Ausgabeordner frueh pruefen und anlegen: fehlende Schreibrechte sollen
  # sofort gemeldet werden und nicht erst nach dem Rendern aller Briefe
  ziel_ordner <- createFilePath(NULL, "")

  # Zielname ebenfalls vorab bestimmen: eine in Word geoeffnete Zieldatei soll
  # sofort gemeldet werden - sonst wuerde erst minutenlang gerendert und das
  # Kopieren am Ende scheitern
  fn <- file.path(ziel_ordner, paste0("Elternbriefe_", Sys.Date()))
  if("klasse" %in% colnames(df)) {
    kl <- paste0(unique(df$klasse), collapse = "_")
    fn <- paste0(fn, "_", kl)
  }
  zieldatei <- .pfad_nativ(paste0(fn, ".docx"))
  .pruefe_datei_frei(zieldatei)

  # aus einer Arbeitskopie im Temp-Verzeichnis rendern, damit im
  # Programmverzeichnis nichts geschrieben wird (installierte App)
  vorlage <- elternbrief_vorbereiten()
  on.exit(vorlagen_aufraeumen(vorlage), add = TRUE)
  knit_reste_entfernen(vorlage)

  # Ein Fehler bei einem Kind darf die restlichen Briefe nicht verhindern:
  # Fehler werden gesammelt und am Ende gemeldet.
  brief_pfade <- character(0)
  fehler <- character(0)
  anzahl <- dim(df)[1]

  for(i in seq_len(anzahl)) {
    tmp <- tempfile(fileext = ".docx")
    message("Composing letter for ",  df$name[i], " ", i, "/", anzahl)
    melde((i - 1) / anzahl,
          paste0("Brief ", i, " von ", anzahl, ": ", df$name[i]))
    ok <- tryCatch({
      compose_letter(name = df$name[i],
                     klasse = parse_number(df$klasse[i]),
                     kat = df$kat[i],
                     lehrername = lehrername,
                     signatur = signatur,
                     output = tmp,
                     qrLink = qrLink,
                     vorlage = vorlage)
      TRUE
    }, error = function(e) {
      fehler <<- c(fehler, paste0(df$name[i], ": ", conditionMessage(e)))
      message("Elternbrief fuer ", df$name[i], " konnte nicht erstellt werden: ",
              conditionMessage(e))
      FALSE
    })

    if (ok) brief_pfade <- c(brief_pfade, tmp)
    knit_reste_entfernen(vorlage)
  }

  if(length(brief_pfade) == 0) {
    stop("Es konnte kein Elternbrief erstellt werden. ",
         paste(utils::head(fehler, 3), collapse = " | "), call. = FALSE)
  }

  # Briefe zu einer Datei zusammenfuegen (erster Brief direkt, weitere als
  # eingebettete Dokumente)
  melde(0.97, paste0("Briefe werden zusammengefuegt (", length(brief_pfade), ") ..."))
  rdocx <- officer::read_docx(brief_pfade[1])
  for(pfad in brief_pfade[-1]) {
    rdocx <- combine_letters(rdocx, temp_path = pfad)
  }
  
  # Zielname wurde oben schon bestimmt (Pruefung auf gesperrte Datei)
  message("Elternbriefe gespeichert unter: ", zieldatei)
  print(rdocx, target = zieldatei)
  melde(1, paste0(length(brief_pfade), " von ", anzahl, " Briefen erstellt"))

  return(list(datei = zieldatei,
              erstellt = length(brief_pfade),
              fehler = fehler))
}


