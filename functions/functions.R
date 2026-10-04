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
  # Die Spalten stehen im DT-Objekt unter x$data: das Objekt selbst hat keine
  # Spaltennamen (colnames(dt) ist leer). Eine Pruefung auf colnames(dt) hat die
  # Faerbung deshalb frueher immer uebersprungen. Ein data.frame kann DT nicht
  # faerben und wird unveraendert zurueckgegeben.
  if (is.data.frame(dt)) return(dt)
  daten <- dt$x$data
  if (is.null(daten) || !("Kat." %in% colnames(daten))) return(dt)

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
       # Innenansicht der eigenen Schule (Vergleichswerte) - Vorgabe: aus. Die
       # Werte stehen in Vergleichswerte_C-Test.xlsx im Vorlagenordner.
       vergleich_anzeigen = "nein",
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
  # Reihenfolge fuer Word und Excel: erst Klasse, dann Name alphabetisch
  # (die tsv behaelt die Eingabereihenfolge - sie ist das Datenformat).
  # order() folgt der Locale; app.R setzt "German", damit Umlaute richtig
  # einsortiert werden.
  if ("Name" %in% colnames(bericht)) {
    schluessel <- list(as.character(bericht$Name))
    if ("Klasse" %in% colnames(bericht)) {
      schluessel <- c(list(as.character(bericht$Klasse)), schluessel)
    }
    bericht <- bericht[do.call(order, schluessel), , drop = FALSE]
    rownames(bericht) <- NULL
  }
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


convert_kat_meaning <- function(kat, table_path = vorlagen_quelle("ergebnisse.xlsx")) {
  # ohne Kategorie (z. B. Kind ohne Werte) gibt es keinen Text - nicht abbrechen
  if (length(kat) == 0 || is.na(kat) || !nzchar(trimws(as.character(kat)))) {
    return("")
  }

  df <- readxl::read_xlsx(table_path) %>%
    janitor::clean_names()
  
  idx  <- which(df$kategorie == kat)
  
  return(paste0(df$kat_ext[idx], ": ", df$bedeutung[idx]))
}

#### Ergebnistabelle des Elternbriefes ####
#
# Die Tabelle steht im Blatt "Tabelle2" der Kategorie-Datei (ergebnisse.xlsx):
# Kopfzeile, Zeilen und verbundene Zellen kommen von dort. Fehlt das Blatt -
# das ist bei allen persoenlichen Kopien der Fall, die vorher angelegt wurden -
# wird dieselbe Darstellung aus der Zuordnungstabelle abgeleitet: nur kat_ext
# als "Kategorie", ohne die Zeile "0", ohne die Sternchen-Dubletten und mit
# verbundenen Zellen fuer gleiche Empfehlungstexte.

# Datei im xlsx-Paket als Text lesen
.xlsx_teil <- function(pfad, teil) {
  con <- unz(pfad, teil)
  on.exit(close(con), add = TRUE)
  paste(readLines(con, warn = FALSE), collapse = "")
}

# Arbeitsblatt-Datei zu einem Blattindex (Reihenfolge wie readxl::excel_sheets)
.blatt_datei <- function(pfad, index) {
  wb <- .xlsx_teil(pfad, "xl/workbook.xml")
  rels <- .xlsx_teil(pfad, "xl/_rels/workbook.xml.rels")
  blaetter <- regmatches(wb, gregexpr("<sheet [^>]*>", wb))[[1]]
  if (index < 1 || index > length(blaetter)) return(NA_character_)
  rid <- sub('^.*r:id="([^"]+)".*$', "\\1", blaetter[index])
  rel <- regmatches(rels, regexpr(paste0('<Relationship[^>]*Id="', rid, '"[^>]*>'), rels))
  if (length(rel) == 0) return(NA_character_)
  paste0("xl/", sub('^.*Target="([^"]+)".*$', "\\1", rel))
}

# Senkrechte verbundene Bereiche eines Blatts, umgerechnet auf Spaltenindex und
# Datenzeilen der gelesenen Tabelle: data.frame(von, bis, spalte). Die erste
# Zeile mit Inhalt gilt als Kopfzeile; die Datenzeilen zaehlen ab der Zeile
# darunter (so liest auch readxl, das fuehrende leere Zeilen weglaesst).
.blatt_verbunde <- function(pfad, index) {
  leer <- data.frame(von = integer(0), bis = integer(0), spalte = integer(0))
  datei <- .blatt_datei(pfad, index)
  if (is.na(datei)) return(leer)
  xml <- .xlsx_teil(pfad, datei)

  zeilen <- regmatches(xml, gregexpr("<row [^>]*>.*?</row>", xml))[[1]]
  if (length(zeilen) == 0) return(leer)
  hat_wert <- grepl("<v>|<is>", zeilen)
  if (!any(hat_wert)) return(leer)
  kopf_pos <- which(hat_wert)[1]
  kopf_zeile <- as.integer(sub('^.*<row r="([0-9]+)".*$', "\\1", zeilen[kopf_pos]))

  # Spalten der Kopfzeile mit Inhalt - leere Zellen (z. B. eine Spalte vor der
  # Tabelle) gehoeren nicht dazu und fallen beim Lesen ebenfalls weg. Zerlegt
  # wird an "</c>": ein Stueck enthaelt dann die Zelle samt ihrem Wert.
  stuecke <- strsplit(zeilen[kopf_pos], "</c>", fixed = TRUE)[[1]]
  stuecke <- stuecke[grepl("<v>|<is>", stuecke)]
  spalten <- sub('^.*<c [^>]*r="([A-Z]+)[0-9]+".*$', "\\1", stuecke)

  verbuende <- regmatches(xml, gregexpr('<mergeCell ref="[^"]+"', xml))[[1]]
  if (length(verbuende) == 0) return(leer)
  verbuende <- sub('"$', "", sub('^.*ref="', "", verbuende))

  von_ref <- sub(":.*$", "", verbuende)
  bis_ref <- ifelse(grepl(":", verbuende), sub("^.*:", "", verbuende), von_ref)
  spalte_von <- sub("[0-9]+$", "", von_ref)
  spalte_bis <- sub("[0-9]+$", "", bis_ref)
  senkrecht <- spalte_von == spalte_bis
  if (!any(senkrecht)) return(leer)

  ergebnis <- data.frame(
    von = as.integer(sub("^[A-Z]+", "", von_ref[senkrecht])) - kopf_zeile,
    bis = as.integer(sub("^[A-Z]+", "", bis_ref[senkrecht])) - kopf_zeile,
    spalte = match(spalte_von[senkrecht], spalten))
  ergebnis[!is.na(ergebnis$spalte) & ergebnis$von >= 1 &
             ergebnis$bis > ergebnis$von, , drop = FALSE]
}

# Roh gelesenes Blatt: erste Zeile ist die Kopfzeile, fuehrende leere Zeilen und
# Spalten fallen weg.
.tabelle_kopf_und_daten <- function(roh) {
  roh <- as.data.frame(roh, stringsAsFactors = FALSE)
  while (nrow(roh) > 0 && all(is.na(roh[1, ]))) roh <- roh[-1, , drop = FALSE]
  while (ncol(roh) > 0 && all(is.na(roh[, 1]))) roh <- roh[, -1, drop = FALSE]
  while (ncol(roh) > 0 && all(is.na(roh[, ncol(roh)]))) roh <- roh[, -ncol(roh), drop = FALSE]
  if (nrow(roh) < 2) stop("Die Kategorie-Tabelle ist leer.", call. = FALSE)

  namen <- trimws(as.character(unlist(roh[1, ])))
  leer_name <- is.na(namen) | !nzchar(namen)
  namen[leer_name] <- paste0("Spalte ", which(leer_name))
  daten <- roh[-1, , drop = FALSE]
  names(daten) <- namen
  rownames(daten) <- NULL
  daten
}

# Anzeige aus der Zuordnungstabelle ableiten (aeltere Dateien ohne "Tabelle2")
.ergebnis_anzeige_aus_zuordnung <- function(daten) {
  finde <- function(muster) {
    i <- grep(muster, names(daten), ignore.case = TRUE)[1]
    if (length(i) == 0 || is.na(i)) NULL else i
  }
  i_ext <- finde("^kat_?ext$")
  i_bed <- finde("^bedeutung$")
  i_emp <- finde("^handlungsempfehlung$")
  if (is.null(i_ext) || is.null(i_bed) || is.null(i_emp)) return(daten)

  anzeige <- data.frame(Kategorie = as.character(daten[[i_ext]]),
                        Bedeutung = as.character(daten[[i_bed]]),
                        Handlungsempfehlung = as.character(daten[[i_emp]]),
                        stringsAsFactors = FALSE)
  # die Zeile "0" (nicht teilgenommen) gehoert nicht in die Elterntabelle
  weg <- !is.na(anzeige$Kategorie) & anzeige$Kategorie %in% c("0", "")
  anzeige <- anzeige[!weg, , drop = FALSE]
  # Zeilen, die sich nur in der internen Kategorie unterscheiden, einmal zeigen
  anzeige <- anzeige[!duplicated(anzeige), , drop = FALSE]
  rownames(anzeige) <- NULL
  anzeige
}

# Gleiche Empfehlungstexte in aufeinanderfolgenden Zeilen verbinden
.ergebnis_verbunde_aus_text <- function(anzeige) {
  leer <- data.frame(von = integer(0), bis = integer(0), spalte = integer(0))
  if (nrow(anzeige) < 2) return(leer)
  spalte <- ncol(anzeige)
  text <- as.character(anzeige[[spalte]])
  text[is.na(text)] <- paste0("<leer ", which(is.na(text)), ">")
  gruppen <- rle(text)
  bis <- cumsum(gruppen$lengths)
  von <- c(1, utils::head(bis, -1) + 1)
  mehrfach <- gruppen$lengths > 1
  if (!any(mehrfach)) return(leer)
  data.frame(von = von[mehrfach], bis = bis[mehrfach], spalte = spalte)
}

# Spaltenbreiten (in cm): die Kategorie-Spalte schmal, der Rest teilt sich die
# Textbreite.
.ergebnis_breiten <- function(spalten, gesamt = 16) {
  if (spalten <= 1) return(gesamt)
  c(2.0, rep((gesamt - 2.0) / (spalten - 1), spalten - 1))
}

# Ergebnistabelle des Elternbriefes als echte Word-Tabelle (frueher ein Bild).
# Inhalt, Ueberschriften und verbundene Zellen kommen aus ergebnisse.xlsx.
ergebnis_tabelle <- function(pfad = "ergebnisse.xlsx", schrift = 7, breite = 16) {
  blaetter <- tryCatch(readxl::excel_sheets(pfad), error = function(e) character(0))
  if (length(blaetter) == 0) {
    stop("Die Kategorie-Tabelle '", pfad, "' ist nicht lesbar.", call. = FALSE)
  }

  anzeige_blatt <- if ("Tabelle2" %in% blaetter) "Tabelle2" else blaetter[1]
  roh <- readxl::read_xlsx(pfad, sheet = anzeige_blatt, col_names = FALSE,
                           .name_repair = "minimal")
  anzeige <- .tabelle_kopf_und_daten(roh)
  verbunde <- .blatt_verbunde(pfad, match(anzeige_blatt, blaetter))

  if (!identical(anzeige_blatt, "Tabelle2")) {
    anzeige <- .ergebnis_anzeige_aus_zuordnung(anzeige)
    verbunde <- .ergebnis_verbunde_aus_text(anzeige)
  }
  if (nrow(anzeige) == 0) {
    stop("Die Kategorie-Tabelle '", pfad, "' enthaelt keine Zeilen.", call. = FALSE)
  }
  verbunde <- verbunde[verbunde$bis <= nrow(anzeige), , drop = FALSE]

  ft <- flextable::flextable(anzeige)
  ft <- flextable::theme_booktabs(ft)
  ft <- flextable::fontsize(ft, size = schrift, part = "all")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::set_table_properties(ft, layout = "fixed", width = 0)
  ft <- flextable::width(ft, width = .ergebnis_breiten(ncol(anzeige), breite) / 2.54)

  for (i in seq_len(nrow(verbunde))) {
    ft <- flextable::merge_at(ft, i = verbunde$von[i]:verbunde$bis[i],
                              j = verbunde$spalte[i], part = "body")
  }
  # Zellentrenner: ohne Linien laesst sich nicht erkennen, welcher Text zu
  # welcher Kategorie gehoert (waagerecht zwischen den Zeilen, senkrecht
  # zwischen den Spalten, plus Rahmen)
  rahmen <- flextable::fp_border_default(width = 0.5)
  ft <- flextable::border_outer(ft, border = rahmen, part = "all")
  ft <- flextable::border_inner_h(ft, border = rahmen, part = "all")
  ft <- flextable::border_inner_v(ft, border = rahmen, part = "all")
  # verbundene Zelle senkrecht mittig - wie "Verbinden und zentrieren" in Excel
  flextable::valign(ft, j = ncol(anzeige), valign = "center", part = "body")
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
#
# Die Ergebnistabelle des Elternbriefes ist eine echte Word-Tabelle und wird aus
# ergebnisse.xlsx erzeugt; das frueher anpassbare Bild table.png kommt im Brief
# nicht mehr vor und ist deshalb nicht mehr Teil dieser Liste.
.vorlagen_dateien <- c("template.docx", "ergebnisse.xlsx")

# Zentraler Ordner mit den mitgelieferten, anpassbaren Dateien (im App-Ordner).
# Beide Briefe holen ihre Word-Vorlage hier - es gibt nur noch eine Datei.
# basis = Ordner, in dem "vorlagen" liegt; voreingestellt das Arbeitsverzeichnis
# der App, beim Vorbereiten der Arbeitskopie der Ordner neben dem Briefordner.
vorlagen_quelle <- function(datei = NULL, basis = getwd()) {
  ordner <- file.path(basis, "vorlagen")
  if (is.null(datei)) ordner else file.path(ordner, datei)
}

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
    quelle <- vorlagen_quelle(datei)
    if (!file.exists(quelle)) {
      stop("Die mitgelieferte Vorlage '", datei, "' wurde nicht gefunden (erwartet in '",
           vorlagen_quelle(), "').", call. = FALSE)
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

# Welche Vorlage gilt gerade? Nur lesen - es wird nichts angelegt. Entweder die
# persoenliche Kopie (Pfad und Aenderungsdatum) oder die mitgelieferte Vorlage.
vorlage_status <- function(datei = "template.docx") {
  pfad <- .pfad_nativ(file.path(vorlagen_ordner(), datei))
  if (file.exists(pfad)) {
    return(list(eigen = TRUE, pfad = pfad,
                zeit = format(file.info(pfad)$mtime, "%d.%m.%Y %H:%M")))
  }
  list(eigen = FALSE, pfad = .pfad_nativ(vorlagen_quelle(datei)), zeit = NA_character_)
}

# Zeile fuer die Oberflaeche: nennt Pfad und Datum der aktiven Vorlage.
vorlage_status_text <- function(status = vorlage_status()) {
  if (isTRUE(status$eigen)) {
    paste0("Persönliche Vorlage: ", status$pfad, " (geändert ", status$zeit, ")")
  } else {
    paste0("Es gilt die mitgelieferte Vorlage: ", status$pfad,
           ". Eine persönliche Kopie entsteht beim ersten Klick auf ",
           "'Briefvorlage öffnen'.")
  }
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

# zentral: Dateien aus dem Ordner "vorlagen", die zusaetzlich in die Arbeitskopie
# gehoeren (Word-Vorlage fuer beide Briefe, Kategorie-Texte fuer den Elternbrief).
vorlagen_vorbereiten <- function(quelle, praefix, zentral = "template.docx") {
  if (!dir.exists(quelle)) {
    stop("Vorlagen nicht gefunden: '", quelle, "'", call. = FALSE)
  }
  # Der zentrale Vorlagenordner liegt neben dem Briefordner - so haengt die
  # Arbeitskopie nicht am Arbeitsverzeichnis des Prozesses.
  basis <- dirname(quelle)
  fehlend <- zentral[!file.exists(vorlagen_quelle(zentral, basis = basis))]
  if (length(fehlend) > 0) {
    stop("Die mitgelieferten Vorlagen fehlen: ", paste(fehlend, collapse = ", "),
         " (erwartet in '", vorlagen_quelle(basis = basis), "').", call. = FALSE)
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
  # zentral mitgelieferte Dateien dazu (Word-Vorlage, Kategorie-Texte)
  kopiert <- c(kopiert, file.copy(vorlagen_quelle(zentral, basis = basis), ziel,
                                  overwrite = TRUE))
  if (!all(kopiert)) {
    stop("Vorlagen konnten nicht kopiert werden.", call. = FALSE)
  }
  # persoenliche Vorlagen ueberlagern (Briefkopf, Schrift, Ergebnistabelle ...)
  vorlagen_ueberlagern(ziel)
  return(ziel)
}

elternbrief_vorbereiten <- function(quelle = file.path(getwd(), "elternbrief")) {
  vorlagen_vorbereiten(quelle, "elternbrief",
                       zentral = c("template.docx", "ergebnisse.xlsx"))
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
  # Harter Zeilenumbruch nach dem ersten Satz: die zwei Leerzeichen vor dem
  # Zeilenwechsel sind in Markdown ein harter Umbruch. Ein einzelnes "\n" waere
  # nur ein weicher Umbruch - pandoc zieht die beiden Saetze dann zu einer Zeile
  # zusammen, die unschon umbricht.
  linkText <- paste("Sie möchten Ihr Kind unterstützen?  ",
                    "Dann schauen Sie hier in unsere Sammlung:", ziel,
                    sep = "\n")

  return(list(img = name,
              txt = linkText))
}

#### Auswahl der Klassen fuer die Briefe ####
#
# Der Elternbrief kann fuer eine einzelne Klasse, fuer einen ganzen Jahrgang oder
# fuer alle geladenen Klassen erzeugt werden. Vorausgewaehlt ist die hoechste
# Klasse; die Logik steht hier, damit sie ohne Oberflaeche pruefbar ist.

# Jahrgang aus dem Klassennamen: "5c" -> 5, "10a" -> 10, sonst NA.
brief_jahrgang <- function(klassen) {
  suppressWarnings(as.numeric(sub("^[^0-9]*([0-9]+).*$", "\\1", as.character(klassen))))
}

# Geladene Klassen: eindeutig, sortiert, ohne leere Werte.
brief_klassen <- function(df) {
  if (is.null(df) || !("Klasse" %in% colnames(df)) || nrow(df) == 0) return(character(0))
  klassen <- trimws(as.character(df$Klasse))
  sort(unique(klassen[!is.na(klassen) & nzchar(klassen)]))
}

# Auswahlliste fuer die Oberflaeche: alle Klassen, je Jahrgang, jede Klasse.
# Rueckgabe: benannter Vektor (Anzeigetext = Name, Schluessel = Wert).
brief_auswahl_liste <- function(klassen) {
  klassen <- sort(unique(as.character(klassen)))
  klassen <- klassen[!is.na(klassen) & nzchar(trimws(klassen))]
  if (length(klassen) == 0) return(stats::setNames(character(0), character(0)))

  jahre <- brief_jahrgang(klassen)
  auswahl <- c("Alle geladenen Klassen" = "alle")
  for (jahr in sort(unique(jahre[!is.na(jahre)]))) {
    drin <- klassen[!is.na(jahre) & jahre == jahr]
    auswahl[[paste0("Jahrgang ", jahr, " (", paste(drin, collapse = ", "), ")")]] <-
      paste0("jahrgang:", jahr)
  }
  for (klasse in klassen) auswahl[[klasse]] <- klasse
  auswahl
}

# Vorauswahl: hoechster Jahrgang, bei Gleichstand alphanumerisch der erste
# (bei 5a/5b/6a also 6a, bei 5a/5b/5c also 5a).
brief_vorauswahl <- function(klassen) {
  klassen <- sort(unique(as.character(klassen)))
  klassen <- klassen[!is.na(klassen) & nzchar(trimws(klassen))]
  if (length(klassen) == 0) return(NULL)

  jahre <- brief_jahrgang(klassen)
  if (all(is.na(jahre))) return(klassen[1])
  klassen[order(-jahre, klassen, na.last = TRUE)][1]
}

# Daten auf die Auswahl einschraenken. Ohne (oder mit unbekannter) Auswahl
# bleiben alle Zeilen - so verhaelt sich der Knopf wie vor der Auswahl.
brief_daten <- function(df, auswahl = "alle") {
  if (is.null(df) || nrow(df) == 0) return(df)
  if (is.null(auswahl) || length(auswahl) != 1 || is.na(auswahl)) return(df)
  if (identical(as.character(auswahl), "alle")) return(df)

  klassen <- as.character(df$Klasse)
  if (startsWith(as.character(auswahl), "jahrgang:")) {
    jahr <- suppressWarnings(as.numeric(sub("^jahrgang:", "", as.character(auswahl))))
    idx <- which(!is.na(klassen) & brief_jahrgang(klassen) == jahr)
    return(df[idx, , drop = FALSE])
  }
  idx <- which(!is.na(klassen) & klassen == as.character(auswahl))
  if (length(idx) == 0) {
    # Die Auswahl ist dann ein Buchstabe (Kohorte des Entwicklungsbriefs):
    # alle Klassen dieses Buchstabens - z. B. direkt nach dem Umschalten der
    # Briefart, bevor die Auswahlliste neu aufgebaut ist.
    idx <- which(!is.na(klassen) & .brief_buchstabe(klassen) == .brief_buchstabe(auswahl))
  }
  df[idx, , drop = FALSE]
}

# Buchstabe einer Klasse: "6c" -> "c"
.brief_buchstabe <- function(klassen) {
  buchstabe <- gsub("[^A-Za-z]", "", as.character(klassen))
  buchstabe[!is.na(buchstabe) & nzchar(buchstabe)]
}

# Klassenbuchstaben der Auswahl - fuer den Entwicklungsbrief: dort gehoeren zu
# einer Klasse immer BEIDE Jahrgaenge, deshalb wird ueber den Buchstaben
# gefiltert ("6c" -> c, "Jahrgang 5" -> die Buchstaben der 5er Klassen).
# Ein leerer Vektor bedeutet: keine Einschraenkung (alle).
brief_buchstaben <- function(klassen, auswahl = "alle") {
  if (is.null(auswahl) || length(auswahl) != 1 || is.na(auswahl)) return(character(0))
  if (identical(as.character(auswahl), "alle")) return(character(0))

  klassen <- as.character(klassen)
  if (startsWith(as.character(auswahl), "jahrgang:")) {
    jahr <- suppressWarnings(as.numeric(sub("^jahrgang:", "", as.character(auswahl))))
    gewaehlt <- klassen[!is.na(brief_jahrgang(klassen)) & brief_jahrgang(klassen) == jahr]
  } else {
    gewaehlt <- as.character(auswahl)
  }
  sort(unique(.brief_buchstabe(gewaehlt)))
}

# Daten auf diese Buchstaben einschraenken: beide Jahrgaenge bleiben erhalten.
brief_daten_buchstaben <- function(df, buchstaben) {
  if (is.null(df) || nrow(df) == 0 || length(buchstaben) == 0) return(df)
  klassen <- as.character(df$Klasse)
  idx <- which(!is.na(klassen) & .brief_buchstabe(klassen) %in% buchstaben)
  df[idx, , drop = FALSE]
}

# Auswahlliste fuer den Entwicklungsbrief: eine Kohorte je Klassenbuchstabe,
# beschriftet mit dem Paar aus der Stufenauswahl ("a: 5a -> 6a"). Schluessel ist
# der Buchstabe; ein Jahr allein laesst sich nicht vergleichen.
brief_kohorten_liste <- function(klassen, stufe_alt = NULL, stufe_neu = NULL) {
  klassen <- sort(unique(as.character(klassen)))
  klassen <- klassen[!is.na(klassen) & nzchar(trimws(klassen))]
  if (length(klassen) == 0) return(stats::setNames(character(0), character(0)))

  buchstaben <- gsub("[^A-Za-z]", "", klassen)
  jahre <- brief_jahrgang(klassen)
  auswahl <- c("Alle Kohorten" = "alle")
  for (buchstabe in sort(unique(buchstaben))) {
    drin <- klassen[buchstaben == buchstabe]
    von <- drin[!is.na(jahre[buchstaben == buchstabe]) &
                  jahre[buchstaben == buchstabe] == stufe_alt]
    bis <- drin[!is.na(jahre[buchstaben == buchstabe]) &
                  jahre[buchstaben == buchstabe] == stufe_neu]
    label <- if (length(von) > 0 && length(bis) > 0) {
      paste0(buchstabe, ": ", von[1], " \u2192 ", bis[1])
    } else if (length(bis) > 0) {
      paste0(buchstabe, " (nur ", bis[1], ")")
    } else if (length(von) > 0) {
      paste0(buchstabe, " (nur ", von[1], ")")
    } else {
      paste0(buchstabe, ": ", paste(drin, collapse = ", "))
    }
    auswahl[[label]] <- buchstabe
  }
  auswahl
}

# Vorauswahl im Entwicklungsbrief: die Kohorte der hoechsten geladenen Klasse
# (6a -> Buchstabe a).
brief_vorauswahl_kohorte <- function(klassen) {
  klasse <- brief_vorauswahl(klassen)
  if (is.null(klasse)) return(NULL)
  buchstabe <- gsub("[^A-Za-z]", "", as.character(klasse))
  if (!nzchar(buchstabe)) buchstabe <- .brief_buchstabe(klassen)[1]
  if (length(buchstabe) == 0 || is.na(buchstabe) || !nzchar(buchstabe)) return(NULL)
  buchstabe
}

#### Vergleichswerte der Schule ####
#
# Eine Innenansicht: Sie sagt, wo ein Kind oder eine Klasse innerhalb des
# eigenen Bestands liegt - nicht, ob ein Ergebnis dem Verfahrensstandard
# entspricht. Die Werte stehen in einer eigenen Datei
# (Vergleichswerte_C-Test.xlsx), die einmalig aus den vorhandenen Daten erzeugt
# wird; die App rechnet sie nie selbst aus. Fehlt die Datei, gibt es keine
# Vergleichsaussage. Angezeigt wird nur mit eingeschaltetem Schalter
# (Einstellungen: vergleich_anzeigen).

.vergleich_dateiname <- "Vergleichswerte_C-Test.xlsx"

# Messfehler eines Einzelwerts in Prozentpunkten (eigene Analyse: SD ~21 pp,
# Reliabilitaet ~0,84 -> SEM ~8,5 pp, 95 % also rund +/-17 pp). Bei Mittelwerten
# wird er durch die Wurzel der Kinderzahl geteilt; fuer die Entwicklung kommt er
# aus dem Rest_SD der Referenzdatei (Kohorten sind Mittelwerte).
.vergleich_sem <- c("R/F" = 8.5, "WE" = 7.5)

# Mindestgroessen: darunter keine Aussage, darunter nur Viertel-Baender
.vergleich_min_kinder <- 30
.vergleich_min_dezile <- 100
.vergleich_min_klassen <- 8
# Klassen mit weniger Kindern werden nicht verglichen - dieselbe Grenze wie im
# Generator: so kleine Klassen kommen auch in die Referenz nicht hinein
.vergleich_min_klasse_kinder <- 10

# Pfad der Referenzdatei: persoenliche Kopie zuerst, sonst die mitgelieferte
.vergleich_datei <- function(datei = .vergleich_dateiname) {
  eigen <- .pfad_nativ(file.path(vorlagen_ordner(), datei))
  if (file.exists(eigen)) return(eigen)
  geliefert <- vorlagen_quelle(datei)
  if (file.exists(geliefert)) return(.pfad_nativ(geliefert))
  NULL
}

# Ein Arbeitsblatt als data.frame (Spalten technisch benannt), NULL bei Fehler
.vergleich_blatt <- function(pfad, blatt) {
  tryCatch({
    d <- readxl::read_xlsx(pfad, sheet = blatt)
    d <- as.data.frame(janitor::clean_names(d), stringsAsFactors = FALSE)
    d <- d[rowSums(!is.na(d)) > 0, , drop = FALSE]
    if (nrow(d) == 0) NULL else d
  }, error = function(e) NULL)
}

# Referenz einlesen. Rueckgabe: Liste (kinder, klassen, entwicklung, info, datei)
# oder NULL, wenn keine Datei da oder nicht lesbar ist.
vergleich_referenz <- function(pfad = .vergleich_datei()) {
  if (is.null(pfad) || !file.exists(pfad)) return(NULL)
  blaetter <- tryCatch(readxl::excel_sheets(pfad), error = function(e) character(0))
  if (length(blaetter) == 0) return(NULL)

  lesen <- function(name) if (name %in% blaetter) .vergleich_blatt(pfad, name) else NULL
  ref <- list(kinder = lesen("Kinder"), klassen = lesen("Klassen"),
              entwicklung = lesen("Entwicklung"), info = lesen("Info"),
              datei = pfad)
  if (all(vapply(ref[c("kinder", "klassen", "entwicklung")], is.null, logical(1)))) {
    return(NULL)
  }
  ref
}

# Grenzen einer Bezugsgruppe: Liste(grenzen = p10..p90, n, zeile) oder NULL
.vergleich_gruppe <- function(ref, ebene, stufe, kennzahl, lagemass = NULL) {
  if (is.null(ref)) return(NULL)
  d <- ref[[ebene]]
  if (is.null(d) || !all(c("klassenstufe", "kennzahl") %in% names(d))) return(NULL)

  idx <- which(as.character(d$klassenstufe) == as.character(stufe) &
                 toupper(as.character(d$kennzahl)) == toupper(kennzahl))
  if (!is.null(lagemass) && "lagemass" %in% names(d)) {
    idx <- idx[grepl(lagemass, as.character(d$lagemass[idx]), ignore.case = TRUE)]
  }
  if (length(idx) == 0) return(NULL)

  zeile <- d[idx[1], , drop = FALSE]
  namen <- paste0("p", c(10, 25, 50, 75, 90))
  grenzen <- vapply(namen, function(spalte) {
    if (!(spalte %in% names(zeile))) return(NA_real_)
    suppressWarnings(as.numeric(zeile[[spalte]][1]))
  }, numeric(1))
  names(grenzen) <- namen
  if (all(is.na(grenzen))) return(NULL)

  n <- NA_real_
  for (spalte in c("n", "n_klassen")) {
    if (spalte %in% names(zeile)) {
      n <- suppressWarnings(as.numeric(zeile[[spalte]][1]))
      if (!is.na(n)) break
    }
  }
  list(grenzen = grenzen, n = n, zeile = zeile)
}

# Perzentil eines Werts innerhalb der Schnittpunkte (stueckweise linear;
# ausserhalb wird bis 0 bzw. 100 fortgeschrieben).
.vergleich_perzentil <- function(wert, grenzen) {
  if (is.na(wert)) return(NA_real_)
  vorhanden <- !is.na(grenzen)
  if (sum(vorhanden) < 2) return(NA_real_)
  xs <- as.numeric(grenzen[vorhanden])
  ps <- as.numeric(sub("^p", "", names(grenzen)[vorhanden]))
  if (wert <= xs[1]) {
    if (xs[1] <= 0) return(ps[1])
    return(max(0, ps[1] * wert / xs[1]))
  }
  if (wert >= xs[length(xs)]) {
    letzte <- length(xs)
    if (xs[letzte] >= 100) return(ps[letzte])
    spanne <- xs[letzte] - xs[letzte - 1]
    if (spanne <= 0) return(ps[letzte])
    return(min(100, ps[letzte] + (100 - ps[letzte]) *
                 (wert - xs[letzte]) / spanne))
  }
  suppressWarnings(stats::approx(xs, ps, xout = wert, ties = mean, rule = 2)$y)
}

# Urteil zu einem Wert. art: "kind" (Einzelwert) oder "klasse" (Klassenmittel
# oder -median). sem ist der Messfehler des Werts. Die Entwicklung hat ein
# eigenes Urteil (vergleich_entwicklung_urteil), weil sie am Startniveau haengt.
vergleich_urteil <- function(ref, wert, kennzahl, art = c("kind", "klasse"),
                             stufe = NULL, lagemass = NULL, n_kinder = NA_real_,
                             sem = NA_real_) {
  art <- match.arg(art)
  if (is.null(ref) || length(wert) != 1 || is.na(wert)) return(NULL)

  ebene <- if (art == "kind") "kinder" else "klassen"
  gruppe <- .vergleich_gruppe(ref, ebene, stufe, kennzahl, lagemass)
  if (is.null(gruppe)) return(NULL)

  # Messfehler: beim Einzelwert der feste SEM, bei Klassen geteilt durch die
  # Wurzel der Kinderzahl (Mittelwerte sind genauer)
  if (is.na(sem)) {
    grund <- .vergleich_sem[kennzahl]
    sem <- if (is.na(grund)) NA_real_ else unname(grund)
    if (art == "klasse" && !is.na(n_kinder) && n_kinder > 0) {
      sem <- sem / sqrt(n_kinder)
    }
  }

  # Mindestgroessen
  if (art == "klasse") {
    if (is.na(gruppe$n) || gruppe$n < .vergleich_min_klassen) return(NULL)
  } else {
    if (is.na(gruppe$n) || gruppe$n < .vergleich_min_kinder) return(NULL)
  }

  grenzen <- gruppe$grenzen
  dezile <- !is.na(grenzen["p10"]) && !is.na(grenzen["p90"])
  if (art != "kind" || !dezile) {
    unten <- grenzen["p25"]; oben <- grenzen["p75"]
  } else {
    unten <- grenzen["p10"]; oben <- grenzen["p90"]
  }
  if (is.na(unten) || is.na(oben)) return(NULL)

  # oberes Band nur, wenn es sich abgrenzen laesst (bei WE sitzt der p90 am
  # Maximum von 100 %)
  oben_abgrenzbar <- is.finite(oben) && oben < 100

  band <- if (wert < unten) {
    if (art == "kind") { if (dezile) "untere 10 %" else "unteres Viertel" } else
      "unteres Viertel"
  } else if (wert > oben) {
    if (art == "kind" && oben_abgrenzbar) {
      if (dezile) "obere 10 %" else "oberes Viertel"
    } else if (art == "kind") {
      "oberer Bereich (nicht abgrenzbar)"
    } else {
      "oberes Viertel"
    }
  } else {
    if (art == "kind") { if (dezile) "im Jahrgangsbereich" else "im mittleren Bereich" } else
      "Mittelfeld"
  }

  rang <- c(von = NA_real_, bis = NA_real_)
  if (!is.na(sem)) {
    rang <- sort(c(.vergleich_perzentil(wert - sem, grenzen),
                   .vergleich_perzentil(wert + sem, grenzen)))
    rang <- round(pmax(0, pmin(100, rang)))
  } else {
    rang <- rep(round(.vergleich_perzentil(wert, grenzen)), 2)
  }

  list(art = art, band = unname(band), rang = rang, grenzen = grenzen,
       n = gruppe$n, dezile = dezile, wert = wert, kennzahl = kennzahl,
       lagemass = lagemass, stufe = stufe, sem = sem)
}

# Kurzform fuer die Tabelle: "obere 10 % (Rang 3-12 %)"
vergleich_kurz <- function(urteil) {
  if (is.null(urteil)) return(NA_character_)
  rang <- urteil$rang
  if (any(is.na(rang))) return(urteil$band)
  if (rang[1] == rang[2]) return(paste0(urteil$band, " (Rang ", rang[1], " %)"))
  paste0(urteil$band, " (Rang ", rang[1], "\u2013", rang[2], " %)")
}

# Satz fuer Statistik-Tab und Briefe
vergleich_satz <- function(urteil, was = NULL) {
  if (is.null(urteil)) return(NULL)
  kennzahl <- if (is.null(was)) urteil$kennzahl else was
  if (isTRUE(urteil$art == "entwicklung")) return(vergleich_entwicklung_satz(urteil, kennzahl))
  if (urteil$art == "kind") {
    return(paste0("Vergleichswert ", kennzahl, ": ", vergleich_kurz(urteil), "."))
  }
  if (urteil$art == "klasse") {
    lage <- if (isTRUE(urteil$lagemass == "Median")) "Klassen-Median" else "Klassenmittel"
    return(paste0(lage, " ", kennzahl, ": ", vergleich_kurz(urteil),
                  " im Vergleich zu den ", urteil$stufe, ". Klassen dieser Schule (n = ",
                  urteil$n, ")."))
  }
  NULL
}

#### Entwicklung: erwartete Entwicklung aus dem Startniveau ####
#
# Die Entwicklung haengt am Ausgangsniveau: wer in der 5 schon weit oben steht,
# kann sich kaum verbessern. Deshalb wird nicht die Veraenderung allein
# bewertet, sondern die Abweichung von der Erwartung
#   erwartet = Erwartung_a + Erwartung_b * Startniveau
# (Startniveau = Mittel der 5. Klasse der Kohorte). Die Erwartung ist gedeckelt
# auf den verbleibenden Raum bis 100 %.
#
# Das Band kommt aus der Streuung der Kohortenabweichungen dieser Schule
# (Kohorten_SD): innerhalb einer Standardabweichung gilt die Entwicklung als
# ueblich. Das Rangintervall (Rest_SD/wurzel(n), also der Fehler des Mittels)
# entscheidet nur ueber die Wortwahl: beruehrt es die Bandgrenze, heisst es
# "leicht", liegt es ganz ausserhalb, "deutlich".

# Kennzahlen der Erwartung aus dem Blatt "Entwicklung" lesen
.vergleich_erwartung <- function(ref, kennzahl) {
  if (is.null(ref) || is.null(ref$entwicklung)) return(NULL)
  d <- ref$entwicklung
  if (!all(c("klassenstufe", "kennzahl") %in% names(d))) return(NULL)
  idx <- which(toupper(as.character(d$kennzahl)) == toupper(kennzahl))
  if (length(idx) == 0) return(NULL)
  z <- d[idx[1], , drop = FALSE]
  # Spaltennamen tolerant suchen (die App liest die Datei klein geschrieben ein,
  # eine von Hand gepflegte Datei kann anders schreiben)
  hol <- function(name) {
    i <- which(tolower(names(z)) == tolower(name))
    if (length(i) == 0) return(NA_real_)
    suppressWarnings(as.numeric(z[[i[1]]][1]))
  }
  a <- hol("erwartung_a")
  b <- hol("erwartung_b")
  if (is.na(a) || is.na(b)) return(NULL)
  list(a = a, b = b, rest_sd = hol("rest_sd"), kohorten_sd = hol("kohorten_sd"),
       n_kohorten = hol("n_kohorten"), n = hol("n"))
}

# Bewertung der Entwicklung einer Kohorte. start und delta sind Mittelwerte der
# Kohorte, n_kinder die Zahl der Kinder dahinter.
vergleich_entwicklung_urteil <- function(ref, start, delta, kennzahl, n_kinder = NA_real_) {
  if (is.null(ref) || length(start) != 1 || length(delta) != 1) return(NULL)
  if (is.na(start) || is.na(delta)) return(NULL)
  e <- .vergleich_erwartung(ref, kennzahl)
  if (is.null(e) || is.na(e$kohorten_sd) || e$kohorten_sd <= 0) return(NULL)

  erwartet <- e$a + e$b * start
  # nicht mehr als der verbleibende Raum bis 100 %
  erwartet <- min(erwartet, max(0, 100 - start))
  abweichung <- delta - erwartet
  sem <- if (!is.na(e$rest_sd) && !is.na(n_kinder) && n_kinder > 0) {
    e$rest_sd / sqrt(n_kinder)
  } else {
    NA_real_
  }
  intervall <- if (is.na(sem)) c(abweichung, abweichung) else
    c(abweichung - sem, abweichung + sem)
  # Wortwahl: liegt das Rangintervall ganz im Band, ist die Entwicklung ueblich.
  # Liegt es ganz ausserhalb, "deutlich"; beruehrt es die Grenze, "leicht".
  oben <- e$kohorten_sd
  unten <- -e$kohorten_sd
  bewertung <- if (intervall[1] >= unten && intervall[2] <= oben) {
    "im üblichen Bereich"
  } else if (intervall[1] > oben) {
    "deutlich über dem Üblichen"
  } else if (intervall[2] < unten) {
    "deutlich unter dem Üblichen"
  } else if (intervall[1] < unten) {
    "leicht unter dem Üblichen"
  } else {
    "leicht über dem Üblichen"
  }

  list(art = "entwicklung", kennzahl = kennzahl, start = start, delta = delta,
       erwartet = erwartet, abweichung = abweichung, bewertung = bewertung,
       band = bewertung, sd_kohorten = e$kohorten_sd, sem = sem, n = e$n,
       n_kohorten = e$n_kohorten)
}

# Satz fuer den Entwicklungsbrief (je Kohorte und Kennzahl)
vergleich_entwicklung_satz <- function(urteil, was = NULL) {
  if (is.null(urteil) || !isTRUE(urteil$art == "entwicklung")) return(NULL)
  kennzahl <- if (is.null(was)) urteil$kennzahl else was
  paste0("Die Kohorte startete bei ", de_zahl(urteil$start), " % (", kennzahl,
         ") und erreichte einen mittleren Zuwachs von ",
         de_vz(urteil$delta), " Punkten \u2013 üblich für dieses Niveau sind etwa ",
         de_vz(urteil$erwartet), "; die Entwicklung liegt ", urteil$bewertung, ".")
}

# Zeile(n) fuer den Statistik-Tab. urteile ist eine benannte Liste von
# Entwicklung-Urteilen (Name = Beschriftung wie "5c->6c"; ohne Namen steht die
# Zeile fuer die Gesamtuebersicht).
vergleich_entwicklung_zeile <- function(urteile, kennzahl) {
  if (length(urteile) == 0) return(NULL)
  namen <- names(urteile)
  behalten <- !vapply(urteile, is.null, logical(1))
  urteile <- urteile[behalten]
  namen <- if (is.null(namen)) rep("", length(urteile)) else namen[behalten]
  if (length(urteile) == 0) return(NULL)

  teile <- vapply(seq_along(urteile), function(i) {
    u <- urteile[[i]]
    text <- paste0(de_vz(u$delta), " (erwartet ", de_vz(u$erwartet), ")")
    if (!is.na(namen[i]) && nzchar(namen[i])) text <- paste0(text, "; ", namen[i])
    text
  }, character(1))
  bewertungen <- unique(vapply(urteile, function(u) u$bewertung, character(1)))
  paste0("Mittlere Entwicklung ", kennzahl, " (5 \u2192 6): ",
         paste(teile, collapse = ", "),
         if (length(bewertungen) == 1) paste0(" \u2013 ", bewertungen) else "")
}

# Zeilen fuer den Statistik-Tab: je Klasse der Median-Vergleich (der Median ist
# robuster als der Mittelwert und steht dort schon). kennzahl waehlt die
# Kennzahl; ohne Angabe kommen beide (R/F zuerst).
vergleich_klassen_zeilen <- function(df, ref, kennzahl = NULL) {
  if (is.null(ref) || is.null(df) || nrow(df) == 0) return(character(0))
  if (!all(c("Klasse", "R/F-%", "WE-%") %in% colnames(df))) return(character(0))
  kennzahlen <- if (is.null(kennzahl)) c("R/F", "WE") else kennzahl

  zeilen <- character(0)
  for (klasse in sort(unique(as.character(df$Klasse)))) {
    teil <- df[as.character(df$Klasse) == klasse, , drop = FALSE]
    stufe <- suppressWarnings(as.numeric(gsub("[^0-9]", "", klasse)))
    if (is.na(stufe)) next
    for (kennzahl in kennzahlen) {
      werte <- suppressWarnings(as.numeric(teil[[paste0(kennzahl, "-%")]]))
      werte <- werte[!is.na(werte)]
      if (length(werte) < .vergleich_min_klasse_kinder) next
      urteil <- vergleich_urteil(ref, stats::median(werte), kennzahl, "klasse",
                                 stufe = as.character(stufe), lagemass = "Median",
                                 n_kinder = length(werte))
      if (is.null(urteil)) next
      zeilen <- c(zeilen, paste0(klasse, " (n = ", length(werte), "): ", kennzahl,
                                 "-Median ", format(round(stats::median(werte), 1),
                                                    decimal.mark = ","),
                                 " % \u2192 ", vergleich_kurz(urteil),
                                 " \u00b7 Bezug: ", stufe, ". Klassen dieser Schule (n = ",
                                 urteil$n, ")"))
    }
  }
  zeilen
}

# Zeilen fuer den Statistik-Tab: mittlere Entwicklung gegen die Erwartung.
# gematcht ist die Zuordnung (cohort_gematcht). pro_kohorte = FALSE ergibt eine
# Zeile ueber alle zugeordneten Kinder, TRUE eine je Kohorte (Buchstabe).
vergleich_entwicklung_zeilen <- function(gematcht, ref, kennzahl, pro_kohorte = FALSE) {
  if (is.null(ref) || is.null(gematcht) || !is.data.frame(gematcht) ||
      nrow(gematcht) == 0) {
    return(character(0))
  }
  spalte_alt <- paste0(if (kennzahl == "R/F") "RF" else "WE", "_Alt")
  spalte_neu <- paste0(if (kennzahl == "R/F") "RF" else "WE", "_Neu")
  if (!all(c(spalte_alt, spalte_neu) %in% names(gematcht))) return(character(0))

  start <- suppressWarnings(as.numeric(gematcht[[spalte_alt]]))
  ziel <- suppressWarnings(as.numeric(gematcht[[spalte_neu]]))
  ok <- !is.na(start) & !is.na(ziel)
  if (!any(ok)) return(character(0))

  urteil_aus <- function(start_v, ziel_v) {
    vergleich_entwicklung_urteil(ref, mean(start_v), mean(ziel_v - start_v), kennzahl,
                                 n_kinder = length(start_v))
  }

  if (!pro_kohorte) {
    u <- urteil_aus(start[ok], ziel[ok])
    if (is.null(u)) return(character(0))
    # leerer Name = Zeile fuer die Gesamtuebersicht (list("" = u) ist in R nicht erlaubt)
    zeile <- vergleich_entwicklung_zeile(stats::setNames(list(u), ""), kennzahl)
    return(if (is.null(zeile)) character(0) else zeile)
  }

  # je Kohorte (Buchstabe), Beschriftung wie im Brief (5c -> 6c)
  klasse_alt <- as.character(gematcht$Klasse_Alt)
  klasse_neu <- as.character(gematcht$Klasse_Neu)
  klasse <- ifelse(is.na(klasse_neu) | !nzchar(klasse_neu), klasse_alt, klasse_neu)
  buchstabe <- tolower(gsub("[^A-Za-z]", "", klasse))
  urteile <- list()
  for (b in sort(unique(buchstabe[ok]))) {
    idx <- which(ok & buchstabe == b)
    if (length(idx) < .vergleich_min_klasse_kinder) next
    u <- urteil_aus(start[idx], ziel[idx])
    if (is.null(u)) next
    beschriftung <- paste0(unique(klasse_alt[idx])[1], "\u2192", unique(klasse_neu[idx])[1])
    if (any(is.na(c(unique(klasse_alt[idx])[1], unique(klasse_neu[idx])[1])))) {
      beschriftung <- unique(klasse[idx])[1]
    }
    urteile[[beschriftung]] <- u
  }
  if (length(urteile) == 0) return(character(0))
  zeile <- vergleich_entwicklung_zeile(urteile, kennzahl)
  if (is.null(zeile)) character(0) else zeile
}

# Zwei Anzeigespalten fuer die Uebersichtstabelle (Innenansicht je Kind).
# Rueckgabe NULL, wenn nichts zu vergleichen ist.
vergleich_spalten <- function(df, ref) {
  if (is.null(ref) || is.null(df) || nrow(df) == 0) return(NULL)
  if (!all(c("Klasse", "R/F-%", "WE-%") %in% colnames(df))) return(NULL)

  spalte <- function(kennzahl, werte_spalte) {
    werte <- suppressWarnings(as.numeric(df[[werte_spalte]]))
    klassen <- as.character(df$Klasse)
    werte_urteil <- vapply(seq_along(werte), function(i) {
      stufe <- suppressWarnings(as.numeric(gsub("[^0-9]", "", klassen[i])))
      u <- vergleich_urteil(ref, werte[i], kennzahl, "kind", stufe = as.character(stufe))
      if (is.null(u)) "" else vergleich_kurz(u)
    }, character(1))
    werte_urteil
  }
  data.frame("Vergleich R/F" = spalte("R/F", "R/F-%"),
             "Vergleich WE" = spalte("WE", "WE-%"),
             check.names = FALSE, stringsAsFactors = FALSE)
}

# Zeitraum und Quelle aus dem Info-Blatt der Referenz (fuer die Anzeige)
vergleich_zeitraum <- function(ref) {
  if (is.null(ref) || is.null(ref$info)) return(NA_character_)
  info <- ref$info
  if (!all(c("feld", "wert") %in% names(info))) return(NA_character_)
  hole <- function(name) {
    i <- which(tolower(trimws(as.character(info$feld))) == tolower(name))
    if (length(i) == 0) NA_character_ else as.character(info$wert[i[1]])
  }
  von <- hole("Zeitraum von")
  bis <- hole("Zeitraum bis")
  if (is.na(von) && is.na(bis)) return(NA_character_)
  paste0(von, "\u2013", bis)
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

  # Feste Reihenfolge der Briefe: erst nach Jahrgang/Klasse, dann nach Name.
  # Damit ist auch eine Auswahl ueber mehrere Klassen (Jahrgang oder alle) im
  # Dokument sauber gruppiert.
  stufe <- suppressWarnings(as.numeric(gsub("[^0-9]", "", as.character(df$klasse))))
  df <- df[order(stufe, as.character(df$klasse), as.character(df$name)), , drop = FALSE]

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


