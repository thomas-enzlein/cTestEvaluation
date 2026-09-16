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

getRFlevel <- function(rfPerc) {
  # Wende Grenzwerte an um rf Kategorie zu erhalten.
  res <- case_when (rfPerc >= 71.3 ~ 1,
                    rfPerc >= 66.3 ~ 2,
                    rfPerc >= 56.3 ~ 3,
                    rfPerc >= 36.2 ~ 4,
                    rfPerc < 36.2 ~ 5,
                    is.na(rfPerc)  ~ 0)
  return(res)
}

getWElevel <- function(rfPerc, wePerc) {
  diff <- wePerc - rfPerc
  rflvl <- getRFlevel(rfPerc)
  
  res <- case_when(rflvl <= 2 & diff <= 10 ~ "A", 
                   rflvl <= 2 & diff > 10 ~ "B",  
                   rflvl > 2 & between(diff, 10, 19.9) & wePerc > 65 ~ "C",
                   rflvl > 2 & diff >= 20 & wePerc > 65 ~ "C*",
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

# Ausgabeordner bestimmen: Vorgabe (Option) > Programmordner > Benutzerordner
ctest_ausgabeordner <- function(neu = FALSE) {
  vorgabe <- getOption("ctest.outdir")
  if (!is.null(vorgabe)) {
    if (!verzeichnis_sicherstellen(vorgabe)) {
      stop("Ausgabeordner '", vorgabe, "' kann nicht angelegt werden. ",
           "Bitte Option ctest.outdir pruefen.", call. = FALSE)
    }
    return(vorgabe)
  }

  programmordner <- file.path(getwd(), "Auswertungen")

  # Zwischenspeicher nur nutzen, wenn er zum aktuellen Programmordner passt
  if (!neu && !is.null(.ctest_env$outdir) &&
      identical(.ctest_env$programmordner, programmordner)) {
    return(.ctest_env$outdir)
  }

  .ctest_env$programmordner <- programmordner

  if (verzeichnis_sicherstellen(programmordner)) {
    .ctest_env$outdir <- programmordner
    return(programmordner)
  }

  benutzerordner <- benutzer_ausgabeordner()
  if (verzeichnis_sicherstellen(benutzerordner)) {
    message("Programmordner ist nicht beschreibbar - Ausgaben gehen nach: ", benutzerordner)
    .ctest_env$outdir <- benutzerordner
    return(benutzerordner)
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
  
  fullpath <- file.path(outpath, paste0(filename, ".", extension))
  return(fullpath)
}

# Spaltennamen in tsv pruefen
checkColumnNames <- function(df1, df2) {
  return(all(colnames(df1) %in% colnames(df2)))
}

# Median und Mittelwert berechnen und aufbereiten
createStatsText <- function(df, column, label, multiple = FALSE) {
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
                
  
  res <- div(HTML(paste(l1, l2, l3,  sep = "<br/>")),
             style = "margin-left:15px;
             margin-right:15px;
             font-size: 20px;
             font-style: bold")
  
  return(res)
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
                "Empfehlung" = getRecommendation(kat))
  
  df <- bind_rows(df, new)
  
  return(df)
}

saveData <- function(df, vergleich = NULL) {
  fn <- paste0("C-Test_Auswertung_", Sys.Date())
  
  if("Klasse" %in% colnames(df)) {
    kl <- paste0(unique(df$Klasse), collapse = "_")
    fn <- paste0(fn, "_", kl)
  }
  
  write_tsv(df, 
            file = createFilePath(fn, "tsv"))
  table2doc_(df, 
             file = createFilePath(fn, ""), 
             digits = 1, 
             width = 8.3,
             height = 11.7,
             pointsize = 7)
  
  table2spreadsheet_(df, 
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
    ft <- tabelle_infobrief(vergleich, farb_spalten = c("\u0394 WE", "\u0394 R/F"))
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

loadData <- function(inputFile) {
  pfad <- checkInputFile(inputFile)
  raw <- read_tsv(pfad, show_col_types = FALSE)
  if(!"Klasse" %in% colnames(raw)) {
    message("Old .tsv file detected, converting to new format.")
    new_df <- raw %>%
      mutate(Klasse = "") %>%
      select(Name,
             Klasse, 
             `WE-Wert`,
             `WE-%`,
             `R/F-Wert`,
             `R/F-%`,
             `Kat.`,
             Empfehlung) %>%
      mutate(Name = as.character(Name),
             Klasse = as.character(Klasse),
             `WE-Wert` = as.numeric(`WE-Wert`),
             `WE-%` = as.numeric(`WE-%`),
             `R/F-Wert` = as.numeric(`R/F-Wert`),
             `R/F-%` = as.numeric(`R/F-%`),
             # Kat. bewusst als Text: "0" (hat nicht teilgenommen) ist keine
             # der 15 Kategorien und wurde als Faktor zu NA (Datenverlust)
             `Kat.` = as.character(`Kat.`),
             Empfehlung = as.character(Empfehlung))
    return(new_df)
  }
  
  new_df <- read_tsv(
    pfad,
    col_types = list(col_character(),
                     col_character(),
                     col_number(),
                     col_number(),
                     col_number(),
                     col_number(),
                     col_character(),
                     col_character()), 
    col_select = c(Name,
                   Klasse, 
                   `WE-Wert`,
                   `WE-%`,
                   `R/F-Wert`,
                   `R/F-%`,
                   `Kat.`,
                   Empfehlung))
  
  return(new_df)
}

convert_kat_meaning <- function(kat, table_path = "elternbrief/ergebnisse.xlsx") {
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

vorlagen_vorbereiten <- function(quelle, praefix) {
  if (!dir.exists(quelle)) {
    stop("Vorlagen nicht gefunden: '", quelle, "'", call. = FALSE)
  }
  ziel <- file.path(tempdir(), paste0(praefix, "_", Sys.getpid()))
  if (dir.exists(ziel)) vorlagen_aufraeumen(ziel)
  if (!dir.create(ziel, recursive = TRUE, showWarnings = FALSE)) {
    stop("Temporaeres Arbeitsverzeichnis konnte nicht angelegt werden.", call. = FALSE)
  }
  dateien <- list.files(quelle, full.names = TRUE)
  kopiert <- file.copy(dateien, ziel, recursive = TRUE)
  if (!all(kopiert)) {
    stop("Vorlagen konnten nicht kopiert werden.", call. = FALSE)
  }
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

generate_qrcode <- function(qrLink) {
  if (is.null(qrLink) || !isTruthy(qrLink)) return(NA)

  # QR-Code immer aus dem Originallink erzeugen - das braucht kein Internet
  qr <- qr_code(qrLink, ecl = "M")
  qr_tmp <- tempfile(fileext = ".png")
  png(filename = qr_tmp)
  plot(qr)
  dev.off()

  # Verweis auf die Uebungssammlung: mit Kurzlink, wenn das moeglich ist
  kurz <- shorten_url(qrLink)
  ziel <- if (is.null(kurz)) qrLink else kurz
  linkText <- paste("Sie möchten Ihr Kind unterstützen?",
                    "Dann schauen Sie hier in unsere Sammlung:", ziel,
                    sep = "\n")

  return(list(img = qr_tmp,
              txt = linkText))
}

create_letters <- function(df, lehrername, signatur = "", qrLink = NULL) {
  
  df <- janitor::clean_names(df) %>%
    mutate(kat = str_remove(kat, pattern = "\\*"))
  
  # Ausgabeordner frueh pruefen und anlegen: fehlende Schreibrechte sollen
  # sofort gemeldet werden und nicht erst nach dem Rendern aller Briefe
  ziel_ordner <- createFilePath(NULL, "")

  # aus einer Arbeitskopie im Temp-Verzeichnis rendern, damit im
  # Programmverzeichnis nichts geschrieben wird (installierte App)
  vorlage <- elternbrief_vorbereiten()
  on.exit(vorlagen_aufraeumen(vorlage), add = TRUE)
  knit_reste_entfernen(vorlage)
  
  # Ein Fehler bei einem Kind darf die restlichen Briefe nicht verhindern:
  # Fehler werden gesammelt und am Ende gemeldet.
  brief_pfade <- character(0)
  fehler <- character(0)

  for(i in seq_len(dim(df)[1])) {
    tmp <- tempfile(fileext = ".docx")
    message("Composing letter for ",  df$name[i], " ", i, "/", dim(df)[1])
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
  rdocx <- officer::read_docx(brief_pfade[1])
  for(pfad in brief_pfade[-1]) {
    rdocx <- combine_letters(rdocx, temp_path = pfad)
  }
  
  fn <- file.path(ziel_ordner, paste0("Elternbriefe_", Sys.Date()))
  
  if("klasse" %in% colnames(df)) {
    kl <- paste0(unique(df$klasse), collapse = "_")
    fn <- paste0(fn, "_", kl)
  }
  zieldatei <- paste0(fn, ".docx")
  message("Elternbriefe gespeichert unter: ", zieldatei)
  print(rdocx, target = zieldatei)

  return(list(datei = zieldatei,
              erstellt = length(brief_pfade),
              fehler = fehler))
}


