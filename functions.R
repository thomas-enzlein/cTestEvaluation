composeClass <- function(klassenstufe, klassenBuchstabe) {
  if(is.na(klassenstufe) | nchar(klassenBuchstabe) == 0) {
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
  recommendations <- c("1A" = "Kein Handlungsbedarf für Rechtschreibung und Wortschatz",
                       "2A" = "Kein Handlungsbedarf für Rechtschreibung und Wortschatz",
                       "1B" = "Kein Handlungsbedarf, Rechtschreibung lässt sich durch Übung weiter verbessern",
                       "2B" = "Kein Handlungsbedarf, Rechtschreibung lässt sich durch Übung weiter verbessern",
                       "3C" = "Rechtschreibung deutlich schlechter als Wortschatz",
                       "3C*" = "Rechtschreibung deutlich schlechter als Wortschatz (mögl. LRS)",
                       "3D" = "Mögl. Handlungsbedarf im Bereich Lesen",
                       "4C" = "Handlungsbedarf, Rechtschreibung deutlich schlechter als Wortschatz",
                       "4C*" = "Handlungsbedarf, Rechtschreibung deutlich schlechter als Wortschatz (mögl. LRS)",
                       "4D" = "Handlungsbedarf, sowohl schwacher Wortschatz als auch schwache Rechtschreibung",
                       "4E" = "Handlungsbedarf besonders beim Wortschatz",
                       "5C" = "hoher Handlungsbedarf, Rechtschreibung deutlich schlechter als Wortschatz",
                       "5C*" = "hoher Handlungsbedarf, Rechtschreibung deutlich schlechter als Wortschatz (mögl. LRS)",
                       "5D" = "hoher Handlungsbedarf, sowohl schwacher Wortschatz als auch schwache Rechtschreibung",
                       "5E" = "hoher Handlungsbedarf besonders beim Wortschatz",
                       "0"  =  "Hat nicht teilgenommen")
  
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

# refocus js-function
jscode <- "
shinyjs.refocus = function(e_id) {
  document.getElementById(e_id).focus();
}"

# Dateipfad erstellen
createFilePath <- function(filename, extension) {
  outpath <-  file.path(getwd(), "Auswertungen")
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

# Histogram erstellen
createHistogram <- function(df, x, fill, xlab, bins = 30) {
  df <- df %>%
    mutate(Name = paste0(Name, ", ", Klasse))
  
  p <- ggplot(df, aes_string(x = x, text = "Name", fill = fill)) +
    geom_histogram(bins = bins, col = "black", show.legend = FALSE) +
    scale_fill_manual(values =  cols, limits = lvls) +
    labs(x = xlab,
         y = "Anzahl") +
    theme(legend.position='none') 
  
  return(p)
}

# Median und Mittelwert berechnen und aufbereiten
createStatsText <- function(df, column, label) {
  l1 <- paste0("Mittelwert ", label, ": ", round(mean(df[[column]], na.rm = TRUE), 1), "±", round(sd(df[[column]], na.rm = TRUE), 1), "%")
  l2 <- paste0("Median ", label, ": ", round(median(df[[column]], na.rm = TRUE), 1), "%")
  
  res <- div(HTML(paste(l1, l2,  sep = "<br/>")),
             style = "margin-left:15px;
             margin-right:15px;
             font-size: 20px;
             font-style: bold")
  
  return(res)
}

# plot/stats output vorbereiten
createPlotOutput <- function(outputId) {
  div(plotlyOutput(outputId = outputId), class = "plot-div")
}

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
                        margin-left:15px',
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

saveData <- function(df) {
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
  
  return(msgs)
}

checkInputFile <- function(inputFile) {
  req(inputFile)
  ext <- tools::file_ext(inputFile$datapath)
  
  validate(need(ext == "tsv", "Bitte tsv Datei auswählen"))
  return(inputFile$datapath)
}

loadData <- function(inputFile) {
  raw <- read_tsv(checkInputFile(inputFile), show_col_types = FALSE)
  
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
             `Kat.` = factor(`Kat.`, levels = lvls),
             Empfehlung = as.character(Empfehlung))
    return(new_df)
  }
  
  new_df <- read_tsv(
    checkInputFile(inputFile), 
    col_types = list(col_character(),
                     col_character(),
                     col_number(),
                     col_number(),
                     col_number(),
                     col_number(),
                     col_factor(levels = lvls),
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

compose_letter <- function(name, klasse, kat, lehrername, signatur, output = NULL) {
  kat <- convert_kat_meaning(kat)
  
  rmarkdown::render("elternbrief/elternbrief.Rmd", output_file = output)
}

combine_letters <- function(rdocx, temp_path, out_path) {
  rdocx <- 
    rdocx %>% 
    officer::body_add_break() %>% 
    officer::body_add_docx(temp_path)  
  
  return(rdocx)
}

create_letters <- function(df, lehrername, signatur) {
  
  df <- janitor::clean_names(df)
  
  if(fs::file_exists("elternbrief/elternbrief.knit.md")) {
    fs::file_delete("elternbrief/elternbrief.knit.md")
  }
  
  for(i in 1:dim(df)[1]) {
    tmp <- tempfile(fileext = ".docx")
    message("Composing letter for ",  df$name[i], " ", i, "/", dim(df)[1])
    compose_letter(name = df$name[i], 
                   klasse = parse_number(df$klasse[i]), 
                   kat = df$kat[i], 
                   lehrername = lehrername, 
                   signatur = signatur,
                   output = tmp)
    
    if(i == 1) {
      rdocx <- officer::read_docx(tmp)
      next()
    } 
    rdocx <- combine_letters(rdocx, temp_path = tmp)
  }
  
  fn <- paste0(createFilePath(NULL, ""),"/Elternbriefe_", Sys.Date())
  
  if("klasse" %in% colnames(df)) {
    kl <- paste0(unique(df$klasse), collapse = "_")
    fn <- paste0(fn, "_", kl)
  }
  cat("saved letters to", fn)
  rdocx %>%
    print(paste0(fn, ".docx"))
}


