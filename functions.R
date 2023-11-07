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

