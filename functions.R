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

checkInputErrors <- function(inputName, inputRf, inputWe, numItems) {
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

addEntry <- function(df, name, rf, we, numItems) {
  rfPerc <- round(rf/numItems*100,1)
  wePerc <- round(we/numItems*100,1)
  
  kat <- paste0(getRFlevel(rfPerc), 
                getWElevel(rfPerc, wePerc))
  new <- tibble("Name" = name,
                "R/F-Wert" = rf,
                "R/F-%" = rfPerc,
                "WE-Wert" = we,
                "WE-%" = wePerc,
                "Kat." = kat,
                "Empfehlung" = getRecommendation(kat))
  
  df <- bind_rows(df, new)
  
  return(df)
}

setDocBackgroundColor <- function(tab, df, colors, levels) {
  
  stopifnot(length(colors) == length(levels))
  for (i in seq_along(levels)) {
    lvl <- levels[i]
    kat <- pull(df, "Kat.")
    idx <- which(kat == lvl)
    tab <- flextable::bg(x = tab, 
                         j = 6, # Kat. column
                         i = idx, 
                         bg = colors[i])
  }
  return(tab)
}

# angepasste export::table2doc funktion, original see https://github.com/tomwenseleers/export, credit to Tom Wenseleers
table2doc_ = function(x = NULL, file = "Rtable", append = FALSE, digits = 2, 
                         digitspvals = NULL, trim.pval = 1E-16, width = NULL, height = NULL, offx = 1, offy = 1, 
                         font = ifelse(Sys.info()["sysname"]=="Windows","Arial","Helvetica")[[1]], pointsize = 12, 
                         add.rownames = FALSE) {
  
  if(is.null(digitspvals)) digitspvals <- digits
  obj=x
  if (is.null(obj)) {
    outp = .Last.value # capture previously shown output or use passed object
  } else {
    outp = obj
  }
  if (is.null(outp)) stop("no R stats object available to export")
  supobjects = unique(c(as.character(gsub("xtable.", "", methods(xtable::xtable))), 
                        as.character(gsub("tidy.", "", methods(broom::tidy))),
                        "xtabs"))
  if (length(intersect(class(outp), supobjects)) == 0) stop(paste0(class(outp), " is currently not supported by table2office"))
  
  ext <-  ".docx"
  
  file = sub("^(.*)[.].*", "\\1", file)  # remove extension if given
  file = paste0(file, ext)  # add extension
  
  if (append & file.exists(file)) { 
    doc = officer::read_docx(path = file) 
    doc = officer::body_add_break(doc, pos = "after")
  } else { 
    doc = officer::read_docx(path = "template.docx") 
  }
  pagesize <- (doc$sect_dim$page - doc$sect_dim$margins[c(3,2)])/1440 # 1440 is a factor to convert to inches
  
  # deal with specific classes of objects 
  if (inherits(outp, "summary.merMod")) {
    outp <- data.frame(coef(summary(outp)), check.names = F)
  } else if(inherits(outp, "Matrix")) {
    outp <- as.data.frame(as.matrix(x))
  } else if (inherits(outp, c("xtabs", "ftable"))) {
    outp <- ftable(outp)
  } 
  
  # Depending on the data class, call xtable or tidy
  if (length(intersect(class(outp), as.character(gsub("xtable.", "", methods(xtable::xtable))))) >= 1) {
    tab <- export:::xtable2(x=outp, ndigits = digits, ndigitspvals = digitspvals, trim.pval = trim.pval)
  } else if (length(intersect(class(outp), as.character(gsub("tidy.", "", methods(broom::tidy))))) >= 1) {
    tab <- export:::tidy2(x=outp, ndigits = digits, ndigitspvals = digitspvals, trim.pval = trim.pval)
  } else { # should not occur
    tab <- export:::data.frame2(x=outp, ndigits = digits, ndigitspvals = digitspvals, trim.pval = trim.pval)
  }
  
  nc <- ncol(tab)
  nr <- nrow(tab)
  tblaspectr = nc / nr * 2  # guess table aspect ratio
  pageaspectr = pagesize["width"]/pagesize["height"]
  if (pageaspectr > tblaspectr) {
    xf = tblaspectr/pageaspectr
    yf = 1
  } else {
    xf = 1
    yf = pageaspectr/tblaspectr
  }
  w = pagesize["width"] * xf
  h = pagesize["height"] * yf
  # if width and height is given override other scaling params
  if (!is.null(width)) w = width  
  if (!is.null(height)) h = height
  
  # Avoid bug in flextable: when one of the colnames = x, flextable returns an empty table
  x.col <- which(colnames(tab) == "x")
  if(length(x.col)>0) colnames(tab)[x.col]<- "x "
  
  cell.height <- min(h, pagesize["height"] - offy)/(nr+1)
  cell.width <- min(w, pagesize["width"] - offx)/(nc+1)
  
  if(inherits(tab,"xtable")){
    tab <- flextable::as_flextable(tab, include.rownames = add.rownames, rowname_col = ".")
    tab <- flextable::width(tab, width=c(1, 0.5, 0.5, 0.5, 0.5, 0.5, 4))
    tab <- flextable::height(tab, height=cell.height)
  } else {
    if(add.rownames) x <- cbind(" " = rownames(x), x)
    tab <- flextable::flextable(tab, cheight = cell.height, cwidth = cell.width)
  }
  
  # Format the digits 
  col.pval <- grep("\\QPr(\\E|\\Qp-value\\E|\\Qp value\\E|\\Qpadj\\E|^p$|^padj$|p[.]value", tab$col_keys, value = TRUE)
  col.df <- grep("^df$", tab$col_keys, value = TRUE, ignore.case = TRUE) 
  col.other <- tab$col_keys[! tab$col_keys %in% c(col.pval, col.df)]
  tab <- flextable::colformat_double(x = tab, j = col.other, digits = digits)
  tab <- flextable::colformat_int(x = tab, j = col.df)
  tab <- flextable::colformat_double(x = tab, j = col.pval)
  tab <- flextable::bold(tab, part = "header") # bold header
  tab <- flextable::fontsize(tab, part = "all", size = pointsize) 
  tab <- flextable::font(tab, part = "all", fontname = font)
  #tab <- setDocBackgroundColor(tab = tab, df = x, colors = cols, levels = lvls)
  
  doc <- flextable::body_add_flextable(doc, value = tab)
  
  
  print(doc, target = file)
  message(paste0("Exported table as ",file))
  return(tab)
}
