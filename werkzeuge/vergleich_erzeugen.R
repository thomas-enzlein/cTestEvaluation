# Erzeugt die Datei mit den Vergleichswerten der Schule
# (Vergleichswerte_C-Test.xlsx) fuer die Innenansicht der App.
#
# Aufruf (aus dem Projektordner der App):
#
#   Rscript werkzeuge/vergleich_erzeugen.R [Datenordner] [Schulart]
#
#   Datenordner  Ordner mit den tsv-Dateien; er wird rekursiv gelesen.
#                Ohne Angabe gilt der Ausgabeordner der App
#                (Dokumente\C-Test Auswertung).
#   Schulart     freier Text fuer das Info-Blatt, z. B. "Gesamtschule".
#                Ohne Angabe steht dort "nicht angegeben".
#
# Die Datei landet im Vorlagenordner der App
# (Dokumente\C-Test Auswertung\vorlagen) und wird von der App nur gelesen.
# Geschrieben werden ausschliesslich Kennzahlen - keine Namen, keine
# Einzelwerte, keine Klassenlisten.
#
# Regeln (siehe auch helpfiles/anleitung.md, Abschnitt "Vergleichswerte"):
#   - Uebungs-/Testdateien (Testset) fallen raus
#   - gleiche Datei (md5) nur einmal; Dateien, die ganz in einer anderen
#     stecken, ebenfalls nicht
#   - ein Kind je Klasse und Jahr nur einmal: es gewinnt die Zeile MIT Werten,
#     bei zwei Messungen die spaetere
#   - Klassen mit weniger als 10 Kindern mit Werten kommen nicht in die
#     Klassen-Verteilung
#   - Entwicklung nur fuer Paare mit Jahr_alt < Jahr_neu; Namen werden mit
#     derselben Zuordnung verglichen wie im Entwicklungsbrief (build_cohort:
#     Schreibvarianten werden erkannt, mehrdeutige Namen bleiben unzugeordnet)

vergleich_erzeugen <- function(ordner_daten,
                               schulart = "nicht angegeben",
                               schulart_erhebung = "Herbst (Klasse 5 und 6)",
                               verfahren = "C-Test, 2 Texte a 20 Luecken",
                               min_kinder_klasse = 10) {

  if (missing(ordner_daten) || is.null(ordner_daten) || !nzchar(ordner_daten)) {
    ordner_daten <- benutzer_ausgabeordner()
  }
  if (!dir.exists(ordner_daten)) {
    stop("Datenordner nicht gefunden: ", ordner_daten, call. = FALSE)
  }

  q <- function(x, p) as.numeric(stats::quantile(x, p, na.rm = TRUE, names = FALSE))
  punkte <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  spalten <- paste0("p", c(10, 25, 50, 75, 90))
  grenzen_zeile <- function(x) {
    if (all(is.na(x)) || length(x) == 0) return(rep(NA_real_, length(punkte)))
    q(x, punkte)
  }

  #### 1) Dateien einlesen ####
  dateien_roh <- list.files(ordner_daten, pattern = "\\.tsv$", recursive = TRUE,
                            full.names = TRUE)
  dateien <- dateien_roh[!grepl("Testset", dateien_roh, ignore.case = TRUE)]
  dateien <- dateien[!duplicated(tools::md5sum(dateien))]
  if (length(dateien) == 0) {
    stop("Keine tsv-Dateien in ", ordner_daten, " gefunden.", call. = FALSE)
  }

  # Gelesen wird mit der Funktion der App (loadData): Spaltenzuordnung, Umrechnung
  # und Bereichspruefung sind damit genau dieselben wie in der Auswertung.
  lese <- function(p) {
    d <- tryCatch(loadData(list(datapath = p, name = basename(p))),
                  error = function(e) NULL)
    if (is.null(d) || nrow(d) == 0) return(NULL)
    klasse <- as.character(d$Klasse)
    # Einzelne Dateien haben Zeilen ohne Klassenangabe. Fuer die Vergleichswerte
    # zaehlen sie zur Klasse aus dem Dateinamen (..._5c.tsv); sonst wuerden die
    # Werte dieser Kinder fehlen.
    from_name <- sub("^.*_([0-9][a-z]+)\\.tsv$", "\\1", basename(p))
    if (grepl("^[0-9][a-z]+$", from_name)) {
      ersetzen <- is.na(klasse) | !nzchar(trimws(klasse))
      klasse[ersetzen] <- from_name
    }
    # Jahr aus dem Datum der Datei (nicht aus dem Ordnernamen - die Ordner
    # koennen Dateien anderer Jahre enthalten)
    jahr <- regmatches(basename(p), regexpr("20[0-9]{2}", basename(p)))
    data.frame(Name = as.character(d$Name),
               Klasse = klasse,
               Jahr = if (length(jahr) == 1) as.integer(jahr) else NA_integer_,
               RF = suppressWarnings(as.numeric(d[["R/F-%"]])),
               WE = suppressWarnings(as.numeric(d[["WE-%"]])),
               Datei = basename(p), stringsAsFactors = FALSE)
  }

  teile <- lapply(dateien, lese)
  alle <- do.call(rbind, teile[!vapply(teile, is.null, logical(1))])
  if (is.null(alle) || nrow(alle) == 0) {
    stop("In den tsv-Dateien wurden keine Werte gefunden.", call. = FALSE)
  }

  #### 2) Datei-Dubletten (Inhalt steckt in einer anderen Datei) ####
  schluessel <- function(x) paste(x$Klasse, x$RF, x$WE, sep = "|")
  mengen <- split(schluessel(alle), alle$Datei)
  behalten <- names(mengen)
  for (a in names(mengen)) {
    if (!a %in% behalten) next
    for (b in names(mengen)) {
      if (a == b || !b %in% behalten) next
      ka <- table(mengen[[a]]); kb <- table(mengen[[b]])
      if (sum(ka) > sum(kb)) next
      keys <- union(names(ka), names(kb))
      va <- numeric(length(keys)); vb <- numeric(length(keys))
      va[match(names(ka), keys)] <- as.numeric(ka)
      vb[match(names(kb), keys)] <- as.numeric(kb)
      if (all(va == vb)) behalten <- setdiff(behalten, b)
      else if (all(va <= vb)) { behalten <- setdiff(behalten, a); break }
    }
  }
  alle <- alle[alle$Datei %in% behalten, , drop = FALSE]

  #### 3) Ein Kind je Klasse und Jahr: Zeile mit Werten, sonst die spaetere ####
  hat_wert <- !is.na(alle$RF) | !is.na(alle$WE)
  alle$Schluessel <- paste(alle$Name, alle$Klasse, alle$Jahr)
  alle <- alle[order(alle$Schluessel, !hat_wert, alle$Jahr), , drop = FALSE]
  alle <- alle[!duplicated(alle$Schluessel), , drop = FALSE]
  alle$Stufe <- suppressWarnings(as.numeric(gsub("[^0-9]", "", alle$Klasse)))
  alle <- alle[!is.na(alle$Stufe), , drop = FALSE]

  #### 4) Die drei Blaetter ####
  stufen <- sort(unique(alle$Stufe))
  kinder <- do.call(rbind, lapply(stufen, function(st) {
    d <- alle[alle$Stufe == st, , drop = FALSE]
    do.call(rbind, lapply(c("RF", "WE"), function(k) {
      werte <- d[[k]][!is.na(d[[k]])]
      data.frame(Klassenstufe = as.character(st),
                 Kennzahl = if (k == "RF") "R/F" else "WE",
                 n = length(werte), t(grenzen_zeile(werte)), check.names = FALSE,
                 row.names = NULL)
    }))
  }))
  names(kinder)[4:8] <- spalten

  klassen <- do.call(rbind, lapply(stufen, function(st) {
    d <- alle[alle$Stufe == st & (!is.na(alle$RF) | !is.na(alle$WE)), , drop = FALSE]
    gruppen <- split(d, paste(d$Klasse, d$Jahr))
    gruppen <- gruppen[vapply(gruppen, function(g) sum(!is.na(g$RF)), integer(1)) >=
                         min_kinder_klasse]
    if (length(gruppen) == 0) return(NULL)
    lage_masse <- c(Mittel = "mean", Median = "median")
    do.call(rbind, lapply(c("RF", "WE"), function(k) {
      do.call(rbind, lapply(names(lage_masse), function(name) {
        f <- lage_masse[[name]]
        lage <- vapply(gruppen, function(g) {
          werte <- g[[k]][!is.na(g[[k]])]
          if (length(werte) == 0) return(NA_real_)
          if (f == "mean") mean(werte) else stats::median(werte)
        }, numeric(1))
        lage <- lage[!is.na(lage)]
        data.frame(Klassenstufe = as.character(st),
                   Kennzahl = if (k == "RF") "R/F" else "WE",
                   Lagemass = name, n_Klassen = length(lage),
                   t(grenzen_zeile(lage)), check.names = FALSE, row.names = NULL)
      }))
    }))
  }))
  names(klassen)[5:9] <- spalten

  # Entwicklung 5 -> 6: dieselbe Zuordnung wie im Entwicklungsbrief. Der Brief
  # vergleicht genau zwei Kohorten (z. B. 5c -> 6c); hier wird genauso je
  # Jahrgangspaar und Buchstabe aufgerufen - und dem naechsten Jahr, in dem es
  # eine 6er-Messung gibt. So bekommt build_cohort immer nur eine Handvoll
  # Kinder statt der ganzen Schule (Namensvergleiche wachsen quadratisch).
  # Gerechnet wird ausschliesslich mit App-Funktionen (build_cohort,
  # cohort_gematcht); "5->6" ist der Schluessel, den die App liest.
  stufe_paar <- "5->6"
  paare_bilden <- function() {
    teil <- alle[alle$Stufe %in% c(5, 6) & !is.na(alle$Jahr), , drop = FALSE]
    if (!all(c(5, 6) %in% teil$Stufe)) return(NULL)
    teil$B <- tolower(gsub("[^A-Za-z]", "", teil$Klasse))
    jahre_alt <- sort(unique(teil$Jahr[teil$Stufe == 5]))
    jahre_neu <- sort(unique(teil$Jahr[teil$Stufe == 6]))
    gefunden <- list()
    for (ja in jahre_alt) {
      # naechstes Jahr mit einer 6er-Messung (eine Klasse kann ein Jahr
      # ueberspringen); ohne Nachfolger gibt es fuer diese Kinder keine
      # Entwicklung
      jn <- jahre_neu[jahre_neu > ja]
      if (length(jn) == 0) next
      jn <- jn[1]
      for (b in sort(unique(teil$B[teil$Stufe == 5 & teil$Jahr == ja]))) {
        d <- teil[(teil$Stufe == 5 & teil$Jahr == ja) |
                    (teil$Stufe == 6 & teil$Jahr == jn), , drop = FALSE]
        d <- d[d$B == b, , drop = FALSE]
        if (!all(c(5, 6) %in% d$Stufe)) next
        k <- tryCatch(build_cohort(data.frame(Name = d$Name, Klasse = d$Klasse,
                                              `WE-%` = d$WE, `R/F-%` = d$RF,
                                              check.names = FALSE), 5, 6),
                      error = function(e) NULL)
        if (is.null(k)) next
        p <- tryCatch(cohort_gematcht(k), error = function(e) NULL)
        if (is.null(p) || nrow(p) == 0) next
        p$Jahr_Alt <- ja
        p$Jahr_Neu <- jn
        p$Kohorte_B <- b
        gefunden[[length(gefunden) + 1]] <- p
      }
    }
    if (length(gefunden) == 0) return(NULL)
    p <- do.call(rbind, gefunden)
    p[!duplicated(paste(p$Name_Alt, p$Klasse_Alt)), , drop = FALSE]
  }
  paare <- paare_bilden()
  if (!is.null(paare)) {
    cat("Zuordnung wie im Entwicklungsbrief (je Kohorte und Jahrgangspaar):",
        nrow(paare), "Kinder mit zwei Messungen\n")
  }
  # Entwicklung: Quantile (Kontext) + die Kennzahlen der startkorrigierten
  # Erwartung. Erwartete Entwicklung = Erwartung_a + Erwartung_b * Startwert
  # (Startwert = Mittel der 5. Klasse). Der Rest_SD gehoert zum Rangintervall
  # der Kohorte (Rest_SD/wurzel(n)), der Kohorten_SD zum Band
  # ("ueber/unter dem Ueblichen" ab einer Standardabweichung).
  entwicklung <- do.call(rbind, lapply(c("RF", "WE"), function(k) {
    start <- if (is.null(paare)) numeric(0) else paare[[paste0(k, "_Alt")]]
    ziel <- if (is.null(paare)) numeric(0) else paare[[paste0(k, "_Neu")]]
    delta <- ziel - start
    ok <- !is.na(start) & !is.na(delta)
    a <- NA_real_; b <- NA_real_; rest_sd <- NA_real_
    if (sum(ok) >= 10) {
      fit <- stats::lm(delta[ok] ~ start[ok])
      a <- unname(stats::coef(fit)[1]); b <- unname(stats::coef(fit)[2])
      rest_sd <- stats::sd(stats::resid(fit))
    }
    # Kohortenabweichungen: je Kohorte (Buchstabe x Jahrgangspaar) das Mittel
    if (is.null(paare) || is.na(a) || is.na(b)) {
      abw <- numeric(0)
    } else {
      leiste <- do.call(rbind, lapply(split(paare, paare$Jahr_Alt), function(j) {
        do.call(rbind, lapply(split(j, j$Kohorte_B), function(g) {
          gueltig <- !is.na(g[[paste0(k, "_Alt")]]) & !is.na(g[[paste0(k, "_Neu")]])
          if (sum(gueltig) < 5) return(NULL)
          data.frame(
            Start = mean(g[[paste0(k, "_Alt")]][gueltig]),
            Delta = mean((g[[paste0(k, "_Neu")]] - g[[paste0(k, "_Alt")]])[gueltig]),
            n = sum(gueltig))
        }))
      }))
      abw <- if (is.null(leiste)) numeric(0) else
        leiste$Delta - (a + b * leiste$Start)
    }
    delta <- delta[!is.na(delta)]
    daten <- data.frame(Klassenstufe = stufe_paar,
                        Kennzahl = if (k == "RF") "R/F" else "WE",
                        n = length(delta), t(grenzen_zeile(delta)),
                        Erwartung_a = a, Erwartung_b = b, Rest_SD = rest_sd,
                        Kohorten_SD = if (length(abw) >= 4) stats::sd(abw) else NA_real_,
                        n_Kohorten = length(abw),
                        check.names = FALSE, row.names = NULL)
    daten
  }))
  names(entwicklung)[4:8] <- spalten
  cat("\nEntwicklung: Erwartung und Streuung\n")
  print(entwicklung[, c("Kennzahl", "n", "Erwartung_a", "Erwartung_b", "Rest_SD",
                        "Kohorten_SD", "n_Kohorten")], row.names = FALSE, digits = 3)

  #### 5) Metadaten ####
  version <- tryCatch({
    z <- grep("APP_VERSION", readLines("app.R", warn = FALSE), value = TRUE)
    if (length(z) > 0) gsub('^.*"([^"]+)".*$', "\\1", z[1]) else "?"
  }, error = function(e) "?")

  info <- data.frame(
    Feld = c("Bezugsgruppe", "Zeitraum von", "Zeitraum bis", "Schulart",
             "Klassenstufen", "Anzahl Klassen", "Kinder mit Werten",
             "Verfahren", "Stand der Erhebung", "Erzeugt am", "Erzeugt von",
             "Bandregeln Kinder", "Bandregeln Klassen", "Bandregeln Entwicklung",
             "Mindestgroessen", "Hinweis"),
    Wert = c("diese Schule",
             as.character(min(alle$Jahr, na.rm = TRUE)),
             as.character(max(alle$Jahr, na.rm = TRUE)),
             schulart,
             paste(stufen, collapse = ", "),
             as.character(length(unique(paste(alle$Klasse, alle$Jahr)))),
             as.character(sum(!is.na(alle$RF) | !is.na(alle$WE))),
             verfahren,
             schulart_erhebung,
             format(Sys.Date(), "%d.%m.%Y"),
             paste0("werkzeuge/vergleich_erzeugen.R (C-Test Auswertung ", version, ")"),
             "untere 10 % | im Jahrgangsbereich | obere 10 % (p10/p90)",
             "unteres Viertel | Mittelfeld | oberes Viertel (p25/p75)",
             paste0("Erwartung = Erwartung_a + Erwartung_b * Startniveau; Band = +/-1 ",
                    "Kohorten_SD; im ueblichen Bereich / leicht / deutlich ueber oder ",
                    "unter dem Ueblichen (Rangintervall +/- Rest_SD/wurzel(n))"),
             paste0("Kinder ab 30, Klassen ab 8 Klassengruppen, Klassen ab ",
                    min_kinder_klasse, " Kindern mit Werten; Kohorten-SD ab 4 Kohorten"),
             "Innenansicht der eigenen Schule, keine Norm. Keine Namen, keine Einzelwerte."),
    stringsAsFactors = FALSE)

  #### 6) Schreiben####
  ziel_ordner <- vorlagen_ordner(anlegen = TRUE)
  if (is.null(ziel_ordner)) stop("Vorlagenordner konnte nicht angelegt werden.", call. = FALSE)
  ziel <- file.path(ziel_ordner, .vergleich_dateiname)

  wb <- openxlsx::createWorkbook()
  for (blatt in c("Info", "Kinder", "Klassen", "Entwicklung")) {
    openxlsx::addWorksheet(wb, blatt)
    inhalt <- switch(blatt, Info = info, Kinder = kinder, Klassen = klassen,
                     Entwicklung = entwicklung)
    if (!is.null(inhalt)) openxlsx::writeData(wb, blatt, inhalt)
    openxlsx::setColWidths(wb, blatt, cols = 1:9, widths = "auto")
    openxlsx::freezePane(wb, blatt, firstRow = TRUE)
  }

  cat("Kinder:\n"); print(kinder)
  cat("\nKlassen:\n"); print(klassen)
  cat("\nEntwicklung:\n"); print(entwicklung)

  # openxlsx meldet einen Schreibfehler (Datei z. B. in Excel geoeffnet) nicht
  # immer als Fehler - deshalb wird danach geprueft, ob die Datei neu ist.
  vorher <- if (file.exists(ziel)) file.info(ziel)$mtime else NA
  tryCatch(openxlsx::saveWorkbook(wb, ziel, overwrite = TRUE), error = function(e) NULL)
  nachher <- if (file.exists(ziel)) file.info(ziel)$mtime else NA
  if (!file.exists(ziel) ||
      (!is.na(vorher) && identical(as.numeric(vorher), as.numeric(nachher)))) {
    stop("Die Datei konnte nicht (neu) geschrieben werden: ", ziel,
         "\n  Ist sie gerade in Excel geoeffnet? Dann schliessen und erneut ",
         "starten. Eine vorhandene Datei bleibt unveraendert gueltig.",
         call. = FALSE)
  }

  cat("\nDatei geschrieben:", ziel, "\n")
  invisible(ziel)
}

# Beim Aufruf ueber Rscript: App laden, Argumente auswerten und starten. Beim
# Sourcen (z. B. aus einem lokalen Skript) passiert nichts.
if (sys.nframe() == 0) {
  if (!file.exists("global.R")) {
    stop("Bitte aus dem Projektordner der App starten (dort liegt global.R).",
         call. = FALSE)
  }
  source("global.R", local = FALSE)

  argumente <- commandArgs(trailingOnly = TRUE)
  ordner <- if (length(argumente) >= 1 && nzchar(argumente[1])) argumente[1] else NULL
  schulart <- if (length(argumente) >= 2 && nzchar(argumente[2])) argumente[2] else
    "nicht angegeben"
  if (is.null(ordner)) {
    vergleich_erzeugen(schulart = schulart)
  } else {
    vergleich_erzeugen(ordner_daten = ordner, schulart = schulart)
  }
}
