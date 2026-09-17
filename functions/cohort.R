# functions/cohort.R
#
# Jahrgangsuebergreifender Vergleich (Kohorten) - Grundlage fuer den
# Lehrkraefte-Infobrief UND fuer das Entwicklungsdiagramm. Beide nutzen
# dieselbe Zuordnung ("eine Wahrheit").
#
# Fachliche Regeln (abgestimmt):
#  - Verglichen werden zwei Stufen (z. B. 5 -> 6). Die hoehere Stufe ist der
#    aktuellere Messzeitpunkt.
#  - Zugeordnet wird ueber den Namen. Es wird NICHT vorausgesetzt, dass Namen
#    als "Nachname, Vorname" geschrieben sind: verglichen werden Token-Mengen
#    (Reihenfolge, Satzzeichen und fehlende Zweitnamen sind damit egal).
#  - Jedes Kind wird hoechstens einmal zugeordnet. Ist die Aehnlichkeit nicht
#    eindeutig genug, entscheidet der Mensch in der Oberflaeche (Ja/Nein).
#  - Namensgleiche Kinder innerhalb einer Stufe gelten als mehrdeutig und
#    werden nicht automatisch zugeordnet.
#  - Die Zuordnung wird als Datei gespeichert und beim naechsten Lauf wieder
#    angewendet (Dateiname nach den geladenen tsv-Dateien).

# Schwellen der Aehnlichkeit
.cohort_auto_ab <- 0.90      # ab hier automatisch zuordnen
.cohort_vorschlag_ab <- 0.75 # ab hier Vorschlag zur Bestaetigung

#### Namen vergleichen ####

# Name in vergleichbare Tokens zerlegen: Kleinschreibung, Umlaute auffalten,
# alles ausser Buchstaben/Ziffern trennen, Tokens sortieren.
name_tokens <- function(x) {
  if (length(x) != 1 || is.na(x) || !nzchar(trimws(x))) return(character(0))
  x <- tolower(x)
  for (paar in list(c("\u00e4", "ae"), c("\u00f6", "oe"), c("\u00fc", "ue"),
                    c("\u00df", "ss"), c("\u00e0", "a"), c("\u00e9", "e"))) {
    x <- gsub(paar[1], paar[2], x, fixed = TRUE)
  }
  x <- gsub("[^a-z0-9]+", " ", x)
  tokens <- unlist(strsplit(trimws(x), "\\s+"), use.names = FALSE)
  sort(unique(tokens[nzchar(tokens)]))
}

normalisiere_name <- function(x) paste(name_tokens(x), collapse = " ")

# Aehnlichkeit zweier Namen in 0..1 (0 = verschieden, 1 = gleich).
# 60 % Token-Abdeckung (jeder Token zaehlt ab Jaro-Winkler 0.80 als Treffer),
# 40 % Aehnlichkeit der gesamten Zeichenkette.
name_aehnlichkeit <- function(a, b) {
  ta <- name_tokens(a)
  tb <- name_tokens(b)
  if (length(ta) == 0 || length(tb) == 0) return(0)

  jw <- function(x, y) 1 - stringdist::stringdist(x, y, method = "jw", p = 0.1)
  abdeckung <- function(x, y) {
    werte <- vapply(x, function(t) {
      max(vapply(y, function(u) jw(t, u), numeric(1)))
    }, numeric(1))
    mean(werte >= 0.80)
  }

  token_score <- (abdeckung(ta, tb) + abdeckung(tb, ta)) / 2
  ganz <- jw(paste(ta, collapse = " "), paste(tb, collapse = " "))
  0.6 * token_score + 0.4 * ganz
}

#### Zuordnung ####

# Arbeitskopie der Schuelerdaten mit einheitlichen Spaltennamen
.cohort_daten <- function(df) {
  tibble::tibble(
    Name = as.character(df$Name),
    Klasse = as.character(df$Klasse),
    WE = as.numeric(df$`WE-%`),
    RF = as.numeric(df$`R/F-%`)
  ) %>%
    mutate(Stufe = suppressWarnings(as.numeric(gsub("[^0-9]", "", Klasse))),
           Buchstabe = tolower(gsub("[^A-Za-z]", "", Klasse)))
}

# Zuordnung zweier Stufen berechnen.
#
# entscheidungen: data.frame mit Name_Alt, Klasse_Alt, Name_Neu, Klasse_Neu,
#                 Aktion ("ja" = zusammenfuehren, "nein" = trennen)
build_cohort <- function(df, stufe_alt, stufe_neu, entscheidungen = NULL,
                         auto_ab = .cohort_auto_ab, vorschlag_ab = .cohort_vorschlag_ab) {
  daten <- .cohort_daten(df)
  stufen <- sort(unique(daten$Stufe[!is.na(daten$Stufe)]))

  if (is.null(stufe_alt) || is.null(stufe_neu) || !nzchar(as.character(stufe_alt)) ||
      !nzchar(as.character(stufe_neu))) {
    stop("Bitte beide Stufen fuer den Vergleich auswaehlen.", call. = FALSE)
  }
  stufe_alt <- as.numeric(stufe_alt)
  stufe_neu <- as.numeric(stufe_neu)
  if (identical(stufe_alt, stufe_neu)) {
    stop("Gleiche Stufe kann nicht als Entwicklung verglichen werden.", call. = FALSE)
  }
  if (!all(c(stufe_alt, stufe_neu) %in% stufen)) {
    stop("Die gewaehlten Stufen sind in den geladenen Daten nicht vorhanden.",
         call. = FALSE)
  }

  alt <- daten[daten$Stufe == stufe_alt, , drop = FALSE]
  neu <- daten[daten$Stufe == stufe_neu, , drop = FALSE]

  # namensgleiche Kinder innerhalb einer Stufe: nicht automatisch zuordnen
  namensgleiche <- function(x) {
    schluessel <- vapply(x$Name, normalisiere_name, character(1))
    unique(x$Name[duplicated(schluessel) | duplicated(schluessel, fromLast = TRUE)])
  }
  mehrdeutig <- unique(c(namensgleiche(alt), namensgleiche(neu)))
  alt_frei <- alt[!(alt$Name %in% mehrdeutig), , drop = FALSE]
  neu_frei <- neu[!(neu$Name %in% mehrdeutig), , drop = FALSE]

  # alle Paare bewerten
  paare <- expand.grid(i = seq_len(nrow(alt_frei)), j = seq_len(nrow(neu_frei)))
  if (nrow(paare) > 0) {
    paare$Aehnlichkeit <- mapply(function(i, j) {
      name_aehnlichkeit(alt_frei$Name[i], neu_frei$Name[j])
    }, paare$i, paare$j)
    paare$gleicher_buchstabe <- mapply(function(i, j) {
      isTRUE(alt_frei$Buchstabe[i] == neu_frei$Buchstabe[j]) &&
        nzchar(alt_frei$Buchstabe[i])
    }, paare$i, paare$j)
    # absteigend nach Aehnlichkeit, bei Gleichstand gleicher Buchstabe zuerst
    paare <- paare[order(-paare$Aehnlichkeit, -paare$gleicher_buchstabe), , drop = FALSE]

    # 1:1 zuordnen
    vergeben_alt <- logical(nrow(alt_frei))
    vergeben_neu <- logical(nrow(neu_frei))
    zuordnung <- list()
    for (k in seq_len(nrow(paare))) {
      i <- paare$i[k]; j <- paare$j[k]
      if (vergeben_alt[i] || vergeben_neu[j]) next
      if (paare$Aehnlichkeit[k] < vorschlag_ab) break
      vergeben_alt[i] <- TRUE
      vergeben_neu[j] <- TRUE
      zuordnung[[length(zuordnung) + 1]] <- list(
        i = i, j = j,
        Aehnlichkeit = paare$Aehnlichkeit[k],
        Status = if (paare$Aehnlichkeit[k] >= auto_ab) "auto" else "vorschlag"
      )
    }
  } else {
    vergeben_alt <- logical(nrow(alt_frei))
    vergeben_neu <- logical(nrow(neu_frei))
    zuordnung <- list()
  }

  # Entscheidungen anwenden
  if (!is.null(entscheidungen) && nrow(entscheidungen) > 0) {
    finde_entscheidung <- function(name_alt, name_neu) {
      treffer <- entscheidungen[
        entscheidungen$Name_Alt == name_alt & entscheidungen$Name_Neu == name_neu, ,
        drop = FALSE]
      if (nrow(treffer) == 0) NA_character_ else utils::tail(treffer$Aktion, 1)
    }
    behalten <- list()
    for (z in zuordnung) {
      aktion <- finde_entscheidung(alt_frei$Name[z$i], neu_frei$Name[z$j])
      if (identical(aktion, "nein")) next
      if (identical(aktion, "ja")) z$Status <- "bestaetigt"
      behalten[[length(behalten) + 1]] <- z
    }
    # durch "nein" getrennte Kinder wieder als ohne Partner fuehren
    zuordnung <- behalten
    vergeben_alt <- logical(nrow(alt_frei))
    vergeben_neu <- logical(nrow(neu_frei))
    for (z in zuordnung) {
      vergeben_alt[z$i] <- TRUE
      vergeben_neu[z$j] <- TRUE
    }
  }

  # Ergebnistabelle aufbauen
  zeile <- function(name_alt = NA_character_, klasse_alt = NA_character_,
                    we_alt = NA_real_, rf_alt = NA_real_,
                    name_neu = NA_character_, klasse_neu = NA_character_,
                    we_neu = NA_real_, rf_neu = NA_real_,
                    aehnlichkeit = NA_real_, status = "kein_partner") {
    tibble::tibble(Name_Alt = name_alt, Klasse_Alt = klasse_alt,
                   WE_Alt = we_alt, RF_Alt = rf_alt,
                   Name_Neu = name_neu, Klasse_Neu = klasse_neu,
                   WE_Neu = we_neu, RF_Neu = rf_neu,
                   dWE = we_neu - we_alt, dRF = rf_neu - rf_alt,
                   Aehnlichkeit = aehnlichkeit, Status = status)
  }

  ergebnis <- list()
  for (z in zuordnung) {
    i <- z$i; j <- z$j
    ergebnis[[length(ergebnis) + 1]] <- zeile(
      alt_frei$Name[i], alt_frei$Klasse[i], alt_frei$WE[i], alt_frei$RF[i],
      neu_frei$Name[j], neu_frei$Klasse[j], neu_frei$WE[j], neu_frei$RF[j],
      z$Aehnlichkeit, z$Status)
  }
  for (i in which(!vergeben_alt)) {
    ergebnis[[length(ergebnis) + 1]] <- zeile(
      alt_frei$Name[i], alt_frei$Klasse[i], alt_frei$WE[i], alt_frei$RF[i])
  }
  for (j in which(!vergeben_neu)) {
    ergebnis[[length(ergebnis) + 1]] <- zeile(
      name_neu = neu_frei$Name[j], klasse_neu = neu_frei$Klasse[j],
      we_neu = neu_frei$WE[j], rf_neu = neu_frei$RF[j],
      status = "nur_neu")
  }

  # namensgleiche Kinder sichtbar machen (ohne Zuordnung)
  for (nm in unique(alt$Name[alt$Name %in% mehrdeutig])) {
    i <- which(alt$Name == nm)[1]
    ergebnis[[length(ergebnis) + 1]] <- zeile(
      alt$Name[i], alt$Klasse[i], alt$WE[i], alt$RF[i], status = "mehrdeutig")
  }
  for (nm in unique(neu$Name[neu$Name %in% mehrdeutig])) {
    j <- which(neu$Name == nm)[1]
    ergebnis[[length(ergebnis) + 1]] <- zeile(
      name_neu = neu$Name[j], klasse_neu = neu$Klasse[j],
      we_neu = neu$WE[j], rf_neu = neu$RF[j], status = "mehrdeutig")
  }

  paare_tbl <- if (length(ergebnis) > 0) dplyr::bind_rows(ergebnis) else zeile()[0, ]

  structure(list(
    paare = paare_tbl,
    stufe_alt = stufe_alt,
    stufe_neu = stufe_neu,
    klassen_alt = sort(unique(alt$Klasse)),
    klassen_neu = sort(unique(neu$Klasse)),
    n_alt = nrow(alt),
    n_neu = nrow(neu),
    n_alt_werte = sum(!is.na(alt$WE) | !is.na(alt$RF)),
    n_neu_werte = sum(!is.na(neu$WE) | !is.na(neu$RF)),
    mehrdeutig = mehrdeutig
  ), class = "cohort")
}

# die fuer die Auswertung gueltigen Paare (automatisch oder bestaetigt)
cohort_gematcht <- function(x) {
  stopifnot(inherits(x, "cohort"))
  x$paare[x$paare$Status %in% c("auto", "bestaetigt") &
             !is.na(x$paare$Name_Alt) & !is.na(x$paare$Name_Neu), , drop = FALSE]
}

# offene Vorschlaege (muessen bestaetigt oder getrennt werden)
cohort_vorschlaege <- function(x) {
  stopifnot(inherits(x, "cohort"))
  x$paare[x$paare$Status == "vorschlag", , drop = FALSE]
}

# Bewertung als Text fuer die Oberflaeche
cohort_status_text <- function(status) {
  texte <- c(auto = "automatisch zugeordnet",
             bestaetigt = "bestätigt",
             vorschlag = "Vorschlag (bitte prüfen)",
             kein_partner = "kein Vorjahreswert",
             nur_neu = "neu in der Klasse",
             mehrdeutig = "nicht eindeutig (nicht zugeordnet)",
             getrennt = "getrennt")
  ergebnis <- unname(texte[as.character(status)])
  fehlend <- is.na(ergebnis)
  ergebnis[fehlend] <- as.character(status[fehlend])
  ergebnis
}

#### Kennzahlen ####

# Mittelwert, Median und Standardabweichung je Stufe und Klasse - nur ueber die
# gematchte Gruppe (fairer Vorjahresvergleich).
cohort_statistik <- function(x) {
  g <- cohort_gematcht(x)
  if (nrow(g) == 0) return(data.frame())

  lang <- dplyr::bind_rows(
    tibble::tibble(Stufe = x$stufe_alt, Klasse = g$Klasse_Alt,
                   WE = g$WE_Alt, RF = g$RF_Alt),
    tibble::tibble(Stufe = x$stufe_neu, Klasse = g$Klasse_Neu,
                   WE = g$WE_Neu, RF = g$RF_Neu)
  )

  zusammenfassen <- function(daten) {
    tibble::tibble(
      n = nrow(daten),
      n_werte = sum(!is.na(daten$WE) | !is.na(daten$RF)),
      mittel_WE = round(mean(daten$WE, na.rm = TRUE), 1),
      median_WE = round(stats::median(daten$WE, na.rm = TRUE), 1),
      sd_WE = round(stats::sd(daten$WE, na.rm = TRUE), 1),
      mittel_RF = round(mean(daten$RF, na.rm = TRUE), 1),
      median_RF = round(stats::median(daten$RF, na.rm = TRUE), 1),
      sd_RF = round(stats::sd(daten$RF, na.rm = TRUE), 1)
    )
  }

  je_klasse <- lang %>%
    dplyr::group_by(.data$Stufe, .data$Klasse) %>%
    dplyr::group_modify(~ zusammenfassen(.x)) %>%
    dplyr::ungroup()
  gesamt <- lang %>%
    dplyr::group_by(.data$Stufe) %>%
    dplyr::group_modify(~ zusammenfassen(.x)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Klasse = "gesamt", .after = "Stufe")

  as.data.frame(dplyr::bind_rows(
    je_klasse %>% dplyr::mutate(Klasse = .data$Klasse),
    gesamt
  ) %>% dplyr::arrange(.data$Stufe, .data$Klasse))
}

# Wer liegt im aktuellen Jahr unter dem Referenzwert - und wie hat sich das
# gegenueber dem Vorjahr entwickelt?
unter_referenz <- function(x, grenze = .normgrenze("R/F")) {
  g <- cohort_gematcht(x)
  g <- g[!is.na(g$RF_Alt) & !is.na(g$RF_Neu), , drop = FALSE]

  vorher <- g$RF_Alt < grenze
  nachher <- g$RF_Neu < grenze
  namen <- function(idx) g$Name_Neu[idx]

  list(
    grenze = grenze,
    n = nrow(g),
    vorher_unter = sum(vorher),
    nachher_unter = sum(nachher),
    unveraendert_kritisch = sum(vorher & nachher),
    neu_kritisch = sum(!vorher & nachher),
    nicht_mehr_kritisch = sum(vorher & !nachher),
    nie_kritisch = sum(!vorher & !nachher),
    namen_unveraendert = namen(vorher & nachher),
    namen_neu = namen(!vorher & nachher),
    namen_verbessert = namen(vorher & !nachher)
  )
}

# Groesste Verbesserungen und deutliche Rueckgaenge (gemessen am R/F-Wert,
# WE-Wert wird mitberichtet).
cohort_ranking <- function(x, top = 3, rueckgang = 10) {
  g <- cohort_gematcht(x)
  g <- g[!is.na(g$dRF), , drop = FALSE]

  if (nrow(g) == 0) {
    return(list(verbesserungen = g, rueckgaenge = g))
  }

  g <- g[order(-g$dRF), , drop = FALSE]
  list(
    verbesserungen = utils::head(g[g$dRF > 0, , drop = FALSE], top),
    rueckgaenge = g[g$dRF <= -abs(rueckgang), , drop = FALSE]
  )
}

#### Zuordnung speichern und wieder anwenden ####

# Pruefsumme der Kohorte: erkennt, ob eine gespeicherte Zuordnung zu den
# aktuell geladenen Kindern gehoert (z. B. gleiches Stufenpaar im naechsten
# Jahr). Kurz gehalten, damit sie in der Datei lesbar bleibt. Das "k" davor
# sorgt dafuer, dass die Pruefsumme beim Einlesen immer als Text gilt.
kohorte_pruefsumme <- function(cohort) {
  namen <- c(cohort$paare$Name_Alt, cohort$paare$Name_Neu)
  schluessel <- sort(unique(vapply(namen[!is.na(namen)], normalisiere_name,
                                   character(1))))
  paste0("k", digest::digest(paste(schluessel, collapse = "|"), algo = "crc32"))
}

# Dateiname der Zuordnungsdatei aus den DATEN ableiten (Klassen der beiden
# Stufen) - unabhaengig davon, ob die Daten getippt oder geladen wurden und
# unabhaengig von Dateinamen.
zuordnung_dateiname <- function(stufe_alt = NULL, stufe_neu = NULL,
                                klassen_alt = NULL, klassen_neu = NULL,
                                dateien = NULL) {
  kurz <- function(x) {
    x <- as.character(x)
    x <- x[!is.na(x) & nzchar(x)]
    if (length(x) == 0) return("unbekannt")
    gsub("[^A-Za-z0-9]+", "_", paste(sort(unique(x)), collapse = "_"))
  }
  paste0("zuordnung_", kurz(klassen_alt), "-", kurz(klassen_neu), ".tsv")
}

entscheidungen_pfad <- function(cohort) {
  .pfad_nativ(file.path(createFilePath(NULL, ""),
                        zuordnung_dateiname(stufe_alt = cohort$stufe_alt,
                                            stufe_neu = cohort$stufe_neu,
                                            klassen_alt = cohort$klassen_alt,
                                            klassen_neu = cohort$klassen_neu)))
}

# Entscheidungen laden (fehlende Datei -> leere Tabelle).
# Neben den Entscheidungen stehen Herkunft (Quelldateien) und Pruefsumme in
# der Datei; die Pruefsumme wird beim Laden verglichen und nur gemeldet.
read_entscheidungen <- function(pfad) {
  leer <- tibble::tibble(Name_Alt = character(0), Klasse_Alt = character(0),
                         Name_Neu = character(0), Klasse_Neu = character(0),
                         Aktion = character(0), Aehnlichkeit = numeric(0),
                         Zeitstempel = character(0), Quelle_Alt = character(0),
                         Quelle_Neu = character(0), Kohorte = character(0))
  if (is.null(pfad) || !fs::file_exists(pfad)) return(leer)
  daten <- tryCatch(
    readr::read_tsv(pfad, show_col_types = FALSE),
    error = function(e) NULL)
  if (is.null(daten) || !all(c("Name_Alt", "Name_Neu", "Aktion") %in% colnames(daten))) {
    message("Zuordnungsdatei konnte nicht gelesen werden: ", pfad)
    return(leer)
  }
  for (spalte in colnames(leer)) {
    if (!spalte %in% colnames(daten)) daten[[spalte]] <- NA
  }
  daten[, colnames(leer), drop = FALSE]
}

# eine Entscheidung ergaenzen/ersetzen und speichern
setze_entscheidung <- function(entscheidungen, pfad, name_alt, klasse_alt,
                               name_neu, klasse_neu, aktion, aehnlichkeit = NA_real_,
                               quelle_alt = NA_character_, quelle_neu = NA_character_,
                               kohorte = NA_character_) {
  neu <- tibble::tibble(Name_Alt = name_alt, Klasse_Alt = klasse_alt,
                        Name_Neu = name_neu, Klasse_Neu = klasse_neu,
                        Aktion = aktion, Aehnlichkeit = aehnlichkeit,
                        Zeitstempel = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                        Quelle_Alt = quelle_alt, Quelle_Neu = quelle_neu,
                        Kohorte = kohorte)
  if (is.null(entscheidungen) || nrow(entscheidungen) == 0) {
    ergebnis <- neu
  } else {
    behalten <- entscheidungen[!(entscheidungen$Name_Alt == name_alt &
                                   entscheidungen$Name_Neu == name_neu), , drop = FALSE]
    # Herkunft/Pruefsumme der bestehenden Eintraege erhalten
    for (spalte in c("Quelle_Alt", "Quelle_Neu", "Kohorte")) {
      if (!spalte %in% colnames(behalten)) behalten[[spalte]] <- NA
    }
    ergebnis <- dplyr::bind_rows(behalten, neu)
  }
  write_entscheidungen(ergebnis, pfad)
  ergebnis
}

write_entscheidungen <- function(entscheidungen, pfad) {
  if (is.null(pfad) || !nzchar(pfad)) return(invisible(FALSE))
  readr::write_tsv(entscheidungen, pfad)
  invisible(TRUE)
}

#### Vergleichstabellen je Kind ####

# Grund fuer eine fehlende Seite als kurzer Text (nur fuer die vollstaendige
# Nachschlageliste). Leer, wenn beide Jahrgaenge zugeordnet sind und Werte haben.
tabellen_hinweis <- function(daten) {
  status <- as.character(daten$Status)
  zugeordnet <- status %in% c("auto", "bestaetigt")

  hinweis <- ifelse(zugeordnet, "", cohort_status_text(status))
  ohne_alt <- is.na(daten$WE_Alt) & is.na(daten$RF_Alt)
  ohne_neu <- is.na(daten$WE_Neu) & is.na(daten$RF_Neu)

  hinweis[zugeordnet & ohne_alt & ohne_neu] <- "nicht teilgenommen (5. und 6. Klasse)"
  hinweis[zugeordnet & ohne_alt & !ohne_neu] <- "nicht teilgenommen (5. Klasse)"
  hinweis[zugeordnet & !ohne_alt & ohne_neu] <- "nicht teilgenommen (6. Klasse)"
  hinweis[is.na(hinweis)] <- ""
  hinweis
}

# Kinderzeilen eines Kohorten-Ergebnisses in die Anzeigeform bringen.
# Vorher/aktuell stehen zusammen in einer Spalte ("45,0 -> 35,0"). Fehlt eine
# Seite, steht dort "-" (z. B. "45,0 -> -"): vorhandene Werte gehen nie
# verloren, die fehlende Seite ist sofort sichtbar.
# mit_klasse = FALSE laesst die Klassenspalte weg (im Infobrief nennt die
# Ueberschrift die Stufen), mit_hinweis = TRUE ergaenzt die Spalte "Hinweis".
cohort_tabelle <- function(daten, mit_klasse = TRUE, mit_hinweis = FALSE,
                           stufe_alt = NULL, stufe_neu = NULL) {
  if (is.null(daten) || nrow(daten) == 0) return(NULL)

  als_zahl <- function(x) ifelse(is.na(x), "-", de_zahl(x))
  spanne <- function(vorher, aktuell) {
    paste0(als_zahl(vorher), " \u2192 ", als_zahl(aktuell))
  }
  # Klasse: das Kind kann nur in einem Jahrgang vorkommen
  klasse_text <- function(alt, neu) {
    ifelse(!is.na(alt) & !is.na(neu), paste0(alt, " \u2192 ", neu),
           ifelse(!is.na(alt), alt, neu))
  }
  # Name: ohne aktuelle Zeile den Namen aus dem Vorjahr nehmen
  name_text <- function(alt, neu) ifelse(!is.na(neu), neu, alt)

  kopf_we <- if (is.null(stufe_alt) || is.null(stufe_neu)) "WE % (vorher \u2192 aktuell)"
    else paste0("WE % (", stufe_alt, " \u2192 ", stufe_neu, ")")
  kopf_rf <- if (is.null(stufe_alt) || is.null(stufe_neu)) "R/F % (vorher \u2192 aktuell)"
    else paste0("R/F % (", stufe_alt, " \u2192 ", stufe_neu, ")")

  tab <- data.frame(Name = name_text(daten$Name_Alt, daten$Name_Neu),
                    check.names = FALSE, stringsAsFactors = FALSE)
  if (mit_klasse) {
    tab[["Klasse"]] <- klasse_text(daten$Klasse_Alt, daten$Klasse_Neu)
  }
  tab[[kopf_we]] <- spanne(daten$WE_Alt, daten$WE_Neu)
  tab[["Δ WE"]] <- daten$dWE
  tab[[kopf_rf]] <- spanne(daten$RF_Alt, daten$RF_Neu)
  tab[["Δ R/F"]] <- daten$dRF
  if (mit_hinweis) tab[["Hinweis"]] <- tabellen_hinweis(daten)
  tab
}

# Verschlankte Uebersicht fuer Statistik-Tab, Infobrief-Anhang und Export.
# Enthaelt ALLE Kinder aus beiden Jahrgaengen mit den vorhandenen Werten -
# auch die, die nur in einem Jahrgang vorkommen (Vergleichbarkeit der Kennzahlen
# stellt cohort_gematcht() her, nicht diese Liste).
# mit_hinweis = FALSE laesst die Spalte "Hinweis" weg (im Word-Anhang ist die
# Seite zu schmal dafuer).
vergleich_tabelle <- function(cohort, mit_hinweis = TRUE) {
  if (is.null(cohort) || !inherits(cohort, "cohort")) return(NULL)
  paare <- cohort$paare
  if (is.null(paare) || nrow(paare) == 0) return(NULL)

  tab <- cohort_tabelle(paare, mit_klasse = TRUE, mit_hinweis = mit_hinweis,
                        stufe_alt = cohort$stufe_alt, stufe_neu = cohort$stufe_neu)
  # alphabetisch nach Namen (Nachschlageliste; in der App zusaetzlich sortierbar)
  tab <- tab[order(tab$Name), , drop = FALSE]
  rownames(tab) <- NULL
  tab
}

# Top-Verbesserungen und die schwaechsten Entwicklungen (je Kind eine Zeile).
# Bei den schwaechsten zuerst die Kinder, die weiterhin unter dem unteren
# Normbereich liegen (dabei "schon im Vorjahr betroffen" vor "neu dazu"),
# erst danach die groessten Verschlechterungen ueber dem Normbereich.
cohort_rangliste <- function(cohort, top = 5, schwach = 5, grenze = .normgrenze("R/F")) {
  if (is.null(cohort) || !inherits(cohort, "cohort")) {
    return(list(verbesserungen = NULL, schwach = NULL))
  }
  g <- cohort_gematcht(cohort)
  g <- g[!is.na(g$dRF), , drop = FALSE]
  if (nrow(g) == 0) return(list(verbesserungen = NULL, schwach = NULL))

  # im Brief ohne Klassenspalte (die Ueberschrift nennt die Stufen)
  als_tabelle <- function(daten) {
    cohort_tabelle(daten, mit_klasse = FALSE,
                   stufe_alt = cohort$stufe_alt, stufe_neu = cohort$stufe_neu)
  }

  verbesserungen <- g[g$dRF > 0, , drop = FALSE]
  verbesserungen <- verbesserungen[order(-verbesserungen$dRF), , drop = FALSE]

  unter <- g[!is.na(g$RF_Neu) & g$RF_Neu < grenze, , drop = FALSE]
  vorjahr <- unter[!is.na(unter$RF_Alt) & unter$RF_Alt < grenze, , drop = FALSE]
  neu_dazu <- unter[!(unter$Name_Neu %in% vorjahr$Name_Neu), , drop = FALSE]
  vorjahr <- vorjahr[order(vorjahr$RF_Neu), , drop = FALSE]
  neu_dazu <- neu_dazu[order(neu_dazu$RF_Neu), , drop = FALSE]
  ueber <- g[!is.na(g$RF_Neu) & g$RF_Neu >= grenze, , drop = FALSE]
  ueber <- ueber[order(ueber$dRF), , drop = FALSE]

  schwach_tbl <- dplyr::bind_rows(vorjahr, neu_dazu, ueber)
  list(verbesserungen = als_tabelle(utils::head(verbesserungen, top)),
       schwach = als_tabelle(utils::head(schwach_tbl, schwach)))
}
