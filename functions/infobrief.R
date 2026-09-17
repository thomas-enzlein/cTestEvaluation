# functions/infobrief.R
#
# Lehrkraefte-Infobrief.
#
# Aufteilung (bewusst so, damit die Formulierungen aenderbar bleiben):
#   - Der Fliesstext steht in infobrief/kopf.Rmd, infobrief/abschnitt.Rmd und
#     infobrief/abschluss.Rmd (Markdown wie beim Elternbrief, mit Inline-R fuer
#     die Zahlen).
#   - Hier in R stehen nur: Zahlen (deutsches Format), Grammatik-Hilfen, die
#     farbigen Wert-Bausteine und die Word-Tabellen.
#   - Pro Klassenbuchstabe wird ein Abschnitt gerendert; die Teile werden zu
#     einer docx zusammengefuegt (Muster wie beim Elternbrief: rendern + merge).

# Zahl im deutschen Format (Dezimalkomma). Fehlende Werte werden einzeln zu "-":
# sonst stuende in einer Tabelle mit teils fehlenden Werten "NA" (z. B. die
# Standardabweichung einer Gruppe mit nur einem Kind mit Werten).
de_zahl <- function(x, digits = 1) {
  if (length(x) == 0) return("-")
  ifelse(is.na(x), "-",
         formatC(x, format = "f", digits = digits, decimal.mark = ","))
}

# Zahl mit Vorzeichen (fuer Veraenderungen)
de_vz <- function(x, digits = 1) {
  if (length(x) == 0) return("-")
  ifelse(is.na(x), "-", paste0(ifelse(x > 0, "+", ""), de_zahl(x, digits)))
}

# "1 Kind" / "3 Kinder"
kind_text <- function(n) {
  ifelse(n == 1, "1 Kind", paste0(n, " Kinder"))
}

# "1 Kind" / "3 Kindern" (Dativ)
kind_dativ <- function(n) {
  ifelse(n == 1, "1 Kind", paste0(n, " Kindern"))
}

# Ab dieser Groesse werden Veraenderungen zusaetzlich fett dargestellt
.infobrief_fett_ab <- 20
.infobrief_gruen <- "#1E7B34"
.infobrief_rot <- "#B00020"

# Aufzaehlung von Kindern: zwischen den Kindern Semikolon, damit das Komma im
# Namen ("Dahlbruch, Colin") eindeutig bleibt.
namen_liste <- function(x) {
  x <- x[!is.na(x) & nzchar(x)]
  paste(sort(x), collapse = "; ")
}

# Ein Wert als farbiger (und ab 20 zusaetzlich fetter) Textbaustein
wert_run <- function(wert, einheit = " %") {
  if (length(wert) == 0 || is.na(wert)) return(officer::ftext("-", officer::fp_text()))
  officer::ftext(paste0(de_vz(wert), einheit),
                 officer::fp_text(color = if (wert > 0) .infobrief_gruen else .infobrief_rot,
                                  bold = abs(wert) >= .infobrief_fett_ab))
}

# Absatz "Titel: Name (+x %), Name (-y %)." mit farbigen Werten.
# (fp_text akzeptiert keine Vektoren, deshalb Run fuer Run.)
liste_absatz <- function(titel, tabelle, einheit = " %", spalte = "\u0394 R/F") {
  if (is.null(tabelle) || nrow(tabelle) == 0) return(NULL)
  runs <- list(officer::ftext(paste0(titel, ": "), officer::fp_text()))
  for (i in seq_len(nrow(tabelle))) {
    if (i > 1) runs <- c(runs, list(officer::ftext(", ", officer::fp_text())))
    runs <- c(runs, list(
      officer::ftext(tabelle$Name[i], officer::fp_text()),
      officer::ftext(" (", officer::fp_text()),
      wert_run(tabelle[[spalte]][i], einheit),
      officer::ftext(")", officer::fp_text())
    ))
  }
  runs <- c(runs, list(officer::ftext(".", officer::fp_text())))
  do.call(officer::fpar, c(runs, list(fp_p = officer::fp_par(padding.top = 4,
                                                             padding.bottom = 4))))
}

# Word-Tabelle im einheitlichen Look.
# - feste Spaltenbreiten (Summe <= 16 cm, sonst laufen die Tabellen aus der
#   A4-Seite heraus: 21 cm - 2 x 2,5 cm Rand = 16 cm Textbreite)
# - Veraenderungsspalten mit Vorzeichen und farbig (fett ab 20)
tabelle_infobrief <- function(df, farb_spalten = character(0), schrift = 8.5,
                              breiten = NULL, vorzeichen_spalten = character(0),
                              farb_zellen = list(), zell_stile = list()) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  anzeige <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  for (spalte in colnames(anzeige)) {
    if (is.numeric(anzeige[[spalte]])) {
      anzeige[[spalte]] <- if (spalte %in% vorzeichen_spalten) {
        de_vz(anzeige[[spalte]])
      } else {
        de_zahl(anzeige[[spalte]])
      }
    }
  }
  ft <- flextable::flextable(anzeige)
  ft <- flextable::theme_booktabs(ft)
  ft <- flextable::fontsize(ft, size = schrift, part = "all")
  ft <- flextable::bold(ft, part = "header")
  for (spalte in farb_spalten) {
    if (!spalte %in% colnames(df)) next
    werte <- suppressWarnings(as.numeric(df[[spalte]]))
    for (i in seq_along(werte)) {
      if (is.na(werte[i])) next
      ft <- flextable::color(ft, i = i, j = spalte,
                             color = if (werte[i] > 0) .infobrief_gruen else .infobrief_rot)
      if (abs(werte[i]) >= .infobrief_fett_ab) {
        ft <- flextable::bold(ft, i = i, j = spalte)
      }
    }
  }
  # einzelne Zellen faerben (z. B. die Differenzzeile der Klassentabelle);
  # gleiche Farblogik wie bei den Einzelkindern: gruen aufwaerts, rot abwaerts,
  # ab 20 Prozentpunkten zusaetzlich fett
  for (zelle in farb_zellen) {
    if (is.null(zelle$wert) || length(zelle$wert) == 0 || is.na(zelle$wert)) next
    if (!zelle$spalte %in% colnames(anzeige)) next
    zeile <- zelle$zeile
    if (is.null(zeile) || zeile < 1 || zeile > nrow(anzeige)) next
    ft <- flextable::color(ft, i = zeile, j = zelle$spalte,
                           color = if (zelle$wert > 0) .infobrief_gruen else .infobrief_rot)
    if (abs(zelle$wert) >= .infobrief_fett_ab) {
      ft <- flextable::bold(ft, i = zeile, j = zelle$spalte)
    }
  }
  # beliebige Zellen gestalten: Hintergrund (z. B. die Kategorie wie im
  # Excel-Export und im Word-Dokument), Schriftfarbe und fett.
  # Ohne "spalte" gilt der Stil fuer die ganze Zeile.
  for (stil in zell_stile) {
    spalte <- stil$spalte
    if (!is.null(spalte) && !spalte %in% colnames(anzeige)) next
    zeile <- stil$zeile
    if (is.null(zeile) || length(zeile) == 0 || is.na(zeile) ||
        zeile < 1 || zeile > nrow(anzeige)) next
    if (!is.null(stil$hintergrund) && !is.na(stil$hintergrund)) {
      ft <- flextable::bg(ft, i = zeile, j = spalte, bg = stil$hintergrund)
    }
    if (!is.null(stil$farbe) && !is.na(stil$farbe)) {
      ft <- flextable::color(ft, i = zeile, j = spalte, color = stil$farbe)
    }
    if (isTRUE(stil$fett)) {
      ft <- flextable::bold(ft, i = zeile, j = spalte)
    }
  }
  # Breiten in cm vorgeben, damit die Tabelle in die Seite passt
  if (!is.null(breiten)) {
    stopifnot(length(breiten) == ncol(anzeige))
    ft <- flextable::set_table_properties(ft, layout = "fixed", width = 0)
    ft <- flextable::width(ft, width = breiten / 2.54)
  } else {
    ft <- flextable::autofit(ft)
  }
  ft
}

# Spaltenbreiten je Tabelle (in cm, Summe jeweils <= 16)
.breiten_a <- c(2.0, 2.8, 4.6, 4.6)                                  # 14,0 cm
.breiten_b <- c(5.4, 3.4, 1.8, 3.4, 1.8)                             # 15,8 cm
.breiten_lese <- c(4.2, 2.3, 3.1, 1.6, 3.1, 1.6)                     # 15,9 cm

# Tabelle A: Kennzahlen je Klasse (nur gematchte Gruppe). differenz sind die
# Zahlen der Differenzzeile (c(we=, rf=)); sie wird wie die Werte einzelner
# Kinder farbig gesetzt.
tabelle_a <- function(df, differenz = NULL) {
  farb_zellen <- list()
  if (!is.null(differenz) && !is.null(df) && nrow(df) > 0) {
    farb_zellen <- list(
      list(spalte = "WE % (Mittel \u00b1 SD)", zeile = nrow(df),
           wert = differenz[["we"]]),
      list(spalte = "R/F % (Mittel \u00b1 SD)", zeile = nrow(df),
           wert = differenz[["rf"]]))
  }
  tabelle_infobrief(df, breiten = .breiten_a, farb_zellen = farb_zellen)
}
# Tabelle B: Kinderliste (Top 5 bzw. schwaechste 5), ohne Klassenspalte
tabelle_b <- function(df) {
  tabelle_infobrief(df, farb_spalten = c("\u0394 WE", "\u0394 R/F"),
                    vorzeichen_spalten = c("\u0394 WE", "\u0394 R/F"),
                    breiten = .breiten_b)
}
# Anhang: Vergleich je Kind (alle Kinder, mit Klassenspalte)
tabelle_lese <- function(df) {
  tabelle_infobrief(df, farb_spalten = c("\u0394 WE", "\u0394 R/F"),
                    vorzeichen_spalten = c("\u0394 WE", "\u0394 R/F"),
                    breiten = .breiten_lese)
}

#### Tabellen des Stand-Briefs ####

.breiten_stand <- c(2.6, 3.8, 3.8, 2.9, 2.9)   # 16,0 cm, 5 Spalten
# Gruppe, Querverweis auf die Kategorien, Anzahl, Anteil
.breiten_kat <- c(6.2, 5.0, 2.4, 2.4)           # 16,0 cm, 4 Spalten
.breiten_werte <- c(7.0, 3.0, 3.0, 3.0)         # 16,0 cm, 4 Spalten

# Kategorie-Farbe wie in der App, im Excel-Export und im Word-Dokument
# (cols/lvls stehen in global.R). Nicht teilgenommen ("0") bleibt ohne Farbe.
.kat_farbe <- function(kat) {
  kat <- trimws(as.character(kat))
  if (!exists("lvls", inherits = TRUE) || !exists("cols", inherits = TRUE)) {
    return(rep(NA_character_, length(kat)))
  }
  unname(cols[match(kat, lvls)])
}

# Farbe je Gruppe der Uebersicht - dieselbe Skala wie die Kategorien der Kinder
.stand_gruppen_farben <- c("darkgreen", "#FFA500", "#CD8500", "orangered",
                           NA_character_, NA_character_)

# Mehr Luft in den Tabellenzeilen. flextable::padding() wirkt in dieser
# flextable-Fassung nicht (die Zellraender bleiben im docx bei 0, geprueft),
# deshalb wird die Zeilenhoehe gesetzt: 0.32 Zoll sind rund 23 pt je Zeile.
.tabellen_luft <- function(ft) {
  if (is.null(ft)) return(NULL)
  flextable::height_all(ft, height = 0.32)
}

# Abstand ueber und unter einer Tabelle. Die Vorlage bringt keinen mit, deshalb
# ein kleiner leerer Absatz mit Vorlauf bzw. Nachlauf (optional mit Titelzeile).
.tabellen_abstand <- function(ft, oben = 10, unten = 10, titel = NULL) {
  if (is.null(ft)) return(NULL)
  leer <- function(...) {
    officer::fpar(officer::ftext(" ", officer::fp_text(font.size = 2)),
                  fp_p = officer::fp_par(...))
  }
  teile <- c(if (!is.null(titel)) list(officer::fpar(
                officer::ftext(titel, officer::fp_text(bold = TRUE)),
                fp_p = officer::fp_par(padding.top = 14, padding.bottom = 2))),
             list(leer(padding.top = oben), ft, leer(padding.bottom = unten)))
  do.call(block_list, teile)
}

# Hinweiszeile unter einer Tabelle: fett = unter dem unteren Normbereich.
# Leerer Text, wenn kein Kind markiert ist (dann steht dort nichts).
.abstand_hinweis <- function(unter, grenze) {
  if (!isTRUE(any(as.logical(unter)))) return("")
  paste0("Fett gedruckt sind Kinder mit einem R/F-Wert unter dem unteren ",
         "Normbereich (unter ", grenze, " %).")
}

# Kennzahlen der Klasse: ohne Klassenspalte (die Ueberschrift nennt die Klasse)
# und ohne Veraenderungsspalten - es gibt nur eine Messung.
tabelle_stand <- function(df) {
  .tabellen_luft(tabelle_infobrief(df, breiten = .breiten_stand))
}

# Kategorien-Uebersicht (Gruppe, Anzahl, Anteil), Gruppe farbig hinterlegt
tabelle_kategorien <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  farben <- .stand_gruppen_farben[match(as.character(df$Gruppe), .stand_gruppen)]
  stile <- lapply(seq_len(nrow(df)), function(i) {
    list(spalte = "Gruppe", zeile = i, hintergrund = farben[i])
  })
  .tabellen_luft(tabelle_infobrief(df, breiten = .breiten_kat, zell_stile = stile))
}

# Kinderliste mit Einzelwerten (hoechste Werte und Anhang). Die Kategorie wird
# wie im Excel-/Word-Dokument farbig hinterlegt; unter_norm markiert die Kinder
# unter dem unteren Normbereich: die ganze Zeile (inklusive Name) steht fett.
tabelle_werte <- function(df, unter_norm = NULL) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  stile <- list()
  if ("Kat." %in% colnames(df)) {
    farben <- .kat_farbe(df[["Kat."]])
    for (i in seq_along(farben)) {
      if (is.na(farben[i])) next
      stile[[length(stile) + 1]] <- list(spalte = "Kat.", zeile = i,
                                         hintergrund = farben[i])
    }
  }
  if (!is.null(unter_norm)) {
    # ohne "spalte": gilt fuer die ganze Zeile
    for (i in which(as.logical(unter_norm))) {
      stile[[length(stile) + 1]] <- list(zeile = i, fett = TRUE)
    }
  }
  .tabellen_luft(tabelle_infobrief(df, breiten = .breiten_werte, zell_stile = stile))
}

# Teilmenge eines Kohorten-Ergebnisses
cohort_teil <- function(x, idx) {
  neu <- x
  neu$paare <- x$paare[idx, , drop = FALSE]
  neu
}

# Mittelwerte einer Stufe (als Text und als Zahl)
.infobrief_mittel <- function(statistik, stufe) {
  z <- statistik[statistik$Stufe == stufe & statistik$Klasse == "gesamt", , drop = FALSE]
  if (nrow(z) == 0) z <- statistik[statistik$Stufe == stufe, , drop = FALSE]
  if (nrow(z) == 0 || is.na(z$mittel_WE[1])) {
    return(list(we = "-", rf = "-", we_num = NA_real_, rf_num = NA_real_))
  }
  list(we = de_zahl(z$mittel_WE[1]), rf = de_zahl(z$mittel_RF[1]),
       we_num = z$mittel_WE[1], rf_num = z$mittel_RF[1])
}

# Tabelle A aufbereiten: eine Zeile je Klasse, Mittelwert mit SD kombiniert,
# Kinder mit Werten in Klammern. Bewusst schmal (4 Spalten, ohne Stufenspalte -
# die Klasse enthaelt die Stufe bereits). Darunter eine Zeile "Differenz" mit
# der Veraenderung der Mittelwerte (aktuelle Stufe minus fruehere Stufe).
#
# Rueckgabe: list(tabelle = <data.frame>, differenz = c(we =, rf =)) - die
# Zahlen der Differenzzeile fuer die Farbgebung (NULL, wenn es keine zwei
# Stufen gibt).
.infobrief_tabelle_a <- function(statistik) {
  if (is.null(statistik) || nrow(statistik) == 0) {
    return(list(tabelle = NULL, differenz = NULL))
  }
  tab <- statistik
  # bei nur einer Klasse je Stufe waere "gesamt" eine Doppelung
  klassen_je_stufe <- table(tab$Stufe[tab$Klasse != "gesamt"])
  einzige <- suppressWarnings(as.numeric(names(klassen_je_stufe)[klassen_je_stufe == 1]))
  einzige <- einzige[!is.na(einzige)]
  if (length(einzige) > 0) {
    tab <- tab[!(tab$Klasse == "gesamt" & tab$Stufe %in% einzige), , drop = FALSE]
  }
  klasse <- ifelse(tab$Klasse == "gesamt", paste0(tab$Stufe, " gesamt"), tab$Klasse)
  daten.frame <- data.frame(
    Klasse = klasse,
    "n (mit Werten)" = paste0(de_zahl(tab$n, 0), " (", de_zahl(tab$n_werte, 0), ")"),
    "WE % (Mittel \u00b1 SD)" = paste0(de_zahl(tab$mittel_WE), " \u00b1", de_zahl(tab$sd_WE)),
    "R/F % (Mittel \u00b1 SD)" = paste0(de_zahl(tab$mittel_RF), " \u00b1", de_zahl(tab$sd_RF)),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  rownames(daten.frame) <- NULL

  # Differenz der Mittelwerte zwischen den beiden Stufen
  stufen <- sort(unique(stats::na.omit(tab$Stufe)))
  differenz <- NULL
  if (length(stufen) == 2) {
    frueher <- .infobrief_mittel(tab, stufen[1])
    spaeter <- .infobrief_mittel(tab, stufen[2])
    differenz <- c(we = spaeter$we_num - frueher$we_num,
                   rf = spaeter$rf_num - frueher$rf_num)
    daten.frame <- rbind(daten.frame, data.frame(
      Klasse = "Differenz",
      "n (mit Werten)" = "-",
      "WE % (Mittel \u00b1 SD)" = de_vz(differenz[["we"]]),
      "R/F % (Mittel \u00b1 SD)" = de_vz(differenz[["rf"]]),
      check.names = FALSE, stringsAsFactors = FALSE))
    rownames(daten.frame) <- NULL
  }

  list(tabelle = daten.frame, differenz = differenz)
}

# Datensatz fuer EINEN Abschnitt (einen Klassenbuchstaben)
infobrief_abschnitt <- function(cohort, idx, buchstabe, rueckgang = 10, top = 5,
                                schwach = 5) {
  teil <- cohort_teil(cohort, idx)
  g <- cohort_gematcht(teil)
  statistik <- cohort_statistik(teil)
  referenz <- unter_referenz(teil)
  rangliste <- cohort_rangliste(teil, top = top, schwach = schwach)

  klassen <- sort(unique(stats::na.omit(c(g$Klasse_Alt, g$Klasse_Neu))))

  # Nur Zahlen nennen - die Kinder stehen in den Tabellen
  zusatz <- character(0)
  if (referenz$unveraendert_kritisch > 0) {
    zusatz <- c(zusatz, paste0(" Bei ", kind_dativ(referenz$unveraendert_kritisch),
                               " trifft das schon im Vorjahr zu.",
                               if (referenz$unveraendert_kritisch < referenz$nachher_unter) {
                                 paste0(" ", kind_text(referenz$nachher_unter -
                                                         referenz$unveraendert_kritisch),
                                        if (referenz$nachher_unter -
                                           referenz$unveraendert_kritisch == 1) {
                                          " ist neu hinzugekommen."
                                        } else {
                                          " sind neu hinzugekommen."
                                        })
                               } else "")
    )
  } else if (referenz$neu_kritisch > 0) {
    zusatz <- c(zusatz, paste0(" ", kind_text(referenz$neu_kritisch),
                               if (referenz$neu_kritisch == 1) " ist" else " sind",
                               " neu hinzugekommen."))
  }
  if (referenz$nicht_mehr_kritisch > 0) {
    zusatz <- c(zusatz, paste0(" ", kind_text(referenz$nicht_mehr_kritisch),
                               if (referenz$nicht_mehr_kritisch == 1) " hat" else " haben",
                               " den Normbereich wieder erreicht."))
  }

  tabelle_aufbereitet <- .infobrief_tabelle_a(statistik)

  list(
    buchstabe = buchstabe,
    klassen_kombi = if (length(klassen) > 0) paste(klassen, collapse = " \u2192 ") else "-",
    stufe_alt = cohort$stufe_alt,
    stufe_neu = cohort$stufe_neu,
    n = nrow(g),
    rueckgang_schwelle = de_zahl(rueckgang, 0),
    referenz = list(grenze = de_zahl(referenz$grenze, 0),
                    n = referenz$n,
                    nachher_unter = referenz$nachher_unter,
                    zusatz = paste(zusatz, collapse = "")),
    tabelle = tabelle_aufbereitet$tabelle,
    tabelle_differenz = tabelle_aufbereitet$differenz,
    rangliste_top = rangliste$verbesserungen,
    rangliste_schwach = rangliste$schwach
  )
}

# Gesamtdaten fuer den Brief: Kopf, Abschnitte je Buchstabe, Anhang
infobrief_bericht <- function(cohort, rueckgang = 10, top_prosa = 3,
                              top = 5, schwach = 5) {
  stopifnot(inherits(cohort, "cohort"))

  p <- cohort$paare
  gematcht <- cohort_gematcht(cohort)
  nur_neu <- p[p$Status == "nur_neu", , drop = FALSE]
  ohne_partner <- p[p$Status == "kein_partner" & !is.na(p$Name_Alt), , drop = FALSE]
  vorschlaege <- cohort_vorschlaege(cohort)

  buchstabe <- sub("[0-9]+", "", ifelse(is.na(gematcht$Klasse_Neu),
                                        gematcht$Klasse_Alt, gematcht$Klasse_Neu))
  abschnitte <- lapply(sort(unique(buchstabe)), function(b) {
    idx <- which(cohort$paare$Status %in% c("auto", "bestaetigt") &
                   sub("[0-9]+", "",
                       ifelse(is.na(cohort$paare$Klasse_Neu),
                              cohort$paare$Klasse_Alt,
                              cohort$paare$Klasse_Neu)) == b)
    infobrief_abschnitt(cohort, idx, b, rueckgang = rueckgang,
                        top = top, schwach = schwach)
  })

  hinweise <- character(0)
  if (nrow(nur_neu) > 0) {
    hinweise <- c(hinweise, paste0("Neu in der Klasse (kein Vorjahreswert): ",
                                   namen_liste(nur_neu$Name_Neu), "."))
  }
  if (nrow(ohne_partner) > 0) {
    hinweise <- c(hinweise, paste0("Kein Partner im aktuellen Jahrgang gefunden: ",
                                   namen_liste(ohne_partner$Name_Alt), "."))
  }
  if (length(cohort$mehrdeutig) > 0) {
    hinweise <- c(hinweise, paste0("Nicht eindeutig zuzuordnen (gleicher Name mehrfach): ",
                                   namen_liste(cohort$mehrdeutig), "."))
  }
  if (nrow(vorschlaege) > 0) {
    hinweise <- c(hinweise, paste0("Noch nicht bestätigte Zuordnungen (nicht mitgezählt): ",
                                   namen_liste(paste0(vorschlaege$Name_Alt, " \u2013 ",
                                                      vorschlaege$Name_Neu)), "."))
  }
  wechsel <- gematcht[!is.na(gematcht$Klasse_Alt) & !is.na(gematcht$Klasse_Neu) &
                        sub("[0-9]+", "", gematcht$Klasse_Alt) !=
                        sub("[0-9]+", "", gematcht$Klasse_Neu), , drop = FALSE]
  if (nrow(wechsel) > 0) {
    hinweise <- c(hinweise, paste0("Klassenwechsel: ",
                                   namen_liste(paste0(wechsel$Name_Neu, " (",
                                                      wechsel$Klasse_Alt, " \u2192 ",
                                                      wechsel$Klasse_Neu, ")")), "."))
  }
  fehlend_alt <- cohort$n_alt - cohort$n_alt_werte
  fehlend_neu <- cohort$n_neu - cohort$n_neu_werte
  if (fehlend_alt > 0 || fehlend_neu > 0) {
    teile <- character(0)
    if (fehlend_alt > 0) {
      teile <- c(teile, paste0(kind_text(fehlend_alt), " in der ",
                               cohort$stufe_alt, ". Klasse"))
    }
    if (fehlend_neu > 0) {
      teile <- c(teile, paste0(kind_text(fehlend_neu), " in der ",
                               cohort$stufe_neu, ". Klasse"))
    }
    hinweise <- c(hinweise, paste0("Nicht teilgenommen: ", paste(teile, collapse = ", "), "."))
  }

  klassen_alt <- cohort$klassen_alt
  list(
    stufe_alt = cohort$stufe_alt,
    stufe_neu = cohort$stufe_neu,
    klassen_alt = paste(klassen_alt, collapse = ", "),
    klassen_text = if (length(klassen_alt) == 1) {
      paste0("Die Klasse ", klassen_alt)
    } else {
      paste0("Die Klassen ", paste(klassen_alt, collapse = ", "))
    },
    anrede = infobrief_anrede(""),
    n_gematcht = nrow(gematcht),
    n_nur_neu = nrow(nur_neu),
    n_ohne_partner = nrow(ohne_partner),
    n_mehrdeutig = length(cohort$mehrdeutig),
    zusatz_neu = if (nrow(nur_neu) > 0) {
      paste0(" ", kind_text(nrow(nur_neu)),
             if (nrow(nur_neu) == 1) " ist" else " sind", " neu hinzugekommen.")
    } else "",
    zusatz_partner = if (nrow(ohne_partner) > 0) {
      paste0(" Für ", kind_dativ(nrow(ohne_partner)),
             " wurde kein Vorjahreswert gefunden.")
    } else "",
    abschnitte = abschnitte,
    hinweise = hinweise,
    # im Anhang ohne Hinweisspalte: die Seite ist zu schmal dafuer
    lesetabelle = vergleich_tabelle(cohort, mit_hinweis = FALSE)
  )
}

# Anrede aus den Klassen des Briefes.
#   eine Klasse   -> "Liebe Klassenleitung der 6b,"
#   mehrere       -> "Liebe Klassenleitungen der 6a, 6b und 6c,"
#   keine Klasse  -> neutral
# Ein einzelner Text (z. B. "6c") funktioniert weiterhin wie bisher.
infobrief_anrede <- function(klassen = character(0)) {
  klassen <- as.character(klassen)
  klassen <- trimws(klassen[!is.na(klassen)])
  klassen <- sort(unique(klassen[nzchar(klassen)]))
  if (length(klassen) == 0) return("Liebe Kollegin, lieber Kollege,")
  if (length(klassen) == 1) return(paste0("Liebe Klassenleitung der ", klassen, ","))
  aufzaehlung <- paste(c(paste(klassen[-length(klassen)], collapse = ", "),
                         klassen[length(klassen)]), collapse = " und ")
  paste0("Liebe Klassenleitungen der ", aufzaehlung, ",")
}

# Klassen, die der Entwicklungsbrief nennt: die aktuelle Stufe (dort sitzen die
# Kinder heute), sonst die fruehere Stufe, sonst die geladenen Klassen.
klassen_fuer_anrede <- function(cohort) {
  g <- cohort_gematcht(cohort)
  neu <- unique(stats::na.omit(g$Klasse_Neu))
  if (length(neu) > 0) return(as.character(neu))
  alt <- unique(stats::na.omit(g$Klasse_Alt))
  if (length(alt) > 0) return(as.character(alt))
  as.character(c(cohort$klassen_neu, cohort$klassen_alt))
}

# Arbeitskopie der Briefvorlagen (wie beim Elternbrief)
infobrief_vorbereiten <- function(quelle = file.path(getwd(), "infobrief")) {
  vorlagen_vorbereiten(quelle, "infobrief")
}

# Kopf, Abschnitte und Abschluss zu EINEM Rmd zusammensetzen.
# Die drei Dateien bleiben reine Inhalts-Fragmente (dort steht der Fliesstext,
# editierbar); Kopfzeile/Format kommen aus der Vorlage. So entsteht ein einziges
# Dokument - ohne eingebettete Teildokumente (die sonst eigene
# Abschnittseigenschaften mitbringen und leere Seiten erzeugen).
.infobrief_rmd_bauen <- function(vorlage, daten, datei) {
  fragment <- function(name) readLines(file.path(vorlage, paste0(name, ".Rmd")),
                                       warn = FALSE, encoding = "UTF-8")
  setze_d <- function(ausdruck) c("```{r, echo=FALSE}", ausdruck, "```", "")

  inhalt <- c(
    "---",
    "output: officedown::rdocx_document",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "library(officer)",
    "library(flextable)",
    "knitr::opts_chunk$set(echo = FALSE)",
    "```",
    "",
    setze_d("d <- daten$kopf"),
    fragment("kopf"),
    unlist(lapply(seq_along(daten$abschnitte), function(i) {
      c(setze_d(paste0("d <- daten$abschnitte[[", i, "]]")), fragment("abschnitt"))
    })),
    setze_d("d <- daten$abschluss"),
    fragment("abschluss")
  )
  writeLines(inhalt, datei, useBytes = FALSE)
  invisible(datei)
}

# Infobrief erstellen: Kopf + ein Abschnitt je Buchstabe + Abschluss in einem Lauf
create_infobrief <- function(cohort, klassenleitung = "", absender = "",
                             rueckgang = 10, top_prosa = 3, top = 5, schwach = 5,
                             fortschritt = NULL) {
  # fortschritt(anteil, text) meldet den Stand an die Oberflaeche (0 bis 1)
  melde <- function(anteil, text) {
    if (is.function(fortschritt)) {
      fortschritt(max(0, min(1, anteil)), text)
    }
    invisible(NULL)
  }

  melde(0.05, "Daten werden zusammengestellt ...")
  bericht <- infobrief_bericht(cohort, rueckgang = rueckgang, top_prosa = top_prosa,
                               top = top, schwach = schwach)
  if (bericht$n_gematcht == 0) {
    stop("Keine zugeordneten Kinder - es kann kein Infobrief erstellt werden.",
         call. = FALSE)
  }

  melde(0.2, "Vorlagen werden vorbereitet ...")
  ziel_ordner <- createFilePath(NULL, "")
  vorlage <- infobrief_vorbereiten()
  on.exit(vorlagen_aufraeumen(vorlage), add = TRUE)
  rest <- file.path(vorlage, "infobrief.knit.md")
  if (fs::file_exists(rest)) fs::file_delete(rest)

  name <- paste0("Infobrief_", gsub("[^A-Za-z0-9]+", "", cohort$stufe_alt), "-",
                 gsub("[^A-Za-z0-9]+", "", cohort$stufe_neu), "_",
                 gsub("[^A-Za-z0-9]+", "_",
                      paste(unique(c(cohort$klassen_alt, cohort$klassen_neu)),
                            collapse = "_")))
  datei <- .pfad_nativ(file.path(ziel_ordner, paste0(name, ".docx")))
  # Eine in Word geoeffnete Zieldatei kann nicht ersetzt werden: sofort melden,
  # statt erst nach dem Rendern zu scheitern
  .pruefe_datei_frei(datei)

  kopf <- bericht
  # Anrede automatisch aus den Klassen des Briefes; wer es anders braucht,
  # uebergibt klassenleitung ausdruecklich (die Oberflaeche tut das nicht mehr).
  kopf$anrede <- infobrief_anrede(if (!is.null(klassenleitung) &&
                                      nzchar(trimws(klassenleitung))) {
    klassenleitung
  } else {
    klassen_fuer_anrede(cohort)
  })
  daten <- list(kopf = kopf,
                abschnitte = bericht$abschnitte,
                abschluss = list(hinweise = bericht$hinweise, absender = absender,
                                 lesetabelle = bericht$lesetabelle))

  rmd <- .infobrief_rmd_bauen(vorlage, daten, file.path(vorlage, "infobrief_gesamt.Rmd"))
  melde(0.35, "Word-Datei wird geschrieben (pandoc) ...")
  rmarkdown::render(rmd, output_file = datei, quiet = TRUE)
  message("Infobrief gespeichert unter: ", datei)
  melde(1, "Infobrief fertig")

  list(datei = datei, bericht = bericht)
}

#### Stand-Brief: je Klasse eine Seite in einem Dokument ####
#
# Der Stand-Brief beschreibt den aktuellen Stand EINER Klasse. Er braucht keine
# Zuordnung und keinen Vorjahresvergleich: er funktioniert nach dem Ersttest in
# der 5 genauso wie nach dem Re-Test in der 6.
# Die Texte stehen in infobrief/stand.Rmd, hier stehen Zahlen und Tabellen.

# Grobe Einordnung der Kategorien. Die feinen Stufen (1A ... 5E) stehen im
# Anhang je Kind; im Ueberblick reichen vier Gruppen plus "nicht teilgenommen".
# Die Reihenfolge hier ist die Reihenfolge im Brief.
.stand_gruppen <- c(
  "kein Handlungsbedarf",
  "Rechtschreibung ausbauf\u00e4hig",
  "Rechtschreibung ausbauf\u00e4hig, gro\u00dfer Unterschied WE zu R/F",
  "Handlungsbedarf (Rechtschreibung und Wortschatz)",
  "nicht teilgenommen",
  "ohne Zuordnung")                      # nur bei unbekannten Werten (alte Dateien)

.kategorien_gruppe <- c(
  "1A" = 1L, "1B" = 1L, "2A" = 1L, "2B" = 1L,
  "3C" = 2L, "4C" = 2L, "5C" = 2L,
  "3C*" = 3L, "4C*" = 3L, "5C*" = 3L,
  "3D" = 4L, "4D" = 4L, "5D" = 4L, "4E" = 4L, "5E" = 4L)

# Gruppennummer einer Kategorie: ohne Werte ("0") = nicht teilgenommen,
# unbekannte Werte = "ohne Zuordnung"
stand_gruppe <- function(kat) {
  kat <- trimws(as.character(kat))
  gruppe <- unname(.kategorien_gruppe[kat])
  fehlt <- is.na(kat) | !nzchar(kat) | (!is.na(kat) & kat == "0")
  gruppe[fehlt] <- 5L
  gruppe[is.na(gruppe)] <- 6L
  as.integer(gruppe)
}

# Kategorie eines Kindes als Text: ohne Teilnahme steht "-" (intern "0")
kat_text <- function(kat) {
  kat <- trimws(as.character(kat))
  kat[is.na(kat) | !nzchar(kat) | (!is.na(kat) & kat == "0")] <- "-"
  kat
}

# Welche feinen Kategorien in welcher Gruppe landen (Querverweis in der
# Uebersicht). Wird aus .kategorien_gruppe abgeleitet, damit es nur eine Quelle
# gibt. Reihenfolge = Reihenfolge der Gruppen.
kategorien_je_gruppe <- function() {
  namen <- names(.kategorien_gruppe)
  vapply(seq_along(.stand_gruppen), function(g) {
    if (g == 5L) return("ohne Werte")
    if (g == 6L) return("unbekannte Werte")
    paste(namen[.kategorien_gruppe == g], collapse = ", ")
  }, character(1))
}

# Uebersicht "wie viele Kinder in welcher Gruppe". Der Anteil bezieht sich auf
# ALLE Kinder der Klasse, damit die Zeilen zusammen 100 % ergeben und "nicht
# teilgenommen" eingeordnet ist. Die Spalte "Kategorien (Kat.)" nennt die
# feinen Kategorien der Spalte "Kat." aus den Kindertabellen.
kategorien_uebersicht <- function(kat) {
  kat <- as.character(kat)
  n <- length(kat)
  if (n == 0) return(NULL)
  anzahl <- tabulate(stand_gruppe(kat), nbins = length(.stand_gruppen))
  zeilen <- 1:5
  if (anzahl[6] > 0) zeilen <- 1:6
  data.frame(Gruppe = .stand_gruppen[zeilen],
             "Kategorien (Kat.)" = kategorien_je_gruppe()[zeilen],
             Anzahl = formatC(anzahl[zeilen], format = "d"),
             Anteil = paste0(de_zahl(100 * anzahl[zeilen] / n, 0), " %"),
             check.names = FALSE, stringsAsFactors = FALSE)
}

# Stufenzahl aus dem Klassennamen ("5a" -> 5)
.stufe_aus_klasse <- function(x) {
  suppressWarnings(as.numeric(gsub("[^0-9]", "", as.character(x))))
}

# Kennzahlen je Klasse (eine Zeile je Klasse) - dieselben Zahlen wie im
# Entwicklungsbrief: nur Kinder mit Werten gehen in die Mittelwerte ein.
stand_statistik <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(data.frame())
  klasse <- as.character(df$Klasse)
  ok <- !is.na(klasse) & nzchar(klasse)
  if (!any(ok)) return(data.frame())

  daten <- data.frame(
    Klasse = klasse[ok],
    WE = suppressWarnings(as.numeric(df[["WE-%"]][ok])),
    RF = suppressWarnings(as.numeric(df[["R/F-%"]][ok])),
    stringsAsFactors = FALSE)

  # bei fehlenden Werten "-" statt "NA"; eine Gruppe ohne Werte ergibt NA
  kennzahl <- function(x, f) {
    if (length(x) == 0 || all(is.na(x))) return(NA_real_)
    round(f(x, na.rm = TRUE), 1)
  }

  zeilen <- lapply(sort(unique(daten$Klasse)), function(k) {
    teil <- daten[daten$Klasse == k, , drop = FALSE]
    data.frame(
      Stufe = .stufe_aus_klasse(k), Klasse = k,
      n = nrow(teil),
      n_werte = sum(!is.na(teil$WE) | !is.na(teil$RF)),
      mittel_WE = kennzahl(teil$WE, mean), median_WE = kennzahl(teil$WE, stats::median),
      sd_WE = kennzahl(teil$WE, stats::sd),
      mittel_RF = kennzahl(teil$RF, mean), median_RF = kennzahl(teil$RF, stats::median),
      sd_RF = kennzahl(teil$RF, stats::sd),
      stringsAsFactors = FALSE)
  })

  ergebnis <- do.call(rbind, zeilen)
  rownames(ergebnis) <- NULL
  ergebnis
}

# Kinderliste mit Einzelwerten, alphabetisch (Anhang).
# Rueckgabe: list(tabelle, unter) - "unter" ist die Markierung "unter dem
# unteren Normbereich" in der Reihenfolge der Tabelle.
.kinder_tabelle <- function(daten, unter = NULL) {
  if (nrow(daten) == 0) return(list(tabelle = NULL, unter = NULL))
  idx <- order(as.character(daten$Name))
  daten <- daten[idx, , drop = FALSE]
  tab <- data.frame(Name = as.character(daten$Name),
                    "WE %" = daten$WE,
                    "R/F %" = daten$RF,
                    "Kat." = kat_text(daten$Kat),
                    check.names = FALSE, stringsAsFactors = FALSE)
  rownames(tab) <- NULL
  list(tabelle = tab, unter = if (is.null(unter)) NULL else unter[idx])
}

# Ein Abschnitt des Stand-Briefs (eine Klasse).
# Rueckgabe: Liste mit allen Feldern, die infobrief/stand.Rmd braucht.
stand_abschnitt <- function(df, klasse, grenze = .normgrenze("R/F"), top = 5) {
  klasse <- as.character(klasse)
  teil <- df[as.character(df$Klasse) == klasse, , drop = FALSE]
  kat <- as.character(teil$Kat.)
  we <- suppressWarnings(as.numeric(teil[["WE-%"]]))
  rf <- suppressWarnings(as.numeric(teil[["R/F-%"]]))
  statistik <- stand_statistik(teil)

  # Kennzahlentabelle: n (mit Werten), Mittel +- SD, Median
  tabelle <- data.frame(
    "n (mit Werten)" = paste0(de_zahl(statistik$n[1], 0), " (",
                              de_zahl(statistik$n_werte[1], 0), ")"),
    "WE % (Mittel \u00b1 SD)" = paste0(de_zahl(statistik$mittel_WE[1]), " \u00b1",
                                       de_zahl(statistik$sd_WE[1])),
    "R/F % (Mittel \u00b1 SD)" = paste0(de_zahl(statistik$mittel_RF[1]), " \u00b1",
                                        de_zahl(statistik$sd_RF[1])),
    "Median WE %" = de_zahl(statistik$median_WE[1]),
    "Median R/F %" = de_zahl(statistik$median_RF[1]),
    check.names = FALSE, stringsAsFactors = FALSE)

  # unterer Normbereich (Grenze aus den Einstellungen). Die Kinder stehen nicht
  # mehr namentlich im Text - markiert ist die ganze Zeile in der Tabelle.
  mit_rf <- !is.na(rf)
  unter <- mit_rf & rf < grenze

  # hoechste Werte (R/F, bei Gleichstand WE); idx haelt die Zuordnung zum Kind,
  # damit die Markierung "unter dem Normbereich" mitwandert
  hoechste <- data.frame(Name = as.character(teil$Name)[mit_rf],
                         WE = we[mit_rf], RF = rf[mit_rf],
                         Kat = kat_text(kat)[mit_rf],
                         idx = which(mit_rf),
                         stringsAsFactors = FALSE)
  hoechste <- hoechste[order(-hoechste$RF, -hoechste$WE, hoechste$Name), , drop = FALSE]
  hoechste <- utils::head(hoechste, top)
  top_tabelle <- if (nrow(hoechste) > 0) {
    tab <- data.frame(Name = hoechste$Name, "WE %" = hoechste$WE,
                      "R/F %" = hoechste$RF, "Kat." = hoechste$Kat,
                      check.names = FALSE, stringsAsFactors = FALSE)
    rownames(tab) <- NULL
    tab
  } else {
    NULL
  }

  anhang <- .kinder_tabelle(data.frame(Name = as.character(teil$Name), WE = we,
                                       RF = rf, Kat = kat,
                                       stringsAsFactors = FALSE),
                            unter = unter)

  list(
    klasse = klasse,
    stufe = .stufe_aus_klasse(klasse),
    anrede = infobrief_anrede(klasse),
    n = nrow(teil),
    n_werte = sum(!is.na(we) | !is.na(rf)),
    tabelle = tabelle,
    kategorien = kategorien_uebersicht(kat),
    referenz = list(grenze = de_zahl(grenze, 0),
                    n = sum(mit_rf),
                    unter = sum(unter)),
    top = top_tabelle,
    # TRUE = Kind liegt unter dem unteren Normbereich (steht fett in der Tabelle)
    top_unten = if (nrow(hoechste) > 0) unter[hoechste$idx] else NULL,
    lesetabelle = anhang$tabelle,
    lese_unten = anhang$unter
  )
}

# Alle Abschnitte des Stand-Briefs: eine je Klasse, in der Reihenfolge der
# Klassennamen. Klassen ohne Kinder mit Werten werden uebersprungen (die
# Oberflaeche nennt sie in der Meldung).
stand_bericht <- function(df, grenze = .normgrenze("R/F"), top = 5) {
  statistik <- stand_statistik(df)
  if (nrow(statistik) == 0) {
    return(list(abschnitte = list(), klassen = character(0),
                uebersprungen = character(0)))
  }
  klassen <- as.character(statistik$Klasse[statistik$n_werte > 0])
  uebersprungen <- as.character(statistik$Klasse[statistik$n_werte == 0])
  list(abschnitte = lapply(klassen, function(k) stand_abschnitt(df, k, grenze = grenze,
                                                               top = top)),
       klassen = klassen,
       uebersprungen = uebersprungen)
}

# Kopf + ein Abschnitt je Klasse zu EINEM Rmd zusammensetzen (Muster wie beim
# Entwicklungsbrief: ein Dokument, keine eingebetteten Teildokumente).
.standbrief_rmd_bauen <- function(vorlage, daten, datei) {
  fragment <- function(name) readLines(file.path(vorlage, paste0(name, ".Rmd")),
                                       warn = FALSE, encoding = "UTF-8")
  setze_d <- function(ausdruck) c("```{r, echo=FALSE}", ausdruck, "```", "")

  teile <- unlist(lapply(seq_along(daten$abschnitte), function(i) {
    c(if (i > 1) c("", "\\newpage", ""),                 # je Klasse eine neue Seite
      setze_d(paste0("d <- daten$abschnitte[[", i, "]]")),
      fragment("stand"))
  }))

  inhalt <- c(
    "---",
    "output: officedown::rdocx_document",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "library(officer)",
    "library(flextable)",
    "knitr::opts_chunk$set(echo = FALSE)",
    "```",
    "",
    teile
  )
  writeLines(inhalt, datei, useBytes = FALSE)
  invisible(datei)
}

# Stand-Brief erstellen: eine docx mit einer Seite je Klasse.
create_standbrief <- function(df, absender = "", fortschritt = NULL,
                              grenze = .normgrenze("R/F"), top = 5) {
  melde <- function(anteil, text) {
    if (is.function(fortschritt)) {
      fortschritt(max(0, min(1, anteil)), text)
    }
    invisible(NULL)
  }

  melde(0.05, "Daten werden zusammengestellt ...")
  bericht <- stand_bericht(df, grenze = grenze, top = top)
  if (length(bericht$abschnitte) == 0) {
    stop("Keine Kinder mit Werten - es kann kein Stand-Brief erstellt werden.",
         call. = FALSE)
  }

  melde(0.2, "Vorlagen werden vorbereitet ...")
  ziel_ordner <- createFilePath(NULL, "")
  vorlage <- infobrief_vorbereiten()
  on.exit(vorlagen_aufraeumen(vorlage), add = TRUE)
  rest <- file.path(vorlage, "infobrief.knit.md")
  if (fs::file_exists(rest)) fs::file_delete(rest)

  # eine Datei fuer alle Klassen; der Absender steht in jedem Klassenschreiben
  abschnitte <- lapply(bericht$abschnitte, function(a) {
    a$absender <- absender
    a
  })

  name <- paste0("Infobrief_Stand_",
                 gsub("[^A-Za-z0-9]+", "_", paste(bericht$klassen, collapse = "_")))
  datei <- .pfad_nativ(file.path(ziel_ordner, paste0(name, ".docx")))
  # Eine in Word geoeffnete Zieldatei kann nicht ersetzt werden: sofort melden,
  # statt erst nach dem Rendern zu scheitern
  .pruefe_datei_frei(datei)

  # rmarkdown::render() wertet das Rmd im Frame von create_standbrief() aus -
  # der Name "daten" ist deshalb Pflicht (wie bei .infobrief_rmd_bauen)
  daten <- list(abschnitte = abschnitte)
  rmd <- .standbrief_rmd_bauen(vorlage, daten,
                               file.path(vorlage, "standbrief_gesamt.Rmd"))
  melde(0.35, "Word-Datei wird geschrieben (pandoc) ...")
  rmarkdown::render(rmd, output_file = datei, quiet = TRUE)
  message("Stand-Brief gespeichert unter: ", datei)
  melde(1, "Stand-Brief fertig")

  list(datei = datei, bericht = bericht)
}
