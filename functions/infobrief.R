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

# Zahl im deutschen Format (Dezimalkomma)
de_zahl <- function(x, digits = 1) {
  if (length(x) == 0 || all(is.na(x))) return("-")
  formatC(x, format = "f", digits = digits, decimal.mark = ",")
}

# Zahl mit Vorzeichen (fuer Veraenderungen)
de_vz <- function(x, digits = 1) {
  if (length(x) == 0 || all(is.na(x))) return("-")
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
                              breiten = NULL, vorzeichen_spalten = character(0)) {
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

# Tabelle A: Kennzahlen je Klasse (nur gematchte Gruppe)
tabelle_a <- function(df) tabelle_infobrief(df, breiten = .breiten_a)
# Tabelle B: Kinderliste (Top 5 bzw. schwaechste 5), ohne Klassenspalte
tabelle_b <- function(df) {
  tabelle_infobrief(df, farb_spalten = c("\u0394 WE", "\u0394 R/F"),
                    vorzeichen_spalten = c("\u0394 WE", "\u0394 R/F"),
                    breiten = .breiten_b)
}
# Anhang: Vergleich je Kind (mit Klassenspalte)
tabelle_lese <- function(df) {
  tabelle_infobrief(df, farb_spalten = c("\u0394 WE", "\u0394 R/F"),
                    vorzeichen_spalten = c("\u0394 WE", "\u0394 R/F"),
                    breiten = .breiten_lese)
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
# die Klasse enthaelt die Stufe bereits).
.infobrief_tabelle_a <- function(statistik) {
  if (is.null(statistik) || nrow(statistik) == 0) return(NULL)
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
  daten.frame
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
    tabelle = .infobrief_tabelle_a(statistik),
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
    lesetabelle = vergleich_tabelle(cohort)
  )
}

# Anrede (neutral, optional an die Klassenleitung gerichtet)
infobrief_anrede <- function(klassenleitung = "") {
  if (!is.null(klassenleitung) && nzchar(trimws(klassenleitung))) {
    paste0("Liebe Klassenleitung der ", trimws(klassenleitung), ",")
  } else {
    "Liebe Kollegin, lieber Kollege,"
  }
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
                             rueckgang = 10, top_prosa = 3, top = 5, schwach = 5) {
  bericht <- infobrief_bericht(cohort, rueckgang = rueckgang, top_prosa = top_prosa,
                               top = top, schwach = schwach)
  if (bericht$n_gematcht == 0) {
    stop("Keine zugeordneten Kinder - es kann kein Infobrief erstellt werden.",
         call. = FALSE)
  }

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
  datei <- file.path(ziel_ordner, paste0(name, ".docx"))

  kopf <- bericht
  kopf$anrede <- infobrief_anrede(klassenleitung)
  daten <- list(kopf = kopf,
                abschnitte = bericht$abschnitte,
                abschluss = list(hinweise = bericht$hinweise, absender = absender,
                                 lesetabelle = bericht$lesetabelle))

  rmd <- .infobrief_rmd_bauen(vorlage, daten, file.path(vorlage, "infobrief_gesamt.Rmd"))
  rmarkdown::render(rmd, output_file = datei, quiet = TRUE)
  message("Infobrief gespeichert unter: ", datei)

  list(datei = datei, bericht = bericht)
}
