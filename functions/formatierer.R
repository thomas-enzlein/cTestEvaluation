# functions/formatierer.R
#
# Darstellung: Zahlen, Namen, Farben, Tabellen-Look.
#
# Ausgelagert aus functions.R (nur verschoben, Inhalt unveraendert), damit
# die Fachlogik in functions.R uebersichtlich bleibt.

# Ab dieser Groesse werden Veraenderungen zusaetzlich fett dargestellt
.infobrief_fett_ab <- 20
.infobrief_gruen <- "#1E7B34"
.infobrief_rot <- "#B00020"

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

# Spaltenbreiten (in cm): die Kategorie-Spalte schmal, der Rest teilt sich die
# Textbreite.
.ergebnis_breiten <- function(spalten, gesamt = 16) {
  if (spalten <= 1) return(gesamt)
  c(2.0, rep((gesamt - 2.0) / (spalten - 1), spalten - 1))
}

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

# Aufzaehlung von Kindern: zwischen den Kindern Semikolon, damit das Komma im
# Namen ("Dahlbruch, Colin") eindeutig bleibt.
namen_liste <- function(x) {
  x <- x[!is.na(x) & nzchar(x)]
  paste(sort(x), collapse = "; ")
}

# Kategorie-Spalte eines Datenrahmens als Text (NA, wenn es die Spalte nicht
# gibt). Eine Stelle fuer alle drei Faerbungen (App, Word, Excel).
.kat_spalte <- function(df) {
  if ("Kat." %in% names(df)) trimws(as.character(df[["Kat."]])) else rep(NA_character_, nrow(df))
}

# Kategorie-Farbe wie in der App, im Excel-Export und im Word-Dokument
# (cols/lvls stehen in global.R). Nicht teilgenommen ("0") bleibt ohne Farbe.
.kat_farbe <- function(kat) {
  kat <- trimws(as.character(kat))
  if (!exists("lvls", inherits = TRUE) || !exists("cols", inherits = TRUE)) {
    return(rep(NA_character_, length(kat)))
  }
  unname(cols[match(kat, lvls)])
}

# Zeilen je Kategorie (fuer die Word-Faerbung). Exakter Vergleich: "3C" faerbt
# nicht die Zelle "3C*" (siehe auch die Excel-Regel in table2spreadsheet_.R).
.kat_zeilen <- function(kat, levels) {
  kat <- trimws(as.character(kat))
  lapply(levels, function(l) which(kat == l))
}


# --- Kuerzel geladener Dateien (Liste in der Sidebar) ------------------------
# Datum aus dem Dateinamen, kurz: "2024-10-29" oder "29.10.2024" -> "29.10.24".
datum_aus_dateiname <- function(name) {
  if (is.null(name) || !nzchar(name)) return(NULL)
  m <- regmatches(name, regexec("(20[0-9]{2})-([0-9]{2})-([0-9]{2})", name))[[1]]
  if (length(m) == 4) return(paste0(m[4], ".", m[3], ".", substr(m[2], 3, 4)))
  m <- regmatches(name, regexec("([0-9]{2})\\.([0-9]{2})\\.([0-9]{4})", name))[[1]]
  if (length(m) == 4) return(paste0(m[2], ".", m[3], ".", substr(m[4], 3, 4)))
  NULL
}

# Kuerzel einer geladenen Datei: "29.10.24 - 5c", "29.10.24 - 5c,d,e",
# "29.10.24 - 5c, 6c", "29.10.24 - 5c, +8".
# Gleiche Stufe: nur das erste Kuerzel traegt die Stufenzahl, damit bis zu drei
# Klassen in sechs Zeichen passen. Passen mehr nicht hinein (oder sind es mehr
# als zwei Stufen), steht das erste Kuerzel und dahinter die Zahl der weiteren.
datei_kuerzel <- function(klassen, name = NULL) {
  klassen <- sort(unique(as.character(klassen)))
  klassen <- klassen[!.ist_leer(klassen)]

  if (length(klassen) == 0) {
    text <- "ohne Kl."
  } else {
    stufen <- unique(gsub("[^0-9]", "", klassen))
    gleich <- length(stufen) == 1
    kurz <- if (gleich) gsub("[^A-Za-z]", "", klassen) else klassen
    # ohne Buchstaben (z. B. Klasse "5") lieber die volle Form zeigen
    if (any(!nzchar(kurz))) kurz <- klassen
    kurz[1] <- klassen[1]
    hoechstens <- if (gleich) 3 else 2
    text <- if (length(kurz) > hoechstens) {
      paste0(klassen[1], ", +", length(klassen) - 1)
    } else {
      paste(kurz, collapse = if (gleich) "," else ", ")
    }
  }

  datum <- datum_aus_dateiname(name)
  if (is.null(datum)) text else paste0(datum, " - ", text)
}

# Vollstaendige Beschriftung (Tooltip und Statusmeldungen)
beschriftung_datei <- function(klassen, name = NULL) {
  klassen <- sort(unique(as.character(klassen)))
  klassen <- klassen[!.ist_leer(klassen)]
  liste <- if (length(klassen) == 0) "ohne Klassenangabe" else paste(klassen, collapse = ", ")
  if (is.null(name) || !nzchar(name)) liste else paste0(name, " (", liste, ")")
}
