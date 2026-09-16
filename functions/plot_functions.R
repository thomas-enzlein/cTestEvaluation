library(dplyr)
library(ggplot2)
library(forcats)
library(tidyr)

# Diagramme fuer den Statistik-Tab.
#
# Das Entwicklungsdiagramm nutzt die Zuordnung aus functions/cohort.R - also
# dieselben Paare (inklusive manuell bestaetigter) wie der Lehrkraefte-Infobrief.

# Histogramm, Dichte oder Entwicklungsdiagramm erstellen
createPlot<- function(df, x, fill, xlab, bins = 30, allCombined = TRUE,
                      type = "Histogramm", cohort = NULL) {
  df <- df %>%
    mutate(NameKlasse = paste0(Name, ", ", Klasse))

  p <- switch(type,
              "Histogramm" = {
                ggplot(df, aes(x = !!sym(x), 
                               text = NameKlasse, 
                               fill = !!sym(fill))) +
                  geom_histogram(bins = bins, 
                                 col = "black", 
                                 show.legend = FALSE) +
                  scale_fill_manual(values = cols, 
                                    limits = lvls) +
                  labs(x = xlab, 
                       y = "Anzahl") +
                  theme(legend.position = 'none')
              },
              
              "Dichte" = {
                ggplot(df, aes(x = !!sym(x), 
                               col = Klasse)) +
                  stat_density(geom="line", 
                               position="identity", 
                               linewidth = 1) +
                  labs(x = xlab, 
                       y = "Anzahl") +
                  theme(legend.position = "inside", 
                        legend.position.inside = c(.94, .75))
              },
              
              "Entwicklung" = {
                plot_veraenderung(cohort, variable = x)
              },
              
              "Verlauf" = {
                plot_verlauf(cohort)
              },
              
              {
                warning(paste("Unbekannter Plot-Typ:", type))
                ggplot() + 
                  annotate("text", 
                           x = 0.5, 
                           y = 0.5, 
                           label = "Unbekannter Plot-Typ", 
                           size = 6) + 
                  theme_void()
              }
  )
  
  
  if(!allCombined & !type %in% c("Entwicklung", "Verlauf")) {
    p <- p + 
      facet_wrap(~Klasse, ncol = 1) 
  }
  
  return(p)
}

# Hinweistext, wenn kein Vergleich moeglich ist
plot_hinweis <- function(text) {
  ggplot() +
    annotate("text", x = 0, y = 0, label = text, size = 5) +
    theme_void()
}

# Verlaufsdiagramm fuer zwei Messzeitpunkte: eine Linie je Kind (nur
# zugeordnete Kinder), getrennt nach WE- und R/F-Wert.
plot_verlauf <- function(cohort, grenze = 65) {
  if (is.null(cohort) || !inherits(cohort, "cohort")) {
    return(plot_hinweis("Bitte zwei Stufen auswaehlen (Menue links)"))
  }

  g <- cohort_gematcht(cohort)
  if (nrow(g) == 0) {
    return(plot_hinweis("Keine zugeordneten Kinder fuer diesen Vergleich"))
  }

  lang <- dplyr::bind_rows(
    tibble::tibble(Name = g$Name_Neu, Stufe = cohort$stufe_alt,
                   Kennzahl = "WE-Wert in %", Wert = g$WE_Alt),
    tibble::tibble(Name = g$Name_Neu, Stufe = cohort$stufe_neu,
                   Kennzahl = "WE-Wert in %", Wert = g$WE_Neu),
    tibble::tibble(Name = g$Name_Neu, Stufe = cohort$stufe_alt,
                   Kennzahl = "R/F-Wert in %", Wert = g$RF_Alt),
    tibble::tibble(Name = g$Name_Neu, Stufe = cohort$stufe_neu,
                   Kennzahl = "R/F-Wert in %", Wert = g$RF_Neu)
  )
  lang <- lang[!is.na(lang$Wert), , drop = FALSE]
  if (nrow(lang) == 0) {
    return(plot_hinweis("Keine Kinder mit zwei Messwerten vorhanden"))
  }
  lang$Kennzahl <- factor(lang$Kennzahl,
                          levels = c("WE-Wert in %", "R/F-Wert in %"))

  ggplot(lang, aes(x = Stufe, y = Wert, group = Name, color = Name)) +
    geom_line(linewidth = 0.7, alpha = 0.8) +
    geom_point(size = 2) +
    geom_hline(yintercept = grenze, linetype = "dotted", color = "grey40") +
    facet_wrap(~Kennzahl, ncol = 1) +
    scale_x_continuous(breaks = c(cohort$stufe_alt, cohort$stufe_neu)) +
    labs(x = "Jahrgangsstufe", y = NULL) +
    theme(legend.position = "none")
}

# Entwicklungsdiagramm fuer ein Stufenpaar.
#
# cohort: Ergebnis von build_cohort() (functions/cohort.R)
# variable: "WE-%", "R/F-%" oder "diff" (Differenz WE - R/F)
plot_veraenderung <- function(cohort, variable = c("WE-%", "R/F-%", "diff")) {
  variable <- match.arg(variable)

  if (is.null(cohort) || !inherits(cohort, "cohort")) {
    return(plot_hinweis("Bitte zwei Stufen auswaehlen (Menue links)"))
  }

  g <- cohort_gematcht(cohort)
  if (nrow(g) == 0) {
    return(plot_hinweis("Keine zugeordneten Kinder fuer diesen Vergleich"))
  }

  # Veraenderung und aktueller Wert je nach Kennzahl
  df <- switch(variable,
    "WE-%" = tibble::tibble(
      Name = g$Name_Neu, Name_Alt = g$Name_Alt,
      Veraenderung = g$dWE, Aktuell = g$WE_Neu, Aehnlichkeit = g$Aehnlichkeit),
    "R/F-%" = tibble::tibble(
      Name = g$Name_Neu, Name_Alt = g$Name_Alt,
      Veraenderung = g$dRF, Aktuell = g$RF_Neu, Aehnlichkeit = g$Aehnlichkeit),
    "diff" = tibble::tibble(
      Name = g$Name_Neu, Name_Alt = g$Name_Alt,
      Veraenderung = g$dWE - g$dRF,
      Aktuell = g$WE_Neu - g$RF_Neu, Aehnlichkeit = g$Aehnlichkeit)
  )
  df <- df[!is.na(df$Veraenderung), , drop = FALSE]
  if (nrow(df) == 0) {
    return(plot_hinweis("Keine Kinder mit zwei Messwerten vorhanden"))
  }

  # Namen mit abweichender Schreibweise kennzeichnen
  df <- df %>%
    mutate(Name_Display = if_else(is.na(Aehnlichkeit) | Aehnlichkeit >= 0.999,
                                  Name, paste0(Name, "'"))) %>%
    mutate(Name_Display = forcats::fct_reorder(Name_Display, Veraenderung))

  k1 <- cohort$klassen_alt[1]
  k2 <- cohort$klassen_neu[1]
  xlab <- paste("Entwicklung",
                if (variable != "diff") variable else "Diff. WE-R/F",
                k1, "->", k2)

  grenzen <- range(df$Veraenderung, na.rm = TRUE)
  puffer <- diff(grenzen) * 0.33
  if (!is.finite(puffer) || puffer == 0) puffer <- 1
  x_limits <- c(-max(abs(grenzen)) - puffer, max(abs(grenzen)) + puffer)
  vmax <- max(df$Veraenderung, na.rm = TRUE)
  pos <- if (isTRUE(vmax > 50)) 25 else max(vmax, 1) / 2

  p <- ggplot(df,
              aes(x = Veraenderung,
                  y = Name_Display,
                  fill = Veraenderung > 0)) +
    geom_col() +
    geom_vline(xintercept = 0, color = "black", linewidth = 1) +
    geom_text(
      data = subset(df, round(Veraenderung, 1) != 0),
      aes(label = sprintf("%+.1f%%", Veraenderung),
          x = if_else(Veraenderung < 0, -pos, pos),
          color = if_else(Veraenderung > 0, "#004000", "#400000")),
      fontface = "bold", size = 3.3
    ) +
    geom_text(
      aes(label = sprintf("%.1f%%", Aktuell),
          x = max(Veraenderung, na.rm = TRUE) + puffer * 0.66,
          color = case_when(
            variable == "diff" & Aktuell >= 20 ~ "darkred",
            variable == "diff" & Aktuell < 20 ~ "black",
            Aktuell >= 65 ~ "darkgreen",
            Aktuell < 65 ~ "darkred"
          )),
      hjust = 0, fontface = "bold", size = 3.3
    ) +
    scale_x_continuous(limits = x_limits) +
    scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato")) +
    scale_color_identity() +
    labs(x = xlab, y = NULL) +
    theme(legend.position = "none",
          axis.text.y = element_text(color = "black"))

  return(p)
}

# plot output vorbereiten
createPlotOutput <- function(outputId) {
  div(plotlyOutput(outputId = outputId, height = "500px"), class = "plot-div")
}
