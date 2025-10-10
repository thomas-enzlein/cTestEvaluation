library(dplyr)
library(ggplot2)
library(forcats)
library(tidyr)

# Histogram erstellen
createPlot<- function(df, x, fill, xlab, bins = 30, allCombined = TRUE, type = c("Histogramm", "Dichte", "Entwicklung")) {
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
                plot_veraenderung(df, 
                                  variable = x)
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
  
  
  if(!allCombined & !type == "Entwicklung") {
    p <- p + 
      facet_wrap(~Klasse, ncol = 1) 
  }
  
  return(p)
}


plot_veraenderung <- function(data, variable = c("WE-%", "R/F-%", "diff"), 
                              use_fuzzy = TRUE, max_dist = 3) {
  variable <- match.arg(variable)
  
  # copy to remove confusion in ggplot
  var <- variable
  
  # Daten vorbereiten
  df <- data %>%
    select(Name, Klasse, !!sym(variable)) %>%
    mutate(Klasse = as.character(Klasse))
  
  # Alle Jahrgangsstufen im Datensatz
  klassen <- sort(unique(df$Klasse))
  
  # Jahrgangszahl extrahieren und nach Zahlen sortieren
  jahrgang_num <- suppressWarnings(as.numeric(gsub("[^0-9]", "", klassen)))
  if (any(is.na(jahrgang_num))) {
    warning("Klassennamen enthalten keine erkennbaren Jahrgangszahlen – alphabetische Sortierung wird verwendet.")
  } else {
    klassen <- klassen[order(jahrgang_num)]
  }
  
  n_klassen <- length(klassen)
  
  # --- Fall 1: Mehr als zwei Messzeitpunkte (Linienplot) ---
  if (n_klassen > 2) {
    if (use_fuzzy) {
      warning("Fuzzy-Matching wird bei mehr als zwei Klassen nicht unterstützt. Verwende exakte Namenszuordnung.")
    }
    
    df_long <- df %>%
      filter(Klasse %in% klassen) %>%
      mutate(Klasse = factor(Klasse, 
                             levels = klassen))
    
    p <- ggplot(df_long, aes(x = Klasse, 
                             y = !!sym(variable), 
                             group = Name, 
                             color = Name)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      labs(
        x = "Klasse",
        y = variable
      ) +
      theme(legend.position = "none")
    
    # --- Fall 2: Genau zwei Messzeitpunkte (Barchart mit Annotationen) ---
  } else if (n_klassen == 2) {
    k1 <- klassen[1]
    k2 <- klassen[2]
    
    if (use_fuzzy) {
      # Fuzzy-Matching durchführen
      fuzzy_results <- fuzzy_match_names(df = df, 
                                         var_name = variable, 
                                         max_dist = max_dist)
      
      if (nrow(fuzzy_results) == 0) {
        return(ggplot() +
                 geom_text(aes(0, 0, label = "Keine Übereinstimmungen mit Fuzzy-Matching gefunden")))
      }
      
      # Daten für Plot vorbereiten mit gematchten Namen
      df_wide <- fuzzy_results %>%
        mutate(
          Veränderung = as.numeric(Wert_Neu) - as.numeric(Wert_Alt),
          Aktuell = as.numeric(Wert_Neu)
        ) %>%
        filter(!is.na(Veränderung))
      
      if (nrow(df_wide) == 0) {
        return(ggplot() +
                 geom_text(aes(0, 0, label = "Keine Personen mit zwei Messungen vorhanden")))
      }
      
      # Namen für Plot: zeige beide Versionen mit Match-Info
      df_wide <- df_wide %>%
        mutate(Name_Display = if_else(
          Name_Alt == Name_Neu,
          Name_Alt,
          paste0(Name_Alt,"'")
        )) %>%
        mutate(Name_Display = forcats::fct_reorder(Name_Display, Veränderung))
      
    } else {
      # Ohne Fuzzy-Matching (originale Logik)
      df_wide <- df %>%
        filter(Klasse %in% c(k1, k2)) %>%
        tidyr::pivot_wider(names_from = Klasse, values_from = !!sym(variable)) %>%
        mutate(
          Veränderung = as.numeric(!!sym(k2)) - as.numeric(!!sym(k1)),
          Aktuell = as.numeric(!!sym(k2))
        ) %>%
        filter(!is.na(Veränderung))
      
      if (nrow(df_wide) == 0) {
        return(ggplot() +
                 geom_text(aes(0, 0, label = "Keine Personen mit zwei Messungen vorhanden")))
      }
      
      df_wide <- df_wide %>% mutate(Name = forcats::fct_reorder(Name, Veränderung))
    }
    
    # Plot erstellen
    xlim_range <- range(df_wide$Veränderung, na.rm = TRUE)
    x_buffer <- diff(xlim_range) * 0.33
    x_limits <- c(-max(abs(xlim_range)) - x_buffer, max(abs(xlim_range)) + x_buffer)
    xlab <- paste("Entwicklung", if_else(variable != "diff", variable, "Diff. WE-R/F"), k1, "->", k2)
    
    vmax <- max(df_wide$Veränderung, na.rm = TRUE)
    pos <- if_else(vmax > 50, 25, vmax/2)
    
    p <- ggplot(df_wide, 
                aes(x = Veränderung, 
                    y = Name_Display, 
                    fill = Veränderung > 0)) +
      geom_col() +
      geom_vline(xintercept = 0, color = "black", linewidth = 1) +
      # Veränderungstext nur wenn ≠ 0
      geom_text(
        data = subset(df_wide, round(Veränderung, 1) != 0),
        aes(label = sprintf("%+.1f%%", Veränderung), 
            x = if_else(Veränderung < 0, 
                        -pos, 
                        pos),
            color = if_else(Veränderung > 0, "#004000", "#400000")),
        fontface = "bold", size = 3.3
      ) +
      # Aktuelle Werte rechtsbündig
      geom_text(
        aes(
          label = sprintf("%.1f%%", Aktuell),
          x = max(Veränderung, na.rm = TRUE) + x_buffer * 0.66,
          color = case_when(
            var == "diff" & Aktuell >= 20 ~ "darkred",
            var == "diff" & Aktuell < 20 ~ "black",
            Aktuell >= 65 ~ "darkgreen", 
            Aktuell < 65 ~ "darkred"
          )
        ),
        hjust = 0, fontface = "bold", size = 3.3
      ) +
      scale_x_continuous(limits = x_limits) +
      scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato")) +
      scale_color_identity() +
      labs(
        x = xlab,
        y = NULL
      ) +
      theme(
        legend.position = "none",
        axis.text.y = element_text(color = "black")
      )
    
    # --- Fall 3: Nur eine Klasse vorhanden ---
  } else {
    p <- ggplot() +
      geom_text(aes(0, 0, label = "Nur eine Klasse im Datensatz vorhanden – keine Entwicklung darstellbar"))
  }
  return(p)
}

# plot output vorbereiten
createPlotOutput <- function(outputId) {
  div(plotlyOutput(outputId = outputId, height = "500px"), class = "plot-div")
}
