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
                ggplot(df, aes(x = !!sym(x), text = NameKlasse, fill = !!sym(fill))) +
                  geom_histogram(bins = bins, col = "black", show.legend = FALSE) +
                  scale_fill_manual(values = cols, limits = lvls) +
                  labs(x = xlab, y = "Anzahl") +
                  theme(legend.position = 'none')
              },
              
              "Dichte" = {
                ggplot(df, aes(x = !!sym(x), col = Klasse)) +
                  geom_density(linewidth = 1) +
                  labs(x = xlab, y = "Anzahl") +
                  theme(legend.position = "inside", legend.position.inside = c(.94, .75))
              },
              
              "Entwicklung" = {
                  plot_veraenderung(df, variable = x)
              },
              
              {
                warning(paste("Unbekannter Plot-Typ:", type))
                ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Unbekannter Plot-Typ", size = 6) + theme_void()
              }
  )
  
  
  if(!allCombined & !type == "Entwicklung") {
    p <- p + 
      facet_wrap(~Klasse, ncol = 1) 
  }
  
  return(p)
}


plot_veraenderung <- function(data, variable = c("WE-%", "R/F-%", "diff")) {
  variable <- match.arg(variable)
  
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
    df_long <- df %>%
      filter(Klasse %in% klassen) %>%
      mutate(Klasse = factor(Klasse, levels = klassen))
    
    ggplot(df_long, aes(x = Klasse, y = !!sym(variable), group = Name, color = Name)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      labs(
        x = "Klasse",
        y = variable
      ) +
      theme(legend.position = "none")
    
    # --- Fall 2: Genau zwei Messzeitpunkte (Barchart mit Annotationen) ---
  } else if (n_klassen == 2) {
    k1 <- klassen[1]; k2 <- klassen[2]
    
    df_wide <- df %>%
      filter(Klasse %in% c(k1, k2)) %>%
      pivot_wider(names_from = Klasse, values_from = !!sym(variable)) %>%
      mutate(
        Veränderung = !!sym(k2) - !!sym(k1),
        Aktuell = !!sym(k2)
      ) %>%
      filter(!is.na(Veränderung))
    
    if (nrow(df_wide) == 0) {
      ggplot() +
        geom_text(aes(0, 0, label = "Keine Personen mit zwei Messungen vorhanden")) 

    } else {
      df_wide <- df_wide %>% mutate(Name = fct_reorder(Name, Veränderung))
      
      # Achsenreichweite erweitern, damit Text nicht abgeschnitten wird
      xlim_range <- range(df_wide$Veränderung, na.rm = TRUE)
      x_buffer <- diff(xlim_range) * 0.33
      x_limits <- c(-max(xlim_range) - x_buffer, max(xlim_range) + x_buffer)
      
      ggplot(df_wide, aes(x = Veränderung, y = Name, fill = Veränderung > 0)) +
        geom_col() +
        geom_vline(xintercept = 0, color = "black", linewidth = 1) +
        
        # Veränderungstext nur wenn ≠ 0
        geom_text(
          data = subset(df_wide, round(Veränderung, 1) != 0),
          aes(label = sprintf("%+.1f%%", Veränderung), 
              x = if_else(Veränderung < 0, -mean(abs(Veränderung)), mean(abs(Veränderung))),
              color = if_else(Veränderung>0, "darkgreen", "darkred")),
          fontface = "bold", size = 3.3
        ) +
        
        # Aktuelle Werte rechtsbündig alle auf gleicher Linie
        geom_text(
          aes(
            label = sprintf("%.1f%%", Aktuell),
            x = max(Veränderung, na.rm = TRUE) + x_buffer * 0.66
          ),
          hjust = 0, color = "black", size = 3.3
        ) +
        
        scale_x_continuous(limits = x_limits) +
        scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato")) +
        scale_color_identity() +
        labs(
          x = paste("Entwicklung", variable, k1, "→", k2),
          y = NULL
        ) +
        theme(
          legend.position = "none",
          plot.margin = margin(10, 60, 10, 10)  # rechter Rand vergrößert
        )
    }
    
    # --- Fall 3: Nur eine Klasse vorhanden ---
  } else {
    ggplot() +
      geom_text(aes(0, 0, label = "Nur eine Klasse im Datensatz vorhanden – keine Entwicklung darstellbar"))
  }
}

# plot output vorbereiten
createPlotOutput <- function(outputId) {
  div(plotlyOutput(outputId = outputId), class = "plot-div")
}
