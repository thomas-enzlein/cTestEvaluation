# Hilfsfunktion zur Standardisierung von Namen
normalize_name <- function(x) {
  x %>%
    tolower() %>%
    trimws() %>%
    gsub("\\s+", " ", .) %>%
    gsub(",", "", .) %>%
    sapply(function(n) paste(sort(unlist(strsplit(n, " "))), collapse = " "))
}

# Fuzzy-Match Funktion
fuzzy_match_names <- function(df, var_name, max_dist) {
  df_clean <- df %>%
    mutate(clean_name = normalize_name(Name))
  
  klassen <- sort(unique(df_clean$Klasse))
  if (length(klassen) < 2) return(data.frame())
  
  k1 <- klassen[1]; k2 <- klassen[2]
  df1 <- df_clean %>% filter(Klasse == k1)
  df2 <- df_clean %>% filter(Klasse == k2)
  
  # Fuzzy Join
  matches <- fuzzyjoin::stringdist_inner_join(df1, df2,
                                              by = "clean_name",
                                              max_dist = max_dist,
                                              distance_col = "dist") %>%
    mutate(match_quality = 1 - dist / pmax(nchar(clean_name.x), nchar(clean_name.y)))
  
  # Beste Treffer auswählen
  best_matches <- matches %>%
    group_by(Name.x) %>%
    slice_min(dist, n = 1) %>%
    ungroup()
  
  # Spalten umbenennen - korrigierte Syntax
  best_matches <-
    best_matches %>%
    select(
      Name_Alt = Name.x, 
      Name_Neu = Name.y, 
      Klasse_Alt = Klasse.x, 
      Klasse_Neu = Klasse.y, 
      Wert_Alt = paste0(var_name, ".x"), 
      Wert_Neu = paste0(var_name, ".y"), 
      Distanz = dist, 
      Ähnlichkeit = match_quality
    )
  return(best_matches)
}