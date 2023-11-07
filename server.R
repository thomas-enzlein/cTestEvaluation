server <- function(input, output, session) {
  # app beenden wenn brower geschlossen wird
  session$onSessionEnded(function() {
    stopApp()
  })
  
  observe_helpers()
  theme_set(theme_minimal(base_size = 16) +
              theme(panel.grid = element_blank()))
  
  #### App-Object ####
  rv <- reactiveValues(df = tibble("Name" = character(0),
                                   "WE-Wert" = numeric(0),
                                   "WE-%" = numeric(0),
                                   "R/F-Wert" = numeric(0),
                                   "R/F-%" = numeric(0),
                                   "Kat." = factor(character(0), 
                                                   levels = lvls),
                                   "Empfehlung" = character(0)),
                       numItems = 80,
                       inital = TRUE
  )
  #### Anzahl der Testitems aendern 
  observeEvent(input$numItems, {
    rv$numItems <- as.numeric(input$numItems)
  })
  
  
  #### Uebersichtstabelle ####
  observeEvent(rv$df, {
    dt <- datatable(rv$df,
                    selection = "multiple",
                    options = list(searching = TRUE,
                                   pageLength = 15,
                                   lengthMenu = c(10, 15, 30), 
                                   scrollX = TRUE,
                                   language = list(url = "de-DE.json")))  %>%
      styleTable()
    output$tabUebersicht <- renderDT(dt) 
  })
  
  
  #### Hinzufuegen button ####
  observeEvent(input$btHinzufuegen, {
    error <- checkInputErrors(inputName = input$schuelerName, 
                              inputRf = input$rfWert, 
                              inputWe = input$weWert, 
                              numItems = rv$numItems,
                              klasse = composeClass(klassenstufe = input$klassenstufe, 
                                                    klassenBuchstabe = input$klassenBuchstabe))
    
    # Falls Fehler, Ausgabe als Text in der App
    if(!is.null(error)) {
      cat(paste("Fehler:", error, "\n"))
      output$text <-  renderText(paste("Fehler:", error, "\n"))
      return()
    }
    # Eingabe korrekt, fuege Schueler hinzu
    rv$df <- addEntry(df = rv$df, 
                      name = input$schuelerName, 
                      rf = input$rfWert, 
                      we = input$weWert, 
                      numItems = rv$numItems)
    
    if(rv$inital) {
      # erster schueler hinzugefügt
      rv$inital <- FALSE
    }
    
    # setze Eingaben zurueck
    updateTextInput(inputId = "schuelerName", value = "")
    updateNumericInput(inputId = "rfWert", value = NA)
    updateNumericInput(inputId = "weWert", value = NA)
    output$text <-  renderText("")
    
    # refocus auf Namensfeld
    js$refocus("schuelerName")
    
  })
  
  #### Entfernen Button ####
  disable(id = "btEntfernen")
  observeEvent(input$tabUebersicht_rows_selected, {
    if(length(input$tabUebersicht_rows_selected) > 0) {
      enable(id = "btEntfernen")
    } else {
      disable(id = "btEntfernen")
    }
  })
  
  observeEvent(input$btEntfernen, {
    if(rv$inital) {
      cat("Noch keine Schueler in der Tabelle. Nichts zu entfernen.")
      return()
    }
    if(length(input$tabUebersicht_rows_selected)>0) {
      rv$df <- rv$df[-input$tabUebersicht_rows_selected,]
    }
    
  })
  
  
  #### Speichern Button ####
  observeEvent(input$btSpeichern, {
    if(rv$inital) {
      cat("Noch keine Schueler in der Tabelle. Nichts zu speichern.")
      return()
    }
    
    fn <- paste0("C-Test_Auswertung_", Sys.Date())

    write_tsv(rv$df, 
              file = createFilePath(fn, "tsv"))
    table2doc_(rv$df, 
               file = createFilePath(fn, ""), 
               digits = 1, 
               width = 8.3,
               height = 11.7,
               pointsize = 7)
    
    table2excel(rv$df, 
                file = createFilePath(fn, ""), 
                sheetName = "C-Test", 
                digits = 1)
    
    msgs <- paste0("Daten gespeichert unter ", createFilePath(NULL, ""))
    output$text <-  renderText(msgs)
    utils::browseURL(createFilePath(NULL, ""))
  })
  
  #### Laden Button #####
  observeEvent(input$input_tsv, {
    file <- input$input_tsv
    req(file)
    ext <- tools::file_ext(file$datapath)
    
    validate(need(ext == "tsv", "Bitte tsv Datei auswählen"))
    
    new_df <- read_tsv(file$datapath, col_types = list(col_character(),
                                                       col_number(),
                                                       col_number(),
                                                       col_number(),
                                                       col_number(),
                                                       col_factor(levels = lvls),
                                                       col_character()))
    
    if(!checkColumnNames(new_df, rv$df)) {
      cat("Fehler, Spaltennamen stimmen nicht.")
      return()
    }
    
    if(rv$inital) {
      # wenn noch keine Daten eingegeben wurden benutze die neuen Daten
      rv$df <- new_df
      rv$inital <- FALSE
      return()
    } 
    
    rv$df <- 
      rv$df %>%
      bind_rows(new_df)
  })
  
  #### Verteilungs plot ####
  observeEvent(list(rv$df, input$WEplotType), {
    
    if(rv$inital) {
      return()
    }
    
    pRF <- createHistogram(rv$df, x = "`R/F-%`", 
                           fill = "`Kat.`", 
                           xlab = "R/F-Wert in %")
    
    pRF <- pRF + 
      geom_vline(xintercept = 71.3, linetype = "dashed", color = "grey40") + 
      geom_vline(xintercept = 65, linetype = "dotted", color = "grey40")
    
    output$histRF <-  renderPlotly(ggplotly(pRF))
    
    if(input$WEplotType == "WE-%") {
      pWE <- createHistogram(rv$df, 
                             x = "`WE-%`", 
                             fill = "`Kat.`", 
                             xlab = "WE-Wert in %")
    } else if(input$WEplotType == "Differenz") {
      pWE <-
        rv$df %>%
        mutate(diff = `WE-%` - `R/F-%`) %>%
        createHistogram(x = "`diff`", 
                        fill = "`Kat.`", 
                        xlab = "Abweichung zwischen R/F- und WE-Wert in %")
    }
    
    output$histWE <-  renderPlotly(ggplotly(pWE))
    
    # Mittelwert und Median unter Plots
    output$statsRF <- renderUI({
      createStatsText(rv$df, "R/F-%", "R/F")
    })
    
    output$statsWE <- renderUI({
      createStatsText(rv$df, "WE-%", "WE")
    })
  })
}
