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
                                   "Klasse" = character(0),
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
                              klasse = composeClass(input$klassenstufe, input$klBuchstabe))
    
    # Falls Fehler, Ausgabe als Text in der App
    if(!is.null(error)) {
      cat(paste("Fehler:", error, "\n"))
      output$text <-  renderText(paste("Fehler:", error, "\n"))
      return()
    }
    # Eingabe korrekt, fuege Schueler hinzu
    rv$df <- addEntry(df = rv$df, 
                      name = input$schuelerName, 
                      klasse = composeClass(input$klassenstufe, input$klBuchstabe),
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
    
    msgs <- saveData(rv$df)
    
    output$text <-  renderText(msgs)
    utils::browseURL(createFilePath(NULL, ""))
  })
  
  #### Laden Button #####
  observeEvent(input$input_tsv, {
    
    new_df <- loadData(input$input_tsv)
    
    if(!checkColumnNames(rv$df, new_df)) {
      msgs <- paste0("Fehler, Spaltennamen stimmen nicht in ", 
                     checkInputFile(input$input_tsv), "\n")
      output$text <-  renderText(msgs)
      message(msgs)
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
                        xlab = "Differenz R/F- und WE-Wert in %")
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
