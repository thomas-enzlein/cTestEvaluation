server <- function(input, output, session) {
  # app beenden wenn brower geschlossen wird
  session$onSessionEnded(function() {
    stopApp()
  })
  
  observe_helpers()
  theme_set(theme_minimal(base_size = 18) +
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
                       inital = TRUE,
                       # Namen und Stufen der geladenen tsv-Dateien (fuer die
                       # Zuordnungsdatei des Infobriefs sowie fuer die Pruefung,
                       # ob eine Stufe doppelt geladen wurde)
                       dateien = list(),
                       # manuelle Zuordnungs-Entscheidungen (Ja/Nein)
                       entscheidungen = NULL
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
                                   pageLength = 30,
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
      errText <- paste("Fehler:", error, "\n")
      output$text <- renderText(errText)
      showNotification(errText, type = "error")
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
    # Bewusst runjs() statt js$refocus(): das Objekt `js` liegt auf dem
    # Suchpfad und kann von anderen Paketen verdeckt werden (xfun exportiert
    # ebenfalls ein `js`), was den Observer mit einem Fehler abgebrochen hat.
    shinyjs::runjs("document.getElementById('schuelerName').focus();")
    
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
      showNotification("Noch keine Schueler in der Tabelle. Nichts zu speichern.", type = "error")
      return()
    }
    showNotification("Speichere Daten. Bitte warten...")
    # Vergleichstabelle (nur wenn zwei Stufen mit mindestens einem zugeordneten
    # Kind vorliegen) landet als Blatt "Vergleich" im Excel und als Anhang im Word
    vergleich <- tryCatch(vergleich_tabelle(cohort_oder_null()),
                          error = function(e) NULL)
    # Fehler beim Schreiben (z.B. keine Schreibrechte) sichtbar melden
    ergebnis <- tryCatch(list(ok = TRUE, wert = saveData(rv$df, vergleich = vergleich)),
                         error = function(e) list(ok = FALSE, wert = conditionMessage(e)))

    if(!ergebnis$ok) {
      msgs <- paste0("Fehler beim Speichern: ", ergebnis$wert)
      showNotification(msgs, type = "error", duration = NULL)
      output$text <- renderText(msgs)
      message(msgs)
      return()
    }

    msgs <- ergebnis$wert
    showNotification(msgs)

    output$text <-  renderText(msgs)
    utils::browseURL(createFilePath(NULL, ""))
  })
  
  #### Laden Button #####
  observeEvent(input$input_tsv, {
    
    new_df <- loadData(input$input_tsv)
    
    if(!checkColumnNames(rv$df, new_df)) {
      msgs <- paste0("Fehler, Spaltennamen stimmen nicht in ", 
                     checkInputFile(input$input_tsv), "\n")
      showNotification(msgs, type = "error")
      output$text <-  renderText(msgs)
      message(msgs)
      return()
    }
    
    # geladene Datei merken: Name und enthaltene Stufen (fuer den Infobrief)
    rv$dateien[[length(rv$dateien) + 1]] <- list(
      name = input$input_tsv$name,
      klassen = as.character(unique(new_df$Klasse)),
      # Stufenzahlen der enthaltenen Klassen (z. B. 5 fuer "5c")
      stufen = sort(unique(stats::na.omit(suppressWarnings(
        as.numeric(gsub("[^0-9]", "", as.character(unique(new_df$Klasse)))))))))
    
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
  
  #### Elternbrief #####
  observeEvent(input$btBrief, {
    
    if(rv$inital) {
      showNotification("Noch keine Schueler in der Tabelle. Kann keine Briefe erstellen.", type = "error")
      return()
    }
    
    if(!isTruthy(input$lehrername)) {
      showNotification("Bitte Lehrername eingeben.", type = "error")
      return()
    }
    
    showNotification("Elternbriefe werden erstellen.\n Dies kann mehrere Minuten dauern.\nBitte warten...",
                     duration = 30)
    
    # Fehler beim Erstellen (z.B. fehlende Schreibrechte) sichtbar melden
    ergebnis <- tryCatch({
      list(ok = TRUE, wert = create_letters(rv$df, 
                                            lehrername = input$lehrername, 
                                            signatur = input$signatur,
                                            qrLink = input$qrLink))
    }, error = function(e) list(ok = FALSE, wert = conditionMessage(e)))

    if(!ergebnis$ok) {
      msgs <- paste0("Fehler beim Erstellen der Elternbriefe: ", ergebnis$wert)
      showNotification(msgs, type = "error", duration = NULL)
      output$text <- renderText(msgs)
      message(msgs)
      return()
    }
    
    msgs <- paste0("Elternbriefe erstellt unter: ", createFilePath(NULL, ""),
                   " (", ergebnis$wert$erstellt, " Brief(e))")
    if(length(ergebnis$wert$fehler) > 0) {
      # einzelne Briefe konnten nicht erstellt werden - Kind und Grund nennen
      msgs <- paste0(msgs, "\nNicht erstellt: ",
                     paste(utils::head(ergebnis$wert$fehler, 5), collapse = " | "))
      showNotification(msgs, type = "warning", duration = 15)
    } else {
      showNotification(msgs, duration = 5)
    }
    output$text <- renderText(msgs)
    
    utils::browseURL(createFilePath(NULL, ""))
  })
  
  #### Stufenvergleich und Infobrief ####

  # verfuegbare Stufen aus den geladenen Daten (numerisch sortiert)
  stufen_verfuegbar <- reactive({
    if(rv$inital) return(numeric(0))
    klassen <- unique(as.character(rv$df$Klasse))
    klassen <- klassen[nzchar(klassen)]
    stufen <- suppressWarnings(as.numeric(gsub("[^0-9]", "", klassen)))
    sort(unique(stufen[!is.na(stufen)]))
  })

  # dieselbe Stufe aus zwei Dateien: kein eindeutiger Vergleich moeglich
  doppelte_stufen <- reactive({
    if(length(rv$dateien) == 0) return(numeric(0))
    alle <- unlist(lapply(rv$dateien, function(d) d$stufen), use.names = FALSE)
    alle <- alle[!is.na(alle)]
    unique(alle[duplicated(alle)])
  })

  auswahl_moeglich <- reactive({
    length(stufen_verfuegbar()) >= 2 && length(doppelte_stufen()) == 0
  })

  # Auswahl fuellen, sperren wenn nicht moeglich, und erklaeren warum
  observeEvent(list(rv$df, rv$dateien), {
    stufen <- stufen_verfuegbar()

    if(length(stufen) < 2 || length(doppelte_stufen()) > 0) {
      shinyjs::disable("siStufeAlt")
      shinyjs::disable("siStufeNeu")
      updateSelectInput(session, "siStufeAlt", choices = character(0))
      updateSelectInput(session, "siStufeNeu", choices = character(0))
      rv$entscheidungen <- NULL
      return()
    }

    shinyjs::enable("siStufeAlt")
    shinyjs::enable("siStufeNeu")

    aktuelle_auswahl <- c(input$siStufeAlt, input$siStufeNeu)
    auswahl_ok <- length(aktuelle_auswahl) == 2 &&
      all(!is.na(aktuelle_auswahl)) &&
      all(as.character(aktuelle_auswahl) %in% as.character(stufen)) &&
      !identical(as.character(aktuelle_auswahl[1]), as.character(aktuelle_auswahl[2]))

    if(auswahl_ok) {
      updateSelectInput(session, "siStufeAlt", choices = stufen,
                        selected = as.numeric(aktuelle_auswahl[1]))
      updateSelectInput(session, "siStufeNeu", choices = stufen,
                        selected = as.numeric(aktuelle_auswahl[2]))
      return()
    }

    # Vorschlag: das Paar mit den meisten zuordenbaren Kindern, bei Gleichstand
    # das juengere Paar
    paare <- lapply(seq_len(length(stufen) - 1), function(i) c(stufen[i], stufen[i + 1]))
    anzahl <- vapply(paare, function(p) {
      tryCatch(nrow(cohort_gematcht(build_cohort(rv$df, p[1], p[2]))),
               error = function(e) 0)
    }, numeric(1))
    bestes <- paare[[which.max(anzahl)]]
    updateSelectInput(session, "siStufeAlt", choices = stufen, selected = bestes[1])
    updateSelectInput(session, "siStufeNeu", choices = stufen, selected = bestes[2])
  })

  # Hinweis nur in den Sonderfaellen (gesperrte Auswahl begruenden).
  # Im Normalfall steht hier nichts: was verglichen wird, zeigen die Felder
  # selbst, und Diagramm ("Entwicklung (5 -> 6)") sowie Infobrief nennen das
  # Stufenpaar ebenfalls.
  output$vergleichHinweis <- renderUI({
    if(rv$inital) return(NULL)
    if(length(doppelte_stufen()) > 0) {
      return(helpText(paste0("Die Stufe ", paste(doppelte_stufen(), collapse = ", "),
                             " liegt in zwei geladenen Dateien. Bitte je Stufe nur eine ",
                             "Datei laden, damit der Vergleich eindeutig ist.")))
    }
    if(length(stufen_verfuegbar()) < 2) {
      return(helpText(paste0("Für den Vergleich bitte eine zweite tsv-Datei mit einem ",
                             "anderen Jahrgang laden (Laden-Button oben).")))
    }
    NULL
  })

  # Zuordnungsdatei: Name kommt aus den Daten (Stufen und Klassen) - passt damit
  # auch fuer getippte Daten, und Umbenennungen der tsv aendern nichts
  entscheidungen_datei <- reactive({
    if(!auswahl_moeglich()) return(NULL)
    k <- cohort_oder_null()
    if(is.null(k)) return(NULL)
    entscheidungen_pfad(k)
  })

  # gespeicherte Entscheidungen laden, sobald das Vergleichspaar feststeht
  observeEvent(list(input$siStufeAlt, input$siStufeNeu, rv$df), {
    # Diagrammarten mit dem aktuellen Stufenpaar beschriften (Werte bleiben gleich)
    if(!is.null(input$siStufeAlt) && !is.null(input$siStufeNeu)) {
      kurz <- paste0(input$siStufeAlt, " \u2192 ", input$siStufeNeu)
      updateSelectInput(session, "siPlotType",
                        choices = stats::setNames(
                          c("Histogramm", "Dichte", "Entwicklung", "Verlauf"),
                          c("Histogramm", "Dichte",
                            paste0("Entwicklung (", kurz, ")"),
                            paste0("Verlauf (", kurz, ")"))),
                        selected = input$siPlotType)
    }
    if(!auswahl_moeglich()) {
      rv$entscheidungen <- NULL
      return()
    }
    req(input$siStufeAlt, input$siStufeNeu)
    k <- cohort_oder_null()
    if(is.null(k)) {
      rv$entscheidungen <- NULL
      return()
    }
    geladen <- read_entscheidungen(entscheidungen_pfad(k))
    rv$entscheidungen <- geladen
    # Pruefsumme: Hinweis auf einen anderen Durchgang - nur warnen, nicht blocken
    if(nrow(geladen) > 0) {
      gespeichert <- unique(stats::na.omit(geladen$Kohorte))
      if(length(gespeichert) > 0 && !any(gespeichert == kohorte_pruefsumme(k))) {
        showNotification(paste0("Die gespeicherte Zuordnung gehört zu einem anderen ",
                                "Durchgang (Prüfsumme weicht ab). Sie wird nur dort ",
                                "angewendet, wo Namen exakt übereinstimmen."),
                         type = "warning", duration = 15)
      }
    }
  })

  # Zuordnung berechnen (eine Wahrheit fuer Tabelle, Diagramm und Infobrief)
  cohort_daten <- reactive({
    req(auswahl_moeglich())
    build_cohort(rv$df, input$siStufeAlt, input$siStufeNeu,
                 entscheidungen = rv$entscheidungen)
  })

  cohort_oder_null <- function() {
    tryCatch(cohort_daten(), error = function(e) NULL)
  }

  output$tabZuordnung <- renderDT({
    k <- cohort_oder_null()
    if(is.null(k)) return(NULL)
    p <- k$paare
    anzeige <- tibble("Name (Vorjahr)" = p$Name_Alt,
                      "Klasse (Vorjahr)" = p$Klasse_Alt,
                      "WE % (Vorjahr)" = p$WE_Alt,
                      "R/F % (Vorjahr)" = p$RF_Alt,
                      "Name (aktuell)" = p$Name_Neu,
                      "Klasse (aktuell)" = p$Klasse_Neu,
                      "WE % (aktuell)" = p$WE_Neu,
                      "R/F % (aktuell)" = p$RF_Neu,
                      "Veränderung R/F" = round(p$dRF, 1),
                      "Ähnlichkeit" = round(p$Aehnlichkeit, 2),
                      "Bewertung" = cohort_status_text(p$Status))
    dt <- datatable(anzeige,
                    selection = "multiple",
                    rownames = FALSE,
                    options = list(searching = TRUE,
                                   pageLength = 30,
                                   lengthMenu = c(10, 15, 30),
                                   scrollX = TRUE,
                                   language = list(url = "de-DE.json"))) %>%
      formatStyle("Bewertung",
                  backgroundColor = styleEqual(
                    c("Vorschlag (bitte prüfen)", "getrennt",
                      "nicht eindeutig (nicht zugeordnet)",
                      "kein Vorjahreswert", "neu in der Klasse"),
                    c("#FFF3CD", "#F8D7DA", "#FDE2E4", "#F2F2F2", "#F2F2F2")))
    dt
  })

  output$zuordnungStatus <- renderUI({
    k <- cohort_oder_null()
    if(is.null(k)) return(helpText("Kein Vergleich ausgewählt."))
    helpText(paste0(nrow(cohort_gematcht(k)), " Kinder sind zugeordnet (",
                    sum(k$paare$Status == "bestaetigt"), " davon bestätigt, ",
                    nrow(cohort_vorschlaege(k)), " Vorschläge offen). ",
                    "Ohne Vorjahreswert: ",
                    sum(k$paare$Status == "kein_partner"), ", neu: ",
                    sum(k$paare$Status == "nur_neu"), "."))
  })

  output$infobriefHinweis <- renderUI({
    if(!auswahl_moeglich()) {
      return(helpText("Für den Infobrief werden zwei Jahrgänge benötigt."))
    }
    helpText(paste0("Der Infobrief nutzt die oben bestätigten Zuordnungen. ",
                    "Offene Vorschläge werden nicht mitgezählt."))
  })

  # Zuordnung per Knopf bestaetigen oder trennen
  aendere_zuordnung <- function(aktion) {
    zeilen <- input$tabZuordnung_rows_selected
    if(length(zeilen) == 0) {
      showNotification("Bitte zuerst Zeilen in der Tabelle markieren.", type = "error")
      return()
    }
    k <- cohort_oder_null()
    if(is.null(k)) return()
    p <- k$paare[zeilen, , drop = FALSE]
    p <- p[!is.na(p$Name_Alt) & !is.na(p$Name_Neu), , drop = FALSE]
    if(nrow(p) == 0) {
      showNotification("Für die markierten Zeilen gibt es kein Paar.", type = "error")
      return()
    }
    quelle <- function(stufe) {
      treffer <- Filter(function(d) as.character(stufe) %in% as.character(d$stufen),
                        rv$dateien)
      if(length(treffer) == 0) NA_character_ else treffer[[1]]$name
    }
    for(i in seq_len(nrow(p))) {
      rv$entscheidungen <- setze_entscheidung(
        rv$entscheidungen, entscheidungen_datei(),
        name_alt = p$Name_Alt[i], klasse_alt = p$Klasse_Alt[i],
        name_neu = p$Name_Neu[i], klasse_neu = p$Klasse_Neu[i],
        aktion = aktion, aehnlichkeit = p$Aehnlichkeit[i],
        quelle_alt = quelle(k$stufe_alt), quelle_neu = quelle(k$stufe_neu),
        kohorte = kohorte_pruefsumme(k))
    }
    showNotification(paste0(nrow(p), " Zuordnung(en) ",
                            if(aktion == "ja") "bestätigt." else "getrennt."))
  }

  observeEvent(input$btZuordnungJa, {
    aendere_zuordnung("ja")
  })

  observeEvent(input$btZuordnungNein, {
    aendere_zuordnung("nein")
  })

  # Zuordnungen des aktuellen Vergleichs verwerfen (mit Rueckfrage)
  observeEvent(input$btZuordnungReset, {
    if(!auswahl_moeglich()) return()
    datei <- entscheidungen_datei()
    if(is.null(datei) || !fs::file_exists(datei)) {
      showNotification("Für diesen Vergleich ist keine Zuordnung gespeichert.",
                       type = "message")
      return()
    }
    showModal(modalDialog(
      title = "Zuordnungen zurücksetzen",
      paste0("Die gespeicherten Zuordnungen für den Vergleich Stufe ",
             input$siStufeAlt, " \u2192 ", input$siStufeNeu,
             " werden gelöscht (Datei: ", basename(datei), ")."),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Abbrechen"),
        actionButton("btZuordnungResetBestaetigt", "Zurücksetzen", class = "btn-danger")
      )
    ))
  })

  observeEvent(input$btZuordnungResetBestaetigt, {
    removeModal()
    datei <- entscheidungen_datei()
    if(!is.null(datei) && fs::file_exists(datei)) fs::file_delete(datei)
    rv$entscheidungen <- NULL
    showNotification("Zuordnungen wurden zurückgesetzt.")
  })

  #### Infobrief erstellen ####
  observeEvent(input$btInfobrief, {
    if(!auswahl_moeglich()) {
      showNotification("Bitte zwei Stufen laden und auswählen.", type = "error")
      return()
    }
    k <- cohort_oder_null()
    if(is.null(k) || nrow(cohort_gematcht(k)) == 0) {
      showNotification(paste0("Keine zugeordneten Kinder - bitte die Zuordnung ",
                              "in der Tabelle prüfen."), type = "error")
      return()
    }

    showNotification("Infobrief wird erstellt. Bitte warten...", duration = 30)
    ergebnis <- tryCatch({
      list(ok = TRUE,
           wert = create_infobrief(k,
                                   klassenleitung = input$infoKlassenleitung,
                                   absender = input$infoAbsender))
    }, error = function(e) list(ok = FALSE, wert = conditionMessage(e)))

    if(!ergebnis$ok) {
      msgs <- paste0("Fehler beim Erstellen des Infobriefs: ", ergebnis$wert)
      showNotification(msgs, type = "error", duration = NULL)
      output$text <- renderText(msgs)
      message(msgs)
      return()
    }

    msgs <- paste0("Infobrief erstellt: ", basename(ergebnis$wert$datei))
    showNotification(msgs, duration = 5)
    output$text <- renderText(msgs)
    utils::browseURL(createFilePath(NULL, ""))
  })

  #### Verteilungs plot ####
  observeEvent(list(rv$df, input$cbWEDiff, input$cbAllCombined, input$siPlotType,
                    input$siStufeAlt, input$siStufeNeu, rv$entscheidungen), {
    
    if(rv$inital) {
      return()
    }
    
    pRF <- createPlot(rv$df, x = "R/F-%", 
                      fill = "Kat.", 
                      xlab = "R/F-Wert in %", 
                      allCombined = input$cbAllCombined, 
                      type = input$siPlotType,
                      cohort = cohort_oder_null())
    
    if(!input$siPlotType %in% c("Entwicklung", "Verlauf")) {
      pRF <- pRF + 
        geom_vline(xintercept = 71.3, linetype = "dashed", color = "grey40") + 
        geom_vline(xintercept = 65, linetype = "dotted", color = "grey40")
    }
    
    output$histRF <-  renderPlotly(ggplotly(pRF))
    
    if(!input$cbWEDiff) {
      pWE <- createPlot(rv$df, 
                        x = "WE-%", 
                        fill = "Kat.", 
                        xlab = "WE-Wert in %",
                        allCombined = input$cbAllCombined,
                        type = input$siPlotType,
                        cohort = cohort_oder_null())
    } else  {
        pWE <-
          rv$df %>%
          mutate(diff = `WE-%` - `R/F-%`) %>%
          createPlot(x = "diff", 
                     fill = "Kat.", 
                     # Richtung wie gerechnet (WE minus R/F): der WE-Wert ist
                     # immer mindestens so hoch wie der R/F-Wert
                     xlab = "Differenz WE- und R/F-Wert in %",
                     allCombined = input$cbAllCombined,
                     type = input$siPlotType,
                     cohort = cohort_oder_null())
    }
    
    output$histWE <-  renderPlotly(ggplotly(pWE))
    
    # Mittelwert und Median unter Plots
    output$statsRF <- renderUI({
      createStatsText(rv$df, "R/F-%", "R/F", multiple = !input$cbAllCombined)
    })
    
    output$statsWE <- renderUI({
      # bei aktiver Checkbox "Differenz" zeigt das linke Diagramm die Differenz -
      # dann muessen auch die Kennzahlen dazu passen
      if(input$cbWEDiff) {
        createStatsText(rv$df %>% mutate(diff = `WE-%` - `R/F-%`),
                        "diff", "Differenz WE-R/F",
                        multiple = !input$cbAllCombined)
      } else {
        createStatsText(rv$df, "WE-%", "WE", multiple = !input$cbAllCombined)
      }
    })
    
    # Info text unter plots
    output$dynamicText <- renderUI({
      if(!input$siPlotType %in% c("Entwicklung", "Verlauf")) {
        tagList(
          div(h4("Gestrichelte Linie zeigt Referenzswert"), 
              style = "margin-left:15px; margin-right:15px"),
          div(h4("Gepunktete Linie zeigt unteren Normbereich"),
              style = "margin-left:15px; margin-right:15px")
        )
      }
    })

    # Vergleichstabelle je Kind (nur wenn zwei Stufen mit Zuordnung vorliegen)
    output$vergleichTabHinweis <- renderUI({
      tab <- tryCatch(vergleich_tabelle(cohort_oder_null()), error = function(e) NULL)
      if(is.null(tab)) {
        return(helpText(paste0("Für die Vergleichstabelle werden zwei Stufen mit ",
                               "mindestens einem zugeordneten Kind benötigt ",
                               "(Menü links: Vergleich von Stufe).")))
      }
      helpText(paste0("Stufe ", input$siStufeAlt, " \u2192 ", input$siStufeNeu,
                      ": ", nrow(tab), " Kinder mit zwei Messungen."))
    })

    output$tabVergleich <- renderDT({
      tab <- tryCatch(vergleich_tabelle(cohort_oder_null()), error = function(e) NULL)
      if(is.null(tab)) return(NULL)
      dt <- datatable(tab,
                      selection = "none",
                      rownames = FALSE,
                      options = list(searching = TRUE,
                                     pageLength = 30,
                                     lengthMenu = c(10, 15, 30),
                                     scrollX = TRUE,
                                     language = list(url = "de-DE.json"))) %>%
        formatStyle(c("\u0394 WE", "\u0394 R/F"),
                    color = styleInterval(c(-.Machine$double.eps, .Machine$double.eps),
                                          c("#B00020", "black", "#1E7B34")),
                    fontWeight = styleInterval(c(-20, 20), c("bold", "normal", "bold")))
      dt
    })
  })
}
