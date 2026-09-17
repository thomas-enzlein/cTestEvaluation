body <- dashboardBody(
  tabItems(
    tabItem(tabName = "anleitung",
            includeMarkdown("helpfiles/anleitung.md")
    ),
    
    ### Auswertungs Tab ####
    tabItem(
      useShinyjs(),
      tags$head(
        includeCSS("style.css")
      ),
      
      tabName = "auswertung",
      fluidRow(
        box(
          title = "Schüler hinzufügen",
          fluidRow(
            column(width = 6,
                   textInput(inputId = "schuelerName", label = "Name", placeholder = "Nachname, Vorname (wie in SchILD)")
            ),
            column(width = 3,
                   createInputField("weWert", "WE-Wert")
            ),
            column(width = 3,
                   createInputField("rfWert", "R/F-Wert")
            )
          ),
          fluidRow(
            column(width = 3,
                   createActionButton("btHinzufuegen", "hinzufügen", icon("check"))
            ),
            column(width = 3,
                   createActionButton("btEntfernen", "löschen", icon("trash"))
            ),
            column(width = 2,
                   selectInput(inputId = "numItems", 
                               label = "Anzahl Test-Items", 
                               choices = c(20, 26, 40, 60, 80, 100), 
                               selected = 40) %>%
                     helper(content = "numItems")
            ),
            column(width = 2,
                   selectInput(inputId = "klassenstufe", 
                               label = "Klassenstufe", 
                               choices = c("Bitte wählen" = "", 5:13)) 
            ),
            column(width = 2,
                   selectInput(inputId = "klBuchstabe", 
                               label = "Klasse", 
                               choices = c("Bitte wählen" = "", letters[1:8])) 
            )
          ),
          fluidRow(
            textOutput(outputId = "text") , 
            tags$head(tags$style("#text{color: red;
                                 font-size: 20px;
                                 font-style: bold;
                                 margin-left:15px;
                                 }"
            )
            )
            
          ),
          width = 12
          
        ),
        
        box(title = "Übersichtstabelle",
            fluidRow(
              column(width = 2,
              )
            ),
            fluidRow(
              div(dataTableOutput("tabUebersicht"), 
                  style = "margin-left:15px;
                           margin-right:15px")
            ),
            width = 12
        )
      )
    ),
    
    ### Statistik Tab ####
    tabItem(
      tabName = "statistik",
      # Boxen in einer Zeile: sonst "schweben" sie und die Tabellenbox laeuft
      # unter den hellen Hintergrund hinaus
      fluidRow(
        box(title = "WE-Verteilung der Schüler",
            fluidRow(
              createPlotOutput("histWE")
            ),
            fluidRow(
              createStatsOutput("statsWE")
            ),
            fluidRow(
              column(width = 6, offset = 1,
                     fluidRow(
                       column(width = 6,
                              checkboxInput(inputId = "cbWEDiff", 
                                            value = FALSE, 
                                            label = "Differenz")),
                       column(width = 6, 
                              checkboxInput(inputId = "cbAllCombined", 
                                            value = TRUE, 
                                            label = "Gesamtübersicht")
                       )
                     ),
                     fluidRow(
                       selectInput(inputId = "siPlotType", 
                                   choices = c("Histogramm",
                                               "Dichte",
                                               "Entwicklung",
                                               "Verlauf"), 
                                   label = "Diagramm Typ", 
                                   selected = "Histogramm", 
                                   multiple = FALSE, width = "180px")
                     ))
            ),
            width = 6
        ),
        box(title = "R/F-Verteilung der Schüler",
            fluidRow(
              createPlotOutput("histRF")
            ),
            fluidRow(
              createStatsOutput("statsRF"),
              uiOutput("dynamicText")
              
            ),
            width = 6
        ),
      ),
      ### Vergleich je Kind (nur wenn zwei Stufen zugeordnet werden koennen)
      fluidRow(
        box(title = "Vergleich je Kind (zwei Stufen)",
            fluidRow(
              column(width = 12, uiOutput("vergleichTabHinweis"))
            ),
            fluidRow(
              div(dataTableOutput("tabVergleich"),
                  style = "margin-left:15px;
                         margin-right:15px")
            ),
            width = 12
        )
      )
    ),
    ### Infobrief-Tab ####
    tabItem(
      tabName = "infobrief",
      fluidRow(
        box(
          title = "Lehrkräfte-Infobrief erstellen",
          fluidRow(
            column(width = 4,
                   shiny::textInput(inputId = "infoKlassenleitung",
                                    label = "Klassenleitung (optional)",
                                    placeholder = "6c")),
            column(width = 4,
                   shiny::textInput(inputId = "infoAbsender",
                                    label = "Absender (optional)",
                                    placeholder = "Max Mustermann")),
            column(width = 4,
                   createActionButton("btInfobrief", "Infobrief erstellen", icon("file-lines")))
          ),
          fluidRow(
            column(width = 12, uiOutput("infobriefHinweis"))
          ),
          width = 12
        ),
        box(
          title = "Zuordnung zum Vorjahr",
          fluidRow(
            column(width = 12, uiOutput("zuordnungHinweis"))
          ),
          fluidRow(
            div(dataTableOutput("tabZuordnung"),
                style = "margin-left:15px;
                         margin-right:15px")
          ),
          fluidRow(
            column(width = 3,
                   createActionButton("btZuordnungJa", "Zuordnung bestätigen", icon("check"))),
            column(width = 3,
                   createActionButton("btZuordnungNein", "trennen", icon("xmark"))),
            column(width = 3,
                   createActionButton("btZuordnungReset", "Zuordnungen zurücksetzen",
                                      icon("rotate-left"))),
            column(width = 3, uiOutput("zuordnungStatus"))
          ),
          width = 12
        )
      )
    ),
    tabItem(
      tabName = "experten",
      fluidRow(
        column(width = 4,
               box(
                 title = "Elternbriefe erstellen",
                 column(width = 12,
                        fluidRow(
                          shiny::textInput(inputId = "lehrername", 
                                           label = "Name des Lehrers",
                                           placeholder = "Max Mustermann")  
                        ),
                        fluidRow(
                          shiny::textInput(inputId = "signatur", 
                                           label = "Signatur/Position des Lehrers (optional)",
                                           placeholder = "Abteilungsleitung I")  
                        ),
                        fluidRow(
                          shiny::textInput(inputId = "qrLink", 
                                           label = "Link zu Übungen (wird als QR-Code eingefügt, optional)",
                                           placeholder = "https://www...")  
                        ),
                        fluidRow(
                          shiny::actionButton(inputId = "btBrief", 
                                              icon = icon("envelope"), 
                                              label = "Elternbriefe erstellen")  
                        )
                 ),
                 width = 12
               )
        ),
        column(width = 8,
               box(
                 title = "Vorlagen und Einstellungen",
                 fluidRow(
                   column(width = 12, uiOutput("vorlagenHinweis"))
                 ),
                 fluidRow(
                   column(width = 4,
                          createActionButton("btVorlageOeffnen", "Briefvorlage öffnen",
                                             icon("file-word"))),
                   column(width = 4,
                          createActionButton("btVorlagenOrdner", "Vorlagen-Ordner öffnen",
                                             icon("folder-open"))),
                   column(width = 4,
                          createActionButton("btEinstellungen", "Einstellungen öffnen",
                                             icon("gear")))
                 ),
                 width = 12
               )
        )
      )
    )
  )
)