body <- dashboardBody(
  tabItems(
    tabItem(tabName = "anleitung",
            includeMarkdown("helpfiles/anleitung.md")
    ),
    
    ### Auswertungs Tab ####
    tabItem(
      useShinyjs(),
      # refocus Funktion
      extendShinyjs(text = jscode, 
                    functions = "refocus"),
      
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
                                      "Entwicklung"), 
                          label = "Diagramm Typ", 
                          selected = "Histogram", 
                          multiple = FALSE, width = "130px")
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
      )
    ),
    tabItem(
      tabName = "experten",
      column(width = 4,
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
      )
    )
  )
)