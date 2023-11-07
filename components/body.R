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
                   textInput(inputId = "schuelerName", label = "Name")
            ),
            column(width = 3,
                   createInputField("weWert", "WE-Wert")
            ),
            column(width = 3,
                   createInputField("rfWert", "R/F-Wert")
            )
          ),
          fluidRow(
            column(width = 6,
                   createActionButton("btHinzufuegen", "hinzufügen", icon("check"))
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
              div(dataTableOutput("tabUebersicht"), 
                  style = "margin-left:15px;
                           margin-right:15px")
            ), 
            fluidRow(
              column(width = 2,
                     createActionButton("btEntfernen", "entfernen", icon("trash"))
              )
            ),
            fluidRow(
              column(width = 2,
                     createActionButton("btSpeichern", "speichern", icon("floppy-disk"))
              )
            ),
            width = 12
        )
      )
    ),
    
    ### Statistik Tab ####
    tabItem(
      tabName = "statistik",
      box(title = "R/F-Verteilung der Schüler",
          fluidRow(
            createPlotOutput("histRF")
          ),
          fluidRow(
            createStatsOutput("statsRF"),
            div(h4("Gestrichelte Linie zeigt Referenzswert"), 
                style = "margin-left:15px;
                           margin-right:15px"),
            div(h4("Gepunktete Linie zeigt unteren Normbereich"),
                style = "margin-left:15px;
                           margin-right:15px"),
            
          ),
          width = 6
      ),
      box(title = "WE-Verteilung der Schüler",
          fluidRow(
            createPlotOutput("histWE")
          ),
          fluidRow(
            createStatsOutput("statsWE")
          ),
          fluidRow(
            column(width = 2,
                   selectInput(inputId = "WEplotType", 
                               choices = c("WE-%",
                                           "Differenz"), 
                               label = "Diagramm Typ", 
                               selected = "WE-%", 
                               multiple = FALSE))
          ),
          width = 6
      )
    ),
    tabItem(
      tabName = "experten",
      shiny::fileInput(inputId = "input_tsv", 
                       label = "Daten laden (*.tsv)", 
                       accept = ".tsv", 
                       buttonLabel = "Durchsuchen",
                       placeholder = "Nichts ausgewählt")
    )
  )
)