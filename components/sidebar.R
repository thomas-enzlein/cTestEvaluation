###################
# sidebar.R
# 
# Create the sidebar menu options for the ui.
###################
sidebar <- dashboardSidebar(
  fluidRow(
    column(width =6,
           offset = 0,
           createActionButton("btSpeichern", "speichern", icon("floppy-disk")))
    ),
    fluidRow(
    column(width = 12,
           offset = 0,
           shiny::fileInput(inputId = "input_tsv", 
                            label = NULL, 
                            accept = ".tsv", 
                            buttonLabel = "Laden",
                            placeholder = "*.tsv auswählen"))
    ),
  # Stufenvergleich: gilt fuer das Entwicklungsdiagramm UND den Infobrief
  div(style = "padding-left: 15px; padding-right: 15px;",
      selectInput(inputId = "siStufeAlt",
                  label = "Vergleich von Stufe",
                  choices = NULL),
      selectInput(inputId = "siStufeNeu",
                  label = "bis Stufe",
                  choices = NULL),
      uiOutput("vergleichHinweis")
  ),
  sidebarMenu(
    menuItem("Auswertung", tabName = "auswertung", icon = icon("th")),
    menuItem("Statistik", tabName = "statistik", icon = icon("chart-line")),
    menuItem("Infobrief", tabName = "infobrief", icon = icon("file-lines")),
    menuItem("Elternbrief", tabName = "experten", icon = icon("envelope")),
    menuItem("Anleitung", tabName = "anleitung", icon = icon("circle-question"))
  )
  
  
)
