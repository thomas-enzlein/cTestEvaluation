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
  sidebarMenu(
    menuItem("Auswertung", tabName = "auswertung", icon = icon("th")),
    menuItem("Statistik", tabName = "statistik", icon = icon("chart-line")),
    menuItem("Elternbrief", tabName = "experten", icon = icon("envelope")),
    menuItem("Anleitung", tabName = "anleitung", icon = icon("circle-question"))
  )
  
  
)
