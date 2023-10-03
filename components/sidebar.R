###################
# sidebar.R
# 
# Create the sidebar menu options for the ui.
###################
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Anleitung", tabName = "anleitung", icon = icon("circle-question")),
    menuItem("Auswertung", tabName = "auswertung", icon = icon("th")),
    menuItem("Statistik", tabName = "statistik", icon = icon("chart-line")),
    menuItem("Erweitert", tabName = "experten", icon = icon("flask"))
  )
)
