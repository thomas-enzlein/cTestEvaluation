source('./ui.R')
source('./server.R')

Sys.setlocale(category = "LC_ALL", locale = "German")
options(encoding="UTF-8")

shinyApp(ui, server)
