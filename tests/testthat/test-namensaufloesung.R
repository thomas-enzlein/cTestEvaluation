# Waechter gegen Paket-Verdeckungen (Paket F).
#
# Hintergrund: R loest Namen ueber den Suchpfad auf, und das ZULETZT geladene
# Paket steht vorne. In der alten req.txt stand `xfun` nach `shinyjs` - beide
# exportieren `js`, und xfun gewann. Dadurch brach `js$refocus()` im Server mit
# "Objekt des Typs 'closure' ist nicht indizierbar" ab, sobald man einen
# Schueler hinzufuegen wollte.
#
# Diese Tests pruefen, dass jeder kritische Name auf das ERWARTETE Paket zeigt.
# Kommt kuenftig ein Paket hinzu, das einen dieser Namen ebenfalls exportiert,
# schlaegt der Test an - und nicht erst die App beim Benutzen.

test_that("kritische Funktionsnamen zeigen auf das erwartete Paket", {
  erwartet <- c(
    filter = "package:dplyr", select = "package:dplyr", mutate = "package:dplyr",
    summarise = "package:dplyr", arrange = "package:dplyr", between = "package:dplyr",
    pull = "package:dplyr", bind_rows = "package:dplyr", ungroup = "package:dplyr",
    read_tsv = "package:readr", write_tsv = "package:readr",
    str_trim = "package:stringr", str_remove = "package:stringr",
    datatable = "package:DT", renderDT = "package:DT", formatStyle = "package:DT",
    styleEqual = "package:DT",
    helper = "package:shinyhelper", observe_helpers = "package:shinyhelper",
    runjs = "package:shinyjs", useShinyjs = "package:shinyjs",
    disable = "package:shinyjs", enable = "package:shinyjs",
    dashboardPage = "package:shinydashboard", box = "package:shinydashboard",
    selectInput = "package:shiny", actionButton = "package:shiny",
    fileInput = "package:shiny", showNotification = "package:shiny",
    renderPlotly = "package:plotly", ggplotly = "package:plotly",
    ggplot = "package:ggplot2", theme_set = "package:ggplot2",
    fct_reorder = "package:forcats", pivot_wider = "package:tidyr",
    # Achtung: officer wird nach readxl geladen und exportiert read_xlsx
    # ebenfalls. Der unqualifizierte Aufruf landet daher bei officer - die App
    # nutzt deshalb bewusst readxl::read_xlsx().
    read_xlsx = "package:officer", clean_names = "package:janitor",
    qr_code = "package:qrcode", flextable = "package:flextable",
    read_docx = "package:officer", body_add_docx = "package:officer"
  )

  # Der jeweils erste Treffer auf dem Suchpfad entscheidet in R. Die eigene
  # Umgebung (.GlobalEnv) steht dabei immer vorne und ist hier nicht gemeint -
  # geprueft wird, welches PAKET einen Namen zuerst anbietet.
  treffer <- vapply(names(erwartet), function(name) {
    gefunden <- setdiff(find(name), ".GlobalEnv")
    if (length(gefunden) == 0) NA_character_ else gefunden[1]
  }, character(1))

  expect_equal(treffer, erwartet)
})

test_that("die App benutzt kein `js`-Objekt mehr (Verdeckung ausgeschlossen)", {
  # seit Paket F laeuft der Fokus ueber shinyjs::runjs()
  # (Kommentarzeilen werden ignoriert - sie erklaeren den alten Zustand)
  ohne_kommentare <- function(datei) {
    zeilen <- readLines(datei, warn = FALSE)
    zeilen[!grepl("^\\s*#", zeilen)]
  }

  server_zeilen <- ohne_kommentare(file.path(projekt_root, "server.R"))
  expect_false(any(grepl("js$", server_zeilen, fixed = TRUE)))
  expect_true(any(grepl("shinyjs::runjs", server_zeilen, fixed = TRUE)))

  # extendShinyjs und der JS-Baustein sind entfallen
  body_zeilen <- ohne_kommentare(file.path(projekt_root, "components", "body.R"))
  expect_false(any(grepl("extendShinyjs", body_zeilen, fixed = TRUE)))
  funktionen <- ohne_kommentare(file.path(projekt_root, "functions", "functions.R"))
  expect_false(any(grepl("jscode", funktionen, fixed = TRUE)))
})
