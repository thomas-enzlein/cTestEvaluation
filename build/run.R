# Startet die Shiny-App aus dem Installationsordner.
#
# Aufruf (aus run.bat):  R.exe --no-save --slave -f run.R --args "<AppDir>"
#
# Der Installationsordner (rd) enthaelt:
#   app\      Quellcode der App
#   R\        portables R mit allen Paketen
#   chrome\   portables Chrome (normale Chrome-Installation)
#   pandoc\   pandoc
#   run.bat, run.R, icon.ico

args <- commandArgs(trailingOnly = TRUE)
wd <- if (length(args) >= 1) args[1] else NA_character_
rd <- if (length(wd) == 1 && !is.na(wd)) dirname(wd) else NA_character_

if (length(wd) != 1 || is.na(wd) || !dir.exists(wd)) {
  stop("Bitte die Anwendung ueber run.bat starten.", call. = FALSE)
}
cat("App-Ordner: ", wd, "\n", sep = "")
cat("R: ", R.version.string, " (", R.home(), ")\n", sep = "")

# 1) Mitgelieferte Bibliothek zuerst, damit das portable R seine Pakete nutzt
lib_gebunden <- file.path(rd, "R", "library")
if (dir.exists(lib_gebunden)) {
  .libPaths(c(lib_gebunden, .libPaths()))
}

# 2) pandoc aus dem Installationsordner: kein Nachladen aus dem Internet noetig
pandoc_dir <- file.path(rd, "pandoc")
if (file.exists(file.path(pandoc_dir, "pandoc.exe"))) {
  Sys.setenv(RSTUDIO_PANDOC = pandoc_dir)
  cat("pandoc: ", file.path(pandoc_dir, "pandoc.exe"), "\n", sep = "")
} else if (requireNamespace("rmarkdown", quietly = TRUE) &&
           !rmarkdown::pandoc_available()) {
  cat("Hinweis: pandoc nicht gefunden - Elternbrief und Infobrief koennen nicht ",
      "erzeugt werden.\n", sep = "")
}

# 3) Fehlende Pakete nur im Ausnahmefall nachinstallieren. Im Normalfall ist
#    alles im Setup enthalten; ohne gebuendelte Bibliothek (Entwicklungsbetrieb
#    mit systemweitem R) ist Internet noetig.
req_datei <- file.path(wd, "req.txt")
if (file.exists(req_datei) && !dir.exists(lib_gebunden)) {
  req <- readLines(req_datei, warn = FALSE)
  req <- trimws(req[nzchar(trimws(req))])
  fehlend <- req[!(req %in% rownames(installed.packages()))]
  if (length(fehlend) > 0) {
    cat("Fehlende Pakete werden installiert: ", paste(fehlend, collapse = ", "), "\n",
        sep = "")
    try(install.packages(fehlend, lib = .libPaths()[1],
                         repos = "https://cloud.r-project.org"), silent = TRUE)
  }
}

# 4) Arbeitsverzeichnis ist der App-Ordner: ui.R, elternbrief/ und infobrief/
#    werden relativ dazu geladen
setwd(wd)

# 5) Browser: mitgeliefertes Chrome in einem eigenen Profil.
#    - eigenes Profil: das persoenliche Chrome-Profil bleibt unberuehrt
#    - --no-first-run / --no-default-browser-check: das Fenster zeigt auch beim
#      allerersten Start sofort die App (kein Willkommensbildschirm, keine Frage
#      nach dem Standardbrowser)
browser_kandidaten <- function() {
  k <- c(file.path(rd, "chrome", "chrome.exe"),
         file.path(rd, "chrome", "chrome-win64", "chrome.exe"))
  k[file.exists(k)]
}

profil_ordner <- function() {
  basis <- Sys.getenv("LOCALAPPDATA")
  if (!nzchar(basis)) basis <- rd
  ziel <- gsub("\\\\", "/", file.path(basis, "C-Test Auswertung", "chrome_profile"))
  if (!dir.exists(ziel)) dir.create(ziel, recursive = TRUE, showWarnings = FALSE)
  if (dir.exists(ziel)) ziel else rd
}

kandidaten <- browser_kandidaten()
oeffne <- if (length(kandidaten) > 0) {
  browser <- kandidaten[1]
  profil <- profil_ordner()
  cat("Browser: ", browser, "\n", sep = "")
  cat("Profil:  ", profil, "\n", sep = "")
  function(url) {
    system(paste0('"', browser, '"',
                  ' --app="', url, '"',
                  ' --user-data-dir="', profil, '"',
                  ' --no-first-run --no-default-browser-check'),
           wait = FALSE)
  }
} else {
  TRUE
}

cat("Falls kein Fenster erscheint: die Adresse aus der Zeile \"Listening on ...\" ",
    "in einem Browser oeffnen.\n", sep = "")

shiny::runApp(appDir = wd, launch.browser = oeffne)