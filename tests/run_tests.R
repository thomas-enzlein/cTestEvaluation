# Test-Runner fuer das C-Test-Auswertungstool.
#
# Das Projekt ist kein R-Paket, deshalb gibt es kein tests/testthat.R,
# sondern dieses Skript. Aufruf (aus dem Projektordner oder von ueberall):
#
#     Rscript tests/run_tests.R
#
# Test-Pakete stehen in req_dev.txt und sind NICHT Teil des Endprodukts:
#
#     install.packages(readLines("req_dev.txt"))

args <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(script_arg) > 0) {
  normalizePath(file.path(dirname(sub("^--file=", "", script_arg[1])), ".."), mustWork = TRUE)
} else {
  normalizePath(".", mustWork = TRUE)
}
setwd(root)
options(encoding = "UTF-8")

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Paket 'testthat' fehlt. Bitte zuerst install.packages(readLines('req_dev.txt')) ausfuehren.")
}

# pandoc finden: in RStudio ist RSTUDIO_PANDOC gesetzt, beim Aufruf ueber die
# Konsole nicht. Ohne pandoc wird der Elternbrief-Test uebersprungen.
if (!nzchar(Sys.getenv("RSTUDIO_PANDOC")) && !rmarkdown::pandoc_available()) {
  kandidaten <- c(
    file.path(Sys.getenv("ProgramFiles"), "RStudio", "resources", "app", "bin", "quarto", "bin", "tools"),
    file.path(Sys.getenv("LOCALAPPDATA"), "r-pandoc", "r-pandoc", c("3.9", "3.8.2", "3.5")),
    file.path(Sys.getenv("ProgramFiles"), "Pandoc")
  )
  treffer <- kandidaten[file.exists(file.path(kandidaten, "pandoc.exe"))]
  if (length(treffer) > 0) {
    Sys.setenv(RSTUDIO_PANDOC = treffer[1])
    message("pandoc gefunden unter: ", treffer[1])
  } else {
    message("Kein pandoc gefunden - der Elternbrief-Test wird uebersprungen.")
  }
}

library(testthat)
cat("Projektordner:", root, "\n")
cat("R:", R.version.string, "\n")
cat("pandoc verfuegbar:", rmarkdown::pandoc_available(), "\n")

# Optionaler Filter, z. B.: Rscript tests/run_tests.R rechenkern
filter <- commandArgs(trailingOnly = TRUE)
filter <- if (length(filter) > 0) filter[1] else NULL
if (!is.null(filter)) cat("Filter:", filter, "\n")
cat("\n")

res <- test_dir("tests/testthat",
                filter = filter,
                stop_on_failure = FALSE,
                reporter = "summary")

info <- as.data.frame(res)
fehlschlaege <- sum(info$failed) + sum(info$error)
cat("\n== Zusammenfassung ==\n")
cat(sprintf("Tests: %d | Erwartungen: %d | bestanden: %d | Fehlschlaege: %d | uebersprungen: %d | Dauer: %.1f s\n",
            nrow(info), sum(info$nb), sum(info$passed), fehlschlaege,
            sum(info$skipped), sum(info$real)))
if (sum(info$skipped) > 0) {
  cat("\nUebersprungene Tests dokumentieren bekannte Fehler (siehe Ausgabe oben);\n")
  cat("sie werden mit der Behebung in den Paketen D/E aktiviert.\n")
}

if (fehlschlaege > 0) {
  stop("Es gab Fehlschlaege.")
} else {
  cat("\nAlles gruen.\n")
}
