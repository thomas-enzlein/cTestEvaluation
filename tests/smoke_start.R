# Startet die App wirklich (wie beim Doppelklick) und prueft, dass sie hochkommt.
#
# Warum ein eigenes Skript und nicht Teil der testthat-Suite: testServer() wertet
# den Server in einem Reaktiv-Kontext aus. Ein Reaktiv-Wert, der oben in der
# Server-Funktion gelesen wird, faellt der Suite deshalb nicht auf - die App
# bricht dann erst beim echten Start ab. Dieses Skript laeuft im Release-Workflow
# VOR dem Setup-Bau, damit kein Paket entsteht, dessen App nicht startet.
#
# Aufruf:   Rscript tests/smoke_start.R [Sekunden] [App-Ordner]
# Rueckgabe: Exitcode 0 = App ist gestartet, 1 = nicht (mit Logausgabe)

args <- commandArgs(trailingOnly = TRUE)
wartezeit <- if (length(args) > 0) suppressWarnings(as.numeric(args[1])) else 60
if (is.na(wartezeit) || wartezeit < 10) wartezeit <- 60
app_ordner <- if (length(args) > 1) args[2] else "."
app_ordner <- normalizePath(app_ordner, mustWork = TRUE)
startzeit <- 8                       # so lange hat die App zum Hochkommen Zeit

# R-Quelltext vertraegt auf Windows keine einfachen Backslashes ("C:\Users" ->
# "\U ohne Hexziffern). Deshalb alle Pfade mit Schraegstrichen schreiben.
r_pfad <- function(p) shQuote(gsub("\\\\", "/", p))

log_datei <- tempfile("ctest_start_", fileext = ".log")
marke <- tempfile("ctest_start_ok_", fileext = ".txt")
kind_datei <- tempfile("ctest_start_kind_", fileext = ".R")
port <- httpuv::randomPort(min = 3000, max = 9000, host = "127.0.0.1")

writeLines(c(
  sprintf("setwd(%s)", r_pfad(app_ordner)),
  "suppressPackageStartupMessages(library(shiny))",
  "suppressPackageStartupMessages(library(later))",
  # Wenn die App laeuft, laeuft auch die Ereignisschleife - dann wird die Marke
  # geschrieben und die App wieder beendet. Bricht sie beim Start ab, passiert
  # das nie und der Elternprozess meldet den Fehler. (later rechnet in Sekunden.)
  sprintf("later::later(function() { writeLines(\"ok\", %s); shiny::stopApp() }, %d)",
          r_pfad(marke), startzeit),
  # Sicherheitsnetz: falls die Marke nicht geschrieben werden konnte, beendet
  # sich das Kind trotzdem - es soll nie im Hintergrund stehen bleiben.
  sprintf("later::later(function() shiny::stopApp(), %d)", startzeit + 30),
  sprintf("shiny::runApp(%s, port = %d, host = \"127.0.0.1\", launch.browser = FALSE)",
          r_pfad(app_ordner), port)
), kind_datei)

rscript <- file.path(R.home("bin"),
                     if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
system2(rscript, kind_datei, wait = FALSE, stdout = log_datei, stderr = log_datei)

ende <- Sys.time() + wartezeit
repeat {
  if (file.exists(marke)) break
  if (Sys.time() > ende) break
  Sys.sleep(1)
}

# Das Kind haelt die Logdatei offen, solange es laeuft - Lesefehler abfangen.
log_text <- tryCatch({
  if (file.exists(log_datei)) paste(readLines(log_datei, warn = FALSE), collapse = "\n") else ""
}, error = function(e) paste("(Log nicht lesbar:", conditionMessage(e), ")"))

if (file.exists(marke)) {
  cat("Start ok: die App ist hochgekommen (Ordner", app_ordner, ", Port", port, ")\n")
  # dem Kind Zeit geben, sich selbst zu beenden, dann aufraeumen
  Sys.sleep(2)
  unlink(c(kind_datei, log_datei, marke))
  quit(status = 0)
}

cat("FEHLER: die App ist nicht gestartet (Ordner", app_ordner, ", Port", port, ").\n")
cat("---- Ausgabe der App ----\n")
cat(if (nzchar(log_text)) log_text else "(keine Ausgabe)", "\n")
cat("-------------------------\n")
quit(status = 1)
