# Paket A: Verhalten ohne Internet.
#
# Der Linkdienst (tinyurl) wird dabei simuliert, damit die Tests unabhaengig
# vom Netz laufen: mal schlaegt die Anfrage fehl (kein Internet), mal
# antwortet der Dienst. Wichtig: ein fehlender Link darf den Brieflauf
# niemals abbrechen.

test_that("ohne Link wird kein Netz aufgerufen und nichts erzeugt", {
  kurzlink_zuruecksetzen()
  testthat::local_mocked_bindings(
    req_perform = function(...) stop("kein Netz im Test"),
    .package = "httr2"
  )
  expect_no_error(qr <- generate_qrcode(NULL))
  expect_true(is.na(qr$img))
  expect_true(is.na(qr$txt))
  expect_no_error(qr_leer <- generate_qrcode(""))
  expect_true(is.na(qr_leer$img))
})

test_that("shorten_url liefert ohne Internet NULL statt abzubrechen", {
  kurzlink_zuruecksetzen()
  testthat::local_mocked_bindings(
    req_perform = function(...) stop("kein Netz im Test"),
    .package = "httr2"
  )
  expect_no_error(kurz <- shorten_url("https://example.org/uebungen"))
  expect_null(kurz)
})

test_that("ohne Internet entsteht trotzdem ein QR-Code mit dem Originallink", {
  kurzlink_zuruecksetzen()
  testthat::local_mocked_bindings(
    req_perform = function(...) stop("kein Netz im Test"),
    .package = "httr2"
  )
  ziel <- file.path(tempdir(), paste0("qr_", Sys.getpid()))
  dir.create(ziel, showWarnings = FALSE)
  qr <- generate_qrcode("https://example.org/uebungen", zielordner = ziel)

  expect_type(qr, "list")
  # nur der Dateiname: knitr::include_graphics() rechnet absolute Pfade beim
  # Einbetten relativ zum Ausgabeordner um und findet die Datei dann nicht mehr
  expect_false(grepl("[/\\\\]", qr$img))
  expect_true(file.exists(file.path(ziel, qr$img)))
  expect_match(qr$txt, "https://example.org/uebungen", fixed = TRUE)
})

test_that("mit erreichbarem Dienst wird der Kurzlink verwendet", {
  kurzlink_zuruecksetzen()
  testthat::local_mocked_bindings(
    req_perform = function(...) structure(list(), class = "httr2_response"),
    resp_body_string = function(...) "https://tinyurl.com/test",
    .package = "httr2"
  )
  expect_equal(shorten_url("https://example.org/uebungen"), "https://tinyurl.com/test")
  # zweiter Aufruf nutzt den Zwischenspeicher (tinyurl begrenzt Anfragen)
  expect_equal(shorten_url("https://example.org/uebungen"), "https://tinyurl.com/test")

  qr <- generate_qrcode("https://example.org/andere")
  expect_match(qr$txt, "https://tinyurl.com/test", fixed = TRUE)
})

test_that("Fehlerantworten des Linkdienstes werden verworfen", {
  kurzlink_zuruecksetzen()
  testthat::local_mocked_bindings(
    req_perform = function(...) structure(list(), class = "httr2_response"),
    resp_body_string = function(...) "Error: invalid url",
    .package = "httr2"
  )
  expect_null(shorten_url("https://example.org/uebungen"))
})
