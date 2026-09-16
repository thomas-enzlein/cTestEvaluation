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
  expect_true(is.na(qr))
  expect_no_error(qr_leer <- generate_qrcode(""))
  expect_true(is.na(qr_leer))
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
  qr <- generate_qrcode("https://example.org/uebungen")

  expect_type(qr, "list")
  expect_true(file.exists(qr$img))
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
