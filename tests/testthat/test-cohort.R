# Paket E: Zuordnung der Kinder ueber die Jahrgaenge (functions/cohort.R).
#
# Grundlage: die Fixtures klasse_5c/klasse_6c (erfundene Namen) mit
#  - exakter Uebereinstimmung
#  - Tippfehler (Beispiel -> Beispel)
#  - Sonderzeichen-Variante (Meyer-Lange mit normalem und mit U+2010-Bindestrich)
#  - fehlendem Zweitnamen bei langem Namen (van der Berg Kessler)
#  - einem Kind ohne Vorjahreswert (Probst, Ella) und einem neuen Kind (Neu, Nino)

kohorte_fixture <- function() {
  df <- dplyr::bind_rows(lade_fixture("klasse_5c.tsv"), lade_fixture("klasse_6c.tsv"))
  build_cohort(df, 5, 6)
}

test_that("Namen werden ohne Annahme ueber die Schreibweise verglichen", {
  expect_equal(normalisiere_name("Muster, Max"), "max muster")
  expect_equal(normalisiere_name("Max Muster"), "max muster")
  expect_equal(normalisiere_name("Muster,Max"), "max muster")
  expect_equal(normalisiere_name("Muster , Max"), "max muster")
  expect_equal(normalisiere_name("Müller-Lange, Ben-Elias"),
               normalisiere_name("Mueller\u2010Lange, Ben\u2010Elias"))
  expect_equal(name_aehnlichkeit("Muster, Max", "Max Muster"), 1)
  expect_lt(name_aehnlichkeit("Muster, Max", "Beispiel, Ben"), 0.6)
  expect_equal(normalisiere_name(NA), "")
})

test_that("Kinder werden ueber die Stufen hinweg zugeordnet", {
  k <- kohorte_fixture()
  g <- cohort_gematcht(k)

  expect_true(all(c("Testmann, Anna", "Beispiel, Ben", "Krüger, Tim") %in% g$Name_Alt))

  anna <- g[g$Name_Alt == "Testmann, Anna", ]
  expect_equal(anna$Aehnlichkeit, 1)
  expect_equal(anna$WE_Alt, 85)
  expect_equal(anna$WE_Neu, 92.5)
  expect_equal(anna$dWE, 7.5)

  # Rueckgang bleibt negativ (Aydin: R/F 40 % -> 30 %)
  sara <- g[g$Name_Alt == "Aydin, Sara", ]
  expect_equal(sara$dRF, -10)

  # Tippfehler und Sonderzeichen verhindern die Zuordnung nicht
  expect_true("Beispel, Ben" %in% g$Name_Neu)
  expect_true(any(grepl("Meyer", g$Name_Neu)))
})

test_that("ein fehlender Zweitname ergibt einen Vorschlag statt Automatik", {
  k <- kohorte_fixture()
  vorschlaege <- cohort_vorschlaege(k)
  expect_true("van der Berg Kessler, Diego Armando" %in% vorschlaege$Name_Alt)
  # solange nicht bestaetigt, zaehlt das Kind nicht mit
  expect_false("van der Berg Kessler, Diego Armando" %in% cohort_gematcht(k)$Name_Alt)
})

test_that("jedes Kind wird hoechstens einmal zugeordnet", {
  g <- cohort_gematcht(kohorte_fixture())
  expect_false(any(duplicated(g$Name_Alt)))
  expect_false(any(duplicated(g$Name_Neu)))
})

test_that("Kinder ohne Vorjahreswert bzw. neue Kinder werden ausgewiesen", {
  k <- kohorte_fixture()
  p <- k$paare
  expect_true("Probst, Ella" %in% p$Name_Alt[p$Status == "kein_partner"])
  expect_true("Neu, Nino" %in% p$Name_Neu[p$Status == "nur_neu"])
  expect_equal(k$n_alt, 9)
  expect_equal(k$n_neu, 9)
  expect_equal(length(k$mehrdeutig), 0)
})

test_that("Entscheidungen bestaetigen und trennen Zuordnungen", {
  df <- dplyr::bind_rows(lade_fixture("klasse_5c.tsv"), lade_fixture("klasse_6c.tsv"))
  entscheidung_fuer <- function(p, aktion) {
    tibble::tibble(Name_Alt = p$Name_Alt, Klasse_Alt = p$Klasse_Alt,
                   Name_Neu = p$Name_Neu, Klasse_Neu = p$Klasse_Neu,
                   Aktion = aktion, Aehnlichkeit = p$Aehnlichkeit,
                   Zeitstempel = "2026-01-01 10:00:00")
  }

  # offenen Vorschlag bestaetigen
  vorschlag <- cohort_vorschlaege(build_cohort(df, 5, 6))[1, ]
  bestaetigt <- build_cohort(df, 5, 6, entscheidungen = entscheidung_fuer(vorschlag, "ja"))
  status <- bestaetigt$paare$Status[which(bestaetigt$paare$Name_Alt == vorschlag$Name_Alt)]
  expect_equal(status, "bestaetigt")
  expect_true(vorschlag$Name_Alt %in% cohort_gematcht(bestaetigt)$Name_Alt)

  # automatische Zuordnung trennen
  auto <- cohort_gematcht(build_cohort(df, 5, 6))[1, ]
  getrennt <- build_cohort(df, 5, 6, entscheidungen = entscheidung_fuer(auto, "nein"))
  expect_false(auto$Name_Alt %in% cohort_gematcht(getrennt)$Name_Alt)
  expect_true(auto$Name_Alt %in% getrennt$paare$Name_Alt[getrennt$paare$Status == "kein_partner"])
})

test_that("die Richtung folgt der Stufenzahl, nicht der alphabetischen Sortierung", {
  df <- dplyr::bind_rows(dplyr::mutate(lade_fixture("klasse_5c.tsv"), Klasse = "9a"),
                         dplyr::mutate(lade_fixture("klasse_6c.tsv"), Klasse = "10a"))
  k <- build_cohort(df, 9, 10)
  g <- cohort_gematcht(k)

  expect_true(all(g$Klasse_Alt == "9a"))
  expect_true(all(g$Klasse_Neu == "10a"))
  sara <- g[g$Name_Alt == "Aydin, Sara", ]
  expect_lt(sara$dRF, 0)
})

test_that("unbrauchbare Vergleiche werden abgelehnt", {
  df <- dplyr::bind_rows(lade_fixture("klasse_5c.tsv"), lade_fixture("klasse_6c.tsv"))
  expect_error(build_cohort(df, 5, 5), "Gleiche Stufe")
  expect_error(build_cohort(df, 5, 9), "nicht vorhanden")
  expect_error(build_cohort(df, NULL, 6), "beide Stufen")
})

test_that("namensgleiche Kinder werden nicht automatisch zugeordnet", {
  df <- dplyr::bind_rows(lade_fixture("klasse_5c.tsv"), lade_fixture("klasse_6c.tsv"))
  # zweites Kind mit demselben Namen in derselben Stufe (Namensgleichheit)
  doppelt <- df[df$Klasse == "6c" & df$Name == "Testmann, Anna", , drop = FALSE]
  df <- dplyr::bind_rows(df, doppelt)

  k <- build_cohort(df, 5, 6)
  expect_equal(k$n_neu, 10)
  expect_true("Testmann, Anna" %in% k$mehrdeutig)
  expect_false("Testmann, Anna" %in% cohort_gematcht(k)$Name_Alt)
  expect_false("Testmann, Anna" %in% cohort_gematcht(k)$Name_Neu)
  # das Kind bleibt in der Tabelle sichtbar, aber als "mehrdeutig" markiert
  expect_equal(sum(k$paare$Status == "mehrdeutig"), 2)   # je Stufe eine Zeile
  expect_true("Testmann, Anna" %in% k$paare$Name_Alt[k$paare$Status == "mehrdeutig"])
  expect_true("Testmann, Anna" %in% k$paare$Name_Neu[k$paare$Status == "mehrdeutig"])
})

test_that("Kennzahlen nutzen nur die gematchte Gruppe", {
  k <- kohorte_fixture()
  stat <- cohort_statistik(k)
  g <- cohort_gematcht(k)

  expect_true(all(c(5, 6) %in% stat$Stufe))
  expect_true("gesamt" %in% stat$Klasse)
  expect_equal(stat$mittel_WE[stat$Stufe == 6 & stat$Klasse == "gesamt"],
               round(mean(g$WE_Neu, na.rm = TRUE), 1))
  expect_equal(stat$mittel_RF[stat$Stufe == 5 & stat$Klasse == "gesamt"],
               round(mean(g$RF_Alt, na.rm = TRUE), 1))
  # jede Stufe hat eine Zeile je Klasse plus Gesamt
  expect_true(all(c("5c", "6c") %in% stat$Klasse))
})

test_that("Kinder unter dem Referenzwert werden gruppiert", {
  k <- kohorte_fixture()
  g <- cohort_gematcht(k)
  ref <- unter_referenz(k, grenze = 65)

  expect_equal(ref$grenze, 65)
  expect_equal(ref$n, nrow(g))
  expect_equal(ref$nachher_unter, sum(g$RF_Neu < 65, na.rm = TRUE))
  expect_equal(ref$vorher_unter, sum(g$RF_Alt < 65, na.rm = TRUE))
  expect_equal(ref$unveraendert_kritisch + ref$neu_kritisch +
                 ref$nicht_mehr_kritisch + ref$nie_kritisch, ref$n)
})

test_that("Verbesserungen und Rueckgaenge werden sortiert", {
  r <- cohort_ranking(kohorte_fixture(), top = 3, rueckgang = 10)

  expect_true(all(diff(r$verbesserungen$dRF) <= 0))
  expect_true(all(r$rueckgaenge$dRF <= -10))
  expect_true("Aydin, Sara" %in% r$rueckgaenge$Name_Alt)
})

test_that("der Dateiname der Zuordnungsdatei kommt aus den Daten", {
  # Stufen und Klassenbuchstaben - unabhaengig von Dateinamen, damit es auch
  # fuer getippte Daten passt
  expect_equal(zuordnung_dateiname(stufe_alt = 5, stufe_neu = 6,
                                   klassen_alt = "5c", klassen_neu = "6c"),
               "zuordnung_5c-6c.tsv")
  expect_equal(zuordnung_dateiname(stufe_alt = 5, stufe_neu = 6,
                                   klassen_alt = c("5b", "5a"), klassen_neu = c("6b", "6a")),
               "zuordnung_5a_5b-6a_6b.tsv")
  # ohne Klassenangabe bleibt ein erkennbarer Platzhalter
  expect_match(zuordnung_dateiname(5, 6), "^zuordnung_unbekannt-unbekannt\\.tsv$")
})

test_that("die Pruefsumme der Kohorte erkennt einen anderen Durchgang", {
  cohort <- kohorte_fixture()
  summe <- kohorte_pruefsumme(cohort)
  expect_type(summe, "character")
  expect_equal(kohorte_pruefsumme(kohorte_fixture()), summe)   # stabil

  df <- dplyr::bind_rows(lade_fixture("klasse_5c.tsv"), lade_fixture("klasse_6c.tsv"))
  df$Name[1] <- "Neumann, Nina"
  expect_false(kohorte_pruefsumme(build_cohort(df, 5, 6)) == summe)
})

test_that("Entscheidungen werden gespeichert und wieder angewendet", {
  withr::with_tempdir({
    pfad <- file.path(getwd(), "zuordnung_test.tsv")

    e <- setze_entscheidung(NULL, pfad, "Muster, Max", "5a", "Muster, Max", "6a", "ja", 0.95)
    expect_true(file.exists(pfad))
    gelesen <- read_entscheidungen(pfad)
    expect_equal(nrow(gelesen), 1)
    expect_equal(gelesen$Aktion, "ja")
    expect_equal(gelesen$Name_Neu, "Muster, Max")

    # dieselbe Zuordnung erneut entschieden: Eintrag wird ersetzt
    e <- setze_entscheidung(e, pfad, "Muster, Max", "5a", "Muster, Max", "6a", "nein", 0.95)
    expect_equal(nrow(e), 1)
    expect_equal(read_entscheidungen(pfad)$Aktion, "nein")

    # fehlende Datei ergibt eine leere Tabelle mit den richtigen Spalten
    leer <- read_entscheidungen(file.path(getwd(), "gibt_es_nicht.tsv"))
    expect_equal(nrow(leer), 0)
    expect_true(all(c("Name_Alt", "Klasse_Alt", "Name_Neu", "Klasse_Neu",
                      "Aktion", "Aehnlichkeit", "Zeitstempel") %in% colnames(leer)))
  })
})

test_that("Status wird in Text uebersetzt", {
  expect_equal(cohort_status_text("auto"), "automatisch zugeordnet")
  expect_equal(cohort_status_text("vorschlag"), "Vorschlag (bitte prüfen)")
  expect_equal(cohort_status_text(c("auto", "nur_neu")),
               c("automatisch zugeordnet", "neu in der Klasse"))
})
