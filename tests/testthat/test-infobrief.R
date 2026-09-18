# Paket E/G: Lehrkraefte-Infobrief.
#
# Der Fliesstext steht in infobrief/kopf.Rmd, abschnitt.Rmd und abschluss.Rmd.
# Hier werden die Zulieferungen geprueft (Zahlen, Farben, Tabellen, Ranglisten)
# plus ein echtes docx-Rendering.

infobrief_fixture <- function() {
  df <- dplyr::bind_rows(lade_fixture("klasse_5c.tsv"), lade_fixture("klasse_6c.tsv"))
  build_cohort(df, 5, 6)
}

test_that("Zahlen werden im deutschen Format ausgegeben", {
  expect_equal(de_zahl(12.5), "12,5")
  expect_equal(de_zahl(65, 0), "65")
  expect_equal(de_vz(-3), "-3,0")
  expect_equal(de_vz(7.5), "+7,5")
  expect_equal(de_zahl(NA_real_), "-")
  # teils fehlende Werte: jede Zelle einzeln, kein "NA" in der Tabelle
  expect_equal(de_zahl(c(12.5, NA, 3), 1), c("12,5", "-", "3,0"))
  expect_equal(de_vz(c(7.5, NA, -2)), c("+7,5", "-", "-2,0"))
  expect_equal(kind_text(1), "1 Kind")
  expect_equal(kind_text(3), "3 Kinder")
  expect_equal(kind_dativ(3), "3 Kindern")
})

test_that("Werte werden gruen bzw. rot und ab 20 zusaetzlich fett gesetzt", {
  gruen <- wert_run(22.5)
  expect_equal(gruen$pr$color, "#1E7B34")
  expect_true(gruen$pr$bold)
  expect_match(gruen$value, "^\\+22,5", perl = TRUE)

  rot <- wert_run(-30)
  expect_equal(rot$pr$color, "#B00020")
  expect_true(rot$pr$bold)

  schwach <- wert_run(-5)
  expect_equal(schwach$pr$color, "#B00020")
  expect_false(schwach$pr$bold)

  knapp <- wert_run(20)          # ab 20 wird fett
  expect_true(knapp$pr$bold)
  expect_match(knapp$value, "%$")
})

test_that("die Anrede ist neutral und optional an die Klassenleitung gerichtet", {
  expect_equal(infobrief_anrede(""), "Liebe Kollegin, lieber Kollege,")
  expect_equal(infobrief_anrede(NULL), "Liebe Kollegin, lieber Kollege,")
  expect_equal(infobrief_anrede("6c"), "Liebe Klassenleitung der 6c,")
})

test_that("der Bericht liefert Kopf, Abschnitte und Anhang", {
  bericht <- infobrief_bericht(infobrief_fixture())

  expect_equal(bericht$stufe_alt, 5)
  expect_equal(bericht$stufe_neu, 6)
  expect_equal(bericht$n_gematcht, nrow(cohort_gematcht(infobrief_fixture())))
  expect_equal(bericht$n_nur_neu, 1)      # Neu, Nino
  expect_equal(bericht$n_ohne_partner, 1) # Probst, Ella
  expect_match(bericht$zusatz_neu, "neu hinzugekommen")
  expect_match(bericht$zusatz_partner, "kein Vorjahreswert")

  # ein Abschnitt fuer den Buchstaben c
  expect_length(bericht$abschnitte, 1)
  abschnitt <- bericht$abschnitte[[1]]
  expect_equal(abschnitt$buchstabe, "c")
  expect_equal(abschnitt$klassen_kombi, "5c \u2192 6c")
  expect_equal(abschnitt$n, bericht$n_gematcht)
  expect_equal(abschnitt$rueckgang_schwelle, "10")

  # Kennzahlen-Tabelle: eine Zeile je Klasse, n mit Werten in Klammern,
  # Mittelwert und SD zusammen, keine Stufen-/Median-Spalte - darunter die
  # Differenzzeile mit der Veraenderung der Mittelwerte
  expect_equal(colnames(abschnitt$tabelle),
               c("Klasse", "n (mit Werten)",
                 "WE % (Mittel \u00b1 SD)", "R/F % (Mittel \u00b1 SD)"))
  expect_equal(abschnitt$tabelle$Klasse, c("5c", "6c", "Differenz"))
  expect_equal(abschnitt$tabelle[["n (mit Werten)"]][1:2], c("7 (7)", "7 (7)"))
  expect_match(abschnitt$tabelle[["WE % (Mittel \u00b1 SD)"]][1], "^[0-9]+,[0-9] \u00b1[0-9]+,[0-9]$")
  expect_match(abschnitt$tabelle[["R/F % (Mittel \u00b1 SD)"]][1], "^[0-9]+,[0-9] \u00b1[0-9]+,[0-9]$")
  expect_match(abschnitt$tabelle[["WE % (Mittel \u00b1 SD)"]][3], "^[+-][0-9]+,[0-9]$")
  expect_match(abschnitt$tabelle[["R/F % (Mittel \u00b1 SD)"]][3], "^[+-][0-9]+,[0-9]$")

  # der Satz zum unteren Normbereich steht nicht mehr im Abschnitt, sondern
  # ueber der Anhangstabelle (wie im Stand-Brief) - mit den Zahlen des Briefes
  erwartet <- unter_referenz(infobrief_fixture())
  expect_null(abschnitt$referenz)
  expect_equal(bericht$referenz$grenze, "65")
  expect_equal(bericht$referenz$n, erwartet$n)
  expect_equal(bericht$referenz$unter, erwartet$nachher_unter)
  expect_match(bericht$referenz$zusatz, "im Vorjahr zu|neu hinzugekommen|wieder erreicht")

  # Fettmarkierung in der Reihenfolge der Anhangstabelle: genau die Kinder,
  # deren aktueller R/F-Wert unter der Grenze liegt. Gelesen wird das letzte
  # Wort der Zelle (der Wert hinter dem Pfeil), Komma als Dezimaltrenner.
  markierung <- bericht$lese_unten
  expect_length(markierung, nrow(bericht$lesetabelle))
  roh <- sub("^.* ", "", bericht$lesetabelle[["R/F % (5 \u2192 6)"]])
  werte <- suppressWarnings(as.numeric(sub(",", ".", roh, fixed = TRUE)))
  expect_equal(sum(markierung), sum(!is.na(werte) & werte < 65))
  expect_gt(sum(markierung), 0)
})

test_that("Top-Verbesserungen und schwaechste Entwicklung werden aufbereitet", {
  abschnitt <- infobrief_bericht(infobrief_fixture())$abschnitte[[1]]

  # EINE Tabelle: ausgewaehlt werden die 3 groessten Zugaenge und die 3
  # schwaechsten Entwicklungen; in der Tabelle stehen die Kinder unter dem
  # Normbereich zuerst, innerhalb jeder Gruppe die groesste Veraenderung zuerst
  rang <- abschnitt$rangliste
  expect_true(!is.null(rang))
  expect_equal(nrow(rang), 6)
  expect_equal(colnames(rang),
               c("Name", "WE % (5 \u2192 6)", "\u0394 WE", "R/F % (5 \u2192 6)", "\u0394 R/F"))
  # vorher/aktuell stehen zusammen in einer Spalte
  expect_match(rang[["WE % (5 \u2192 6)"]][1], "^[0-9]+,[0-9] \u2192 [0-9]+,[0-9]$")
  expect_false("Bewertung" %in% colnames(rang))
  expect_false("\u00c4hnlichkeit" %in% colnames(rang))

  # Fettmarkierung: aktueller R/F-Wert unter dem unteren Normbereich
  mark <- abschnitt$rangliste_unten
  expect_length(mark, nrow(rang))
  aktuell <- suppressWarnings(as.numeric(gsub(",", ".",
                                              sub("^.* ", "", rang[["R/F % (5 \u2192 6)"]]))))
  expect_equal(unname(mark), !is.na(aktuell) & aktuell < 65)
  expect_gt(sum(mark), 0)
  # erst die Kinder unter der Grenze, dann die uebrigen: die Markierung laeuft
  # von TRUE nach FALSE und nie zurueck
  expect_true(all(diff(as.integer(mark)) <= 0))
  expect_true(all(diff(as.integer(mark)) <= 0) && any(mark) && any(!mark))
  # innerhalb jeder Gruppe: groesste Veraenderung zuerst
  delta <- rang[["\u0394 R/F"]]
  for (block in list(which(mark), which(!mark))) {
    if (length(block) > 1) expect_true(all(diff(delta[block]) <= 0))
  }
  # der Fett-Hinweis haengt an der Ueberschrift
  expect_match(abschnitt$rangliste_titel,
               "Die größten Verbesserungen/schwächste Entwicklung (Fett: unter Normbereich)",
               fixed = TRUE)
})

test_that("die schwaechsten zuerst: weiterhin unter dem Normbereich, dann Rueckgaenge", {
  cohort <- infobrief_fixture()
  rang <- cohort_rangliste(cohort, top = 0, schwach = 10)
  schwach <- rang$schwach

  g <- cohort_gematcht(cohort)
  aktuell <- schwach[["R/F % (5 \u2192 6)"]]
  aktuell <- suppressWarnings(as.numeric(gsub(",", ".",
                                              sub(".*\u2192 ", "", aktuell))))
  unter <- schwach$Name[aktuell < 65]
  ueber <- schwach$Name[aktuell >= 65]
  # alle Kinder unter dem Normbereich stehen vor allen ueber dem Normbereich
  if (length(unter) > 0 && length(ueber) > 0) {
    expect_lt(max(match(unter, schwach$Name)), min(match(ueber, schwach$Name)))
  }
  # innerhalb der Gruppe unter dem Normbereich zuerst "schon im Vorjahr betroffen"
  beide <- intersect(unter, g$Name_Neu[!is.na(g$RF_Alt) & g$RF_Alt < 65])
  nur_jetzt <- setdiff(unter, beide)
  if (length(beide) > 0 && length(nur_jetzt) > 0) {
    expect_lt(max(match(beide, schwach$Name)), min(match(nur_jetzt, schwach$Name)))
  }
})

test_that("die Lesetabelle zeigt alle Kinder mit ihren vorhandenen Werten", {
  k <- infobrief_fixture()
  # so steht sie im Anhang: ohne Hinweisspalte (die Seite ist zu schmal)
  tab <- vergleich_tabelle(k, mit_hinweis = FALSE)

  expect_equal(colnames(tab),
               c("Name", "Klasse", "WE % (5 \u2192 6)", "\u0394 WE",
                 "R/F % (5 \u2192 6)", "\u0394 R/F"))
  # alle Kinder aus beiden Jahrgaengen, nicht nur die zugeordneten
  expect_equal(nrow(tab), nrow(k$paare))
  expect_gt(nrow(tab), nrow(cohort_gematcht(k)))
  expect_equal(tab$Name, sort(tab$Name))               # alphabetisch
  expect_false(any(grepl("Bewertung|Ähnlichkeit", colnames(tab))))

  # die Wertspalten enthalten nie ein "NA" - fehlende Seiten stehen als Strich
  werte <- unlist(tab[, c("WE % (5 \u2192 6)", "R/F % (5 \u2192 6)")], use.names = FALSE)
  expect_false(any(grepl("NA", werte, fixed = TRUE)))
  expect_true(all(grepl("\u2192", werte)))

  # einseitige Kinder (kein Vergleichswert): Wert sichtbar
  einseitig <- tab[is.na(tab[["\u0394 WE"]]) & is.na(tab[["\u0394 R/F"]]), , drop = FALSE]
  expect_gt(nrow(einseitig), 0)
  expect_true(any(grepl("\u2192 -$", einseitig[["WE % (5 \u2192 6)"]])))
  # Klassenspalte nennt, was vorhanden ist - kein "5c -> NA"
  expect_false(any(grepl("NA", einseitig$Klasse, fixed = TRUE)))

  # die vollstaendige Fassung (App-Tabelle, Excel) nennt zusaetzlich den Grund
  voll <- vergleich_tabelle(k)
  expect_true("Hinweis" %in% colnames(voll))
  expect_equal(voll[, setdiff(colnames(voll), "Hinweis")], tab)
  expect_true(all(nzchar(voll$Hinweis[
    is.na(voll[["\u0394 WE"]]) & is.na(voll[["\u0394 R/F"]])])))
  # Hinweise stammen nur aus der bekannten Liste (leer = alles vorhanden)
  expect_true(all(voll$Hinweis %in% c(
    "", "Vorschlag (bitte prüfen)", "getrennt", "kein Vorjahreswert",
    "neu in der Klasse", "nicht eindeutig (nicht zugeordnet)",
    "nicht teilgenommen (5. Klasse)", "nicht teilgenommen (6. Klasse)",
    "nicht teilgenommen (5. und 6. Klasse)")))

  # ohne Vergleich gibt es keine Tabelle
  expect_null(vergleich_tabelle(NULL))
})

test_that("die Klassentabelle endet mit einer farbigen Differenzzeile", {
  k <- infobrief_fixture()
  abschnitt <- infobrief_abschnitt(k, seq_len(nrow(k$paare)), "c")
  tab <- abschnitt$tabelle
  differenz <- abschnitt$tabelle_differenz

  # zwei Klassenzeilen plus die Differenzzeile
  expect_equal(tab$Klasse[nrow(tab)], "Differenz")
  expect_equal(nrow(tab), 3)
  expect_equal(tab[["n (mit Werten)"]][nrow(tab)], "-")

  # Differenz = Mittel der 6. Stufe minus Mittel der 5. Stufe
  stat <- cohort_statistik(cohort_teil(k, seq_len(nrow(k$paare))))
  mittel <- function(stufe) {
    z <- stat[stat$Stufe == stufe & stat$Klasse == "gesamt", , drop = FALSE]
    c(we = z$mittel_WE[1], rf = z$mittel_RF[1])
  }
  erwartet <- mittel(6) - mittel(5)
  expect_equal(unname(differenz), unname(erwartet))
  expect_match(tab[["WE % (Mittel \u00b1 SD)"]][nrow(tab)], "^[+-]")
  expect_match(tab[["R/F % (Mittel \u00b1 SD)"]][nrow(tab)], "^[+-]")

  # Farbgebung wie bei den Einzelkindern
  ft <- tabelle_a(tab, differenz)
  farben <- ft$body$styles$text$color$data
  spalte_we <- which(colnames(tab) == "WE % (Mittel \u00b1 SD)")
  spalte_rf <- which(colnames(tab) == "R/F % (Mittel \u00b1 SD)")
  erwartete_farbe <- function(wert) if (wert > 0) "#1E7B34" else "#B00020"
  expect_equal(as.character(farben[nrow(tab), spalte_we]),
               as.character(erwartete_farbe(differenz[["we"]])))
  expect_equal(as.character(farben[nrow(tab), spalte_rf]),
               as.character(erwartete_farbe(differenz[["rf"]])))
  # die Klassenzeilen darueber bleiben ungefaerbt
  expect_false(identical(as.character(farben[1, spalte_we]),
                         as.character(erwartete_farbe(differenz[["we"]]))))
})

test_that("die Kennzahlen rechnen weiter nur mit zugeordneten Kindern", {
  k <- infobrief_fixture()
  tab <- vergleich_tabelle(k)
  bericht <- infobrief_bericht(k)

  # die Liste ist laenger als die Auswertungsgruppe
  expect_gt(nrow(tab), bericht$n_gematcht)
  stat <- cohort_statistik(k)
  expect_true(all(stat$n_werte <= bericht$n_gematcht))
})

test_that("Kindernamen werden mit Semikolon getrennt (Komma gehoert zum Namen)", {
  expect_equal(namen_liste(c("Dauer, Anna", "Albers, Ben")), "Albers, Ben; Dauer, Anna")
  expect_equal(namen_liste("Muster, Max"), "Muster, Max")
  expect_equal(namen_liste(character(0)), "")
  expect_equal(namen_liste(c("B, B", NA)), "B, B")
})

test_that("der Bericht weist neue Kinder, fehlende Partner und offene Vorschlaege aus", {
  bericht <- infobrief_bericht(infobrief_fixture())
  hinweise <- paste(bericht$hinweise, collapse = " ")

  expect_match(hinweise, "Neu in der Klasse", fixed = TRUE)
  expect_match(hinweise, "Neu, Nino", fixed = TRUE)
  expect_match(hinweise, "Kein Partner im aktuellen Jahrgang", fixed = TRUE)
  expect_match(hinweise, "Probst, Ella", fixed = TRUE)
  expect_match(hinweise, "Noch nicht bestätigte Zuordnungen", fixed = TRUE)
  expect_match(hinweise, "van der Berg Kessler", fixed = TRUE)
  # ein normaler Stufenwechsel (5c -> 6c) ist kein Klassenwechsel
  expect_false(grepl("Klassenwechsel", hinweise, fixed = TRUE))
  # nicht teilgenommen: nur die Stufe mit fehlenden Werten wird genannt
  expect_match(hinweise, "Nicht teilgenommen: 1 Kind in der 5. Klasse.", fixed = TRUE)
})

test_that("ein echter Infobrief entsteht als docx mit Tabellen und Farben", {
  skip_if_not(rmarkdown::pandoc_available(), "pandoc nicht gefunden")

  withr::with_tempdir({
    dir.create("infobrief")
    dateien <- list.files(file.path(projekt_root, "infobrief"), full.names = TRUE)
    file.copy(dateien, "infobrief", recursive = TRUE)
    # Persoenlicher Vorlagenordner: sonst wuerde eine vom Benutzer angepasste
    # Vorlage unter "Dokumente" die Testergebnisse veraendern
    withr::local_options(ctest.outdir.fallback = file.path(getwd(), "benutzer"))

    k <- infobrief_fixture()
    ergebnis <- NULL
    # Paket L: der Fortschritt wird waehrend des Renderns gemeldet
    meldungen <- list()
    utils::capture.output(
      suppressMessages(
        ergebnis <- create_infobrief(k, klassenleitung = "6c", absender = "Test, Tina",
                                     fortschritt = function(anteil, text) {
                                       meldungen[[length(meldungen) + 1]] <<-
                                         list(anteil = anteil, text = text)
                                     })
      )
    )

    anteile <- vapply(meldungen, function(m) m$anteil, numeric(1))
    texte <- vapply(meldungen, function(m) m$text, character(1))
    expect_gt(length(anteile), 1)
    expect_true(all(diff(anteile) >= 0))            # nur vorwaerts
    expect_equal(anteile[length(anteile)], 1)        # endet bei 100 %
    expect_match(paste(texte, collapse = " "), "Word-Datei wird geschrieben",
                 fixed = TRUE)

    expect_true(file.exists(ergebnis$datei))
    expect_match(basename(ergebnis$datei), "^Infobrief_5-6_5c_6c\\.docx$")

    # Die Teile (Kopf, Abschnitte, Abschluss) werden wie beim Elternbrief per
    # altChunk eingebettet - Text und Formatierung stecken daher teilweise in
    # eingebetteten Dokumenten. Deshalb alle Teile zusammen auswerten.
    teile_auslesen <- function(datei) {
      z <- tempfile("teile"); dir.create(z)
      utils::unzip(datei, exdir = z)
      xmls <- character(0)
      for (teil in list.files(z, recursive = TRUE, full.names = TRUE,
                              pattern = "\\.docx$")) {
        zz <- tempfile("ein"); dir.create(zz)
        utils::unzip(teil, exdir = zz)
        pfad <- file.path(zz, "word", "document.xml")
        if (file.exists(pfad)) {
          xmls <- c(xmls, paste(readLines(pfad, warn = FALSE, encoding = "UTF-8"),
                                collapse = ""))
        }
      }
      haupt <- file.path(z, "word", "document.xml")
      if (file.exists(haupt)) {
        xmls <- c(paste(readLines(haupt, warn = FALSE, encoding = "UTF-8"),
                        collapse = ""), xmls)
      }
      alle_xml <- paste(xmls, collapse = "")
      list(xml = alle_xml,
           text = gsub("\\s+", " ", gsub("<[^>]+>", " ", alle_xml)))
    }
    geholt <- teile_auslesen(ergebnis$datei)
    text <- geholt$text
    alle_xml <- geholt$xml

    expect_match(text, "Lernentwicklung im Fach Deutsch", fixed = TRUE)
    expect_match(text, "Liebe Klassenleitung der 6c", fixed = TRUE)
    expect_match(text, "Kohorte c", fixed = TRUE)
    expect_match(text, "n (mit Werten)", fixed = TRUE)
    expect_match(text, "Veränderungen sind farbig hervorgehoben.", fixed = TRUE)
    expect_false(grepl("Deutliche Rückgänge", text, fixed = TRUE))
    expect_false(grepl("grün bei Verbesserung", text, fixed = TRUE))
    expect_match(text, "Testmann, Anna", fixed = TRUE)
    expect_match(text, "Aydin, Sara", fixed = TRUE)
    expect_match(text, "Mit freundlichen Grüßen", fixed = TRUE)
    expect_match(text, "Anhang: Vergleich je Kind", fixed = TRUE)
    expect_match(text, "Test, Tina", fixed = TRUE)
    # der Mittelwert-Absatz und die Prosa-Listen sind entfallen
    expect_false(grepl("Im Mittel erreichte die Gruppe", text, fixed = TRUE))
    expect_false(grepl("Die größten Verbesserungen im R/F-Wert:", text, fixed = TRUE))
    expect_false(grepl("In dieser Gruppe liegen für", text, fixed = TRUE))
    expect_false(grepl("%-Punkte", text, fixed = TRUE))
    # Normbereich-Absatz nennt nur Zahlen, keine Namen - und steht ueber der
    # Anhangstabelle, mit dem kurzen Fett-Hinweis in Klammern
    expect_match(text, "Bei 4 Kindern trifft das schon im Vorjahr zu.", fixed = TRUE)
    expect_match(text, "Unterhalb des unteren Normbereichs (R/F-Wert unter 65 %) liegen aktuell",
                 fixed = TRUE)
    expect_match(text, "(Fett: unter Normbereich)", fixed = TRUE)
    # die Ueberschrift der Rangliste traegt denselben Hinweis
    expect_match(text, "Die größten Verbesserungen/schwächste Entwicklung (Fett: unter Normbereich)",
                 fixed = TRUE)
    expect_false(grepl("im Vorjahr zu (", text, fixed = TRUE))
    # der Satz steht nach dem Gruss, also auf der Anhang-Seite
    expect_lt(regexpr("Unterhalb des unteren Normbereichs", text, fixed = TRUE)[1],
              regexpr("Anhang: Vergleich je Kind", text, fixed = TRUE)[1])
    expect_gt(regexpr("Unterhalb des unteren Normbereichs", text, fixed = TRUE)[1],
              regexpr("Mit freundlichen Grüßen", text, fixed = TRUE)[1])

    # ein einziges Dokument: keine eingebetteten Teildokumente, keine
    # zusaetzlichen Abschnittseigenschaften (Ursache leerer Seiten).
    # Zwei Seitenumbrueche: vor den Hinweisen und vor dem Anhang.
    z <- tempfile("pruef"); dir.create(z)
    utils::unzip(ergebnis$datei, exdir = z)
    expect_length(list.files(z, recursive = TRUE, pattern = "\\.docx$"), 0)
    expect_equal(sum(gregexpr("<w:sectPr", geholt$xml, perl = TRUE)[[1]] > 0), 1)
    expect_equal(sum(gregexpr('<w:br w:type="page"', geholt$xml, perl = TRUE)[[1]] > 0), 2)

    # Farben (gruen/rot), Fett und echte Word-Tabellen
    expect_true(grepl("1E7B34", alle_xml, ignore.case = TRUE))
    expect_true(grepl("B00020", alle_xml, ignore.case = TRUE))
    expect_true(grepl("<w:b", alle_xml))
    expect_true(grepl("<w:tbl", alle_xml))

    # keine Tabelle darf breiter als die Textbreite (16 cm) sein
    tabellen <- regmatches(geholt$xml, gregexpr("<w:tbl[ >].*?</w:tbl>", geholt$xml,
                                               perl = TRUE))[[1]]
    expect_gte(length(tabellen), 3)          # Kennzahlen + 2 Kinderlisten + Anhang
    for (tabelle in tabellen) {
      spalten <- as.numeric(sub('.*"([0-9]+)"', '\\1',
                                regmatches(tabelle, gregexpr('w:gridCol w:w="[0-9]+"',
                                                             tabelle, perl = TRUE))[[1]]))
      breite_cm <- sum(spalten) / 567          # Twips -> cm
      expect_lte(round(breite_cm, 2), 16)
    }

    # im Programmverzeichnis bleibt nichts liegen
    expect_false(file.exists("infobrief/infobrief.knit.md"))
    expect_false(dir.exists(file.path(tempdir(), paste0("infobrief_", Sys.getpid()))))
  })
})

test_that("ohne zugeordnete Kinder wird kein Infobrief erstellt", {
  df <- lade_fixture("klasse_5c.tsv")
  df$Klasse <- c(rep("5c", 4), rep("7c", nrow(df) - 4))   # kein Namensbezug zu 6c
  k <- build_cohort(df, 5, 7)
  expect_error(create_infobrief(k, klassenleitung = "", absender = ""),
               "Keine zugeordneten Kinder")
})
