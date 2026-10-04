# Änderungen

Der Abschnitt der Version, die als Tag veröffentlicht wird, wird als Beschreibung des
GitHub-Releases verwendet.

## 1.10

**Elternbrief: Klassenauswahl.** Im Tab `Elternbrief` wird jetzt gewählt, für wen die Briefe
entstehen: eine einzelne Klasse, ein ganzer Jahrgang oder alle geladenen Klassen. Voreingestellt
ist die höchste geladene Klasse (bei 5a und 6a also 6a, bei 5a, 5b und 5c die 5a). Alle Briefe
einer Auswahl stehen in **einer** Word-Datei und sind darin nach Klasse und Name sortiert.

**Ergebnistabelle als echte Word-Tabelle.** Die Seite `Ergebnisse` des Elternbriefes enthielt
bisher ein Bild (`table.png`): Der Text war nicht markierbar. Die Tabelle ist jetzt eine normale
Word-Tabelle aus `ergebnisse.xlsx`. Vorlage dafür ist das Blatt `Tabelle2` – Zeilen, Überschriften
und **verbundene Zellen** kommen von dort (12 Zeilen, Kategorie, Bedeutung, Handlungsempfehlung;
gleiche Empfehlungen einmal über mehrere Zeilen verbunden). Fehlt das Blatt, wie in den bisherigen
persönlichen Kopien, wird dieselbe Tabelle aus dem ersten Blatt abgeleitet: ohne die Zeile „0",
ohne die Sternchen-Doppelzeilen und mit verbundenen Empfehlungstexten. Schriftgröße 7 pt und
Zellentrenner (waagerecht und senkrecht), damit jede Zeile eindeutig zu ihrer Kategorie gehört –
so bleibt der Brief auf zwei Seiten.

**QR-Code und Linktext.** Der QR-Code war im Brief 25 % breiter als hoch (1,25 × 1,00 Zoll bei
quadratischem Bild) und dadurch sichtbar verzerrt; er wird jetzt quadratisch und mit 1 Zoll
eingefügt. Zwischen `Sie möchten Ihr Kind unterstützen?` und `Dann schauen Sie hier in unsere
Sammlung:` steht ein echter Zeilenumbruch, die Zeile bricht also nicht mehr an beliebiger Stelle
um; vor dem Linktext steht jetzt ein Abstand zur Tabelle.

**Infobrief: Klassenauswahl und optionale Grußformel.** Der Tab `Infobrief` hat dieselbe
Auswahl wie der Elternbrief, und sie richtet sich nach der Briefart: beim `Stand je Klasse` sind es
die **Klassen** (alle, ein Jahrgang, eine Klasse), beim Entwicklungsbrief die **Kohorten** mit dem
Paar aus der Stufenauswahl (`c: 5c → 6c`; fehlt ein Jahr, steht `c (nur 5c)`). Vorausgewählt ist
jeweils die höchste geladene Klasse bzw. ihre Kohorte. Beim Stand-Brief bestimmt die Auswahl die
Seiten im Dokument, beim Entwicklungsbrief die Kohorte – alle Zahlen im Brief beziehen sich dann
auf die Auswahl. Der `Absender` ist optional: bleibt das Feld leer, endet der Brief ohne
Grußformel (der Infobrief geht ans Kollegium).

**Kategorie-Spalte wieder farbig.** Die Spalte `Kat.` der Übersichtstabelle war seit 1.7 nicht
mehr eingefärbt: die Prüfung „hat die Tabelle eine Kategorie-Spalte?" fragte das DT-Objekt statt
seiner Daten, dessen Spaltennamen leer sind. Jetzt gilt wieder die gewohnte Farbzuordnung
(grün/gelb/orange/rot wie in Excel und in den Briefen).

**Rechtschreibung.** Im Elternbrief heißt es jetzt richtig `bei allen Schülerinnen und Schülern
der 5. Klasse` (Dativ), ein doppeltes Leerzeichen ist entfernt.

**Vorlagen an einer Stelle.** Beide Briefe nutzen dieselbe Word-Vorlage; die zweite, identische
Kopie im Infobrief-Ordner ist entfallen. Die mitgelieferten anpassbaren Dateien liegen zentral in
`vorlagen\` (Word-Vorlage, `ergebnisse.xlsx`), die persönliche Kopie weiterhin unter
`Dokumente\C-Test Auswertung\vorlagen` – sie überlebt ein Update unverändert. Im Tab
`Elternbrief` steht jetzt unter den Knöpfen, **welche** Vorlage gerade gilt (Pfad und
Änderungsdatum).

## 1.9

**Sortierung in Word und Excel.** Die beim Speichern erzeugten Dateien sind jetzt erst nach Klasse,
dann nach Namen sortiert. Die tsv behält die Eingabereihenfolge, das Vergleichsblatt war schon
alphabetisch.

## 1.8

**Stand-Brief je Klasse.** Im Tab `Infobrief` gibt es jetzt die Briefart-Wahl: `Stand je Klasse`
(neu, Voreinstellung) oder `Entwicklung (zwei Jahrgänge)` (wie bisher). Der Stand-Brief braucht
keine zwei Jahrgänge – er funktioniert nach dem Ersttest in der 5 genauso wie nach dem Re-Test
in der 6. Er liefert **eine** Word-Datei `Infobrief_Stand_<Klassen>.docx` mit einer Seite je
geladener Klasse: Kennzahlen (`n (mit Werten)`, Mittel ± SD, Median), vier Gruppen mit Anzahl,
Prozent und den zugehörigen Kategorien, Kinder unter dem unteren Normbereich (ganze Zeile fett),
die höchsten Werte und ein Anhang mit allen Kindern. Kategorien sind farbig wie in Excel und
Word, Plots gibt es nicht, Klassen ohne Werte werden übersprungen und gemeldet.

**Anrede automatisch.** Das Feld `Klassenleitung` ist entfallen: Die Anrede kommt aus den Klassen
des Briefes (`Liebe Klassenleitung der 5c,` / `Liebe Klassenleitungen der 6b und 6c,`). Die alte
Zeile `info_klassenleitung` fällt beim nächsten Speichern aus `einstellungen.txt`.

**Kleinigkeiten.** Mehr Abstand um die Tabellen, README aufgeräumt (deutsch nur noch im Kopf).

## 1.7

**Robustes Laden von Ergebnissen.** Eine tsv-Datei muss nicht mehr exakt aufgebaut sein: Die
App erkennt die Spalten auch bei anderer Schreibweise (`WE %`, `we_percent`, `Kategorie`,
`Schüler` …), ergänzt fehlende Spalten, berechnet Kategorie und Empfehlung aus den
Prozentwerten und meldet in der Oberfläche, was sie vorgefunden und ergänzt hat. Fehlt die
Spalte `Name`, gibt es eine klare Fehlermeldung und die App läuft weiter – vorher brach das
Laden mit einem Fehler ab. Damit lassen sich auch ältere Exporte (z. B. von 2023) wieder
auswerten.

**Itemzahl in der tsv.** Beim Speichern wird je Kind die Anzahl der Test-Items
mitgeschrieben. Sie steht nur in der tsv (nicht in der Übersichtstabelle und nicht in den
Word- und Excel-Berichten) und macht fehlende Werte nachrechenbar: Fehlt ein Prozentwert
oder – bei bekannten Prozentwerten – der Rohwert, ergänzt die App ihn und weist darauf hin.
Bei älteren Dateien ohne diese Spalte ermittelt die App die Itemzahl aus Wert und
Prozentwert, sodass einmal Laden und Speichern die Datei ins neue Format überführt.

**Plausibilitätsprüfung.** Die App meldet Auffälligkeiten, statt damit zu rechnen:
Prozentwerte außerhalb 0 bis 100 % (die Spalte wird nicht übernommen und, wenn möglich,
korrekt nachgerechnet), `WE` kleiner als `R/F`, Werte oberhalb der Itemzahl, Prozentwerte,
die nicht zu Wert und Itemzahl passen, sowie eine Kategorie-Spalte, die nicht zu den Werten
gehört.

**Fortschrittsanzeige.** Elternbriefe und Infobrief laufen nicht mehr unsichtbar: Ein
Fortschrittsbalken nennt „Brief 3 von 24: Mustermann, Anna" und beim Infobrief den
jeweiligen Schritt. Auch das Speichern zeigt einen Fortschritt.

**Spaltenzuordnung.** Die Zuordnung arbeitet mit Merkmalen statt mit einer festen
Namensliste und deckt damit auch englische Schreibweisen ab (`vocabulary_percentage`,
`spelling %`). Mehrdeutige und nicht verwendete Spalten werden genannt.

**Keine Datenverluste mehr beim Briefversand.** „speichern" ist ein eigener Knopf – wer
Werte eintippt und nur die Briefe erstellt, verlor den Datensatz bisher: Er existierte
danach nur noch als Word-Datei, ohne Itemzahl und ohne Vorjahresvergleich. Jetzt sichert die
App den Datenstand automatisch als tsv, bevor Elternbriefe oder Infobrief gerendert werden.
Die Sicherung schreibt nur die tsv (keine Berichte), überschreibt nichts, fragt nicht nach
und wird übersprungen, wenn der Stand bereits als Datei vorliegt. Ein Fehler beim Sichern
wird gemeldet, blockiert die Briefe aber nicht.

**Übungslink und QR-Code im Elternbrief.** Mit eingetragenem Link entstand bisher kein
einziger Brief: Das QR-Bild wurde im Temp-Verzeichnis erzeugt, und das Einbetten rechnet
absolute Pfade relativ zum Ausgabeordner um – Word fand das Bild nicht mehr („src must be a
string starting with 'rId' or an existing image filename"). Das Bild entsteht jetzt neben dem
Dokument und wird mit relativem Pfad eingebettet; der Link zur Übungssammlung steht wieder
als Text und als QR-Code im Brief. Dieser Pfad ist ab jetzt durch einen Test abgedeckt.

**Vorlagen anpassen.** Im Tab Elternbrief öffnet ein Knopf die Word-Vorlage, ein zweiter den
Vorlagenordner. Angepasst wird eine persönliche Kopie unter
`Dokumente\C-Test Auswertung\vorlagen`, die ein Update der App übersteht und beim Erstellen
der Briefe die mitgelieferte Vorlage ersetzt. Sie gilt für Elternbrief **und** Infobrief
(beide nutzen dieselbe Vorlage) und steuert Briefkopf, Logo, Schrift und Seitenränder.
Zusätzlich anpassbar sind die Ergebnistabelle (`table.png`) und die Kategorie-Texte
(`ergebnisse.xlsx`); eine unlesbare Tabelle wird nicht übernommen, damit ein misslungener
Bearbeitungsversuch die Briefe nicht unbrauchbar macht.

**Einstellungen, die bleiben.** Name, Signatur, Link zur Übungssammlung, Absender,
Klassenleitung, Anzahl der Test-Items und die Ansicht im Statistik-Tab werden automatisch in
`Dokumente\C-Test Auswertung\einstellungen.txt` gespeichert und beim nächsten Start wieder in
die Felder eingesetzt – einmal eintragen genügt. Ein Knopf im Tab Elternbrief öffnet die
Datei. Unbekannte Schlüssel und Handeinträge in der Datei bleiben erhalten.

**Zwei R/F-Marken einstellbar.** In derselben Datei stehen `rf_referenz` (Standard 71,3,
Referenzwert für Gesamtschulen) und `rf_norm_unten` (Standard 65, unterer Normbereich). Beide
betreffen den **R/F-Wert**, haben noch kein Feld in der Oberfläche und wirken ab dem nächsten
Start auf die Diagrammlinien, die Färbung im Entwicklungsdiagramm und die Markierungen im
Infobrief. Unbrauchbare Angaben – Text, Werte außerhalb 0 bis 100 oder ein unterer
Normbereich über dem Referenzwert – werden beim Start ignoriert und gemeldet, dann gelten die
Standardwerte.

**Differenzzeile in der Klassentabelle.** Die Kennzahlen-Tabelle im Infobrief (eine Zeile je
Klasse und Jahrgang) endet jetzt mit einer Zeile `Differenz`: der Veränderung der Mittelwerte
zwischen den beiden Jahrgängen, farbig wie bei einzelnen Kindern (grün = Verbesserung, rot =
Rückgang, ab 20 Prozentpunkten fett).

**Der Anhang „Vergleich je Kind“ ist vollständig.** Bisher enthielt er nur Kinder, die in
beiden Jahrgängen zugeordnet waren, und ersetzte einen Wert durch einen Strich, sobald die
andere Seite fehlte – vorhandene Werte gingen dadurch verloren (wer in der 5. Klasse 62,5 %
erreicht hatte und in der 6. fehlte, stand mit vier Strichen da). Jetzt stehen **alle** Kinder
aus beiden Jahrgängen in der Liste, jeweils mit den vorhandenen Werten (`62,5 → -`, `- → 85,0`);
ein Vergleichswert `Δ` erscheint nur, wenn beide Seiten vorliegen, und eine Spalte `Hinweis`
nennt den Grund (`kein Vorjahreswert`, `neu in der Klasse`, `nicht teilgenommen (6. Klasse)`,
`Vorschlag (bitte prüfen)` …). Dieselbe vollständige Liste zeigen auch die Vergleichstabelle im
Tab Statistik und das Blatt `Vergleich` im Excel; in den Word-Ausgaben fehlt die Spalte
`Hinweis`, damit die Tabelle auf die Seitenbreite passt. **Die Auswertung selbst bleibt
unverändert:** Mittelwerte, Ranglisten, „unter dem Normbereich" und der Text des Infobriefs
rechnen weiter nur mit Kindern, die in beiden Jahrgängen zugeordnet sind.

**Kleinigkeiten und Fehlerbehebungen.** Ist die Zieldatei noch geöffnet – etwa der Infobrief
in Word –, meldet die App das **vor** dem Rendern im Klartext („Die Datei … ist gerade
geöffnet oder schreibgeschützt … Bitte schließen und erneut starten.") statt nach Minuten mit
„Permission denied" abzubrechen; das gilt für Infobrief, Elternbriefe und das Speichern. In
Meldungen und beim Öffnen von Ordnern stehen die
Pfade jetzt einheitlich in Windows-Schreibweise (`C:\Users\…\Documents\…` statt gemischt mit
Schrägstrichen). In Tabellen wird ein fehlender Einzelwert als `-` gezeigt statt als `NA` –
sichtbar zum Beispiel bei der Standardabweichung einer Gruppe, in der nur ein Kind Werte hat
(vorher „85,0 ±NA", jetzt „85,0 ±-"). Die Vergleichstabelle im gespeicherten Word-Dokument
zeigt Veränderungen mit Vorzeichen (`+10,0`) wie der Anhang des Infobriefs.

## 1.6

**Neu: Infobrief für das Kollegium.** Vergleicht zwei Jahrgänge (z. B. 5 → 6),
ordnet die Kinder automatisch über den Namen zu und erzeugt ein Word-Dokument je
Klasse – mit Kennzahlen, Kindern unter dem Normbereich samt Vorjahresvergleich,
den größten Verbesserungen, der schwächsten Entwicklung, einer Seite „Hinweise"
und dem Anhang „Vergleich je Kind".

**Zuordnung prüfen und merken.** Ähnliche Namen werden als Vorschlag angezeigt;
bestätigen oder trennen per Klick. Die Entscheidungen werden gespeichert
(`zuordnung_5c-6c.tsv`) und beim nächsten Lauf automatisch wieder angewendet.

**Statistik erweitert.** Neue Diagrammtypen „Entwicklung (5 → 6)" und
„Verlauf (5 → 6)" sowie die Tabelle „Vergleich je Kind (zwei Stufen)". Die
Differenz-Ansicht beim WE-Wert zeigt jetzt korrekt „erkannt, aber falsch
geschrieben" (WE minus R/F).

**Installation ohne Adminrechte.** R, Chrome und pandoc sind im Setup enthalten –
auf dem Schulrechner muss nichts installiert werden. Die App startet in einem
eigenen Fenster und rührt das persönliche Browserprofil nicht an.

**Robuster.** Ohne Internet scheitert kein Brief mehr (Kurzlink/QR-Code), und
wenn der Auswertungsordner nicht beschreibbar ist, weicht die App auf die eigenen
Dokumente aus und nennt den Ordner in der Meldung.

**Fehlerbehebungen.** Die Kategorie „0" (nicht teilgenommen) ging beim Laden einer
tsv-Datei verloren; der Excel-Export enthält jetzt ein Blatt „Vergleich"; das
Speichern und die Brief-Erzeugung melden Schreibfehler sichtbar statt still
abzubrechen.

## 1.5 und älter

Für Versionen vor 1.6 siehe die
[Releases](https://github.com/thomas-enzlein/cTestEvaluation/releases) und die
Commit-Historie.
