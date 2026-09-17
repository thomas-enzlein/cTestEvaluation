---
output: 
  html_document: 
    fig_caption: true
---

```{=html}
<style type="text/css">
  blockquote {
    padding: 10px 20px;
    margin: 0 0 20px;
    font-size: 15px;
    border-left: 5px solid #eee;
}
</style>
```

## C-Test-Auswertungstool

Diese Anwendung dient der Auswertung von C-Tests. Die Leistungen der Schüler werden anhand von Referenzwerten bewertet und eine Empfehlung zur Förderung der Schüler wird generiert.

Diese Empfehlung sollte allerdings als erster Hinweis verstanden werden und im Bedarfsfall mit weiteren diagnostischen Werkzeugen verifiziert werden.

#### Schritt-für-Schritt

Dies ist eine **Schnell-Start Anleitung** mit den wichtigsten Schritten. Weitere Informationen sind in den folgenden Kapiteln zu finden.

1.  **Schüler hinzufügen**: Gehen Sie zum Menüpunkt "Auswertung". Hier können Sie den `Name` des Schülers sowie den entsprechenden `WE-Wert` und `R/F-Wert` je Schüler eingeben. Klicken Sie auf `Hinzufügen`, um die Daten zur `Übersichtstabelle` hinzuzufügen. Stellen Sie sicher, dass `Anzahl der Test-Items` auf die Anzahl der bearbeiteten Lücken eingestellt ist und dass die `Klassenstufe` und `Klasse` richtig gewählt ist.

2.  **Übersichtstabelle**: Die `Übersichtstabelle` fasst alle Einträge zusammen und gibt Auskunft über die Handlungsempfehlungen je Schüler. Wenn ein oder mehrere Schüler falsch eingetragen wurden, können diese durch Anklicken markiert und mit dem `Entfernen`-Button gelöscht werden.

3.  **Daten speichern**: Ist die Auswertung abgeschlossen, können Sie mit dem `Speichern`-Button die Daten als Word- und Excel-Dokument ablegen. Es wird auch eine \*.tsv Datei erstellt, die später wieder geladen werden kann.

4.  **Suchfunktion**: Einzelne Schüler lassen sich durch Eintrag des Namens in das Feld `Suchen` finden.

5.  **Zwei Jahrgänge vergleichen**: Für Entwicklungsdiagramm und Entwicklungsbrief laden Sie mit dem `Laden`-Button nacheinander die \*.tsv-Datei des früheren und die des aktuellen Jahrgangs. Prüfen Sie links im Menü das Stufenpaar (`Vergleich von Stufe` → `bis Stufe`), kontrollieren Sie im Tab `Infobrief` die Zuordnung der Kinder und erstellen Sie dort den Brief. Für den **Stand-Brief je Klasse** genügt ein Jahrgang.

#### Übersicht über die Funktionen

![Linkes Menüband](images/sidebar.png){style="float:left;margin-left:5px;margin-right:25px" width="150"}

Auf der linken Seite befindet sich ein Menüband (siehe Abbildung).

Die oberen beiden Buttons können genutzt werden um die aktuell geladenen/eingegebenen Daten zu speichern bzw. bereits vorhandene Daten im \*.tsv-Format zu laden. Es können auch mehrere Datensätze nacheinander geladen werden um so erweiterte Analysen durchzuführen.

Unter dem Laden-Button stehen `Vergleich von Stufe` und `bis Stufe`: hier wird festgelegt, welche beiden Jahrgänge verglichen werden (z. B. 5 → 6). Die Auswahl gilt für das Entwicklungsdiagramm im Tab Statistik **und** für den Infobrief. Sobald zwei Jahrgänge geladen sind, ist das Stufenpaar mit den meisten zuordenbaren Kindern bereits eingestellt. Solange keine zwei Jahrgänge geladen sind, ist die Auswahl gesperrt und darunter erscheint ein kurzer Hinweis, was zu tun ist – im Normalfall steht dort nichts.

1.  **Auswertung**: Hier können die **Schülerdaten eingegeben**, die Ergebnisse als **Tabelle betrachtet** und **gespeichert** werden.
2.  **Statistik**: Hier können Verteilungen und Kennzahlen (Mittelwert ± Standardabweichung, Median) betrachtet sowie die **Entwicklung zweier Jahrgänge** dargestellt werden.
3.  **Infobrief**: Hier wird entweder der **Stand-Brief je Klasse** oder – mit Prüfung der **Zuordnung der Kinder zum Vorjahr** – der **Entwicklungsbrief für das Kollegium** erstellt (siehe unten).
4.  **Elternbrief**: Hier können automatisiert Elternbriefe erstellt werden.
5.  **Anleitung**: Diese Anleitung.

<br>
<br>

### Auswertung

#### Schüler hinzufügen

Dieser Unterpunkt dient vor allem dem Einpflegen der Schülerdaten. Hier können der `Name` des Schülers sowie der entsprechende `WE-Wert` und `R/F-Wert` je Schüler eingegeben werden und mit `Hinzufügen` zur `Übersichtstabelle` hinzugefügt werden. Vor der Eingabe des ersten Schülers sollte `Anzahl der Test-Items` auf die Anzahl der bearbeiteten Lücken (typischerweise 20 pro Text) eingestellt werden. Dieser Wert sollte eigentlich für alle Schüler gleich sein. Falls ein Schüler nur einen Teil der Texte bearbeitet hat, kann der Wert aber auch für jeden Schüler verändert werden. Weiterhin muss die `Klassenstufe` und `Klasse` eingestellt werden. Normalerweise sollte auch dies für alle Schüler gleich sein, kann aber wie die `Anzahl der Test-Items` für jeden Schüler individuell gesetzt werden sodass auch mehrere Klassen aufeinmal eingetragen werden können.

Wurde ein oder mehrere Schüler falsch eingetragen, können diese durch Anklicken markiert und mit dem `löschen`-Button gelöscht werden.

![](images/auswertung_eingabe.PNG){width="600"}

> **Tipp**: Am schnellsten lassen sich Schüler eingeben, indem man die Felder mit der **Tabulatortaste** wechselt. Also ins Feld `Name` klicken, `Name` eingeben, **Tab** (Cursor springt ins Feld `WE-Wert`), `WE-Wert` eintragen, **Tab**, `R/F-Wert` eintragen, **Tab** (`Hinzufügen`-Button ist markiert), **Enter** (Cursor springt zurück ins Feld **Name** und der nächste Schüler kann eingetragen werden).

#### Übersichtstabelle

Die `Übersichtstabelle` fasst alle Einträge zusammen und gibt Auskunft über die Handlungsempfehlungen je Schüler.

> **Tipp**: Die Tabelle lässt sich sortieren, indem man auf die Spaltennamen klickt. Dabei wechselt die Ansicht zwischen absteigend und aufsteigend. Sortiert man nach `Kat.`, kann man beispielsweise schnell die Schüler ermitteln, die am meisten Unterstützung brauchen.

Ist die Auswertung abgeschlossen, kann man mit dem `Speichern`-Button die Daten als Word- und Excel-Dokument ablegen (siehe linkes Menüband). Weiterhin wird eine \*.tsv Datei erstellt, die später wieder geladen werden kann (siehe Kapitel Erweitert).

Einzelne Schüler lassen sich durch Eintrag des Namens in das Feld `Suchen` finden.

![](images/tabelle.PNG){width="600"}

#### Speichern und Ausgabeordner

Alle Dateien landen im Ordner `Auswertungen`. Nach dem Speichern öffnet die App diesen Ordner automatisch, der Pfad steht zusätzlich in der Meldung. Kann die App dort nicht schreiben (zum Beispiel, weil sie unter `C:\ProgramData` installiert ist und der angemeldete Benutzer dort keine Rechte hat), weicht sie auf den Ordner `C-Test Auswertung` in den eigenen Dokumenten aus – die Meldung nennt dann diesen Pfad. Ist keiner der beiden Ordner beschreibbar, meldet die App einen Fehler, statt die Arbeit stillschweigend zu verwerfen.

**Zusätzliche Sicherung:** Bevor Elternbriefe oder Infobrief erstellt werden, legt die App den aktuellen Datenstand automatisch als \*.tsv im Auswertungsordner ab (`..._sicherung_<Uhrzeit>.tsv`), falls er dort noch nicht liegt. Ein vergessener Klick auf `Speichern` geht damit nicht verloren – der Ordner enthält dadurch zusätzliche Dateien, die sich gefahrlos löschen lassen.

### Statistik

Hier können die eingegebenen Daten als Diagramme dargestellt werden.

![](images/statistik_histogramm.PNG){width="700"}

Das linke Diagramm zeigt den `WE-Wert` in Prozent. Mit der Checkbox `Differenz` zeigt es stattdessen die Differenz zwischen `WE-Wert` und `R/F-Wert`. Da der `WE-Wert` immer mindestens so hoch ist wie der `R/F-Wert`, zeigt diese Differenz, wie viele Wörter erkannt, aber falsch geschrieben wurden. Mit der Checkbox `Gesamtübersicht` werden (falls mehrere Klassen vorhanden sind) alle Daten zusammen oder nach Klasse getrennt ausgewertet.

Das rechte Diagramm zeigt die `R/F-Werte` in Prozent; die gestrichelte Linie markiert den Referenzwert für Gesamtschulen und die gepunktete Linie den unteren Grenzwert des Normalbereichs (65 %). Alle Schüler, die unterhalb dieser Grenze liegen, brauchen u.U. Unterstützung oder sollten zumindest näher betrachtet werden. Unter den Diagrammen stehen Mittelwert ± Standardabweichung sowie der Median; den Median zeigt außerdem der Stand-Brief (der Entwicklungsbrief nicht).

Mit `Diagramm Typ` wird die Darstellung gewählt:

-   `Histogramm`: Verteilung der Werte.
-   `Dichte`: geglättete Verteilung, praktisch um mehrere Klassen zu vergleichen.
-   `Entwicklung (5 → 6)`: je Kind ein Balken mit der Veränderung vom früheren zum aktuellen Jahrgang (blau = Verbesserung, rot = Rückgang), daneben der aktuelle Wert. Bezieht sich auf das links eingestellte Stufenpaar.
-   `Verlauf (5 → 6)`: eine Linie je Kind über die beiden Jahrgänge, oben der WE-Wert, unten der R/F-Wert. Lohnt sich vor allem bei kleinen Gruppen – bei vielen Kindern werden die Linien unübersichtlich.

![](images/statistik_entwicklung.PNG){width="1000"}

> **Tipp**: Die Diagramme sind interaktiv. Mit der Maus lässt sich ein Bereich aufziehen (Zoom), und beim Überfahren der Balken werden die zugehörigen Schüler angezeigt.

Die Diagrammtypen `Entwicklung` und `Verlauf` verwenden **nur Kinder, für die beide Messungen vorliegen**. Grundlage ist dieselbe Zuordnung, die auch der Infobrief nutzt (siehe unten).

#### Vergleich je Kind (zwei Stufen)

Sobald zwei Jahrgänge geladen und einander zugeordnet sind, steht unter den Diagrammen die Tabelle `Vergleich je Kind (zwei Stufen)`: eine Zeile je Kind mit `Name`, `Klasse` (z. B. `5c → 6c`), `WE % (5 → 6)`, `Δ WE`, `R/F % (5 → 6)`, `Δ R/F` und `Hinweis`. Die Spalten lassen sich durch Klick auf die Überschrift sortieren, das Suchfeld filtert nach Namen. Rückgänge stehen rot, Verbesserungen grün; ab 20 Prozentpunkten zusätzlich fett.

Die Liste enthält **alle** Kinder aus beiden Jahrgängen – auch die, die nur in einem Jahrgang vorkommen oder in einem Jahrgang nicht teilgenommen haben. Vorhandene Werte stehen immer da; die fehlende Seite wird als `-` gezeigt (`62,5 → -`), ein Vergleichswert `Δ` gibt es dann nicht. Die Spalte `Hinweis` nennt den Grund (`kein Vorjahreswert`, `neu in der Klasse`, `nicht teilgenommen (6. Klasse)`, `Vorschlag (bitte prüfen)` …) – im Word-Anhang fehlt sie, damit die Tabelle auf die Seite passt. Gezählt und gerechnet wird weiterhin nur mit Kindern, die in beiden Jahrgängen zugeordnet sind.

### Lehrkräfte-Infobrief

Der Tab `Infobrief` kennt zwei Briefarten – oben im Abschnitt „Art des Briefes" wählen:

-   **`Stand je Klasse`** (Voreinstellung): der **Stand eines Jahrgangs**, ohne Vorjahresvergleich.
-   **`Entwicklung (zwei Jahrgänge)`**: die **Entwicklung einer Klasse über zwei Jahrgänge**.

Beide nutzen dieselbe Word-Vorlage und denselben `Absender`. Die **Anrede schreibt die App selbst** („Liebe Klassenleitung der 5c," bzw. „Liebe Klassenleitungen der 6b und 6c,"); ein Feld für die Klassenleitung gibt es nicht mehr. Der Tab ist für das Kollegium gedacht (nicht für die Eltern).

#### Stand-Brief je Klasse

Für die Rückmeldung an eine Klasse – nach dem Ersttest in der 5 genauso wie nach dem Re-Test in der 6. Es wird **keine** zweite Stufe gebraucht, es gibt keine Zuordnung und keinen Vorjahresvergleich.

1.  **Briefart wählen**: `Stand je Klasse` (steht schon so).
2.  **`Infobrief erstellen`**: Es entsteht **eine** Word-Datei `Infobrief_Stand_<Klassen>.docx` (z. B. `Infobrief_Stand_5a_5b.docx`) mit **einer Seite je geladener Klasse**. Klassen ohne Werte werden übersprungen und in der Meldung genannt.

Inhalt je Klasse:

-   Kennzahlen: `n (mit Werten)`, `WE %` und `R/F %` als Mittel ± SD und Median.
-   `Ergebnisse im Überblick`: vier Gruppen mit Anzahl, Prozent und den Kategorien, die dahinterstehen – *kein Handlungsbedarf* (1A, 1B, 2A, 2B) · *Rechtschreibung ausbaufähig* (3C, 4C, 5C) · *Rechtschreibung ausbaufähig, großer Unterschied WE zu R/F* (3C\*, 4C\*, 5C\*) · *Handlungsbedarf* (3D, 4D, 5D, 4E, 5E) – plus die Zeile *nicht teilgenommen*. Die Gruppe ist farbig hinterlegt, die Kategorien der Kinder sind in den Tabellen farbig wie in Excel und App.
-   Ein Satz, wie viele Kinder mit ihrem R/F-Wert unter dem unteren Normbereich liegen (Grenze aus den Einstellungen, Standard 65 %). Diese Kinder stehen in den Tabellen **komplett fett** – samt Name.
-   `Die höchsten Werte` (bis zu 5 Kinder) und der `Anhang: Übersicht aller Kinder der Klasse` mit allen Kindern, alphabetisch, mit `WE %`, `R/F %` und `Kat.`.

Plots enthält der Stand-Brief nicht.

#### Entwicklung über zwei Jahrgänge

Der Entwicklungsbrief fasst die **Entwicklung einer Klasse über zwei Jahrgänge** zusammen (z. B. 5c → 6c). Darunter steht die Zuordnungstabelle.

##### Voraussetzungen

1.  **Zwei Jahrgänge laden**: die \*.tsv-Datei des früheren Jahrgangs und die des aktuellen (Laden-Button oben links). Die Klasse bleibt über die Jahre gleich (z. B. 5c → 6c), deshalb findet die App dieselben Kinder über den Namen wieder.

2.  **Vergleich einstellen**: links im Menü stehen `Vergleich von Stufe` und `bis Stufe`. Die Auswahl ist gesperrt, solange keine zwei Jahrgänge geladen sind – dann steht darunter ein Hinweis. Ein Vorschlag (das Paar mit den meisten zuordenbaren Kindern) ist bereits eingestellt.

3.  **Zuordnung prüfen**: Die Tabelle im Abschnitt `Zuordnung zum Vorjahr` listet die Kinder beider Jahrgänge mit Bewertung.
    -   *automatisch zugeordnet*: Der Name stimmt eindeutig überein (Schreibvarianten wie "Beispel" statt "Beispiel" oder ein anderer Bindestrich sind kein Problem).
    -   *Vorschlag (bitte prüfen)*: Die Namen sind ähnlich, aber nicht eindeutig (z. B. fehlender Zweitname). Zeile markieren und `Zuordnung bestätigen` oder `trennen` wählen.
    -   *nicht eindeutig*: Derselbe Name kommt in einer Stufe mehrfach vor – hier entscheidet die App bewusst nicht.
    -   `Zuordnungen zurücksetzen` verwirft alle Entscheidungen nach einer Rückfrage.

    Die Entscheidungen werden gespeichert und beim nächsten Lauf automatisch wieder angewendet (siehe *Zuordnung und Ausgabe*).

![](images/infobrief_zuordnung.PNG){width="1000"}

4.  **Brief erstellen**: optional `Absender` eintragen, dann `Infobrief erstellen`. Es entsteht eine Word-Datei je Stufenpaar (z. B. `Infobrief_5-6_5c_6c.docx`) im Auswertungsordner; die Anrede bildet die App aus den Klassen des Briefes.

![](images/infobrief_brief.PNG){width="600"}

##### Inhalt des Briefes

-   Kopf mit Datum, Überschrift (Jahrgang X im Vergleich zu Jahrgang Y) und Anrede (automatisch aus den Klassen des Briefes).
-   Je Klassenbuchstabe ein Abschnitt `Kohorte <Buchstabe>` mit einer Tabelle: eine Zeile je Klasse und Jahrgang, mit `n (mit Werten)`, `WE % (Mittel ± SD)` und `R/F % (Mittel ± SD)`. Darunter eine Zeile `Differenz` mit der Veränderung der Mittelwerte (aktuelle Stufe minus frühere Stufe, z. B. `+16,3`): grün bei Verbesserung, rot bei Rückgang, ab 20 Prozentpunkten fett – dieselbe Farbgebung wie bei einzelnen Kindern.
-   Darunter: wie viele Kinder aktuell unter dem unteren Normbereich (R/F-Wert unter dem eingestellten Wert, Standard 65 %) liegen, wie viele davon schon im Vorjahr betroffen waren und wie viele neu hinzugekommen sind bzw. den Normbereich wieder erreicht haben.
-   Ein Hinweis auf deutliche Rückgänge (mehr als 10 %); solche Werte sind farbig hervorgehoben (grün = Verbesserung, rot = Rückgang) und ab 20 Prozentpunkten zusätzlich fett.
-   Die Tabellen `Die größten Verbesserungen` und `Die schwächste Entwicklung` mit je bis zu 5 Kindern. Bei der schwächsten Entwicklung stehen zuerst die Kinder, die weiterhin unter dem unteren Normbereich liegen.
-   Auf einer eigenen Seite `Hinweise`, danach auf einer weiteren Seite der `Anhang: Vergleich je Kind`: **alle** Kinder in alphabetischer Reihenfolge mit den Werten, die vorhanden sind (`Name`, `Klasse`, `WE % (5 → 6)`, `Δ WE`, `R/F % (5 → 6)`, `Δ R/F`). Fehlt ein Jahrgang, steht dort `-` – die Werte des vorhandenen Jahrgangs sind trotzdem zu sehen. Den Grund für eine fehlende Seite nennt die Liste im Tab Statistik (Spalte `Hinweis`), im Word-Anhang steht sie wegen der Seitenbreite nicht.

##### Hinweise im Brief

Diese Punkte stehen unter `Hinweise`. Die Kinder werden dort mit Namen genannt, weil sie in keiner der Tabellen auftauchen:

-   Neu in der Klasse (kein Vorjahreswert).
-   Kein Partner im aktuellen Jahrgang gefunden.
-   Nicht eindeutig zuzuordnen (gleicher Name mehrfach).
-   Noch nicht bestätigte Zuordnungen – diese werden nicht mitgezählt.
-   Klassenwechsel (Kind ist im aktuellen Jahrgang in einer anderen Klasse).
-   Nicht teilgenommen (Kategorie 0).

##### Zuordnung und Ausgabe

-   Die Zuordnungsdatei heißt nach den Klassen der beiden Jahrgänge, z. B. `zuordnung_5c-6c.tsv`, und liegt im Auswertungsordner. Neben den Entscheidungen stehen darin die Quelldateien und eine Prüfsumme: Passt die Prüfsumme nicht zu den geladenen Daten, weist die App darauf hin, dass die gespeicherte Zuordnung zu anderen Daten gehört.
-   Nach dem Erstellen des Briefes öffnet die App den Auswertungsordner; die Meldung nennt den Dateinamen.
-   Sobald ein Vergleich möglich ist, enthält die Excel-Datei beim `Speichern` ein zusätzliches Blatt `Vergleich` mit derselben Tabelle; die Word-Auswertung bekommt sie als Anhang.

> **Hinweis**: Ausgewertet und gezählt werden nur Kinder, für die zwei Messungen vorliegen. Alle anderen (neue Kinder, fehlende Vorjahreswerte, nicht bestätigte Vorschläge, nicht teilgenommene Kinder) erscheinen unter `Hinweise` und werden nicht mitgezählt – im `Anhang: Vergleich je Kind` stehen sie trotzdem mit ihren vorhandenen Werten.

### Vorlagen und Einstellungen

Elternbrief und Infobrief nutzen **dieselbe** Word-Vorlage. Im Tab `Elternbrief` liegt dafür der Abschnitt `Vorlagen und Einstellungen`:

-   `Briefvorlage öffnen`: legt beim ersten Klick eine persönliche Kopie der Vorlage unter `Dokumente\C-Test Auswertung\vorlagen` an und öffnet sie in Word. Angepasst werden hier **Briefkopf und Logo** (Kopf- und Fußzeile), Schriftarten, Absatzformate und Seitenränder. Die Änderung gilt für alle künftigen Elternbriefe **und** für den Infobrief. Der Wortlaut der Briefe steht nicht in der Vorlage – der gehört zur App und ändert sich mit ihr.
-   `Vorlagen-Ordner öffnen`: öffnet den Ordner mit allen anpassbaren Dateien. Neben der Vorlage (`template.docx`) liegen dort die Ergebnistabelle des Briefes (`table.png`) und die Texte zu den Kategorien (`ergebnisse.xlsx`). Auch diese beiden werden übernommen, sobald sie dort geändert vorliegen; eine unlesbare `ergebnisse.xlsx` wird ignoriert, dann gilt weiter die mitgelieferte Tabelle.
-   `Einstellungen öffnen`: öffnet `Dokumente\C-Test Auswertung\einstellungen.txt`. Darin stehen Ihre Eingaben aus der App – `Name des Lehrers`, `Signatur`, `Link zu Übungen`, `Absender`, `Anzahl der Test-Items` und die Ansicht im Tab Statistik (Differenz, Gesamtübersicht, Diagrammtyp). Sie werden **automatisch gespeichert**, sobald Sie etwas ändern, und beim nächsten Start wieder in die Felder eingesetzt. Sie müssen sie also nur einmal eintragen.

Die Anpassungen liegen bewusst **nicht** im Programmordner: dort würde ein Update der App sie überschreiben. Wird eine Datei im persönlichen Ordner gelöscht, gilt wieder das mitgelieferte Original (der Knopf legt es auf Wunsch erneut an).

#### Die beiden R/F-Marken einstellen

In derselben Einstellungsdatei stehen zwei Marken des **R/F-Werts** (Prozent):

```
rf_referenz=71.3
rf_norm_unten=65
```

-   `rf_referenz` ist der **Referenzwert für Gesamtschulen** – die gestrichelte Linie im R/F-Diagramm.
-   `rf_norm_unten` ist der **untere Normbereich** – die gepunktete Linie im R/F-Diagramm, die Linie im R/F-Teil des Verlaufsdiagramms, die Färbung im Entwicklungsdiagramm und die Markierungen im Lehrkräfte-Infobrief („Unterhalb des unteren Normbereichs liegen aktuell …", Reihenfolge der Tabelle „Die schwächste Entwicklung").

Beide haben noch kein Feld in der Oberfläche, lassen sich aber in der Datei ändern; die Änderung gilt ab dem nächsten Start.

**Die Kategorien der Kinder ändern sich dadurch nicht.** Die Grenzen, aus denen sich `Kat.`, Empfehlung und Briefe ergeben (71,3 / 66,3 / 56,3 / 36,2 % für den R/F-Wert, 65 % für den Wortschatz-Wert), gehören zum Verfahren und sind fest. Auch der Wortschatz-Wert ist unabhängig, obwohl er denselben Betrag hat wie der untere Normbereich.

Erwartet werden Prozentwerte mit `rf_norm_unten` < `rf_referenz` (beide zwischen 0 und 100). Andere Angaben – Text, Werte außerhalb des Bereichs oder ein unterer Normbereich oberhalb des Referenzwerts – werden beim Start **ignoriert und gemeldet**; dann gelten wieder 71,3 und 65. Ein von Hand geänderter Wert wird von der App nie überschrieben.

### Tests

Fuer die Weiterentwicklung gibt es eine Testsuite (nicht Teil der installierten App):

```
Rscript tests/run_tests.R            # alle Tests
Rscript tests/run_tests.R cohort     # nur eine Datei (Filter)
```

Voraussetzung sind **alle** Pakete aus `req.txt` (die Tests laden die App) plus die Testpakete aus `req_dev.txt`:

```
install.packages(c(readLines("req.txt"), readLines("req_dev.txt")))
```

Ohne pandoc werden die Tests übersprungen, die Word-Dateien rendern.
