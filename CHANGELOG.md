# Änderungen

Der Abschnitt der Version, die als Tag veröffentlicht wird, wird als Beschreibung des
GitHub-Releases verwendet.

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
