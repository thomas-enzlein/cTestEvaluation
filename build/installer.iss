; Inno-Setup-Skript fuer die C-Test-Auswertung.
;
; Gebaut wird ueber build/build_factory.ps1 - der Aufruf lautet dort:
;
;   ISCC.exe /DMyAppVersion=<Version> /DSrcDir="<Auslieferungsordner>" /O"<Ausgabe>" installer.iss
;
; Von Hand (Inno-Setup-IDE) gelten die Vorgaben unten; dann muessen im Ordner
; {#SrcDir} folgende Dinge liegen:
;
;   run.bat, run.R, icon.ico, README.md, LICENSE.md, build-info.txt
;   app\      Quellcode der App (app.R, ui.R, server.R, global.R, components\, functions\, ...)
;   R\        portables R (bin\, library\, ...)
;   chrome\   portables Chrome (chrome.exe)
;   pandoc\   pandoc (pandoc.exe)

#ifndef MyAppVersion
  #define MyAppVersion "0.0"
#endif
#ifndef SrcDir
  #define SrcDir "."
#endif

#define MyAppName "C-Test Auswertung"
#define MyAppPublisher "Thomas Enzlein"
#define MyAppURL "https://github.com/thomas-enzlein/cTestEvaluation"
#define MyAppExeName "run.bat"
#define MyAppIcoName "icon.ico"

[Setup]
; Die AppId kennzeichnet die Anwendung. Gleiche AppId = die bestehende
; Installation wird aktualisiert (keine zweite Programmzeile in Windows).
AppId={{01732D79-0061-4950-A581-7A797DE96CA4}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
AppVerName={#MyAppName} {#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
DefaultDirName=C:/ProgramData/{#MyAppName}
DisableDirPage=yes
DisableProgramGroupPage=yes
; Installation ohne Adminrechte moeglich; mit /DMyAppVersion gesetzte Version
PrivilegesRequired=lowest
PrivilegesRequiredOverridesAllowed=dialog
OutputBaseFilename=ctest_auswertung_{#MyAppVersion}
SetupIconFile={#SrcDir}\{#MyAppIcoName}
Compression=lzma
SolidCompression=yes
WizardStyle=modern
UninstallDisplayIcon={app}\{#MyAppIcoName}

[Languages]
Name: "german"; MessagesFile: "compiler:Languages\German.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "{#SrcDir}\{#MyAppExeName}"; DestDir: "{app}"; Flags: ignoreversion
Source: "{#SrcDir}\run.R"; DestDir: "{app}"; Flags: ignoreversion
Source: "{#SrcDir}\{#MyAppIcoName}"; DestDir: "{app}"; Flags: ignoreversion
Source: "{#SrcDir}\README.md"; DestDir: "{app}"; Flags: ignoreversion
Source: "{#SrcDir}\LICENSE.md"; DestDir: "{app}"; Flags: ignoreversion
Source: "{#SrcDir}\build-info.txt"; DestDir: "{app}"; Flags: ignoreversion
; Paketversionen zum Nachschauen; fehlt die Datei, wird trotzdem gebaut
Source: "{#SrcDir}\build-info-pakete.csv"; DestDir: "{app}"; Flags: ignoreversion skipifsourcedoesntexist
Source: "{#SrcDir}\app\*"; DestDir: "{app}\app"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "{#SrcDir}\R\*"; DestDir: "{app}\R"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "{#SrcDir}\chrome\*"; DestDir: "{app}\chrome"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "{#SrcDir}\pandoc\*"; DestDir: "{app}\pandoc"; Flags: ignoreversion recursesubdirs createallsubdirs

[Icons]
Name: "{autoprograms}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; IconFilename: "{app}\{#MyAppIcoName}"
Name: "{autodesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; IconFilename: "{app}\{#MyAppIcoName}"; Tasks: desktopicon

[Dirs]
; Der Installationsordner muss auch fuer normale Benutzer schreibbar sein:
; die App legt dort "app\Auswertungen" mit den Ergebnissen an.
Name: "{app}"; Permissions: users-full

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: shellexec postinstall skipifsilent
